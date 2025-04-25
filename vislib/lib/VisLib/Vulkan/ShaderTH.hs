{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module VisLib.Vulkan.ShaderTH where

import Control.Applicative hiding (many, some)
import Control.Lens.Operators ((??))
import Control.Monad
import Data.Char
import Data.Functor
import Data.Maybe
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import VisLib.Parse
import VisLib.Vulkan.Shader
import qualified Vulkan as VK
import Vulkan.Utils.ShaderQQ.GLSL.Shaderc
import Vulkan.Zero (zero)

space :: Parser String ()
space = L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser String a -> Parser String a
lexeme = L.lexeme space

identifier :: Parser String String
identifier = lexeme $ some $ satisfy isAlphaNum

parseTypeSize :: Parser String Int
parseTypeSize =
  lexeme $
    asum
      [ string "float" $> 4,
        string "vec2" $> 2 * 4,
        string "vec3" $> 3 * 4,
        string "vec4" $> 4 * 4,
        string "mat2" $> 2 * 2 * 4,
        string "mat3" $> 3 * 3 * 4,
        string "mat4" $> 4 * 4 * 4
      ]

parseBlockBody :: Parser String (Int, [ShaderPushConstantDescription])
parseBlockBody = do
  nameMap <- some $ do
    size <- parseTypeSize
    name <- identifier
    void $ lexeme $ char ';'
    return (name, size)
  let totalSize = sum $ map snd nameMap
  let offsets = scanl (+) 0 (map snd nameMap)
  let spcds =
        zipWith
          ( \(name, size) offset ->
              ShaderPushConstantDescription
                { _shaderPushConstantName = name,
                  _shaderPushConstantSize = size,
                  _shaderPushConstantOffset = offset
                }
          )
          nameMap
          offsets
  return (totalSize, spcds)

parseLocation :: Parser String ShaderLocationInformation
parseLocation = do
  void $ lexeme $ string "layout"
  void $ lexeme $ char '('
  void $ lexeme $ string "location"
  void $ lexeme $ char '='
  index <- lexeme L.decimal
  void $ lexeme $ char ')'
  void $ lexeme $ asum [string "in "]
  type' <-
    lexeme $
      asum
        [ string "vec2 " $> VK.FORMAT_R32G32_SFLOAT,
          string "vec3 " $> VK.FORMAT_R32G32B32_SFLOAT,
          string "vec4 " $> VK.FORMAT_R32G32B32A32_SFLOAT
        ]
  name <- identifier
  void $ lexeme $ char ';'
  return $ ShaderLocationInformation (Just name) index type'

parsePushConstant :: Parser String (VK.PushConstantRange, [ShaderPushConstantDescription])
parsePushConstant = do
  void $ lexeme $ string "layout"
  void $ lexeme $ char '('
  void $ lexeme $ string "push_constant"
  void $ lexeme $ char ')'
  void $ lexeme $ string "uniform"
  void $ lexeme $ manyTill anySingle (try (lexeme $ char '{'))

  (size, pcMap) <- parseBlockBody
  void $ lexeme $ char '}'
  _name <- identifier
  void $ lexeme $ char ';'
  return (VK.PushConstantRange {VK.offset = 0, VK.size = fromIntegral size, VK.stageFlags = zero}, pcMap)

parseStartOfLine :: (Show a) => Parser String a -> Parser String [a]
parseStartOfLine p = do
  lines' <- manyTill ?? eof $ do
    void $ many space1
    m <- optional (try p)
    when (isNothing m) $ skipManyTill anySingle (void eol <|> eof)
    return m
  return $ catMaybes lines'

findLocations :: String -> Q [(String, ShaderLocationInformation)]
findLocations source = do
  locations <- case runParser (parseStartOfLine parseLocation) "" source of
    Left err -> fail $ errorBundlePretty err
    Right x -> return x
  return $ map (\sli@ShaderLocationInformation {_shaderLocationName = name} -> (fromJust name, sli)) locations

findPushConstants :: String -> Q [(VK.PushConstantRange, [ShaderPushConstantDescription])]
findPushConstants source = case runParser (parseStartOfLine parsePushConstant) "" source of
  Left err -> fail $ errorBundlePretty err
  Right x -> return x

instance Lift VK.ShaderStageFlagBits where
  lift VK.SHADER_STAGE_VERTEX_BIT = [|VK.SHADER_STAGE_VERTEX_BIT|]
  lift VK.SHADER_STAGE_FRAGMENT_BIT = [|VK.SHADER_STAGE_FRAGMENT_BIT|]
  lift _ = error "Unsupported shader stage"
  liftTyped VK.SHADER_STAGE_VERTEX_BIT = [||VK.SHADER_STAGE_VERTEX_BIT||]
  liftTyped VK.SHADER_STAGE_FRAGMENT_BIT = [||VK.SHADER_STAGE_FRAGMENT_BIT||]
  liftTyped _ = error "Unsupported shader stage"

instance Lift VK.PushConstantRange where
  lift VK.PushConstantRange {VK.offset = offset, VK.size = size, VK.stageFlags = stageFlags} =
    [|VK.PushConstantRange {VK.offset = offset, VK.size = size, VK.stageFlags = stageFlags}|]
  liftTyped VK.PushConstantRange {VK.offset = offset, VK.size = size, VK.stageFlags = stageFlags} =
    [||VK.PushConstantRange {VK.offset = offset, VK.size = size, VK.stageFlags = stageFlags}||]

createShader :: String -> String -> Q Exp
createShader type' source = do
  let st = case type' of
        "vert" -> VK.SHADER_STAGE_VERTEX_BIT
        "frag" -> VK.SHADER_STAGE_FRAGMENT_BIT
        _ -> error $ "Unsupported shader type: " ++ type'
  sourceLocation <- location
  locations <- findLocations source
  pushConstants <- findPushConstants source
  let pushConstants' = map (\(VK.PushConstantRange {VK.size = size}, _) -> VK.PushConstantRange {VK.offset = 0, VK.size = size, VK.stageFlags = st}) pushConstants
  let pcMap = join $ map snd pushConstants
  (warnings, result) <- compileShader (Just sourceLocation) Nothing type' source
  case result of
    Left err -> fail $ show err
    Right compiledSource -> do
      forM_ warnings $ \msg -> do
        reportWarning $ "Shader compilation warning: " ++ msg
      [|
        ShaderDescription
          { _shaderName = Nothing,
            _shaderSource = compiledSource,
            _shaderType = st,
            _shaderNameResolve = lookup ?? locations,
            _shaderPushConstants = pushConstants',
            _shaderMainFunction = Nothing,
            _shaderPushConstantMap = pcMap
          }
        |]

glslQuoter :: String -> QuasiQuoter
glslQuoter type' =
  QuasiQuoter
    { quoteExp = createShader type',
      quotePat = error "Pattern quoting not supported for GLSL",
      quoteType = error "Type quoting not supported for GLSL",
      quoteDec = error "Declaration quoting not supported for GLSL"
    }

vert :: QuasiQuoter
vert = glslQuoter "vert"

frag :: QuasiQuoter
frag = glslQuoter "frag"