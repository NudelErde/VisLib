{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module VisLib.Vulkan.ShaderTH where

import Vulkan.Utils.ShaderQQ.GLSL.Shaderc
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import VisLib.Vulkan.Shader
import VisLib.Parse

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Control.Applicative hiding (some, many)
import Data.Functor
import qualified Vulkan as VK
import Data.Maybe
import Control.Lens.Operators ((??))

lexeme :: Parser String a -> Parser String a
lexeme = L.lexeme (L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/"))

parseLocation :: Parser String ShaderLocationInformation
parseLocation = do
  void $ lexeme $ string "layout"
  void $ lexeme $ char '('
  void $ lexeme $ string "location"
  void $ lexeme $ char '='
  index <- lexeme L.decimal
  void $ lexeme $ char ')'
  void $ lexeme $ asum [string "in "]
  type' <- lexeme $ asum [string "vec2 " $> VK.FORMAT_R32G32_SFLOAT
                         ,string "vec3 " $> VK.FORMAT_R32G32B32_SFLOAT
                         ,string "vec4 " $> VK.FORMAT_R32G32B32A32_SFLOAT]
  name <- lexeme $ manyTill anySingle (try (lexeme $ char ';'))
  return $ ShaderLocationInformation (Just name) index type'

parseStartOfLine :: Show a => Parser String a -> Parser String [a]
parseStartOfLine p = do
  lines' <- manyTill ?? eof $ do
    void $ many space1
    m <- optional (try p)
    skipManyTill anySingle (void eol <|> eof)
    return m
  return $ catMaybes lines'

findLocations :: String -> Q [(String, ShaderLocationInformation)]
findLocations source = do
  locations <- case runParser (parseStartOfLine parseLocation) "" source of
      Left err -> fail $ errorBundlePretty err
      Right x -> return x
  return $ map (\sli@ShaderLocationInformation{_shaderLocationName=name} -> (fromJust name, sli)) locations

instance Lift VK.ShaderStageFlagBits where
  lift VK.SHADER_STAGE_VERTEX_BIT = [| VK.SHADER_STAGE_VERTEX_BIT |]
  lift VK.SHADER_STAGE_FRAGMENT_BIT = [| VK.SHADER_STAGE_FRAGMENT_BIT |]
  lift _ = error "Unsupported shader stage"
  liftTyped VK.SHADER_STAGE_VERTEX_BIT = [|| VK.SHADER_STAGE_VERTEX_BIT ||]
  liftTyped VK.SHADER_STAGE_FRAGMENT_BIT = [|| VK.SHADER_STAGE_FRAGMENT_BIT ||]
  liftTyped _ = error "Unsupported shader stage"

createShader :: String -> String -> Q Exp
createShader type' source = do
  let st = case type' of
        "vert" -> VK.SHADER_STAGE_VERTEX_BIT
        "frag" -> VK.SHADER_STAGE_FRAGMENT_BIT
        _ -> error $ "Unsupported shader type: " ++ type'
  sourceLocation <- location
  locations <- findLocations source
  (warnings, result) <- compileShader (Just sourceLocation) Nothing type' source
  case result of
    Left err -> fail $ show err
    Right compiledSource -> do
      forM_ warnings $ \msg -> do
        reportWarning $ "Shader compilation warning: " ++ msg
      [|ShaderDescription{_shaderName=Nothing,
                          _shaderSource=compiledSource,
                          _shaderType=st,
                          _shaderNameResolve=lookup ?? locations,
                          _shaderPushConstants=[],
                          _shaderMainFunction=Nothing}|]

glslQuoter :: String -> QuasiQuoter
glslQuoter type' = QuasiQuoter
  { quoteExp = createShader type',
    quotePat = error "Pattern quoting not supported for GLSL",
    quoteType = error "Type quoting not supported for GLSL",
    quoteDec = error "Declaration quoting not supported for GLSL"
  }

vert :: QuasiQuoter
vert = glslQuoter "vert"

frag :: QuasiQuoter
frag = glslQuoter "frag"