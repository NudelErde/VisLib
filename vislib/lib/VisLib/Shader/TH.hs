  {-# LANGUAGE TemplateHaskell #-}

  module VisLib.Shader.TH where

  import Data.Char (isAlphaNum)
  import Language.Haskell.TH.Lib
  import Language.Haskell.TH.Quote
  import Language.Haskell.TH.Syntax

  parseShaderString :: String -> [String]
  parseShaderString str =
    let predicat c = isAlphaNum c || c == '_' || c == '$'
    in case break (== '$') str of
          (a, "") -> [a]
          (a, b) -> a : takeWhile predicat b : parseShaderString (dropWhile predicat b)

  convertShaderString :: [String] -> Q Exp
  convertShaderString strs = do
    let strs' = map convertShaderString' strs
    expr <- foldl (\a b -> [|$a ++ $b|]) [|""|] strs'
    return $ ParensE expr

  convertShaderString' :: String -> Q Exp
  convertShaderString' ('$' : var) = do
    let var' = varE $ mkName var
    [|show $(var')|]
  convertShaderString' str = return $ LitE $ StringL str

  shader :: QuasiQuoter
  shader =
    QuasiQuoter
      { quoteExp = \str -> do
          let strs = parseShaderString str
          convertShaderString strs,
        quotePat = undefined,
        quoteType = undefined,
        quoteDec = undefined
      }