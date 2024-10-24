{-# LANGUAGE TemplateHaskellQuotes #-}
module VisLib.Embed where

import Language.Haskell.TH.Syntax

import qualified Data.ByteString as BS

embed :: FilePath -> Q Exp
embed path = do
  path' <- makeRelativeToProject path
  contents <- runIO $ readFile path'
  [|contents|]

embedBS :: FilePath -> Q Exp
embedBS path = do
  path' <- makeRelativeToProject path
  contents <- runIO $ BS.readFile path'
  [|contents|]