{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Load where

import qualified Data.ByteString as BS
import Data.Functor
import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Dec, Exp, runIO)
import System.Exit
import System.Process
import VisLib.Base
import VisLib.Buffer.Image
import VisLib.Buffer.ObjectFile
import VisLib.Buffer.Texture
import VisLib.Buffer.VertexBuffer
import VisLib.Embed
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad ((<=<))
import Data.Char (isSpace)

readObj :: FilePath -> ComputationIO Buffer
readObj p = do
  (description, vertices, indices) <- parseObjectFile p []
  buffer <- createBuffer description
  bufferData buffer $= (vertices, indices <&> fromIntegral)
  return buffer

readObj' :: String -> ComputationIO Buffer
readObj' cont = do
  (description, vertices, indices) <- liftComputationIO $ parseObjectFile cont
  buffer <- createBuffer description
  bufferData buffer $= (vertices, indices <&> fromIntegral)
  return buffer

readTex :: FilePath -> ComputationIO Texture
readTex p = do
  img <- parseImage' p
  tex <- createTexture
  writeTexture' tex GL.RGBA' img
  return tex

readTex' :: BS.ByteString -> ImageType -> ComputationIO Texture
readTex' cont imgType = do
  img <- parseImage imgType cont
  tex <- createTexture
  writeTexture' tex GL.RGBA' img
  return tex

embedTexture :: InnerEmbedder ()
embedTexture = do
  c <- contentBS
  p <- path
  case guessImageType p of
    Just imgType -> emitExp' [e|readTex' c imgType|] [t|ComputationIO Texture|]
    Nothing -> fail "Not an image file."

embedAssets :: String -> FilePath -> Q [Dec]
embedAssets = runEmbedder $ do
  whenExtension' ".obj" $ do
    c <- content
    emitExp' [e|readObj' c|] [t|ComputationIO Buffer|]
  whenFile' embedTexture
  whenFile' $ do
    c <- contentBS
    n <- name
    emitExprN' (n ++ "_raw") [e|c|] [t|BS.ByteString|]
  recursiveDirectory

type GitInfo = (String, String, String)

embedGitInfo :: Q Exp
embedGitInfo = do
  let procMap :: (ExitCode, String, String) -> ExceptT String Q String
      procMap (ExitSuccess, out, _) = return out
      procMap (ExitFailure n, _, err) = throwError $ show "ExitCode: " ++ show n ++ "\n" ++ err
      liftProc :: IO (ExitCode, String, String) -> ExceptT String Q String
      liftProc = procMap <=< lift . runIO
      strip :: String -> String
      strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  e <- runExceptT $ do
    hash <- liftProc $ readProcessWithExitCode "git" ["rev-parse", "HEAD"] ""
    branch <- liftProc $ readProcessWithExitCode "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
    commitMessage <- liftProc $ readProcessWithExitCode "git" ["log", "-1", "--pretty=%B"] ""
    let info = (strip hash, strip branch, strip commitMessage)
    return [e|info|]
  case e of
    Left err -> fail err
    Right v -> v
