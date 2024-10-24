{-# LANGUAGE TemplateHaskellQuotes #-}
module Load where


import qualified Data.ByteString as BS
import Data.Functor
import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL
import VisLib.Base
import VisLib.Buffer.Image
import VisLib.Buffer.ObjectFile (parseObjectFile', parseObjectFile)
import VisLib.Buffer.Texture
import VisLib.Buffer.VertexBuffer
import VisLib.Embed
import Language.Haskell.TH.Syntax (Exp)
import Language.Haskell.TH (Q)

readObj :: FilePath -> ComputationIO Buffer
readObj path = do
  (description, vertices, indices) <- parseObjectFile' path
  buffer <- createBuffer description
  bufferData buffer $= (vertices, indices <&> fromIntegral)
  return buffer

readObj' :: String -> ComputationIO Buffer
readObj' content = do
  (description, vertices, indices) <- liftComputationIO $ parseObjectFile content
  buffer <- createBuffer description
  bufferData buffer $= (vertices, indices <&> fromIntegral)
  return buffer

readObjEmbedded :: FilePath -> Q Exp
readObjEmbedded path = do
  let content = embed path
  [e|readObj' $content|]

readTex :: FilePath -> ComputationIO Texture
readTex path = do
  img <- parseImage' path
  tex <- createTexture
  writeTexture' tex GL.RGBA' img
  return tex

readTex' :: BS.ByteString -> ImageType -> ComputationIO Texture
readTex' content imgType = do
  img <- parseImage imgType content
  tex <- createTexture
  writeTexture' tex GL.RGBA' img
  return tex

readTexEmbedded :: FilePath -> Q Exp
readTexEmbedded path = do
  let content = embedBS path
  let imgType = guessImageType path
  [e|readTex' $content imgType|]
