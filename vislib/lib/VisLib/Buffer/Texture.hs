{-# OPTIONS_GHC -Wno-orphans #-}
module VisLib.Buffer.Texture(createTexture, writeTexture', writeTexture) where

import qualified Graphics.Rendering.OpenGL.GL as GL
import VisLib.Base
import Foreign
import Data.Typeable
import Control.Monad.IO.Class
import Data.IORef
import Data.StateVar (($=))
import Control.Monad.Except
import VisLib.Shader.GL
import Control.Monad.Trans.State

instance ShaderTypeable Texture where
  getType _ = Sampler Sampler2D

instance UniformValue Texture where
  bindUniform loc (GLTexture texture _) = do
    freeTextureUnit <- gets fromIntegral
    modify' (+1)
    GL.activeTexture $= GL.TextureUnit freeTextureUnit
    GL.textureBinding GL.Texture2D $= Just texture
    GL.uniform loc $= GL.TextureUnit freeTextureUnit

createTexture :: ComputationIO Texture
createTexture = do
  texture <- liftIO GL.genObjectName
  specs <- liftIO $ newIORef Nothing
  return $ GLTexture texture specs

writeTexture' :: Storable a => Texture -> TextureSpecification -> GL.PixelFormat -> GL.DataType -> Ptr a -> IO ()
writeTexture' (GLTexture texture specs) specs'@(TextureSpecification internalFormat (width, height)) format dataType ptr = do
  GL.textureBinding GL.Texture2D $= Just texture
  specs'' <- readIORef specs
  if specs'' == Just specs'
    then GL.texSubImage2D GL.Texture2D 0 (GL.TexturePosition2D 0 0) (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) (GL.PixelData format dataType ptr)
    else GL.texImage2D GL.Texture2D GL.NoProxy 0 internalFormat (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 0 (GL.PixelData format dataType ptr)
  GL.textureBinding GL.Texture2D $= Nothing

getGLType :: Typeable a => a -> Computation GL.DataType
getGLType = getGLType' . typeOf

getGLType' :: TypeRep -> Computation GL.DataType
getGLType' rep | rep == typeRep (Proxy :: Proxy Float) = return GL.Float
               | rep == typeRep (Proxy :: Proxy Double) = return GL.Double
               | rep == typeRep (Proxy :: Proxy Int) = return GL.Int
               | rep == typeRep (Proxy :: Proxy Word8) = return GL.UnsignedByte
               | rep == typeRep (Proxy :: Proxy Word16) = return GL.UnsignedShort
               | rep == typeRep (Proxy :: Proxy Word32) = return GL.UnsignedInt
               | rep == typeRep (Proxy :: Proxy Int8) = return GL.Byte
               | rep == typeRep (Proxy :: Proxy Int16) = return GL.Short
               | rep == typeRep (Proxy :: Proxy Int32) = return GL.Int
               | otherwise = throwError "Unsupported type"

writeTexture :: (Typeable a, Storable a) => Texture -> TextureSpecification -> GL.PixelFormat -> [a] -> ComputationIO ()
writeTexture texture specs format data' = do
  dataType <- liftComputationIO $ getGLType (head data')
  liftIO $ withArray data' $ writeTexture' texture specs format dataType
