{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GADTs #-}

module VisLib.Base (module VisLib.Base, module VisLib.Shader.ShaderTypes) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.IORef
import qualified Graphics.Rendering.OpenGL.GL as GL
import VisLib.Shader.ShaderTypes
import Data.Array.Storable (StorableArray)
import Foreign
import Data.Data

type ErrorType = String

type ComputationIO a = ExceptT ErrorType IO a

type Computation a = Except ErrorType a

data UniformInfo = UniformInfo [Type] [String]

data Shader = GLShader GL.Program GL.Shader GL.Shader (Maybe UniformInfo)

data BufferAttribute = BufferAttribute {-Type-} Int Int

data BufferLayout = ArrayOfStruct | StructOfArray

data BufferDescription = BufferDescription [BufferAttribute] BufferLayout

data Buffer = GLBuffer
  { _bufferVao :: GL.VertexArrayObject,
    _bufferVbo :: GL.BufferObject,
    _bufferIbo :: GL.BufferObject,
    _bufferSize :: IORef Int,
    _indexCount :: IORef Int,
    _bufferDescription :: BufferDescription
  }

$(makeLenses ''Buffer)

data TextureSpecification = TextureSpecification GL.PixelInternalFormat (Int, Int) deriving (Eq)

data Image where
  Image :: (Typeable a, Storable a) => (Int, Int) -> GL.PixelFormat -> StorableArray Int a -> Image

data Texture = GLTexture GL.TextureObject (IORef (Maybe TextureSpecification))

liftComputationIO :: Computation a -> ComputationIO a
liftComputationIO = ExceptT . liftIO . return . runExcept
