{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module VisLib.Base (module VisLib.Base, module VisLib.Shader.ShaderTypes) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.IORef
import qualified Graphics.Rendering.OpenGL.GL as GL
import VisLib.Shader.ShaderTypes

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

liftComputationIO :: Computation a -> ComputationIO a
liftComputationIO = ExceptT . liftIO . return . runExcept
