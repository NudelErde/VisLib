module VisLib.Buffer.GL (createBuffer, bindBuffer, writeVertices, writeIndices, writeData, bufferVertices, bufferIndices, bufferData, drawBuffer) where

import Control.Monad (forM_, when)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.StateVar
import Foreign (nullPtr, plusPtr, withArray)
import Foreign.Storable
import qualified Graphics.Rendering.OpenGL.GL as GL
import VisLib.Base

bindAttribs :: Buffer -> ComputationIO ()
bindAttribs b@(GLBuffer _ _ _ _ _ (BufferDescription attributes ArrayOfStruct)) = do
  bindBuffer b
  let stride = fromIntegral $ sum $ map (\(BufferAttribute count rep) -> count * 4 * rep) attributes
  let offsets = scanl plusPtr nullPtr $ map (\(BufferAttribute count rep) -> count * 4 * rep) attributes
  --let descriptors = zipWith (\(BufferAttribute count repeat) offset -> GL.VertexArrayDescriptor (fromIntegral count) GL.Float stride offset) attributes offsets
  let locations = scanl (+) 0 $ map (\(BufferAttribute _ rep) -> rep) attributes
  forM_ (zip3 locations attributes offsets) $ \(i, BufferAttribute count rep, offset) -> do
    forM_ [0..rep-1] $ \j -> do
      let descriptor = GL.VertexArrayDescriptor (fromIntegral count) GL.Float stride (offset `plusPtr` (j * count * 4))
      let loc = GL.AttribLocation $ fromIntegral $ i + j
      GL.vertexAttribArray loc $= GL.Enabled
      GL.vertexAttribPointer loc $= (GL.ToFloat, descriptor)
bindAttribs (GLBuffer _ _ _ _ _ (BufferDescription _ StructOfArray)) = do
  throwError "Creating Buffer using \"Struct\" layout is not supported yet."

createBuffer :: BufferDescription -> ComputationIO Buffer
createBuffer description = do
  vbo <- liftIO GL.genObjectName
  ibo <- liftIO GL.genObjectName
  vao <- liftIO GL.genObjectName

  bufSize <- liftIO $ newIORef 0
  count <- liftIO $ newIORef 0

  let buffer = GLBuffer vao vbo ibo bufSize count description
  bindAttribs buffer
  return buffer

bindBuffer :: Buffer -> ComputationIO ()
bindBuffer (GLBuffer vao vbo ibo _ _ _) = do
  GL.bindVertexArrayObject $= Just vao
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  GL.bindBuffer GL.ElementArrayBuffer $= Just ibo
  return ()

storableArraySize :: (Storable a, Num b) => [a] -> b
storableArraySize = fromIntegral . ((*) <$> sizeOf . head <*> length)

writeVertices :: (Storable a) => Buffer -> [a] -> ComputationIO ()
writeVertices (GLBuffer _ vbo _ bufSize _ _) d = liftIO $ withArray d $ \ptr -> do
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  let size' = storableArraySize d
  size <- readIORef bufSize
  if size' == size
    then GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer 0 (fromIntegral size) ptr
    else GL.bufferData GL.ArrayBuffer $= (fromIntegral size', ptr, GL.StaticDraw)
  when (size' /= size) $ writeIORef bufSize size'

bufferVertices :: (Storable a) => Buffer -> SettableStateVar [a]
bufferVertices b = makeSettableStateVar $ modifyError error . writeVertices b

writeIndices :: Buffer -> [GL.GLuint] -> ComputationIO ()
writeIndices (GLBuffer _ _ ibo _ idxCount _) idx = liftIO $ do
  count <- readIORef idxCount
  withArray idx $ \ptr -> do
    GL.bindBuffer GL.ElementArrayBuffer $= Just ibo
    if count == length idx
      then GL.bufferSubData GL.ElementArrayBuffer GL.WriteToBuffer 0 (storableArraySize idx) ptr
      else GL.bufferData GL.ElementArrayBuffer $= (storableArraySize idx, ptr, GL.StaticDraw)
  when (count /= length idx) $ writeIORef idxCount $ length idx

bufferIndices :: Buffer -> SettableStateVar [GL.GLuint]
bufferIndices b = makeSettableStateVar $ modifyError error . writeIndices b

writeData :: (Storable a) => Buffer -> ([a], [GL.GLuint]) -> ComputationIO ()
writeData b (v, i) = writeVertices b v >> writeIndices b i

bufferData :: (Storable a) => Buffer -> SettableStateVar ([a], [GL.GLuint])
bufferData b = makeSettableStateVar $ modifyError error . writeData b

drawBuffer :: Buffer -> ComputationIO ()
drawBuffer b@(GLBuffer _ _ _ _ idxCount _) = do
  bindBuffer b
  count <- liftIO $ readIORef idxCount
  when (count == 0) $ throwError "No indices to draw"
  liftIO $ GL.drawElements GL.Triangles (fromIntegral count) GL.UnsignedInt nullPtr
