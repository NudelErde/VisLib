{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module VisLib.Vulkan.Model where

import VisLib.Loader.GLTF
import VisLib.Vulkan.Memory
import VisLib.Vulkan.Vulkan
import VisLib.App
import qualified Vulkan as VK
import qualified Vulkan.CStruct.Extends as VK
import qualified Vulkan.Exception as VK
import qualified Data.Vector as V
import Data.Vector (Vector)
import Vulkan.CStruct.Extends (pattern (:&), pattern(::&))
import Vulkan.Zero
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Maybe
import Data.List
import Data.Bits
import qualified Data.ByteString as BS

getBufferData :: MonadIO io => GLTF -> [ByteString] -> Int -> io (Maybe ByteString)
getBufferData gltf glbBuffers bufferIndex = do
  case bufferUri (buffers gltf !! bufferIndex) of
    Just _uri -> do
      error "uri loading not implemented"
    Nothing -> do
      return $ glbBuffers !? bufferIndex

createBufferFromGLTF :: MonadIO io => Int -> GLTF -> [ByteString] -> VK.Device -> ResourceMutable MemoryState -> AppMonad io d r (VK.Buffer, MemoryBinding)
createBufferFromGLTF bufferViewIndex gltf glbBuffers device memoryAllocator = do
  let bufferView = bufferViews gltf !! bufferViewIndex
  let bufferIndex = fromJust (buffer bufferView)
  bufferData <- fromJust <$> getBufferData gltf glbBuffers bufferIndex
  let usage = case target bufferView of
        Just 34962 -> VK.BUFFER_USAGE_VERTEX_BUFFER_BIT
        Just 34963 -> VK.BUFFER_USAGE_INDEX_BUFFER_BIT
        _ -> VK.BUFFER_USAGE_VERTEX_BUFFER_BIT .|. VK.BUFFER_USAGE_INDEX_BUFFER_BIT
  buffer <- createBuffer device usage (fromJust (viewByteLength bufferView))
  memoryAllocator <- getResource memoryAllocator
  (binding, memoryAllocator') <- bindBuffer VK.MEMORY_PROPERTY_HOST_VISIBLE_BIT buffer memoryAllocator
  let data' = BS.drop (fromMaybe 0 (viewByteOffset bufferView)) bufferData
  let data'' = BS.take (fromJust (viewByteLength bufferView)) data'
  writeBufferBS binding data'' memoryAllocator'

  return (buffer, binding)