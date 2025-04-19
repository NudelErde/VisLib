{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module VisLib.Vulkan.Memory (MemoryState, MemoryBinding(..), createMemoryState, createMemory, bindBuffer, writeBufferFrom, writeBufferPtr, writeBufferBS, writeBufferArray) where

import Control.Lens
import Control.Monad (guard, when)
import Control.Monad.State
import Data.Bits
import Data.List (sortOn)
import qualified Data.Vector as V
import Data.Word
import VisLib.App
import qualified Vulkan as VK
import Vulkan.Zero
import Foreign (Ptr, copyBytes, castPtr, Storable (..), withArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

data MemoryArea = MemoryArea
  { _memorySize :: Int,
    _memoryUsedSize :: Int,
    _memoryDeviceMemory :: VK.DeviceMemory,
    _memoryType :: VK.MemoryType,
    _memoryIndex :: Int
  }

data MemoryBinding = MemoryBinding
  { _memoryAreaIndex :: Int,
    _memoryOffset :: Int,
    _memoryBindingSize :: Int,
    _memoryBindingBuffer :: VK.Buffer
  }

data MemoryState = MemoryState
  { _memoryBuffers :: [MemoryArea],
    _device :: VK.Device,
    _physicalDevice :: VK.PhysicalDevice,
    _stagingQueue :: VK.Queue
  }

$(makeLenses ''MemoryState)
$(makeLenses ''MemoryArea)

createMemoryState :: (MonadIO io) => VK.Device -> VK.PhysicalDevice -> VK.Queue -> AppMonad io d r MemoryState
createMemoryState device' physicalDevice' stagingQueue' = do
  return MemoryState
          { _memoryBuffers = [],
            _device = device',
            _physicalDevice = physicalDevice',
            _stagingQueue = stagingQueue'
          }

createMemory :: (MonadIO io) => Word32 -> VK.MemoryPropertyFlags -> Int -> MemoryState -> AppMonad io d r (Int, MemoryState)
createMemory memoryTypeBits memoryPropertyFlags size = runStateT $ do
  pdevice <- use physicalDevice
  device' <- use device
  memProperties <- VK.getPhysicalDeviceMemoryProperties pdevice
  let validMemory = do
        (i, memType@VK.MemoryType {VK.propertyFlags = flags}) <- zip [0 :: Int ..] $ V.toList $ VK.memoryTypes memProperties
        guard $ memoryTypeBits .&. (1 .<<. i) /= 0
        guard $ flags .&. memoryPropertyFlags == memoryPropertyFlags
        return (i, memType)
  let (i, memType) = head validMemory
  currentIndex <- length <$> use memoryBuffers
  let allocInfo =
        (zero :: VK.MemoryAllocateInfo '[])
          { VK.allocationSize = fromIntegral size,
            VK.memoryTypeIndex = fromIntegral i
          }
  memory <- lift $ VK.withMemory device' allocInfo Nothing $ resourceN ("buffer memory (" ++ show currentIndex ++ ")")
  memoryBuffers <>= [MemoryArea size 0 memory memType i]
  return currentIndex

getNewAllocationSize :: (MonadIO io) => MemoryState -> AppMonad io d r Int
getNewAllocationSize _memoryState = return $ 1024 * 1024

bindBuffer :: (MonadIO io) => VK.MemoryPropertyFlags -> VK.Buffer -> MemoryState -> AppMonad io d r (MemoryBinding, MemoryState)
bindBuffer memoryPropertyFlags buffer = runStateT $ do
  memoryAreas <- use memoryBuffers
  device' <- use device
  VK.MemoryRequirements
    { VK.memoryTypeBits = memReqBits,
      VK.size = allocSize
    } <-
    VK.getBufferMemoryRequirements device' buffer
  let memoryAreas' = filter ?? zip [0 ..] memoryAreas $ \(_, MemoryArea {..}) ->
        _memorySize - _memoryUsedSize >= fromIntegral allocSize
          && VK.propertyFlags _memoryType .&. memoryPropertyFlags == memoryPropertyFlags
          && (1 .<<. _memoryIndex) .&. memReqBits /= 0
  let memoryAreas'' = sortOn (\(_, x) -> _memorySize x - _memoryUsedSize x) memoryAreas'
  (idx, memoryArea) <- case memoryAreas'' of
    [] -> do
      allocationSize <- lift . getNewAllocationSize =<< get
      index' <- StateT (createMemory memReqBits memoryPropertyFlags allocationSize)
      buffer' <- gets (^?! memoryBuffers . ix index')
      return (index', buffer')
    b : _ -> return b
  VK.bindBufferMemory device' buffer (_memoryDeviceMemory memoryArea) (fromIntegral $ _memorySize memoryArea)
  memoryBuffers . ix idx . memoryUsedSize += fromIntegral allocSize
  return $ MemoryBinding idx (fromIntegral $ _memoryUsedSize memoryArea) (fromIntegral allocSize) buffer

writeBufferFrom  :: (MonadIO io) => MemoryBinding -> (Ptr () -> IO ()) -> Int -> MemoryState -> AppMonad io d r ()
writeBufferFrom MemoryBinding{..} cp size memState = scope $ do
  when (size > _memoryBindingSize) $ error "Buffer size is too small"
  let device' = memState ^. device
  let MemoryArea{..} = memState ^?! memoryBuffers . ix _memoryAreaIndex
  if VK.propertyFlags _memoryType .&. VK.MEMORY_PROPERTY_HOST_VISIBLE_BIT /= zero then do
    ptr <- VK.withMappedMemory device' _memoryDeviceMemory (fromIntegral _memoryOffset) (fromIntegral size) zero resource
    liftIO $ cp ptr
    when (VK.propertyFlags _memoryType .&. VK.MEMORY_PROPERTY_HOST_COHERENT_BIT == zero) $ do
      lift $ VK.invalidateMappedMemoryRanges device' $ V.fromList [VK.MappedMemoryRange{
        VK.memory = _memoryDeviceMemory,
        VK.offset = fromIntegral _memoryOffset,
        VK.size = fromIntegral size
      }]
  else
    error "Buffer is not host visible (TODO STAGING BUFFER)"


writeBufferPtr :: (MonadIO io) => MemoryBinding -> Ptr () -> Int -> MemoryState -> AppMonad io d r ()
writeBufferPtr binding ptr size = writeBufferFrom binding (\destPtr -> copyBytes destPtr ptr size) size

writeBufferBS :: (MonadIO io) => MemoryBinding -> ByteString -> MemoryState -> AppMonad io d r ()
writeBufferBS binding bs memState = do
  let size = BS.length bs
  let cp destPtr = BS.useAsCStringLen bs $ uncurry (copyBytes (castPtr destPtr))
  writeBufferFrom binding cp size memState

writeBufferArray :: (MonadIO io, Storable a) => MemoryBinding -> [a] -> MemoryState -> AppMonad io d r ()
writeBufferArray binding xs memState = do
  let size = sum $ sizeOf <$> xs
  let cp destPtr = withArray xs $ \srcPtr -> copyBytes (castPtr destPtr) srcPtr size
  writeBufferFrom binding cp size memState
