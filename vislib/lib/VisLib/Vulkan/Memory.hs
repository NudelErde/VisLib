{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module VisLib.Vulkan.Memory
  ( MemoryState,
    MemoryBinding (..),
    createMemoryState,
    createMemory,
    bindBuffer,
    writeDataFrom,
    writeDataPtr,
    writeDataBS,
    writeDataArray,
    bindImage,
  )
where

import Control.Lens
import Control.Monad (guard, when)
import Control.Monad.State
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (find, sortOn)
import qualified Data.Vector as V
import Data.Word
import Foreign (Ptr, Storable (..), castPtr, copyBytes, withArray)
import VisLib.App
import VisLib.Vulkan.Vulkan
import qualified Vulkan as VK
import qualified Vulkan.CStruct.Extends as VK
import Vulkan.Zero

data MemoryArea = MemoryArea
  { _memorySize :: Int,
    _memoryUsedSize :: Int,
    _memoryDeviceMemory :: VK.DeviceMemory,
    _memoryType :: VK.MemoryType,
    _memoryIndex :: Int
  }
  deriving (Show)

data BindingTarget = Buffer VK.Buffer | Image VK.Image
  deriving (Show, Eq)

data MemoryBinding = MemoryBinding
  { _memoryAreaIndex :: Int,
    _memoryOffset :: Int,
    _memoryBindingSize :: Int,
    _memoryBindingBuffer :: BindingTarget
  }
  deriving (Show)

data StagingInfo = StagingInfo
  { _stagingBuffer :: VK.Buffer,
    _stagingMemory :: MemoryBinding
  }
  deriving (Show)

data MemoryState = MemoryState
  { _memoryBuffers :: [MemoryArea],
    _device :: VK.Device,
    _physicalDevice :: VK.PhysicalDevice,
    _stagingQueue :: VK.Queue,
    _stagingCommandPool :: VK.CommandPool,
    _stagingBuffers :: [StagingInfo],
    _stagingFamilyIndex :: Word32
  }
  deriving (Show)

$(makeLenses ''MemoryState)
$(makeLenses ''MemoryArea)
$(makeLenses ''MemoryBinding)
$(makeLenses ''StagingInfo)

createMemoryState :: (MonadIO io) => VK.Device -> VK.PhysicalDevice -> VK.Queue -> Int -> AppMonad io d r MemoryState
createMemoryState device' physicalDevice' stagingQueue' stagingFamily = do
  let allocInfo =
        (zero :: VK.CommandPoolCreateInfo)
          { VK.queueFamilyIndex = fromIntegral stagingFamily,
            VK.flags = VK.COMMAND_POOL_CREATE_TRANSIENT_BIT
          }
  sCommandPool <- lift $ VK.createCommandPool device' allocInfo Nothing
  let ms =
        MemoryState
          { _memoryBuffers = [],
            _device = device',
            _physicalDevice = physicalDevice',
            _stagingQueue = stagingQueue',
            _stagingCommandPool = sCommandPool,
            _stagingBuffers = [],
            _stagingFamilyIndex = fromIntegral stagingFamily
          }
  return ms

updateM :: (Monad m) => (s -> m s) -> StateT s m ()
updateM f = do
  s <- get
  s' <- lift $ f s
  put s'
  return ()

createStagingBuffer :: (MonadIO io) => Int -> MemoryState -> AppMonad io d r (StagingInfo, MemoryState)
createStagingBuffer size = runStateT $ do
  device' <- use device
  sb <- lift $ createBuffer device' VK.BUFFER_USAGE_TRANSFER_SRC_BIT size
  binding <- StateT $ bindBuffer VK.MEMORY_PROPERTY_HOST_VISIBLE_BIT sb
  let newStagingBuffer = StagingInfo {_stagingBuffer = sb, _stagingMemory = binding}
  stagingBuffers <>= [newStagingBuffer]
  stagingBuffers %= sortOn (^. stagingMemory . memoryBindingSize)
  return newStagingBuffer

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

getNewAllocationSize :: (MonadIO io) => AppMonad io d r Int
getNewAllocationSize = return $ 1024 * 1024

getStagingBufferSize :: Int -> Int
getStagingBufferSize size = (1024 * 1024) * ceiling (fromIntegral size / (1024 * 1024) :: Double)

bindStuff :: (MonadIO io) => VK.MemoryPropertyFlags -> BindingTarget -> MemoryState -> AppMonad io d r (MemoryBinding, MemoryState)
bindStuff memoryPropertyFlags target = runStateT $ do
  memoryAreas <- use memoryBuffers
  device' <- use device
  VK.MemoryRequirements
    { VK.memoryTypeBits = memReqBits,
      VK.size = allocSize,
      VK.alignment = alignment
    } <- case target of
      Buffer buffer -> VK.getBufferMemoryRequirements device' buffer
      Image image -> VK.getImageMemoryRequirements device' image
  let memoryAreas' = filter ?? zip [0 ..] memoryAreas $ \(_, MemoryArea {..}) ->
        _memorySize - align (fromIntegral alignment) _memoryUsedSize >= fromIntegral allocSize
          && VK.propertyFlags _memoryType .&. memoryPropertyFlags == memoryPropertyFlags
          && (1 .<<. _memoryIndex) .&. memReqBits /= 0
  let memoryAreas'' = sortOn (\(_, x) -> _memorySize x - _memoryUsedSize x) memoryAreas'
  (idx, memoryArea) <- case memoryAreas'' of
    [] -> do
      allocationSize <- lift getNewAllocationSize
      index' <- StateT (createMemory memReqBits memoryPropertyFlags $ max allocationSize $ fromIntegral allocSize)
      buffer' <- gets (^?! memoryBuffers . ix index')
      return (index', buffer')
    b : _ -> return b
  case target of
    Buffer buffer -> do
      VK.bindBufferMemory device' buffer (_memoryDeviceMemory memoryArea) (fromIntegral $ align (fromIntegral alignment) $ _memoryUsedSize memoryArea)
    Image image -> do
      VK.bindImageMemory device' image (_memoryDeviceMemory memoryArea) (fromIntegral $ align (fromIntegral alignment) $ _memoryUsedSize memoryArea)
  memoryBuffers . ix idx . memoryUsedSize += fromIntegral allocSize
  return $ MemoryBinding idx (fromIntegral $ align (fromIntegral alignment) $ _memoryUsedSize memoryArea) (fromIntegral allocSize) target

bindBuffer :: (MonadIO io) => VK.MemoryPropertyFlags -> VK.Buffer -> MemoryState -> AppMonad io d r (MemoryBinding, MemoryState)
bindBuffer memoryPropertyFlags buffer = bindStuff memoryPropertyFlags (Buffer buffer)

bindImage :: (MonadIO io) => VK.MemoryPropertyFlags -> VK.Image -> MemoryState -> AppMonad io d r (MemoryBinding, MemoryState)
bindImage memoryPropertyFlags image = bindStuff memoryPropertyFlags (Image image)

writeStagingBufferToBuffer :: (MonadIO io) => VK.Buffer -> VK.CommandBuffer -> VK.Buffer -> Int -> io ()
writeStagingBufferToBuffer dstBuffer commandBuffer srcBuffer size =
  VK.cmdCopyBuffer
    commandBuffer
    srcBuffer
    dstBuffer
    [VK.BufferCopy 0 0 (fromIntegral size)]

writeStagingBufferToImage :: (MonadIO io) => VK.Image -> VK.CommandBuffer -> VK.Buffer -> Int -> io ()
writeStagingBufferToImage image commandBuffer buffer size = return ()

writeUsingStagingBuffer :: (MonadIO io) => (VK.CommandBuffer -> VK.Buffer -> Int -> IO ()) -> (Ptr () -> IO ()) -> Int -> MemoryState -> AppMonad io d r MemoryState
writeUsingStagingBuffer bufferCopy cp size = execStateT $ do
  sbs <- use stagingBuffers
  let stagingBuf = find (\sb -> sb ^. stagingMemory . memoryBindingSize >= size) sbs
  stagingBuf' <- case stagingBuf of
    Just sb -> return sb
    Nothing -> StateT $ createStagingBuffer $ getStagingBufferSize size
  updateM $ writeDataFrom (stagingBuf' ^. stagingMemory) cp size
  device' <- use device
  stagingCommandPool' <- use stagingCommandPool
  sq <- use stagingQueue
  lift $ scope $ do
    commandBuffer <- createCommandBuffer device' stagingCommandPool'
    let beginInfo =
          (zero :: VK.CommandBufferBeginInfo '[])
            { VK.flags = VK.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
            }
    VK.useCommandBuffer commandBuffer beginInfo $ do
      liftIO $ bufferCopy commandBuffer (stagingBuf' ^. stagingBuffer) size
    let submitInfo =
          VK.SomeStruct
            (zero :: VK.SubmitInfo '[])
              { VK.commandBuffers = [VK.commandBufferHandle commandBuffer],
                VK.waitSemaphores = [],
                VK.signalSemaphores = []
              }
    VK.queueSubmit sq [submitInfo] VK.NULL_HANDLE
    VK.queueWaitIdle sq
    return ()

writeDataFrom :: (MonadIO io) => MemoryBinding -> (Ptr () -> IO ()) -> Int -> MemoryState -> AppMonad io d r MemoryState
writeDataFrom MemoryBinding {..} cp size memState = do
  when (size > _memoryBindingSize) $ error "Buffer size is too small"
  let device' = memState ^. device
  let MemoryArea {..} = memState ^?! memoryBuffers . ix _memoryAreaIndex
  if VK.propertyFlags _memoryType .&. VK.MEMORY_PROPERTY_HOST_VISIBLE_BIT /= zero
    then scope $ do
      ptr <- VK.withMappedMemory device' _memoryDeviceMemory (fromIntegral _memoryOffset) (fromIntegral size) zero resource
      liftIO $ cp ptr
      when (VK.propertyFlags _memoryType .&. VK.MEMORY_PROPERTY_HOST_COHERENT_BIT == zero) $ do
        lift $
          VK.invalidateMappedMemoryRanges device' $
            V.fromList
              [ VK.MappedMemoryRange
                  { VK.memory = _memoryDeviceMemory,
                    VK.offset = fromIntegral _memoryOffset,
                    VK.size = fromIntegral size
                  }
              ]
      return memState
    else case _memoryBindingBuffer of
      Buffer buffer -> writeUsingStagingBuffer (writeStagingBufferToBuffer buffer) cp size memState
      Image image -> writeUsingStagingBuffer (writeStagingBufferToImage image) cp size memState

writeDataPtr :: (MonadIO io) => MemoryBinding -> Ptr () -> Int -> MemoryState -> AppMonad io d r MemoryState
writeDataPtr binding ptr size = writeDataFrom binding (\destPtr -> copyBytes destPtr ptr size) size

writeDataBS :: (MonadIO io) => MemoryBinding -> ByteString -> MemoryState -> AppMonad io d r MemoryState
writeDataBS binding bs memState = do
  let size = BS.length bs
  let cp destPtr = BS.useAsCStringLen bs $ uncurry (copyBytes (castPtr destPtr))
  writeDataFrom binding cp size memState

writeDataArray :: (MonadIO io, Storable a) => MemoryBinding -> [a] -> MemoryState -> AppMonad io d r MemoryState
writeDataArray binding xs memState = do
  let size = sum $ sizeOf <$> xs
  let cp destPtr = withArray xs $ \srcPtr -> copyBytes (castPtr destPtr) srcPtr size
  writeDataFrom binding cp size memState

align :: Int -> Int -> Int
align alignment size =
  let alignedSize = size + (alignment - 1)
   in alignedSize - (alignedSize `mod` alignment)