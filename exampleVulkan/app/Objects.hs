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
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Objects where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign hiding (void)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import VisLib.App
import VisLib.Vulkan.Memory
import VisLib.Vulkan.Shader
import VisLib.Vulkan.Vulkan hiding (ShaderDescription)
import qualified Vulkan as VK
import qualified Vulkan.CStruct.Extends as VK
import Linear.V2
import Vulkan.Zero
import Data.List.Extra

data Window = Window
  { _windowHandle :: SDL.Window
  }
  deriving (Eq, Show)

data Instance = Instance
  { _instanceHandle :: VK.Instance
  }
  deriving (Eq, Show)

data PhysicalDevice = PhysicalDevice
  { _physicalDeviceHandle :: VK.PhysicalDevice
  }
  deriving (Eq, Show)

data Device = Device
  { _deviceHandle :: VK.Device,
    _deviceQueue :: Queues
  }
  deriving (Eq, Show)

data Surface = Surface
  { _surfaceHandle :: VK.SurfaceKHR
  }
  deriving (Eq, Show)

data SwapChain = SwapChain
  { _swapChainHandle :: ResourceMutable VK.SwapchainKHR,
    _swapChainInfo :: VK.SwapchainCreateInfoKHR '[],
    _swapChainImageViews :: ResourceMutable (Vector VK.ImageView)
  }

data Framebuffer = Framebuffer
  { _framebufferHandle :: ResourceMutable (Vector VK.Framebuffer)
  }

data RenderPass = RenderPass
  { _renderPassHandle :: VK.RenderPass
  }

data Pipeline = Pipeline
  { _pipelineHandle :: VK.Pipeline,
    _pipelineLayout :: VK.PipelineLayout
  }

data Buffer = Buffer
  { _bufferHandle :: VK.Buffer,
    _bufferMemory :: MemoryBinding
  }

data VulkanData = VulkanData
  { _vdWindow :: Window,
    _vdInstance :: Instance,
    _vdPhysicalDevice :: PhysicalDevice,
    _vdDevice :: Device,
    _vdSurface :: Surface,
    _vdSwapChain :: SwapChain,
    _vdFramebuffer :: Framebuffer,
    _vdRenderPass :: RenderPass,
    _vdMemoryAllocator :: ResourceMutable MemoryState
  }

data IndexBuffer = IndexBuffer
  { _indexBufferBuffer :: Buffer,
    _indexBufferCount :: Int,
    _indexBufferType :: VK.IndexType
  }

data Object = Object
  { _objectShaderInfo :: ShaderInfo,
    _objectPipeline :: Pipeline,
    _objectBuffers :: [Buffer],
    _objectDrawInformation :: Either IndexBuffer Int,
    _objectCommandBuffer :: Maybe VK.CommandBuffer
  }

$(makeLenses ''Window)
$(makeLenses ''Instance)
$(makeLenses ''PhysicalDevice)
$(makeLenses ''Device)
$(makeLenses ''Surface)
$(makeLenses ''SwapChain)
$(makeLenses ''Framebuffer)
$(makeLenses ''RenderPass)
$(makeLenses ''Pipeline)
$(makeLenses ''VulkanData)
$(makeLenses ''Buffer)
$(makeLenses ''Object)
$(makeLenses ''IndexBuffer)

cmdBindPipeline :: (MonadIO io) => VK.CommandBuffer -> Pipeline -> io ()
cmdBindPipeline commandBuffer pipeline = do
  VK.cmdBindPipeline commandBuffer VK.PIPELINE_BIND_POINT_GRAPHICS (pipeline ^. pipelineHandle)

cmdBindIndexBuffer :: (MonadIO io) => VK.CommandBuffer -> IndexBuffer -> io ()
cmdBindIndexBuffer commandBuffer indexBuffer = do
  VK.cmdBindIndexBuffer commandBuffer (indexBuffer ^. indexBufferBuffer . bufferHandle) 0 (indexBuffer ^. indexBufferType)

cmdSetPushConstant :: (MonadIO io, Storable a) => VK.CommandBuffer -> Pipeline -> VK.ShaderStageFlags -> ShaderPushConstantDescription -> a -> io ()
cmdSetPushConstant commandBuffer pipeline flags pushConstant value = do
  let size = fromIntegral $ pushConstant ^. shaderPushConstantSize
  when (size /= sizeOf value) $
    liftIO $ throwIO $ userError $ "Push constant size mismatch: expected " ++ show size ++ ", got " ++ show (sizeOf value)
  liftIO $ with value (VK.cmdPushConstants
        commandBuffer
        (pipeline ^. pipelineLayout)
        flags
        (fromIntegral $ pushConstant ^. shaderPushConstantOffset)
        (fromIntegral $ pushConstant ^. shaderPushConstantSize) . castPtr)

acquireNextImage :: (MonadIO io) => Device -> SwapChain -> VK.Semaphore -> io Word32
acquireNextImage device swapChain semaphore = do
  swapChainHandle' <- getResource (swapChain ^. swapChainHandle)
  (_, imageIndex) <- VK.acquireNextImageKHR (device ^. deviceHandle) swapChainHandle' maxBound semaphore VK.NULL_HANDLE
  return imageIndex

submit :: (MonadIO io) => Device -> VK.CommandBuffer -> VK.Semaphore -> VK.Semaphore -> VK.Fence -> io ()
submit device commandBuffer wait signal commandBufferFree = do
  let submitInfo =
        (zero :: VK.SubmitInfo '[])
          { VK.commandBuffers = [VK.commandBufferHandle commandBuffer],
            VK.waitSemaphores = [wait],
            VK.signalSemaphores = [signal],
            VK.waitDstStageMask = [VK.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
          }
  VK.queueSubmit (device ^. deviceQueue . graphicsQueue) [VK.SomeStruct submitInfo] commandBufferFree

present :: (MonadIO io) => Device -> SwapChain -> VK.Semaphore -> Word32 -> io ()
present device swapChain ready index = do
  swapChainHandle' <- getResource (swapChain ^. swapChainHandle)
  let presentInfo =
        (zero :: VK.PresentInfoKHR '[])
          { VK.swapchains = [swapChainHandle'],
            VK.imageIndices = [index],
            VK.waitSemaphores = [ready]
          }
  void $ VK.queuePresentKHR (device ^. deviceQueue . presentQueue) presentInfo

recreateSwapChain :: (MonadIO io) => AppMonad io VulkanData r ()
recreateSwapChain = do
  vd <- ask
  VK.deviceWaitIdle (vd ^. vdDevice . deviceHandle)
  destroyResource (vd ^. vdFramebuffer . framebufferHandle)
  destroyResource (vd ^. vdSwapChain . swapChainImageViews)
  destroyResource (vd ^. vdSwapChain . swapChainHandle)
  VK.SurfaceCapabilitiesKHR
    { VK.minImageCount = minImageCount,
      VK.currentTransform = transform,
      VK.minImageExtent = VK.Extent2D {VK.width = minWidth, VK.height = minHeight},
      VK.maxImageExtent = VK.Extent2D {VK.width = maxWidth, VK.height = maxHeight}
    } <-
    VK.getPhysicalDeviceSurfaceCapabilitiesKHR (vd ^. vdPhysicalDevice . physicalDeviceHandle) (vd ^. vdSurface . surfaceHandle)
  (_, formats) <- VK.getPhysicalDeviceSurfaceFormatsKHR (vd ^. vdPhysicalDevice . physicalDeviceHandle) (vd ^. vdSurface . surfaceHandle)
  (_, presentModes) <- VK.getPhysicalDeviceSurfacePresentModesKHR (vd ^. vdPhysicalDevice . physicalDeviceHandle) (vd ^. vdSurface . surfaceHandle)
  let VK.SurfaceFormatKHR {..} = head $ filter (\(VK.SurfaceFormatKHR {..}) -> format == VK.FORMAT_B8G8R8A8_SRGB && colorSpace == VK.COLOR_SPACE_SRGB_NONLINEAR_KHR) $ V.toList formats
  let presentModeScore = \case
        VK.PRESENT_MODE_IMMEDIATE_KHR -> 0 :: Int
        VK.PRESENT_MODE_FIFO_KHR -> 1
        VK.PRESENT_MODE_FIFO_RELAXED_KHR -> 2
        VK.PRESENT_MODE_MAILBOX_KHR -> 3
        _ -> 0
  let presentMode = maximumOn presentModeScore $ V.toList presentModes
  windowSize <- SDL.vkGetDrawableSize (vd ^. vdWindow . windowHandle)
  let width = max (min (fromIntegral $ windowSize ^. _x) maxWidth) minWidth
  let height = max (min (fromIntegral $ windowSize ^. _y) maxHeight) minHeight
  let swapExtent =
        VK.Extent2D
          { VK.width = width,
            VK.height = height
          }
  let imageCount = minImageCount + 1
  let (sharingMode, queueFamily) =
        if (vd ^. vdDevice . deviceQueue . graphicsFamilyIndex) == (vd ^. vdDevice . deviceQueue . presentFamilyIndex)
          then (VK.SHARING_MODE_EXCLUSIVE, [])
          else
            ( VK.SHARING_MODE_CONCURRENT,
              [ fromIntegral $ vd ^. vdDevice . deviceQueue . graphicsFamilyIndex,
                fromIntegral $ vd ^. vdDevice . deviceQueue . presentFamilyIndex
              ]
            )
  let createInfo =
        (zero :: VK.SwapchainCreateInfoKHR '[])
          { VK.surface = vd ^. vdSurface . surfaceHandle,
            VK.minImageCount = imageCount,
            VK.imageFormat = format,
            VK.imageColorSpace = colorSpace,
            VK.imageExtent = swapExtent,
            VK.imageArrayLayers = 1,
            VK.imageUsage = VK.IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
            VK.imageSharingMode = sharingMode,
            VK.queueFamilyIndices = V.fromList queueFamily,
            VK.preTransform = transform,
            VK.compositeAlpha = VK.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
            VK.presentMode = presentMode,
            VK.clipped = True,
            VK.oldSwapchain = VK.NULL_HANDLE
          }
  swapChain <- VK.withSwapchainKHR (vd ^. vdDevice . deviceHandle) createInfo Nothing (updateResourceMutableN "swap chain" (vd ^. vdSwapChain . swapChainHandle))

  (_, images) <- VK.getSwapchainImagesKHR (vd ^. vdDevice . deviceHandle) swapChain
  let prepareWithImageView = fmap $ \image ->
        let imageViewCreateInfo =
              (zero :: VK.ImageViewCreateInfo '[])
                { VK.image = image,
                  VK.viewType = VK.IMAGE_VIEW_TYPE_2D,
                  VK.format = format,
                  VK.components =
                    (zero :: VK.ComponentMapping)
                      { VK.r = VK.COMPONENT_SWIZZLE_IDENTITY,
                        VK.g = VK.COMPONENT_SWIZZLE_IDENTITY,
                        VK.b = VK.COMPONENT_SWIZZLE_IDENTITY,
                        VK.a = VK.COMPONENT_SWIZZLE_IDENTITY
                      },
                  VK.subresourceRange =
                    (zero :: VK.ImageSubresourceRange)
                      { VK.aspectMask = VK.IMAGE_ASPECT_COLOR_BIT,
                        VK.baseMipLevel = 0,
                        VK.levelCount = 1,
                        VK.baseArrayLayer = 0,
                        VK.layerCount = 1
                      }
                }
         in VK.withImageView (vd ^. vdDevice . deviceHandle) imageViewCreateInfo Nothing
  imageViews <- updateResourceMutableN' "ImageView" (vd ^. vdSwapChain . swapChainImageViews) $ prepareWithImageView images
  let prepareWithFrameBuffer = fmap $ \imageView ->
        let framebufferCreateInfo =
              (zero :: VK.FramebufferCreateInfo '[])
                { VK.renderPass = vd ^. vdRenderPass . renderPassHandle,
                  VK.attachments = [imageView],
                  VK.width = width,
                  VK.height = height,
                  VK.layers = 1
                }
         in VK.withFramebuffer (vd ^. vdDevice . deviceHandle) framebufferCreateInfo Nothing
  void $ updateResourceMutableN' "framebuffer" (vd ^. vdFramebuffer . framebufferHandle) $ prepareWithFrameBuffer imageViews
