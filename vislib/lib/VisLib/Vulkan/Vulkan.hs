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

module VisLib.Vulkan.Vulkan where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Cont
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Traversable.WithIndex
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign hiding (void)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import System.IO
import VisLib.App
import qualified Vulkan as VK
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import qualified Vulkan.CStruct.Extends as VK
import Vulkan.Zero
import qualified Vulkan as Vk
import qualified Data.Maybe as V

data Queues = Queues
  { _graphicsQueue :: VK.Queue,
    _graphicsCommandPool :: VK.CommandPool,
    _graphicsFamilyIndex :: Int,
    _presentQueue :: VK.Queue,
    _presentCommandPool :: VK.CommandPool,
    _presentFamilyIndex :: Int
  }
  deriving (Show, Eq)

$(makeLenses ''Queues)

data ShaderDescription = ShaderDescription
  { _fragmentShaderSource :: ByteString,
    _vertexShaderSource :: ByteString,
    _vertexInputBindingDescription :: Vector VK.VertexInputBindingDescription,
    _vertexInputAttributeDescription :: Vector VK.VertexInputAttributeDescription,
    _pushConstants :: Vector VK.PushConstantRange
  }

data ShaderDescriptionCompiled = ShaderDescriptionCompiled
  { _fragmentShaderModule :: VK.ShaderModule,
    _vertexShaderModule :: VK.ShaderModule,
    _vertexInputBindingDescription' :: Vector VK.VertexInputBindingDescription,
    _vertexInputAttributeDescription' :: Vector VK.VertexInputAttributeDescription,
    _pushConstants' :: Vector VK.PushConstantRange
  }

$(makeLenses ''ShaderDescription)
$(makeLenses ''ShaderDescriptionCompiled)

createSyncObjects :: (MonadIO io) => VK.Device -> AppMonad io d r (VK.Fence, VK.Semaphore, VK.Semaphore)
createSyncObjects device = do
  let fenceCreateInfo =
        (zero :: VK.FenceCreateInfo '[])
          { VK.flags = VK.FENCE_CREATE_SIGNALED_BIT
          }
  fence <- VK.withFence device fenceCreateInfo Nothing $ resourceN "fence"
  let semaphoreCreateInfo = (zero :: VK.SemaphoreCreateInfo '[])
  imageAvailableSemaphore <- VK.withSemaphore device semaphoreCreateInfo Nothing $ resourceN "image available semaphore"
  renderFinishedSemaphore <- VK.withSemaphore device semaphoreCreateInfo Nothing $ resourceN "render finished semaphore"
  return (fence, imageAvailableSemaphore, renderFinishedSemaphore)

createCommandBuffer :: (MonadIO io) => VK.Device -> VK.CommandPool -> AppMonad io d r VK.CommandBuffer
createCommandBuffer device commandPool = V.head <$> createCommandBuffers device commandPool 1

createCommandBuffers :: (MonadIO io) => VK.Device -> VK.CommandPool -> Int -> AppMonad io d r (Vector VK.CommandBuffer)
createCommandBuffers device commandPool count = do
  let commandBufferAllocateInfo =
        (zero :: VK.CommandBufferAllocateInfo)
          { VK.commandPool = commandPool,
            VK.level = VK.COMMAND_BUFFER_LEVEL_PRIMARY,
            VK.commandBufferCount = fromIntegral count
          }
  VK.withCommandBuffers device commandBufferAllocateInfo $ resourceN "command buffer"

createFramebuffers :: (MonadIO io) => VK.Device -> VK.RenderPass -> Vector VK.ImageView -> VK.SwapchainCreateInfoKHR '[] -> Maybe VK.ImageView -> AppMonad io d r (ResourceMutable (Vector VK.Framebuffer))
createFramebuffers device renderPass imageViews VK.SwapchainCreateInfoKHR {..} depthImage = do
  let VK.Extent2D width height = imageExtent
  let prepareWithFrameBuffer = fmap $ \imageView ->
        let framebufferCreateInfo =
              (zero :: VK.FramebufferCreateInfo '[])
                { VK.renderPass = renderPass,
                  VK.attachments = V.fromList (imageView : maybeToList depthImage),
                  VK.width = width,
                  VK.height = height,
                  VK.layers = 1
                }
         in VK.withFramebuffer device framebufferCreateInfo Nothing
  resourceMN' "framebuffer" $ prepareWithFrameBuffer imageViews

getViewPortScissor :: VK.Extent2D -> (VK.Viewport, VK.Rect2D)
getViewPortScissor extent@(VK.Extent2D width height) = (viewport, scissor)
  where
    viewport =
      (zero :: VK.Viewport)
        { VK.x = 0,
          VK.y = 0,
          VK.width = fromIntegral width,
          VK.height = fromIntegral height,
          VK.minDepth = 0.0,
          VK.maxDepth = 1.0
        }
    scissor =
      (zero :: VK.Rect2D)
        { VK.offset =
            (zero :: VK.Offset2D)
              { VK.x = 0,
                VK.y = 0
              },
          VK.extent = extent
        }

createRenderPass :: (MonadIO io) => VK.Device -> VK.SwapchainCreateInfoKHR '[] -> Maybe VK.Format -> AppMonad io d r VK.RenderPass
createRenderPass device VK.SwapchainCreateInfoKHR {..} Nothing = do
  let renderPassCreateInfo =
        (zero :: VK.RenderPassCreateInfo '[])
          { VK.attachments =
              [ VK.AttachmentDescription
                  { VK.flags = zero,
                    VK.format = imageFormat,
                    VK.samples = VK.SAMPLE_COUNT_1_BIT,
                    VK.loadOp = VK.ATTACHMENT_LOAD_OP_CLEAR,
                    VK.storeOp = VK.ATTACHMENT_STORE_OP_STORE,
                    VK.stencilLoadOp = VK.ATTACHMENT_LOAD_OP_DONT_CARE,
                    VK.stencilStoreOp = VK.ATTACHMENT_STORE_OP_DONT_CARE,
                    VK.initialLayout = VK.IMAGE_LAYOUT_UNDEFINED,
                    VK.finalLayout = VK.IMAGE_LAYOUT_PRESENT_SRC_KHR
                  }
              ],
            VK.subpasses =
              [ VK.SubpassDescription
                  { VK.flags = zero,
                    VK.pipelineBindPoint = VK.PIPELINE_BIND_POINT_GRAPHICS,
                    VK.inputAttachments = [],
                    VK.colorAttachments = V.fromList [VK.AttachmentReference 0 VK.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL],
                    VK.resolveAttachments = [],
                    VK.depthStencilAttachment = Nothing,
                    VK.preserveAttachments = []
                  }
              ],
            VK.dependencies =
              [ VK.SubpassDependency
                  { VK.srcSubpass = VK.SUBPASS_EXTERNAL,
                    VK.dstSubpass = 0,
                    VK.srcStageMask = VK.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                    VK.dstStageMask = VK.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                    VK.srcAccessMask = zero,
                    VK.dstAccessMask = VK.ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
                    VK.dependencyFlags = zero
                  }
              ]
          }
  VK.withRenderPass device renderPassCreateInfo Nothing $ resourceN "render pass"
createRenderPass device VK.SwapchainCreateInfoKHR {..} (Just depthFormat) = do
  let renderPassCreateInfo =
        (zero :: VK.RenderPassCreateInfo '[])
          { VK.attachments =
              [ VK.AttachmentDescription
                  { VK.flags = zero,
                    VK.format = imageFormat,
                    VK.samples = VK.SAMPLE_COUNT_1_BIT,
                    VK.loadOp = VK.ATTACHMENT_LOAD_OP_CLEAR,
                    VK.storeOp = VK.ATTACHMENT_STORE_OP_STORE,
                    VK.stencilLoadOp = VK.ATTACHMENT_LOAD_OP_DONT_CARE,
                    VK.stencilStoreOp = VK.ATTACHMENT_STORE_OP_DONT_CARE,
                    VK.initialLayout = VK.IMAGE_LAYOUT_UNDEFINED,
                    VK.finalLayout = VK.IMAGE_LAYOUT_PRESENT_SRC_KHR
                  },
                VK.AttachmentDescription
                  { VK.flags = zero,
                    VK.format = depthFormat,
                    VK.samples = VK.SAMPLE_COUNT_1_BIT,
                    VK.loadOp = VK.ATTACHMENT_LOAD_OP_CLEAR,
                    VK.storeOp = VK.ATTACHMENT_STORE_OP_DONT_CARE,
                    VK.stencilLoadOp = VK.ATTACHMENT_LOAD_OP_DONT_CARE,
                    VK.stencilStoreOp = VK.ATTACHMENT_STORE_OP_DONT_CARE,
                    VK.initialLayout = VK.IMAGE_LAYOUT_UNDEFINED,
                    VK.finalLayout = VK.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                  }
              ],
            VK.subpasses =
              [ VK.SubpassDescription
                  { VK.flags = zero,
                    VK.pipelineBindPoint = VK.PIPELINE_BIND_POINT_GRAPHICS,
                    VK.inputAttachments = [],
                    VK.colorAttachments = V.fromList [VK.AttachmentReference 0 VK.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL],
                    VK.resolveAttachments = [],
                    VK.depthStencilAttachment = Just (VK.AttachmentReference 1 VK.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL),
                    VK.preserveAttachments = []
                  }
              ],
            VK.dependencies =
              [ VK.SubpassDependency
                  { VK.srcSubpass = VK.SUBPASS_EXTERNAL,
                    VK.dstSubpass = 0,
                    VK.srcStageMask = VK.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT,
                    VK.dstStageMask = VK.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. VK.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT,
                    VK.srcAccessMask = zero,
                    VK.dstAccessMask = VK.ACCESS_COLOR_ATTACHMENT_WRITE_BIT .|. VK.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT,
                    VK.dependencyFlags = zero
                  }
              ]
          }
  VK.withRenderPass device renderPassCreateInfo Nothing $ resourceN "render pass"

createShaderModule :: (MonadIO io) => VK.Device -> ByteString -> AppMonad io d r VK.ShaderModule
createShaderModule device code = do
  let createInfo =
        (zero :: VK.ShaderModuleCreateInfo '[])
          { VK.code = code
          }
  VK.withShaderModule device createInfo Nothing $ resourceN "shader module"

createSwapChain :: (MonadIO io) => VK.PhysicalDevice -> VK.Device -> Queues -> VK.SurfaceKHR -> AppMonad io d r (ResourceMutable VK.SwapchainKHR, VK.SwapchainCreateInfoKHR '[], ResourceMutable (Vector VK.ImageView))
createSwapChain physicalDevice device queues surface = do
  VK.SurfaceCapabilitiesKHR
    { VK.currentExtent = currentExtent,
      VK.minImageCount = minImageCount,
      VK.currentTransform = transform
    } <-
    VK.getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
  (_, formats) <- VK.getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface
  (_, presentModes) <- VK.getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface
  let VK.SurfaceFormatKHR {..} = head $ filter (\(VK.SurfaceFormatKHR {..}) -> format == VK.FORMAT_B8G8R8A8_SRGB && colorSpace == VK.COLOR_SPACE_SRGB_NONLINEAR_KHR) $ V.toList formats
  let presentModeScore = \case
        VK.PRESENT_MODE_IMMEDIATE_KHR -> 0 :: Int
        VK.PRESENT_MODE_FIFO_KHR -> 1
        VK.PRESENT_MODE_FIFO_RELAXED_KHR -> 2
        VK.PRESENT_MODE_MAILBOX_KHR -> 3
        _ -> 0
  let presentMode = maximumOn presentModeScore $ V.toList presentModes
  let swapExtent = currentExtent
  let imageCount = minImageCount + 1
  let (sharingMode, queueFamily) =
        if queues ^. graphicsFamilyIndex == queues ^. presentFamilyIndex
          then (VK.SHARING_MODE_EXCLUSIVE, [])
          else (VK.SHARING_MODE_CONCURRENT, [fromIntegral $ queues ^. graphicsFamilyIndex, fromIntegral $ queues ^. presentFamilyIndex])
  let createInfo =
        (zero :: VK.SwapchainCreateInfoKHR '[])
          { VK.surface = surface,
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
  swapChain <- VK.withSwapchainKHR device createInfo Nothing $ resourceMN "swapchain"
  swapChain' <- getResource swapChain

  (_, images) <- VK.getSwapchainImagesKHR device swapChain'
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
         in VK.withImageView device imageViewCreateInfo Nothing
  imageViews <- resourceMN' "ImageView" $ prepareWithImageView images
  return (swapChain, createInfo, imageViews)

createDeviceQueueCreateInfo :: Int -> VK.DeviceQueueCreateInfo '[]
createDeviceQueueCreateInfo queueFamilyIndex =
  (zero :: VK.DeviceQueueCreateInfo '[])
    { VK.queueFamilyIndex = fromIntegral queueFamilyIndex,
      VK.queuePriorities = V.fromList [1.0]
    }
    ::& ()

createQueuesCreateInfo :: Int -> Int -> V.Vector (VK.SomeStruct VK.DeviceQueueCreateInfo)
createQueuesCreateInfo graphicsFamilyIndex presentFamilyIndex
  | graphicsFamilyIndex == presentFamilyIndex = V.fromList [VK.SomeStruct $ createDeviceQueueCreateInfo graphicsFamilyIndex]
  | otherwise =
      V.fromList
        [ VK.SomeStruct $ createDeviceQueueCreateInfo graphicsFamilyIndex,
          VK.SomeStruct $ createDeviceQueueCreateInfo presentFamilyIndex
        ]

createQueues :: (MonadIO io) => VK.Device -> Int -> Int -> AppMonad io d r Queues
createQueues device graphicsFamilyIndex presentFamilyIndex
  | graphicsFamilyIndex == presentFamilyIndex = do
      queue <- VK.getDeviceQueue device (fromIntegral graphicsFamilyIndex) 0
      commandPool <-
        VK.withCommandPool
          device
          (zero :: VK.CommandPoolCreateInfo)
            { VK.flags = VK.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
              VK.queueFamilyIndex = fromIntegral graphicsFamilyIndex
            }
          Nothing
          $ resourceN "command pool (graphics/present)"
      return $
        Queues
          { _graphicsQueue = queue,
            _graphicsFamilyIndex = graphicsFamilyIndex,
            _graphicsCommandPool = commandPool,
            _presentQueue = queue,
            _presentFamilyIndex = graphicsFamilyIndex,
            _presentCommandPool = commandPool
          }
  | otherwise = do
      graphicsQueue <- VK.getDeviceQueue device (fromIntegral graphicsFamilyIndex) 0
      presentQueue <- VK.getDeviceQueue device (fromIntegral presentFamilyIndex) 0
      graphicsCommandPool <-
        VK.withCommandPool
          device
          (zero :: VK.CommandPoolCreateInfo)
            { VK.flags = VK.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
              VK.queueFamilyIndex = fromIntegral graphicsFamilyIndex
            }
          Nothing
          $ resourceN "command pool (graphics)"
      presentCommandPool <-
        VK.withCommandPool
          device
          (zero :: VK.CommandPoolCreateInfo)
            { VK.flags = VK.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
              VK.queueFamilyIndex = fromIntegral presentFamilyIndex
            }
          Nothing
          $ resourceN "command pool (present)"
      return $
        Queues
          { _graphicsQueue = graphicsQueue,
            _graphicsFamilyIndex = graphicsFamilyIndex,
            _graphicsCommandPool = graphicsCommandPool,
            _presentQueue = presentQueue,
            _presentFamilyIndex = presentFamilyIndex,
            _presentCommandPool = presentCommandPool
          }

createLogicalDevice :: (MonadIO io) => VK.PhysicalDevice -> VK.SurfaceKHR -> [ByteString] -> AppMonad io d r (VK.Device, Queues)
createLogicalDevice physicalDevice surface extensions = do
  queueFamilies <- VK.getPhysicalDeviceQueueFamilyProperties physicalDevice
  let Just graphicsFamily = findIndex (\q -> VK.queueFlags q .&. VK.QUEUE_GRAPHICS_BIT /= zero) $ V.toList queueFamilies
  presentFamilies <- forM [0 .. V.length queueFamilies - 1] $ \i -> VK.getPhysicalDeviceSurfaceSupportKHR physicalDevice (fromIntegral i) surface
  let Just presentFamily = findIndex id presentFamilies

  let createInfo =
        (zero :: VK.DeviceCreateInfo '[])
          { VK.queueCreateInfos = createQueuesCreateInfo graphicsFamily presentFamily,
            VK.enabledExtensionNames = V.fromList extensions
          }
          ::& ()

  device <- VK.withDevice physicalDevice createInfo Nothing $ resourceN "device"
  queues <- createQueues device graphicsFamily presentFamily
  return (device, queues)

choosePhysicalDevice :: (MonadIO io) => [ByteString] -> VK.Instance -> io (Maybe VK.PhysicalDevice)
choosePhysicalDevice requiredExtensions inst = do
  (_, devices) <- VK.enumeratePhysicalDevices inst
  devices' <- fmap catMaybes $ forM (V.toList devices) $ \device -> do
    props <- VK.getPhysicalDeviceProperties device
    let dType = VK.deviceType props
    let dTypeScore = case dType of
          VK.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> 10
          VK.PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> 8
          VK.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 7
          VK.PHYSICAL_DEVICE_TYPE_CPU -> 2
          _ -> 0 :: Int
    (_, extensions) <- VK.enumerateDeviceExtensionProperties device Nothing
    let extNames = map VK.extensionName $ V.toList extensions
    if all (`elem` extNames) requiredExtensions
      then return $ Just (dTypeScore, device)
      else return Nothing
  if null devices'
    then return Nothing
    else do
      let device = maximumOn fst devices'
      return $ Just $ snd device

getSurfaceCreationExtensions :: (MonadIO io) => SDL.Window -> io [ByteString]
getSurfaceCreationExtensions window = do
  surfaceCreationExtensions <- SDL.vkGetInstanceExtensions window
  forM surfaceCreationExtensions $ liftIO . BS.packCString

getAvailableInstanceExtensions :: (MonadIO io) => io [ByteString]
getAvailableInstanceExtensions = do
  (_, extensions) <- VK.enumerateInstanceExtensionProperties Nothing
  return $ map VK.extensionName $ V.toList extensions

foreign import ccall "wrapper" mkDebugMessage :: VK.FN_vkDebugUtilsMessengerCallbackEXT -> IO VK.PFN_vkDebugUtilsMessengerCallbackEXT

debugMessage :: VK.FN_vkDebugUtilsMessengerCallbackEXT
debugMessage severity _messageTypes callbackDataPtr _userDataPtr = do
  callbackData <- VK.peekSomeCStruct callbackDataPtr
  VK.withSomeStruct callbackData $ \callbackData' -> do
    let messageString = VK.message callbackData'
    let messageIdName = fromMaybe "Unknown" $ VK.messageIdName callbackData'
    let severityString = case severity of
          VK.DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT -> "VERBOSE"
          VK.DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT -> "INFO"
          VK.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT -> "WARNING"
          VK.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT -> "ERROR"
          _ -> "UNKNOWN"
    putStrLn $ "[" ++ severityString ++ "] " ++ BSC.unpack messageIdName ++ " |  " ++ BSC.unpack messageString
    hFlush stdout
    return VK.FALSE

getDebugCreateInfo :: (MonadIO io) => io VK.DebugUtilsMessengerCreateInfoEXT
getDebugCreateInfo = do
  debugLogger <- liftIO $ mkDebugMessage debugMessage
  let debugCreateInfo =
        (zero :: VK.DebugUtilsMessengerCreateInfoEXT)
          { VK.messageSeverity = VK.DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT .|. VK.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|. VK.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
            VK.messageType = VK.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|. VK.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT .|. VK.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
            VK.pfnUserCallback = debugLogger,
            VK.userData = nullPtr
          }
  return debugCreateInfo

createBuffer :: (MonadIO io) => VK.Device -> VK.BufferUsageFlags -> Int -> AppMonad io d r VK.Buffer
createBuffer device usage size = do
  VK.withBuffer device ?? Nothing ?? resourceN "buffer" $
    (zero :: VK.BufferCreateInfo '[])
      { VK.size = fromIntegral size,
        VK.usage = usage,
        VK.sharingMode = VK.SHARING_MODE_EXCLUSIVE
      }

createImageBase :: (MonadIO io) => VK.Device -> VK.ImageUsageFlags -> VK.Format -> Int -> Maybe Int -> Maybe Int -> (io Vk.Image -> (Vk.Image -> io ()) -> r) -> r
createImageBase device usage format width height depth = do
  let height' = fromMaybe 1 height
  let depth' = fromMaybe 1 depth
  let imageType = case (height, depth) of
        (Nothing, Nothing) -> VK.IMAGE_TYPE_1D
        (Just _, Nothing) -> VK.IMAGE_TYPE_2D
        (Just _, Just _) -> VK.IMAGE_TYPE_3D
        (Nothing, Just _) -> error "Depth without height is not supported"
  let imageCreateInfo =
        (zero :: VK.ImageCreateInfo '[])
          { VK.imageType = imageType,
            VK.format = format,
            VK.extent = VK.Extent3D (fromIntegral width) (fromIntegral height') (fromIntegral depth'),
            VK.mipLevels = 1,
            VK.arrayLayers = 1,
            VK.samples = VK.SAMPLE_COUNT_1_BIT,
            VK.tiling = VK.IMAGE_TILING_OPTIMAL,
            VK.usage = usage,
            VK.sharingMode = VK.SHARING_MODE_EXCLUSIVE
          }
  VK.withImage device imageCreateInfo Nothing

createImage' :: (MonadIO io) => VK.Device -> VK.ImageUsageFlags -> VK.Format -> Int -> Maybe Int -> Maybe Int -> AppMonad io d r (ResourceMutable VK.Image)
createImage' device usage format width height depth = createImageBase device usage format width height depth $ resourceMN "image"

createImage :: (MonadIO io) => VK.Device -> VK.ImageUsageFlags -> VK.Format -> Int -> Maybe Int -> Maybe Int -> AppMonad io d r VK.Image
createImage device usage format width height depth = 
  createImageBase device usage format width height depth $ resourceN "image"

createImageView :: (MonadIO io) => VK.Device -> VK.Image -> VK.Format -> VK.ImageAspectFlags -> AppMonad io d r VK.ImageView
createImageView device image format aspectFlags = do
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
                { VK.aspectMask = aspectFlags,
                  VK.baseMipLevel = 0,
                  VK.levelCount = 1,
                  VK.baseArrayLayer = 0,
                  VK.layerCount = 1
                }
          }
  VK.withImageView device imageViewCreateInfo Nothing $ resourceN "image view"

recordTransitionImageLayout :: (MonadIO io) => VK.CommandBuffer -> VK.Image -> VK.ImageAspectFlags -> VK.ImageLayout -> VK.ImageLayout -> AppMonad io d r ()
recordTransitionImageLayout commandBuffer image aspect oldLayout newLayout = do
  let (srcAcces, dstAccess, srcStage, dstStage) = case (oldLayout, newLayout) of
        (VK.IMAGE_LAYOUT_UNDEFINED, VK.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) -> (zero, VK.ACCESS_TRANSFER_WRITE_BIT, VK.PIPELINE_STAGE_TOP_OF_PIPE_BIT, VK.PIPELINE_STAGE_TRANSFER_BIT)
        (VK.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) -> (VK.ACCESS_TRANSFER_WRITE_BIT, VK.ACCESS_SHADER_READ_BIT, VK.PIPELINE_STAGE_TRANSFER_BIT, VK.PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
        _ -> error "Unsupported image layout transition"

  let barrierInfo =
        VK.SomeStruct
          (zero :: VK.ImageMemoryBarrier '[])
            { VK.oldLayout = oldLayout,
              VK.newLayout = newLayout,
              VK.srcQueueFamilyIndex = VK.QUEUE_FAMILY_IGNORED,
              VK.dstQueueFamilyIndex = VK.QUEUE_FAMILY_IGNORED,
              VK.image = image,
              VK.subresourceRange =
                (zero :: VK.ImageSubresourceRange)
                  { VK.aspectMask = aspect,
                    VK.baseMipLevel = 0,
                    VK.levelCount = 1,
                    VK.baseArrayLayer = 0,
                    VK.layerCount = 1
                  },
              VK.srcAccessMask = srcAcces,
              VK.dstAccessMask = dstAccess
            }
  VK.cmdPipelineBarrier commandBuffer srcStage dstStage zero [] [] [barrierInfo]