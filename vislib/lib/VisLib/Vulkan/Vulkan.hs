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
import Vulkan.Utils.ShaderQQ.GLSL.Shaderc
import Control.Exception (bracket)

data Queues = Queues
  { _graphicsQueue :: VK.Queue,
    _graphicsCommandPool :: VK.CommandPool,
    _graphicsFamilyIndex :: Int,
    _presentQueue :: VK.Queue,
    _presentCommandPool :: VK.CommandPool,
    _presentFamilyIndex :: Int
  }

$(makeLenses ''Queues)

data ShaderDescription = ShaderDescription {
  _fragmentShaderSource :: ByteString,
  _vertexShaderSource :: ByteString,
  _vertexInputBindingDescription :: Vector VK.VertexInputBindingDescription,
  _vertexInputAttributeDescription :: Vector VK.VertexInputAttributeDescription,
  _pushConstants :: Vector VK.PushConstantRange
}

data ShaderDescriptionCompiled = ShaderDescriptionCompiled {
  _fragmentShaderModule :: VK.ShaderModule,
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

createFramebuffers :: (MonadIO io) => VK.Device -> VK.RenderPass -> Vector VK.ImageView -> VK.SwapchainCreateInfoKHR '[] -> AppMonad io d r (Vector VK.Framebuffer, AppMonad io d r ())
createFramebuffers device renderPass imageViews VK.SwapchainCreateInfoKHR {..} = do
  let VK.Extent2D width height = imageExtent
  fmap (second (V.sequence_ . V.reverse) . V.unzip) $ iforM imageViews $ \index imageView -> do
    let framebufferCreateInfo =
          (zero :: VK.FramebufferCreateInfo '[])
            { VK.renderPass = renderPass,
              VK.attachments = V.fromList [imageView],
              VK.width = width,
              VK.height = height,
              VK.layers = 1
            }
    VK.withFramebuffer device framebufferCreateInfo Nothing $ resourceRN ("framebuffer " ++ show index)

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

createRenderPass :: (MonadIO io) => VK.Device -> VK.SwapchainCreateInfoKHR '[] -> AppMonad io d r VK.RenderPass
createRenderPass device VK.SwapchainCreateInfoKHR {..} = do
  let renderPassCreateInfo =
        (zero :: VK.RenderPassCreateInfo '[])
          { VK.attachments =
              V.fromList
                [ (zero :: VK.AttachmentDescription)
                    { VK.format = imageFormat,
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
              V.fromList
                [ (zero :: VK.SubpassDescription)
                    { VK.pipelineBindPoint = VK.PIPELINE_BIND_POINT_GRAPHICS,
                      VK.colorAttachments = V.fromList [VK.AttachmentReference 0 VK.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL]
                    }
                ],
            VK.dependencies =
              V.fromList
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

createPipeline :: (MonadIO io) => VK.Device -> ShaderDescriptionCompiled -> VK.RenderPass -> AppMonad io d r (VK.Pipeline, VK.PipelineLayout)
createPipeline device ShaderDescriptionCompiled{..} renderPass = do
  liftIO $ print _pushConstants'
  let layoutInfo = (zero :: VK.PipelineLayoutCreateInfo) {
    VK.pushConstantRanges = _pushConstants'
  }
  layout <- VK.withPipelineLayout device layoutInfo Nothing $ resourceN "pipeline layout"
  let pipelineCreateInfo =
        (zero :: VK.GraphicsPipelineCreateInfo '[])
          { VK.stageCount = 2,
            VK.stages =
              V.fromList [(_vertexShaderModule, VK.SHADER_STAGE_VERTEX_BIT), (_fragmentShaderModule, VK.SHADER_STAGE_FRAGMENT_BIT)] <&> \(shaderModule, stage) -> do
                VK.SomeStruct
                  (zero :: VK.PipelineShaderStageCreateInfo '[])
                    { VK.stage = stage,
                      VK.module' = shaderModule,
                      VK.name = "main"
                    },
            VK.vertexInputState =
              Just $
                VK.SomeStruct
                  (zero :: VK.PipelineVertexInputStateCreateInfo '[])
                    { VK.vertexBindingDescriptions = _vertexInputBindingDescription',
                      VK.vertexAttributeDescriptions = _vertexInputAttributeDescription'
                    },
            VK.inputAssemblyState =
              Just
                (zero :: VK.PipelineInputAssemblyStateCreateInfo)
                  { VK.topology = VK.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
                    VK.primitiveRestartEnable = False
                  },
            VK.viewportState =
              Just $
                VK.SomeStruct
                  (zero :: VK.PipelineViewportStateCreateInfo '[])
                    { VK.viewportCount = 1,
                      VK.scissorCount = 1
                    },
            VK.rasterizationState =
              Just $
                VK.SomeStruct
                  (zero :: VK.PipelineRasterizationStateCreateInfo '[])
                    { VK.depthClampEnable = False,
                      VK.rasterizerDiscardEnable = False,
                      VK.polygonMode = VK.POLYGON_MODE_FILL,
                      VK.lineWidth = 1.0,
                      VK.cullMode = VK.CULL_MODE_NONE,
                      VK.frontFace = VK.FRONT_FACE_COUNTER_CLOCKWISE,
                      VK.depthBiasEnable = False
                    },
            VK.multisampleState =
              Just $
                VK.SomeStruct
                  (zero :: VK.PipelineMultisampleStateCreateInfo '[])
                    { VK.sampleShadingEnable = False,
                      VK.rasterizationSamples = VK.SAMPLE_COUNT_1_BIT
                    },
            VK.colorBlendState =
              Just $
                VK.SomeStruct
                  (zero :: VK.PipelineColorBlendStateCreateInfo '[])
                    { VK.logicOpEnable = False,
                      VK.attachments =
                        V.fromList
                          [ (zero :: VK.PipelineColorBlendAttachmentState)
                              { VK.blendEnable = False,
                                VK.colorWriteMask = VK.COLOR_COMPONENT_R_BIT .|. VK.COLOR_COMPONENT_G_BIT .|. VK.COLOR_COMPONENT_B_BIT .|. VK.COLOR_COMPONENT_A_BIT
                              }
                          ],
                      VK.attachmentCount = 1
                    },
            VK.dynamicState =
              Just
                (zero :: VK.PipelineDynamicStateCreateInfo)
                  { VK.dynamicStates = V.fromList [VK.DYNAMIC_STATE_VIEWPORT, VK.DYNAMIC_STATE_SCISSOR]
                  },
            VK.layout = layout,
            VK.renderPass = renderPass,
            VK.subpass = 0
          }
  (_, pipelines) <- VK.withGraphicsPipelines device VK.NULL_HANDLE (V.fromList [VK.SomeStruct pipelineCreateInfo]) Nothing $ resourceN "graphics pipeline"

  return (V.head pipelines, layout)

compileShaderDescription :: MonadIO io => VK.Device -> ShaderDescription -> AppMonad io d r ShaderDescriptionCompiled
compileShaderDescription device ShaderDescription{..} = do
  vertexShader' <- createShaderModule device _vertexShaderSource
  fragmentShader' <- createShaderModule device _fragmentShaderSource
  return
    ShaderDescriptionCompiled
      { _vertexShaderModule = vertexShader',
        _fragmentShaderModule = fragmentShader',
        _vertexInputBindingDescription' = _vertexInputBindingDescription,
        _vertexInputAttributeDescription' = _vertexInputAttributeDescription,
        _pushConstants' = _pushConstants
      }

createShaderModule :: (MonadIO io) => VK.Device -> ByteString -> AppMonad io d r VK.ShaderModule
createShaderModule device code = do
  let createInfo =
        (zero :: VK.ShaderModuleCreateInfo '[])
          { VK.code = code
          }
  VK.withShaderModule device createInfo Nothing $ resourceN "shader module"

createSwapChain :: (MonadIO io) => VK.PhysicalDevice -> VK.Device -> Queues -> VK.SurfaceKHR -> AppMonad io d () (VK.SwapchainKHR, VK.SwapchainCreateInfoKHR '[], Vector VK.Image, Vector VK.ImageView, AppMonad io d () ())
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
  (swapChain, relSwapChain) <- VK.withSwapchainKHR device createInfo Nothing $ resourceRN "swapchain"

  (_, images) <- VK.getSwapchainImagesKHR device swapChain
  (imageViews, relImageViews) <- fmap V.unzip $ iforM images $ \index image -> do
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
    VK.withImageView device imageViewCreateInfo Nothing $ resourceRN ("ImageView " ++ show index)
  let rel = do
        V.sequence_ $ V.reverse relImageViews
        relSwapChain
  return (swapChain, createInfo, images, imageViews, rel)

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

present :: (MonadIO io) => VK.SwapchainKHR -> VK.Semaphore -> VK.Queue -> Word32 -> io ()
present swapChain waitSemaphore presentQueue index = do
  let presentInfo =
        (zero :: VK.PresentInfoKHR '[])
          { VK.swapchains = V.fromList [swapChain],
            VK.imageIndices = V.fromList [index],
            VK.waitSemaphores = V.fromList [waitSemaphore]
          }
  void $ VK.queuePresentKHR presentQueue presentInfo

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
  VK.withBuffer device ?? Nothing ?? resourceN "buffer" $ (zero :: VK.BufferCreateInfo '[]) {
    VK.size = fromIntegral size,
    VK.usage = usage,
    VK.sharingMode = VK.SHARING_MODE_EXCLUSIVE
  }

-- createBuffer :: (MonadIO io) => VK.Device -> VK.PhysicalDevice -> VK.MemoryPropertyFlags -> VK.BufferUsageFlags -> Int -> AppMonad io d r (VK.Buffer, VK.DeviceMemory)
-- createBuffer device pdevice memoryFlags usage size = do
--   let createInfo =
--         (zero :: VK.BufferCreateInfo '[])
--           { VK.size = fromIntegral size,
--             VK.usage = usage,
--             VK.sharingMode = VK.SHARING_MODE_EXCLUSIVE
--           }
--   buffer <- VK.withBuffer device createInfo Nothing $ resourceN "buffer"
--   memRequirements@VK.MemoryRequirements
--     { VK.memoryTypeBits = memReqBits,
--       VK.size = allocSize
--     } <-
--     VK.getBufferMemoryRequirements device buffer
--   memProperties <- VK.getPhysicalDeviceMemoryProperties pdevice
--   let validMemTypes = do
--         (i, memType@VK.MemoryType {VK.propertyFlags = flags}) <- zip [0 :: Int ..] $ V.toList $ VK.memoryTypes memProperties
--         guard $ memReqBits .&. (1 .<<. i) /= 0
--         guard $ flags .&. memoryFlags == memoryFlags
--         return (i, memType)
--   let memType = head validMemTypes
--   let allocInfo = (zero :: VK.MemoryAllocateInfo '[])
--         { VK.allocationSize = allocSize,
--           VK.memoryTypeIndex = fromIntegral $ fst memType
--         }
--   memory <- VK.withMemory device allocInfo Nothing $ resourceN "buffer memory"
--   VK.bindBufferMemory device buffer memory 0

--   return (buffer, memory)

-- writeHostBuffer' :: (MonadIO io) => VK.Device -> VK.DeviceMemory -> ByteString -> Int -> AppMonad io d r ()
-- writeHostBuffer' device memory bytes size = scope $ do
--   when (BS.length bytes > size) $ error "writeHostBuffer: size mismatch"
--   ptr <- VK.withMappedMemory device memory zero (fromIntegral size) zero resource
--   liftIO $ BS.useAsCStringLen bytes $ \(bytesPtr, len) -> do
--     copyBytes ptr (castPtr bytesPtr) len

-- writeHostBuffer :: (MonadIO io) => VK.Device -> VK.DeviceMemory -> ByteString -> AppMonad io d r ()
-- writeHostBuffer device memory bytes = do
--   let size = BS.length bytes
--   writeHostBuffer' device memory bytes size

-- writeHostBufferS :: (MonadIO io, Storable a) => VK.Device -> VK.DeviceMemory -> [a] -> AppMonad io d r ()
-- writeHostBufferS device memory bytes = scope $ do
--   let size = sum $ sizeOf <$> bytes
--   ptr <- VK.withMappedMemory device memory zero (fromIntegral size) zero resource
--   liftIO $ withArray bytes $ \bytesPtr -> do
--     copyBytes ptr (castPtr bytesPtr) size