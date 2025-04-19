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

module Main where

-- import Vulkan.Utils.ShaderQQ.GLSL.Shaderc

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Cont
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Data.Tuple.Extra (uncurry3)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign hiding (void)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Text.Megaparsec
import VisLib.App
import VisLib.Loader.GLTF
import VisLib.Vulkan.Memory
import VisLib.Vulkan.Model
import VisLib.Vulkan.Shader
import VisLib.Vulkan.ShaderTH
import VisLib.Vulkan.Vulkan hiding (ShaderDescription, present)
import qualified VisLib.Vulkan.Vulkan as Vulkan hiding (ShaderDescription)
import qualified Vulkan as VK
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import qualified Vulkan.CStruct.Extends as VK
import qualified Vulkan.Exception as VK
import Vulkan.Zero
import Data.Maybe
import Objects

interleave :: [[a]] -> [a]
interleave = ((++) <$> fst <*> (interleave . snd)) . unzip . mapMaybe uncons

main :: IO ()
main = runApp ?? () $ do
  vulkanData <- initialize
  object <- withData vulkanData load
  withData ([object], vulkanData) $ callCC $ \exit -> do
    renderAction <- cycle <$> forM [0..2] createFlightAction
    let eventsAction = repeat $ handleSDLEvents [checkExit exit]
    sequence_ $ interleave [eventsAction, renderAction]
  VK.deviceWaitIdle (vulkanData^.vdDevice.deviceHandle)

  return ()

createFlightAction :: (MonadIO io) => Int -> AppMonad io ([Object], VulkanData) r (AppMonad io ([Object], VulkanData) r2 ())
createFlightAction i = do
  (_objs, vd) <- ask
  let commandBufferAllocateInfo
        = (zero :: VK.CommandBufferAllocateInfo)
            { VK.commandPool = vd^.vdDevice. deviceQueue . graphicsCommandPool,
              VK.level = VK.COMMAND_BUFFER_LEVEL_PRIMARY,
              VK.commandBufferCount = 1
            }
  commandBuffer <- V.head <$> VK.withCommandBuffers (vd^.vdDevice.deviceHandle) commandBufferAllocateInfo (resourceN $ "command buffer " ++ show i)
  let fenceInfo = (zero :: VK.FenceCreateInfo '[]) {VK.flags = VK.FENCE_CREATE_SIGNALED_BIT}
  fence <- VK.withFence (vd^.vdDevice. deviceHandle) fenceInfo Nothing $ resourceN $ "fence " ++ show i
  imageAvailableSemaphore <- VK.withSemaphore (vd^.vdDevice. deviceHandle) (zero :: VK.SemaphoreCreateInfo '[]) Nothing $ resourceN $ "image available semaphore " ++ show i
  renderFinishedSemaphore <- VK.withSemaphore (vd^.vdDevice. deviceHandle) (zero :: VK.SemaphoreCreateInfo '[]) Nothing $ resourceN $ "render finished semaphore " ++ show i
  return $ do
    (objs, vd) <- ask
    void $ VK.waitForFences (vd^.vdDevice. deviceHandle) [fence] True maxBound
    imageIndex <- acquireNextImage (vd^.vdDevice) (vd^.vdSwapChain) imageAvailableSemaphore
    VK.resetFences (vd^.vdDevice. deviceHandle) [fence]
    VK.resetCommandBuffer commandBuffer zero
    framebuffers <- getResource (vd^. vdFramebuffer . framebufferHandle)
    let framebuffer = framebuffers ^?! ix (fromIntegral imageIndex)
    let renderPass = vd^.vdRenderPass . renderPassHandle
    let VK.SwapchainCreateInfoKHR{VK.imageExtent=extent} = vd^.vdSwapChain . swapChainInfo
    record objs commandBuffer renderPass framebuffer (getViewPortScissor extent)
    submit (vd^.vdDevice) commandBuffer imageAvailableSemaphore renderFinishedSemaphore fence
    present (vd^.vdDevice) (vd^.vdSwapChain) renderFinishedSemaphore imageIndex
    return ()

handleSDLEvents :: MonadIO io => [SDL.Event -> AppMonad io d r ()] -> AppMonad io d r ()
handleSDLEvents handlers = do
  events <- liftIO SDL.pollEvents
  sequence_ (handlers <*> events)

checkExit :: MonadIO io => (() -> AppMonad io d r ()) -> SDL.Event -> AppMonad io d r ()
checkExit exit event = do
  case SDL.eventPayload event of
    SDL.QuitEvent -> exit ()
    _ -> return ()

recordObject :: (MonadIO io) => Object -> VK.CommandBuffer -> io ()
recordObject obj commandBuffer = do
  cmdBindPipeline commandBuffer (obj ^. objectPipeline)
  cmdBindBuffers commandBuffer (obj ^. objectShaderInfo) (obj ^.. objectBuffers . traverse . bufferHandle)
  case obj ^. objectDrawInformation of
    Left indexBuffer -> do
      cmdBindIndexBuffer commandBuffer indexBuffer
      VK.cmdDrawIndexed commandBuffer (fromIntegral $ indexBuffer ^. indexBufferCount) 1 0 0 0
    Right count -> do
      let vertexCount = fromIntegral count
      VK.cmdDraw commandBuffer vertexCount 1 0 0

record :: (MonadIO io) => [Object] -> VK.CommandBuffer -> VK.RenderPass -> VK.Framebuffer -> (VK.Viewport, VK.Rect2D) -> AppMonad io d r ()
record objects commandBuffer renderPass framebuffer (viewport, scissor) = do
  let commandBeginInfo = (zero :: VK.CommandBufferBeginInfo '[])
  VK.useCommandBuffer commandBuffer commandBeginInfo $ do
    let renderPassBeginInfo =
          (zero :: VK.RenderPassBeginInfo '[])
            { VK.renderPass = renderPass,
              VK.framebuffer = framebuffer,
              VK.renderArea = scissor,
              VK.clearValues =
                [ VK.Color $ VK.Float32 0 0 0 1
                ]
            }
    VK.cmdUseRenderPass commandBuffer renderPassBeginInfo VK.SUBPASS_CONTENTS_INLINE $ do
      VK.cmdSetViewport commandBuffer 0 [viewport]
      VK.cmdSetScissor commandBuffer 0 [scissor]
      forM_ objects $ \obj -> do
        case obj ^. objectCommandBuffer of
          Just _ -> error "secondary command buffer not implemented"
          Nothing -> recordObject obj commandBuffer

load :: (MonadIO io) => AppMonad io VulkanData r Object
load = do
  device <- view vdDevice
  memoryAllocator <- view vdMemoryAllocator
  renderPass <- view vdRenderPass
  vertexShader' <- compileShaderRepresentation (device ^. deviceHandle) vertexShader
  fragmentShader' <- compileShaderRepresentation (device ^. deviceHandle) fragmentShader
  let filePath = "../assets/Box.glb"
  fileContent <- liftIO $ BS.readFile filePath

  (gltf, chunks) <- case runParser glbParser filePath fileContent of
    Left err -> error $ "Error parsing GLTF: " ++ show err
    Right glb@GLB {chunks = chunks} -> do
      gltf <- loadGLTF glb
      let dataChunks = map chunkData $ filter ((== "BIN") . chunkType) chunks
      return (gltf, dataChunks)

  let shaderInfo = createShaderInfoFromGLTF [vertexShader', fragmentShader'] gltf 0 0
  pipeline <- uncurry Pipeline <$> createPipelineFromShaderInfo (device ^. deviceHandle) shaderInfo (renderPass ^. renderPassHandle)

  bufferViews <- fmap (uncurry Buffer) <$> loadAllBuffers gltf chunks (device ^. deviceHandle) memoryAllocator

  let drawInfo = case getIndexBuffer gltf 0 0 of
        Just (bufferViewIndex, count, size) ->
          let indexType = case size of
                2 -> VK.INDEX_TYPE_UINT16
                4 -> VK.INDEX_TYPE_UINT32
                _ -> error "Unsupported index buffer size"
           in Left $ IndexBuffer (bufferViews !! bufferViewIndex) count indexType
        Nothing ->
          let vertexCount = getVertexCount gltf 0 0
           in Right vertexCount

  return $
    Object
      { _objectShaderInfo = shaderInfo,
        _objectPipeline = pipeline,
        _objectBuffers = bufferViews,
        _objectCommandBuffer = Nothing,
        _objectDrawInformation = drawInfo
      }

initialize :: (MonadIO io) => AppMonad io d r VulkanData
initialize = do
  SDL.initialize ([SDL.InitVideo, SDL.InitEvents] :: [SDL.InitFlag])
  SDL.vkLoadLibrary Nothing
  window <- Window <$> resourceN "window" (SDL.createWindow "Vulkan Example" vulkanWindow) SDL.destroyWindow
  swcExtensions <- getSurfaceCreationExtensions (window ^. windowHandle)
  debugCreateInfo <- getDebugCreateInfo
  inst <-
    Instance
      <$> VK.withInstance
        ( (zero :: VK.InstanceCreateInfo '[])
            { VK.enabledExtensionNames = V.fromList (swcExtensions ++ ["VK_EXT_debug_utils"]),
              VK.enabledLayerNames = ["VK_LAYER_KHRONOS_validation"]
            }
            ::& VK.ValidationFeaturesEXT [VK.VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT] []
              :& debugCreateInfo
              :& ()
        )
        Nothing
        (resourceN "instance")
  void $
    resourceN
      "debug messenger"
      (VK.createDebugUtilsMessengerEXT (inst ^. instanceHandle) debugCreateInfo Nothing)
      (\m -> VK.destroyDebugUtilsMessengerEXT (inst ^. instanceHandle) m Nothing)
  windowSurface <-
    Surface
      <$> resourceN
        "window surface"
        (fmap VK.SurfaceKHR $ SDL.vkCreateSurface (window ^. windowHandle) $ castPtr $ VK.instanceHandle $ inst ^. instanceHandle)
        (\surf -> VK.destroySurfaceKHR (inst ^. instanceHandle) surf Nothing)
  physicalDevice <- PhysicalDevice . fromJust <$> choosePhysicalDevice ["VK_KHR_swapchain"] (inst ^. instanceHandle)
  device <- uncurry Device <$> createLogicalDevice (physicalDevice ^. physicalDeviceHandle) (windowSurface ^. surfaceHandle) ["VK_KHR_swapchain"]
  swapChain <- uncurry3 SwapChain <$> createSwapChain (physicalDevice ^. physicalDeviceHandle) (device ^. deviceHandle) (device ^. deviceQueue) (windowSurface ^. surfaceHandle)
  renderPass <- RenderPass <$> createRenderPass (device ^. deviceHandle) (swapChain ^. swapChainInfo)
  imageViews <- getResource $ swapChain ^. swapChainImageViews
  framebuffers <- Framebuffer <$> createFramebuffers (device ^. deviceHandle) (renderPass ^. renderPassHandle) imageViews (swapChain ^. swapChainInfo)
  memoryAllocator <- pureResourceM =<< createMemoryState (device ^. deviceHandle) (physicalDevice ^. physicalDeviceHandle) (device ^. deviceQueue . graphicsQueue)

  return $
    VulkanData
      { _vdWindow = window,
        _vdInstance = inst,
        _vdPhysicalDevice = physicalDevice,
        _vdDevice = device,
        _vdSurface = windowSurface,
        _vdSwapChain = swapChain,
        _vdFramebuffer = framebuffers,
        _vdRenderPass = renderPass,
        _vdMemoryAllocator = memoryAllocator
      }

vertexShader :: ShaderDescription
vertexShader =
  [vert|
    #version 450

    layout(location = 0) out vec3 fragColor;

    layout(location = 0) in vec3 POSITION;

    void main() {
        gl_Position = vec4(POSITION, 1);
        fragColor = vec3(1, 0, 0);
    }
  |]

fragmentShader :: ShaderDescription
fragmentShader =
  [frag|
    #version 450

    layout(location = 0) in vec3 fragColor;

    layout(location = 0) out vec4 outColor;

    void main() {
        outColor = vec4(fragColor, 1.0);
    }
  |]