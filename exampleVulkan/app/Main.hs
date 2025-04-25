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
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Main where

import Control.Exception
import Control.Lens hiding (uncons)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.List.Extra hiding (transpose)
import Data.Maybe
import Data.Tuple.Extra (uncurry3)
import qualified Data.Vector as V
import Foreign hiding (void)
import Linear.V2
import Objects
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Text.Megaparsec hiding (try)
import VisLib.App
import VisLib.Loader.GLTF hiding (translation, max, min)
import VisLib.Vulkan.Memory
import VisLib.Vulkan.Model
import VisLib.Vulkan.Shader
import VisLib.Vulkan.ShaderTH
import VisLib.Vulkan.Vulkan hiding (ShaderDescription, present)
import qualified Vulkan as VK
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import qualified Vulkan.Exception as VK
import Vulkan.Zero
import Linear.Matrix
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef
import Data.StateVar
import Linear.Projection
import Linear.V3
import Linear.Quaternion

vulkanWindow :: SDL.WindowConfig
vulkanWindow =
  SDL.defaultWindow
    { SDL.windowGraphicsContext = SDL.VulkanContext,
      SDL.windowResizable = False,
      SDL.windowInitialSize = SDL.V2 600 600
    }

interleave :: [[a]] -> [a]
interleave = ((++) <$> fst <*> interleave . snd) . unzip . mapMaybe uncons

data RunData = RunData {
  vulkanData :: VulkanData,
  objects :: [Object],
  viewProjection :: IORef (M44 Float)
}

handleUniform :: MonadIO io => String -> (Ptr () -> IO ()) -> AppMonad io RunData r ()
handleUniform "pv" binder = do
  vp <- get =<< asks viewProjection
  -- currTime <- liftIO (round . (* 1000) <$> getPOSIXTime)
  -- let currTime' = currTime `mod` 1000
  -- let currTime'' = fromIntegral currTime' / 1000
  -- let mvp :: M44 Float = identity !!* currTime'' & _w . _w .~ 1
  liftIO $ with (transpose vp) (binder . castPtr)
handleUniform "m" binder = do
  currTime <- liftIO (round . (* 1000) <$> getPOSIXTime)
  let currTime' = currTime `mod` 1000000
  let currTime'' = fromIntegral currTime' / 1000
  let m :: M44 Float = (identity & translation .~ V3 0 1 0)
                   !*! (identity & _m33 .~ fromQuaternion (axisAngle (V3 0 1 0) currTime''))

  liftIO $ with (transpose m) (binder . castPtr)
handleUniform _name _binder = return ()

main :: IO ()
main = runApp ?? () $ do
  vulkanData <- initialize
  object <- withData vulkanData load
  vp <- liftIO $ newIORef (identity :: M44 Float)
  let rdata = RunData {
    vulkanData = vulkanData,
    objects = [object],
    viewProjection = vp
  }
  withData rdata $ callCC $ \exit -> do
    renderAction <- cycle <$> forM [0 .. 2] createFlightAction
    let eventsAction = repeat $ handleSDLEvents [checkExit exit]
    let updateAction = repeat update
    sequence_ $ interleave [eventsAction, renderAction, updateAction]
  VK.deviceWaitIdle (vulkanData ^. vdDevice . deviceHandle)

  return ()

update :: (MonadIO io) => AppMonad io RunData r ()
update = do
  vd <- asks vulkanData
  windowSize <- SDL.vkGetDrawableSize (vd ^. vdWindow . windowHandle)
  let cameraPos = V3 0 0 (-2)
  let proj :: M44 Float = perspective (pi / 2) (fromIntegral (windowSize^._x) / fromIntegral (windowSize^._y)) 0.1 10
  let view :: M44 Float =  identity --(identity & _m33 .~ fromQuaternion (axisAngle (V3 0 1 0) currTime)) 
                        !*! (identity & translation .~ cameraPos)
  vp <- asks viewProjection
  vp $=! (proj !*! view)

createFlightAction :: (MonadIO io) => Int -> AppMonad io RunData r (AppMonad io RunData r2 ())
createFlightAction i = do
  vd <- asks vulkanData
  let commandBufferAllocateInfo =
        (zero :: VK.CommandBufferAllocateInfo)
          { VK.commandPool = vd ^. vdDevice . deviceQueue . graphicsCommandPool,
            VK.level = VK.COMMAND_BUFFER_LEVEL_PRIMARY,
            VK.commandBufferCount = 1
          }
  commandBuffer <- V.head <$> VK.withCommandBuffers (vd ^. vdDevice . deviceHandle) commandBufferAllocateInfo (resourceN $ "command buffer " ++ show i)
  let fenceInfo = (zero :: VK.FenceCreateInfo '[]) {VK.flags = VK.FENCE_CREATE_SIGNALED_BIT}
  fence <- VK.withFence (vd ^. vdDevice . deviceHandle) fenceInfo Nothing $ resourceN $ "fence " ++ show i
  imageAvailableSemaphore <- VK.withSemaphore (vd ^. vdDevice . deviceHandle) (zero :: VK.SemaphoreCreateInfo '[]) Nothing $ resourceN $ "image available semaphore " ++ show i
  renderFinishedSemaphore <- VK.withSemaphore (vd ^. vdDevice . deviceHandle) (zero :: VK.SemaphoreCreateInfo '[]) Nothing $ resourceN $ "render finished semaphore " ++ show i
  return $ callCC $ \skip -> do
    vd <- asks vulkanData
    objs <- asks objects
    void $ VK.waitForFences (vd ^. vdDevice . deviceHandle) [fence] True maxBound
    imageIndex <- checkRecreateSwapChain skip $ acquireNextImage (vd ^. vdDevice) (vd ^. vdSwapChain) imageAvailableSemaphore
    VK.resetFences (vd ^. vdDevice . deviceHandle) [fence]
    VK.resetCommandBuffer commandBuffer zero
    framebuffers <- getResource (vd ^. vdFramebuffer . framebufferHandle)
    let framebuffer = framebuffers ^?! ix (fromIntegral imageIndex)
    let renderPass = vd ^. vdRenderPass . renderPassHandle
    windowSize <- SDL.vkGetDrawableSize (vd ^. vdWindow . windowHandle)
    record objs commandBuffer renderPass framebuffer $ getViewPortScissor $ VK.Extent2D{VK.width = fromIntegral $ windowSize ^. _x, VK.height = fromIntegral $ windowSize ^. _y}
    submit (vd ^. vdDevice) commandBuffer imageAvailableSemaphore renderFinishedSemaphore fence
    checkRecreateSwapChain skip $ present (vd ^. vdDevice) (vd ^. vdSwapChain) renderFinishedSemaphore imageIndex
    return ()

handleSDLEvents :: (MonadIO io) => [SDL.Event -> AppMonad io d r ()] -> AppMonad io d r ()
handleSDLEvents handlers = do
  events <- liftIO SDL.pollEvents
  sequence_ (handlers <*> events)

checkExit :: (MonadIO io) => (() -> AppMonad io d r ()) -> SDL.Event -> AppMonad io d r ()
checkExit exit event = case SDL.eventPayload event of
  SDL.QuitEvent -> exit ()
  _ -> return ()

checkRecreateSwapChain :: (MonadIO io) => (() -> AppMonad io RunData r ()) -> IO a -> AppMonad io RunData r a
checkRecreateSwapChain skip action = do
  vd <- asks vulkanData
  res <- liftIO $ try action
  case res of
    Left (VK.VulkanException VK.ERROR_OUT_OF_DATE_KHR) -> do
      withData vd recreateSwapChain
      skip ()
      error "Could not escape from swapchain recreation"
    Right x -> return x
    Left x -> liftIO $ throwIO x

recordObject :: (MonadIO io) => Object -> VK.CommandBuffer -> AppMonad io RunData r ()
recordObject obj commandBuffer = do
  cmdBindPipeline commandBuffer (obj ^. objectPipeline)
  cmdBindBuffers commandBuffer (obj ^. objectShaderInfo) (obj ^.. objectBuffers . traverse . bufferHandle)
  let pushConstants = obj ^.. objectShaderInfo . shaderInfoModules . traverse . shaderRepPushConstantMap . traverse
  forM_ pushConstants $ \pc -> do
    let binder = cmdSetPushConstantPtr commandBuffer (obj ^. objectPipeline) VK.SHADER_STAGE_VERTEX_BIT pc
    handleUniform (pc ^. shaderPushConstantName) binder

  case obj ^. objectDrawInformation of
    Left indexBuffer -> do
      cmdBindIndexBuffer commandBuffer indexBuffer
      VK.cmdDrawIndexed commandBuffer (fromIntegral $ indexBuffer ^. indexBufferCount) 1 0 0 0
    Right count -> do
      let vertexCount = fromIntegral count
      VK.cmdDraw commandBuffer vertexCount 1 0 0

record :: (MonadIO io) => [Object] -> VK.CommandBuffer -> VK.RenderPass -> VK.Framebuffer -> (VK.Viewport, VK.Rect2D) -> AppMonad io RunData r ()
record objects commandBuffer renderPass framebuffer (viewport, scissor) = do
  let commandBeginInfo = (zero :: VK.CommandBufferBeginInfo '[])
  VK.useCommandBuffer commandBuffer commandBeginInfo $ do
    let renderPassBeginInfo =
          (zero :: VK.RenderPassBeginInfo '[])
            { VK.renderPass = renderPass,
              VK.framebuffer = framebuffer,
              VK.renderArea = scissor,
              VK.clearValues =
                [ VK.Color $ VK.Float32 0 0 0 1,
                  VK.DepthStencil $ VK.ClearDepthStencilValue 1 0
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
  let filePath = "assets/Box.glb"
  fileContent <- liftIO $ BS.readFile filePath

  (gltf, chunks) <- case runParser glbParser filePath fileContent of
    Left err -> error $ "Error parsing GLTF: " ++ show err
    Right glb@GLB {chunks = chunks} -> do
      gltf <- loadGLTF glb
      let dataChunks = map chunkData $ filter ((== "BIN") . chunkType) chunks
      return (gltf, dataChunks)

  let shaderInfo = createShaderInfoFromGLTF [vertexShader', fragmentShader'] gltf 0 0
  let pipelineConfig = PipelineConfiguration{
                      _pipelineCullMode = VK.CULL_MODE_NONE,
                      _pipelineDepthTest = True
                    }
  pipeline <- uncurry Pipeline <$> createPipelineFromShaderInfo (device ^. deviceHandle) shaderInfo pipelineConfig (renderPass ^. renderPassHandle)

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
  ms <- createMemoryState (device ^. deviceHandle) (physicalDevice ^. physicalDeviceHandle) (device ^. deviceQueue . graphicsQueue) (device ^. deviceQueue.graphicsFamilyIndex)
  
  swapChain <- uncurry3 SwapChain <$> createSwapChain (physicalDevice ^. physicalDeviceHandle) (device ^. deviceHandle) (device ^. deviceQueue) (windowSurface ^. surfaceHandle)
  depthImage <- createImage (device^.deviceHandle) VK.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT VK.FORMAT_D32_SFLOAT 600 (Just 600) Nothing
  (depthBinding, ms') <- bindImage VK.MEMORY_PROPERTY_DEVICE_LOCAL_BIT depthImage ms
  depthImageView <- createImageView (device ^. deviceHandle) depthImage VK.FORMAT_D32_SFLOAT VK.IMAGE_ASPECT_DEPTH_BIT
  let depthBuffer = DepthBuffer {
      _depthBufferHandle = depthImage,
      _depthBufferView = depthImageView,
      _depthBufferMemory = depthBinding
    }
  renderPass <- RenderPass <$> createRenderPass (device ^. deviceHandle) (swapChain ^. swapChainInfo) (Just VK.FORMAT_D32_SFLOAT)
  imageViews <- getResource $ swapChain ^. swapChainImageViews
  framebuffers <- Framebuffer <$> createFramebuffers (device ^. deviceHandle) (renderPass ^. renderPassHandle) imageViews (swapChain ^. swapChainInfo) (Just depthImageView)
  memoryAllocator <- liftIO $ newIORef ms'
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
        _vdMemoryAllocator = memoryAllocator,
        _vdDepthBuffer = Just depthBuffer
      }

vertexShader :: ShaderDescription
vertexShader =
  [vert|
    #version 450

    layout(location = 0) out vec3 fragColor;
    layout(location = 1) out vec3 fragNormal;
    layout(location = 2) out vec3 fragPos;

    layout(location = 0) in vec3 POSITION;
    layout(location = 1) in vec3 NORMAL;

    layout(push_constant) uniform PushConstant {
        mat4 pv;
        mat4 m;
    } pc;

    void main() {
        gl_Position = pc.pv * pc.m * vec4(POSITION, 1);
        fragPos = (pc.m * vec4(POSITION, 1)).xyz;
        fragNormal = NORMAL;
        fragColor = vec3(0.8, 0.8, 0.8);
    }
  |]

fragmentShader :: ShaderDescription
fragmentShader =
  [frag|
    #version 450

    layout(location = 0) in vec3 fragColor;
    layout(location = 1) in vec3 fragNormal;
    layout(location = 2) in vec3 fragPos;

    layout(location = 0) out vec4 outColor;

    void main() {
        vec3 lightPos = vec3(1, 1, 0);
        vec3 lightDirection = normalize(lightPos - fragPos);

        //float ambientStrength = 0.1;
        //float diffuseStrength = 0.5 * abs(dot(fragNormal, lightDirection));
        //outColor.rgb = fragColor.rgb * (ambientStrength + diffuseStrength);
        //outColor.a = 1;
        outColor = vec4(abs(fragNormal), 1);
    }
  |]