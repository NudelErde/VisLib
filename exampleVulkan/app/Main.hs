{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main where

import qualified SDL
import Control.Monad.Cont
import Control.Monad.IO.Class
import Control.Monad
import qualified SDL.Video.Vulkan as SDL
import qualified Vulkan as VK
import qualified Vulkan.CStruct.Extends as VK
import qualified Vulkan.Exception as VK
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (pattern (:&), pattern(::&))
import Vulkan.Zero
import Foreign hiding (void)
import Data.Vector (Vector)
-- import Vulkan.Utils.ShaderQQ.GLSL.Shaderc
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Lens
import VisLib.App
import VisLib.Vulkan.Vulkan hiding (ShaderDescription, present)
import qualified VisLib.Vulkan.Vulkan as Vulkan hiding (ShaderDescription)
import Control.Exception
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (uncurry3)
import VisLib.Vulkan.Memory
import VisLib.Vulkan.Shader
import VisLib.Vulkan.ShaderTH
import VisLib.Loader.GLTF
import qualified Data.ByteString as BS
import Text.Megaparsec

-- data FlightData = FlightData {
  -- _imageAvailable :: VK.Semaphore,
  -- _renderFinished :: VK.Semaphore,
  -- _fence :: VK.Fence,
  -- _commandBuffer :: VK.CommandBuffer
-- }
-- 
-- data MeshRep = MeshRep {
  -- _vertexBuffer :: Vector VK.Buffer,
  -- _vertexBufferMemory :: VK.DeviceMemory,
  -- _vertexCount :: Word32,
  -- _instanceCount :: Word32,
  -- _renderAction :: VK.CommandBuffer -> VK.Pipeline -> App () ()
-- }
-- 
-- data AppData = AppData {
  -- _vulkanInfo :: VulkanInfo,
  -- _flightData :: Vector FlightData,
  -- _exit :: App () (),
  -- _next :: AppData -> App () (),
  -- _frameIndex :: Int,
  -- _currentFlightData :: FlightData,
  -- _meshes :: Vector MeshRep
-- }
-- 
-- type App r a = AppMonad IO AppData r a
-- 
-- data VulkanInfo = VulkanInfo {
  -- _window :: SDL.Window,
  -- _instance' :: VK.Instance,
  -- _surface :: VK.SurfaceKHR,
  -- _device :: VK.Device,
  -- _physicalDevice :: VK.PhysicalDevice,
  -- _queues :: Queues,
  -- _swapChain :: VK.SwapchainKHR,
  -- _swapChainInfo :: VK.SwapchainCreateInfoKHR '[],
  -- _releaseSwapChainData :: App () (),
  -- _images :: Vector VK.Image,
  -- _imageViews :: Vector VK.ImageView,
  -- _framebuffers :: Vector VK.Framebuffer,
  -- _pipeline :: VK.Pipeline,
  -- _pipelineLayout :: VK.PipelineLayout,
  -- _renderPass :: VK.RenderPass
-- }
-- 
-- $(makeLenses ''VulkanInfo)
-- $(makeLenses ''AppData)
-- $(makeLenses ''MeshRep)
-- $(makeLenses ''FlightData)
-- 
vulkanWindow :: SDL.WindowConfig
vulkanWindow = SDL.defaultWindow {
  SDL.windowGraphicsContext = SDL.VulkanContext,
  SDL.windowResizable = True,
  SDL.windowInitialSize = SDL.V2 600 600
}

-- main :: IO ()
-- main = (`runApp` AppData {
      -- _vulkanInfo = undefined,
      -- _flightData = undefined,
      -- _exit = undefined,
      -- _frameIndex = undefined,
      -- _currentFlightData = undefined,
      -- _next = undefined,
      -- _meshes = undefined
    -- }) $ do
    -- vulkanInfo@VulkanInfo{_device=device} <- initialize
    -- 
    -- flightData <- createFlightData device (vulkanInfo ^. queues) 2
    -- mesh <- loadTriangle
    -- callCC $ \exit -> do
      -- (loop, appData) <- label $ AppData {
        -- _vulkanInfo = vulkanInfo,
        -- _flightData = flightData,
        -- _exit = exit (),
        -- _frameIndex = 0,
        -- _currentFlightData = flightData V.! 0,
        -- _next = undefined,
        -- _meshes = V.singleton mesh
      -- }
      -- changeData (const appData{_next = loop}) $ do
        -- apploop
        -- appData' <- ask
        -- loop $ appData' &~ do
          -- frameIndex %= (+1)
          -- currentFlightData .= (flightData V.! ((appData' ^. frameIndex) `mod` V.length flightData))
    -- VK.deviceWaitIdle device

-- apploop :: App () ()
-- apploop = do
  -- VK.SwapchainCreateInfoKHR{imageExtent = imageExtent} <- view (vulkanInfo . swapChainInfo)
  -- vk <- view vulkanInfo
  -- curr <- view currentFlightData
  -- meshes' <- view meshes
-- 
  -- events <- SDL.pollEvents
  -- checkQuit events
  -- checkRebuildSwapChain events
-- 
  -- void $ VK.waitForFences (vk^.device) (V.fromList [curr^.fence]) True maxBound
-- 
  -- (_ , imageIndex) <- tryRebuildSwapChain $ VK.acquireNextImageKHR (vk^.device) (vk^.swapChain) maxBound (curr^.imageAvailable) VK.NULL_HANDLE
  -- VK.resetFences (vk^.device) (V.fromList [curr^.fence])
  -- VK.resetCommandBuffer (curr^.commandBuffer) zero
  -- recordCommandBuffer meshes' (curr^.commandBuffer) (vk^.pipeline) (vk^.renderPass) (vk^?!framebuffers.ix (fromIntegral imageIndex)) (getViewPortScissor imageExtent)
  -- submit
  -- present imageIndex
-- 
  -- return ()

-- submit :: App r ()
-- submit = do
  -- v <- view vulkanInfo
  -- curr <- view currentFlightData
  -- let submitInfo = (zero :: VK.SubmitInfo '[]) {
    -- VK.commandBuffers = V.fromList [VK.commandBufferHandle (curr^.commandBuffer)],
    -- VK.waitSemaphores = V.fromList [curr^.imageAvailable],
    -- VK.signalSemaphores = V.fromList [curr^.renderFinished],
    -- VK.waitDstStageMask = V.fromList [VK.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
  -- }
  -- VK.queueSubmit (v ^. queues . graphicsQueue) (V.fromList [VK.SomeStruct submitInfo]) (curr^.fence)
-- 
-- present :: Word32 -> App () ()
-- present index = do
  -- v <- view vulkanInfo
  -- curr <- view currentFlightData
  -- tryRebuildSwapChain $ Vulkan.present (v^.swapChain) (curr^.renderFinished) (v ^. queues . presentQueue) index
-- 
-- tryRebuildSwapChain :: IO a -> App () a
-- tryRebuildSwapChain action = do
  -- res <- lift $ lift $ try action
  -- case res of
    -- Left (VK.VulkanException VK.ERROR_OUT_OF_DATE_KHR) -> do
      -- rebuildSwapChain
    -- Right x -> return x
    -- Left x -> lift $ lift $ throwIO x

-- rebuildSwapChain :: App () r
-- rebuildSwapChain = do
  -- v <- view vulkanInfo
  -- VK.deviceWaitIdle (v^.device)
-- 
  -- v^.releaseSwapChainData
-- 
  -- liftIO $ putStrLn "Rebuilding swap chain"
-- 
  -- (swapChain', swapChainInfo', images', imageViews', releaseSwapChain') <- createSwapChain (v^.physicalDevice) (v^.device) (v^.queues) (v^.surface)
  -- (framebuffers', releaseFramebuffers') <- createFramebuffers (v^.device) (v^.renderPass) imageViews' swapChainInfo'
  -- currentData <- ask
  -- (currentData^.next) $ currentData &~ do
    -- vulkanInfo.swapChain .= swapChain'
    -- vulkanInfo.swapChainInfo .= swapChainInfo'
    -- vulkanInfo.releaseSwapChainData .= (releaseFramebuffers' >> releaseSwapChain')
    -- vulkanInfo.images .= images'
    -- vulkanInfo.imageViews .= imageViews'
    -- vulkanInfo.framebuffers .= framebuffers'
  -- error "somehow rebuildSwapChain failed"

-- recordCommandBuffer :: Vector MeshRep -> VK.CommandBuffer -> VK.Pipeline -> VK.RenderPass -> VK.Framebuffer -> (VK.Viewport, VK.Rect2D) -> App () ()
-- recordCommandBuffer meshes' commandBuffer pipeline renderPass framebuffer (viewport, scissor) = do
  -- let commandBeginInfo = (zero :: VK.CommandBufferBeginInfo '[])
  -- VK.useCommandBuffer commandBuffer commandBeginInfo $ do
    -- let renderPassBeginInfo = (zero :: VK.RenderPassBeginInfo '[]) {
      -- VK.renderPass = renderPass,
      -- VK.framebuffer = framebuffer,
      -- VK.renderArea = scissor,
      -- VK.clearValues = V.fromList [
        -- VK.Color $ VK.Float32 0 0 0 1
      -- ]
    -- }
    -- VK.cmdUseRenderPass commandBuffer renderPassBeginInfo VK.SUBPASS_CONTENTS_INLINE $ do
      -- VK.cmdSetViewport commandBuffer 0 (V.fromList [viewport])
      -- VK.cmdSetScissor commandBuffer 0 (V.fromList [scissor])
      -- forM_ meshes' $ \m -> do
        -- (m^.renderAction) commandBuffer pipeline

-- createFlightData :: VK.Device -> Queues -> Int -> App r (Vector FlightData)
-- createFlightData device queues count = do
  -- flightData <- forM [0..count-1] $ \i -> do
    -- fence <- VK.withFence device ((zero :: VK.FenceCreateInfo '[]) {VK.flags = VK.FENCE_CREATE_SIGNALED_BIT}) Nothing $ resourceN ("fence " ++ show i)
    -- imageAvailableSemaphore <- VK.withSemaphore device (zero :: VK.SemaphoreCreateInfo '[]) Nothing $ resourceN ("image available semaphore " ++ show i)
    -- renderFinishedSemaphore <- VK.withSemaphore device (zero :: VK.SemaphoreCreateInfo '[]) Nothing $ resourceN ("render finished semaphore " ++ show i)
    -- let commandBufferAllocateInfo = (zero :: VK.CommandBufferAllocateInfo) {
          -- VK.commandPool = queues ^. graphicsCommandPool,
          -- VK.level = VK.COMMAND_BUFFER_LEVEL_PRIMARY,
          -- VK.commandBufferCount = 1
        -- }
    -- commandBuffers <- VK.withCommandBuffers device commandBufferAllocateInfo $ resourceN ("command buffer " ++ show i)
    -- return $ FlightData {
      -- _imageAvailable = imageAvailableSemaphore,
      -- _renderFinished = renderFinishedSemaphore,
      -- _fence = fence,
      -- _commandBuffer = V.head commandBuffers
    -- }
  -- return $ V.fromList flightData

-- loadTriangle :: App () MeshRep
-- loadTriangle = do
  -- v <- view vulkanInfo
  -- let vertexData :: [Float] = [ 0.5,   0.5, 0.0, 1, 0, 0
                              --  ,-0.5,  0.5, 0.0, 0, 1, 0
                              --  , 0.5, -0.5, 0.0, 0, 0, 1
                              --  ,-0.5, -0.5, 0.0, 1, 1, 0
                              --  ,-0.5,  0.5, 0.0, 0, 1, 0
                              --  , 0.5, -0.5, 0.0, 0, 0, 1
                              --  , 0.5,  0.5,   0
                              --  , 0.0,  0.5,   0
                              --  ,-0.5,  0.5,   0
                              --  , 0.5, -0.5,   0
                              --  , 0.0, -0.5,   0
                              --  ,-0.5, -0.5,   0
                              --  , 0.5,  0.0,   0
                              --  ,-0.5,  0.0,   0
                              --  , 0.0,  0.0,   0]
  -- vertexBuffer <- createBuffer (v^.device) VK.BUFFER_USAGE_VERTEX_BUFFER_BIT (sum $ sizeOf <$> vertexData)
  -- (vertexBuffer, vertexMemory) <- createBuffer (v^.device) (v^.physicalDevice) VK.MEMORY_PROPERTY_HOST_COHERENT_BIT VK.BUFFER_USAGE_VERTEX_BUFFER_BIT (sum $ sizeOf <$> vertexData)
  -- writeHostBufferS (v^.device) vertexMemory vertexData
  -- let vertexCount = 6
  -- let instanceCount = 9
  -- let render :: VK.CommandBuffer -> VK.Pipeline -> App () ()
      -- render commandBuffer pipeline = do
          -- frame <- view frameIndex
          -- let time :: Float = fromIntegral frame / 60
          -- VK.cmdBindPipeline commandBuffer VK.PIPELINE_BIND_POINT_GRAPHICS pipeline
          -- VK.cmdBindVertexBuffers commandBuffer 0 (V.fromList [vertexBuffer, vertexBuffer]) (V.fromList [0, 36*4])
          -- pipelineLayout <- view (vulkanInfo . pipelineLayout)
          -- liftIO $ with time $ \t -> do
            -- VK.cmdPushConstants commandBuffer pipelineLayout VK.SHADER_STAGE_VERTEX_BIT 0 (fromIntegral $ sizeOf time) (castPtr t)
          -- VK.cmdDraw commandBuffer vertexCount instanceCount 0 0
          -- return ()
  -- return $ MeshRep {
    -- _vertexBuffer = V.singleton vertexBuffer,
    -- _vertexBufferMemory = vertexMemory,
    -- _vertexCount = vertexCount,
    -- _instanceCount = instanceCount,
    -- _renderAction = render
  -- }

-- loadTriangleShader :: App () ShaderDescription
-- loadTriangleShader = do
--   let vertexShader = [vert|
--     #version 450

--     layout(location = 0) out vec3 fragColor;

--     layout(location = 0) in vec3 pos;

--     layout(location = 1) in vec3 color;

--     layout(location = 2) in vec3 offset;

--     layout(push_constant) uniform PushConstants {
--       float rotation;
--     } pc;

--     void main() {
--         vec3 rotatedPos = vec3(
--           pos.x * cos(pc.rotation) - pos.y * sin(pc.rotation),
--           pos.x * sin(pc.rotation) + pos.y * cos(pc.rotation),
--           pos.z
--         ) * 0.25;
--         gl_Position = vec4(rotatedPos + offset, 1);
--         fragColor = abs(rotatedPos + offset);
--     }
--   |]

--   let fragmentShader = [frag|
--     #version 450

--     layout(location = 0) in vec3 fragColor;

--     layout(location = 0) out vec4 outColor;

--     void main() {
--         outColor = vec4(fragColor, 1.0);
--     }
--   |]
--   let bindingDescription = [(zero :: VK.VertexInputBindingDescription) {
--     VK.binding = 0,
--     VK.stride = 24,
--     VK.inputRate = VK.VERTEX_INPUT_RATE_VERTEX
--   }, (zero :: VK.VertexInputBindingDescription) {
--     VK.binding = 1,
--     VK.stride = 12,
--     VK.inputRate = VK.VERTEX_INPUT_RATE_INSTANCE
--   }]
--   let attributeDescription = [(zero :: VK.VertexInputAttributeDescription) {
--     VK.binding = 0,
--     VK.location = 0,
--     VK.format = VK.FORMAT_R32G32B32_SFLOAT,
--     VK.offset = 0
--   },(zero :: VK.VertexInputAttributeDescription) {
--     VK.binding = 0,
--     VK.location = 1,
--     VK.format = VK.FORMAT_R32G32B32_SFLOAT,
--     VK.offset = 12
--   },(zero :: VK.VertexInputAttributeDescription) {
--     VK.binding = 1,
--     VK.location = 2,
--     VK.format = VK.FORMAT_R32G32B32_SFLOAT,
--     VK.offset = 0
--   }]
--   let pushConstants = [(zero :: VK.PushConstantRange) {
--     VK.offset = 0,
--     VK.size = 4,
--     VK.stageFlags = VK.SHADER_STAGE_VERTEX_BIT
--   }]
--   return $ ShaderDescription {
--     _vertexShaderSource = vertexShader,
--     _fragmentShaderSource = fragmentShader,
--     _vertexInputBindingDescription = V.fromList bindingDescription,
--     _vertexInputAttributeDescription = V.fromList attributeDescription,
--     _pushConstants = V.fromList pushConstants
--   }

-- initialize :: App () VulkanInfo
-- initialize = do
  -- resourceN "SDL" (SDL.initialize [SDL.InitVideo, SDL.InitEvents]) (const SDL.quit)
  -- SDL.vkLoadLibrary Nothing
  -- window <- resourceN "window" (SDL.createWindow "Vulkan Example" vulkanWindow) SDL.destroyWindow
  -- swcExtensions <- getSurfaceCreationExtensions window
  -- debugCreateInfo <- getDebugCreateInfo
  -- inst <- VK.withInstance ((zero :: VK.InstanceCreateInfo '[]) {
        -- VK.enabledExtensionNames = V.fromList (swcExtensions ++ ["VK_EXT_debug_utils"]),
        -- VK.enabledLayerNames = V.fromList ["VK_LAYER_KHRONOS_validation"]
      -- }
      -- ::& VK.ValidationFeaturesEXT (V.fromList [VK.VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]) (V.fromList [])
      -- :& debugCreateInfo
      -- :& ()) Nothing $ resourceN "instance"
  -- void $ resourceN "debug messenger"
        -- (VK.createDebugUtilsMessengerEXT inst debugCreateInfo Nothing)
        -- (\m -> VK.destroyDebugUtilsMessengerEXT inst m Nothing)
  -- windowSurface <- resourceN "window surface"
                    -- (fmap VK.SurfaceKHR $ SDL.vkCreateSurface window $ castPtr $ VK.instanceHandle inst)
                    -- (\surf -> VK.destroySurfaceKHR inst surf Nothing)
  -- Just physicalDevice <- choosePhysicalDevice ["VK_KHR_swapchain"] inst
  -- (device, queues) <- createLogicalDevice physicalDevice windowSurface ["VK_KHR_swapchain"]
  -- (swapChain, swapChainInfo, images, imageViews, releaseSwapChain) <- createSwapChain physicalDevice device queues windowSurface
  -- renderPass <- createRenderPass device swapChainInfo
-- 
  -- shaderDesc <- loadTriangleShader
  -- shaderDesc' <- compileShaderDescription device shaderDesc
-- 
  -- (pipeline, pipelineLayout) <- createPipeline device shaderDesc' renderPass
  -- (framebuffers, releaseFramebuffers) <- createFramebuffers device renderPass imageViews swapChainInfo
  -- return $ VulkanInfo {
    -- _window = window,
    -- _instance' = inst,
    -- _device = device,
    -- _physicalDevice = physicalDevice,
    -- _queues = queues,
    -- _surface = windowSurface,
    -- _swapChain = swapChain,
    -- _swapChainInfo = swapChainInfo,
    -- _releaseSwapChainData = releaseFramebuffers >> releaseSwapChain,
    -- _images = images,
    -- _imageViews = imageViews,
    -- _framebuffers = framebuffers,
    -- _pipeline = pipeline,
    -- _pipelineLayout = pipelineLayout,
    -- _renderPass = renderPass
  -- }

-- checkQuit :: [SDL.Event] -> App () ()
-- checkQuit events = do
  -- -- exit <- view exit
  -- -- forM_ events $ \event -> do
    -- case SDL.eventPayload event of
      -- SDL.QuitEvent -> exit
      -- _ -> return ()

-- checkRebuildSwapChain :: [SDL.Event] -> App () ()
-- checkRebuildSwapChain = mapM_ $ \event -> do
    -- case SDL.eventPayload event of
      -- SDL.WindowSizeChangedEvent{} -> rebuildSwapChain
      -- _ -> return ()


data Window = Window {
  _windowHandle :: SDL.Window
} deriving (Eq, Show)

data Instance = Instance {
  _instanceHandle :: VK.Instance
} deriving (Eq, Show)

data PhysicalDevice = PhysicalDevice {
  _physicalDeviceHandle :: VK.PhysicalDevice
} deriving (Eq, Show)

data Device = Device {
  _deviceHandle :: VK.Device,
  _deviceQueue :: Queues
} deriving (Eq, Show)

data Surface = Surface {
  _surfaceHandle :: VK.SurfaceKHR
} deriving (Eq, Show)

data Shader = Shader {
  _shaderSource :: ByteString
}

data SwapChain = SwapChain {
  _swapChainHandle :: ResourceMutable VK.SwapchainKHR,
  _swapChainInfo :: VK.SwapchainCreateInfoKHR '[],
  _swapChainImageViews :: ResourceMutable (Vector VK.ImageView)
}

data Framebuffer = Framebuffer {
  _framebufferHandle :: ResourceMutable (Vector VK.Framebuffer)
}

data RenderPass = RenderPass {
  _renderPassHandle :: VK.RenderPass
}

data Pipeline = Pipeline {
  _pipeline :: VK.Pipeline,
  _pipelineLayout :: VK.PipelineLayout
}

data VulkanData = VulkanData {
  _vdWindow :: Window,
  _vdInstance :: Instance,
  _vdPhysicalDevice :: PhysicalDevice,
  _vdDevice :: Device,
  _vdSurface :: Surface,
  _vdSwapChain :: SwapChain,
  _vdFramebuffer :: Framebuffer,
  _vdRenderPass :: RenderPass,
  _vdMemoryAllocator :: ResourceMutable MemoryState
}

$(makeLenses ''Window)
$(makeLenses ''Instance)
$(makeLenses ''PhysicalDevice)
$(makeLenses ''Device)
$(makeLenses ''Surface)
$(makeLenses ''Shader)
$(makeLenses ''SwapChain)
$(makeLenses ''Framebuffer)
$(makeLenses ''RenderPass)
$(makeLenses ''Pipeline)
$(makeLenses ''VulkanData)

main :: IO ()
main = runApp ?? () $ do
  vulkanData <- initialize
  withData vulkanData load
  return ()

load :: MonadIO io => AppMonad io VulkanData r ()
load = do
  device <- view vdDevice
  vertexShader' <- compileShaderRepresentation (device^.deviceHandle) vertexShader
  fragmentShader' <- compileShaderRepresentation (device^.deviceHandle) fragmentShader
  let filePath = "../assets/Box.glb"
  fileContent <- liftIO $ BS.readFile filePath

  (gltf, chunks)<- case runParser glbParser filePath fileContent of
    Left err -> error $ "Error parsing GLTF: " ++ show err
    Right glb@GLB{chunks=chunks} -> do
      gltf <- loadGLTF glb
      let dataChunks = map chunkData $ filter ((=="BIN") . chunkType) chunks
      return (gltf, dataChunks)

  let shaderInfo = createShaderInfoFromGLTF [vertexShader', fragmentShader'] gltf 0 0
  liftIO $ print shaderInfo
  
  return ()

initialize :: MonadIO io => AppMonad io d r VulkanData
initialize = do
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]
  SDL.vkLoadLibrary Nothing
  window <- Window <$> resourceN "window" (SDL.createWindow "Vulkan Example" vulkanWindow) SDL.destroyWindow
  swcExtensions <- getSurfaceCreationExtensions (window ^. windowHandle)
  debugCreateInfo <- getDebugCreateInfo
  inst <- Instance <$> VK.withInstance ((zero :: VK.InstanceCreateInfo '[]) {
        VK.enabledExtensionNames = V.fromList (swcExtensions ++ ["VK_EXT_debug_utils"]),
        VK.enabledLayerNames = V.fromList ["VK_LAYER_KHRONOS_validation"]
      }
      ::& VK.ValidationFeaturesEXT (V.fromList [VK.VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]) (V.fromList [])
      :& debugCreateInfo
      :& ()) Nothing (resourceN "instance")
  void $ resourceN "debug messenger"
        (VK.createDebugUtilsMessengerEXT (inst ^. instanceHandle) debugCreateInfo Nothing)
        (\m -> VK.destroyDebugUtilsMessengerEXT (inst ^. instanceHandle) m Nothing)
  windowSurface <- Surface <$> resourceN "window surface"
                    (fmap VK.SurfaceKHR $ SDL.vkCreateSurface (window ^. windowHandle) $ castPtr $ VK.instanceHandle $ inst ^. instanceHandle)
                    (\surf -> VK.destroySurfaceKHR (inst ^. instanceHandle) surf Nothing)
  physicalDevice <- PhysicalDevice . fromJust <$> choosePhysicalDevice ["VK_KHR_swapchain"] (inst ^. instanceHandle)
  device <- uncurry Device <$> createLogicalDevice (physicalDevice ^. physicalDeviceHandle) (windowSurface ^. surfaceHandle) ["VK_KHR_swapchain"]
  swapChain <- uncurry3 SwapChain <$> createSwapChain (physicalDevice ^. physicalDeviceHandle) (device ^. deviceHandle) (device ^. deviceQueue) (windowSurface ^. surfaceHandle)
  renderPass <- RenderPass <$> createRenderPass (device ^. deviceHandle) (swapChain ^. swapChainInfo)
  imageViews <- getResource $ swapChain^.swapChainImageViews
  framebuffers <- Framebuffer <$> createFramebuffers (device ^. deviceHandle) (renderPass ^. renderPassHandle) imageViews (swapChain ^. swapChainInfo)
  memoryAllocator <- pureResourceM =<< createMemoryState (device ^. deviceHandle) (physicalDevice ^. physicalDeviceHandle) (device ^. deviceQueue . graphicsQueue)

  return $ VulkanData {
    _vdWindow = window,
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
vertexShader = [vert|
    #version 450

    layout(location = 0) out vec3 fragColor;

    layout(location = 0) in vec3 POSITION;

    void main() {
        gl_Position = vec4(POSITION, 1);
        fragColor = vec3(1, 0, 0);
    }
  |]

fragmentShader :: ShaderDescription
fragmentShader = [frag|
    #version 450

    layout(location = 0) in vec3 fragColor;

    layout(location = 0) out vec4 outColor;

    void main() {
        outColor = vec4(fragColor, 1.0);
    }
  |]