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

vulkanWindow :: SDL.WindowConfig
vulkanWindow =
  SDL.defaultWindow
    { SDL.windowGraphicsContext = SDL.VulkanContext,
      SDL.windowResizable = False,
      SDL.windowInitialSize = SDL.V2 600 600
    }

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

acquireNextImage :: (MonadIO io) => Device -> SwapChain -> VK.Semaphore -> AppMonad io d r Word32
acquireNextImage device swapChain semaphore = do
  swapChainHandle' <- getResource (swapChain ^. swapChainHandle)
  (_, imageIndex) <- VK.acquireNextImageKHR (device ^. deviceHandle) swapChainHandle' maxBound semaphore VK.NULL_HANDLE
  return imageIndex

submit :: (MonadIO io) => Device -> VK.CommandBuffer -> VK.Semaphore -> VK.Semaphore -> VK.Fence -> AppMonad io d r ()
submit device commandBuffer wait signal commandBufferFree = do
  let submitInfo =
        (zero :: VK.SubmitInfo '[])
          { VK.commandBuffers = V.fromList [VK.commandBufferHandle commandBuffer],
            VK.waitSemaphores = V.fromList [wait],
            VK.signalSemaphores = V.fromList [signal],
            VK.waitDstStageMask = V.fromList [VK.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
          }
  VK.queueSubmit (device ^. deviceQueue . graphicsQueue) (V.fromList [VK.SomeStruct submitInfo]) commandBufferFree

present :: (MonadIO io) => Device -> SwapChain -> VK.Semaphore -> Word32 -> AppMonad io d r ()
present device swapChain ready index = do
  swapChainHandle' <- getResource (swapChain ^. swapChainHandle)
  let presentInfo =
        (zero :: VK.PresentInfoKHR '[])
          { VK.swapchains = [swapChainHandle'],
            VK.imageIndices = [index],
            VK.waitSemaphores = [ready]
          }
  void $ VK.queuePresentKHR (device ^. deviceQueue . presentQueue) presentInfo
