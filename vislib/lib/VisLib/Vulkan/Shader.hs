{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module VisLib.Vulkan.Shader where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Function
import Data.List
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Haskell.TH.Syntax (Lift (..))
import VisLib.App
import VisLib.Loader.GLTF
import VisLib.Vulkan.Vulkan hiding (ShaderDescription)
import qualified Vulkan as VK
import Vulkan.Zero
import qualified Vulkan.CStruct.Extends as VK
import Data.Bits
import Control.Monad

data ShaderPushConstantDescription = ShaderPushConstantDescription
  {_shaderPushConstantName :: String,
    _shaderPushConstantSize :: Int,
    _shaderPushConstantOffset :: Int
  } deriving (Show, Eq, Lift)

data ShaderLocationInformation = ShaderLocationInformation
  { _shaderLocationName :: Maybe String,
    _shaderLocationIndex :: Int,
    _shaderLocationType :: VK.Format
  }
  deriving (Show, Eq)

instance Lift ShaderLocationInformation where
  liftTyped (ShaderLocationInformation name index fmt) =
    let fmt' = show fmt
     in [||
        ShaderLocationInformation
          $$(liftTyped name)
          $$(liftTyped index)
          (read fmt')
        ||]

data ShaderDescription = ShaderDescription
  { _shaderName :: Maybe String,
    _shaderSource :: ByteString,
    _shaderType :: VK.ShaderStageFlags,
    _shaderNameResolve :: String -> Maybe ShaderLocationInformation,
    _shaderPushConstants :: [VK.PushConstantRange],
    _shaderMainFunction :: Maybe ByteString,
    _shaderPushConstantMap :: [ShaderPushConstantDescription]
  }

data ShaderRepresentation = ShaderRepresentation
  { _shaderRepModule :: VK.ShaderModule,
    _shaderRepStage :: VK.SomeStruct VK.PipelineShaderStageCreateInfo,
    _shaderRepNameResolve :: String -> Maybe ShaderLocationInformation,
    _shaderRepPushConstants :: [VK.PushConstantRange],
    _shaderRepPushConstantMap :: [ShaderPushConstantDescription]
  }

instance Show ShaderRepresentation where
  show ShaderRepresentation {..} =
    "ShaderRepresentation { _shaderRepModule = "
      ++ show _shaderRepModule
      ++ ", _shaderRepStage = "
      ++ show _shaderRepStage
      ++ ", _shaderRepNameResolve = [function]"
      ++ ", _shaderRepPushConstants = "
      ++ show _shaderRepPushConstants
      ++ " }"

data ShaderInfo = ShaderInfo
  { _shaderInfoModules :: Vector ShaderRepresentation,
    _shaderInfoVertexInputBindingDescription :: Vector (VK.VertexInputBindingDescription, Maybe Int),
    _shaderInfoVertexInputAttributeDescription :: Vector VK.VertexInputAttributeDescription,
    _shaderInfoPushConstants :: Vector VK.PushConstantRange,
    _shaderInfoPrimitive :: VK.PrimitiveTopology
  }
  deriving (Show)

$(makeLenses ''ShaderDescription)
$(makeLenses ''ShaderRepresentation)
$(makeLenses ''ShaderInfo)
$(makeLenses ''ShaderLocationInformation)
$(makeLenses ''ShaderPushConstantDescription)

compileShaderRepresentation :: (MonadIO io) => VK.Device -> ShaderDescription -> AppMonad io d r ShaderRepresentation
compileShaderRepresentation device ShaderDescription {..} = do
  shaderModule <- createShaderModule device _shaderSource
  let shaderStageCreateInfo =
        (zero :: VK.PipelineShaderStageCreateInfo '[])
          { VK.stage = _shaderType,
            VK.module' = shaderModule,
            VK.name = fromMaybe "main" _shaderMainFunction
          }
  return
    ShaderRepresentation
      { _shaderRepModule = shaderModule,
        _shaderRepStage = VK.SomeStruct shaderStageCreateInfo,
        _shaderRepNameResolve = _shaderNameResolve,
        _shaderRepPushConstants = _shaderPushConstants,
        _shaderRepPushConstantMap = _shaderPushConstantMap
      }

createShaderInfoFromGLTF :: [ShaderRepresentation] -> GLTF -> Int -> Int -> ShaderInfo
createShaderInfoFromGLTF shaderRepresentations gltf meshIndex primitiveIndex =
  let mesh = meshes gltf !! meshIndex
      primitive = primitives mesh !! primitiveIndex
      attribs = attributes primitive
      attributeDescriptions = zipWith (\i g -> (i, snd $ head g, fst <$> g)) [0 :: Int ..] $ groupBy ((==) `on` snd) $ nub $ do
        (name, accessorIndex) <- attribs
        location <- catMaybes $ (shaderRepresentations ^.. traverse . shaderRepNameResolve) <*> pure name
        let accessor = accessors gltf !! accessorIndex
        let bufferViewIndex = fromJust (bufferView accessor)
        return ((location, accessor), bufferViewIndex)
      attributeDescriptions' = do
        (binding, _bufferViewIndex, attributes) <- attributeDescriptions
        (location, accessor) <- attributes
        let offset = fromMaybe 0 $ byteOffset accessor
        return
          (zero :: VK.VertexInputAttributeDescription)
            { VK.binding = fromIntegral binding,
              VK.location = fromIntegral $ location ^. shaderLocationIndex,
              VK.format = location ^. shaderLocationType,
              VK.offset = fromIntegral offset
            }
      bindingDescriptions = do
        (binding, bufferViewIndex, attributes) <- attributeDescriptions
        let defaultStride = sum $ map ?? attributes $ \(_, accessor) ->
              let componentType' = fromJust $ componentType accessor
                  accessorType = fromJust $ type' accessor
               in sizeOfComponentType componentType' * sizeOfType accessorType
        let stride = bufferViews gltf !! bufferViewIndex & viewByteStride
        return
          ( (zero :: VK.VertexInputBindingDescription)
              { VK.binding = fromIntegral binding,
                VK.stride = fromIntegral $ fromMaybe defaultStride stride,
                VK.inputRate = VK.VERTEX_INPUT_RATE_VERTEX
              },
            Just bufferViewIndex
          )
      pushConstantRangeSize VK.PushConstantRange {VK.size = size} = fromIntegral size
      pushConstantOffsets = scanl (+) 0 $ map pushConstantRangeSize $ shaderRepresentations ^.. traverse . shaderRepPushConstants . traverse
      pushConstants = zipWith ?? pushConstantOffsets ?? shaderRepresentations ^.. traverse . shaderRepPushConstants . traverse $
        \offset range ->
          (range :: VK.PushConstantRange)
            { VK.offset = offset
            }
      primitiveTopology = case fromMaybe 4 $ mode primitive of
        0 -> VK.PRIMITIVE_TOPOLOGY_POINT_LIST
        1 -> VK.PRIMITIVE_TOPOLOGY_LINE_LIST
        2 -> error "Line loop not supported at the moment"
        3 -> VK.PRIMITIVE_TOPOLOGY_LINE_STRIP
        4 -> VK.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        5 -> VK.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
        6 -> VK.PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
        n -> error $ "Unsupported primitive topology: " ++ show n

   in ShaderInfo
        { _shaderInfoModules = V.fromList shaderRepresentations,
          _shaderInfoVertexInputBindingDescription = V.fromList bindingDescriptions,
          _shaderInfoVertexInputAttributeDescription = V.fromList attributeDescriptions',
          _shaderInfoPushConstants = V.fromList pushConstants,
          _shaderInfoPrimitive = primitiveTopology
        }

createPipelineFromShaderInfo :: (MonadIO io) => VK.Device -> ShaderInfo -> VK.RenderPass -> AppMonad io d r (VK.Pipeline, VK.PipelineLayout)
createPipelineFromShaderInfo device si renderPass = do
  let layoutInfo = (zero :: VK.PipelineLayoutCreateInfo) {
    VK.pushConstantRanges = si ^. shaderInfoPushConstants
  }
  layout <- VK.withPipelineLayout device layoutInfo Nothing $ resourceN "pipeline layout"
  let pipelineCreateInfo = (zero :: VK.GraphicsPipelineCreateInfo '[]) {
    VK.stages = V.fromList $ si ^.. shaderInfoModules . traverse . shaderRepStage,
    VK.stageCount = fromIntegral $ V.length (si ^. shaderInfoModules),
    VK.vertexInputState = Just $ VK.SomeStruct (zero :: VK.PipelineVertexInputStateCreateInfo '[])
      { VK.vertexBindingDescriptions = V.fromList $ si ^.. shaderInfoVertexInputBindingDescription . traverse . _1,
        VK.vertexAttributeDescriptions = si ^. shaderInfoVertexInputAttributeDescription
      },
    VK.inputAssemblyState = Just $ (zero :: VK.PipelineInputAssemblyStateCreateInfo)
      { VK.topology = si ^. shaderInfoPrimitive,
        VK.primitiveRestartEnable = False
      },
    VK.viewportState = Just $ VK.SomeStruct (zero :: VK.PipelineViewportStateCreateInfo '[])
      { VK.viewportCount = 1,
        VK.scissorCount = 1
      },
    VK.rasterizationState = Just $ VK.SomeStruct (zero :: VK.PipelineRasterizationStateCreateInfo '[])
      { VK.depthClampEnable = False,
        VK.rasterizerDiscardEnable = False,
        VK.polygonMode = VK.POLYGON_MODE_FILL,
        VK.lineWidth = 1.0,
        VK.cullMode = VK.CULL_MODE_NONE,
        VK.frontFace = VK.FRONT_FACE_COUNTER_CLOCKWISE,
        VK.depthBiasEnable = False
      },
    VK.multisampleState = Just $ VK.SomeStruct (zero :: VK.PipelineMultisampleStateCreateInfo '[])
      { VK.sampleShadingEnable = False,
        VK.rasterizationSamples = VK.SAMPLE_COUNT_1_BIT
      },
    VK.colorBlendState = Just $ VK.SomeStruct (zero :: VK.PipelineColorBlendStateCreateInfo '[])
      { VK.logicOpEnable = False,
        VK.attachments = [ (zero :: VK.PipelineColorBlendAttachmentState)
              { VK.blendEnable = False,
                VK.colorWriteMask = VK.COLOR_COMPONENT_R_BIT .|. VK.COLOR_COMPONENT_G_BIT .|. VK.COLOR_COMPONENT_B_BIT .|. VK.COLOR_COMPONENT_A_BIT
              }
          ],
        VK.attachmentCount = 1
      },
    VK.dynamicState = Just $ (zero :: VK.PipelineDynamicStateCreateInfo)
      { VK.dynamicStates = [VK.DYNAMIC_STATE_VIEWPORT, VK.DYNAMIC_STATE_SCISSOR]
      },
    VK.layout = layout,
    VK.renderPass = renderPass,
    VK.subpass = 0
  }
  (_, pipelines) <- VK.withGraphicsPipelines device VK.NULL_HANDLE [VK.SomeStruct pipelineCreateInfo] Nothing $ resourceN "graphics pipeline"

  return (V.head pipelines, layout)

cmdBindBuffers :: (MonadIO io) => VK.CommandBuffer -> ShaderInfo -> [VK.Buffer] -> io ()
cmdBindBuffers commandBuffer shaderInfo buffers = do
  forM_ (shaderInfo ^.. shaderInfoVertexInputBindingDescription . traverse) $ \(VK.VertexInputBindingDescription{VK.binding = binding}, bufferViewIndex) -> do
    VK.cmdBindVertexBuffers commandBuffer (fromIntegral binding) [buffers !! fromJust bufferViewIndex] [0]