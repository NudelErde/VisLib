{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveLift #-}

module VisLib.Vulkan.Shader where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import VisLib.App
import VisLib.Loader.GLTF
import VisLib.Vulkan.Vulkan hiding (ShaderDescription)
import qualified Vulkan as VK
import Vulkan.Zero
import Data.Function
import Language.Haskell.TH.Syntax (Lift(..))

data ShaderLocationInformation = ShaderLocationInformation
  { _shaderLocationName :: Maybe String,
    _shaderLocationIndex :: Int,
    _shaderLocationType :: VK.Format
    } deriving (Show, Eq)
  
instance Lift ShaderLocationInformation where
  liftTyped (ShaderLocationInformation name index fmt) =
    let fmt' = show fmt
    in [|| ShaderLocationInformation
          $$(liftTyped name)
          $$(liftTyped index)
          (read fmt') ||]

data ShaderDescription = ShaderDescription
  { _shaderName :: Maybe String,
    _shaderSource :: ByteString,
    _shaderType :: VK.ShaderStageFlags,
    _shaderNameResolve :: String -> Maybe ShaderLocationInformation,
    _shaderPushConstants :: [VK.PushConstantRange],
    _shaderMainFunction :: Maybe ByteString
  }

data ShaderRepresentation = ShaderRepresentation
  { _shaderRepModule :: VK.ShaderModule,
    _shaderRepStage :: VK.PipelineShaderStageCreateInfo '[],
    _shaderRepNameResolve :: String -> Maybe ShaderLocationInformation,
    _shaderRepPushConstants :: [VK.PushConstantRange]
  } 
instance Show ShaderRepresentation where
  show ShaderRepresentation {..} =
    "ShaderRepresentation { _shaderRepModule = " ++ show _shaderRepModule ++
    ", _shaderRepStage = " ++ show _shaderRepStage ++
    ", _shaderRepNameResolve = [function]" ++
    ", _shaderRepPushConstants = " ++ show _shaderRepPushConstants ++
    " }"

data ShaderInfo = ShaderInfo
  { _shaderInfoModules :: Vector ShaderRepresentation,
    _shaderInfoVertexInputBindingDescription :: Vector (VK.VertexInputBindingDescription, Maybe Int),
    _shaderInfoVertexInputAttributeDescription :: Vector VK.VertexInputAttributeDescription,
    _shaderInfoPushConstants :: Vector VK.PushConstantRange
  } deriving (Show)

$(makeLenses ''ShaderDescription)
$(makeLenses ''ShaderRepresentation)
$(makeLenses ''ShaderInfo)
$(makeLenses ''ShaderLocationInformation)

compileShaderRepresentation :: MonadIO io => VK.Device -> ShaderDescription -> AppMonad io d r ShaderRepresentation
compileShaderRepresentation device ShaderDescription {..} = do
  shaderModule <- createShaderModule device _shaderSource
  let shaderStageCreateInfo = (zero :: VK.PipelineShaderStageCreateInfo '[])
        { VK.stage = _shaderType,
          VK.module' = shaderModule,
          VK.name = fromMaybe "main" _shaderMainFunction
        }
  return ShaderRepresentation
    { _shaderRepModule = shaderModule,
      _shaderRepStage = shaderStageCreateInfo,
      _shaderRepNameResolve = _shaderNameResolve,
      _shaderRepPushConstants = _shaderPushConstants
    }

createShaderInfoFromGLTF :: [ShaderRepresentation] -> GLTF -> Int -> Int -> ShaderInfo
createShaderInfoFromGLTF shaderRepresentations gltf meshIndex primitiveIndex =
  let mesh = meshes gltf !! meshIndex
      primitive = primitives mesh !! primitiveIndex & attributes
      attributeDescriptions = zipWith (\i g -> (i, snd $ head g, fst <$> g)) [0::Int ..] $ groupBy ((==) `on` snd) $ nub $ do
        (name, accessorIndex) <- primitive
        location <- catMaybes $ (shaderRepresentations ^.. traverse . shaderRepNameResolve) <*> pure name
        let accessor = accessors gltf !! accessorIndex
        let bufferViewIndex = fromJust (bufferView accessor)
        return ((location, accessor), bufferViewIndex)
      attributeDescriptions' = do
        (binding, _bufferViewIndex, attributes) <- attributeDescriptions
        (location, accessor) <- attributes
        let offset = fromMaybe 0 $ byteOffset accessor
        return (zero :: VK.VertexInputAttributeDescription) {
            VK.binding = fromIntegral binding,
            VK.location = fromIntegral $ location^.shaderLocationIndex,
            VK.format = location^.shaderLocationType,
            VK.offset = fromIntegral offset
          }
      bindingDescriptions = do
        (binding, bufferViewIndex, attributes) <- attributeDescriptions
        let defaultStride = sum $ map ?? attributes $ \(_, accessor) ->
              let componentType' = fromJust $ componentType accessor
                  accessorType = fromJust $ type' accessor
               in sizeOfComponentType componentType' * sizeOfType accessorType
        let stride = bufferViews gltf !! bufferViewIndex & viewByteStride
        return ((zero :: VK.VertexInputBindingDescription) {
            VK.binding = fromIntegral binding,
            VK.stride = fromIntegral $ fromMaybe defaultStride stride,
            VK.inputRate = VK.VERTEX_INPUT_RATE_VERTEX
          }, Just bufferViewIndex)
      pushConstantRangeSize VK.PushConstantRange {VK.size = size} = fromIntegral size
      pushConstantOffsets = scanl (+) 0 $ map pushConstantRangeSize $ shaderRepresentations ^.. traverse . shaderRepPushConstants . traverse
      pushConstants = zipWith ?? pushConstantOffsets ?? shaderRepresentations ^.. traverse . shaderRepPushConstants . traverse $ 
        \offset range -> (range :: VK.PushConstantRange)
                { VK.offset = offset}
  in ShaderInfo
    { _shaderInfoModules = V.fromList shaderRepresentations,
      _shaderInfoVertexInputBindingDescription = V.fromList bindingDescriptions,
      _shaderInfoVertexInputAttributeDescription = V.fromList attributeDescriptions',
      _shaderInfoPushConstants = V.fromList pushConstants
    }
