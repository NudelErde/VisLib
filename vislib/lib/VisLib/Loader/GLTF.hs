{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE RecordWildCards #-}
module VisLib.Loader.GLTF where
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad.Error.Class
import Control.Monad (guard, forM_, forM, join)
import Control.Applicative (Alternative)
import VisLib.Parse hiding (Parser)
import qualified VisLib.Parse as Parse
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
import Text.Megaparsec.Byte.Binary
import Data.Functor (void)
import Text.Megaparsec as P
import Data.List
import Control.Monad.IO.Class
import Data.Aeson hiding ((<?>))
import Data.Maybe
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as M
import Data.Traversable.WithIndex

type Parser a = Parse.Parser ByteString a

data GLB = GLB {
  chunks :: [GLBChunk]
}

data GLBChunk = GLBChunk
  { chunkType :: String
  , chunkData :: ByteString
  } deriving (Show)

data GLTFScene = GLTFScene {
  sceneNodes :: [Int]
}

instance FromJSON GLTFScene where
  parseJSON (Object v) = GLTFScene
    <$> v .:? "nodes" .!= []
  parseJSON _ = mempty

data GLTFNode = GLTFNode {
  nodeName :: Maybe String,
  translation :: Maybe [Float],
  rotation :: Maybe [Float],
  scale :: Maybe [Float],
  children :: Maybe [Int],
  matrix :: Maybe [Float],
  mesh :: Maybe Int,
  skin :: Maybe Int,
  camera :: Maybe Int
}

instance FromJSON GLTFNode where
  parseJSON (Object v) = GLTFNode
    <$> v .:? "name"
    <*> v .:? "translation"
    <*> v .:? "rotation"
    <*> v .:? "scale"
    <*> v .:? "children"
    <*> v .:? "matrix"
    <*> v .:? "mesh"
    <*> v .:? "skin"
    <*> v .:? "camera"
  parseJSON _ = mempty

data GLTFPrimitive = GLTFPrimitive {
  attributes :: [(String, Int)],
  indices :: Maybe Int,
  material :: Maybe Int,
  mode :: Maybe Int
}

instance FromJSON GLTFPrimitive where
  parseJSON (Object v) = GLTFPrimitive .
    M.toList <$> (v .:? "attributes" .!= M.empty)
    <*> v .:? "indices"
    <*> v .:? "material"
    <*> v .:? "mode"
  parseJSON _ = mempty

data GLTFMesh = GLTFMesh {
  meshName :: Maybe String,
  primitives :: [GLTFPrimitive]
}

instance FromJSON GLTFMesh where
  parseJSON (Object v) = GLTFMesh
    <$> v .:? "name"
    <*> v .:? "primitives" .!= []
  parseJSON _ = mempty

data GLTFMaterial = GLTFMaterial

instance FromJSON GLTFMaterial where
  parseJSON (Object v) = return GLTFMaterial
  parseJSON _ = mempty

data GLTFTexture = GLTFTexture

instance FromJSON GLTFTexture where
  parseJSON (Object v) = return GLTFTexture
  parseJSON _ = mempty

data GLTFImage = GLTFImage

instance FromJSON GLTFImage where
  parseJSON (Object v) = return GLTFImage
  parseJSON _ = mempty

data GLTFSampler = GLTFSampler

instance FromJSON GLTFSampler where
  parseJSON (Object v) = return GLTFSampler
  parseJSON _ = mempty

data GLTFAccessor = GLTFAccessor {
  bufferView :: Maybe Int,
  byteOffset :: Maybe Int,
  componentType :: Maybe Int,
  accessorCount :: Maybe Int,
  type' :: Maybe String,
  normalized :: Maybe Bool,
  max :: Maybe [Float],
  min :: Maybe [Float]
} deriving (Eq, Show)

instance FromJSON GLTFAccessor where
  parseJSON (Object v) = GLTFAccessor
    <$> v .:? "bufferView"
    <*> v .:? "byteOffset"
    <*> v .:? "componentType"
    <*> v .:? "count"
    <*> v .:? "type"
    <*> v .:? "normalized"
    <*> v .:? "max"
    <*> v .:? "min"
  parseJSON _ = mempty

data GLTFBuffer = GLTFBuffer {
  bufferByteLength :: Maybe Int,
  bufferUri :: Maybe String
}

instance FromJSON GLTFBuffer where
  parseJSON (Object v) = GLTFBuffer
    <$> v .:? "byteLength"
    <*> v .:? "uri"
  parseJSON _ = mempty

data GLTFBufferView = GLTFBufferView {
  buffer :: Maybe Int,
  viewByteOffset :: Maybe Int,
  viewByteLength :: Maybe Int,
  viewByteStride :: Maybe Int,
  target :: Maybe Int
}

instance FromJSON GLTFBufferView where
  parseJSON (Object v) = GLTFBufferView
    <$> v .:? "buffer"
    <*> v .:? "byteOffset"
    <*> v .:? "byteLength"
    <*> v .:? "byteStride"
    <*> v .:? "target"
  parseJSON _ = mempty

data GLTFAnimation = GLTFAnimation

instance FromJSON GLTFAnimation where
  parseJSON (Object v) = return GLTFAnimation
  parseJSON _ = mempty

data GLTFSkin = GLTFSkin

instance FromJSON GLTFSkin where
  parseJSON (Object v) = return GLTFSkin
  parseJSON _ = mempty

data GLTFCamera = GLTFCamera

instance FromJSON GLTFCamera where
  parseJSON (Object v) = return GLTFCamera
  parseJSON _ = mempty

data GLTFLight = GLTFLight

instance FromJSON GLTFLight where
  parseJSON (Object v) = return GLTFLight
  parseJSON _ = mempty

data GLTF = GLTF {
  scene :: Maybe Int,
  scenes :: [GLTFScene],
  nodes :: [GLTFNode],
  meshes :: [GLTFMesh],
  materials :: [GLTFMaterial],
  textures :: [GLTFTexture],
  images :: [GLTFImage],
  samplers :: [GLTFSampler],
  accessors :: [GLTFAccessor],
  buffers :: [GLTFBuffer],
  bufferViews :: [GLTFBufferView],
  animations :: [GLTFAnimation],
  skins :: [GLTFSkin],
  cameras :: [GLTFCamera],
  lights :: [GLTFLight],
  extensionsUsed :: [String],
  extensionsRequired :: [String]
}

instance FromJSON GLTF where
  parseJSON (Object v) = GLTF
    <$> v .:? "scene"
    <*> v .:? "scenes" .!= []
    <*> v .:? "nodes" .!= []
    <*> v .:? "meshes" .!= []
    <*> v .:? "materials" .!= []
    <*> v .:? "textures" .!= []
    <*> v .:? "images" .!= []
    <*> v .:? "samplers" .!= []
    <*> v .:? "accessors" .!= []
    <*> v .:? "buffers" .!= []
    <*> v .:? "bufferViews" .!= []
    <*> v .:? "animations" .!= []
    <*> v .:? "skins" .!= []
    <*> v .:? "cameras" .!= []
    <*> v .:? "lights" .!= []
    <*> v .:? "extensionsUsed" .!= []
    <*> v .:? "extensionsRequired" .!= []
  parseJSON _ = mempty

parseChunk :: Parser GLBChunk
parseChunk = do
  -- 4 byte long bytestring
  size <- word32le
  chunkType' <- BSC.unpack . BS.filter (/=0) . BS.pack <$> count 4 anySingle
  chunkData' <- BS.pack <$> count (fromIntegral size) anySingle

  return $ GLBChunk chunkType' chunkData'

glbParser :: Parser GLB
glbParser = do
  expect word32le 0x46546C67 <?> "magic number"
  expect word32le 2 <?> "version"
  _length <- word32le
  chunks' <- many parseChunk

  return GLB{chunks = chunks'}

loadGLTF :: (MonadIO m, Monad m) => GLB -> m GLTF
loadGLTF GLB{chunks=chunks} = do
  let Just jsonChunk = find ((=="JSON") . chunkType) chunks
  case eitherDecodeStrict (chunkData jsonChunk) of
    Right x -> return x
    Left err -> error $ "Error parsing JSON chunk: " ++ show err

getDataOfBuffer :: GLTF -> [ByteString] -> Int -> Maybe ByteString
getDataOfBuffer gltf@GLTF{buffers=buffers} glbBuffers bufferIndex = do
  buffer <- buffers !? bufferIndex
  case bufferUri buffer of
    Just uri -> do
      error "Buffer URI not supported at the moment"
    Nothing -> do
      bufferData <- glbBuffers !? bufferIndex
      bufferLength <- bufferByteLength buffer
      return $ BS.take (fromIntegral bufferLength) bufferData

getBuffersForNode :: GLTF -> [ByteString] -> Int -> [ByteString]
getBuffersForNode gltf@GLTF{bufferViews=bufferViews, nodes=nodes, meshes=meshes, accessors=accessors} glbBuffers nodeIndex = fromMaybe [] $ do
  node <- nodes !? nodeIndex
  meshIndex <- mesh node
  mesh <- meshes !? meshIndex
  buffers <- fmap (nub.join) $ forM (primitives mesh) $ \prim -> do
    let attributeAccessors = snd <$> attributes prim
    let accessorIndices = attributeAccessors ++ maybeToList (indices prim)
    forM accessorIndices $ \accessorIndex -> do
      accessor <- accessors !? accessorIndex
      bufferViewIndex <- bufferView accessor
      bufferView' <- bufferViews !? bufferViewIndex
      buffer bufferView'
  forM buffers $ getDataOfBuffer gltf glbBuffers

getAttributesForNode :: GLTF -> Int -> [(Int, [(String, Int)])]
getAttributesForNode _gltf@GLTF{nodes=nodes, meshes=meshes} nodeIndex = fromMaybe [] $ do
  node <- nodes !? nodeIndex
  meshIndex <- mesh node
  mesh <- meshes !? meshIndex
  iforM (primitives mesh) $ \i prim -> do
    let attributeNames = attributes prim
    let attributeNames' = maybe [] (\i -> [("INDICES",i)]) (indices prim) ++ attributeNames
    return (i, attributeNames')


getAttributesForMesh :: GLTF -> Int -> [(Int, [(String, Int)])]
getAttributesForMesh _gltf@GLTF{meshes=meshes} meshIndex = fromMaybe [] $ do
  mesh <- meshes !? meshIndex
  iforM (primitives mesh) $ \i prim -> do
    let attributeNames = attributes prim
    let attributeNames' = maybe [] (\i -> [("INDICES",i)]) (indices prim) ++ attributeNames
    return (i, attributeNames')

getLayoutForAccessor :: GLTF -> [ByteString] -> Int -> Maybe (Int, Int, Int, Int, Int)
getLayoutForAccessor _gltf@GLTF{accessors=accessors, bufferViews=bufferViews} _glbBuffers accessorIndex = do
  accessor <- accessors !? accessorIndex
  let offset = fromMaybe 0 (byteOffset accessor)
  count <- accessorCount accessor
  elementSize <- do
    componentType' <- componentType accessor
    type' <- type' accessor
    let size = sizeOfComponentType componentType' * sizeOfType type'
    return size

  bufferViewIndex <- bufferView accessor
  bufferView' <- bufferViews !? bufferViewIndex
  bufferIndex <- buffer bufferView'
  let byteOffset = fromMaybe 0 (viewByteOffset bufferView')
  let byteStride = fromMaybe elementSize (viewByteStride bufferView')
  let realOffset = byteOffset + offset
  return (realOffset, count, byteStride, elementSize, bufferIndex)

sizeOfComponentType :: Int -> Int
sizeOfComponentType 5120 = 1 -- signed byte
sizeOfComponentType 5121 = 1 -- unsigned byte
sizeOfComponentType 5122 = 2 -- signed short
sizeOfComponentType 5123 = 2 -- unsigned short
sizeOfComponentType 5125 = 4 -- unsigned int
sizeOfComponentType 5126 = 4 -- float
sizeOfComponentType _ = error "Unknown component type"

sizeOfType :: String -> Int
sizeOfType "SCALAR" = 1
sizeOfType "VEC2" = 2
sizeOfType "VEC3" = 3
sizeOfType "VEC4" = 4
sizeOfType "MAT2" = 4
sizeOfType "MAT3" = 9
sizeOfType "MAT4" = 16
sizeOfType _ = error "Unknown type"

getIndexBuffer :: GLTF -> Int -> Int -> Maybe (Int, Int, Int)
getIndexBuffer gltf meshIndex primitiveIndex = do
  mesh <- meshes gltf !? meshIndex
  primitive <- primitives mesh !? primitiveIndex
  indexAccessorIndex <- indices primitive
  accessor <- accessors gltf !? indexAccessorIndex
  guard $ type' accessor == Just "SCALAR"
  let count = fromJust (accessorCount accessor)
  let indexType = sizeOfComponentType (fromJust (componentType accessor))
  let bufferViewIndex = fromJust (bufferView accessor)
  return (bufferViewIndex, count, indexType)

getVertexCount :: GLTF -> Int -> Int -> Int
getVertexCount gltf meshIndex primitiveIndex =
  let mesh = meshes gltf !! meshIndex
      primitive = primitives mesh !! primitiveIndex
      attribute = head (attributes primitive)
      accessorIndex = snd attribute
      accessor = accessors gltf !! accessorIndex
      count = fromJust (accessorCount accessor)
  in count

testGLTF :: IO (GLTF, [ByteString])
testGLTF = do
  -- Load a GLTF file
  let filePath = "../assets/Box.glb"
  fileContent <- BS.readFile filePath

  -- Parse the GLTF file
  case runParser glbParser filePath fileContent of
    Left err -> error $ "Error parsing GLTF: " ++ show err
    Right glb@GLB{chunks=chunks} -> do
      forM_ chunks $ \GLBChunk{chunkType=chunkType} -> do
        putStrLn $ "Chunk Type: " ++ show chunkType
      gltf <- loadGLTF glb
      let dataChunks = map chunkData $ filter ((=="BIN") . chunkType) chunks
      return (gltf, dataChunks)
