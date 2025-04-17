{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module VisLib.Buffer.ObjectFile where

import Control.Applicative (asum)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.State.Class
import Data.Bifunctor
import Data.Composition
import qualified Data.HashMap as HM
import Data.List
import Data.List.Split
import Data.Maybe (catMaybes, isJust, mapMaybe, fromJust)
import Text.Megaparsec (MonadParsec (eof), label, many, noneOf, optional, sepBy1, try, (<|>))
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal, float)
import VisLib.Base
import VisLib.Parse
{-
-- parseObjectFile' :: FilePath -> [String] -> ComputationIO (BufferDescription, [Float], HM.Map String [Int])
-- parseObjectFile' = liftIO . readFile >=> liftComputationIO .: parseObjectFile

parseVertex :: StatefullParser String (Float, Float, Float, Float)
parseVertex = label "vertex position" $ do
  void $ string "v "
  x <- float
  void $ char ' '
  y <- float
  void $ char ' '
  z <- float
  void $ char ' '
  w <- try float <|> return 1
  void newline
  return (x, y, z, w)

parseNormal :: StatefullParser String (Float, Float, Float)
parseNormal = label "vertex normal" $ do
  void $ string "vn "
  x <- float
  void $ char ' '
  y <- float
  void $ char ' '
  z <- float
  void newline
  return (x, y, z)

parseTextureCoord :: StatefullParser String (Float, Float)
parseTextureCoord = label "vertex texture coordinate" $ do
  void $ string "vt "
  u <- float
  void $ char ' '
  v <- float
  void newline
  return (u, v)

parseGroup :: StatefullParser String String
parseGroup = label "group" $ do
  void $ string "g "
  grp <- many (noneOf "\n")
  put grp
  void newline
  return grp

parseFaceVertex :: StatefullParser String (Int, Maybe Int, Maybe Int)
parseFaceVertex = do
  v <- decimal
  t <- optional (char '/' *> decimal)
  n <- optional (char '/' *> decimal)
  return (v, t, n)

parseFace :: StatefullParser String (String, [(Int, Maybe Int, Maybe Int)])
parseFace = label "face" $ do
  grp <- get
  void $ string "f "
  face <- sepBy1 parseFaceVertex (char ' ')
  guard $ length face >= 3
  void newline
  return (grp, face)

data ObjectFileData
  = Face String [(Int, Maybe Int, Maybe Int)]
  | Vertex (Float, Float, Float, Float)
  | Normal (Float, Float, Float)
  | TextureCoord (Float, Float)
  | Group String

mergeGroups :: [String] -> (BufferDescription, [Float], HM.Map String [Int]) -> Computation (BufferDescription, [Float], [Int])
mergeGroups reqGroup (bufDesc, vertexData, groups) =
  let groups' = HM.filterWithKey (\k _ -> k `elem` reqGroup) groups
      indices = join $ HM.elems groups'
  in return (bufDesc, vertexData, indices)

parseObjectFile :: String -> [String] -> Computation (BufferDescription, [Float], HM.Map String [Int])
parseObjectFile = parseObjectFileSource Nothing

parseObjectFileSource :: Maybe String -> String -> [String] -> Computation (BufferDescription, [Float], HM.Map String [Int])
parseObjectFileSource source content groupFilter = do
  parsed <-
    parseStateComp source content "" $
      many $
        asum
          [ uncurry Face <$> parseFace,
            Vertex <$> parseVertex,
            Normal <$> parseNormal,
            TextureCoord <$> parseTextureCoord,
            Group <$> parseGroup
          ]
          <* eof
  let vertices = mapMaybe (\case Vertex v -> Just v; _ -> Nothing) parsed
  let normals = mapMaybe (\case Normal n -> Just n; _ -> Nothing) parsed
  let textureCoords = mapMaybe (\case TextureCoord t -> Just t; _ -> Nothing) parsed
  let faces = mapMaybe (\case Face g f -> Just (g, f); _ -> Nothing) parsed
  let triangles = do
        (grp, s : verts) <- faces
        return $ (grp,) $ do
          (v1, v2) <- zip verts $ drop 1 verts
          return (s, v1, v2)
  let triangles' = HM.fromListWith (++) triangles
  let filterFunc = case groupFilter of
          [] -> const True
          _ -> (`elem` groupFilter)
  (indexData, vertexData) <- (`runStateT` []) $ forM (filter (filterFunc . fst) $ HM.toList triangles') $ \(grp, ts) -> do
    idxs <- forM ts $ \(v1, v2, v3) -> do
      let v1' = fromJust $ buildVertex v1 vertices textureCoords normals
      let v2' = fromJust $ buildVertex v2 vertices textureCoords normals
      let v3' = fromJust $ buildVertex v3 vertices textureCoords normals
      i1 <- findVertex v1'
      i2 <- findVertex v2'
      i3 <- findVertex v3'
      return [i1, i2, i3]
    return (grp, join idxs)
  guard $ vertexDataListValid vertexData
  bufDesc <- case vertexData of
    Position {} : _ -> return $ BufferDescription [BufferAttribute 4 1] ArrayOfStruct
    PosTex {} : _ -> return $ BufferDescription [BufferAttribute 4 1, BufferAttribute 2 1] ArrayOfStruct
    PosNorm {} : _ -> return $ BufferDescription [BufferAttribute 4 1, BufferAttribute 3 1] ArrayOfStruct
    PosTexNorm {} : _ -> return $ BufferDescription [BufferAttribute 4 1, BufferAttribute 2 1, BufferAttribute 3 1] ArrayOfStruct
    _ -> throwError "VertexData list is empty"
  let vertexData' = flattenVertexData =<< vertexData
  return (bufDesc, vertexData', HM.fromList indexData)

data VertexData
  = Position (Float, Float, Float, Float)
  | PosTex (Float, Float, Float, Float) (Float, Float)
  | PosNorm (Float, Float, Float, Float) (Float, Float, Float)
  | PosTexNorm (Float, Float, Float, Float) (Float, Float) (Float, Float, Float)
  deriving (Eq, Show)

vertexDataListValid :: [VertexData] -> Bool
vertexDataListValid [] = True
vertexDataListValid [_] = True
vertexDataListValid (Position {} : v@(Position {}) : vs) = vertexDataListValid (v : vs)
vertexDataListValid (PosTex {} : v@(PosTex {}) : vs) = vertexDataListValid (v : vs)
vertexDataListValid (PosNorm {} : v@(PosNorm {}) : vs) = vertexDataListValid (v : vs)
vertexDataListValid (PosTexNorm {} : v@(PosTexNorm {}) : vs) = vertexDataListValid (v : vs)
vertexDataListValid _ = False

flattenVertexData :: VertexData -> [Float]
flattenVertexData (Position (x, y, z, w)) = [x, y, z, w]
flattenVertexData (PosTex (x, y, z, w) (u, v)) = [x, y, z, w, u, v]
flattenVertexData (PosNorm (x, y, z, w) (nx, ny, nz)) = [x, y, z, w, nx, ny, nz]
flattenVertexData (PosTexNorm (x, y, z, w) (u, v) (nx, ny, nz)) = [x, y, z, w, u, v, nx, ny, nz]

findVertex :: (MonadState [VertexData] m) => VertexData -> m Int
findVertex v = do
  index <- gets (elemIndex v)
  case index of
    Just i -> return i
    Nothing -> do
      modify (++ [v])
      gets (subtract 1 . length)

buildVertex :: (Int, Maybe Int, Maybe Int) -> [(Float, Float, Float, Float)] -> [(Float, Float)] -> [(Float, Float, Float)] -> Maybe VertexData
buildVertex (n, Nothing, Nothing) positions _ _ = Position <$> positions !? (n - 1)
buildVertex (n, Just t, Nothing) positions texture _ = PosTex <$> (positions !? (n - 1)) <*> (texture !? (t - 1))
buildVertex (n, Nothing, Just m) positions _ normals = PosNorm <$> (positions !? (n - 1)) <*> (normals !? (m - 1))
buildVertex (n, Just t, Just m) positions texture normals = PosTexNorm <$> (positions !? (n - 1)) <*> (texture !? (t - 1)) <*> (normals !? (m - 1))

-}