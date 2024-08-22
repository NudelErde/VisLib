{-# LANGUAGE LambdaCase #-}

module VisLib.Buffer.ObjectFile (parseObjectFile, parseObjectFile') where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List
import VisLib.Base
import Data.Bifunctor
import Data.List.Split

parseObjectFile' :: String -> ComputationIO (BufferDescription, [Float], [Int])
parseObjectFile' = liftIO . readFile >=> (liftComputationIO . parseObjectFile)

parseObjectFile :: String -> Computation (BufferDescription, [Float], [Int])
parseObjectFile content = do
  let l = lines content
  let l' = l
  let vertexPositions = filter ("v " `isPrefixOf`) l'
  let vertexNormals = filter ("vn " `isPrefixOf`) l'
  let vertexTextureCoords = filter ("vt " `isPrefixOf`) l'
  let faces = filter ("f " `isPrefixOf`) l'
  vertexPositions' <-
    mapM
      ( \s -> case words s of
          "v" : x : y : z : w : _ -> return (read x, read y, read z, read w)
          "v" : x : y : z : _ -> return (read x, read y, read z, 1)
          _ -> throwError $ "Invalid vertex position (" ++ show s ++ ")"
      )
      vertexPositions
  vertexNormals' <-
    mapM
      ( \s -> case words s of
          "vn" : x : y : z : _ -> return (read x, read y, read z)
          _ -> throwError $ "Invalid vertex normal (" ++ show s ++ ")"
      )
      vertexNormals
  vertexTextureCoords' <-
    mapM
      ( \s -> case words s of
          "vt" : _ : _ : _ : _ -> throwError $ "Weirdo? Who uses 3D texture coordinates? lmao (" ++ show s ++ ")"
          "vt" : u : v : _ -> return (read u, read v)
          "vt" : u : _ -> return (read u, 0)
          _ -> throwError $ "Invalid vertex texture coordinate (" ++ show s ++ ")"
      )
      vertexTextureCoords
  faces' <-
    mapM
      ( \s -> case words s of
          "f" : v1 : v2 : v3 : v4 : _ -> return (v1, v2, v3, Just v4)
          "f" : v1 : v2 : v3 : _ -> return (v1, v2, v3, Nothing)
          _ -> throwError $ "Invalid face (" ++ show s ++ ")"
      )
      faces
  buildData vertexPositions' vertexTextureCoords' vertexNormals' faces'

buildData :: [(Float, Float, Float, Float)] -> [(Float, Float)] -> [(Float, Float, Float)] -> [(String, String, String, Maybe String)] -> Computation (BufferDescription, [Float], [Int])
buildData vertexPositions [] [] faces = do
  let parseVertexIndex :: String -> Computation Int
      parseVertexIndex s = case reads s of
        [(i, "")] -> return i
        _ -> throwError $ "Invalid vertex index (" ++ show s ++ ")"
  faces' <- mapM
      ( \case
          (v1, v2, v3, Nothing) -> do
            a <- parseVertexIndex v1
            b <- parseVertexIndex v2
            c <- parseVertexIndex v3
            return [(a, b, c)]
          (v1, v2, v3, Just v4) -> do
            a <- parseVertexIndex v1
            b <- parseVertexIndex v2
            c <- parseVertexIndex v3
            d <- parseVertexIndex v4
            return [(a, b, c), (c, d, a)]
      )
      faces
  let faces'' = join faces'
  let vertexData = vertexPositions >>= \(x, y, z, w) -> [x, y, z, w]
  let indexData = faces'' >>= \(a, b, c) -> [a - 1, b - 1, c - 1]
  return (BufferDescription [BufferAttribute 4 1] ArrayOfStruct, vertexData, indexData)

buildData vertexPosition [] vertexNormals faces = do
  let parseVertexIndex :: String -> Computation (Int, Int)
      parseVertexIndex s = do
        let [a, b] = splitOn "//" s
        case reads a of
          [(i, "")] -> case reads b of
            [(j, "")] -> return (i, j)
            _ -> throwError $ "Invalid vertex index (" ++ show s ++ ")"
          _ -> throwError $ "Invalid vertex index (" ++ show s ++ ")"

  faces' <- mapM
      ( \case
          (v1, v2, v3, Nothing) -> do
            a <- parseVertexIndex v1
            b <- parseVertexIndex v2
            c <- parseVertexIndex v3
            return [(a, b, c)]
          (v1, v2, v3, Just v4) -> do
            a <- parseVertexIndex v1
            b <- parseVertexIndex v2
            c <- parseVertexIndex v3
            d <- parseVertexIndex v4
            return [(a, b, c), (c, d, a)]
      )
      faces
  let faces'' = join faces'
  let realIndices = faces'' >>= \(x, y, z) -> [x, y, z]
  let uniqueIndices = nub realIndices
  let vertexData = map (bimap ((vertexPosition !!) . subtract 1) ((vertexNormals !!) . subtract 1)) uniqueIndices
  indexData <- mapM (\a -> case elemIndex a uniqueIndices of
    Just i -> return i
    Nothing -> throwError "Invalid index") realIndices
  let vertexData' = vertexData >>= \((x, y, z, w), (nx, ny, nz)) -> [x, y, z, w, nx, ny, nz]
  return (BufferDescription [BufferAttribute 4 1, BufferAttribute 3 1] ArrayOfStruct, vertexData', indexData)

buildData vertexPosition vertexTextureCoords [] faces = do
  let parseVertexIndex :: String -> Computation (Int, Int)
      parseVertexIndex s = do
        let [a, b] = splitOn "/" s
        case reads a of
          [(i, "")] -> case reads b of
            [(j, "")] -> return (i, j)
            _ -> throwError $ "Invalid vertex index (" ++ show s ++ ")"
          _ -> throwError $ "Invalid vertex index (" ++ show s ++ ")"

  faces' <- mapM
      ( \case
          (v1, v2, v3, Nothing) -> do
            a <- parseVertexIndex v1
            b <- parseVertexIndex v2
            c <- parseVertexIndex v3
            return [(a, b, c)]
          (v1, v2, v3, Just v4) -> do
            a <- parseVertexIndex v1
            b <- parseVertexIndex v2
            c <- parseVertexIndex v3
            d <- parseVertexIndex v4
            return [(a, b, c), (c, d, a)]
      )
      faces
  let faces'' = join faces'
  let realIndices = faces'' >>= \(x, y, z) -> [x, y, z]
  let uniqueIndices = nub realIndices
  let vertexData = map (bimap ((vertexPosition !!) . subtract 1) ((vertexTextureCoords !!) . subtract 1)) uniqueIndices
  indexData <- mapM (\a -> case elemIndex a uniqueIndices of
    Just i -> return i
    Nothing -> throwError "Invalid index") realIndices
  let vertexData' = vertexData >>= \((x, y, z, w), (tx, ty)) -> [x, y, z, w, tx, ty]
  return (BufferDescription [BufferAttribute 4 1, BufferAttribute 2 1] ArrayOfStruct, vertexData', indexData)

buildData vertexPosition vertexTextureCoords vertexNormals faces = do
  let parseVertexIndex :: String -> Computation (Int, Int, Int)
      parseVertexIndex s = do
        let [a, b, c] = splitOn "/" s
        case reads a of
          [(i, "")] -> case reads b of
            [(j, "")] -> case reads c of
              [(k, "")] -> return (i, j, k)
              _ -> throwError $ "Invalid vertex index (" ++ show s ++ ")"
            _ -> throwError $ "Invalid vertex index (" ++ show s ++ ")"
          _ -> throwError $ "Invalid vertex index (" ++ show s ++ ")"

  faces' <- mapM
      ( \case
          (v1, v2, v3, Nothing) -> do
            a <- parseVertexIndex v1
            b <- parseVertexIndex v2
            c <- parseVertexIndex v3
            return [(a, b, c)]
          (v1, v2, v3, Just v4) -> do
            a <- parseVertexIndex v1
            b <- parseVertexIndex v2
            c <- parseVertexIndex v3
            d <- parseVertexIndex v4
            return [(a, b, c), (c, d, a)]
      )
      faces
  let faces'' = join faces'
  let realIndices = faces'' >>= \(x, y, z) -> [x, y, z]
  let uniqueIndices = nub realIndices
  let vertexData = map (\(a,b,c) -> (vertexPosition !! (a - 1), vertexTextureCoords !! (b-1), vertexNormals !! (c-1))) uniqueIndices
  let vertexData' = vertexData >>= \((x, y, z, w), (tx, ty), (nx, ny, nz)) -> [x, y, z, w, tx, ty, nx, ny, nz]
  indexData <- mapM (\a -> case elemIndex a uniqueIndices of
    Just i -> return i
    Nothing -> throwError "Invalid index") realIndices
  return (BufferDescription [BufferAttribute 4 1, BufferAttribute 2 1, BufferAttribute 3 1] ArrayOfStruct, vertexData', indexData)