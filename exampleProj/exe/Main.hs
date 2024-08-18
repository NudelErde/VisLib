{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Colour.RGBSpace
import Data.Functor
import Data.IORef
import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Linear.Matrix (M22, M33, M44, identity, transpose, (!!*), (!*!), (*!!), _m33, inv33)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import VisLib.Base
import VisLib.Buffer.GL
import VisLib.Shader.GL
import VisLib.Shader.Monad
import VisLib.Buffer.ObjectFile (parseObjectFile')

instance ShaderTypeable (RGB Float) where
  getType _ = vec4

instance ShaderTerm (RGB Float) where
  toOp (RGB r g b) = Lit vec4 $ "vec4(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ "1)"

instance UniformValue (RGB Float) where
  bindUniform loc (RGB r g b) = GL.uniform loc $= GL.Vertex4 r g b 1

vf4 :: Float -> Float -> Float -> Float -> V4 Float
vf4 = V4

vf3 :: Float -> Float -> Float -> V3 Float
vf3 = V3

rotate2 :: Float -> M22 Float
rotate2 angle = V2 (V2 (cos angle) (-sin angle)) (V2 (sin angle) (cos angle))

rotate4xy :: Float -> M44 Float
rotate4xy angle =
  V4
    (V4 (cos angle) (-sin angle) 0 0)
    (V4 (sin angle) (cos angle) 0 0)
    (V4 0 0 1 0)
    (V4 0 0 0 1)

rotate4yz :: Float -> M44 Float
rotate4yz angle =
  V4
    (V4 1 0 0 0)
    (V4 0 (cos angle) (-sin angle) 0)
    (V4 0 (sin angle) (cos angle) 0)
    (V4 0 0 0 1)

rotate4xz :: Float -> M44 Float
rotate4xz angle =
  V4
    (V4 (cos angle) 0 (sin angle) 0)
    (V4 0 1 0 0)
    (V4 (-sin angle) 0 (cos angle) 0)
    (V4 0 0 0 1)

perspective :: Float -> Float -> Float -> Float -> M44 Float
perspective fovy aspect near far =
  transpose $
    V4
      (V4 (f / aspect) 0 0 0)
      (V4 0 f 0 0)
      (V4 0 0 ((far + near) / (near - far)) (-1))
      (V4 0 0 ((2 * far * near) / (near - far)) 0)
  where
    f = 1 / tan (fovy / 2)

scale4 :: Float -> Float -> Float -> M44 Float
scale4 x y z =
  V4
    (V4 x 0 0 0)
    (V4 0 y 0 0)
    (V4 0 0 z 0)
    (V4 0 0 0 1)

translate4 :: Float -> Float -> Float -> M44 Float
translate4 x y z =
  V4
    (V4 1 0 0 x)
    (V4 0 1 0 y)
    (V4 0 0 1 z)
    (V4 0 0 0 1)

spawnWindow :: IO ()
spawnWindow = do
  void $ GLUT.initialize "test" []
  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.WithDepthBuffer]
  GLUT.debugMessageCallback $= Just print

  void $ GLUT.createWindow "test"
  GL.debugMessageCallback $= Just (\(GL.DebugMessage _ _ _ _ s) -> putStrLn s)

  GL.depthFunc $= Just GL.Less
  GL.clearDepth $= 1
  GL.clearColor $= GL.Color4 0.1 0.1 0.1 1

getAspectRatio :: IO Float
getAspectRatio = do
  (GLUT.Size w h) <- get GLUT.windowSize
  return $ fromIntegral w / fromIntegral h

display :: Shader -> Buffer -> IORef Int -> GLUT.DisplayCallback
display prog buffer count = modifyError error $ do
  liftIO $ GLUT.clear [GLUT.ColorBuffer, GLUT.DepthBuffer]
  c <- liftIO $ readIORef count
  liftIO $ modifyIORef' count (+ 1)
  ratio <- liftIO getAspectRatio

  let m = translate4 0 (-5) (-5) !*! rotate4xz (fromIntegral c * 0.01) !*! scale4 0.1 0.1 0.1
  let v = rotate4yz (pi / 4)
  let p = perspective (pi / 4) ratio 0.1 100
  let mvp = p !*! v !*! m
  let normalMatrix = transpose $ inv33 $ m ^. _m33

  bindShader prog [packUniform mvp, packUniform m, packUniform normalMatrix, packUniform (vf4 2 0 (-3) 1)]
  drawBuffer buffer
  liftIO GLUT.swapBuffers

diffuseLightByPos :: (ShaderTerm a, ShaderTerm b, ShaderTerm c) => a -> b -> c -> ShaderM Op
diffuseLightByPos lightPos normal fragPos = do
  let lightDir = normalize_ (lightPos !- fragPos) !. "xyz"
  let diffuse = max_ (0 :: Float) $ dot_ normal lightDir
  return diffuse

specularLightByPos :: (ShaderTerm a, ShaderTerm b, ShaderTerm c, ShaderTerm d) => a -> b -> c -> d -> Int -> ShaderM Op
specularLightByPos lightPos viewPos normal fragPos shininess = do
  let lightDir = normalize_ (fragPos !- lightPos) !. "xyz"
  let viewDir = normalize_ (viewPos !- fragPos) !. "xyz"
  let reflectDir = reflect_ lightDir normal
  let specular = max_ (0 :: Float) (dot_ viewDir reflectDir) `pow_` shininess
  return specular

phong :: (ShaderTerm a, ShaderTerm b, ShaderTerm c, ShaderTerm d) => a -> b -> c -> d -> Float -> Float -> Float -> Int -> ShaderM Op
phong lightPos viewPos normal fragPos ambientStrength diffuseStrength specularStrength shininess = do
  let normal' = normalize_ normal
  diffuse <- diffuseLightByPos lightPos normal' fragPos
  specular <- specularLightByPos lightPos viewPos normal' fragPos shininess
  let ambient = ambientStrength
  let diffuse' = diffuse !* diffuseStrength
  let specular' = specular !* specularStrength
  return $ ambient !+ diffuse' !+ specular'

colorFromPos :: ShaderTerm a => a -> ShaderM Op
colorFromPos pos = do
  let r = pos !. "x"
  let g = pos !. "y"
  let b = pos !. "z"
  let r' = min_ (1::Float) $ max_ (0::Float) r --(abs_ r)
  let g' = min_ (1::Float) $ max_ (0::Float) g --(abs_ g)
  let b' = min_ (1::Float) $ max_ (0::Float) b --(abs_ b)
  return $ r' !: g' !: b'

colorShader :: BufferDescription -> ShaderProgramM ()
colorShader description = do
  [mvp, m, normalMatrix, lightPos] <- uniforms [mat4x4, mat4x4, mat3x3, vec4]
  [pos, normal] <- attributes description
  normal' <- varying vec3
  pos' <- varying vec4
  shaderM VertexShader $ do
    gl_Position <~ mvp !* pos
    pos' <~ m !* pos
    normal' <~ normalMatrix !* normal
  shaderM FragmentShader $ do
    intensity <- phong 
      lightPos (vf4 0 0 0 1) normal' pos'
      0.1 0.8 0.5 32
    color <- colorFromPos pos
    outValue "color" $ intensity !* color

buildShader :: BufferDescription -> ComputationIO Shader
buildShader = compileProgram (ShaderVersion "330 core") . colorShader

readObj :: FilePath -> ComputationIO Buffer
readObj path = do
  (description, vertices, indices) <- parseObjectFile' path
  buffer <- createBuffer description
  bufferData buffer $= (vertices, indices <&> fromIntegral)
  return buffer

addRepeatTimer :: GLUT.Timeout -> GLUT.TimerCallback -> IO ()
addRepeatTimer t f = f >> GLUT.addTimerCallback t (addRepeatTimer t f)

main :: IO ()
main = do
  spawnWindow

  buffer <- modifyError error $ readObj "teapot.obj"
  prog <- modifyError error $ buildShader (buffer ^. bufferDescription)

  --case runExcept $ precompileProgram (ShaderVersion "330 core") $ colorShader (buffer ^. bufferDescription) of
  --  Left e -> print e
  --  Right (v, f, _) -> do
  --    putStrLn v
  --    putStrLn f

  frameCount <- newIORef (0 :: Int)

  GLUT.displayCallback $= display prog buffer frameCount
  GLUT.debugMessageCallback $= Just print

  addRepeatTimer 16 $ GLUT.postRedisplay Nothing

  GLUT.mainLoop
