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

import Math
import Shader
import VisLib.Base
import VisLib.Buffer.Image
import VisLib.Buffer.ObjectFile (parseObjectFile')
import VisLib.Buffer.Texture
import VisLib.Buffer.VertexBuffer
import VisLib.Shader.GL
import VisLib.Shader.Monad
import Object
import Control.Monad
import Linear ((!*!))

instance ShaderTypeable (RGB Float) where
  getType _ = vec4

instance ShaderTerm (RGB Float) where
  toOp (RGB r g b) = Lit vec4 $ "vec4(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ "1)"

instance UniformValue (RGB Float) where
  bindUniform loc (RGB r g b) = GL.uniform loc $= GL.Vertex4 r g b 1

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

display :: [ObjectContainer] -> IORef Int -> GLUT.DisplayCallback
display objects count = modifyError error $ do
  liftIO $ GLUT.clear [GLUT.ColorBuffer, GLUT.DepthBuffer]
  count' <- liftIO $ readIORef count
  liftIO $ modifyIORef' count (+ 1)
  let time = fromIntegral count' / 60

  let lightPosition = vf3 (sin time) 0 (-4.5 + cos time)

  let config = SceneConfiguration {
    viewMatrix = rotate4yz (pi / 4),
    projectionMatrix = perspective (pi / 4) 1 0.1 100,
    lightPos = lightPosition,
    viewPos = vf3 0 0 0,
    ambientStrength = 0.1,
    diffuseStrength = 0.7,
    specularStrength = 0.2,
    shininess = 32
  }
  forM_ objects $ \(ObjectContainer object) -> draw time config object

  liftIO GLUT.swapBuffers

textureShader :: BufferDescription -> ShaderProgramM ()
textureShader description = do
  [pos, tex] <- attributes description
  [colorTex] <- uniforms [sampler2D]
  shaderM VertexShader $ do
    gl_Position <~ pos
  shaderM FragmentShader $ do
    outValue "color" $ sample_ colorTex tex

readObj :: FilePath -> ComputationIO Buffer
readObj path = do
  (description, vertices, indices) <- parseObjectFile' path
  buffer <- createBuffer description
  bufferData buffer $= (vertices, indices <&> fromIntegral)
  return buffer

readTex :: FilePath -> ComputationIO Texture
readTex path = do
  img <- parseImage' path
  tex <- createTexture
  writeTexture' tex GL.RGBA' img
  return tex

addRepeatTimer :: GLUT.Timeout -> GLUT.TimerCallback -> IO ()
addRepeatTimer t f = f >> GLUT.addTimerCallback t (addRepeatTimer t f)

main :: IO ()
main = do
  spawnWindow

  objects <- modifyError error $ do
    let defaultMaterial = LightConfiguration {
      _ambientStrength = 0.1,
      _diffuseStrength = 0.7,
      _specularStrength = 0.2,
      _shininess = 32
    }
    cube1 <- createVertexOnlyObject (vf3 0.8 0.6 0.6) (translate4 1 (-5) (-5)) =<< readObj "CubeVert.obj"
    cube2 <- createPhongNormalObject (vf3 0.6 0.8 0.6) defaultMaterial (translate4 (-1) (-5) (-5)) =<< readObj "Cube.obj"
    teapot <- createPhongNormalObject (vf3 0.6 0.6 0.8) defaultMaterial (translate4 0 (-3) (-5) !*! scale4 0.1 0.1 0.1) =<< readObj "teapot.obj"

    return [ObjectContainer cube1, ObjectContainer cube2, ObjectContainer teapot]

  frameCount <- newIORef (0 :: Int)

  GLUT.displayCallback $= display objects frameCount
  GLUT.debugMessageCallback $= Just print

  addRepeatTimer 16 $ GLUT.postRedisplay Nothing

  GLUT.mainLoop
