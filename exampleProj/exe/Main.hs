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

display :: Shader -> Buffer -> Texture -> IORef Int -> GLUT.DisplayCallback
display prog buffer tex count = modifyError error $ do
  liftIO $ GLUT.clear [GLUT.ColorBuffer, GLUT.DepthBuffer]
  -- c <- liftIO $ readIORef count
  -- liftIO $ modifyIORef' count (+ 1)
  -- ratio <- liftIO getAspectRatio

  -- let m = translate4 0 (-5) (-5) !*! rotate4xz (fromIntegral c * 0.01) !*! scale4 0.1 0.1 0.1
  -- let v = rotate4yz (pi / 4)
  -- let p = perspective (pi / 4) ratio 0.1 100
  -- let mvp = p !*! v !*! m
  -- let normalMatrix = transpose $ inv33 $ m ^. _m33

  -- bindShader prog [packUniform mvp, packUniform m, packUniform normalMatrix, packUniform (vf4 2 0 (-3) 1)]
  bindShader prog [packUniform tex]
  drawBuffer buffer
  liftIO GLUT.swapBuffers

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
    intensity <- phong lightPos (vf4 0 0 0 1) normal' pos' 0.1 0.8 0.5 32
    color <- colorFromPos pos
    outValue "color" $ intensity !* color

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

  buffer <- modifyError error $ readObj "plane.obj"
  tex <- modifyError error $ readTex "neg_x.ppm"
  prog <- modifyError error $ compileProgram (ShaderVersion "330 core") $ textureShader (buffer ^. bufferDescription)

  -- prog <- modifyError error $ compileProgram (ShaderVersion "330 core") $ colorShader (buffer ^. bufferDescription)

  -- case runExcept $ precompileProgram (ShaderVersion "330 core") $ textureShader (buffer ^. bufferDescription) of
  --  Left e -> print e
  --  Right (v, f, _) -> do
  --    putStrLn v
  --    putStrLn f

  frameCount <- newIORef (0 :: Int)

  GLUT.displayCallback $= display prog buffer tex frameCount
  GLUT.debugMessageCallback $= Just print

  addRepeatTimer 16 $ GLUT.postRedisplay Nothing

  GLUT.mainLoop
