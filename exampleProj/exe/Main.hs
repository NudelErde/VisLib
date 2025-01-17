{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

import Control.Lens ((.~), (^.), (^?), _Just)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Colour.RGBSpace
import Data.Foldable (minimumBy)
import Data.Function
import Data.IORef
import Data.Maybe
import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Light
import Linear ((!*!))
import Linear.Metric
import Math
import Object
import Player (Player (Player), PlayerEvent (PlayerInputEvent), playerPos, playerView, updatePlayer)
import Shader
import VisLib.Base
import VisLib.Shader.GL
import VisLib.Shader.Monad
import World
import Load
import Control.Lens.Operators ((%~))
import System.Clock
import Control.Concurrent.Lock
import Data.Composition

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

display :: World -> Player -> Int -> GLUT.DisplayCallback
display world player count = modifyError error $ do
  let time = fromIntegral count / 60
  aspect <- liftIO getAspectRatio

  let lightPosition = vf3 (sin time) 0 (-4.5 + cos time)
  let pPos = player ^. playerPos
  let light = case world ^. lights of
        [] -> Nothing
        x -> Just $ minimumBy (compare `on` (distance pPos . (^. lightPos))) x
  let light' = light & _Just . lightPos .~ lightPosition
  let config =
        SceneConfiguration
          { viewMatrixSC = playerView player,
            projectionMatrixSC = perspective (pi / 4) aspect 0.1 100,
            lightPosSC = fromMaybe (vf3 0 0 0) $ light' ^? _Just . lightPos,
            viewPosSC = pPos,
            ambientStrengthSC = fromMaybe 0.1 $ light' ^? _Just . ambientStrengthLight,
            diffuseStrengthSC = fromMaybe 0.7 $ light' ^? _Just . diffuseStrengthLight,
            specularStrengthSC = fromMaybe 0.2 $ light' ^? _Just . specularStrengthLight,
            shininessSC = fromMaybe 32 $ light' ^? _Just . shininessLight
          }

  forM_ (world ^. objects) $ draw time config

repeatTimer' :: IO GLUT.Timeout -> IO ()
repeatTimer' f = f >>= (`GLUT.addTimerCallback` repeatTimer' f)

inputCallback :: IORef Player -> GLUT.KeyboardMouseCallback
inputCallback playerRef key keystate modifiers position = do
  playerRef $~! updatePlayer (PlayerInputEvent key keystate modifiers position)
  return ()

$(embedAssets "asset" "../assets/")

gitInfo :: GitInfo
gitInfo = $(embedGitInfo)

main :: IO ()
main = do
  spawnWindow 

  objs <- modifyError error $ do
    let defaultMaterial =
          PhongMaterial
            { _ambientStrength = 0.1,
              _diffuseStrength = 0.7,
              _specularStrength = 0.2,
              _shininess = 32
            }
    cube1 <- createVertexOnlyObject (vf3 0.8 0.6 0.6) (translate4 1 (-5) (-5)) =<< assetCubeVert
    cube2 <- createPhongNormalObject (vf3 0.6 0.8 0.6) defaultMaterial (translate4 (-1) (-5) (-5)) =<< assetCube
    teapot <- createPhongNormalObject (vf3 0.6 0.6 0.8) defaultMaterial (translate4 0 (-3) (-5) !*! scale4 0.1 0.1 0.1) =<< assetTeapot

    return [ObjectContainer cube1, ObjectContainer cube2, ObjectContainer teapot]
  frameCount <- newIORef (0 :: Int)
  player <- newIORef (Player (vf3 0 0 5) (vf3 0 (-0.5) (-1)))
  world <- newIORef (World objs [Light (vf3 0 0 0) 0.1 0.7 0.2 32])
  lock <- with <$> new

  GLUT.keyboardMouseCallback $= Just (lock .:: inputCallback player)

  GLUT.displayCallback $= do
    w <- get world
    p <- get player
    f <- get frameCount
    frameCount $~! (+ 1)
    GLUT.clear [GLUT.ColorBuffer, GLUT.DepthBuffer]
    lock $ display w p f
    GLUT.swapBuffers

  lastTimeRef <- newIORef =<< getTime Monotonic
  repeatTimer' $ do
    currentTime <- getTime Monotonic
    lastTime <- get lastTimeRef
    let delta = fromIntegral (toNanoSecs (currentTime - lastTime)) / (10 ^^ (9 :: Int))
    lock $ world $~! objects.traverse %~ update delta
    writeIORef lastTimeRef =<< getTime Monotonic
    GLUT.postRedisplay Nothing
    return $ 16 - round (delta / 1000)

  GLUT.mainLoop
