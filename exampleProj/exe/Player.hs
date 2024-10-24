{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Player where

import Control.Lens
import Graphics.Rendering.OpenGL.GL.CoordTrans (Position)
import Graphics.UI.GLUT.Callbacks.Window
import Linear (lookAt, norm)
import Linear.Matrix (M44, (!*))
import Linear.Metric (normalize)
import Linear.V2
import Linear.V3 (V3 (..), cross, _xz)
import Linear.V4 (vector, _xyz)
import Linear.Vector ((*^))
import Math (rotate2, rotate4xz)

data Player = Player
  { _playerPos :: V3 Float,
    _playerRot :: V3 Float
  }

$(makeLenses ''Player)

playerView :: Player -> M44 Float
playerView p = lookAt (p ^. playerPos) (p ^. playerPos + p ^. playerRot) (V3 0 1 0)

data PlayerEvent where
  PlayerInputEvent :: Key -> KeyState -> Modifiers -> Position -> PlayerEvent

rotateUpDown :: Float -> V3 Float -> V3 Float
rotateUpDown a v =
  let xy = rotate2 a !* V2 (norm $ v & _y .~ 0) (v ^. _y)
   in v
        & _y .~ xy ^. _y
        & _xz %~ (xy ^. _x *^) . normalize

updatePlayer :: PlayerEvent -> Player -> Player
updatePlayer (PlayerInputEvent (Char 'w') Down _ _) p = p & playerPos +~ 0.1 *^ normalize (p ^. playerRot & _y .~ 0)
updatePlayer (PlayerInputEvent (Char 's') Down _ _) p = p & playerPos -~ 0.1 *^ normalize (p ^. playerRot & _y .~ 0)
updatePlayer (PlayerInputEvent (Char 'a') Down _ _) p = p & playerPos -~ 0.1 *^ normalize ((p ^. playerRot & _y .~ 0) `cross` V3 0 1 0)
updatePlayer (PlayerInputEvent (Char 'd') Down _ _) p = p & playerPos +~ 0.1 *^ normalize ((p ^. playerRot & _y .~ 0) `cross` V3 0 1 0)
updatePlayer (PlayerInputEvent (Char ' ') Down _ _) p = p & playerPos +~ V3 0 0.1 0
updatePlayer (PlayerInputEvent (Char 'c') Down _ _) p = p & playerPos -~ V3 0 0.1 0
updatePlayer (PlayerInputEvent (SpecialKey KeyLeft) Down _ _) p = p & playerRot %~ normalize . (^. _xyz) . (rotate4xz 0.1 !*) . vector
updatePlayer (PlayerInputEvent (SpecialKey KeyRight) Down _ _) p = p & playerRot %~ normalize . (^. _xyz) . (rotate4xz (-0.1) !*) . vector
updatePlayer (PlayerInputEvent (SpecialKey KeyUp) Down _ _) p = p & playerRot %~ normalize . rotateUpDown 0.1
updatePlayer (PlayerInputEvent (SpecialKey KeyDown) Down _ _) p = p & playerRot %~ normalize . rotateUpDown (-0.1)
updatePlayer _ p = p