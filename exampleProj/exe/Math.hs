module Math where

import Linear.Matrix (M22, M33, M44, identity, inv33, transpose, (!!*), (!*!), (*!!), _m33)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))

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