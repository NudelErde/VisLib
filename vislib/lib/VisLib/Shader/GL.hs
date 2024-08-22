{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module VisLib.Shader.GL where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.StateVar
import Foreign (withArray)
import qualified Graphics.GL.Functions as GLF
import qualified Graphics.Rendering.OpenGL.GL as GL
import Linear.Matrix (M22, M23, M24, M32, M33, M34, M42, M43, M44)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import VisLib.Base
import Control.Monad.State

data UniformValueContainer = forall a. (UniformValue a) => UniformValueContainer a

class (ShaderTypeable a) => UniformValue a where
  bindUniform :: GL.UniformLocation -> a -> StateT Int (ExceptT String IO) ()

packUniform :: (UniformValue a) => a -> UniformValueContainer
packUniform = UniformValueContainer

createShader' :: String -> GL.ShaderType -> ExceptT String IO GL.Shader
createShader' source shaderType = do
  s <- liftIO $ GL.createShader shaderType
  GL.shaderSourceBS s $= GL.packUtf8 source
  liftIO $ GL.compileShader s
  ok <- GL.get $ GL.compileStatus s
  if ok
    then return s
    else do
      infoLog <- GL.get $ GL.shaderInfoLog s
      throwError infoLog

createShader :: String -> String -> ComputationIO Shader
createShader vertexShader fragmentShader = do
  shaderProgram <- liftIO GL.createProgram
  vs <- createShader' vertexShader GL.VertexShader
  fs <- createShader' fragmentShader GL.FragmentShader
  liftIO $ GL.attachShader shaderProgram vs
  liftIO $ GL.attachShader shaderProgram fs
  liftIO $ GL.linkProgram shaderProgram
  ok <- GL.get $ GL.linkStatus shaderProgram
  if ok
    then return $ GLShader shaderProgram vs fs Nothing
    else do
      infoLog <- GL.get $ GL.programInfoLog shaderProgram
      throwError infoLog

bindShader :: Shader -> [UniformValueContainer] -> ComputationIO ()
bindShader (GLShader prog _ _ Nothing) _ = void $ liftIO $ GL.currentProgram $= Just prog
bindShader (GLShader prog _ _ (Just (UniformInfo ts locs))) container = do
  liftIO $ GL.currentProgram $= Just prog
  let values = zip3 ts locs container
  _ <- (`runStateT` 0) $ forM_ values $ \(t, loc, UniformValueContainer v) -> do
    let vt = getType v
    when (t /= vt) $ throwError "Uniform type mismatch"
    loc' <- liftIO $ GL.uniformLocation prog loc
    bindUniform loc' v
    return ()
  return ()

-- UniformValue instances

instance UniformValue Int where
  bindUniform loc i = GL.uniform loc $= (fromIntegral i :: GL.GLint)

instance UniformValue Float where
  bindUniform loc f = GL.uniform loc $= f

instance UniformValue Double where
  bindUniform loc d = GL.uniform loc $= d

instance UniformValue Bool where
  bindUniform loc b = GL.uniform loc $= (if b then 1 else 0 :: GL.GLint)

instance UniformValue (Float, Float) where
  bindUniform loc (x, y) = GL.uniform loc $= GL.Vector2 x y

instance UniformValue (Float, Float, Float) where
  bindUniform loc (x, y, z) = GL.uniform loc $= GL.Vector3 x y z

instance UniformValue (Float, Float, Float, Float) where
  bindUniform loc (x, y, z, w) = GL.uniform loc $= GL.Vector4 x y z w

instance UniformValue (Int, Int) where
  bindUniform loc (x, y) =
    let vec = GL.Vector2 (fromIntegral x) (fromIntegral y) :: GL.Vector2 GL.GLint
     in GL.uniform loc $= vec

instance UniformValue (Int, Int, Int) where
  bindUniform loc (x, y, z) =
    let vec = GL.Vector3 (fromIntegral x) (fromIntegral y) (fromIntegral z) :: GL.Vector3 GL.GLint
     in GL.uniform loc $= vec

instance UniformValue (Int, Int, Int, Int) where
  bindUniform loc (x, y, z, w) =
    let vec = GL.Vector4 (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral w) :: GL.Vector4 GL.GLint
     in GL.uniform loc $= vec

instance UniformValue (Bool, Bool) where
  bindUniform loc (x, y) =
    let vec = GL.Vector2 (if x then 1 else 0) (if y then 1 else 0) :: GL.Vector2 GL.GLint
     in GL.uniform loc $= vec

instance UniformValue (Bool, Bool, Bool) where
  bindUniform loc (x, y, z) =
    let vec = GL.Vector3 (if x then 1 else 0) (if y then 1 else 0) (if z then 1 else 0) :: GL.Vector3 GL.GLint
     in GL.uniform loc $= vec

instance UniformValue (Bool, Bool, Bool, Bool) where
  bindUniform loc (x, y, z, w) =
    let vec = GL.Vector4 (if x then 1 else 0) (if y then 1 else 0) (if z then 1 else 0) (if w then 1 else 0) :: GL.Vector4 GL.GLint
     in GL.uniform loc $= vec

instance UniformValue (Double, Double) where
  bindUniform loc (x, y) = GL.uniform loc $= GL.Vector2 x y

instance UniformValue (Double, Double, Double) where
  bindUniform loc (x, y, z) = GL.uniform loc $= GL.Vector3 x y z

instance UniformValue (Double, Double, Double, Double) where
  bindUniform loc (x, y, z, w) = GL.uniform loc $= GL.Vector4 x y z w

-- linear

instance UniformValue (V2 Float) where
  bindUniform loc (V2 x y) = GL.uniform loc $= GL.Vector2 x y

instance UniformValue (V3 Float) where
  bindUniform loc (V3 x y z) = GL.uniform loc $= GL.Vector3 x y z

instance UniformValue (V4 Float) where
  bindUniform loc (V4 x y z w) = GL.uniform loc $= GL.Vector4 x y z w

instance UniformValue (V2 Int) where
  bindUniform loc (V2 x y) = GL.uniform loc $= (GL.Vector2 (fromIntegral x) (fromIntegral y) :: GL.Vector2 GL.GLint)

instance UniformValue (V3 Int) where
  bindUniform loc (V3 x y z) = GL.uniform loc $= (GL.Vector3 (fromIntegral x) (fromIntegral y) (fromIntegral z) :: GL.Vector3 GL.GLint)

instance UniformValue (V4 Int) where
  bindUniform loc (V4 x y z w) = GL.uniform loc $= (GL.Vector4 (fromIntegral x) (fromIntegral y) (fromIntegral z) (fromIntegral w) :: GL.Vector4 GL.GLint)

instance UniformValue (V2 Bool) where
  bindUniform loc (V2 x y) = GL.uniform loc $= (GL.Vector2 (if x then 1 else 0) (if y then 1 else 0) :: GL.Vector2 GL.GLint)

instance UniformValue (V3 Bool) where
  bindUniform loc (V3 x y z) = GL.uniform loc $= (GL.Vector3 (if x then 1 else 0) (if y then 1 else 0) (if z then 1 else 0) :: GL.Vector3 GL.GLint)

instance UniformValue (V4 Bool) where
  bindUniform loc (V4 x y z w) = GL.uniform loc $= (GL.Vector4 (if x then 1 else 0) (if y then 1 else 0) (if z then 1 else 0) (if w then 1 else 0) :: GL.Vector4 GL.GLint)

instance UniformValue (V2 Double) where
  bindUniform loc (V2 x y) = GL.uniform loc $= GL.Vector2 x y

instance UniformValue (V3 Double) where
  bindUniform loc (V3 x y z) = GL.uniform loc $= GL.Vector3 x y z

instance UniformValue (V4 Double) where
  bindUniform loc (V4 x y z w) = GL.uniform loc $= GL.Vector4 x y z w

instance UniformValue (M22 Float) where
  bindUniform (GL.UniformLocation loc) (V2 (V2 a b) (V2 c d)) = lift $ lift $ withArray [a, b, c, d] $ GLF.glUniformMatrix2fv loc 1 1

instance UniformValue (M23 Float) where
  bindUniform (GL.UniformLocation loc) (V2 (V3 a b c) (V3 d e f)) = lift $ lift $ withArray [a, b, c, d, e, f] $ GLF.glUniformMatrix2x3fv loc 1 1

instance UniformValue (M24 Float) where
  bindUniform (GL.UniformLocation loc) (V2 (V4 a b c d) (V4 e f g h)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h] $ GLF.glUniformMatrix2x4fv loc 1 1

instance UniformValue (M32 Float) where
  bindUniform (GL.UniformLocation loc) (V3 (V2 a b) (V2 c d) (V2 e f)) = lift $ lift $ withArray [a, b, c, d, e, f] $ GLF.glUniformMatrix3x2fv loc 1 1

instance UniformValue (M33 Float) where
  bindUniform (GL.UniformLocation loc) (V3 (V3 a b c) (V3 d e f) (V3 g h i)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h, i] $ GLF.glUniformMatrix3fv loc 1 1

instance UniformValue (M34 Float) where
  bindUniform (GL.UniformLocation loc) (V3 (V4 a b c d) (V4 e f g h) (V4 i j k l)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h, i, j, k, l] $ GLF.glUniformMatrix3x4fv loc 1 1

instance UniformValue (M42 Float) where
  bindUniform (GL.UniformLocation loc) (V4 (V2 a b) (V2 c d) (V2 e f) (V2 g h)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h] $ GLF.glUniformMatrix4x2fv loc 1 1

instance UniformValue (M43 Float) where
  bindUniform (GL.UniformLocation loc) (V4 (V3 a b c) (V3 d e f) (V3 g h i) (V3 j k l)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h, i, j, k, l] $ GLF.glUniformMatrix4x3fv loc 1 1

instance UniformValue (M44 Float) where
  bindUniform (GL.UniformLocation loc) (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] $ GLF.glUniformMatrix4fv loc 1 1

instance UniformValue (M22 Double) where
  bindUniform (GL.UniformLocation loc) (V2 (V2 a b) (V2 c d)) = lift $ lift $ withArray [a, b, c, d] $ GLF.glUniformMatrix2dv loc 1 1

instance UniformValue (M23 Double) where
  bindUniform (GL.UniformLocation loc) (V2 (V3 a b c) (V3 d e f)) = lift $ lift $ withArray [a, b, c, d, e, f] $ GLF.glUniformMatrix2x3dv loc 1 1

instance UniformValue (M24 Double) where
  bindUniform (GL.UniformLocation loc) (V2 (V4 a b c d) (V4 e f g h)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h] $ GLF.glUniformMatrix2x4dv loc 1 1

instance UniformValue (M32 Double) where
  bindUniform (GL.UniformLocation loc) (V3 (V2 a b) (V2 c d) (V2 e f)) = lift $ lift $ withArray [a, b, c, d, e, f] $ GLF.glUniformMatrix3x2dv loc 1 1

instance UniformValue (M33 Double) where
  bindUniform (GL.UniformLocation loc) (V3 (V3 a b c) (V3 d e f) (V3 g h i)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h, i] $ GLF.glUniformMatrix3dv loc 1 1

instance UniformValue (M34 Double) where
  bindUniform (GL.UniformLocation loc) (V3 (V4 a b c d) (V4 e f g h) (V4 i j k l)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h, i, j, k, l] $ GLF.glUniformMatrix3x4dv loc 1 1

instance UniformValue (M42 Double) where
  bindUniform (GL.UniformLocation loc) (V4 (V2 a b) (V2 c d) (V2 e f) (V2 g h)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h] $ GLF.glUniformMatrix4x2dv loc 1 1

instance UniformValue (M43 Double) where
  bindUniform (GL.UniformLocation loc) (V4 (V3 a b c) (V3 d e f) (V3 g h i) (V3 j k l)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h, i, j, k, l] $ GLF.glUniformMatrix4x3dv loc 1 1

instance UniformValue (M44 Double) where
  bindUniform (GL.UniformLocation loc) (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) = lift $ lift $ withArray [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] $ GLF.glUniformMatrix4dv loc 1 1