{-# LANGUAGE GADTs #-}
module Object where

import VisLib.Base
import Math
import Linear.Matrix (M22, M33, M44, identity, inv33, transpose, (!!*), (!*!), (*!!), _m33)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import VisLib.Shader.GL
import VisLib.Buffer.VertexBuffer (drawBuffer)
import VisLib.Shader.Monad
import Shader
import Control.Lens ((^.))

data SceneConfiguration = SceneConfiguration
  { viewMatrix :: M44 Float
  , projectionMatrix :: M44 Float
  , lightPos :: V3 Float
  , viewPos :: V3 Float
  , ambientStrength :: Float
  , diffuseStrength :: Float
  , specularStrength :: Float
  , shininess :: Int}

data ObjectContainer where
  ObjectContainer :: Object a => a -> ObjectContainer

class Object a where
  draw :: Float -> SceneConfiguration -> a -> ComputationIO ()
  update :: a -> Float -> a

data StaticObject where
  StaticObject :: Shader -> (Float -> StaticObject -> SceneConfiguration -> [UniformValueContainer]) -> Buffer -> Maybe Texture -> M44 Float -> StaticObject

instance Object StaticObject where
  draw time scene o@(StaticObject prog binder buffer _ _) = do
    bindShader prog $ binder time o scene
    drawBuffer buffer
  update = const

createVertexOnlyObject :: V3 Float -> M44 Float -> Buffer -> ComputationIO StaticObject
createVertexOnlyObject color mat buffer = do
  prog <- compileProgram (ShaderVersion "330 core") $ do
    [mvp] <- uniforms [mat4]
    [pos] <- attributes $ _bufferDescription buffer
    shaderM VertexShader $ do
      gl_Position <~ mvp !* pos
    shaderM FragmentShader $ do
      outValue "color" $ color !: (1::Float)
  
  let binder _ (StaticObject _ _ _ _ mm) scene = [packUniform $ projectionMatrix scene !*! viewMatrix scene !*! mm]

  return $ StaticObject prog binder buffer Nothing mat
  
createPhongNormalObject :: V3 Float -> LightConfiguration -> M44 Float -> Buffer -> ComputationIO StaticObject
createPhongNormalObject color lightConfig modelMatrix buffer = do
  prog <- compileProgram (ShaderVersion "330 core") $ do
    [mvp, m, normalMatrix, lPos] <- uniforms [mat4, mat4, mat3, vec3]
    [pos, normal] <- attributes $ _bufferDescription buffer
    normal' <- varying vec3
    pos' <- varying vec4
    shaderM VertexShader $ do
      gl_Position <~ mvp !* pos
      pos' <~ m !* pos
      normal' <~ normalMatrix !* normal
    shaderM FragmentShader $ do
      intensity <- phong (lPos !: (1::Float)) (vf4 0 0 0 1) normal' pos' lightConfig
      outValue "color" $ intensity !* color
  
  let binder _ (StaticObject _ _ _ _ mm) scene = let mvp = projectionMatrix scene !*! viewMatrix scene !*! mm
                                                     m = mm
                                                     normalMatrix = transpose $ inv33 $ mm ^. _m33
                                                     lightPosition = lightPos scene
                                                  in [packUniform mvp, packUniform m, packUniform normalMatrix, packUniform lightPosition]

  return $ StaticObject prog binder buffer Nothing modelMatrix