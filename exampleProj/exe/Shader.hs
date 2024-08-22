module Shader where

import VisLib.Shader.Monad

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

colorFromPos :: (ShaderTerm a) => a -> ShaderM Op
colorFromPos pos = do
  let r = pos !. "x"
  let g = pos !. "y"
  let b = pos !. "z"
  let r' = min_ (1 :: Float) $ max_ (0 :: Float) r -- (abs_ r)
  let g' = min_ (1 :: Float) $ max_ (0 :: Float) g -- (abs_ g)
  let b' = min_ (1 :: Float) $ max_ (0 :: Float) b -- (abs_ b)
  return $ r' !: g' !: b'