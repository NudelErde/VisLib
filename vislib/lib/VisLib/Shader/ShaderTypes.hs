{-# LANGUAGE FlexibleInstances #-}

module VisLib.Shader.ShaderTypes where

import Control.Monad.Except
import Linear.Matrix (M22, M23, M24, M32, M33, M34, M42, M43, M44)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))

data BaseType = Bool | Int | UInt | Float | Double deriving (Eq, Show, Ord)

data SamplerType = Sampler1D | Sampler2D | Sampler3D | SamplerCube | Sampler1DShadow | Sampler2DShadow | Sampler1DArray | Sampler2DArray | Sampler1DArrayShadow | Sampler2DArrayShadow | SamplerCubeShadow | SamplerCubeArray | SamplerCubeArrayShadow deriving (Eq, Show, Ord)

data Type
  = Void String
  | Primitive BaseType
  | Tensor BaseType Int Int
  | Sampler SamplerType
  deriving (Eq, Show, Ord)

class ShaderTypeable a where
  getType :: a -> Type

vector :: BaseType -> Int -> Type
vector t 1 = Primitive t
vector t n = Tensor t n 1

matrix :: BaseType -> Int -> Int -> Type
matrix t 1 1 = Primitive t
matrix t m n = Tensor t m n

vec4 :: Type
vec4 = vector Float 4

vec3 :: Type
vec3 = vector Float 3

vec2 :: Type
vec2 = vector Float 2

ivec4 :: Type
ivec4 = vector Int 4

ivec3 :: Type
ivec3 = vector Int 3

ivec2 :: Type
ivec2 = vector Int 2

uvec4 :: Type
uvec4 = vector UInt 4

uvec3 :: Type
uvec3 = vector UInt 3

uvec2 :: Type
uvec2 = vector UInt 2

bvec4 :: Type
bvec4 = vector Bool 4

bvec3 :: Type
bvec3 = vector Bool 3

bvec2 :: Type
bvec2 = vector Bool 2

dvec4 :: Type
dvec4 = vector Double 4

dvec3 :: Type
dvec3 = vector Double 3

dvec2 :: Type
dvec2 = vector Double 2

mat4x4 :: Type
mat4x4 = matrix Float 4 4

mat4x3 :: Type
mat4x3 = matrix Float 4 3

mat4x2 :: Type
mat4x2 = matrix Float 4 2

mat3x4 :: Type
mat3x4 = matrix Float 3 4

mat3x3 :: Type
mat3x3 = matrix Float 3 3

mat3x2 :: Type
mat3x2 = matrix Float 3 2

mat2x4 :: Type
mat2x4 = matrix Float 2 4

mat2x3 :: Type
mat2x3 = matrix Float 2 3

mat2x2 :: Type
mat2x2 = matrix Float 2 2

dmat4x4 :: Type
dmat4x4 = matrix Double 4 4

dmat4x3 :: Type
dmat4x3 = matrix Double 4 3

dmat4x2 :: Type
dmat4x2 = matrix Double 4 2

dmat3x4 :: Type
dmat3x4 = matrix Double 3 4

dmat3x3 :: Type
dmat3x3 = matrix Double 3 3

dmat3x2 :: Type
dmat3x2 = matrix Double 3 2

dmat2x4 :: Type
dmat2x4 = matrix Double 2 4

dmat2x3 :: Type
dmat2x3 = matrix Double 2 3

dmat2x2 :: Type
dmat2x2 = matrix Double 2 2

mat4 :: Type
mat4 = mat4x4

mat3 :: Type
mat3 = mat3x3

mat2 :: Type
mat2 = mat2x2

dmat4 :: Type
dmat4 = dmat4x4

dmat3 :: Type
dmat3 = dmat3x3

dmat2 :: Type
dmat2 = dmat2x2

float :: Type
float = Primitive Float

double :: Type
double = Primitive Double

int :: Type
int = Primitive Int

uint :: Type
uint = Primitive UInt

bool :: Type
bool = Primitive Bool

isSampler :: Type -> Bool
isSampler (Sampler _) = True
isSampler _ = False

isPrimitive :: Type -> Bool
isPrimitive (Primitive _) = True
isPrimitive _ = False

isTensor :: Type -> Bool
isTensor (Tensor {}) = True
isTensor _ = False

isVector :: Type -> Bool
isVector (Tensor _ n 1) = n > 1
isVector _ = False

isMatrix :: Type -> Bool
isMatrix (Tensor _ n m) = n > 1 && m > 1
isMatrix _ = False

multiplyType :: Type -> Type -> Maybe Type
multiplyType (Void _) _ = Nothing
multiplyType _ (Void _) = Nothing
multiplyType (Sampler _) _ = Nothing
multiplyType _ (Sampler _) = Nothing
multiplyType (Primitive t1) (Primitive t2) = if t1 == t2 then Just (Primitive t1) else Nothing
multiplyType (Tensor t1 n1 m1) (Tensor t2 n2 m2)
  | t1 == t2 && m1 == n2 = Just (Tensor t1 n1 m2)
  | t1 == t2 && m1 == 1 && m2 == 1 && n1 == n2 = Just (Tensor t1 n1 1)
  | otherwise = Nothing
multiplyType (Primitive t1) (Tensor t2 n m) = if t1 == t2 then Just (Tensor t2 n m) else Nothing
multiplyType (Tensor t1 n m) (Primitive t2) = if t1 == t2 then Just (Tensor t1 n m) else Nothing

typeName :: Type -> Except String String
typeName (Void e) = return $ "void (" ++ e ++ ")"
typeName (Primitive Bool) = return "bool"
typeName (Primitive Int) = return "int"
typeName (Primitive UInt) = return "uint"
typeName (Primitive Float) = return "float"
typeName (Primitive Double) = return "double"
typeName (Tensor Bool 4 1) = return "bvec4"
typeName (Tensor Bool 3 1) = return "bvec3"
typeName (Tensor Bool 2 1) = return "bvec2"
typeName (Tensor Int 4 1) = return "ivec4"
typeName (Tensor Int 3 1) = return "ivec3"
typeName (Tensor Int 2 1) = return "ivec2"
typeName (Tensor UInt 4 1) = return "uvec4"
typeName (Tensor UInt 3 1) = return "uvec3"
typeName (Tensor UInt 2 1) = return "uvec2"
typeName (Tensor Float 4 1) = return "vec4"
typeName (Tensor Float 3 1) = return "vec3"
typeName (Tensor Float 2 1) = return "vec2"
typeName (Tensor Double 4 1) = return "dvec4"
typeName (Tensor Double 3 1) = return "dvec3"
typeName (Tensor Double 2 1) = return "dvec2"
typeName (Tensor Float 4 4) = return "mat4x4"
typeName (Tensor Float 4 3) = return "mat4x3"
typeName (Tensor Float 4 2) = return "mat4x2"
typeName (Tensor Float 3 4) = return "mat3x4"
typeName (Tensor Float 3 3) = return "mat3x3"
typeName (Tensor Float 3 2) = return "mat3x2"
typeName (Tensor Float 2 4) = return "mat2x4"
typeName (Tensor Float 2 3) = return "mat2x3"
typeName (Tensor Float 2 2) = return "mat2x2"
typeName (Tensor Double 4 4) = return "dmat4x4"
typeName (Tensor Double 4 3) = return "dmat4x3"
typeName (Tensor Double 4 2) = return "dmat4x2"
typeName (Tensor Double 3 4) = return "dmat3x4"
typeName (Tensor Double 3 3) = return "dmat3x3"
typeName (Tensor Double 3 2) = return "dmat3x2"
typeName (Tensor Double 2 4) = return "dmat2x4"
typeName (Tensor Double 2 3) = return "dmat2x3"
typeName (Tensor Double 2 2) = return "dmat2x2"
typeName (Tensor {}) = throwError "Unknown tensor type"
typeName (Sampler Sampler1D) = return "sampler1D"
typeName (Sampler Sampler2D) = return "sampler2D"
typeName (Sampler Sampler3D) = return "sampler3D"
typeName (Sampler SamplerCube) = return "samplerCube"
typeName (Sampler Sampler1DShadow) = return "sampler1DShadow"
typeName (Sampler Sampler2DShadow) = return "sampler2DShadow"
typeName (Sampler Sampler1DArray) = return "sampler1DArray"
typeName (Sampler Sampler2DArray) = return "sampler2DArray"
typeName (Sampler Sampler1DArrayShadow) = return "sampler1DArrayShadow"
typeName (Sampler Sampler2DArrayShadow) = return "sampler2DArrayShadow"
typeName (Sampler SamplerCubeShadow) = return "samplerCubeShadow"
typeName (Sampler SamplerCubeArray) = return "samplerCubeArray"
typeName (Sampler SamplerCubeArrayShadow) = return "samplerCubeArrayShadow"

tensorSize :: Type -> Maybe (Int, Int)
tensorSize (Tensor _ n m) = Just (n, m)
tensorSize (Primitive _) = Just (1, 1)
tensorSize _ = Nothing

baseType :: Type -> Maybe BaseType
baseType (Tensor t _ _) = Just t
baseType (Primitive t) = Just t
baseType _ = Nothing

-- Type Instances

instance ShaderTypeable Int where
  getType _ = Primitive Int

instance ShaderTypeable Float where
  getType _ = Primitive Float

instance ShaderTypeable Double where
  getType _ = Primitive Double

instance ShaderTypeable Bool where
  getType _ = Primitive Bool

instance ShaderTypeable (Float, Float) where
  getType _ = vec2

instance ShaderTypeable (Float, Float, Float) where
  getType _ = vec3

instance ShaderTypeable (Float, Float, Float, Float) where
  getType _ = vec4

instance ShaderTypeable (Int, Int) where
  getType _ = ivec2

instance ShaderTypeable (Int, Int, Int) where
  getType _ = ivec3

instance ShaderTypeable (Int, Int, Int, Int) where
  getType _ = ivec4

instance ShaderTypeable (Bool, Bool) where
  getType _ = bvec2

instance ShaderTypeable (Bool, Bool, Bool) where
  getType _ = bvec3

instance ShaderTypeable (Bool, Bool, Bool, Bool) where
  getType _ = bvec4

instance ShaderTypeable (Double, Double) where
  getType _ = dvec2

instance ShaderTypeable (Double, Double, Double) where
  getType _ = dvec3

instance ShaderTypeable (Double, Double, Double, Double) where
  getType _ = dvec4

-- linear
instance ShaderTypeable (V2 Float) where
  getType _ = vec2

instance ShaderTypeable (V3 Float) where
  getType _ = vec3

instance ShaderTypeable (V4 Float) where
  getType _ = vec4

instance ShaderTypeable (V2 Int) where
  getType _ = ivec2

instance ShaderTypeable (V3 Int) where
  getType _ = ivec3

instance ShaderTypeable (V4 Int) where
  getType _ = ivec4

instance ShaderTypeable (V2 Bool) where
  getType _ = bvec2

instance ShaderTypeable (V3 Bool) where
  getType _ = bvec3

instance ShaderTypeable (V4 Bool) where
  getType _ = bvec4

instance ShaderTypeable (V2 Double) where
  getType _ = dvec2

instance ShaderTypeable (V3 Double) where
  getType _ = dvec3

instance ShaderTypeable (V4 Double) where
  getType _ = dvec4

instance ShaderTypeable (Linear.Matrix.M22 Float) where
  getType _ = mat2

instance ShaderTypeable (Linear.Matrix.M23 Float) where
  getType _ = mat2x3

instance ShaderTypeable (Linear.Matrix.M24 Float) where
  getType _ = mat2x4

instance ShaderTypeable (Linear.Matrix.M32 Float) where
  getType _ = mat3x2

instance ShaderTypeable (Linear.Matrix.M33 Float) where
  getType _ = mat3

instance ShaderTypeable (Linear.Matrix.M34 Float) where
  getType _ = mat3x4

instance ShaderTypeable (Linear.Matrix.M42 Float) where
  getType _ = mat4x2

instance ShaderTypeable (Linear.Matrix.M43 Float) where
  getType _ = mat4x3

instance ShaderTypeable (Linear.Matrix.M44 Float) where
  getType _ = mat4

instance ShaderTypeable (Linear.Matrix.M22 Double) where
  getType _ = dmat2

instance ShaderTypeable (Linear.Matrix.M23 Double) where
  getType _ = dmat2x3

instance ShaderTypeable (Linear.Matrix.M24 Double) where
  getType _ = dmat2x4

instance ShaderTypeable (Linear.Matrix.M32 Double) where
  getType _ = dmat3x2

instance ShaderTypeable (Linear.Matrix.M33 Double) where
  getType _ = dmat3

instance ShaderTypeable (Linear.Matrix.M34 Double) where
  getType _ = dmat3x4

instance ShaderTypeable (Linear.Matrix.M42 Double) where
  getType _ = dmat4x2

instance ShaderTypeable (Linear.Matrix.M43 Double) where
  getType _ = dmat4x3

instance ShaderTypeable (Linear.Matrix.M44 Double) where
  getType _ = dmat4
