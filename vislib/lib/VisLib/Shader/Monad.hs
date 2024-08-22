{-# LANGUAGE ExistentialQuantification #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module VisLib.Shader.Monad (module VisLib.Shader.Monad, VisLib.Shader.GL.bindShader, VisLib.Base.Shader) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor
import Data.List (group, intercalate, sort)
import Linear.Matrix (M22, M23, M24, M32, M33, M34, M42, M43, M44)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import VisLib.Base
import VisLib.Shader.GL

class (ShaderTypeable a) => ShaderTerm a where
  toOp :: a -> Op

data Op = Lit Type String | Field Type Op String | Add Type Op Op | Sub Type Op Op | Mul Type Op Op | Div Type Op Op | Func Type String [Op] | WrappedVar Var | WrappedError String deriving (Eq, Show, Ord)

instance ShaderTypeable Op where
  getType (Lit t _) = t
  getType (Field t _ _) = t
  getType (Add t _ _) = t
  getType (Sub t _ _) = t
  getType (Mul t _ _) = t
  getType (Div t _ _) = t
  getType (Func t _ _) = t
  getType (WrappedVar v) = getType v
  getType (WrappedError e) = Void e

instance ShaderTerm Op where
  toOp = id

instance ShaderTerm Int where
  toOp i = Lit (Primitive Int) (show i)

instance ShaderTerm Float where
  toOp f = Lit (Primitive Float) (show f)

instance ShaderTerm Double where
  toOp d = Lit (Primitive Double) (show d ++ "lf")

instance ShaderTerm Bool where
  toOp True = Lit (Primitive Bool) "true"
  toOp False = Lit (Primitive Bool) "false"

instance ShaderTerm (Float, Float) where
  toOp (x, y) = Func vec2 "vec2" [toOp x, toOp y]

instance ShaderTerm (Float, Float, Float) where
  toOp (x, y, z) = Func vec3 "vec3" [toOp x, toOp y, toOp z]

instance ShaderTerm (Float, Float, Float, Float) where
  toOp (x, y, z, w) = Func vec4 "vec4" [toOp x, toOp y, toOp z, toOp w]

instance ShaderTerm (Int, Int) where
  toOp (x, y) = Func ivec2 "ivec2" [toOp x, toOp y]

instance ShaderTerm (Int, Int, Int) where
  toOp (x, y, z) = Func ivec3 "ivec3" [toOp x, toOp y, toOp z]

instance ShaderTerm (Int, Int, Int, Int) where
  toOp (x, y, z, w) = Func ivec4 "ivec4" [toOp x, toOp y, toOp z, toOp w]

instance ShaderTerm (Bool, Bool) where
  toOp (x, y) = Func bvec2 "bvec2" [toOp x, toOp y]

instance ShaderTerm (Bool, Bool, Bool) where
  toOp (x, y, z) = Func bvec3 "bvec3" [toOp x, toOp y, toOp z]

instance ShaderTerm (Bool, Bool, Bool, Bool) where
  toOp (x, y, z, w) = Func bvec4 "bvec4" [toOp x, toOp y, toOp z, toOp w]

instance ShaderTerm (Double, Double) where
  toOp (x, y) = Func dvec2 "dvec2" [toOp x, toOp y]

instance ShaderTerm (Double, Double, Double) where
  toOp (x, y, z) = Func dvec3 "dvec3" [toOp x, toOp y, toOp z]

instance ShaderTerm (Double, Double, Double, Double) where
  toOp (x, y, z, w) = Func dvec4 "dvec4" [toOp x, toOp y, toOp z, toOp w]

-- linear
instance ShaderTerm (V2 Float) where
  toOp (V2 x y) = Func vec2 "vec2" [toOp x, toOp y]

instance ShaderTerm (V3 Float) where
  toOp (V3 x y z) = Func vec3 "vec3" [toOp x, toOp y, toOp z]

instance ShaderTerm (V4 Float) where
  toOp (V4 x y z w) = Func vec4 "vec4" [toOp x, toOp y, toOp z, toOp w]

instance ShaderTerm (V2 Int) where
  toOp (V2 x y) = Func ivec2 "ivec2" [toOp x, toOp y]

instance ShaderTerm (V3 Int) where
  toOp (V3 x y z) = Func ivec3 "ivec3" [toOp x, toOp y, toOp z]

instance ShaderTerm (V4 Int) where
  toOp (V4 x y z w) = Func ivec4 "ivec4" [toOp x, toOp y, toOp z, toOp w]

instance ShaderTerm (V2 Bool) where
  toOp (V2 x y) = Func bvec2 "bvec2" [toOp x, toOp y]

instance ShaderTerm (V3 Bool) where
  toOp (V3 x y z) = Func bvec3 "bvec3" [toOp x, toOp y, toOp z]

instance ShaderTerm (V4 Bool) where
  toOp (V4 x y z w) = Func bvec4 "bvec4" [toOp x, toOp y, toOp z, toOp w]

instance ShaderTerm (V2 Double) where
  toOp (V2 x y) = Func dvec2 "dvec2" [toOp x, toOp y]

instance ShaderTerm (V3 Double) where
  toOp (V3 x y z) = Func dvec3 "dvec3" [toOp x, toOp y, toOp z]

instance ShaderTerm (V4 Double) where
  toOp (V4 x y z w) = Func dvec4 "dvec4" [toOp x, toOp y, toOp z, toOp w]

-- Linear is row major GLSL is column major
instance ShaderTerm (M22 Float) where
  toOp (V2 (V2 a b) (V2 c d)) = Func mat2 "mat2" [toOp a, toOp c, toOp b, toOp d]

instance ShaderTerm (M23 Float) where
  toOp (V2 (V3 a b c) (V3 d e f)) = Func mat2x3 "mat2x3" [toOp a, toOp d, toOp b, toOp e, toOp c, toOp f]

instance ShaderTerm (M24 Float) where
  toOp (V2 (V4 a b c d) (V4 e f g h)) = Func mat2x4 "mat2x4" [toOp a, toOp e, toOp b, toOp f, toOp c, toOp g, toOp d, toOp h]

instance ShaderTerm (M32 Float) where
  toOp (V3 (V2 a b) (V2 c d) (V2 e f)) = Func mat3x2 "mat3x2" [toOp a, toOp c, toOp e, toOp b, toOp d, toOp f]

instance ShaderTerm (M33 Float) where
  toOp (V3 (V3 a b c) (V3 d e f) (V3 g h i)) = Func mat3 "mat3" [toOp a, toOp d, toOp g, toOp b, toOp e, toOp h, toOp c, toOp f, toOp i]

instance ShaderTerm (M34 Float) where
  toOp (V3 (V4 a b c d) (V4 e f g h) (V4 i j k l)) = Func mat3x4 "mat3x4" [toOp a, toOp e, toOp i, toOp b, toOp f, toOp j, toOp c, toOp g, toOp k, toOp d, toOp h, toOp l]

instance ShaderTerm (M42 Float) where
  toOp (V4 (V2 a b) (V2 c d) (V2 e f) (V2 g h)) = Func mat4x2 "mat4x2" [toOp a, toOp c, toOp e, toOp g, toOp b, toOp d, toOp f, toOp h]

instance ShaderTerm (M43 Float) where
  toOp (V4 (V3 a b c) (V3 d e f) (V3 g h i) (V3 j k l)) = Func mat4x3 "mat4x3" [toOp a, toOp d, toOp g, toOp j, toOp b, toOp e, toOp h, toOp k, toOp c, toOp f, toOp i, toOp l]

instance ShaderTerm (M44 Float) where
  toOp (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) = Func mat4 "mat4" [toOp a, toOp e, toOp i, toOp m, toOp b, toOp f, toOp j, toOp n, toOp c, toOp g, toOp k, toOp o, toOp d, toOp h, toOp l, toOp p]

instance ShaderTerm (M22 Double) where
  toOp (V2 (V2 a b) (V2 c d)) = Func dmat2 "dmat2" [toOp a, toOp c, toOp b, toOp d]

instance ShaderTerm (M23 Double) where
  toOp (V2 (V3 a b c) (V3 d e f)) = Func dmat2x3 "dmat2x3" [toOp a, toOp d, toOp b, toOp e, toOp c, toOp f]

instance ShaderTerm (M24 Double) where
  toOp (V2 (V4 a b c d) (V4 e f g h)) = Func dmat2x4 "dmat2x4" [toOp a, toOp e, toOp b, toOp f, toOp c, toOp g, toOp d, toOp h]

instance ShaderTerm (M32 Double) where
  toOp (V3 (V2 a b) (V2 c d) (V2 e f)) = Func dmat3x2 "dmat3x2" [toOp a, toOp c, toOp e, toOp b, toOp d, toOp f]

instance ShaderTerm (M33 Double) where
  toOp (V3 (V3 a b c) (V3 d e f) (V3 g h i)) = Func dmat3 "dmat3" [toOp a, toOp d, toOp g, toOp b, toOp e, toOp h, toOp c, toOp f, toOp i]

instance ShaderTerm (M34 Double) where
  toOp (V3 (V4 a b c d) (V4 e f g h) (V4 i j k l)) = Func dmat3x4 "dmat3x4" [toOp a, toOp e, toOp i, toOp b, toOp f, toOp j, toOp c, toOp g, toOp k, toOp d, toOp h, toOp l]

instance ShaderTerm (M42 Double) where
  toOp (V4 (V2 a b) (V2 c d) (V2 e f) (V2 g h)) = Func dmat4x2 "dmat4x2" [toOp a, toOp c, toOp e, toOp g, toOp b, toOp d, toOp f, toOp h]

instance ShaderTerm (M43 Double) where
  toOp (V4 (V3 a b c) (V3 d e f) (V3 g h i) (V3 j k l)) = Func dmat4x3 "dmat4x3" [toOp a, toOp d, toOp g, toOp j, toOp b, toOp e, toOp h, toOp k, toOp c, toOp f, toOp i, toOp l]

instance ShaderTerm (M44 Double) where
  toOp (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) = Func dmat4 "dmat4" [toOp a, toOp e, toOp i, toOp m, toOp b, toOp f, toOp j, toOp n, toOp c, toOp g, toOp k, toOp o, toOp d, toOp h, toOp l, toOp p]

data Var = Var Type String deriving (Eq, Show, Ord)

instance ShaderTypeable Var where
  getType (Var t _) = t

instance ShaderTerm Var where
  toOp = WrappedVar

data Statement = Uniform Type String | In Type String (Maybe Int) | Out Type String | Assign Var Op | CreateVar Var deriving (Eq, Show, Ord)

isToplevel :: Statement -> Bool
isToplevel (Uniform _ _) = True
isToplevel (In {}) = True
isToplevel (Out _ _) = True
isToplevel _ = False

isAssign :: Statement -> Bool
isAssign (Assign _ _) = True
isAssign _ = False

isCreateVar :: Statement -> Bool
isCreateVar (CreateVar _) = True
isCreateVar _ = False

type ShaderM a = WriterT [Statement] (StateT Int (Except String)) a

-- ShaderM

uniformS :: Type -> String -> ShaderM Var
uniformS t n = do
  tell [Uniform t n]
  return $ Var t n

varyIn :: Type -> String -> ShaderM Var
varyIn t n = do
  tell [In t n Nothing]
  return $ Var t n

inAttribute :: Type -> Int -> ShaderM Var
inAttribute t loc = do
  let n = "attr" ++ show loc
  tell [In t n (Just loc)]
  return $ Var t n

varyOut :: Type -> String -> ShaderM Var
varyOut t n = do
  tell [Out t n]
  return $ Var t n

var :: Type -> ShaderM Var
var t = do
  i <- get
  let v = Var t ("var" ++ show i)
  tell [CreateVar v]
  put (i + 1)
  return v

gl_Position :: Var
gl_Position = Var (vector Float 4) "gl_Position"

infixl 6 !*

(!*) :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
o1 !* o2 =
  let t1 = getType o1
      t2 = getType o2
      resType = multiplyType t1 t2
   in case resType of
        Just t -> Mul t (toOp o1) (toOp o2)
        Nothing -> WrappedError $ "Cannot multiply " ++ show t1 ++ " and " ++ show t2

infixl 6 !/

(!/) :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
o1 !/ o2
  | isSampler (getType o1) || isSampler (getType o2) = WrappedError "Cannot divide sampler"
  | getType o1 == getType o2 = Div (getType o1) (toOp o1) (toOp o2)
  | otherwise = WrappedError "Cannot divide"

infixl 5 !+

(!+) :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
o1 !+ o2
  | isSampler (getType o1) || isSampler (getType o2) = WrappedError "Cannot add sampler"
  | getType o1 == getType o2 = Add (getType o1) (toOp o1) (toOp o2)
  | otherwise = WrappedError "Cannot add"

infixl 5 !-

(!-) :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
o1 !- o2
  | isSampler (getType o1) || isSampler (getType o2) = WrappedError "Cannot subtract sampler"
  | getType o1 == getType o2 = Sub (getType o1) (toOp o1) (toOp o2)
  | otherwise = WrappedError "Cannot subtract"

infixl 4 <~

(<~) :: (ShaderTerm a) => Var -> a -> ShaderM ()
(<~) v1 v2 =
  let assign' :: Var -> Op -> ShaderM ()
      assign' _ (WrappedError e) = throwError e
      assign' v1' v2' = if getType v1' == getType v2' then tell [Assign v1' v2'] else throwError $ "Cannot assign. Types do not match (" ++ show (getType v1') ++ " and " ++ show (getType v2') ++ ")"
   in assign' v1 $ toOp v2

normalize_ :: (ShaderTerm a) => a -> Op
normalize_ v =
  let t = getType v
      v' = toOp v
   in if isVector t then Func t "normalize" [v'] else WrappedError $ "Cannot normalize non-vector (" ++ show t ++ ")"

reflect_ :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
reflect_ v n =
  let t1 = getType v
      t2 = getType n
      v' = toOp v
      n' = toOp n
   in if isVector t1 && isVector t2 && t1 == t2 then Func t1 "reflect" [v', n'] else WrappedError $ "Cannot reflect (" ++ show t1 ++ " and " ++ show t2 ++ ")"

pow_ :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
pow_ v1 v2 =
  let t1 = getType v1
      t2 = getType v2
      v1' = toOp v1
      v2' = toOp v2
   in if isPrimitive t1 && isPrimitive t2 then Func t1 "pow" [v1', v2'] else WrappedError $ "Cannot pow non-scalars (" ++ show t1 ++ " and " ++ show t2 ++ ")"

length_ :: (ShaderTerm a) => a -> Op
length_ v =
  let t = getType v
      v' = toOp v
   in if isVector t then Func float "length" [v'] else WrappedError $ "Cannot get length of non-vector (" ++ show t ++ ")"

dot_ :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
dot_ v1 v2 =
  let t1 = getType v1
      t2 = getType v2
      v1' = toOp v1
      v2' = toOp v2
   in if isVector t1 && isVector t2 && t1 == t2 then Func float "dot" [v1', v2'] else WrappedError $ "Cannot get dot product of non-vectors (" ++ show t1 ++ " and " ++ show t2 ++ ")"

max_ :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
max_ v1 v2 =
  let t1 = getType v1
      t2 = getType v2
      v1' = toOp v1
      v2' = toOp v2
   in if t1 == t2 then Func t1 "max" [v1', v2'] else WrappedError $ "Cannot get max of different types (" ++ show t1 ++ " and " ++ show t2 ++ ")"

min_ :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
min_ v1 v2 =
  let t1 = getType v1
      t2 = getType v2
      v1' = toOp v1
      v2' = toOp v2
   in if t1 == t2 then Func t1 "min" [v1', v2'] else WrappedError $ "Cannot get min of different types (" ++ show t1 ++ " and " ++ show t2 ++ ")"

abs_ :: (ShaderTerm a) => a -> Op
abs_ v =
  let t = getType v
      v' = toOp v
   in if isPrimitive t then Func t "abs" [v'] else WrappedError $ "Cannot get abs of non-scalar (" ++ show t ++ ")"

sample_ :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
sample_ s t =
  let t1 = getType s
      t2 = getType t
      s' = toOp s
      t' = toOp t
   in if isSampler t1 && isVector t2 then Func t2 "texture" [s', t'] else WrappedError $ "Cannot sample (" ++ show t1 ++ " and " ++ show t2 ++ ")"

infixl 8 !.

(!.) :: (ShaderTerm a) => a -> String -> Op
v !. a = either WrappedError id $ do
  let t = getType v
  unless (isVector t) $ Left "Cannot access field of non-vector"
  unless (all (`elem` "xyzwrgba") a) $ Left "Invalid field name"
  unless (not (null a) && length a <= 4) $ Left "Invalid field name length"
  let (Just b) = baseType t
  let t' = vector b (length a)
  return $ Field t' (toOp v) a


infixl 7 !:

(!:) :: (ShaderTerm a, ShaderTerm b) => a -> b -> Op
a !: b = case vectorConcatLookup (toOp a) (toOp b) of
  Just op -> op
  Nothing -> WrappedError $ "Cannot concatenate vectors " ++ show (getType a) ++ " and " ++ show (getType b)

vectorConcatLookup :: Op -> Op -> Maybe Op
vectorConcatLookup a b = do
  b1 <- baseType $ getType a
  b2 <- baseType $ getType b
  guard $ b1 == b2

  (n, 1) <- tensorSize $ getType a
  (m, 1) <- tensorSize $ getType b
  guard $ n + m <= 4 && n + m > 1
  return $ Func (vector b1 (n + m)) ("vec" ++ show (n + m)) [a, b]

-- Util

outValue :: (ShaderTerm a) => String -> a -> ShaderM ()
outValue str op = do
  tmp <- varyOut (getType op) str
  tmp <~ op

-- Code generation

infixOp :: String -> Op -> Op -> Computation String
infixOp op o1 o2 = do
  o1' <- opToCode o1
  o2' <- opToCode o2
  return $ "(" ++ o1' ++ " " ++ op ++ " " ++ o2' ++ ")"

opToCode :: Op -> Computation String
opToCode (Field _ o f) = do
  o' <- opToCode o
  return $ "("++o' ++ "." ++ f++")"
opToCode (Add _ o1 o2) = infixOp "+" o1 o2
opToCode (Sub _ o1 o2) = infixOp "-" o1 o2
opToCode (Mul _ o1 o2) = infixOp "*" o1 o2
opToCode (Div _ o1 o2) = infixOp "/" o1 o2
opToCode (Func _ n ops) = do
  ops' <- mapM opToCode ops
  return $ n ++ "(" ++ intercalate ", " ops' ++ ")"
opToCode (WrappedVar (Var _ n)) = return n
opToCode (WrappedError e) = throwError e
opToCode (Lit _ l) = return l

statementToCode :: Statement -> Computation String
statementToCode (Uniform t n) = typeName t >>= \tn -> return $ "uniform " ++ tn ++ " " ++ n ++ ";\n"
statementToCode (In t n loc) = typeName t >>= \tn -> return $ maybe "" (\l -> "layout (location = " ++ show l ++ ") ") loc ++ "in " ++ tn ++ " " ++ n ++ ";\n"
statementToCode (Out t n) = typeName t >>= \tn -> return $ "out " ++ tn ++ " " ++ n ++ ";\n"
statementToCode (CreateVar (Var t n)) = typeName t >>= \tn -> return $ tn ++ " " ++ n ++ ";\n"
statementToCode (Assign (Var _ n) op) = opToCode op >>= \code -> return $ n ++ " = " ++ code ++ ";\n"

newtype ShaderVersion = ShaderVersion String

createShaderM :: ShaderVersion -> ShaderM a -> Computation String
createShaderM (ShaderVersion v) builder = do
  (_, stmts) <- evalStateT (runWriterT builder) 0
  let toplevel = map head $ group $ sort $ filter isToplevel stmts
  let vars = filter isCreateVar stmts
  let assigns = filter isAssign stmts

  toplevel' <- mapM statementToCode toplevel
  vars' <- mapM statementToCode vars
  assigns' <- mapM statementToCode assigns

  return $ "#version " ++ v ++ "\n" ++ concat toplevel' ++ "\n\nvoid main(){\n" ++ concat vars' ++ "\n" ++ concat assigns' ++ "}\n"

data ShaderType = FragmentShader | VertexShader

data ShaderTmp = forall a. ShaderTmp ShaderType (ShaderM a) | UniformLocInfo Type String | AttributeInfo Type String | Varying Type String

isShader :: ShaderTmp -> Bool
isShader (ShaderTmp _ _) = True
isShader _ = False

isUniformInfo :: ShaderTmp -> Bool
isUniformInfo (UniformLocInfo _ _) = True
isUniformInfo _ = False

isAttributeInfo :: ShaderTmp -> Bool
isAttributeInfo (AttributeInfo _ _) = True
isAttributeInfo _ = False

isVaryingInfo :: ShaderTmp -> Bool
isVaryingInfo (Varying _ _) = True
isVaryingInfo _ = False

isFragmentShader :: ShaderTmp -> Bool
isFragmentShader (ShaderTmp FragmentShader _) = True
isFragmentShader _ = False

isVertexShader :: ShaderTmp -> Bool
isVertexShader (ShaderTmp VertexShader _) = True
isVertexShader _ = False

newtype ShaderProgramM a = ShaderProgramM (WriterT [ShaderTmp] (StateT Int (Except String)) a)
  deriving (Functor, Applicative, Monad, MonadWriter [ShaderTmp], MonadState Int, MonadError String)

instance MonadFail ShaderProgramM where
  fail = throwError

shaderM :: ShaderType -> ShaderM a -> ShaderProgramM ()
shaderM t monad = do
  let tmp = ShaderTmp t monad
  tell [tmp]
  return ()

uniform :: Type -> ShaderProgramM Var
uniform ts = do
  i <- get
  put $ i + 1
  let loc = "uniform" ++ show i
  tell [UniformLocInfo ts loc]
  return $ Var ts loc

uniforms :: [Type] -> ShaderProgramM [Var]
uniforms = mapM uniform

attributes :: BufferDescription -> ShaderProgramM [Var]
attributes (BufferDescription attrs _) = do
  let ts =
        attrs <&> \(BufferAttribute x y) -> matrix Float x y
  let vars = zipWith Var ts $ map (\i -> "attribute" ++ show i) [0 :: Int ..]
  mapM_ (\(Var t n) -> tell [AttributeInfo t n]) vars
  return vars

varying :: Type -> ShaderProgramM Var
varying ts = do
  i <- get
  put $ i + 1
  let loc = "varying" ++ show i
  tell [Varying ts loc]
  return $ Var ts loc

precompileProgram :: ShaderVersion -> ShaderProgramM a -> Computation (String, String, Maybe UniformInfo)
precompileProgram version (ShaderProgramM builder) = do
  (_, stmts) <- evalStateT (runWriterT builder) 0
  let shaders = filter isShader stmts

  let unis = filter isUniformInfo stmts
  let shaderUniform = map (\(UniformLocInfo t n) -> Uniform t n) unis
  let uniInfo = case unis of
        [] -> Nothing
        _ -> Just $ UniformInfo (map (\(UniformLocInfo t _) -> t) unis) (map (\(UniformLocInfo _ n) -> n) unis)
  let shaders' = map (\(ShaderTmp t m) -> ShaderTmp t (tell shaderUniform >> m)) shaders

  let attrs = filter isAttributeInfo stmts
  let shaderAttributes = zipWith (\(AttributeInfo t n) i -> In t n (Just i)) attrs [0 :: Int ..]
  let shaders'' =
        map
          ( \case
              ShaderTmp VertexShader m -> ShaderTmp VertexShader $
                do
                  tell shaderAttributes
                  mapM_
                    ( \(AttributeInfo t n) -> do
                        v <- varyOut t ("v_" ++ n)
                        v <~ Var t n
                    )
                    attrs
                  m
              ShaderTmp FragmentShader m -> ShaderTmp FragmentShader $
                do
                  mapM_
                    ( \(AttributeInfo t n) -> do
                        v <- varyIn t ("v_" ++ n)
                        tell [CreateVar (Var t n)]
                        Var t n <~ v
                    )
                    attrs
                  m
              x -> x
          )
          shaders'

  let varyings = filter isVaryingInfo stmts
  let shaderVertexVaryings = map (\(Varying t n) -> Out t n) varyings
  let shaderFragmentVaryings = map (\(Varying t n) -> In t n Nothing) varyings
  let shaders''' =
        map
          ( \case
              ShaderTmp VertexShader m -> ShaderTmp VertexShader $ tell shaderVertexVaryings >> m
              ShaderTmp FragmentShader m -> ShaderTmp FragmentShader $ tell shaderFragmentVaryings >> m
              x -> x
          )
          shaders''

  let vertexShaders = filter isVertexShader shaders'''
  let fragmentShaders = filter isFragmentShader shaders'''
  case (vertexShaders, fragmentShaders) of
    ([ShaderTmp VertexShader v], [ShaderTmp FragmentShader f]) -> do
      v' <- createShaderM version v
      f' <- createShaderM version f
      return (v', f', uniInfo)
    _ -> throwError "Need exactly one fragment shader and one vertex shader"

compileProgram :: ShaderVersion -> ShaderProgramM a -> ComputationIO Shader
compileProgram version builder = do
  (v, f, info) <- liftComputationIO $ precompileProgram version builder
  (GLShader prog vert frag Nothing) <- createShader v f
  return $ GLShader prog vert frag info

-- shaderExample description = do
--  [scale, color] <- uniforms [vec2, vec4]
--  [pos] <- attributes description
--  shaderM VertexShader $ do
--    gl_Position <~ (pos !* scale) !: (0 :: Float, 1 :: Float)
--
--  shaderM FragmentShader $ do
--    v <- var vec4
--    v <~ color
--    outValue "color" v
