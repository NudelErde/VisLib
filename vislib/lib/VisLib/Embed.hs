{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module VisLib.Embed where

import Control.Monad (forM, forM_, guard, join)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Class as TC (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Writer (Writer, WriterT, execWriter, mapWriter, runWriterT, tell)
import Data.Bifunctor (second)
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.Composition
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Lib (sigD, varP)
import Language.Haskell.TH.Syntax (Dec, Exp, Q, Type, makeRelativeToProject, mkName, runIO)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs

type Embedder a = Writer [InnerEmbedder ()] a

data Element = Declaration Dec | File String FilePath

type InnerEmbedder a = ReaderT (String, FilePath) (WriterT [Element] (MaybeT Q)) a

liftQ :: Q a -> InnerEmbedder a
liftQ = TC.lift . TC.lift . TC.lift

liftInner :: InnerEmbedder () -> Embedder ()
liftInner = tell . pure

runEmbedder :: Embedder () -> String -> FilePath -> Q [Dec]
runEmbedder e n p = do
  relP <- makeRelativeToProject p
  elements <- snd . fromMaybe ((), []) <$> runInnerEmbedders (execWriter e) n relP
  fmap join $ forM elements $ \case
    Declaration d -> return [d]
    File n' p' -> runEmbedder e n' p'

runInnerEmbedders :: [InnerEmbedder a] -> String -> FilePath -> Q (Maybe (a, [Element]))
runInnerEmbedders [] _ _ = return Nothing
runInnerEmbedders (i : is) s p =
  runMaybeT (runWriterT $ runReaderT i (s, p)) >>= \case
    Nothing -> runInnerEmbedders is s p
    v -> return v

filterNode :: (String -> FilePath -> Q Bool) -> InnerEmbedder a -> InnerEmbedder a
filterNode f i = do
  (n, p) <- ask
  b <- liftQ $ f n p
  guard b
  i

filterFile :: InnerEmbedder a -> InnerEmbedder a
filterFile = filterNode (curry $ runIO . doesFileExist . snd)

filterDirectory :: InnerEmbedder a -> InnerEmbedder a
filterDirectory = filterNode (curry $ runIO . doesDirectoryExist . snd)

filterExtension :: String -> InnerEmbedder a -> InnerEmbedder a
filterExtension ext = filterFile . filterNode (\_ p -> return $ ext == takeExtension p)

whenFile :: Embedder () -> Embedder ()
whenFile = mapWriter $ second $ map filterFile

whenFile' :: InnerEmbedder () -> Embedder ()
whenFile' = liftInner . filterFile

whenExtension :: String -> Embedder () -> Embedder ()
whenExtension ext = mapWriter $ second $ map $ filterExtension ext

whenExtension' :: String -> InnerEmbedder () -> Embedder ()
whenExtension' ext = liftInner . filterExtension ext

whenDirecory :: Embedder () -> Embedder ()
whenDirecory = mapWriter $ second $ map filterDirectory

whenDirecory' :: InnerEmbedder () -> Embedder ()
whenDirecory' = liftInner . filterDirectory

emitFile :: String -> FilePath -> InnerEmbedder ()
emitFile = tell . pure .: File

emitDecl :: Q Dec -> InnerEmbedder ()
emitDecl decl = do
  decl' <- liftQ decl
  tell [Declaration decl']

emitExprN :: String -> Q Exp -> InnerEmbedder ()
emitExprN n expr = do
  decs <- liftQ [d|$(varP $ mkName n) = $(expr)|]
  tell $ map Declaration decs

emitExprN' :: String -> Q Exp -> Q Type -> InnerEmbedder ()
emitExprN' n expr qtyp = do
  let n' = mkName n
  decs <- liftQ [d|$(varP n') = $(expr)|]
  typeDec <- liftQ $ sigD n' qtyp
  tell $ map Declaration (typeDec : decs)

emitExp :: Q Exp -> InnerEmbedder ()
emitExp expr = do
  (n, _) <- ask
  emitExprN n expr

emitExp' :: Q Exp -> Q Type -> InnerEmbedder ()
emitExp' expr qtyp = do
  (n, _) <- ask
  emitExprN' n expr qtyp

recursiveDirectory :: Embedder ()
recursiveDirectory = liftInner recursiveDirectory'

recursiveDirectory' :: InnerEmbedder ()
recursiveDirectory' = filterDirectory $ do
  (n, p) <- ask
  files <- liftQ $ runIO $ listDirectory p
  forM_ files $ \f -> do
    let p' = p </> f
    let n' = n ++ capitalize (takeBaseName f)
    emitFile n' p'

content :: InnerEmbedder String
content = do
  (_, p) <- ask
  liftQ $ runIO $ readFile p

contentBS :: InnerEmbedder BS.ByteString
contentBS = do
  (_, p) <- ask
  liftQ $ runIO $ BS.readFile p

path :: InnerEmbedder FilePath
path = asks snd

name :: InnerEmbedder String
name = asks fst