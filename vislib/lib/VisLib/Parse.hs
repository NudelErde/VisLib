module VisLib.Parse where

import Text.Megaparsec hiding (State)

import VisLib.Base
import Data.Void
import Data.Maybe
import Control.Monad.Except
import Control.Monad.State
import Control.Monad

type Parser i o = Parsec Void i o

expect :: (MonadParsec e s m, Show a, Eq a) => m a -> a -> m ()
expect p x = label (show x) $ do
  v <- p
  guard (v == x)

parseComp :: (VisualStream s, TraversableStream s) => Maybe String -> s -> Parser s a -> Computation a
parseComp source input parser = case runParser parser (fromMaybe "" source) input of
  Left err -> throwError $ errorBundlePretty err
  Right x -> return x

type StatefullParser s i a = ParsecT Void i (State s) a

parseStateComp :: (VisualStream i, TraversableStream i) => Maybe String -> i -> s -> StatefullParser s i a -> Computation a
parseStateComp source input inputState parser = case evalState (runParserT parser (fromMaybe "" source) input) inputState of
  Left err -> throwError $ errorBundlePretty err
  Right x -> return x