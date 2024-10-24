{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveLift #-}
module VisLib.Buffer.Image where
import VisLib.Base
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Except (MonadError(throwError))
import Control.Monad
import Data.Array.Storable
import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.Word
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Char
import Language.Haskell.TH.Syntax (Lift)

data ImageType = PNG | JPEG | PPM deriving (Show, Lift)

guessImageType :: String -> Maybe ImageType
guessImageType s = case toLower <$> reverse (takeWhile (/= '.') $ reverse s) of
  "png" -> Just PNG
  "jpg" -> Just JPEG
  "ppm" -> Just PPM
  _ -> Nothing

parseImage' :: String -> ComputationIO Image
parseImage' filename = do
  content <- liftIO $ BS.readFile filename
  let ext = toLower <$> reverse (takeWhile (/= '.') $ reverse filename)
  case ext of
    "png" -> parseImage PNG content
    "jpg" -> parseImage JPEG content
    "ppm" -> parseImage PPM content
    _ -> throwError $ "Unknown image format: " ++ ext

readSafe :: Read a => ByteString -> ComputationIO a
readSafe s = case reads $ unpack s of
  [(x, "")] -> return x
  _ -> throwError $ "Invalid number: " ++ unpack s

isWhitespace :: Word8 -> Bool
isWhitespace = (`BS.elem` pack " \t\n\r")

parseImage :: ImageType -> ByteString -> ComputationIO Image
parseImage PPM content = do
  -- check magic
  when (BS.take 2 content /= pack "P6") $ throwError "Invalid PPM magic"
  let content' = BS.drop 3 content
  width <- readSafe @Int $ BS.takeWhile (not . isWhitespace) content'
  let content'' = BS.dropWhile isWhitespace $ BS.dropWhile (not . isWhitespace) content'
  height <- readSafe @Int $ BS.takeWhile (not . isWhitespace) content''
  let content''' = BS.dropWhile isWhitespace $ BS.dropWhile (not . isWhitespace) content''
  maxColor <- readSafe @Int $ BS.takeWhile (not . isWhitespace) content'''
  let content'''' = BS.dropWhile isWhitespace $ BS.dropWhile (not . isWhitespace) content'''

  let pixels = BS.unpack content''''

  if maxColor <= 255 then do
    pixels'' <- liftIO $ newListArray (0, length pixels - 1) pixels
    return $ Image (width, height) GL.RGB pixels''
  else do
    throwError "Not yet supported (max color > 255)"

parseImage format _ = throwError $ "Unsupported image format" ++ show format