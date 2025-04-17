module VisLib.App where

import Control.Monad.Cont
import Control.Monad.State
import Data.IORef
import Control.Monad

type AppMonad io d r a = StateT d (ContT r io) a

runApp :: AppMonad IO d a a -> d -> IO a
runApp app d = evalContT $ evalStateT app d

resource :: MonadIO io => io a -> (a -> io b) -> AppMonad io d r a
resource acc rel = lift $ ContT $ \c -> do
  res <- acc
  res' <- c res
  _ <- rel res
  return res'

(<?>) :: MonadIO m => (m a -> (a -> m b) -> AppMonad m d r c) -> String -> (m a -> (a -> m b) -> AppMonad m d r c)
(<?>) f name acc rel = f (liftIO (putStrLn ("create " ++ name)) >> acc) (\x -> liftIO (putStrLn ("destroy " ++ name)) >> rel x)

resourceN :: MonadIO io => String -> io a -> (a -> io b) -> AppMonad io d r a
resourceN name = resource <?> name

resourceR :: MonadIO io => io a -> (a -> io b) -> AppMonad io d r (a, AppMonad io d r ())
resourceR acc rel = do
  released <- liftIO $ newIORef False
  lift $ ContT $ \c -> do
    res <- acc
    let rel' = do
          released' <- liftIO $ readIORef released
          unless released' $ do
            _ <- rel res
            liftIO $ writeIORef released True
    res' <- c (res, lift $ lift rel')
    _ <- rel'
    
    return res'

resourceRN :: MonadIO io => String -> io a -> (a -> io b) -> AppMonad io d r (a, AppMonad io d r ())
resourceRN name = resourceR <?> name

scope :: MonadIO io => AppMonad io d a a -> AppMonad io d r a
scope app = get >>= lift . lift . evalContT . evalStateT app
