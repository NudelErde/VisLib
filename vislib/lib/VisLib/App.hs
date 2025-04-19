{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module VisLib.App where

import Control.Monad.Cont
import Control.Monad.State
import Data.IORef
import Control.Monad
import Data.Traversable.WithIndex
import Control.Monad.Reader

type AppMonad io d r a = ReaderT d (ContT r io) a

runApp :: AppMonad IO d a a -> d -> IO a
runApp app d = evalContT $ runReaderT app d

resource :: MonadIO io => io a -> (a -> io b) -> AppMonad io d r a
resource acq rel = lift $ ContT $ \c -> do
  res <- acq
  res' <- c res
  _ <- rel res
  return res'

(<?>) :: MonadIO m => (m a -> (a -> m b) -> AppMonad m d r c) -> String -> (m a -> (a -> m b) -> AppMonad m d r c)
(<?>) f name acq rel = f (liftIO (putStrLn ("create " ++ name)) >> acq) (\x -> liftIO (putStrLn ("destroy " ++ name)) >> rel x)

resourceN :: MonadIO io => String -> io a -> (a -> io b) -> AppMonad io d r a
resourceN name = resource <?> name

resourceR :: MonadIO io => io a -> (a -> io b) -> AppMonad io d r (a, AppMonad io d r ())
resourceR acq rel = do
  released <- liftIO $ newIORef False
  lift $ ContT $ \c -> do
    res <- acq
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

withData :: MonadIO io => d -> AppMonad io d r a -> AppMonad io e r a
withData d app = lift $ runReaderT app d

scope :: MonadIO io => AppMonad io d a a -> AppMonad io d r a
scope app = ask >>= lift . lift . evalContT . runReaderT app

newtype ResourceMutable a = ResourceMutable (IORef (a, IO ()))

resourceM :: MonadIO io => IO a -> (a -> IO b) -> AppMonad io d r (ResourceMutable a)
resourceM acq rel = lift $ ContT $ \c -> do
  a <- liftIO acq
  released <- liftIO $ newIORef False
  let release = do
        released' <- readIORef released
        unless released' $ do
          _ <- rel a
          writeIORef released True
  ref <- liftIO $ newIORef (a, release)
  v <- c $ ResourceMutable ref
  (_, release') <- liftIO $ readIORef ref
  liftIO release'
  return v

resourceMN :: MonadIO io => String -> IO a -> (a -> IO b) -> AppMonad io d r (ResourceMutable a)
resourceMN name acq rel = do
  let name' = name ++ " (mutable)"
  let release x = liftIO (putStrLn ("destroy " ++ name')) >> rel x
  let acquire = liftIO (putStrLn ("create " ++ name')) >> acq
  resourceM acquire release

updateResourceMutable :: MonadIO io => ResourceMutable a -> IO a -> (a -> IO b) -> io a
updateResourceMutable (ResourceMutable ref) acq rel = do
  (_, release) <- liftIO $ readIORef ref
  liftIO release
  released <- liftIO $ newIORef False
  a <- liftIO acq
  let release' = do
        released' <- readIORef released
        unless released' $ do
          _ <- rel a
          writeIORef released True
  liftIO $ writeIORef ref (a, release')
  return a

updateResourceMutableN :: MonadIO io => String -> ResourceMutable a -> IO a -> (a -> IO b) -> io a
updateResourceMutableN name (ResourceMutable ref) acq rel = do
  let name' = name ++ " (mutable)"
  let release x = liftIO (putStrLn ("destroy " ++ name')) >> rel x
  let acquire = liftIO (putStrLn ("create " ++ name')) >> acq
  updateResourceMutable (ResourceMutable ref) acquire release

getResource :: MonadIO io => ResourceMutable a -> io a
getResource (ResourceMutable ref) = do
  (a, _) <- liftIO $ readIORef ref
  return a

destroyResource :: MonadIO io => ResourceMutable a -> io ()
destroyResource (ResourceMutable ref) = do
  (_, release) <- liftIO $ readIORef ref
  liftIO release

pureResourceM :: MonadIO io => a -> io (ResourceMutable a)
pureResourceM a = do
  ref <- liftIO $ newIORef (a, return ())
  return $ ResourceMutable ref

resourceM' :: (Traversable t, MonadIO io) => t ((IO a -> (a -> IO b) -> (IO a, a -> IO b)) -> (IO a, a -> IO b)) -> AppMonad io d r (ResourceMutable (t a))
resourceM' t = lift $ ContT $ \c -> do
  let t' = fmap (\x -> x (,)) t
  let acquire = forM t' $ \(acq, rel) -> do
        a <- acq
        let release = rel a
        return (a, release)
  let release = mapM_ $ \(_, rel) -> do
        rel
  released <- liftIO $ newIORef False
  a <- liftIO acquire
  let release' = do
        released' <- readIORef released
        unless released' $ do
          release a
          writeIORef released True
  ref <- liftIO $ newIORef (fmap fst a, release')
  v <- c $ ResourceMutable ref
  (_, release'') <- liftIO $ readIORef ref
  liftIO release''
  return v

resourceMN' :: (Show i, TraversableWithIndex i t, MonadIO io) => String -> t ((IO a -> (a -> IO b) -> (IO a, a -> IO b)) -> (IO a, a -> IO b)) -> AppMonad io d r (ResourceMutable (t a))
resourceMN' s t = lift $ ContT $ \c -> do
  let t' = fmap (\x -> x (,)) t
  let acquire = iforM t' $ \i (acq, rel) -> do
        print $ "create " ++ s ++ " (" ++ show i ++ ", mutable)"
        a <- acq
        let release = do
              print $ "destroy " ++ s ++ " (" ++ show i ++ ", mutable)"
              rel a
        return (a, release)
  let release = mapM_ $ \(_, rel) -> do
        rel
  released <- liftIO $ newIORef False
  a <- liftIO acquire
  let release' = do
        released' <- readIORef released
        unless released' $ do
          release a
          writeIORef released True
  ref <- liftIO $ newIORef (fmap fst a, release')
  v <- c $ ResourceMutable ref
  (_, release'') <- liftIO $ readIORef ref
  liftIO release''
  return v

updateResourceMutable' :: (Traversable t, MonadIO io) => ResourceMutable (t a) -> t ((IO a -> (a -> IO b) -> (IO a, a -> IO b)) -> (IO a, a -> IO b)) -> io (t a)
updateResourceMutable' (ResourceMutable ref) t = do
  (_, releaseOld) <- liftIO $ readIORef ref
  liftIO releaseOld
  released <- liftIO $ newIORef False
  let t' = fmap (\x -> x (,)) t
  let acquire = forM t' $ \(acq, rel) -> do
        a <- acq
        let release = do
              rel a
        return (a, release)
  let release = mapM_ $ \(_, rel) -> do
        rel
  a <- liftIO acquire
  let release' = do
        released' <- readIORef released
        unless released' $ do
          release a
          writeIORef released True
  liftIO $ writeIORef ref (fmap fst a, release')
  return $ fmap fst a


updateResourceMutableN' :: (Show i, TraversableWithIndex i t, MonadIO io) => String -> ResourceMutable (t a) -> t ((IO a -> (a -> IO b) -> (IO a, a -> IO b)) -> (IO a, a -> IO b)) -> io (t a)
updateResourceMutableN' s (ResourceMutable ref) t = do
  (_, releaseOld) <- liftIO $ readIORef ref
  liftIO releaseOld
  released <- liftIO $ newIORef False
  let t' = fmap (\x -> x (,)) t
  let acquire = iforM t' $ \i (acq, rel) -> do
        print $ "create " ++ s ++ " (" ++ show i ++ ", mutable)"
        a <- acq
        let release = do
              print $ "destroy " ++ s ++ " (" ++ show i ++ ", mutable)"
              rel a
        return (a, release)
  let release = mapM_ $ \(_, rel) -> do
        rel
  a <- liftIO acquire
  let release' = do
        released' <- readIORef released
        unless released' $ do
          release a
          writeIORef released True
  liftIO $ writeIORef ref (fmap fst a, release')
  return $ fmap fst a
