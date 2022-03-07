module Control.Monad.Trans.Reader where

import Data.Functor.Identity     ( Identity )
import Control.Monad.IO.Class    ( MonadIO (..) )
import Control.Monad.Trans.Class ( MonadTrans (..) )

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f m = ReaderT $ \r -> fmap f (runReaderT m r)

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . const . pure
  mf <*> m = ReaderT $ \r -> (runReaderT mf r) <*> (runReaderT m r)

instance Monad m => Monad (ReaderT r m) where
  return = pure
  m >>= f = ReaderT $ \r -> do x <- runReaderT m r
                               runReaderT (f x) r

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

type Reader r = ReaderT r Identity
