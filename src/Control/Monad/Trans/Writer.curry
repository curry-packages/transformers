module Control.Monad.Trans.Writer where

import Data.Functor.Identity     ( Identity (..) )
import Control.Monad.IO.Class    ( MonadIO (..) )
import Control.Monad.Trans.Class ( MonadTrans (..) )

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance Functor m => Functor (WriterT w m) where
  fmap f m = WriterT $ fmap (\(x, w) -> (f x, w)) (runWriterT m)

instance (Applicative m, Monoid w) => Applicative (WriterT w m) where
  pure x = WriterT $ pure (x, mempty)
  mf <*> m = WriterT $ (\(f, w1) (x, w2) -> (f x, w1 `mappend` w2)) <$> runWriterT mf <*> runWriterT m

instance (Monad m, Monoid w) => Monad (WriterT w m) where
  return = pure
  m >>= f = WriterT $ do (x, w1) <- runWriterT m
                         (y, w2) <- runWriterT (f x)
                         return (y, w1 `mappend` w2)

instance (Alternative m, Monoid w) => Alternative (WriterT w m) where
  empty = WriterT empty
  x <|> y = WriterT $ runWriterT x <|> runWriterT y

type Writer w = WriterT w Identity
