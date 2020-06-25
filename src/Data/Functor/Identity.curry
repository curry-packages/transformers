module Data.Functor.Identity where

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Ord, Read, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

-- instance Applicative Identity where
--   pure = Identity
--   Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  m >>= k = k (runIdentity m)
  return a = Identity a
