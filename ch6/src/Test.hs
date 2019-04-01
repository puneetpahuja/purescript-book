module Test where

class Monoid m => Action m a where
  act :: m -> a -> a

newtype Self m = Self m

instance Monoid m => Action m (Self m) where
  act x (Self y) = Self (x <> y)