-- Examples.purs

module Examples where

import Prelude

import Control.MonadZero (guard)
import Data.Array ((..))

factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = factors' n == [[1, n]]
