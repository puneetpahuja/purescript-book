module Exercises where

import Prelude

import Control.MonadZero (guard)
import Data.Array (null, filter, (..), cons)
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl)
import Partial.Unsafe (unsafePartial)

-- TODO: make it work, erroring out with stack overflow error. the ps -> js conversion is fucked up.
numEvens :: Array Int -> Int
numEvens arr =
    if null arr
        then 0
        else
            if isEven (unsafePartial head arr)
                then 1 + restEvens
                else restEvens
                  where
                    restEvens = numEvens (unsafePartial tail arr)


isEven :: Int -> Boolean
isEven 0 = true
isEven n = not <<< isEven $ n-1

squares :: Array Number -> Array Number
squares = map (\n -> n * n)

removeNegatives :: Array Number -> Array Number
removeNegatives = filter (\n -> n >= 0.0)

-- TODO: experiment with the precedence level and associativity of your operator in psci.
removeNegatives' :: Array Number -> Array Number
removeNegatives' xs = (\n -> n >= 0.0) <$?> xs

infix 8 filter as <$?>

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1..n
  j <- i..n
  guard $ i*j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = factors n == [[1, n]]

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1..n
  b <- a..n
  c <- b..n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

-- TODO: this
-- factorization :: Int -> Array (Array Int)

-- does only one factorization. do all.
factorization' :: Int -> Array Int
factorization' 1 = []
factorization' n = cons fstFctr (factorization' (n / fstFctr))
  where
    fstFctr = firstFactor n
   
    firstFactor :: Int -> Int
    firstFactor 1 = 1
    firstFactor x = firstFactor' x 2

    firstFactor' :: Int -> Int -> Int
    firstFactor' y q = case (mod y q) of
      0 -> q
      _ -> firstFactor' y (q+1)


areAllTrue :: Array Boolean -> Boolean
areAllTrue = foldl (\y x -> x && y) true

-- q: characterize those Arrays `xs` for which the function `foldl (==) false xs` returns true.
-- a: Boolean Arrays in which Number of elements that are false are odd. because
-- false in an Array flips the accumulator as (false == x = not x) whereas true
-- keeps the accumulator same (true == x = x)
areNumFalsesOdd :: Array Boolean -> Boolean
areNumFalsesOdd = foldl (==) false

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> xs <> [x]) []

count :: forall a. (a -> Boolean) -> Array a -> Int
count p xs = count' p xs 0
  where
    count' _ [] acc = acc
    count' p' ys acc = if p' (unsafePartial head ys)
                        then count' p' (unsafePartial tail ys) (acc + 1)
                        else count' p' (unsafePartial tail ys) acc
