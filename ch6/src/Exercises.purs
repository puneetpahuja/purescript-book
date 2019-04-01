module Exercises where
  
import Prelude

import Data.Array (cons, nubBy)
import Data.Array as Array
import Data.Array.Partial (head, tail)
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (class Monoid, mempty)
import Data.String (toCharArray)
import Data.String as String
import Partial.Unsafe (unsafePartial)
  
class MyShow a where
  show' :: a -> String

instance myShowBoolean :: MyShow Boolean where
  show' true = "true'"
  show' false = "false'"

newtype Complex = Complex
  { real :: Number
  , imaginary:: Number
  }

instance eqComplex :: Eq Complex where
  eq (Complex c1) (Complex c2) = 
    (c1.real == c2.real) 
    && (c1.imaginary == c2.imaginary)

instance showComplex :: Show Complex where
  show (Complex c) = show c.real <> " + " <> show c.imaginary <> "i"

data NonEmpty' a = NonEmpty' a (Array a)

instance eqNonEmpty' :: Eq a => Eq (NonEmpty' a) where
  eq (NonEmpty' x xs) (NonEmpty' y ys) = x == y && xs == ys 

instance semigroupNonEmpty' :: Semigroup (NonEmpty' a) where
  append (NonEmpty' x xs) (NonEmpty' y ys) = NonEmpty' x (append xs (append [y] ys))

instance showNonEmpty' :: Show a => Show (NonEmpty' a) where
  show (NonEmpty' x xs) = show (append [x] xs)

instance functorNonEmpty' :: Functor NonEmpty' where
  map f (NonEmpty' x xs) = NonEmpty' (f x) (map f xs)

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq (Finite x) (Finite y) = x == y
  eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite _) Infinite = LT
  compare Infinite (Finite _) = GT
  compare Infinite Infinite = EQ
  compare (Finite x) (Finite y) = compare x y

instance foldableNonEmpty' :: Foldable NonEmpty' where
  foldr f init (NonEmpty' x xs) = foldr f init ([x] <> xs)
  foldl f init (NonEmpty' x xs) = foldl f init ([x] <> xs)
  foldMap f (NonEmpty' x xs) = foldMap f ([x] <> xs)

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f init (OneMore x xs) = f x (foldr f init xs)
  foldl f init (OneMore x xs) = foldl f (f init x) xs
  foldMap f (OneMore x xs) = append (f x) (foldMap f xs)

class Stream stream element | stream -> element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }

instance streamArray :: Stream (Array a) a where
  uncons = Array.uncons

instance streamString :: Stream String Char where
  uncons = String.uncons

foldStream :: forall l e m. Stream l e => Monoid m => (e -> m) -> l -> m
foldStream f list = 
    case uncons list of
      Nothing -> mempty
      Just cons -> f cons.head <> foldStream f cons.tail

genericTail :: forall stream element. Stream stream element => stream -> Maybe stream
genericTail xs = map _.tail (uncons xs)

maximum' :: Partial => Array Int -> Int
maximum' = fromJust <<< maximum

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance repeatAction :: Action Multiply String where
  act (Multiply 1) str = str
  act (Multiply m) str = str <> act (Multiply (m - 1)) str


repeatAb :: String
repeatAb = act (Multiply 5) "ab"

instance actionArray :: Action m a => Action m (Array a) where
  act m [] = []
  act m xs = cons (act m $ unsafePartial $ head xs) (act m $ unsafePartial $ tail xs)

repeatArray :: Array String
repeatArray = act (Multiply 5) ["ab", "cd"]

newtype Self m = Self m

-- TODO: Why do I need to add Monoid m to the constraint when it's already added to the class
instance actionSelf :: Monoid m => Action m (Self m) where
  act x (Self y) = Self (x <> y)

instance showSelf :: Show a => Show (Self a) where
  show (Self a) = "(Self " <> show a <> ")"

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
  hash :: a -> HashCode

instance eqHashCode :: Eq HashCode where
  eq (HashCode h1) (HashCode h2) = h1 == h2

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hash 0
  hash true = hash 1

instance showHashCode :: Show HashCode where
  show (HashCode h) = "(HashCode " <> show h <> ")"

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hash 0) <<< map hash

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

hasDuplicates :: forall a. Hashable a => Array a -> Boolean
hasDuplicates xs = xs /= nubBy (\x y -> hash x == hash y) xs 

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour x) = hash (x `mod` 12)

