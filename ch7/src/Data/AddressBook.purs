module Data.AddressBook where

import Prelude

import Control.Apply (lift2, lift3)
import Data.Either (Either(..))
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))

newtype Address = Address 
  { street :: String
  , city :: String
  , state :: String
  }

address :: String -> String -> String -> Address
address street city state = Address { street, city, state }

instance showAddress :: Show Address where
  show (Address {street, city, state}) = street <> ", " <> city <> ", " <> state

maybeAddress :: Maybe Address
maybeAddress = lift3 address (Just "a") (Just "b") (Just "c")

maybeAddress' :: Maybe Address
maybeAddress' = address <$> Just "a" <*> Just "b" <*> Just "c"

maybeAddress2 :: Maybe Address
maybeAddress2 = lift3 address (Just "a") Nothing (Just "c")

fullName :: String -> String -> String -> String
fullName first middle last = first <> " " <> middle <> " " <> last

withError :: forall a b. Maybe a -> b -> Either b a
withError Nothing err = Left err
withError (Just x) _ = Right x

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last = 
  fullName <$> (first `withError` "First name was missing.")
           <*> (middle `withError` "Middle name was missing.")
           <*> (last `withError` "Last name was missing.")

combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs

combinedList :: Either Int (List Int)
combinedList = combineList $ fromFoldable [Right 1, Right 2, Right 4, Right 3]

maybeAdd :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
maybeAdd = lift2 (+)

maybeSub :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
maybeSub = lift2 (-)

maybeMul :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
maybeMul = lift2 (*)

maybeDiv :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
maybeDiv = lift2 (/)