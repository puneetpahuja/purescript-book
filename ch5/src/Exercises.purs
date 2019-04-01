module Exercises where

import Prelude

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

pascal :: Int -> Int -> Int
pascal n k | n >= k && k >= 0 = fact n / (fact k * fact (n - k))
pascal _ _ = -1

showPerson :: forall r. {first :: String | r} -> String
showPerson {first: x} = x

type Address = {street :: String, city :: String}
type Person = {name :: String, address :: Address}

sameCity :: Person -> Person -> Boolean
sameCity {address: {city: x}} {address: {city: y}} = x == y

-- most generic type of sameCity
-- sameCity :: forall r s t u. {address :: {city :: String | r} | s} {address :: {city :: String | t} | u} -> Boolean

-- most generic type of livesInLA
-- livesInLA :: forall r s. {address :: {city :: String | r} | s} -> Boolean

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton e _ = e
