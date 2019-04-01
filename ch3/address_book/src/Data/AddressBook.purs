module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe(..))

-- Types
type Entry =
    { firstName :: String
    , lastName :: String
    , address :: Address
    }

type Address =
    { street :: String
    , city :: String
    , state :: String
    }

type AddressBook = List Entry

-- Functions
showEntry :: Entry -> String
showEntry entry =
    entry.lastName <> ", " <>
    entry.firstName <> ": " <>
    showAddress entry.address

showAddress :: Address -> String
showAddress addr =
    addr.street <> ", " <>
    addr.city <> ", " <>
    addr.state

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

find :: (Entry -> Boolean) -> AddressBook -> Maybe Entry
find predFunc = filter predFunc >>> head

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = find filterEntry
  where
    filterEntry :: Entry  -> Boolean
    filterEntry entry =
        entry.firstName == firstName &&
        entry.lastName == lastName

findEntryByAddress :: String -> AddressBook -> Maybe Entry
findEntryByAddress addr = find filterAddress
  where
    filterAddress :: Entry -> Boolean
    filterAddress entry = showAddress entry.address == addr

-- TODO: simplify it to something like `exists = findEntry >>> (Nothing /= _)
exists :: String -> String -> AddressBook -> Boolean
exists firstName lastName addrBook = Nothing /= (findEntry firstName lastName addrBook)
-- exists = (((Nothing /= _) <<< ) <<< ) <<< findEntry

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy areNamesEqual
  where
    areNamesEqual :: Entry -> Entry -> Boolean
    areNamesEqual e1 e2 =
        e1.firstName == e2.firstName &&
        e1.lastName == e2.lastName


-- Test Data
myEmptyBook :: AddressBook
myEmptyBook = empty

myEntry :: Entry
myEntry = {firstName: "John", lastName: "Smith", address: myAddress}

myAddress :: Address
myAddress = {street: "xyz", city: "abc", state: "ADB"}

myAddressBook :: AddressBook
myAddressBook = insertEntry myEntry myEmptyBook

myAddressBookDuplicates :: AddressBook
myAddressBookDuplicates = insertEntry myEntry myAddressBook
