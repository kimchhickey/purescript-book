module Test.MySolutions where

import Prelude
import Data.List (filter, head, null, nubByEq)
import Data.AddressBook (AddressBook, Entry)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = _.address.street entry == street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = not $ null $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubByEq isEq book
  where
    isEq :: Entry -> Entry -> Boolean
    isEq e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
