module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

authour = {name : "Nagendra", interests : ["Reading"]}

type Entry = 
  {  firstName :: String
    , lastName  :: String
    , address   :: Address
  }

type Address =
   {  street :: String
    , city   :: String
    , state  :: String
    , country:: String
    , zipcode:: String 
   }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.firstName <> " " <>
                  entry.lastName <> ":" <>
                  showAddress entry.address

showAddress :: Address -> String 
showAddress address = address.street <> ", " <>
                      address.city <> ", " <>
                      address.state <> ", " <>
                      address.country <> ", " <>
                      address.zipcode

address = { street:"#45 8th cross, 3d main Jaimaruthi nagar, Nanadini Layout", city:"Banglore", state:"KARNATAKA", country:"INDIA", zipcode:"560096"}
nagendra = { firstName:"Nagendra", lastName:"P", address:address}

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book

addressBook = Cons nagendra emptyBook

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName
