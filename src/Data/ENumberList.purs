module Data.ENumberList where

import Prelude
import Data.List ( List(..), filter, head)
import Data.Maybe ( Maybe )
import Control.Plus ( empty )



-- TODO: these are some of the groups, see which we want to add here
data AdditiveGroup =  Colour | Preservative | Antioxidant | FlavourEnchancer | Sweetener | Emulsifier


type ENumber = {
  group :: String
  ,name :: String
  ,code :: Int
  ,description :: String
}

type ENumberList = List ENumber

emptyENumberList:: ENumberList
emptyENumberList = empty

insertEntry :: ENumber -> ENumberList -> ENumberList
-- insertEntry entry list  = Cons entry list
insertEntry = Cons


-- TODO: change this to search by part or one of the fields
findEntry :: Int -> String -> ENumberList -> Maybe ENumber
findEntry code name list = head ( filter filterEntry list)
  where filterEntry::ENumber -> Boolean
        filterEntry entry = entry.code == code && entry.name == name



showENumber :: ENumber -> String
showENumber e = e.name <> " (" <> e.group <> "): " <> show e.code <> " - " <> e.description


