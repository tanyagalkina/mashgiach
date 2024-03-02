module Data.ENumberList where

import Prelude
import Data.List ( List )
-- TODO: these are some of the groups, see which we want to add here
data AdditiveGroup =  Colour | Preservative | Antioxidant | FlavourEnchancer | Sweetener | Emulsifier


type ENumber = {
  group :: String
  ,name :: String
  ,code :: Int
  ,description :: String
}

type ENumberList = List ENumber

showENumber :: ENumber -> String
showENumber e = e.name <> " (" <> e.group <> "): " <> show e.code <> " - " <> e.description


