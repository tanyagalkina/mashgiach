module Main where

import Prelude

import Data.List ( List )
import Effect (Effect)
import Effect.Console (log)


-- TODO: these are some of the groups, see which we want to add here
data AdditiveGroup =  Colour | Preservative | Antioxidant | FlavourEnchancer | Sweetener | Emulsifier


type ENumber = {
  group :: String
  ,name :: String
  ,code :: Int
  ,description :: String
}

type ENumberList = List ENumber


funcToTest :: Int -> Int
funcToTest x = x + 1

--  "Lecitine (Emulgator): 322 - überwiegend Sojabohnen" 
showENumber :: ENumber -> String
showENumber e = e.name <> " (" <> e.group <> "): " <> show e.code <> " - " <> e.description

main :: Effect Unit
main = do
  log "Hello, Purescript 🍝 😂"
