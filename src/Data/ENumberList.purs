module Data.ENumberList where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubByEq)
import Data.Maybe (Maybe)
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Record (equal)


infixr 5 insertEntry as ++

-- TODO: these are some of the groups, see which we want to add here
--[??]  how to add enums into Records  ??
data AdditiveGroup =  Colour | Preservative | Antioxidant | FlavourEnchancer | Sweetener | Emulsifier

--[??]  how to add enums into Records  ??
data Kashrut = UsuallyKosher | NeverKosherWithoutEksher | OftenKosher | NeedCheck | ChalavAcum

type ENumber = {
  group :: String
  ,substance :: String
  ,e_number :: String
  ,description :: String
  -- replace here with data
  ,kosher:: Int
  ,passover ::Int
  -- ,kosher :: Kashrut
  -- ,passover :: Kashrut

}

type ENumberList = List ENumber

emptyENumberList:: ENumberList
emptyENumberList = empty


seedENumberList:: ENumberList
seedENumberList = curcumin ++ riboflavin ++ tartrazine ++ quinoline_yellow ++ sunset_yellow_FCF ++ emptyENumberList

seedENumberListWithDuplicates:: ENumberList
seedENumberListWithDuplicates = curcumin ++ riboflavin ++ tartrazine ++ quinoline_yellow ++ sunset_yellow_FCF ++ curcumin ++ riboflavin ++ tartrazine ++ quinoline_yellow ++ sunset_yellow_FCF ++ emptyENumberList


equivalent:: ENumber -> ENumber -> Boolean
equivalent a b = a.e_number == b.e_number

removeDiplicates:: ENumberList -> ENumberList
removeDiplicates list | null list  = empty
                      | otherwise = nubByEq equivalent list

insertEntry :: ENumber -> ENumberList -> ENumberList
-- insertEntry entry list  = Cons entry list
insertEntry = Cons


-- TODO: change this to search by part or one of the fields
findEntry :: String -> String -> ENumberList -> Maybe ENumber
findEntry e_number substance = head <<< filter filterEntry
  where filterEntry::ENumber -> Boolean
        filterEntry entry = entry.e_number == e_number && entry.substance == substance

findEntryBySubstance :: String -> ENumberList -> Maybe ENumber
findEntryBySubstance substance = head <<< filter filterEntry
  where filterEntry::ENumber -> Boolean
        filterEntry entry = contains (Pattern substance)  (_.substance entry)

findEntryByENumber :: String -> ENumberList -> Maybe ENumber
findEntryByENumber e_number = head <<< filter filterEntry
  where filterEntry::ENumber -> Boolean
        filterEntry entry = contains (Pattern e_number) entry.e_number


findEntryByENumberOrSubstance :: String -> ENumberList -> Maybe ENumber
findEntryByENumberOrSubstance query = head <<< filter filterEntry
  where filterEntry::ENumber -> Boolean
        filterEntry entry = contains (Pattern query) entry.e_number || contains (Pattern query) entry.substance

showENumber :: ENumber -> String
showENumber e = e.substance <> " (" <> e.group <> "): " <> e.e_number <> " - " <> e.description


curcumin ∷ { description ∷ String , e_number ∷ String , group ∷ String , kosher ∷ Int , passover ∷ Int , substance ∷ String }
curcumin = {
  group: "Colour"
  ,substance: "Curcumin"
  ,e_number: "E100"
  ,description: "Yellow colouring"
  -- ,kosher: UsuallyKosher
  -- ,passover: UsuallyKosher
  ,kosher: 1
  ,passover: 1  
}



-- seed data ---
riboflavin ∷ { description ∷ String , e_number ∷ String , group ∷ String , kosher ∷ Int , passover ∷ Int , substance ∷ String }
riboflavin = {
  group: "Colour"
  ,substance: "Riboflavin"
  ,e_number: "E101"
  ,description: "Yellow colouring"
  -- ,kosher: UsuallyKosher
  -- ,passover: UsuallyKosher
  ,kosher: 2
  ,passover: 4  
}

tartrazine ∷ { description ∷ String , e_number ∷ String , group ∷ String , kosher ∷ Int , passover ∷ Int , substance ∷ String }
tartrazine = {
  group: "Colour"
  ,substance: "Tertrazine"
  ,e_number: "E102"
  ,description: "Yellow colouring"
  -- ,kosher: UsuallyKosher
  -- ,passover: UsuallyKosher
  ,kosher: 1
  ,passover: 1  
}

quinoline_yellow ∷ { description ∷ String , e_number ∷ String , group ∷ String , kosher ∷ Int , passover ∷ Int , substance ∷ String }
quinoline_yellow = {
  group: "Colour"
  ,substance: "Quinoline Yellow"
  ,e_number: "E104"
  ,description: "Yellow colouring"
  -- ,kosher: UsuallyKosher
  -- ,passover: UsuallyKosher
  ,kosher: 1
  ,passover: 1  
}

sunset_yellow_FCF ∷ { description ∷ String , e_number ∷ String , group ∷ String , kosher ∷ Int , passover ∷ Int , substance ∷ String }
sunset_yellow_FCF = {
  group: "Colour"
  ,substance: "Sunset Yellow FCF, Orange Yellow S"
  ,e_number: "E110"
  ,description: "Yellow colouring"
  -- ,kosher: UsuallyKosher
  -- ,passover: UsuallyKosher
  ,kosher: 1
  ,passover: 1  
}


