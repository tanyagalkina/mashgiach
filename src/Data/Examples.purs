module Data.Examples where

import Prelude

import Data.List


exampleList :: List Int
exampleList = 1 : 2 : 3 : Nil

learnFoldl:: Int
learnFoldl = foldl (+) 0 exampleList



