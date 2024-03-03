module Test.Main where

import Prelude

import Data.Examples(learnFoldl)
import Data.ENumberList (curcumin, emptyENumberList, findEntryByENumberOrSubstance, insertEntry, removeDiplicates, riboflavin, seedENumberList, seedENumberListWithDuplicates, showENumber)
import Data.List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)


exampleList :: List Int
exampleList = 1 : 2 : 3 : Nil

testEnumber ∷ { e_number ∷ String , description ∷ String , group ∷ String , substance ∷ String, kosher ∷ Int, passover ∷ Int}
testEnumber  = {group : "Emulgator", substance: "Lecitine", e_number: "E322", description: "überwiegend Sojabohnen", kosher: 3, passover: 4}

infixr 5 insertEntry as ++

main :: Effect Unit
main = do
  runTest do
    suite "showEnumber" do
       test "showEnumber" do
          Assert.equal (showENumber testEnumber) "Lecitine (Emulgator): E322 - überwiegend Sojabohnen"    
    suite "insertEntry" do
        test "insertEntry" do
          Assert.equal  (head (insertEntry testEnumber emptyENumberList)) (Just testEnumber )
    suite "insertEntryInifx" do
        test "insertEntryInifx" do
          Assert.equal (head  $ testEnumber ++ emptyENumberList) (Just testEnumber )
    suite "insertEntryApply" do
       test "insertEntryApply" do
          Assert.equal (length $ insertEntry testEnumber $ insertEntry testEnumber emptyENumberList) 2 
    suite "seed List" do
      test "verify that list has 5 elems" do
        Assert.equal 5 (length  $ seedENumberList)
      test "verify that list with duplications has 10 elems" do
        Assert.equal 10 (length  $ seedENumberListWithDuplicates)
    suite "findEntry" do
      test "findEntry" do
        Assert.equal (Just curcumin) (findEntryByENumberOrSubstance "cumin" seedENumberListWithDuplicates)
        Assert.equal (Just riboflavin) (findEntryByENumberOrSubstance "E101" seedENumberListWithDuplicates) 
    suite "remove duplicates" do
      test "remove duplicates" do
        Assert.equal 5 (length $ removeDiplicates $ seedENumberListWithDuplicates)
    suite "learnFoldl" do
      test "learnFoldl" do
        Assert.equal 6 (learnFoldl)
      test "Why is it zero ??" do
         Assert.equal  ( 3 / (2 / (1 / 1))) (foldl (/) 1 exampleList)  
      test "learnFoldR" do
        Assert.equal (1 / (2 /(3 / 1))) (foldr (/) 1 exampleList)      


  log "🍝"
  log "You should add some tests."
