module Test.Main where

import Prelude

import Data.ENumberList (emptyENumberList, insertEntry, showENumber)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Data.List (head)
import Data.Maybe ( Maybe(..) )


testEnumber ∷ { code ∷ Int , description ∷ String , group ∷ String , name ∷ String }
testEnumber  = {group : "Emulgator", name: "Lecitine", code: 322, description: "überwiegend Sojabohnen"}

main :: Effect Unit
main = do
  runTest do
    suite "showEnumber" do
      test "showEnumber" do
        Assert.equal (showENumber testEnumber) "Lecitine (Emulgator): 322 - überwiegend Sojabohnen"    
    suite "insertEntry" do
      test "insertEntry" do
        Assert.equal  (head (insertEntry testEnumber emptyENumberList)) (Just testEnumber )
  log "🍝"
  log "You should add some tests."
