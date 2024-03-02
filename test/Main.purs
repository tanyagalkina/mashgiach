module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Data.ENumberList (showENumber)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testEnumber ∷ { code ∷ Int , description ∷ String , group ∷ String , name ∷ String }
testEnumber  = {group : "Emulgator", name: "Lecitine", code: 322, description: "überwiegend Sojabohnen"}

main :: Effect Unit
main = do
  runTest do
    suite "showEnumber" do
      test "showEnumber" do
        Assert.equal (showENumber testEnumber) "Lecitine (Emulgator): 322 - überwiegend Sojabohnen"    

  log "🍝"
  log "You should add some tests."
