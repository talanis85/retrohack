module Main where

import Test.Tasty

import qualified Memory

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Memory.tests
  ]
