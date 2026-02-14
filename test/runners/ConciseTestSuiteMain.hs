module Main where

import Test.Tasty
import Test.Unit.ConciseTestSuite (conciseTestSuite)

main :: IO ()
main = defaultMain conciseTestSuite