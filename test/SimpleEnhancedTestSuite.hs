module Main where

import Test.Tasty
import Test.Unit.ConciseTestSuite (conciseTestSuite)
import qualified TestSuite.EnhancedSourceLocation as EnhancedSourceLocation
import qualified TestSuite.ParserBoundary as ParserBoundary

main :: IO ()
main = defaultMain $ testGroup "New Enhanced Test Suite"
  [ conciseTestSuite
  , EnhancedSourceLocation.tests
  , ParserBoundary.tests
  ]