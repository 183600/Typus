module Main where

import Test.Tasty
import qualified TestSuite.EnhancedSourceLocation as EnhancedSourceLocation
import qualified TestSuite.ParserBoundary as ParserBoundary

main :: IO ()
main = defaultMain $ testGroup "New Enhanced Test Suite"
  [ EnhancedSourceLocation.tests
  , ParserBoundary.tests
  ]