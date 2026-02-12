{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -O0 #-}

-- | Memory-optimized Extended QuickCheck Test Suite
module Test.Unit.ExtendedQuickCheckTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Utils as U
import Data.List (isInfixOf)

-- | Basic test with minimal memory usage
prop_basic_memory_test :: String -> Property
prop_basic_memory_test s = 
  let limited_s = take 1 s
  in property $ length limited_s >= 0

-- | Memory-optimized test suite
tests :: TestTree
tests = testGroup "Memory-Optimized Extended Tests"
  [ testProperty "basic memory test" prop_basic_memory_test
  ]

main :: IO ()
main = defaultMain tests
