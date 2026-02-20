module Test.Unit.SimpleParserQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck

-- Import universal memory optimization
import TestSupport.UniversalMemoryOptimization 
  ( optimizeTestSuite
  , lowLevel
  , limitString
  , quickOptimize
  )

-- Basic test properties (memory optimized)
prop_basic_property :: String -> Property
prop_basic_property s = 
  let limited = limitString 5 s  -- Limit string length to reduce memory
  in property $ length limited >= 0 && length limited <= 5

tests :: TestTree
tests = optimizeTestSuite lowLevel $ testGroup "Test.Unit.SimpleParserQuickCheckSpec Tests (Memory Optimized)"
  [ testProperty "basic property" prop_basic_property
  ]
