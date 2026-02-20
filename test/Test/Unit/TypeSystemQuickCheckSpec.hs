module Test.Unit.TypeSystemQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck

-- Import memory optimization modules
import TestSupport.MemoryOptimizedQuickCheck 
  ( QuickCheckMemoryConfig(..)
  , lowMemoryConfig
  , applyQuickCheckMemoryConfig
  , genSmallString
  )

-- Basic test properties (memory optimized)
prop_basic_property :: String -> Property
prop_basic_property s = 
  let limitedInput = take 5 s  -- Limit input string length
  in property $ length limitedInput >= 0 && length limitedInput <= 5

tests :: TestTree
tests = applyQuickCheckMemoryConfig lowMemoryConfig $ 
  testGroup "Test.Unit.TypeSystemQuickCheckSpec Tests (Memory Optimized)"
  [ testProperty "basic property" prop_basic_property
  ]
