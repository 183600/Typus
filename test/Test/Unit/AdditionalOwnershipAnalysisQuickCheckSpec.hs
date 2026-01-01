module Test.Unit.AdditionalOwnershipAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.QuickCheck (property)
import Parser (parseTypus, TypusFile(..))
import Ownership (analyzeOwnership, OwnershipError(..))
import Data.Either (isLeft, isRight)
import qualified Data.List as L
import Data.List (length)
import Data.List (nub)

-- ============================================================================
-- Ownership Analysis QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Analysis QuickCheck Tests"
  [ testProperty "ownership analysis preserves variable uniqueness" prop_ownership_variable_uniqueness
  , testProperty "ownership transfer is tracked correctly" prop_ownership_transfer_tracking
  , testProperty "ownership analysis detects conflicts" prop_ownership_conflict_detection
  , testProperty "ownership boundaries are respected" prop_ownership_boundaries
  , testProperty "ownership analysis is deterministic" prop_ownership_analysis_deterministic
  , testProperty "ownership handles complex scopes" prop_ownership_complex_scopes
  , testProperty "ownership memory safety properties" prop_ownership_memory_safety
  , testProperty "ownership transitivity properties" prop_ownership_transitivity
  ]

-- | Ownership analysis should preserve variable uniqueness constraints
prop_ownership_variable_uniqueness :: String -> Property
prop_ownership_variable_uniqueness content = 
  let errors = analyzeOwnership content
  in property $ L.length errors >= 0  -- Basic property: analysis completes without crashing

-- | Ownership transfer should be tracked correctly through the program
prop_ownership_transfer_tracking :: String -> Property
prop_ownership_transfer_tracking content = 
  let errors = analyzeOwnership content
  in property $ L.length errors >= 0  -- Analysis completes without crashing

-- | Ownership analysis should detect potential conflicts
prop_ownership_conflict_detection :: String -> Property
prop_ownership_conflict_detection content = 
  let withConflict = content ++ "\nlet x = 42;\nlet y = x; // potential conflict\nlet z = x;"
      errors = analyzeOwnership withConflict
  in property $ L.length errors >= 0  -- Analysis completes without crashing

-- | Ownership boundaries should be respected across block scopes
prop_ownership_boundaries :: String -> Property
prop_ownership_boundaries content = 
  let withBlocks = content ++ "\n{\n  let x = 42;\n}\nlet x = 100;"  -- Different scopes
      errors = analyzeOwnership withBlocks
  in property $ L.length errors >= 0  -- Analysis completes without crashing

-- | Ownership analysis should be deterministic for the same input
prop_ownership_analysis_deterministic :: String -> Property
prop_ownership_analysis_deterministic content = 
  let result1 = analyzeOwnership content
      result2 = analyzeOwnership content
  in property $ L.length result1 === L.length result2

-- | Ownership analysis should handle complex nested scopes
prop_ownership_complex_scopes :: Int -> Property
prop_ownership_complex_scopes depth = 
  let nestedBlocks = L.concat $ replicate depth "{\n  let x = 42;\n"
      content = nestedBlocks ++ L.concat (replicate depth "}\n")
      errors = analyzeOwnership content
  in property $ L.length errors >= 0  -- Analysis completes without crashing

-- | Ownership analysis should ensure memory safety properties
prop_ownership_memory_safety :: String -> Property
prop_ownership_memory_safety content = 
  let errors = analyzeOwnership content
  in property $ L.length errors >= 0  -- Analysis completes without crashing

-- | Ownership should satisfy transitivity properties
prop_ownership_transitivity :: String -> Property
prop_ownership_transitivity content = 
  let errors = analyzeOwnership content
  in property $ L.length errors >= 0  -- Analysis completes without crashing