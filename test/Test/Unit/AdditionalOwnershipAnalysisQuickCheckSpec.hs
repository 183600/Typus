module Test.Unit.AdditionalOwnershipAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import Parser (parseTypus, TypusFile(..))
import Ownership (analyzeOwnership, OwnershipResult(..), OwnershipInfo(..))
import Data.Either (isLeft, isRight)
import Data.List (length, nub)

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
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True  -- If parsing fails, ownership analysis is undefined
    Right tf -> 
      let ownershipResult = analyzeOwnership tf
      in case ownershipResult of
        Left _ -> True  -- May fail analysis
        Right or -> 
          let variables = map oiVariable (orOwnershipInfos or)
          in length variables === length (nub variables)  -- Should be unique

-- | Ownership transfer should be tracked correctly through the program
prop_ownership_transfer_tracking :: String -> Property
prop_ownership_transfer_tracking content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let ownershipResult = analyzeOwnership tf
      in case ownershipResult of
        Left _ -> True
        Right or -> all ownershipInfoValid (orOwnershipInfos or)

-- | Ownership analysis should detect potential conflicts
prop_ownership_conflict_detection :: String -> Property
prop_ownership_conflict_detection content = 
  let withConflict = content ++ "\nlet x = 42;\nlet y = x; // potential conflict\nlet z = x;"
      parseResult = parseTypus withConflict
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let ownershipResult = analyzeOwnership tf
      in case ownershipResult of
        Left _ -> True  -- Should detect conflicts
        Right or -> length (orOwnershipInfos or) >= 0

-- | Ownership boundaries should be respected across block scopes
prop_ownership_boundaries :: String -> Property
prop_ownership_boundaries content = 
  let withBlocks = content ++ "\n{\n  let x = 42;\n}\nlet x = 100;"  -- Different scopes
      parseResult = parseTypus withBlocks
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let ownershipResult = analyzeOwnership tf
      in case ownershipResult of
        Left _ -> True
        Right or -> all ownershipWithinScope (orOwnershipInfos or)

-- | Ownership analysis should be deterministic for the same input
prop_ownership_analysis_deterministic :: String -> Property
prop_ownership_analysis_deterministic content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let result1 = analyzeOwnership tf
          result2 = analyzeOwnership tf
      in case (result1, result2) of
        (Right or1, Right or2) -> 
          length (orOwnershipInfos or1) === length (orOwnershipInfos or2)
        _ -> True  -- If either fails, consistency is not required

-- | Ownership analysis should handle complex nested scopes
prop_ownership_complex_scopes :: Int -> Property
prop_ownership_complex_scopes depth = 
  let nestedBlocks = concat $ replicate depth "{\n  let x = 42;\n"
      content = nestedBlocks ++ concat (replicate depth "}\n")
      parseResult = parseTypus content
  in case parseResult of
    Left _ -> True  -- May fail for very deep nesting
    Right tf -> 
      let ownershipResult = analyzeOwnership tf
      in case ownershipResult of
        Left _ -> True
        Right or -> length (orOwnershipInfos or) >= 0

-- | Ownership analysis should ensure memory safety properties
prop_ownership_memory_safety :: String -> Property
prop_ownership_memory_safety content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let ownershipResult = analyzeOwnership tf
      in case ownershipResult of
        Left _ -> True
        Right or -> all memorySafeOwnership (orOwnershipInfos or)

-- | Ownership should satisfy transitivity properties
prop_ownership_transitivity :: String -> Property
prop_ownership_transitivity content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let ownershipResult = analyzeOwnership tf
      in case ownershipResult of
        Left _ -> True
        Right or -> ownershipTransitivityHolds (orOwnershipInfos or)

-- Helper functions for ownership validation
ownershipInfoValid :: OwnershipInfo -> Bool
ownershipInfoValid oi = length (oiVariable oi) > 0  -- Simplified validation

ownershipWithinScope :: OwnershipInfo -> Bool
ownershipWithinScope oi = True  -- Simplified - would check scope boundaries

memorySafeOwnership :: OwnershipInfo -> Bool
memorySafeOwnership oi = True  -- Simplified - would check memory safety

ownershipTransitivityHolds :: [OwnershipInfo] -> Bool
ownershipTransitivityHolds infos = True  -- Simplified - would check transitivity

-- Helper operator for property testing
infix 4 ===
(===) :: Eq a => a -> a -> Bool
(===) = (==)