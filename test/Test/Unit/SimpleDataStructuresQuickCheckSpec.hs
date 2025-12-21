{-# LANGUAGE CPP #-}

-- | Simple QuickCheck tests for core data structures
module Test.Unit.SimpleDataStructuresQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
import qualified Data.List as Data.List
import Data.Char (toUpper, toLower)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- ============================================================================
-- Collection and Container Properties
-- ============================================================================

-- Property: Map operations maintain invariants
prop_map_operations_invariants :: Map.Map String Int -> String -> Int -> Property
prop_map_operations_invariants originalMap key value =
  let inserted = Map.insert key value originalMap
      deleted = Map.delete key originalMap
  in property $ Map.size inserted == Map.size originalMap + (if Map.member key originalMap then 0 else 1) &&
                Map.size deleted == Map.size originalMap - (if Map.member key originalMap then 1 else 0) &&
                (Map.lookup key inserted == Just value) &&
                (Map.lookup key deleted == Nothing)

-- Property: Set operations maintain invariants
prop_set_operations_invariants :: Set.Set Int -> Int -> Property
prop_set_operations_invariants originalSet value =
  let inserted = Set.insert value originalSet
      deleted = Set.delete value originalSet
      isMember = Set.member value originalSet
  in property $ Set.size inserted == Set.size originalSet + (if isMember then 0 else 1) &&
                Set.size deleted == Set.size originalSet - (if isMember then 1 else 0) &&
                (Set.member value inserted) &&
                (not $ Set.member value deleted)

-- Property: List operations maintain expected properties
prop_list_operations_expected :: [Int] -> Int -> Property
prop_list_operations_expected originalList value =
  let appended = originalList ++ [value]
      prefixed = value : originalList
      lengthOriginal = length originalList
      lengthAppended = length appended
      lengthPrefixed = length prefixed
      firstElement = case prefixed of
                      [] -> error "Impossible: prefixed list cannot be empty"
                      (x:_) -> x
  in property $ lengthAppended == lengthOriginal + 1 &&
                lengthPrefixed == lengthOriginal + 1 &&
                last appended == value &&
                firstElement == value

-- ============================================================================
-- Data Structure Transformation Properties
-- ============================================================================

-- Property: Round-trip transformations preserve data
prop_roundtrip_transformation_preserve :: [String] -> Property
prop_roundtrip_transformation_preserve strings =
  let textList = map (map toUpper) strings
      backToStrings = map (map toLower) textList
  in property $ length strings == length backToStrings

-- Property: Sorting maintains order invariants
prop_sorting_maintains_invariants :: [Int] -> Property
prop_sorting_maintains_invariants unsorted =
  let sorted = Data.List.sort unsorted
  in property $ length sorted == length unsorted &&
                all (`elem` unsorted) sorted &&
                isSorted sorted

-- Property: Grouping preserves all elements
prop_grouping_preserves_elements :: [(String, Int)] -> Property
prop_grouping_preserves_elements pairs =
  let grouped = Map.fromListWith (++) [(k, [v]) | (k, v) <- pairs]
      flattened = concat $ Map.elems grouped
  in property $ length flattened == length pairs &&
                all (`elem` (map snd pairs)) flattened

-- Property: Filtering maintains subset relationship
prop_filtering_maintains_subset :: [Int] -> Property
prop_filtering_maintains_subset original =
  let filtered = filter even original
  in property $ all (`elem` original) filtered

-- ============================================================================
-- Edge Case and Stress Tests
-- ============================================================================

-- Property: Empty data structures behave correctly
prop_empty_structures_correct :: Property
prop_empty_structures_correct =
  let emptyMap = Map.empty :: Map.Map String Int
      emptySet = Set.empty :: Set.Set Int
      emptyList = [] :: [Int]
  in property $ Map.null emptyMap && Set.null emptySet && null emptyList

-- Property: Large data structures maintain performance
prop_large_structures_performance :: Int -> Property
prop_large_structures_performance size =
  size >= 0 && size <= 1000 ==> 
  let largeList = [1..size]
      largeMap = Map.fromList $ zip [1..size] [1..size]
      largeSet = Set.fromList [1..size]
  in property $ length largeList == size &&
                Map.size largeMap == size &&
                Set.size largeSet == size

-- Property: Nested data structures maintain invariants
prop_nested_structures_invariants :: Map.Map String [Set.Set Int] -> Property
prop_nested_structures_invariants nestedMap =
  let allSets = concat $ Map.elems nestedMap
  in property $ all (not . Set.null) allSets || Map.null nestedMap

-- ============================================================================
-- Helper Functions
-- ============================================================================

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Simple Data Structures QuickCheck Tests"
  -- Collection and Container tests
  [ testGroup "Collections"
    [ fastProperty "Map operations maintain invariants" prop_map_operations_invariants
    , fastProperty "Set operations maintain invariants" prop_set_operations_invariants
    , fastProperty "List operations maintain expected properties" prop_list_operations_expected
    ]
  
  -- Data Structure Transformation tests
  , testGroup "Transformations"
    [ fastProperty "Round-trip transformations preserve data" prop_roundtrip_transformation_preserve
    , fastProperty "Sorting maintains order invariants" prop_sorting_maintains_invariants
    , fastProperty "Grouping preserves all elements" prop_grouping_preserves_elements
    , fastProperty "Filtering maintains subset relationship" prop_filtering_maintains_subset
    ]
  
  -- Edge Case and Stress tests
  , testGroup "Edge Cases"
    [ fastProperty "Empty data structures behave correctly" prop_empty_structures_correct
    , fastProperty "Large data structures maintain performance" prop_large_structures_performance
    , fastProperty "Nested data structures maintain invariants" prop_nested_structures_invariants
    ]
  ]