{-# LANGUAGE CPP #-}

module Test.Unit.SimpleQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub)
import Data.Char (isSpace)

import Utils (trim, splitBy, splitByCollapsed)
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Simple QuickCheck Properties"
  [ stringProperties
  , listProperties
  , mapProperties
  , setProperties
  , sourceLocationProperties
  ]

stringProperties :: TestTree
stringProperties = testGroup "String Properties"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim of empty string is empty" prop_trim_empty
  , fastProperty "splitBy preserves total length" prop_splitBy_preserves_length
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
  ]

listProperties :: TestTree
listProperties = testGroup "List Properties"
  [ fastProperty "sort is idempotent" prop_sort_idempotent
  , fastProperty "sort preserves length" prop_sort_preserves_length
  , fastProperty "nub preserves length or reduces it" prop_nub_preserves_or_reduces_length
  ]

mapProperties :: TestTree
mapProperties = testGroup "Map Properties"
  [ fastProperty "Map lookup after insert returns the value" prop_map_insert_lookup
  , fastProperty "Map size after insert increases by 1 for new key" prop_map_insert_size
  ]

setProperties :: TestTree
setProperties = testGroup "Set Properties"
  [ fastProperty "Set insert preserves element" prop_set_insert_preserves
  , fastProperty "Set size after insert increases by 1 for new element" prop_set_insert_size
  ]

sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "SourceLocation Properties"
  [ fastProperty "SourcePos equality is reflexive" prop_sourcepos_reflexive
  , fastProperty "SourcePos offset increases with line" prop_sourcepos_offset_monotonic
  ]

-- String Properties
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

prop_trim_empty :: Property
prop_trim_empty =
  trim "" === ""

prop_splitBy_preserves_length :: Char -> String -> Property
prop_splitBy_preserves_length delim s =
  let parts = splitBy delim s
      rejoined = concat $ intersperse [delim] parts
  in rejoined === s
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

-- List Properties
prop_sort_idempotent :: [Int] -> Property
prop_sort_idempotent xs =
  sort (sort xs) === sort xs

prop_sort_preserves_length :: [Int] -> Property
prop_sort_preserves_length xs =
  length (sort xs) === length xs

prop_nub_preserves_or_reduces_length :: [Int] -> Property
prop_nub_preserves_or_reduces_length xs =
  let nubbed = nub xs
  in property $ length nubbed <= length xs

-- Map Properties
prop_map_insert_lookup :: String -> Int -> Map.Map String Int -> Property
prop_map_insert_lookup k v m =
  let m' = Map.insert k v m
  in Map.lookup k m' === Just v

prop_map_insert_size :: String -> Int -> Map.Map String Int -> Property
prop_map_insert_size k v m =
  let m' = Map.insert k v m
      newSize = if Map.member k m then Map.size m else Map.size m + 1
  in Map.size m' === newSize

-- Set Properties
prop_set_insert_preserves :: Int -> Set.Set Int -> Property
prop_set_insert_preserves x s =
  let s' = Set.insert x s
  in property $ Set.member x s'

prop_set_insert_size :: Int -> Set.Set Int -> Property
prop_set_insert_size x s =
  let s' = Set.insert x s
      newSize = if Set.member x s then Set.size s else Set.size s + 1
  in Set.size s' === newSize

-- SourceLocation Properties
prop_sourcepos_reflexive :: SourcePos -> Property
prop_sourcepos_reflexive pos =
  pos === pos

prop_sourcepos_offset_monotonic :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_offset_monotonic (Positive l) (Positive c) (Positive o) =
  let pos1 = SourcePos l c o
      pos2 = SourcePos (l + 1) c (o + 10)
  in property $ posOffset pos1 < posOffset pos2