{-# LANGUAGE CPP #-}

module Test.Unit.NewQuickCheckTestCasesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (isPrefixOf)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, mergeSpans, emptySpan)
import Parser (FileDirectives(..), BlockDirectives(..))
import Ownership (OwnershipType(..))
import Analyzer.Types (SymbolInfo(..), SymbolKind(..))
import TestSupport.Arbitrary ()

-- Arbitrary instance for OwnershipType is defined in TestSupport.Arbitrary

-- Property 1: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in property (trim trimmed == trimmed)

-- Property 2: splitByCollapsed never returns empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

-- Property 3: splitBy length relationship
prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length delim s =
  let parts = splitBy delim s
      expectedLength = length (filter (== delim) s) + 1
  in property (length parts == expectedLength)

-- Property 4: SourcePos advancement maintains consistency
prop_sourcepos_advancement_consistent :: String -> Property
prop_sourcepos_advancement_consistent s =
  let initial = startPos
      after = advancePosByText initial s
      lineDiff = posLine after - posLine initial
      colDiff = posColumn after - posColumn initial
  in lineDiff >= 0 .&&. colDiff >= 0
  where
    advancePosByText pos text = foldl advancePos pos text
    advancePos pos '\n' = pos { posLine = posLine pos + 1, posColumn = 1 }
    advancePos pos '\t' = pos { posColumn = posColumn pos + 8 - ((posColumn pos - 1) `mod` 8) }
    advancePos pos _ = pos { posColumn = posColumn pos + 1 }

-- Property 5: mergeSpans is commutative for overlapping spans
prop_merge_spans_commutative :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

-- Property 6: Map union preserves all keys from both maps
prop_map_union_preserves_keys :: Map.Map String Int -> Map.Map String Int -> Property
prop_map_union_preserves_keys m1 m2 =
  let union = Map.union m1 m2
      allKeys = Map.keysSet m1 `Set.union` Map.keysSet m2
  in Map.keysSet union === allKeys

-- Property 7: removeLineComments idempotent on strings without comments
prop_removeLine_comments_idempotent :: String -> Property
prop_removeLine_comments_idempotent s =
  let result = removeLineComments s
      result2 = removeLineComments result
  in property (result == result2)

-- Property 8: normalizeIndentation is idempotent
prop_normalize_indentation_idempotent :: String -> Property
prop_normalize_indentation_idempotent s =
  let normalized = normalizeIndentation s
      normalized2 = normalizeIndentation normalized
  in property (normalized == normalized2)

-- Property 9: OwnershipType equality is reflexive
prop_ownership_type_reflexive :: OwnershipType -> Property
prop_ownership_type_reflexive t = t === t

-- Property 10: Set union contains all elements from both sets
prop_set_union_contains_all :: Set.Set Int -> Set.Set Int -> Property
prop_set_union_contains_all s1 s2 =
  let union = Set.union s1 s2
  in property $ all (`Set.member` union) (Set.toList s1) .&&.
             all (`Set.member` union) (Set.toList s2)

tests :: TestTree
tests = testGroup "New QuickCheck Test Cases"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitByCollapsed never returns empty strings" prop_splitByCollapsed_no_empty
  , fastProperty "splitBy length relationship" prop_splitBy_length
  , fastProperty "SourcePos advancement maintains consistency" prop_sourcepos_advancement_consistent
  , fastProperty "mergeSpans is commutative" prop_merge_spans_commutative
  , fastProperty "Map union preserves all keys" prop_map_union_preserves_keys
  , fastProperty "removeLineComments is idempotent" prop_removeLine_comments_idempotent
  , fastProperty "normalizeIndentation is idempotent" prop_normalize_indentation_idempotent
  , fastProperty "OwnershipType equality is reflexive" prop_ownership_type_reflexive
  , fastProperty "Set union contains all elements from both sets" prop_set_union_contains_all
  ]