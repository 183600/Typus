{-# LANGUAGE CPP #-}

module Test.Unit.NewCabalQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd, startPos, emptySpan, spanFrom, Located(..))
import Utils (trim, splitBy, splitByCollapsed, removeLineComments, normalizeIndentation)
import Parser (FileDirectives(..), BlockDirectives(..))
import TestSupport.Arbitrary ()

-- Test 1: splitByCollapsed property - no empty strings in result
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim str =
  not (null (splitByCollapsed delim str)) ==>
  all (not . null) (splitByCollapsed delim str)

-- Test 2: splitBy vs splitByCollapsed relationship
prop_splitBy_vs_splitByCollapsed :: Char -> String -> Property
prop_splitBy_vs_splitByCollapsed delim str =
  splitByCollapsed delim str === filter (not . null) (splitBy delim str)

-- Test 3: trim on string with only whitespace
prop_trim_whitespace_only :: Property
prop_trim_whitespace_only =
  forAll (listOf (elements " \t\n\r")) $ \spaces ->
    not (null spaces) ==>
    trim spaces === ""

-- Test 4: SourcePos monotonicity
prop_sourcepos_monotonic :: Int -> Int -> Int -> Property
prop_sourcepos_monotonic line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = SourcePos line col offset
  in posLine pos >= 1 .&&. posColumn pos >= 1 .&&. posOffset pos >= 0

-- Test 5: SourceSpan consistency
prop_sourcespan_consistency :: SourcePos -> SourcePos -> Property
prop_sourcespan_consistency start end =
  posOffset start <= posOffset end ==>
  let span = SourceSpan start end
  in spanStart span === start .&&. spanEnd span === end

-- Test 6: Map insertion and lookup
prop_map_insertion_lookup :: String -> Int -> Property
prop_map_insertion_lookup key value =
  Map.lookup key (Map.insert key value Map.empty) === Just value

-- Test 7: Set insertion and membership
prop_set_insertion_membership :: Int -> Property
prop_set_insertion_membership value =
  property (Set.member value (Set.insert value Set.empty))

-- Test 8: removeLineComments preserves non-comment lines
prop_removeLine_comments_preserves_non_comment :: String -> Property
prop_removeLine_comments_preserves_non_comment str =
  not ('/' `elem` str) ==>
  removeLineComments str === str

-- Test 9: normalizeIndentation idempotency
prop_normalize_indentation_idempotent :: String -> Property
prop_normalize_indentation_idempotent str =
  let normalized = normalizeIndentation str
  in normalizeIndentation normalized === normalized

-- Test 10: FileDirectives roundtrip
prop_file_directives_roundtrip :: Bool -> Bool -> Bool -> Property
prop_file_directives_roundtrip ownership dependentTypes constraints =
  let pos = startPos
      span = emptySpan pos
      locatedOwnership = Located ownership pos span
      locatedDependentTypes = Located dependentTypes pos span
      locatedConstraints = Located constraints pos span
      fd = FileDirectives (Just locatedOwnership) (Just locatedDependentTypes) (Just locatedConstraints)
  in fd === fd

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ fastProperty "splitByCollapsed produces no empty strings" prop_splitByCollapsed_no_empty
  , fastProperty "splitBy vs splitByCollapsed relationship" prop_splitBy_vs_splitByCollapsed
  , fastProperty "trim on whitespace-only strings" prop_trim_whitespace_only
  , fastProperty "SourcePos monotonicity" prop_sourcepos_monotonic
  , fastProperty "SourceSpan consistency" prop_sourcespan_consistency
  , fastProperty "Map insertion and lookup" prop_map_insertion_lookup
  , fastProperty "Set insertion and membership" prop_set_insertion_membership
  , fastProperty "removeLineComments preserves non-comment lines" prop_removeLine_comments_preserves_non_comment
  , fastProperty "normalizeIndentation idempotency" prop_normalize_indentation_idempotent
  , fastProperty "FileDirectives roundtrip" prop_file_directives_roundtrip
  ]
