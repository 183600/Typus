{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCoreQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen)
import Test.QuickCheck.Gen (choose, listOf, elements)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), startPos, posAfter, posAt, SourceSpan(..), emptySpan, spanFrom, mergeSpans)
import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.List as Data.List
import Data.List (sort, nub)

-- ============================================================================
-- Core Property Tests for Utils Module
-- ============================================================================

-- Property 1: trim is idempotent - applying trim twice gives same result as once
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property 2: splitBy L.and splitByCollapsed relationship
prop_splitBy_collapsed_relationship :: Char -> String -> Property
prop_splitBy_collapsed_relationship delim str =
  let regular = splitBy delim str
      collapsed = splitByCollapsed delim str
      regularLength = L.length regular
      collapsedLength = L.length collapsed
  in property $ collapsedLength <= regularLength .&&. 
     (if L.null (L.filter (== delim) str) then regularLength === collapsedLength else property True)

-- Property 3: removeLineComments preserves non-comment content
prop_removeLineComments_preserves_content :: String -> String -> Property
prop_removeLineComments_preserves_content prefix suffix =
  -- Avoid strings with quotes that might confuse comment removal
  not (L.any (`elem` "\"'" ) prefix) && not (L.any (`elem` "\"'" ) suffix) ==>
  let content = prefix ++ "\nreal code\n" ++ suffix
      withComment = content ++ "// comment\nmore code"
      cleaned = removeLineComments withComment
  in property $ "real code" `L.isInfixOf` cleaned .&&. "more code" `L.isInfixOf` cleaned

-- Property 4: normalizeIndentation preserves line count
prop_normalizeIndentation_preserves_lines :: [String] -> Property
prop_normalizeIndentation_preserves_lines lineList =
  not (null lineList) ==>
  let input = unlines lineList
      normalized = normalizeIndentation input
      inputLines = lines input
      normalizedLines = lines normalized
  in property $ L.length inputLines === L.length normalizedLines

-- ============================================================================
-- Core Property Tests for SourceLocation Module
-- ============================================================================

-- Property 5: posAfter advances line number for newline characters
prop_posAfter_newline_increments_line :: Int -> Int -> Int -> Property
prop_posAfter_newline_increments_line line col offset =
  line >= 1 && col >= 1 && offset >= 0 ==>
  let pos = SourcePos line col offset
      newPos = posAfter '\n' pos
  in property $ posLine newPos === line + 1 .&&. posColumn newPos === 1

-- Property 6: posAfter advances column for non-newline characters
prop_posAfter_char_increments_column :: Int -> Int -> Int -> Char -> Property
prop_posAfter_char_increments_column line col offset ch =
  line >= 1 && col >= 1 && offset >= 0 && ch /= '\n' && ch /= '\t' ==>
  let pos = SourcePos line col offset
      newPos = posAfter ch pos
  in property $ posLine newPos === line .&&. posColumn newPos === col + 1

-- Property 7: mergeSpans contains both original spans
prop_mergeSpans_contains_originals :: Int -> Int -> Int -> Int -> Property
prop_mergeSpans_contains_originals start1 end1 start2 =
  start1 >= 1 && end1 >= start1 && start2 >= 1 ==>
  let span1 = emptySpan { spanStartLine = start1, spanEndLine = end1 }
      span2 = emptySpan { spanStartLine = start2, spanEndLine = start2 }
      merged = mergeSpans span1 span2
  in property $ spanStartLine merged <= min start1 start2 .&&.
     spanEndLine merged >= max end1 start2

-- Property 8: SourcePos ordering is consistent
prop_sourcepos_ordering_consistent :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_sourcepos_ordering_consistent l1 c1 o1 l2 c2 o2 =
  l1 >= 1 && c1 >= 1 && o1 >= 0 && l2 >= 1 && c2 >= 1 && o2 >= 0 ==>
  let pos1 = SourcePos l1 c1 o1
      pos2 = SourcePos l2 c2 o2
  in if l1 < l2 || (l1 == l2 && c1 < c2) || (l1 == l2 && c1 == c2 && o1 < o2)
     then property $ pos1 <= pos2
     else if l1 > l2 || (l1 == l2 && c1 > c2) || (l1 == l2 && c1 == c2 && o1 > o2)
          then property $ pos1 >= pos2
          else property $ pos1 === pos2

-- Property 9: splitBy roundtrip with join
prop_splitBy_join_roundtrip :: Char -> String -> Property
prop_splitBy_join_roundtrip delim str =
  let parts = splitBy delim str
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === str

-- Property 10: SourcePos offset consistency
prop_sourcepos_offset_consistency :: Int -> Int -> Property
prop_sourcepos_offset_consistent line col =
  line >= 1 && col >= 1 && line <= 100 && col <= 100 ==>
  let pos = posAt line col
      offset = posOffset pos
  in property $ offset >= 0

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Core QuickCheck Tests"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy L.and splitByCollapsed relationship" prop_splitBy_collapsed_relationship
  , fastProperty "removeLineComments preserves content" prop_removeLineComments_preserves_content
  , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_lines
  , fastProperty "posAfter increments line for newline" prop_posAfter_newline_increments_line
  , fastProperty "posAfter increments column for chars" prop_posAfter_char_increments_column
  , fastProperty "mergeSpans contains originals" prop_mergeSpans_contains_originals
  , fastProperty "SourcePos ordering consistent" prop_sourcepos_ordering_consistent
  , fastProperty "splitBy join roundtrip" prop_splitBy_join_roundtrip
  , fastProperty "SourcePos offset consistency" prop_sourcepos_offset_consistency
  ]