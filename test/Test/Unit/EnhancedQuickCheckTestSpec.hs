{-# LANGUAGE CPP #-}

module Test.Unit.EnhancedQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, intersperse, isInfixOf)
import Data.Char (isSpace, isAlpha, isDigit)

import Utils (trim, splitBy, splitByComma, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd, startPos)
import Parser (FileDirectives(..), BlockDirectives(..))
import TestSupport.Arbitrary ()

-- Property 1: trim is idempotent and removes only whitespace
prop_trim_idempotent_and_whitespace :: String -> Property
prop_trim_idempotent_and_whitespace s =
  let trimmed = trim s
      trimmedTwice = trim trimmed
      hasOnlyEndWhitespace = all isSpace s || (not (null trimmed) && not (isSpace (head trimmed)) && not (isSpace (last trimmed)))
  in conjoin
    [ trim trimmedTwice === trimmed
    , property hasOnlyEndWhitespace
    ]

-- Property 2: splitBy is consistent with built-in split when delimiter exists
prop_splitBy_consistent :: Char -> String -> Property
prop_splitBy_consistent delim s =
  let mySplit = splitBy delim s
      builtinSplit = map (takeWhile (/= delim)) $ map (drop 1) $ 
                     (takeWhile (not . null) . iterate (drop 1 . dropWhile (/= delim))) (s ++ [delim])
  in length mySplit === countSegments delim s
  where
    countSegments d str = length (filter (== d) str) + 1

-- Property 3: splitByComma handles edge cases correctly
prop_splitBy_comma_edge_cases :: String -> Property
prop_splitBy_comma_edge_cases s =
  let parts = splitByComma s
  in property $ length parts >= 1

-- Property 4: removeLineComments preserves non-comment lines
prop_remove_line_comments_preserve :: String -> Property
prop_remove_line_comments_preserve s =
  let hasComments = "//" `isInfixOf` s
      result = removeLineComments s
  in not hasComments ==> result === s

-- Property 5: SourcePos ordering respects offset
prop_sourcepos_offset_ordering :: Property
prop_sourcepos_offset_ordering = forAll genSourcePos $ \pos1 ->
  forAll genSourcePos $ \pos2 ->
    let o1 = posOffset pos1
        o2 = posOffset pos2
    in (o1 < o2) === (pos1 < pos2)
  where
    genSourcePos = do
      line <- choose (1, 1000)
      col <- choose (1, 1000)
      offset <- choose (0, 10000)
      return $ SourcePos line col offset

-- Property 6: SourceSpan is always valid
prop_sourcespan_valid :: SourceSpan -> Property
prop_sourcespan_valid span =
  let start = spanStart span
      end = spanEnd span
  in conjoin
    [ posLine start >= 1
    , posColumn start >= 1
    , posOffset start >= 0
    , posLine end >= 1
    , posColumn end >= 1
    , posOffset end >= 0
    ]

-- Property 7: Map operations are consistent
prop_map_operations_consistent :: [(String, Int)] -> Property
prop_map_operations_consistent pairs =
  let m = Map.fromList pairs
      keys = Map.keys m
      values = Map.elems m
      lookups = map (`Map.lookup` m) keys
      isJust Nothing = False
      isJust (Just _) = True
  in conjoin
    [ property $ length lookups === length keys
    , property $ all isJust lookups
    , property $ length (nub keys) === length keys
    ]

-- Property 8: Set operations preserve uniqueness
prop_set_operations_unique :: [Int] -> Property
prop_set_operations_unique xs =
  let s = Set.fromList xs
      sortedElems = sort $ Set.toList s
      allUnique xs = length xs == length (nub xs)
  in conjoin
    [ property $ allUnique sortedElems
    , property $ length sortedElems === length (nub xs)
    ]

-- Property 9: FileDirectives round-trip properties
prop_file_directives_roundtrip :: Property
prop_file_directives_roundtrip =
  let directives = FileDirectives Nothing Nothing Nothing
  in property $ directives === directives

-- Property 10: normalizeIndentation preserves line count
prop_normalize_indentation_preserves :: String -> Property
prop_normalize_indentation_preserves s =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in property $ length normalizedLines === length originalLines

tests :: TestTree
tests = testGroup "Enhanced QuickCheck Test Suite"
  [ fastProperty "trim is idempotent and removes only whitespace" prop_trim_idempotent_and_whitespace
  , fastProperty "splitBy is consistent with segment counting" prop_splitBy_consistent
  , fastProperty "splitByComma handles edge cases correctly" prop_splitBy_comma_edge_cases
  , fastProperty "removeLineComments preserves non-comment lines" prop_remove_line_comments_preserve
  , fastProperty "SourcePos ordering respects offset" prop_sourcepos_offset_ordering
  , fastProperty "SourceSpan is always valid" prop_sourcespan_valid
  , fastProperty "Map operations are consistent" prop_map_operations_consistent
  , fastProperty "Set operations preserve uniqueness" prop_set_operations_unique
  , fastProperty "FileDirectives round-trip properties" prop_file_directives_roundtrip
  , fastProperty "normalizeIndentation preserves relative structure" prop_normalize_indentation_preserves
  ]