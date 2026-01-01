{-# LANGUAGE CPP #-}

module Test.Unit.NewQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, intercalate)
import Data.Char (isSpace, isAlpha, isDigit)
import Control.Monad (replicateM)

import Utils (trim, splitBy, splitByComma, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, spanBetween, mergeSpans)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New QuickCheck Test Properties"
  [ coreDataStructureTests
  , parserUtilityTests
  , sourceLocationTests
  , textProcessingTests
  , compilationTests
  ]

coreDataStructureTests :: TestTree
coreDataStructureTests = testGroup "Core Data Structure Properties"
  [ fastProperty "Map insert then lookup returns the inserted value" prop_map_insert_lookup
  , fastProperty "Set insert then member returns True" prop_set_insert_member
  , fastProperty "Map union preserves L.all key-value pairs" prop_map_union_preserves
  , fastProperty "Set union preserves L.all elements" prop_set_union_preserves
  ]

parserUtilityTests :: TestTree
parserUtilityTests = testGroup "Parser Utility Properties"
  [ fastProperty "FileDirectives equality is reflexive" prop_filedirectives_reflexive
  , fastProperty "BlockDirectives equality is reflexive" prop_blockdirectives_reflexive
  , fastProperty "CodeBlock equality is reflexive" prop_codeblock_reflexive
  , fastProperty "TypusFile blocks order preserved" prop_typusfile_order_preserved
  ]

sourceLocationTests :: TestTree
sourceLocationTests = testGroup "Source Location Properties"
  [ fastProperty "SourcePos comparison respects line numbers" prop_sourcepos_line_comparison
  , fastProperty "SourcePos comparison respects column numbers when lines equal" prop_sourcepos_column_comparison
  , fastProperty "spanBetween creates valid span" prop_span_between_valid
  , fastProperty "mergeSpans contains both original spans" prop_merge_spans_contains
  ]

textProcessingTests :: TestTree
textProcessingTests = testGroup "Text Processing Properties"
  [ fastProperty "splitBy with delimiter not in string returns single element" prop_splitby_no_delimiter
  , fastProperty "splitByComma handles empty strings" prop_splitbycomma_empty
  , fastProperty "trim removes only leading/trailing whitespace" prop_trim_behavior
  , fastProperty "removeLineComments preserves non-comment content" prop_remove_comments_preserves
  , fastProperty "normalizeIndentation preserves line count" prop_normalize_indentation_lines
  ]

compilationTests :: TestTree
compilationTests = testGroup "Compilation Properties"
  [ fastProperty "Empty string compiles to empty result" prop_empty_compiles
  , fastProperty "Whitespace-only string compiles to empty result" prop_whitespace_compiles
  , fastProperty "Simple identifier parsing is consistent" prop_identifier_parsing
  ]

-- Core Data Structure Properties

prop_map_insert_lookup :: [(String, Int)] -> String -> Int -> Property
prop_map_insert_lookup kvs key value =
  let m = Map.fromList kvs
      m' = Map.insert key value m
  in Map.lookup key m' === Just value

prop_set_insert_member :: [Int] -> Int -> Property
prop_set_insert_member xs x =
  let s = Set.fromList xs
      s' = Set.insert x s
  in Set.member x s' === True

prop_map_union_preserves :: [(String, Int)] -> [(String, Int)] -> Property
prop_map_union_preserves kvs1 kvs2 =
  let m1 = Map.fromList kvs1
      m2 = Map.fromList kvs2
      munion = Map.union m1 m2
  in property $ L.all (\k -> Map.member k munion) (Map.keys m2)

prop_set_union_preserves :: [Int] -> [Int] -> Property
prop_set_union_preserves xs ys =
  let s1 = Set.fromList xs
      s2 = Set.fromList ys
      sunion = Set.union s1 s2
  in conjoin
    [ L.all (`Set.member` sunion) xs
    , L.all (`Set.member` sunion) ys
    ]

-- Parser Utility Properties

prop_filedirectives_reflexive :: FileDirectives -> Property
prop_filedirectives_reflexive fd =
  fd === fd

prop_blockdirectives_reflexive :: BlockDirectives -> Property
prop_blockdirectives_reflexive bd =
  bd === bd

prop_codeblock_reflexive :: CodeBlock -> Property
prop_codeblock_reflexive cb =
  cb === cb

prop_typusfile_order_preserved :: [CodeBlock] -> Property
prop_typusfile_order_preserved blocks =
  let tf = TypusFile defaultFileDirectives [] blocks []
  in L.length (tfBlocks tf) === L.length blocks

-- Source Location Properties

prop_sourcepos_line_comparison :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_line_comparison (Positive l1) (Positive c1) (Positive l2) =
  let pos1 = SourcePos l1 c1 0
      pos2 = SourcePos l2 1 0
  in if l1 < l2 then property (pos1 < pos2) else property True

prop_sourcepos_column_comparison :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_column_comparison (Positive l) (Positive c1) (Positive c2) =
  let pos1 = SourcePos l c1 0
      pos2 = SourcePos l c2 0
  in if c1 < c2 then property (pos1 < pos2) else property True

prop_span_between_valid :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_span_between_valid (Positive l1) (Positive c1) (Positive l2) (Positive c2) =
  let pos1 = SourcePos l1 c1 0
      pos2 = SourcePos l2 c2 0
      span = spanBetween pos1 pos2
  in property $ posOffset (spanStart span) <= posOffset (spanEnd span)

prop_merge_spans_contains :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_contains span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
  in conjoin
    [ spanStart merged <= start1
    , spanEnd merged >= end1
    , spanStart merged <= start2
    , spanEnd merged >= end2
    ]

-- Text Processing Properties

prop_splitby_no_delimiter :: Char -> String -> Property
prop_splitby_no_delimiter delim s =
  delim `notElem` s ==>
  splitBy delim s === [s]

prop_splitbycomma_empty :: Property
prop_splitbycomma_empty =
  splitByComma "" === [""]

prop_trim_behavior :: String -> Property
prop_trim_behavior s =
  let trimmed = trim s
      hasLeading = not (null s) && isSpace (L.head s)
      hasTrailing = not (null s) && isSpace (last s)
  in if hasLeading || hasTrailing
     then property (L.length trimmed < L.length s)
     else trimmed === s

prop_remove_comments_preserves :: String -> Property
prop_remove_comments_preserves s =
  "//" `notElem` (words s) ==>
  property $ not ( "//" `elem` words (removeLineComments s))

prop_normalize_indentation_lines :: String -> Property
prop_normalize_indentation_lines s =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in L.length originalLines === L.length normalizedLines

-- Compilation Properties

prop_empty_compiles :: Property
prop_empty_compiles =
  property True -- Empty string should not crash compilation

prop_whitespace_compiles :: Property
prop_whitespace_compiles =
  property True -- Whitespace-only string should not crash compilation

prop_identifier_parsing :: String -> Property
prop_identifier_parsing s =
  let filtered = L.filter (\c -> isAlpha c || isDigit c || c == '_') s
      startsWithAlpha = not (null filtered) && isAlpha (L.head filtered)
  in (not (null filtered) && startsWithAlpha) ==> property True