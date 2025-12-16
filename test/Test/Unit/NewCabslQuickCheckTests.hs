{-# LANGUAGE CPP #-}

module Test.Unit.NewCabslQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, Property, property)
import qualified Data.Map as Map
import Data.List (sort, nub, intersperse)
import Data.Char (isSpace, toLower, toUpper)

import Utils (trim, splitBy, splitByComma, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
-- IR imports removed as they don't exist in the current module structure

-- Arbitrary instances for testing
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    col <- choose (0, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    startLine <- choose (1, 50)
    startCol <- choose (0, 50)
    startOffset <- choose (0, 500)
    let start = SourcePos startLine startCol startOffset
    endLine <- choose (startLine, startLine + 50)
    endCol <- choose (if endLine == startLine then startCol else 0, 100)
    endOffset <- choose (startOffset, startOffset + 500)
    let end = SourcePos endLine endCol endOffset
    return $ SourceSpan start end

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ stringProcessingTests
  , sourceLocationTests
  , dataStructureTests
  ]

-- Test 1: String processing properties
stringProcessingTests :: TestTree
stringProcessingTests = testGroup "String Processing Properties"
  [ fastProperty "trim removes only outer whitespace" prop_trim_outer_only
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy and join are inverses for simple cases" prop_split_join_inverse
  , fastProperty "splitByComma handles empty strings" prop_split_comma_empty
  , fastProperty "normalizeIndentation preserves relative structure" prop_normalize_preserves_structure
  ]

-- Test 2: Source location properties
sourceLocationTests :: TestTree
sourceLocationTests = testGroup "Source Location Properties"
  [ fastProperty "startPos has zero column" prop_start_pos_column
  , fastProperty "spanBetween has positive length" prop_span_positive_length
  , fastProperty "merge spans preserves containment" prop_merge_span_containment
  ]

-- Test 3: Data structure properties
dataStructureTests :: TestTree
dataStructureTests = testGroup "Data Structure Properties"
  [ fastProperty "Map operations are consistent" prop_map_operations
  , fastProperty "List operations preserve length" prop_list_operations
  ]



-- String processing property implementations
prop_trim_outer_only :: String -> Property
prop_trim_outer_only str =
  let trimmed = trim str
      hasLeadingSpace = not (null str) && isSpace (head str)
      hasTrailingSpace = not (null str) && isSpace (last str)
  in property $
    if hasLeadingSpace || hasTrailingSpace
    then length trimmed < length str
    else trimmed == str

prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmed = trim str
      trimmedAgain = trim trimmed
  in property $ trimmed == trimmedAgain

prop_split_join_inverse :: [String] -> Char -> Property
prop_split_join_inverse parts sep =
  let joined = concat $ intersperse [sep] parts
      splitParts = splitBy sep joined
  in property $ splitParts == parts

prop_split_comma_empty :: Property
prop_split_comma_empty =
  property $ splitByComma "" == [""]

prop_normalize_preserves_structure :: [String] -> Property
prop_normalize_preserves_structure lineList =
  let indented = map ("  " ++) lineList
      inputText = unlines indented
      normalized = normalizeIndentation inputText
      resultLines = Prelude.lines normalized
  in property $ length resultLines == length indented

-- Source location property implementations
prop_start_pos_column :: Property
prop_start_pos_column =
  let pos = startPos
  in property $ posColumn pos == 0

prop_span_positive_length :: SourcePos -> SourcePos -> Property
prop_span_positive_length pos1 pos2 =
  let span = spanBetween pos1 pos2
  in property $ spanEnd span >= spanStart span

prop_merge_span_containment :: SourceSpan -> SourceSpan -> Property
prop_merge_span_containment span1 span2 =
  let merged = spanBetween (spanStart span1) (spanEnd span2)
  in property $ 
    spanStart merged <= spanStart span1 &&
    spanEnd merged >= spanEnd span2

-- Data structure property implementations
prop_map_operations :: [(String, Int)] -> String -> Property
prop_map_operations pairs key =
  let mp = Map.fromList pairs
      value = Map.lookup key mp
  in property $ case value of
    Nothing -> not (key `elem` map fst pairs)
    Just v -> (key, v) `elem` pairs

prop_list_operations :: [Int] -> Property
prop_list_operations xs =
  let sorted = sort xs
      unique = nub xs
  in property $ length unique <= length xs && length sorted == length xs
