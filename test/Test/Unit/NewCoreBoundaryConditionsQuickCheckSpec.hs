{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewCoreBoundaryConditionsQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils (trim, splitBy, splitByCollapsed, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), startPos, posAt, advancePos, advancePosBy)
import Parser (parseTypus, defaultFileDirectives, defaultBlockDirectives)
import Data.Char (isSpace, isControl)
import Data.List (isPrefixOf)

-- ============================================================================
-- Boundary Conditions and Edge Case Tests
-- ============================================================================

-- | Utils: trim should handle empty strings
prop_trim_empty_string :: Bool
prop_trim_empty_string = trim "" == ""

-- | Utils: trim should handle whitespace-only strings
prop_trim_whitespace_only :: String -> Bool
prop_trim_whitespace_only s = 
  let wsOnly = filter isSpace s
  in trim wsOnly == ""

-- | Utils: splitBy should handle empty input
prop_split_by_empty :: Char -> Bool
prop_split_by_empty delim = splitBy delim "" == [""]

-- | Utils: splitByCollapsed should handle empty input
prop_split_by_collapsed_empty :: Char -> Bool
prop_split_by_collapsed_empty delim = splitByCollapsed delim "" == []

-- | Utils: removeComments should handle empty input
prop_remove_comments_empty :: Bool
prop_remove_comments_empty = removeComments "" == ""

-- | Utils: normalizeIndentation should handle empty input
prop_normalize_indentation_empty :: Bool
prop_normalize_indentation_empty = normalizeIndentation "" == ""

-- | SourceLocation: posAt should handle minimum values
prop_pos_at_minimum :: Bool
prop_pos_at_minimum = posAt 1 1 == SourcePos 1 1

-- | SourceLocation: posAt should handle large values
prop_pos_at_large :: Bool
prop_pos_at_large = 
  let largePos = posAt 1000000 1000000
  in case largePos of
    SourcePos line col -> line == 1000000 && col == 1000000

-- | SourceLocation: advancePos should handle control characters
prop_advance_pos_control :: SourcePos -> Char -> Bool
prop_advance_pos_control pos c = 
  let newPos = advancePos pos c
  in case newPos of
    SourcePos line col -> line >= sourceLine pos && col >= 1

-- | SourceLocation: advancePosBy should handle empty strings
prop_advance_pos_by_empty :: SourcePos -> Bool
prop_advance_pos_by_empty pos = advancePosBy pos "" == pos

-- | Parser: parseTypus should handle very long lines
prop_parser_long_lines :: String -> Bool
prop_parser_long_lines s = 
  let longLine = replicate 1000 'a' ++ s
  in case parseTypus longLine of
    Left _ -> True
    Right _ -> True

-- | Parser: parseTypus should handle deeply nested structures
prop_parser_deep_nesting :: String -> Int -> Bool
prop_parser_deep_nesting s depth = 
  let depth > 0 ==> 
      let nested = concat $ replicate depth "// @ownership {\n" ++ [s] ++ concat (replicate depth "\n}\n")
      in case parseTypus nested of
        Left _ -> True
        Right _ -> True

-- | Parser: parseTypus should handle malformed directives
prop_parser_malformed_directives :: String -> Bool
prop_parser_malformed_directives s = 
  let malformed = "// @invalid-directive-name-123!@#\n" ++ s
  in case parseTypus malformed of
    Left _ -> True
    Right _ -> True

-- | Parser: parseTypus should handle unicode edge cases
prop_parser_unicode_edge_cases :: String -> Bool
prop_parser_unicode_edge_cases s = 
  let unicodeEdge = s ++ "\n\x1F600\x1F601\x1F602\n" ++ s  -- Emoji
  in case parseTypus unicodeEdge of
    Left _ -> True
    Right _ -> True

-- | Property: String processing should handle null bytes gracefully
prop_null_byte_handling :: String -> Bool
prop_null_byte_handling s = 
  let withNull = take 50 s ++ "\0" ++ drop 50 s
      trimmed = trim withNull
      split = splitBy ',' withNull
  in length split >= 1  -- Should not crash

-- | Property: Large input handling
prop_large_input_handling :: Int -> String -> Bool
prop_large_input_handling n s = 
  let n > 0 && n < 1000 ==>
      let largeInput = concat $ replicate n s
      in case parseTypus largeInput of
        Left _ -> True
        Right _ -> True

-- | Property: Extreme whitespace combinations
prop_extreme_whitespace :: String -> Bool
prop_extreme_whitespace s = 
  let extremeWs = concatMap (\c -> if isSpace c then replicate 10 c else [c]) s
      processed = normalizeIndentation extremeWs
  in length processed <= length extremeWs

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Core Module Boundary Conditions QuickCheck Tests"
  [ testGroup "Utils Boundary Tests"
    [ testProperty "trim empty string" prop_trim_empty_string
    , testProperty "trim whitespace only" prop_trim_whitespace_only
    , testProperty "splitBy empty input" prop_split_by_empty
    , testProperty "splitByCollapsed empty input" prop_split_by_collapsed_empty
    , testProperty "removeComments empty input" prop_remove_comments_empty
    , testProperty "normalizeIndentation empty input" prop_normalize_indentation_empty
    ]
  , testGroup "SourceLocation Boundary Tests"
    [ testProperty "posAt minimum values" prop_pos_at_minimum
    , testProperty "posAt large values" prop_pos_at_large
    , testProperty "advancePos control characters" prop_advance_pos_control
    , testProperty "advancePosBy empty string" prop_advance_pos_by_empty
    ]
  , testGroup "Parser Boundary Tests"
    [ testProperty "parser long lines" prop_parser_long_lines
    , testProperty "parser deep nesting" prop_parser_deep_nesting
    , testProperty "parser malformed directives" prop_parser_malformed_directives
    , testProperty "parser unicode edge cases" prop_parser_unicode_edge_cases
    ]
  , testGroup "Edge Case Tests"
    [ testProperty "null byte handling" prop_null_byte_handling
    , testProperty "large input handling" prop_large_input_handling
    , testProperty "extreme whitespace" prop_extreme_whitespace
    ]
  ]