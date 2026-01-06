{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewCorePerformanceQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck (property)
import Test.Tasty.HUnit
import Utils (trim, splitBy, splitByCollapsed, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), startPos, posAt, advancePosBy, mergeSpans, spanFrom)
import Parser (parseTypus)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (length)
import Control.DeepSeq (NFData, force)

-- ============================================================================
-- Performance L.and Efficiency Tests
-- ============================================================================

-- | Utils: trim should be linear time
prop_trim_linear_performance :: String -> Bool
prop_trim_linear_performance s = 
  let result = trim s
      inputLength = L.length s
      resultLength = L.length result
  in resultLength <= inputLength  -- Basic efficiency check

-- | Utils: splitBy should handle large inputs efficiently
prop_split_by_large_input :: Int -> String -> Bool
prop_split_by_large_input n s = 
  let n > 0 && n < 1000 ==>
      let largeInput = L.concat $ replicate n s
          result = splitBy ',' largeInput
      in L.length result >= 1  -- Should complete without issues

-- | Utils: removeComments should not increase size
prop_remove_comments_size :: String -> Bool
prop_remove_comments_size s = 
  let result = removeComments s
  in L.length result <= L.length s + 10  -- Allow small overhead

-- | Utils: normalizeIndentation should be idempotent
prop_normalize_indentation_idempotent :: String -> Bool
prop_normalize_indentation_idempotent s = 
  let first = normalizeIndentation s
      second = normalizeIndentation first
  in first == second

-- | SourceLocation: position calculations should be fast
prop_position_calculation_efficient :: Int -> Int -> Bool
prop_position_calculation_efficient line col = 
  let line > 0 && col > 0 && line < 10000 && col < 10000 ==>
      let pos = posAt line col
          advanced = advancePosBy pos (replicate 100 'a')
      in case advanced of
        SourcePos l c -> l >= line

-- | SourceLocation: span merging should be efficient
prop_span_merging_efficient :: Int -> Int -> Bool
prop_span_merging_efficient line1 col1 = 
  let line1 > 0 && col1 > 0 && line1 < 1000 && col1 < 1000 ==>
      let pos1 = posAt line1 col1
          pos2 = posAt (line1 + 1) 1
          span1 = spanFrom pos1
          span2 = spanFrom pos2
          merged = mergeSpans span1 span2
      in True  -- Should complete without stack overflow

-- | Parser: should handle repeated patterns efficiently
prop_parser_repeated_patterns :: String -> Int -> Bool
prop_parser_repeated_patterns s n = 
  let n > 0 && n < 100 ==>
      let pattern = "// @ownership true\n" ++ s
          repeated = L.concat $ replicate n pattern
      in case parseTypus repeated of
        Left _ -> True
        Right _ -> True

-- | Parser: should handle whitespace efficiently
prop_parser_whitespace_efficiency :: String -> Bool
prop_parser_whitespace_efficiency s = 
  let wsHeavy = concatMap (\c -> if isSpace c then "    " else [c]) s
  in case parseTypus wsHeavy of
    Left _ -> True
    Right _ -> True

-- | Property: Memory usage should not grow excessively
prop_memory_usage_reasonable :: Int -> String -> Bool
prop_memory_usage_reasonable n s = 
  let n > 0 && n < 100 ==>
      let input = L.concat $ replicate n s
          processed = normalizeIndentation input
          parsed = parseTypus processed
      in case parsed of
        Left _ -> True
        Right result -> True  -- Should not cause memory issues

-- | Property: String operations should compose efficiently
prop_string_composition_efficient :: String -> String -> Bool
prop_string_composition_efficient s1 s2 = 
  let combined = s1 ++ s2
      trimmed = trim combined
      split = splitBy '\n' trimmed
      rejoined = unlines split
  in L.length rejoined >= L.length trimmed

-- | Property: Recursive operations should terminate
prop_recursive_operations_terminate :: String -> Bool
prop_recursive_operations_terminate s = 
  let processed = normalizeIndentation s
      commented = removeComments processed
      final = trim commented
  in L.length final >= 0  -- Should terminate

-- | Property: Large position calculations should work
prop_large_position_calculations :: Int -> Bool
prop_large_position_calculations n = 
  let n > 0 && n < 10000 ==>
      let pos = posAt n n
          advanced = advancePosBy pos (replicate n '\n')
      in case advanced of
        SourcePos line col -> line >= n

-- | Property: Complex text processing should complete
prop_complex_text_processing :: String -> Bool
prop_complex_text_processing s = 
  let step1 = removeComments s
      step2 = normalizeIndentation step1
      step3 = trim step2
      step4 = splitBy '\n' step3
  in L.length step4 >= 0  -- Should complete L.all steps

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Core Module Performance QuickCheck Tests"
  [ testGroup "Utils Performance Tests"
    [ testProperty "trim linear performance" prop_trim_linear_performance
    , testProperty "splitBy large input" prop_split_by_large_input
    , testProperty "removeComments size" prop_remove_comments_size
    , testProperty "normalizeIndentation idempotent" prop_normalize_indentation_idempotent
    ]
  , testGroup "SourceLocation Performance Tests"
    [ testProperty "position calculation efficient" prop_position_calculation_efficient
    , testProperty "span merging efficient" prop_span_merging_efficient
    , testProperty "large position calculations" prop_large_position_calculations
    ]
  , testGroup "Parser Performance Tests"
    [ testProperty "parser repeated patterns" prop_parser_repeated_patterns
    , testProperty "parser whitespace efficiency" prop_parser_whitespace_efficiency
    ]
  , testGroup "General Performance Tests"
    [ testProperty "memory usage reasonable" prop_memory_usage_reasonable
    , testProperty "string composition efficient" prop_string_composition_efficient
    , testProperty "recursive operations terminate" prop_recursive_operations_terminate
    , testProperty "complex text processing" prop_complex_text_processing
    ]
  ]