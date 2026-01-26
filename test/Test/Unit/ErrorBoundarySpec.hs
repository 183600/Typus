{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing #-}
module Test.Unit.ErrorBoundarySpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Utils (trim, splitBy, removeComments, normalizeIndentation, isValidChar)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAt, spanBetween, isValidSpan)
import Data.Char (isSpace, isControl)

-- Test properties for error boundary conditions

-- Property: trim should handle all whitespace strings
prop_trim_all_whitespace :: String -> Property
prop_trim_all_whitespace s = 
  all isSpace s ==> property $ trim s == ""

-- Property: splitBy should handle delimiter at boundaries
prop_split_delimiter_at_boundaries :: Char -> String -> Property
prop_split_delimiter_at_boundaries c s = 
  let sWithDelim = c : s ++ [c]
      parts = splitBy c sWithDelim
      firstPart = case parts of
                   [] -> ""
                   (x:_) -> x
  in property $ 
       not (null parts) &&
       firstPart == "" &&
       last parts == ""

-- Property: removeComments should handle strings without comments
prop_remove_comments_no_comments :: String -> Property
prop_remove_comments_no_comments s = 
  not (("//" `isInfixOf` s) || ("/*" `isInfixOf` s)) ==>
  property $ removeComments s == s

-- Property: normalizeIndentation should handle empty lines
prop_normalize_empty_lines :: String -> Property
prop_normalize_empty_lines s = 
  let linesWithEmpty = s ++ "\n\n" ++ s
      normalized = normalizeIndentation linesWithEmpty
      lineCount = length $ lines normalized
  in property $ lineCount >= 2

-- Property: isValidChar should reject control characters
prop_valid_char_rejects_control :: Char -> Property
prop_valid_char_rejects_control c = 
  isControl c ==> property $ not (isValidChar c)

-- Property: SourceSpan should be invalid for impossible positions
prop_source_span_invalid_impossible :: Int -> Int -> Int -> Int -> Property
prop_source_span_invalid_impossible line1 col1 line2 col2 =
  (line1 > line2 || (line1 == line2 && col1 > col2)) ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
  in property $ not (isValidSpan span)

-- Property: String processing should handle Unicode
prop_unicode_handling :: String -> Property
prop_unicode_handling s = 
  let processed = trim s
  in property $ length processed <= length s

-- Unit tests

test_error_boundary_empty_string :: Assertion
test_error_boundary_empty_string = do
  trim "" @?= ""
  splitBy ',' "" @?= []
  normalizeIndentation "" @?= ""
  removeComments "" @?= ""

test_error_boundary_null_input :: Assertion
test_error_boundary_null_input = do
  -- Test that functions handle edge cases gracefully
  isValidChar '\0' @?= False
  isValidChar '\DEL' @?= False

test_error_boundary_large_input :: Assertion
test_error_boundary_large_input = do
  let largeString = replicate 10000 'a'
  length (trim largeString) @?= 10000
  length (splitBy ',' largeString) @?= 1

test_error_boundary_special_characters :: Assertion
test_error_boundary_special_characters = do
  isValidChar ' ' @?= True
  isValidChar '\t' @?= True
  isValidChar '\n' @?= True
  isValidChar '\r' @?= True

test_error_boundary_nested_comments :: Assertion
test_error_boundary_nested_comments = do
  removeComments "code /* outer /* inner */ still outer */ end" @?= "code  end"

test_error_boundary_mixed_indentation :: Assertion
test_error_boundary_mixed_indentation = do
  normalizeIndentation "  line1\n\tline2\n    line3" @?= "line1\n  line2\n    line3"

tests :: TestTree
tests = testGroup "Error Boundary Tests"
  [ testProperty "trim all whitespace" prop_trim_all_whitespace
  , testProperty "split delimiter at boundaries" prop_split_delimiter_at_boundaries
  , testProperty "remove comments no comments" prop_remove_comments_no_comments
  , testProperty "normalize empty lines" prop_normalize_empty_lines
  , testProperty "valid char rejects control" prop_valid_char_rejects_control
  , testProperty "source span invalid impossible" prop_source_span_invalid_impossible
  , testProperty "unicode handling" prop_unicode_handling
  , testCase "error boundary empty string" test_error_boundary_empty_string
  , testCase "error boundary null input" test_error_boundary_null_input
  , testCase "error boundary large input" test_error_boundary_large_input
  , testCase "error boundary special characters" test_error_boundary_special_characters
  , testCase "error boundary nested comments" test_error_boundary_nested_comments
  , testCase "error boundary mixed indentation" test_error_boundary_mixed_indentation
  ]