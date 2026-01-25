module Test.Unit.CombinedFeaturesSpec where



import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.List (isInfixOf)
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAt, spanBetween, spanStart, spanEnd)

-- Test properties for combined features

-- Property: trim after split should not produce empty strings at ends
prop_trim_after_split_no_empty_ends :: Char -> String -> Property
prop_trim_after_split_no_empty_ends c s = 
  not (null s) ==> 
  let parts = splitBy c s
      trimmedParts = map trim parts
  in property $ 
       (null trimmedParts || case trimmedParts of (x:_) -> not (null x); [] -> True) &&
       (null trimmedParts || not (null (last trimmedParts)))

-- Property: normalizeIndentation after removeComments should preserve line count
prop_normalize_preserves_line_count :: String -> Property
prop_normalize_preserves_line_count s =
  let noComments = removeComments s
      normalized = normalizeIndentation noComments
      originalLines = length $ lines noComments
      normalizedLines = length $ lines normalized
  in property $ originalLines == normalizedLines

-- Property: SourceSpan positions should be consistent
prop_source_span_consistency :: Int -> Int -> Int -> Int -> Property
prop_source_span_consistency line1 col1 line2 col2 =
  line1 >= 1 && col1 >= 1 && line2 >= 1 && col2 >= 1 &&
  (line1 < line2 || (line1 == line2 && col1 <= col2)) ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
  in property $ 
       posLine (spanStart span) == line1 && 
       posColumn (spanStart span) == col1 &&
       posLine (spanEnd span) == line2 &&
       posColumn (spanEnd span) == col2

-- Property: String processing roundtrip
prop_string_processing_roundtrip :: String -> Property
prop_string_processing_roundtrip s =
  let trimmed = trim s
      normalized = normalizeIndentation trimmed
      processed = trim normalized
  in property $ processed == normalized

-- Property: Split and join should be inverse for simple cases
prop_split_join_inverse :: Char -> String -> Property
prop_split_join_inverse c s = 
  c `notElem` s ==> 
  let parts = splitBy c s
      rejoined = foldr (\x acc -> if null acc then x else x ++ [c] ++ acc) "" parts
  in property $ rejoined == s

-- Unit tests

test_source_position_creation :: Assertion
test_source_position_creation = do
  let pos = posAt 5 10
  posLine pos @?= 5
  posColumn pos @?= 10

test_trim_edge_cases :: Assertion
test_trim_edge_cases = do
  trim "" @?= ""
  trim "   " @?= ""
  trim "text" @?= "text"
  trim "  text  " @?= "text"

test_split_edge_cases :: Assertion
test_split_edge_cases = do
  splitBy ',' "" @?= []
  splitBy ',' "," @?= [""]
  splitBy ',' "a,b,c" @?= ["a", "b", "c"]
  splitBy ',' "a,,b" @?= ["a", "", "b"]

test_remove_comments_basic :: Assertion
test_remove_comments_basic = do
  removeComments "code // comment" @?= "code "
  removeComments "code /* comment */ more" @?= "code  more"

test_normalize_indentation :: Assertion
test_normalize_indentation = do
  normalizeIndentation "  line1\n    line2" @?= "line1\n  line2"

tests :: TestTree
tests = testGroup "Combined Features Tests"
  [ testProperty "trim after split no empty ends" prop_trim_after_split_no_empty_ends
  , testProperty "normalize preserves line count" prop_normalize_preserves_line_count
  , testProperty "source span consistency" prop_source_span_consistency
  , testProperty "string processing roundtrip" prop_string_processing_roundtrip
  , testProperty "split join inverse" prop_split_join_inverse
  , testCase "source position creation" test_source_position_creation
  , testCase "trim edge cases" test_trim_edge_cases
  , testCase "split edge cases" test_split_edge_cases
  , testCase "remove comments basic" test_remove_comments_basic
  , testCase "normalize indentation" test_normalize_indentation
  ]