module Test.Unit.TypeSystemEdgeCasesSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, Property, (===), forAll, counterexample, conjoin, testProperty, (==>))
import Test.Tasty.QuickCheck

import Test.QuickCheck (Gen, cover)
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, sort, nub)
import Utils (trim, splitBy, removeComments, normalizeIndentation, safeProcessString)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAt, spanBetween, spanStart, spanEnd)
import Data.Char (isSpace, isAlpha, isAlphaNum, isDigit, isLetter, toLower, toUpper)

-- Test properties for type system edge cases

-- Property: String transformation should preserve character count for simple cases
prop_string_transform_preserve_count :: String -> Property
prop_string_transform_preserve_count s = 
  let trimmed = trim s
      lower = map toLower trimmed
      upper = map toUpper trimmed
  in property $ length lower == length trimmed && length upper == length trimmed

-- Property: Splitting and joining should be reversible for non-empty delimiters
prop_split_join_reversible :: Char -> String -> Property
prop_split_join_reversible c s = 
  c `notElem` s ==> 
  let parts = splitBy c s
      rejoined = intercalate [c] parts
  in property $ rejoined == s
  where
    intercalate _ [] = []
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Property: Comment removal should reduce or maintain string length
prop_comment_reduces_length :: String -> String -> Property
prop_comment_reduces_length code comment = 
  let input = code ++ "// " ++ comment
      withoutComments = removeComments input
  in property $ length withoutComments <= length input

-- Property: Normalization should not increase line count
prop_normalize_no_increase_lines :: String -> Property
prop_normalize_no_increase_lines s = 
  let linesOriginal = length $ lines s
      normalized = normalizeIndentation s
      linesNormalized = length $ lines normalized
  in property $ linesNormalized <= linesOriginal

-- Property: Safe processing should handle all inputs gracefully
prop_safe_processing_graceful :: String -> Property
prop_safe_processing_graceful s = 
  let processed = safeProcessString s
  in case processed of
       Left _ -> property True
       Right str -> property $ length str >= 0 && all (`elem` ['\0'..'\127']) str

-- Property: Position calculations should be monotonic
prop_position_monotonic :: Int -> Int -> Int -> Property
prop_position_monotonic line col offset = 
  line >= 1 && col >= 1 && offset >= 0 ==>
  let original = posAt line col
      newLine = line + offset
      newCol = col + offset
      updated = posAt newLine newCol
  in property $ 
       posLine updated >= posLine original && 
       posColumn updated >= posColumn original

-- Property: Span creation should handle ordered positions
prop_span_ordered_positions :: Int -> Int -> Int -> Int -> Property
prop_span_ordered_positions line1 col1 line2 col2 =
  line1 >= 1 && col1 >= 1 && line2 >= 1 && col2 >= 1 &&
  (line1 < line2 || (line1 == line2 && col1 <= col2)) ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
  in property $ 
       posLine (spanStart span) <= posLine (spanEnd span) &&
       (posLine (spanStart span) < posLine (spanEnd span) || 
        posColumn (spanStart span) <= posColumn (spanEnd span))

-- Property: String operations should handle empty strings
prop_string_operations_empty :: String -> Property
prop_string_operations_empty s = 
  let trimmed = trim s
      split = splitBy ',' s
      processed = safeProcessString s
  in case processed of
       Left _ -> property $ 
         (if null s then null trimmed else True) &&
         (if null s then null split else True)
       Right str -> property $ 
         (if null s then null trimmed else True) &&
         (if null s then null split else True) &&
         length str >= 0

-- Property: Character validation should be consistent
prop_char_validation_consistent :: Char -> Property
prop_char_validation_consistent c = 
  let isAlphaCheck = isAlpha c
      isAlphaNumCheck = isAlphaNum c
      isDigitCheck = isDigit c
      isLetterCheck = isLetter c
  in property $ 
       isDigitCheck ==> isAlphaNumCheck &&
       isAlphaCheck ==> isAlphaNumCheck &&
       isLetterCheck ==> isAlphaCheck

-- Unit tests

test_typesystem_edge_empty :: Assertion
test_typesystem_edge_empty = do
  trim "" @?= ""
  splitBy ',' "" @?= []
  normalizeIndentation "" @?= ""
  safeProcessString "" @?= Right ""

test_typesystem_edge_single_char :: Assertion
test_typesystem_edge_single_char = do
  trim "a" @?= "a"
  splitBy ',' "a" @?= ["a"]
  safeProcessString "a" @?= Right "a"

test_typesystem_edge_whitespace_only :: Assertion
test_typesystem_edge_whitespace_only = do
  trim "   " @?= ""
  trim "\t\n" @?= ""
  normalizeIndentation "  \n  " @?= "\n\n"

test_typesystem_edge_special_chars :: Assertion
test_typesystem_edge_special_chars = do
  let special = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  safeProcessString special @?= Right special
  splitBy ',' special @?= [special]

test_typesystem_edge_long_string :: Assertion
test_typesystem_edge_long_string = do
  let long = replicate 1000 'a'
  length (trim long) @?= 1000
  case safeProcessString long of
    Right str -> length str @?= 1000
    Left _ -> assertFailure "safeProcessString failed on valid input"

test_typesystem_edge_unicode :: Assertion
test_typesystem_edge_unicode = do
  let unicode = "héllo wörld 🌍 测试"
  length (trim unicode) @?= length unicode
  case safeProcessString unicode of
    Right str -> length str @?= length unicode
    Left _ -> assertFailure "safeProcessString failed on valid unicode input"

test_typesystem_edge_nested :: Assertion
test_typesystem_edge_nested = do
  let nested = "{{{{{{nested}}}}}}"
  splitBy '{' nested @?= ["", "", "", "", "", "", "nested", "", "", "", "", ""]
  splitBy '}' nested @?= ["{{{{{{nested", "", "", "", "", "", "", ""]

test_typesystem_edge_comments :: Assertion
test_typesystem_edge_comments = do
  removeComments "// single line" @?= " "
  removeComments "/* multi\nline */" @?= " "
  removeComments "code /* comment */ more" @?= "code  more"

test_typesystem_edge_positions :: Assertion
test_typesystem_edge_positions = do
  let pos1 = posAt 1 1
  let pos2 = posAt 100 100
  let span = spanBetween pos1 pos2
  posLine (spanStart span) @?= 1
  posColumn (spanStart span) @?= 1
  posLine (spanEnd span) @?= 100
  posColumn (spanEnd span) @?= 100

test_typesystem_edge_mixed_content :: Assertion
test_typesystem_edge_mixed_content = do
  let mixed = "  // comment\n  code { /* nested */ }\n"
  let processed = normalizeIndentation (removeComments mixed)
  "code { }" `isInfixOf` processed @?= True

tests :: TestTree
tests = testGroup "Type System Edge Cases Tests"
  [ testProperty "string transform preserve count" prop_string_transform_preserve_count
  , testProperty "split join reversible" prop_split_join_reversible
  , testProperty "comment reduces length" prop_comment_reduces_length
  , testProperty "normalize no increase lines" prop_normalize_no_increase_lines
  , testProperty "safe processing graceful" prop_safe_processing_graceful
  , testProperty "position monotonic" prop_position_monotonic
  , testProperty "span ordered positions" prop_span_ordered_positions
  , testProperty "string operations empty" prop_string_operations_empty
  , testProperty "char validation consistent" prop_char_validation_consistent
  , testCase "typesystem edge empty" test_typesystem_edge_empty
  , testCase "typesystem edge single char" test_typesystem_edge_single_char
  , testCase "typesystem edge whitespace only" test_typesystem_edge_whitespace_only
  , testCase "typesystem edge special chars" test_typesystem_edge_special_chars
  , testCase "typesystem edge long string" test_typesystem_edge_long_string
  , testCase "typesystem edge unicode" test_typesystem_edge_unicode
  , testCase "typesystem edge nested" test_typesystem_edge_nested
  , testCase "typesystem edge comments" test_typesystem_edge_comments
  , testCase "typesystem edge positions" test_typesystem_edge_positions
  , testCase "typesystem edge mixed content" test_typesystem_edge_mixed_content
  ]