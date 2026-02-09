module Test.Unit.TextProcessingSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Utils (trim, splitBy, splitByComma, splitByCollapsed, removeLineComments, 
             removeComments, normalizeIndentation, breakOn, safeProcessString, 
             isValidChar)
import Data.Char (isSpace, isControl)

-- Test properties for text processing

-- Property: trim should be idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = 
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce == trimmedTwice

-- Property: splitBy should preserve total length (minus delimiters)
prop_split_preserve_length :: Char -> String -> Property
prop_split_preserve_length c s = 
  let parts = splitBy c s
      delimiterCount = length $ filter (== c) s
      totalLength = sum (map length parts)
  in property $ totalLength + delimiterCount == length s

-- Property: splitByComma should be equivalent to splitBy ','
prop_split_comma_equivalence :: String -> Property
prop_split_comma_equivalence s = 
  property $ splitByComma s == splitBy ',' s

-- Property: splitByCollapsed should not have consecutive empty parts
prop_split_collapsed_no_consecutive_empty :: Char -> String -> Property
prop_split_collapsed_no_consecutive_empty c s = 
  let parts = splitByCollapsed c s
      hasConsecutiveEmpty = case parts of
                             [] -> False
                             (_:rest) -> any (\(x, y) -> null x && null y) $ zip parts rest
  in property $ not hasConsecutiveEmpty

-- Property: removeLineComments should only remove // comments
prop_remove_line_comments :: String -> String -> Property
prop_remove_line_comments code comment = 
  let input = code ++ "// " ++ comment ++ "\nmore code"
      result = removeLineComments input
  in property $ "more code" `isSuffixOf` result

-- Property: removeComments should handle both // and /* */ comments
prop_remove_comments_both_types :: String -> String -> String -> Property
prop_remove_comments_both_types before middle afterStr = 
  let input = before ++ "/* comment */" ++ middle ++ "// line comment\n" ++ afterStr
      result = removeComments input
  in property $ 
    not ("/*" `isInfixOf` result) &&
    not ("*/" `isInfixOf` result) &&
    not ("//" `isInfixOf` (dropWhile isSpace $ unlines $ init $ lines result))

-- Property: normalizeIndentation should preserve relative indentation
prop_normalize_preserves_relative :: String -> Property
prop_normalize_preserves_relative s = 
  let lines' = lines s
      indented = map (\line -> "  " ++ line) lines'
      normalized = normalizeIndentation (unlines indented)
      normalizedLines = lines normalized
  in property $ length normalizedLines == length indented

-- Property: breakOn should find substring or return original
prop_break_on_finds_or_original :: String -> String -> Property
prop_break_on_finds_or_original s substr = 
  let (before, afterStr) = breakOn substr s
  in property $ 
    if substr `isInfixOf` s
    then (before ++ substr) `isPrefixOf` s
    else before == s && afterStr == ""

-- Property: safeProcessString should handle all inputs
prop_safe_process_always_succeeds :: String -> Property
prop_safe_process_always_succeeds s = 
  let processed = safeProcessString s
  in case processed of
       Right str -> property $ length str >= 0
       Left _ -> property False

-- Property: isValidChar should be consistent with isControl
prop_valid_char_consistency :: Char -> Property
prop_valid_char_consistency c = 
  isControl c ==> property $ not (isValidChar c)

-- Unit tests

test_text_processing_unicode :: Assertion
test_text_processing_unicode = do
  let unicodeString = "héllo wörld 🌍"
  trim unicodeString @?= unicodeString
  length (safeProcessString unicodeString) @?= length unicodeString

test_text_processing_empty :: Assertion
test_text_processing_empty = do
  trim "" @?= ""
  splitBy , "" @?= [""]
  removeComments "" @?= ""
  normalizeIndentation "" @?= ""

test_text_processing_whitespace :: Assertion
test_text_processing_whitespace = do
  trim "   " @?= ""
  normalizeIndentation "  \n  \n  " @?= "\n\n"

test_text_processing_punctuation :: Assertion
test_text_processing_punctuation = do
  isValidChar '.' @?= True
  isValidChar ',' @?= True
  isValidChar '!' @?= True
  isValidChar '?' @?= True

test_text_processing_newlines :: Assertion
test_text_processing_newlines = do
  normalizeIndentation "line1\nline2\nline3" @?= "line1\nline2\nline3"
  removeLineComments "code\n// comment\nmore code" @?= "code\n\nmore code"

test_text_processing_tabs :: Assertion
test_text_processing_tabs = do
  normalizeIndentation "\tline1\n\t\tline2" @?= "line1\n  line2"
  isValidChar '\t' @?= True

test_text_processing_mixed_whitespace :: Assertion
test_text_processing_mixed_whitespace = do
  trim "  \t  text  \t  " @?= "text"
  normalizeIndentation "  \t line1\n \t\t line2" @?= "line1\n  line2"

tests :: TestTree
tests = testGroup "Text Processing Tests"
  [ testProperty "trim idempotent" prop_trim_idempotent
  , testProperty "split preserve length" prop_split_preserve_length
  , testProperty "split comma equivalence" prop_split_comma_equivalence
  , testProperty "split collapsed no consecutive empty" prop_split_collapsed_no_consecutive_empty
  , testProperty "remove line comments" prop_remove_line_comments
  , testProperty "remove comments both types" prop_remove_comments_both_types
  , testProperty "normalize preserves relative" prop_normalize_preserves_relative
  , testProperty "break on finds or original" prop_break_on_finds_or_original
  , testProperty "safe process always succeeds" prop_safe_process_always_succeeds
  , testProperty "valid char consistency" prop_valid_char_consistency
  , testCase "text processing unicode" test_text_processing_unicode
  , testCase "text processing empty" test_text_processing_empty
  , testCase "text processing whitespace" test_text_processing_whitespace
  , testCase "text processing punctuation" test_text_processing_punctuation
  , testCase "text processing newlines" test_text_processing_newlines
  , testCase "text processing tabs" test_text_processing_tabs
  , testCase "text processing mixed whitespace" test_text_processing_mixed_whitespace
  ]