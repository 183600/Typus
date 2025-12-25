{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.EnhancedUtilsBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, ioProperty, (===), (.&&.), counterexample, forAll, oneof, elements, listOf1, resize)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Utils (trim, splitBy, splitByComma, removeComments, removeLineComments, normalizeIndentation, breakOn)
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import Control.Exception (evaluate, try, SomeException)

-- ============================================================================
-- Utils Boundary Cases QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils Boundary QuickCheck Tests"
  [ testProperty "trim idempotent" prop_trim_idempotent
  , testProperty "trim removes only leading/trailing whitespace" prop_trim_only_whitespace
  , testProperty "splitBy consistency with empty delimiter" prop_split_by_empty_delimiter
  , testProperty "splitBy preserves empty segments" prop_split_by_preserves_empty
  , testProperty "removeComments handles nested block comments" prop_remove_comments_nested
  , testProperty "removeComments preserves string literals" prop_remove_comments_preserves_strings
  , testProperty "removeLineComments handles edge cases" prop_remove_line_comments_edge_cases
  , testProperty "normalizeIndentation preserves relative structure" prop_normalize_indentation_relative
  , testProperty "breakOn handles empty pattern" prop_break_on_empty_pattern
  , testProperty "breakOn consistency with split" prop_break_on_consistency
  , testCase "removeComments handles malformed comments" test_remove_comments_malformed
  , testCase "normalizeIndentation handles all whitespace" test_normalize_indentation_all_whitespace
  , testCase "splitBy handles Unicode characters" test_split_by_unicode
  ]

-- ============================================================================
-- Trim Properties
-- ============================================================================

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

prop_trim_only_whitespace :: String -> Property
prop_trim_only_whitespace s = 
  let trimmed = trim s
      leadingRemoved = null s || not (isSpace (head s)) || isSpace (head trimmed)
      trailingRemoved = null trimmed || not (isSpace (last trimmed))
  in counterexample ("Original: " ++ show s ++ ", Trimmed: " ++ show trimmed) $
     leadingRemoved .&&. trailingRemoved

-- ============================================================================
-- Split Properties
-- ============================================================================

prop_split_by_empty_delimiter :: String -> Property
prop_split_by_empty_delimiter s = 
  splitBy ',' s === if null s then [""] else [s]

prop_split_by_preserves_empty :: Char -> String -> Property
prop_split_by_preserves_empty delim s = 
  let parts = splitBy delim s
      expectedCount = length s + 1
  in counterexample ("String: " ++ show s ++ ", Parts: " ++ show parts) $
     length parts === expectedCount

-- ============================================================================
-- Comment Removal Properties
-- ============================================================================

prop_remove_comments_nested :: Property
prop_remove_comments_nested = 
  forAll genNestedComments $ \s ->
    let result = removeComments s
    in counterexample ("Original: " ++ s ++ ", Result: " ++ result) $
       not ("/*" `isInfixOf` result) .&&. not ("*/" `isInfixOf` result)

prop_remove_comments_preserves_strings :: Property
prop_remove_comments_preserves_strings = 
  forAll genStringWithComments $ \s ->
    let result = removeComments s
        strings = extractStringLiterals s
    in counterexample ("Original: " ++ s ++ ", Result: " ++ result) $
       all (`isInfixOf` result) strings

prop_remove_line_comments_edge_cases :: Property
prop_remove_line_comments_edge_cases = 
  forAll genLineCommentEdgeCases $ \s ->
    let result = removeLineComments s
        lines' = lines s
        resultLines = lines result
    in counterexample ("Original: " ++ s ++ ", Result: " ++ result) $
       length resultLines === length lines'

-- ============================================================================
-- Indentation Properties
-- ============================================================================

prop_normalize_indentation_relative :: Property
prop_normalize_indentation_relative = 
  forAll genIndentedCode $ \s ->
    let result = normalizeIndentation s
        originalLines = lines s
        resultLines = lines result
    in counterexample ("Original lines: " ++ show (length originalLines) ++ 
                      ", Result lines: " ++ show (length resultLines)) $
       length originalLines === length resultLines

-- ============================================================================
-- BreakOn Properties
-- ============================================================================

prop_break_on_empty_pattern :: String -> Property
prop_break_on_empty_pattern s = 
  breakOn "" s === ("", s)

prop_break_on_consistency :: String -> String -> Property
prop_break_on_consistency s pat = 
  let (before, after) = breakOn pat s
      reconstructed = before ++ pat ++ after
  in if pat `isInfixOf` s
     then reconstructed === s
     else counterexample ("Pattern not found in string") True

-- ============================================================================
-- Specific Test Cases
-- ============================================================================

test_remove_comments_malformed :: IO ()
test_remove_comments_malformed = do
  let malformed = ["/*", "/*/", "/**/", "*/", "/***/"]
  mapM_ (\s -> do
    let result = removeComments s
    assertBool ("Should handle malformed comment: " ++ s) $ 
      not (null result) || result == "") malformed

test_normalize_indentation_all_whitespace :: IO ()
test_normalize_indentation_all_whitespace = do
  let whitespaceInputs = ["   ", "\t\t", "  \t  ", "\n\n\n", "   \n\t  \n   "]
  mapM_ (\s -> do
    let result = normalizeIndentation s
    assertBool ("Should handle all whitespace: " ++ show s) $ 
      all isSpace result || null result) whitespaceInputs

test_split_by_unicode :: IO ()
test_split_by_unicode = do
  let unicodeString = "hello,世界,🌍,test"
      parts = splitBy ',' unicodeString
      expected = ["hello", "世界", "🌍", "test"]
  assertEqual "Should handle Unicode characters" expected parts

-- ============================================================================
-- Helper Generators
-- ============================================================================

genNestedComments :: Gen String
genNestedComments = do
  depth <- elements [0..3]
  genNestedCommentDepth depth

genNestedCommentDepth :: Int -> Gen String
genNestedCommentDepth 0 = listOf1 (elements "abc")
genNestedCommentDepth n = do
  inner <- genNestedCommentDepth (n-1)
  before <- listOf1 (elements "abc")
  after <- listOf1 (elements "xyz")
  return $ before ++ "/*" ++ inner ++ "*/" ++ after

genStringWithComments :: Gen String
genStringWithComments = do
  str <- genStringLiteral
  comment <- genComment
  before <- listOf1 (elements "abc")
  after <- listOf1 (elements "xyz")
  elements [before ++ str ++ comment ++ after,
            before ++ comment ++ str ++ after,
            str ++ comment ++ after,
            before ++ str ++ after]

genStringLiteral :: Gen String
genStringLiteral = do
  content <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
  return $ "\"" ++ content ++ "\""

genComment :: Gen String
genComment = oneof [genLineComment, genBlockComment]

genLineComment :: Gen String
genLineComment = do
  content <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
  return $ "//" ++ content

genBlockComment :: Gen String
genBlockComment = do
  content <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t")
  return $ "/*" ++ content ++ "*/"

genLineCommentEdgeCases :: Gen String
genLineCommentEdgeCases = oneof
  [ return ""
  , return "//"
  , return "///"
  , return "//\n"
  , return "text // comment\nmore text"
  , return "text // comment // nested\nmore text"
  , return "\"string with // not comment\" // real comment"
  , return "'char with // not comment' // real comment"
  ]

genIndentedCode :: Gen String
genIndentedCode = do
  lines' <- listOf1 genIndentedLine
  return $ unlines lines'

genIndentedLine :: Gen String
genIndentedLine = do
  indent <- listOf1 (elements " \t")
  content <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
  return $ indent ++ content

-- ============================================================================
-- Helper Functions
-- ============================================================================

extractStringLiterals :: String -> [String]
extractStringLiterals [] = []
extractStringLiterals ('"':rest) = 
  case break (== '"') rest of
    (content, '"':remaining) -> ('"':content ++ "\"") : extractStringLiterals remaining
    (content, []) -> ['"':content] -- Unterminated string
extractStringLiterals (_:rest) = extractStringLiterals rest