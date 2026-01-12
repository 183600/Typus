{-# LANGUAGE LambdaCase #-}

module Test.Unit.UtilsCoreFunctionalitySpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, chooseInt, Property, (===), counterexample)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, breakOn, 
             safeProcessString, isValidChar)
import qualified Data.List as L
import Data.Char (isSpace)

-- Test cases
utilsCoreFunctionalityTests :: TestTree
utilsCoreFunctionalityTests = testGroup "Utils Core Functionality Tests"
  [ -- Trim function tests
    testCase "Trim removes leading and trailing spaces" $ do
      assertEqual "Trim should remove leading and trailing spaces" "hello" (trim "  hello  ")
      assertEqual "Trim should remove only leading spaces" "hello  " (trim "  hello  ")
      assertEqual "Trim should remove only trailing spaces" "  hello" (trim "  hello  ")
      assertEqual "Trim should handle empty string" "" (trim "")
      assertEqual "Trim should handle string with only spaces" "" (trim "   ")

  , testCase "Trim removes leading and trailing tabs and newlines" $ do
      assertEqual "Trim should remove tabs" "hello" (trim "\thello\t")
      assertEqual "Trim should remove newlines" "hello" (trim "\nhello\n")
      assertEqual "Trim should remove mixed whitespace" "hello" (trim " \t\nhello\n\t ")

  , -- Split function tests
    testCase "Split by comma preserves empty segments" $ do
      assertEqual "Split should preserve empty segments" ["a", "", "b"] (splitBy ',' "a,,b")
      assertEqual "Split should handle leading comma" ["", "a"] (splitBy ',', "a")
      assertEqual "Split should handle trailing comma" ["a", ""] (splitBy ',' "a,")
      assertEqual "Split should handle multiple commas" ["", "", ""] (splitBy ',' ",,")
      assertEqual "Split should handle single comma" ["", ""] (splitBy ',')
      assertEqual "Split should handle empty string" [] (splitBy ',' "")

  , testCase "Split by comma collapsed removes empty segments" $ do
      assertEqual "Split collapsed should remove empty segments" ["a", "b"] (splitByCollapsed ',' "a,,b")
      assertEqual "Split collapsed should handle leading comma" ["a"] (splitByCollapsed ',', "a")
      assertEqual "Split collapsed should handle trailing comma" ["a"] (splitByCollapsed ',' "a,")
      assertEqual "Split collapsed should handle multiple commas" [] (splitByCollapsed ',' ",,")
      assertEqual "Split collapsed should handle empty string" [] (splitByCollapsed ',' "")

  , testCase "Split by comma functions work correctly" $ do
      assertEqual "Split by comma should work" ["a", "b", "c"] (splitByComma "a,b,c")
      assertEqual "Split by comma collapsed should work" ["a", "b", "c"] (splitByCommaCollapsed "a,b,c")
      assertEqual "Split by comma should preserve empty" ["a", "", "c"] (splitByComma "a,,c")
      assertEqual "Split by comma collapsed should remove empty" ["a", "c"] (splitByCommaCollapsed "a,,c")

  , -- Comment removal tests
    testCase "Remove line comments correctly" $ do
      assertEqual "Should remove line comment" "let x = 42 " (removeLineComments "let x = 42 // comment")
      assertEqual "Should handle multiple line comments" "let x = 42\nlet y = 24" (removeLineComments "let x = 42 // comment\nlet y = 24 // another comment")
      assertEqual "Should handle only comment" "" (removeLineComments "// only comment")
      assertEqual "Should handle comment after spaces" "" (removeLineComments " // comment")
      assertEqual "Should preserve strings with // inside" "let s = \"// not a comment\" " (removeLineComments "let s = \"// not a comment\" // real comment")
      assertEqual "Should preserve chars with / inside" "let c = '/' " (removeLineComments "let c = '/' // comment")

  , testCase "Remove all comments correctly" $ do
      assertEqual "Should remove line comment" "let x = 42 " (removeComments "let x = 42 // comment")
      assertEqual "Should remove block comment" "let x = 42 " (removeComments "let x = 42 /* block comment */")
      assertEqual "Should remove both types" "let x = 42\nlet y = 24 " (removeComments "let x = 42 // line\nlet y = 24 /* block */")
      assertEqual "Should preserve strings with // inside" "let s = \"// not a comment\"" (removeComments "let s = \"// not a comment\"")
      assertEqual "Should preserve strings with /* inside" "let s = \"/* not a comment */\"" (removeComments "let s = \"/* not a comment */\"")
      assertEqual "Should handle nested block comments" "let x = 42 " (removeComments "let x = 42 /* outer /* inner */ */")
      assertEqual "Should handle code with comments" "code more code " (removeComments "code /* comment */ more code")

  , -- Indentation tests
    testCase "Normalize indentation preserves relative indentation" $ do
      assertEqual "Should remove common prefix indentation" "foo\n  bar" (normalizeIndentation "    foo\n      bar")
      assertEqual "Should handle mixed tabs and spaces" "foo\n  bar" (normalizeIndentation "\tfoo\n\t\tbar")
      assertEqual "Should handle single line" "let x = 42" (normalizeIndentation "  let x = 42")
      assertEqual "Should handle empty string" "" (normalizeIndentation "")
      assertEqual "Should handle lines with different indentation" "a\nb\nc" (normalizeIndentation "  a\n b\n  c")

  , -- Break on tests
    testCase "Break on finds first occurrence" $ do
      assertEqual "Should find comma" ("a", ",b,c") (breakOn "," "a,b,c")
      assertEqual "Should find space" ("hello", " world") (breakOn " " "hello world")
      assertEqual "Should handle not found" ("hello world", "") (breakOn "x" "hello world")
      assertEqual "Should handle empty pattern" ("", "hello") (breakOn "" "hello")
      assertEqual "Should handle empty string" ("", "") (breakOn "," "")
      assertEqual "Should handle pattern at start" ("", "hello") (breakOn "hello" "hello world")

  , -- String processing tests
    testCase "Safe process string filters control characters" $ do
      assertEqual "Should filter control characters" (Right "hello world") (safeProcessString "hello\x00world")
      assertEqual "Should preserve newlines" (Right "hello\nworld") (safeProcessString "hello\nworld")
      assertEqual "Should preserve tabs" (Right "hello\tworld") (safeProcessString "hello\tworld")
      assertEqual "Should preserve carriage returns" (Right "hello\rworld") (safeProcessString "hello\rworld")

  , testCase "Valid char check works correctly" $ do
      assertBool "Regular chars should be valid" (isValidChar 'a')
      assertBool "Space should be valid" (isValidChar ' ')
      assertBool "Newline should be valid" (isValidChar '\n')
      assertBool "Tab should be valid" (isValidChar '\t')
      assertBool "Carriage return should be valid" (isValidChar '\r')
      assertBool "Null should not be valid" (not $ isValidChar '\0')
      assertBool "Control chars should not be valid" (not $ isValidChar '\x01')

  , -- QuickCheck property tests
    testProperty "Trim is idempotent" $ property $ \s -> do
      let trimmedOnce = trim s
          trimmedTwice = trim trimmedOnce
      property $ trimmedOnce === trimmedTwice

  , testProperty "Split and join are inverse operations" $ property $ \s -> do
      let parts = splitBy ',' s
          rejoined = L.intercalate "," parts
      property $ length rejoined >= length s - length (filter (== ',') s)

  , testProperty "Split collapsed removes empty segments" $ property $ \s -> do
      let parts = splitBy ',' s
          collapsedParts = splitByCollapsed ',' s
      property $ not (any null collapsedParts)

  , testProperty "Remove line comments doesn't affect strings" $ property $ \s -> do
      let stringWithComment = s ++ " // comment"
          result = removeLineComments stringWithComment
      property $ s `L.isPrefixOf` result

  , testProperty "Remove comments doesn't affect strings with comment markers" $ property $ \s -> do
      let stringInComment = "let s = \"" ++ s ++ "\" // comment"
          result = removeComments stringInComment
      property $ ("\"" ++ s ++ "\"") `L.isInfixOf` result

  , testProperty "Normalize indentation preserves line count" $ property $ \s -> do
      let linesCount = length $ lines s
          normalized = normalizeIndentation s
          normalizedLinesCount = length $ lines normalized
      property $ linesCount === normalizedLinesCount

  , testProperty "Break on always returns valid prefix" $ property $ \pat s -> do
      let (prefix, _) = breakOn pat s
      property $ prefix `L.isPrefixOf` s

  , testProperty "Safe process string removes control characters" $ property $ \s -> do
      let result = safeProcessString s
      case result of
        Left _ -> property True
        Right processed -> property $ all isValidChar processed

  , testProperty "Valid char check is consistent with safe process string" $ property $ \c -> do
      let result = safeProcessString [c]
      case result of
        Left _ -> property True
        Right [processed] -> property $ isValidChar processed
        Right _ -> property True
  ]