{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CabalUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Utils (trim, splitBy, splitByComma, removeLineComments, removeComments, normalizeIndentation, breakOn)

tests :: TestTree
tests = testGroup "Cabal Utils Tests"
  [ trimTests
  , splitTests
  , commentTests
  , indentationTests
  , searchTests
  ]

-- | Test trim function
trimTests :: TestTree
trimTests = testGroup "trim function tests"
  [ testCase "trims whitespace from both ends" $
      assertEqual "trim should remove leading and trailing spaces"
        "hello" (trim "  hello  ")
  
  , testCase "handles empty string" $
      assertEqual "trim should handle empty string"
        "" (trim "")
  
  , testCase "handles string with only whitespace" $
      assertEqual "trim should handle whitespace-only string"
        "" (trim "   \t\n   ")
  
  , testCase "preserves internal whitespace" $
      assertEqual "trim should preserve internal spaces"
        "hello world" (trim "  hello world  ")
  ]

-- | Test split functions
splitTests :: TestTree
splitTests = testGroup "split function tests"
  [ testCase "splitBy preserves empty segments" $
      assertEqual "splitBy should preserve empty segments"
        ["a", "", "b"] (splitBy ',' "a,,b")
  
  , testCase "splitBy handles leading and trailing delimiters" $
      assertEqual "splitBy should handle edge delimiters"
        ["", "a", ""] (splitBy ',' ",a,")
  
  , testCase "splitBy handles empty string" $
      assertEqual "splitBy should return single empty segment for empty input"
        [""] (splitBy ',' "")
  
  , testCase "splitByComma works correctly" $
      assertEqual "splitByComma should split on commas"
        ["apple", "banana", "cherry"] (splitByComma "apple,banana,cherry")
  
  , testCase "splitBy handles multi-character delimiters correctly" $
      assertEqual "splitBy should work with any character"
        ["a", "b", "c"] (splitBy '.' "a.b.c")
  ]

-- | Test comment removal functions
commentTests :: TestTree
commentTests = testGroup "comment removal tests"
  [ testCase "removeLineComments removes line comments" $
      assertEqual "should remove // comments"
        "hello world\n" (removeLineComments "hello world // comment\n")
  
  , testCase "removeLineComments preserves comments in strings" $
      assertEqual "should preserve // in string literals"
        "hello \"// not a comment\" world\n" 
        (removeLineComments "hello \"// not a comment\" world // real comment\n")
  
  , testCase "removeComments removes both line and block comments" $
      assertEqual "should remove both comment types"
        "hello world\n" 
        (removeComments "hello world // line comment\n/* block comment */")
  
  , testCase "removeComments preserves comments in strings" $
      assertEqual "should preserve comments in string literals"
        "hello \"// not comment\" world \"/* not block */\"\n"
        (removeComments "hello \"// not comment\" world \"/* not block */\" // real comment\n")
  
  , testCase "removeComments handles multiline block comments" $
      assertEqual "should handle multiline block comments"
        "hello\nworld\n"
        (removeComments "hello\n/* block\ncomment\n*/world\n")
  ]

-- | Test indentation functions
indentationTests :: TestTree
indentationTests = testGroup "indentation tests"
  [ testCase "normalizeIndentation removes common prefix" $
      assertEqual "should remove common indentation"
        "foo\n  bar\n" (normalizeIndentation "    foo\n      bar\n")
  
  , testCase "normalizeIndentation handles mixed indentation" $
      assertEqual "should handle mixed spaces and tabs"
        "foo\nbar\n" (normalizeIndentation "\tfoo\n  bar\n")
  
  , testCase "normalizeIndentation preserves relative indentation" $
      assertEqual "should preserve relative structure"
        "foo\n  bar\n    baz\n" 
        (normalizeIndentation "  foo\n    bar\n      baz\n")
  
  , testCase "normalizeIndentation handles empty lines" $
      assertEqual "should handle empty lines correctly"
        "foo\n\n  bar\n" 
        (normalizeIndentation "  foo\n\n    bar\n")
  ]

-- | Test search functions
searchTests :: TestTree
searchTests = testGroup "search function tests"
  [ testCase "breakOn finds substring" $
      assertEqual "should split at first occurrence"
        ("he", "o") (breakOn "ll" "hello")
  
  , testCase "breakOn handles missing substring" $
      assertEqual "should return original string when not found"
        ("hello", "") (breakOn "xyz" "hello")
  
  , testCase "breakOn handles empty pattern" $
      assertEqual "should handle empty pattern"
        ("", "hello") (breakOn "" "hello")
  
  , testCase "breakOn handles pattern at start" $
      assertEqual "should handle pattern at beginning"
        ("", "hello") (breakOn "h" "hello")
  
  , testCase "breakOn handles pattern at end" $
      assertEqual "should handle pattern at end"
        ("hell", "") (breakOn "o" "hello")
  ]