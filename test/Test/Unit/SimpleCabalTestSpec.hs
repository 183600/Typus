{-# LANGUAGE CPP #-}

module Test.Unit.SimpleCabalTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), property, (.&&.))

import Utils
  ( trim
  , splitBy
  , removeComments
  )

import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

-- ============================================================================
-- Simple Utils Tests
-- ============================================================================

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let content = prefix ++ "content" ++ suffix
      trimmed = trim content
      hasLeading = any isSpace prefix
      hasTrailing = any isSpace suffix
      noLeadingSpace = null trimmed || not (isSpace (head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in property $ noLeadingSpace .&&. noTrailingSpace

-- Property: splitBy preserves empty segments
prop_splitby_preserves_empty :: Char -> String -> Property
prop_splitby_preserves_empty delim str =
  let result = splitBy delim str
      expectedLength = length (filter (== delim) str) + 1
  in property $ length result === expectedLength

-- Unit test: trim works with basic whitespace
test_trim_basic :: TestTree
test_trim_basic = testCase "trim works with basic whitespace" $ do
    trim "  hello world  " @?= "hello world"
    trim "\t\n  test  \n\t" @?= "test"
    trim "" @?= ""
    trim "   " @?= ""

-- Unit test: splitBy works with comma
test_splitby_comma :: TestTree
test_splitby_comma = testCase "splitBy works with comma" $ do
    splitBy ',' "a,b,c" @?= ["a", "b", "c"]
    splitBy ',' "a,,b" @?= ["a", "", "b"]
    splitBy ',' ",a," @?= ["", "a", ""]
    splitBy ',' "" @?= [""]

-- Unit test: removeComments handles line comments
test_remove_line_comments :: TestTree
test_remove_line_comments = testCase "removeComments handles line comments" $ do
    let input = "code // comment\nmore code // another comment"
        expected = "code \nmore code "
        result = removeComments input
    result @?= expected

-- Unit test: removeComments handles block comments
test_remove_block_comments :: TestTree
test_remove_block_comments = testCase "removeComments handles block comments" $ do
    let input = "code /* block comment */ more code"
        expected = "code  more code"
        result = removeComments input
    result @?= expected

-- Unit test: removeComments handles nested comments
test_remove_nested_comments :: TestTree
test_remove_nested_comments = testCase "removeComments handles nested comments" $ do
    let input = "code /* outer /* inner */ still outer */ more code"
        expected = "code  more code"
        result = removeComments input
    result @?= expected

-- Property: removeComments preserves non-comment content
prop_remove_comments_preserves_content :: String -> Property
prop_remove_comments_preserves_content code =
  let noComments = not (isInfixOf "//" code || isInfixOf "/*" code)
  in noComments ==> 
     let result = removeComments code
     in result === code

-- Unit test: integration of trim and removeComments
test_trim_remove_comments_integration :: TestTree
test_trim_remove_comments_integration = testCase "trim and removeComments integration" $ do
    let input = "  code // comment  \n  /* block */  "
        afterComments = removeComments input
        final = trim afterComments
        expected = "code"
    final @?= expected

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Simple Cabal Tests"
  [ testGroup "Utils Properties"
    [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
    , fastProperty "splitBy preserves empty segments" prop_splitby_preserves_empty
    , fastProperty "removeComments preserves non-comment content" prop_remove_comments_preserves_content
    ]
  , testGroup "Utils Unit Tests"
    [ test_trim_basic
    , test_splitby_comma
    , test_remove_line_comments
    , test_remove_block_comments
    , test_remove_nested_comments
    , test_trim_remove_comments_integration
    ]
  ]