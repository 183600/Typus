{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.UtilsStringProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

-- | Test utility string processing functions
tests :: TestTree
tests = testGroup "Utils String Processing Tests"
  [ testTrimFunctions
  , testSplitFunctions
  , testCommentRemoval
  , testIndentationNormalization
  , testBreakOnFunction
  , testEdgeCases
  ]

-- | Test trim functionality
testTrimFunctions :: TestTree
testTrimFunctions = testCase "Trim Functions" $ do
  trim "  hello  " @?= "hello"
  trim "\t\n  test  \n\t" @?= "test"
  trim "no_whitespace" @?= "no_whitespace"
  trim "" @?= ""
  trim "   " @?= ""

-- | Test split functionality
testSplitFunctions :: TestTree
testSplitFunctions = testCase "Split Functions" $ do
  -- Test splitBy (preserves empty segments)
  splitBy ',' "a,b,c" @?= ["a", "b", "c"]
  splitBy ',' "a,,b" @?= ["a", "", "b"]
  splitBy ',' ",a," @?= ["", "a", ""]
  splitBy ',' "" @?= [""]
  
  -- Test splitByCollapsed (removes empty segments)
  splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
  splitByCollapsed ',' "a,,b" @?= ["a", "b"]
  splitByCollapsed ',' ",a," @?= ["a"]
  splitByCollapsed ',' "" @?= []
  
  -- Test comma-specific functions
  splitByComma "a,b,c" @?= ["a", "b", "c"]
  splitByCommaCollapsed "a,,b" @?= ["a", "b"]

-- | Test comment removal
testCommentRemoval :: TestTree
testCommentRemoval = testCase "Comment Removal" $ do
  -- Test line comment removal
  removeLineComments "code // comment" @?= "code "
  removeLineComments "// full line comment\ncode" @?= "\ncode"
  removeLineComments "code // comment\nmore code" @?= "code \nmore code"
  
  -- Test full comment removal
  removeComments "code // comment\nmore code" @?= "code \nmore code"
  removeComments "code /* block comment */ more code" @?= "code  more code"
  removeComments "code /* multi\nline\ncomment */ more" @?= "code  more"

-- | Test indentation normalization
testIndentationNormalization :: TestTree
testIndentationNormalization = testCase "Indentation Normalization" $ do
  let input = "    line1\n        line2\n    line3"
      expected = "line1\n    line2\nline3"
  normalizeIndentation input @?= expected

-- | Test breakOn function
testBreakOnFunction :: TestTree
testBreakOnFunction = testCase "Break On Function" $ do
  breakOn "," "a,b,c" @?= ("a", "b,c")
  breakOn " " "hello world" @?= ("hello", "world")
  breakOn "x" "abc" @?= ("abc", "")

-- | Test edge cases
testEdgeCases :: TestTree
testEdgeCases = testCase "Edge Cases" $ do
  -- Test with special characters
  splitBy '|' "a|b|c" @?= ["a", "b", "c"]
  trim "\n\r\t  test \n\r\t" @?= "test"
  
  -- Test empty inputs
  splitBy ',' "" @?= [""]
  trim "" @?= ""
  
  -- Test single character inputs
  splitBy ',' "a" @?= ["a"]
  trim "a" @?= "a"