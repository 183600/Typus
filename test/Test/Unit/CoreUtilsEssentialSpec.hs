{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CoreUtilsEssentialSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Utils (trim, splitBy, splitByComma, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)

tests :: TestTree
tests = testGroup "Core Utils Essential Tests"
  [ testGroup "String Trimming"
    [ testCase "trim removes leading and trailing whitespace" $
        assertEqual "trim should remove spaces" "hello" (trim "  hello  ")
    
    , testCase "trim handles tabs and newlines" $
        assertEqual "trim should remove all whitespace" "hello" (trim "\t\n hello \n\t")
    
    , testCase "trim handles empty string" $
        assertEqual "trim should handle empty input" "" (trim "")
    
    , testCase "trim handles only whitespace" $
        assertEqual "trim should handle only whitespace" "" (trim "   \t\n  ")
    ]
  
  , testGroup "String Splitting"
    [ testCase "splitBy preserves empty segments" $
        assertEqual "splitBy should preserve empty segments" ["a", "", "b"] (splitBy ',' "a,,b")
    
    , testCase "splitBy handles leading and trailing delimiters" $
        assertEqual "splitBy should handle boundaries" ["", "a", ""] (splitBy ',' ",a,")
    
    , testCase "splitBy handles single element" $
        assertEqual "splitBy should handle single element" ["hello"] (splitBy ',' "hello")
    
    , testCase "splitByComma works correctly" $
        assertEqual "splitByComma should split by comma" ["x", "y", "z"] (splitByComma "x,y,z")
    
    , testCase "splitByCollapsed removes empty segments" $
        assertEqual "splitByCollapsed should remove empty segments" ["a", "b"] (splitByCollapsed ',' "a,,b")
    
    , testCase "splitByCollapsed handles all empty" $
        assertEqual "splitByCollapsed should handle all empty" [] (splitByCollapsed ',,,')
    ]
  
  , testGroup "Comment Removal"
    [ testCase "removeLineComments removes // comments" $
        assertEqual "should remove line comments" "code " (removeLineComments "code // comment")
    
    , testCase "removeLineComments preserves code before comment" $
        assertEqual "should preserve code" "int x = 5" (removeLineComments "int x = 5 // init")
    
    , testCase "removeComments removes both comment types" $
        assertEqual "should remove both comment types" "code  more" (removeComments "code // line\n/* block */ more")
    
    , testCase "removeComments handles nested block comments" $
        assertEqual "should handle nested blocks" "start  end" (removeComments "start /* outer /* inner */ */ end")
    ]
  
  , testGroup "Indentation Processing"
    [ testCase "normalizeIndentation removes common prefix" $
        assertEqual "should normalize indentation" ["a", "  b", "c"] (normalizeIndentation ["  a", "    b", "  c"])
    
    , testCase "normalizeIndentation handles mixed indentation" $
        assertEqual "should handle mixed tabs/spaces" ["a", "\tb", "c"] (normalizeIndentation ["\ta", "\t\tb", "\tc"])
    ]
  
  , testGroup "Search Operations"
    [ testCase "breakOn finds first occurrence" $
        assertEqual "should find first occurrence" ("hello", " world") (breakOn ' ' "hello world")
    
    , testCase "breakOn handles character not found" $
        assertEqual "should handle not found" ("hello", "") (breakOn 'x' "hello")
    
    , testCase "breakOn handles empty string" $
        assertEqual "should handle empty" ("", "") (breakOn 'x' "")
    ]
  ]