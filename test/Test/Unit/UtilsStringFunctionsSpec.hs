module Test.Unit.UtilsStringFunctionsSpec where



import Test.Tasty.HUnit
import Test.Tasty

import Utils (trim, splitBy, splitByComma, splitByCommaCollapsed)
import Data.Char (isSpace)

tests :: TestTree
tests = testGroup "Utils String Functions Tests"
  [ testCase "trim removes leading and trailing whitespace" $ do
      trim "  hello  " @?= "hello"
      trim "\t\n  hello  \n\t" @?= "hello"
      trim "hello" @?= "hello"
      trim "" @?= ""
      trim "   " @?= ""
      
  , testCase "splitBy handles basic cases" $ do
      splitBy ',' "a,b,c" @?= ["a", "b", "c"]
      splitBy ',' "a,,b" @?= ["a", "", "b"]
      splitBy ',' ",a," @?= ["", "a", ""]
      splitBy ',' "" @?= []
      splitBy ',' "," @?= ["", ""]
      splitBy ',' "a" @?= ["a"]
      
  , testCase "splitBy handles consecutive delimiters" $ do
      splitBy ',' "a,,,b" @?= ["a", "", "", "b"]
      splitBy ',' ",,," @?= ["", "", "", ""]
      splitBy ',' "a," @?= ["a", ""]
      splitBy ',' ",a" @?= ["", "a"]
      
  , testCase "splitByComma works correctly" $ do
      splitByComma "a,b,c" @?= ["a", "b", "c"]
      splitByComma "a,,b" @?= ["a", "", "b"]
      splitByComma "" @?= []
      splitByComma "," @?= ["", ""]
      
  , testCase "splitByCommaCollapsed removes empty segments" $ do
      splitByCommaCollapsed "a,b,c" @?= ["a", "b", "c"]
      splitByCommaCollapsed "a,,b" @?= ["a", "b"]
      splitByCommaCollapsed ",a," @?= ["a"]
      splitByCommaCollapsed "a,,,b" @?= ["a", "b"]
      splitByCommaCollapsed "" @?= []
      splitByCommaCollapsed "," @?= []
      splitByCommaCollapsed ",,," @?= []
      
  , testCase "splitBy with different delimiters" $ do
      splitBy ';' "a;b;c" @?= ["a", "b", "c"]
      splitBy '|' "a|b|c" @?= ["a", "b", "c"]
      splitBy ' ' "a b c" @?= ["a", "b", "c"]
      splitBy '\t' "a\tb\tc" @?= ["a", "b", "c"]
      
  , testCase "trim edge cases" $ do
      trim " " @?= ""
      trim "\n" @?= ""
      trim "\t" @?= ""
      trim "\n\t " @?= ""
      trim "a" @?= "a"
      trim " a " @?= "a"
      trim "a b c" @?= "a b c"
      trim "  a b c  " @?= "a b c"
      
  , testCase "splitBy with special characters" $ do
      splitBy '.' "a.b.c" @?= ["a", "b", "c"]
      splitBy '-' "a-b-c" @?= ["a", "b", "c"]
      splitBy '_' "a_b_c" @?= ["a", "b", "c"]
      
  , testCase "splitBy with unicode characters" $ do
      splitBy ' ' "你好 世界" @?= ["你好", "世界"]
      splitBy ',' "a,b,c,d" @?= ["a", "b", "c", "d"]
  ]