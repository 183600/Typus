{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewUtilsValidationSpec (newUtilsValidationSpec, utilsQuickCheckProperties) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property(..), (==>), Positive(..))
import Utils
import Data.Char (isSpace)
import Data.List (isInfixOf)

-- | Test suite for Utils module validation functions
newUtilsValidationSpec :: TestTree
newUtilsValidationSpec = testGroup "New Utils Validation Tests"
  [ testCase "trim handles various whitespace combinations" $ do
      trim "" @?= ""
      trim "   " @?= ""
      trim "hello" @?= "hello"
      trim "  hello  " @?= "hello"
      trim "\t\n hello \n\t" @?= "hello"
      trim "  hello world  " @?= "hello world"
  
  , testCase "splitBy preserves empty segments" $ do
      splitBy ',' "" @?= [""]
      splitBy ',' "a" @?= ["a"]
      splitBy ',' "," @?= ["", ""]
      splitBy ',' "a,b" @?= ["a", "b"]
      splitBy ',' "a,,b" @?= ["a", "", "b"]
      splitBy ',' ",a," @?= ["", "a", ""]
      splitBy ',' "a,b," @?= ["a", "b", ""]
  
  , testCase "splitByCollapsed removes empty segments" $ do
      splitByCollapsed ',' "" @?= []
      splitByCollapsed ',' "a" @?= ["a"]
      splitByCollapsed ',' "," @?= []
      splitByCollapsed ',' "a,b" @?= ["a", "b"]
      splitByCollapsed ',' "a,,b" @?= ["a", "b"]
      splitByCollapsed ',' ",a," @?= ["a"]
      splitByCollapsed ',' "a,b," @?= ["a", "b"]
  
  , testCase "removeLineComments handles various cases" $ do
      removeLineComments "" @?= ""
      removeLineComments "hello" @?= "hello"
      removeLineComments "hello // comment" @?= "hello "
      removeLineComments "// comment\nhello" @?= "\nhello"
      removeLineComments "hello // comment\nworld" @?= "hello \nworld"
      removeLineComments "hello // comment // another" @?= "hello "
      removeLineComments "hello \"// not a comment\"" @?= "hello \"// not a comment\""
  
  , testCase "normalizeIndentation handles mixed indentation" $ do
      normalizeIndentation "" @?= ""
      normalizeIndentation "hello" @?= "hello"
      normalizeIndentation "  hello\n  world" @?= "hello\nworld"
      normalizeIndentation "\thello\n\tworld" @?= "hello\nworld"
      normalizeIndentation "  hello\n\tworld" @?= "hello\n\tworld"
      normalizeIndentation "    hello\n  world\n    test" @?= "  hello\nworld\n  test"
  ]

-- QuickCheck properties for Utils functions
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_trim_no_leading_trailing_whitespace :: String -> Bool
prop_trim_no_leading_trailing_whitespace s = 
  let t = trim s
  in null t || (not (isSpace (head t)) && not (isSpace (last t)))

prop_splitBy_length :: String -> Char -> Bool
prop_splitBy_length s c = 
  let parts = splitBy c s
      commas = length (filter (== c) s)
  in length parts == commas + 1

prop_splitByCollapsed_no_empty :: String -> Char -> Bool  
prop_splitByCollapsed_no_empty s c = 
  all (not . null) (splitByCollapsed c s)

prop_breakOn_consistency :: String -> String -> Bool
prop_breakOn_consistency s needle = 
  case breakOn needle s of
    (before, after) -> before ++ needle ++ after == s

-- QuickCheck test suite
utilsQuickCheckProperties :: TestTree
utilsQuickCheckProperties = testGroup "Utils QuickCheck Properties"
  [ testProperty "trim is idempotent" prop_trim_idempotent
  , testProperty "trim removes leading/trailing whitespace" prop_trim_no_leading_trailing_whitespace
  , testProperty "splitBy creates correct number of parts" prop_splitBy_length
  , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
  , testProperty "breakOn is consistent" prop_breakOn_consistency
  ]