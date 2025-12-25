{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Unit.StringSplittingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, oneof, elements)
import Utils (splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed)
import Data.Char (isSpace)

tests :: TestTree
tests = testGroup "String Splitting Tests"
  [ testGroup "splitBy basic functionality"
    [ testCase "splits simple string" $
        splitBy ',' "a,b,c" @?= ["a", "b", "c"]
    , testCase "preserves empty segments" $
        splitBy ',' "a,,b" @?= ["a", "", "b"]
    , testCase "handles leading delimiter" $
        splitBy ',' ",a,b" @?= ["", "a", "b"]
    , testCase "handles trailing delimiter" $
        splitBy ',' "a,b," @?= ["a", "b", ""]
    , testCase "handles only delimiters" $
        splitBy ',' ",," @?= ["", "", ""]
    ]
  , testCase "handles empty string" $
        splitBy ',' "" @?= [""]
  , testCase "handles string without delimiter" $
        splitBy ',' "abc" @?= ["abc"]
  , testCase "handles string with only delimiter" $
        splitBy ',' "," @?= ["", ""]
  , testCase "handles consecutive delimiters" $
        splitBy ',' "a,,,b" @?= ["a", "", "", "b"]
  , testGroup "splitByComma basic functionality"
    [ testCase "splits on comma" $
        splitByComma "a,b,c" @?= ["a", "b", "c"]
    , testCase "splits comma with empty segments" $
        splitByComma "a,,b" @?= ["a", "", "b"]
    , testCase "handles comma at boundaries" $
        splitByComma ",a,b," @?= ["", "a", "b", ""]
    ]
  , testGroup "splitByCommaCollapsed basic functionality"
    [ testCase "splits comma and collapses" $
        splitByCommaCollapsed "a,,b" @?= ["a", "b"]
    , testCase "handles leading/trailing commas" $
        splitByCommaCollapsed ",a,b," @?= ["a", "b"]
    , testCase "handles only commas" $
        splitByCommaCollapsed ",,," @?= []
    , testCase "handles empty string" $
        splitByCommaCollapsed "" @?= []
    ]
  , testGroup "splitBy with different delimiters"
    [ testCase "splits on space" $
        splitBy ' ' "hello world test" @?= ["hello", "world", "test"]
    , testCase "splits on tab" $
        splitBy '\t' "col1\tcol2\tcol3" @?= ["col1", "col2", "col3"]
    , testCase "splits on newline" $
        splitBy '\n' "line1\nline2\nline3" @?= ["line1", "line2", "line3"]
    , testCase "splits on semicolon" $
        splitBy ';' "item1;item2;item3" @?= ["item1", "item2", "item3"]
    ]
  , testGroup "splitBy real-world examples"
    [ testCase "splits CSV-like data" $
        splitBy ',' "field1,field2,field3,field4" @?= ["field1", "field2", "field3", "field4"]
    , testCase "splits CSV with empty fields" $
        splitBy ',' "field1,,field3," @?= ["field1", "", "field3", ""]
    , testCase "splits path components" $
        splitBy '/' "/home/user/docs" @?= ["", "home", "user", "docs"]
    , testCase "splits dot-separated identifiers" $
        splitBy '.' "package.module.function" @?= ["package", "module", "function"]
    ]
  , testGroup "splitBy special cases"
    [ testCase "splits mixed delimiters" $
        splitBy ':' "key:value:description" @?= ["key", "value", "description"]
    , testCase "splits on special character" $
        splitBy '|' "option1|option2|option3" @?= ["option1", "option2", "option3"]
    , testCase "splits on equals sign" $
        splitBy '=' "name=value" @?= ["name", "value"]
    ]
  , testGroup "Property tests"
    [ testProperty "splitBy preserves total content" $
        \c s -> concat (splitBy c s) == s
    , testProperty "splitBy length property" $
        \c s -> length (splitBy c s) == length (filter (== c) s) + 1
    , testProperty "splitBy is inverse of intercalate" $
        \c s -> splitBy c (unwords (splitBy c s)) == splitBy c s
    , testProperty "splitByCollapsed removes empty segments" $
        \c s -> all (not . null) (splitByCollapsed c s)
    , testProperty "splitByCollapsed length <= splitBy length" $
        \c s -> length (splitByCollapsed c s) <= length (splitBy c s)
    , testProperty "splitByComma = splitBy ','" $
        \s -> splitByComma s == splitBy ',' s
    , testProperty "splitByCommaCollapsed = splitByCollapsed ','" $
        \s -> splitByCommaCollapsed s == splitByCollapsed ',' s
    , testProperty "splitBy on character not in string returns single element" $
        \c s -> not (c `elem` s) ==> splitBy c s == [s]
    , testProperty "splitByCollapsed on character not in string returns empty list" $
        \c s -> not (c `elem` s) ==> splitByCollapsed c s == (if null s then [] else [s])
    ]
  , testGroup "Performance and edge cases"
    [ testCase "splits very long string" $
        let longString = replicate 1000 'a' ++ "," ++ replicate 1000 'b'
        in splitBy ',' longString @?= [replicate 1000 'a', replicate 1000 'b']
    , testCase "splits string with many delimiters" $
        let manyDelimiters = replicate 50 ','
        in splitBy ',' manyDelimiters @?= replicate 51 ""
    , testCase "splits complex mixed content" $
        let complex = "a,b,,c,   ,d\n,e"
        in splitBy ',' complex @?= ["a", "b", "", "c", "   ", "d\n", "e"]
    ]
  ]

-- Helper functions
countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (== x)

-- Generators for specific test cases
genStringWithDelimiter :: Char -> Gen String
genStringWithDelimiter delim = do
  parts <- listOf $ listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  return $ intercalate [delim] parts

genStringWithRepeatedDelimiter :: Char -> Gen String
genStringWithRepeatedDelimiter delim = do
  parts <- listOf $ listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  repeats <- listOf $ choose (1, 3)
  let delimiters = map (replicate (head repeats)) [delim]
  return $ intercalate (head delimiters) parts

-- Helper function
intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Note: Arbitrary instance for String is provided by QuickCheck