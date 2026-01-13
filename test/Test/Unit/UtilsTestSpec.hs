{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module UtilsTestSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen)
import qualified Data.Text as T
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper)
import Data.List (sort, nub, group, isInfixOf, isPrefixOf, isSuffixOf)
import Data.String (IsString)

import Utils

-- Helper generators for utils tests
genAlphaString :: Gen String
genAlphaString = do
  len <- choose (0, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z']

genAlphaNumString :: Gen String
genAlphaNumString = do
  len <- choose (0, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genWhitespaceString :: Gen String
genWhitespaceString = do
  len <- choose (0, 10)
  vectorOf len $ elements " \t\n\r"

genMixedString :: Gen String
genMixedString = do
  alphaPart <- genAlphaString
  numPart <- vectorOf (choose (0, 5)) $ elements ['0'..'9']
  spacePart <- genWhitespaceString
  return $ alphaPart ++ numPart ++ spacePart

genNonEmptyString :: Gen String
genNonEmptyString = do
  len <- choose (1, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n"

-- Test properties for utils functions

-- Property 1: trim removes leading and trailing whitespace
prop_trimRemovesWhitespace :: String -> String -> String -> Property
prop_trimRemovesWhitespace leading middle trailing =
  not (null middle) ==> 
    let input = leading ++ middle ++ trailing
        result = trim input
        leadingWhitespace = all isSpace leading
        trailingWhitespace = all isSpace trailing
    in leadingWhitespace && trailingWhitespace ==> 
       not (null result) && not (isSpace $ head result) && not (isSpace $ last result)

-- Property 2: trim preserves non-whitespace content
prop_trimPreservesContent :: String -> Property
prop_trimPreservesContent s =
  not (null s) && not (all isSpace s) ==> 
    let result = trim s
        nonSpaceChars = filter (not . isSpace) s
        resultNonSpaceChars = filter (not . isSpace) result
    in sort nonSpaceChars == sort resultNonSpaceChars

-- Property 3: trim of all whitespace returns empty string
prop_trimAllWhitespace :: String -> Property
prop_trimAllWhitespace s =
  all isSpace s ==> trim s == ""

-- Property 4: trim of empty string returns empty string
prop_trimEmptyString :: Bool
prop_trimEmptyString = trim "" == ""

-- Property 5: splitOn preserves delimiter occurrences
prop_splitOnPreservesDelimiters :: String -> String -> String -> Property
prop_splitOnPreservesDelimiters prefix delimiter suffix =
  not (null delimiter) ==> 
    let input = prefix ++ delimiter ++ suffix
        parts = splitOn delimiter input
    in length parts >= 2 && 
       head parts == prefix && 
       last parts == suffix

-- Property 6: splitOn with non-existent delimiter returns single-element list
prop_splitOnNonExistentDelimiter :: String -> String -> Property
prop_splitOnNonExistentDelimiter s delimiter =
  not (null delimiter) && delimiter `notElem` s ==> 
    splitOn delimiter s == [s]

-- Property 7: splitOn empty delimiter splits into characters
prop_splitOnEmptyDelimiter :: String -> Property
prop_splitOnEmptyDelimiter s = splitOn "" s == map (:[]) s

-- Property 8: joinWith is inverse of splitOn for non-empty delimiter
prop_joinWithSplitOnInverse :: String -> String -> [String] -> Property
prop_joinWithSplitOnInverse delimiter parts =
  not (null delimiter) && not (null parts) ==> 
    let joined = joinWith delimiter parts
        splitParts = splitOn delimiter joined
    in splitParts == parts

utilsTests :: TestTree
utilsTests = testGroup "Utils Tests"
  [ testProperties "Trim Properties"
    [ ("trim removes leading and trailing whitespace", prop_trimRemovesWhitespace)
    , ("trim preserves non-whitespace content", prop_trimPreservesContent)
    , ("trim of all whitespace returns empty string", prop_trimAllWhitespace)
    , ("trim of empty string returns empty string", prop_trimEmptyString)
    ]
  , testProperties "Split/Join Properties"
    [ ("splitOn preserves delimiter occurrences", prop_splitOnPreservesDelimiters)
    , ("splitOn with non-existent delimiter returns single-element list", prop_splitOnNonExistentDelimiter)
    , ("splitOn empty delimiter splits into characters", prop_splitOnEmptyDelimiter)
    , ("joinWith is inverse of splitOn for non-empty delimiter", prop_joinWithSplitOnInverse)
    ]
  ]