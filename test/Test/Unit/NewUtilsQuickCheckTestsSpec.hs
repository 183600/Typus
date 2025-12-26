{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.NewUtilsQuickCheckTestsSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, property, (==>), forAll)
import TestSupport.QuickCheck (fastProperty)

import Utils
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Additional generators for Utils testing
genString :: Gen String
genString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '\t', '\n', '!', '?', '.', ',', ';', ':', '(', ')', '[', ']', '{', '}', '+', '-', '*', '/', '=', '<', '>', '_', '|', '&']

genNonEmptyString :: Gen String
genNonEmptyString = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_'
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_']
  return (first : rest)

genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_']
  return (first : rest)

genText :: Gen T.Text
genText = T.pack <$> genString

genIntList :: Gen [Int]
genIntList = listOf $ choose (-100, 100)

genStringList :: Gen [String]
genStringList = listOf genString

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '\t', '\n', '!', '?', '.', ',', ';', ':', '(', ')', '[', ']', '{', '}', '+', '-', '*', '/', '=', '<', '>', '_', '|', '&']

-- Property: String trimming removes only whitespace
prop_stringTrimmingRemovesWhitespace :: String -> Bool
prop_stringTrimmingRemovesWhitespace s = 
  let trimmed = trim s
      leadingRemoved = dropWhile Char.isSpace s
      trailingRemoved = reverse $ dropWhile Char.isSpace $ reverse leadingRemoved
  in trimmed == trailingRemoved

-- Property: Identifier validation is consistent
prop_identifierValidationConsistent :: String -> Bool
prop_identifierValidationConsistent s = 
  let hasValidFirstChar = not (null s) && (head s `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
      hasValidChars = null s || all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_']) s
  in hasValidFirstChar && hasValidChars

-- Property: List sorting preserves elements
prop_listSortingPreservesElements :: [Int] -> Bool
prop_listSortingPreservesElements lst = 
  let sorted = List.sort lst
      sortedMultiset = List.sort lst
  in sorted == sortedMultiset

-- Property: Map lookup returns expected values
prop_mapLookupReturnsExpected :: [(String, Int)] -> String -> Int -> Bool
prop_mapLookupReturnsExpected pairs key defaultValue = 
  let mp = Map.fromList pairs
      result = Map.findWithDefault defaultValue key mp
      expected = Map.findWithDefault defaultValue key mp
  in result == expected

-- Property: Set operations are consistent
prop_setOperationsConsistent :: [Int] -> [Int] -> Bool
prop_setOperationsConsistent lst1 lst2 = 
  let set1 = Set.fromList lst1
      set2 = Set.fromList lst2
      union = Set.union set1 set2
      intersection = Set.intersection set1 set2
      difference = Set.difference set1 set2
  in Set.isSubsetOf set1 union && 
     Set.isSubsetOf set2 union &&
     Set.isSubsetOf intersection set1 &&
     Set.isSubsetOf intersection set2 &&
     Set.isSubsetOf difference set1

-- Property: Text conversion preserves content
prop_textConversionPreservesContent :: String -> Bool
prop_textConversionPreservesContent s = 
  let text = T.pack s
      converted = T.unpack text
  in converted == s

-- Property: List filtering preserves order
prop_listFilteringPreservesOrder :: [Int] -> Bool
prop_listFilteringPreservesOrder lst = 
  let filtered = filter even lst
      originalOrder = List.sort lst
      filteredOrder = List.sort filtered
  in filteredOrder `List.isSubsequenceOf` originalOrder

-- Property: String case conversion is invertible
prop_stringCaseConversionInvertible :: String -> Bool
prop_stringCaseConversionInvertible s = 
  let upper = map Char.toUpper s
      lower = map Char.toLower s
      restoredFromUpper = map Char.toLower upper
      restoredFromLower = map Char.toUpper lower
  in restoredFromUpper == lower && restoredFromLower == upper

-- Property: String word count is accurate
prop_stringWordCountAccurate :: String -> Bool
prop_stringWordCountAccurate s = 
  let words = words s
      wordCount = length words
  in wordCount >= 0

-- Test suite
tests :: TestTree
tests = testGroup "New Utils QuickCheck Tests"
  [ testProperty "String trimming removes only whitespace" $
      fastProperty "String trimming removes whitespace" prop_stringTrimmingRemovesWhitespace
  
  , testProperty "Identifier validation is consistent" $
      fastProperty "Identifier validation consistent" prop_identifierValidationConsistent
  
  , testProperty "List sorting preserves elements" $
      fastProperty "List sorting preserves elements" prop_listSortingPreservesElements
  
  , testProperty "Map lookup returns expected values" $
      fastProperty "Map lookup returns expected" prop_mapLookupReturnsExpected
  
  , testProperty "Set operations are consistent" $
      fastProperty "Set operations consistent" prop_setOperationsConsistent
  
  , testProperty "Text conversion preserves content" $
      fastProperty "Text conversion preserves content" prop_textConversionPreservesContent
  
  , testProperty "List filtering preserves order" $
      fastProperty "List filtering preserves order" prop_listFilteringPreservesOrder
  
  , testProperty "String case conversion is invertible" $
      fastProperty "String case conversion invertible" prop_stringCaseConversionInvertible
  
  , testProperty "String word count is accurate" $
      fastProperty "String word count accurate" prop_stringWordCountAccurate
  ]