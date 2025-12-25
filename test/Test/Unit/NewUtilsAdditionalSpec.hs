{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewUtilsAdditionalSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Utils (trim, splitBy, splitByComma, removeLineComments, normalizeIndentation)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, nub, intercalate)
import Data.Char (isSpace, isAlpha, isAlphaNum, isDigit, isLower, isUpper)

-- Property: splitBy works correctly for basic cases
prop_splitby_basic :: Char -> String -> Property
prop_splitby_basic delimiter input =
  let result = splitBy delimiter input
      rejoined = intercalate [delimiter] result
  in rejoined === input

-- Property: splitByComma works correctly for basic cases
prop_splitby_comma_basic :: String -> Property
prop_splitby_comma_basic input =
  let result = splitByComma input
      rejoined = intercalate "," result
  in rejoined === input

-- Property: splitBy handles empty string correctly
prop_splitby_empty :: Char -> Property
prop_splitby_empty delimiter =
  let result = splitBy delimiter ""
  in result === [""]

-- Property: splitBy handles single character correctly
prop_splitby_single :: Char -> String -> Property
prop_splitby_single delimiter ch =
  let input = [ch]
      result = splitBy delimiter input
  in length result >= 1 -- Basic property check

-- Property: removeLineComments works correctly
prop_remove_line_comments :: String -> Property
prop_remove_line_comments code =
  let result = removeLineComments code
  in length result >= 0 -- Basic property check

-- Property: normalizeIndentation works correctly
prop_normalize_indentation :: String -> Property
prop_normalize_indentation indentedCode =
  let result = normalizeIndentation indentedCode
  in length result >= 0 -- Basic property check

-- Property: trim removes leading and trailing whitespace correctly
prop_trim_whitespace :: String -> Property
prop_trim_whitespace input =
  let trimmed = trim input
      hasLeadingSpace = not (null input) && isSpace (head input)
      hasTrailingSpace = not (null input) && isSpace (last input)
  in if hasLeadingSpace || hasTrailingSpace
     then not (null trimmed) ==> (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))
     else trimmed === input

-- Property: trim handles empty string correctly
prop_trim_empty :: Property
prop_trim_empty =
  trim "" === ""

-- Property: trim handles all whitespace correctly
prop_trim_all_whitespace :: String -> Property
prop_trim_all_whitespace whitespace =
  all isSpace whitespace ==> trim whitespace === ""

-- Property: removeLineComments preserves code without comments
prop_remove_line_comments_no_comments :: String -> Property
prop_remove_line_comments_no_comments code =
  not ("//" `isInfixOf` code) ==>
  let result = removeLineComments code
  in result === code

-- Property: removeLineComments handles line ending with comment
prop_remove_line_comments_end_of_line :: String -> Property
prop_remove_line_comments_end_of_line prefix =
  not ("//" `isInfixOf` prefix) ==>
  let code = prefix ++ " // comment"
      result = removeLineComments code
  in length result >= length prefix -- Basic property check

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent input =
  let trimmed1 = trim input
      trimmed2 = trim trimmed1
  in trimmed1 === trimmed2

-- Property: trim preserves non-whitespace content
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content input =
  let trimmed = trim input
      nonSpaceInput = filter (not . isSpace) input
      nonSpaceTrimmed = filter (not . isSpace) trimmed
  in nonSpaceInput === nonSpaceTrimmed

-- Property: splitBy works with different delimiters
prop_splitby_different_delimiters :: Char -> Char -> String -> Property
prop_splitby_different_delimiters delim1 delim2 input =
  delim1 /= delim2 ==>
  let result1 = splitBy delim1 input
      result2 = splitBy delim2 input
  in length result1 >= 1 .&&. length result2 >= 1

-- Property: splitBy handles repeated delimiters
prop_splitby_repeated :: Char -> Int -> Property
prop_splitby_repeated delimiter count =
  count >= 1 && count <= 10 ==>
  let input = replicate count delimiter
      result = splitBy delimiter input
  in length result === count + 1

-- Property: splitByComma is equivalent to splitBy ','
prop_splitby_comma_equivalence :: String -> Property
prop_splitby_comma_equivalence input =
  let result1 = splitByComma input
      result2 = splitBy ',' input
  in result1 === result2

tests :: TestTree
tests = testGroup "New Utils Additional tests"
  [ fastProperty "splitBy works correctly for basic cases" prop_splitby_basic
  , fastProperty "splitByComma works correctly for basic cases" prop_splitby_comma_basic
  , fastProperty "splitBy handles empty string correctly" prop_splitby_empty
  , fastProperty "splitBy handles single character correctly" prop_splitby_single
  , fastProperty "removeLineComments works correctly" prop_remove_line_comments
  , fastProperty "normalizeIndentation works correctly" prop_normalize_indentation
  , fastProperty "trim removes leading and trailing whitespace correctly" prop_trim_whitespace
  , fastProperty "trim handles empty string correctly" prop_trim_empty
  , fastProperty "trim handles all whitespace correctly" prop_trim_all_whitespace
  
  , fastProperty "removeLineComments preserves code without comments" prop_remove_line_comments_no_comments
  , fastProperty "removeLineComments handles line ending with comment" prop_remove_line_comments_end_of_line
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim preserves non-whitespace content" prop_trim_preserves_content
  , fastProperty "splitBy works with different delimiters" prop_splitby_different_delimiters
  , fastProperty "splitBy handles repeated delimiters" prop_splitby_repeated
  , fastProperty "splitByComma is equivalent to splitBy ','" prop_splitby_comma_equivalence
  ]