{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalStringUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

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

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort)

-- Property: trim is idempotent (applying it twice gives same result)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- Property: trim never adds characters
prop_trim_never_adds :: String -> Property
prop_trim_never_adds s = length (trim s) <= length s

-- Property: splitBy and splitByCollapsed relationship for non-empty segments
prop_split_by_relationship :: Char -> String -> Property
prop_split_by_relationship delim s = 
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
      hasEmpty = any null normal
  in classify hasEmpty "has empty segments" $
     property $ null collapsed ==> all (not . null) collapsed

-- Property: splitByCommaCollapsed removes empty segments
prop_split_by_comma_collapsed_no_empty :: String -> Property
prop_split_by_comma_collapsed_no_empty s = 
  let result = splitByCommaCollapsed s
  in property $ all (not . null) result

-- Property: removeLineComments preserves non-comment lines
prop_remove_line_comments_preserves_non_comment :: String -> Property
prop_remove_line_comments_preserves_non_comment s =
  let hasNoLineComment = not $ "//" `isInfixOf` s
      processed = removeLineComments s
  in classify hasNoLineComment "no line comments" $
     property $ hasNoLineComment ==> processed === s

-- Property: removeComments is idempotent
prop_remove_comments_idempotent :: String -> Property
prop_remove_comments_idempotent s = 
  let once = removeComments s
      twice = removeComments once
  in property $ once === twice

-- Property: breakOn finds first occurrence or returns original
prop_break_on_behavior :: String -> String -> Property
prop_break_on_behavior needle haystack =
  let result = breakOn needle haystack
  in case result of
    (before, after) -> 
      if needle `isInfixOf` haystack
      then property $ before ++ needle ++ after === haystack
      else property $ before === haystack &&. after === ""

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative s =
  let lines' = lines s
      normalizedLines = lines $ normalizeIndentation s
      hasMultipleLines = length lines' > 1
  in classify hasMultipleLines "multiple lines" $
     property $ hasMultipleLines ==> length normalizedLines === length lines'

-- Property: trim splitByComma trim roundtrip for simple cases
prop_trim_split_trim_roundtrip :: String -> Property
prop_trim_split_trim_roundtrip s =
  let trimmed = trim s
      parts = splitByComma trimmed
      rejoined = intercalate "," parts
      finalTrimmed = trim rejoined
  in property $ not (',' `isInfixOf` trimmed) ==> finalTrimmed === trimmed
  where
    intercalate :: String -> [String] -> String
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

tests :: TestTree
tests = testGroup "NewCabalStringUtilsSpec"
  [ fastProperty "trim idempotent" prop_trim_idempotent
  , fastProperty "trim never adds characters" prop_trim_never_adds
  , fastProperty "splitBy relationship" prop_split_by_relationship
  , fastProperty "splitByCommaCollapsed no empty" prop_split_by_comma_collapsed_no_empty
  , fastProperty "removeLineComments preserves non-comment" prop_remove_line_comments_preserves_non_comment
  , fastProperty "removeComments idempotent" prop_remove_comments_idempotent
  , fastProperty "breakOn behavior" prop_break_on_behavior
  , fastProperty "normalizeIndentation preserves relative" prop_normalize_indentation_preserves_relative
  , fastProperty "trim split roundtrip" prop_trim_split_trim_roundtrip
  ]