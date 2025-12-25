{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as Data.List (isInfixOf)
import Data.List (intersperse)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeComments
  , normalizeIndentation
  , breakOn
  , removeLineComments
  )

-- Property: breakOn is consistent with Data.Text.breakOn
prop_breakOn_consistent_with_text :: String -> String -> Property
prop_breakOn_consistent_with_text s pat =
  not (null pat) ==> 
  let (before, after) = breakOn pat s
      textBefore = T.unpack $ fst $ T.breakOn (T.pack pat) (T.pack s)
      textAfter = case T.stripPrefix (T.pack pat) (T.pack $ drop (length before + length pat) s) of
                   Just t -> T.unpack t
                   Nothing -> ""
  in before === textBefore .&&. after === textAfter

-- Property: breakOn with pattern not in string returns original string
prop_breakOn_pattern_not_found :: String -> String -> Property
prop_breakOn_pattern_not_found s pat =
  not (pat `Data.List.isInfixOf` s) ==> 
  breakOn pat s === (s, "")

-- Property: breakOn with empty pattern returns empty prefix
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern s =
  breakOn "" s === ("", s)

-- Property: splitBy length relationship
prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length delim s =
  let parts = splitBy delim s
      expectedCount = length (filter (== delim) s) + 1
  in length parts === expectedCount

-- Property: splitByCollapsed never has empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

-- Property: splitBy followed by join with delimiter reconstructs original (for splitBy)
prop_splitBy_join_reconstruct :: Char -> String -> Property
prop_splitBy_join_reconstruct delim s =
  let parts = splitBy delim s
  in concat (intersperse [delim] parts) === s
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x ++ sep ++ intersperse sep xs

-- Property: normalizeIndentation preserves non-empty line count
prop_normalizeIndentation_preserves_line_count :: String -> Property
prop_normalizeIndentation_preserves_line_count s =
  let lines' = lines s
      nonEmptyCount = length $ filter (not . all isSpace) lines'
      normalizedLines = lines $ normalizeIndentation s
      normalizedNonEmptyCount = length $ filter (not . all isSpace) normalizedLines
  in nonEmptyCount === normalizedNonEmptyCount

-- Property: normalizeIndentation doesn't change relative indentation
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative s =
  let originalLines = filter (not . all isSpace) $ lines s
      normalizedLines = filter (not . all isSpace) $ lines $ normalizeIndentation s
      originalIndents = map (length . takeWhile isSpace) originalLines
      normalizedIndents = map (length . takeWhile isSpace) normalizedLines
      minOriginal = if null originalIndents then 0 else minimum originalIndents
      minNormalized = if null normalizedIndents then 0 else minimum normalizedIndents
      adjustedOriginal = map (\i -> i - minOriginal) originalIndents
      adjustedNormalized = map (\i -> i - minNormalized) normalizedIndents
  in length originalLines === length normalizedLines .&&. 
     adjustedOriginal === adjustedNormalized

-- Property: removeComments preserves string literals
prop_removeComments_preserves_strings :: String -> String -> Property
prop_removeComments_preserves_strings prefix suffix =
  let strContent = "test"
      fullString = prefix ++ "\"" ++ strContent ++ "\"" ++ suffix
      result = removeComments fullString
      hasString = ("\"" ++ strContent ++ "\"") `Data.List.isInfixOf` result
  in classify (not (null prefix)) "has prefix" $
     classify (not (null suffix)) "has suffix" $
     property hasString

-- Property: removeComments preserves character literals
prop_removeComments_preserves_chars :: String -> String -> Property
prop_removeComments_preserves_chars prefix suffix =
  let charContent = 'x'
      fullString = prefix ++ "'" ++ [charContent] ++ "'" ++ suffix
      result = removeComments fullString
      hasChar = ("'" ++ [charContent] ++ "'") `Data.List.isInfixOf` result
  in classify (not (null prefix)) "has prefix" $
     classify (not (null suffix)) "has suffix" $
     property hasChar

-- Property: trim applied twice is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in trimmedOnce === trimmedTwice

-- Property: trim result has no leading/trailing whitespace
prop_trim_no_leading_trailing :: String -> Property
prop_trim_no_leading_trailing s =
  let trimmed = trim s
      hasLeading = not (null trimmed) && isSpace (head trimmed)
      hasTrailing = not (null trimmed) && isSpace (last trimmed)
  in property $ not hasLeading .&&. not hasTrailing

-- Property: removeLineComments preserves newlines
prop_removeLineComments_preserves_newlines :: String -> Property
prop_removeLineComments_preserves_newlines s =
  let originalLines = length $ lines s
      resultLines = length $ lines $ removeLineComments s
  in originalLines === resultLines

tests :: TestTree
tests =
  testGroup "Additional Utils QuickCheck tests"
    [ fastProperty "breakOn consistent with Text.breakOn" prop_breakOn_consistent_with_text
    , fastProperty "breakOn pattern not found" prop_breakOn_pattern_not_found
    , fastProperty "breakOn empty pattern" prop_breakOn_empty_pattern
    , fastProperty "splitBy length relationship" prop_splitBy_length
    , fastProperty "splitByCollapsed has no empty strings" prop_splitByCollapsed_no_empty
    , fastProperty "splitBy join reconstructs original" prop_splitBy_join_reconstruct
    , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_line_count
    , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
    , fastProperty "removeComments preserves strings" prop_removeComments_preserves_strings
    , fastProperty "removeComments preserves chars" prop_removeComments_preserves_chars
    , fastProperty "trim idempotent" prop_trim_idempotent
    , fastProperty "trim no leading/trailing whitespace" prop_trim_no_leading_trailing
    , fastProperty "removeLineComments preserves newlines" prop_removeLineComments_preserves_newlines
    ]