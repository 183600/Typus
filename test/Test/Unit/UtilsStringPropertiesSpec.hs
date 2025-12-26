{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.UtilsStringPropertiesSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck ((==>), Property)
import Utils (trim, splitBy, splitByComma, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- | Test string properties for Utils module
utilsStringPropertiesSpec :: TestTree
utilsStringPropertiesSpec = testGroup "Utils String Properties"
  [ testProperty "trim idempotent" prop_trim_idempotent
  , testProperty "trim removes only leading/trailing spaces" prop_trim_only_edges
  , testProperty "splitBy delimiter consistency" prop_splitBy_consistency
  , testProperty "splitByComma equals splitBy with ','" prop_splitByComma_equals_splitBy
  , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
  , testProperty "removeLineComments preserves non-comment lines" prop_removeLineComments_preserve
  , testProperty "removeComments handles nested strings correctly" prop_removeComments_strings
  , testProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentation_relative
  , testProperty "breakOn returns correct split" prop_breakOn_correct
  , testProperty "breakOn handles empty pattern" prop_breakOn_empty_pattern
  ]

-- | trim applied twice is same as trim applied once
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- | trim only removes leading/trailing whitespace, not internal
prop_trim_only_edges :: String -> String -> Property
prop_trim_only_edges prefix suffix =
  let s = prefix ++ " middle content " ++ suffix
      trimmed = trim s
      hasInternalSpaces = " middle content " `isInfixOf` trimmed
  in not (null prefix && null suffix) ==> hasInternalSpaces
  where
    infix 4 `isInfixOf`
    [] `isInfixOf` _ = False
    (_:_) `isInfixOf` [] = False
    needle `isInfixOf` haystack = any (isPrefixOf needle) (tails haystack)
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

-- | splitBy consistency: joining result gives original when delimiter not in segments
prop_splitBy_consistency :: Char -> String -> Property
prop_splitBy_consistency delim s =
  not (delim `elem` s) ==> splitBy delim s === [s]

-- | splitByComma should equal splitBy with comma delimiter
prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy s = splitByComma s === splitBy ',' s

-- | splitByCollapsed should never return empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Bool
prop_splitByCollapsed_no_empty delim s = all (not . null) (splitByCollapsed delim s)

-- | removeLineComments should preserve lines without comments
prop_removeLineComments_preserve :: String -> Property
prop_removeLineComments_preserve s =
  let linesWithoutComments = filter (not . ("//" `isPrefixOf`)) (lines s)
      result = removeLineComments s
      resultLines = lines result
  in not (null linesWithoutComments) ==> length resultLines >= length linesWithoutComments
  where
    infix 4 `isPrefixOf`
    [] `isPrefixOf` _ = False
    (_:_) `isPrefixOf` [] = False
    needle `isPrefixOf` haystack = take (length needle) haystack === needle

-- | removeComments should preserve string literals
prop_removeComments_strings :: String -> Property
prop_removeComments_strings s =
  let stringWithLiteral = "code \"string with // comment\" more code"
      result = removeComments stringWithLiteral
  in "// comment" `isInfixOf` result
  where
    infix 4 `isInfixOf`
    [] `isInfixOf` _ = False
    (_:_) `isInfixOf` [] = False
    needle `isInfixOf` haystack = any (isPrefixOf needle) (tails haystack)
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

-- | normalizeIndentation should preserve relative indentation differences
prop_normalizeIndentation_relative :: String -> String -> Property
prop_normalizeIndentation_relative line1 line2 =
  let indented1 = "  " ++ line1
      indented2 = "    " ++ line2
      input = indented1 ++ "\n" ++ indented2 ++ "\n"
      result = normalizeIndentation input
      resultLines = lines result
  in length resultLines >= 2 ==> 
     let indent1 = length (takeWhile isSpace (head resultLines))
         indent2 = length (takeWhile isSpace (resultLines !! 1))
     in indent2 >= indent1

-- | breakOn should correctly split at first occurrence
prop_breakOn_correct :: String -> String -> Property
prop_breakOn_correct pat s =
  not (null pat) && pat `isInfixOf` s ==> 
    let (before, after) = breakOn pat s
        combined = before ++ pat ++ after
    in combined === s
  where
    infix 4 `isInfixOf`
    [] `isInfixOf` _ = False
    (_:_) `isInfixOf` [] = False
    needle `isInfixOf` haystack = any (isPrefixOf needle) (tails haystack)
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

-- | breakOn with empty pattern should return ("", string)
prop_breakOn_empty_pattern :: String -> Bool
prop_breakOn_empty_pattern s = breakOn "" s === ("", s)

-- Helper for equality in QuickCheck
(===) :: Eq a => a -> a -> Bool
(===) = (==)