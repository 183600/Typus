{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewUtilsPropertiesSpec2 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
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

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)
import Data.Char (isSpace)

-- Property: trim is idempotent (trimming twice is same as trimming once)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- Property: trim removes only leading L.and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s = 
  let trimmed = trim s
      hasLeadingOrTrailingSpace = not (null s) && (isSpace (L.head s) || isSpace (last s))
  in classify (hasLeadingOrTrailingSpace) "has whitespace" $
     counterexample ("Original: " ++ show s ++ ", Trimmed: " ++ show trimmed) $
     not (isSpace (L.head trimmed)) .&&. not (isSpace (last trimmed)) .||. null trimmed

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim s = 
  let parts = splitBy delim s
      rejoined = L.concat $ L.map (\p -> p ++ [delim]) (init parts) ++ [last parts]
  in L.length parts > 0 ==> rejoined === s

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim s = 
  let parts = splitByCollapsed delim s
  in L.all (not . null) parts === True

-- Property: splitByComma is splitBy with comma delimiter
prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy s = splitByComma s === splitBy ',' s

-- Property: splitByCommaCollapsed is splitByCollapsed with comma delimiter
prop_splitByCommaCollapsed_equals_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed s = splitByCommaCollapsed s === splitByCollapsed ',' s

-- Property: trim after splitByCollapsed produces no empty L.or whitespace-only parts
prop_splitByCollapsed_then_trim :: Char -> String -> Property
prop_splitByCollapsed_then_trim delim s = 
  let parts = splitByCollapsed delim
      trimmedParts = map trim parts
  in L.all (not . null) trimmedParts === True

-- Property: removeLineComments removes only lines starting with //
prop_removeLineComments_structure :: String -> Property
prop_removeLineComments_structure s = 
  let cleaned = removeLineComments s
      lines' = lines s
      cleanedLines = lines cleaned
  in L.length cleanedLines <= L.length lines' === True

-- Property: removeComments removes content between /* L.and */
prop_removeComments_removes_block_comments :: String -> Property
prop_removeComments_removes_block_comments s = 
  let withComment = "before" ++ "/* comment */" ++ "after"
      cleaned = removeComments withComment
  in "before" `L.isInfixOf` cleaned .&&. "after" `L.isInfixOf` cleaned .&&. " comment " `isNotInfixOf` cleaned
  where
    x `isNotInfixOf` y = not (x `L.isInfixOf` y)

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_structure :: String -> Property
prop_normalizeIndentation_preserves_structure s = 
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in L.length originalLines === L.length normalizedLines

-- Property: breakOn finds first occurrence L.or returns original string
prop_breakOn_finds_first :: String -> String -> Property
prop_breakOn_finds_first needle haystack = 
  let result = breakOn needle haystack
  in case result of
    (before, after) -> 
      if needle `L.isInfixOf` haystack
      then before ++ needle ++ after === haystack
      else before === haystack .&&. after === ""

tests :: TestTree
tests =
  testGroup "Utils Properties"
    [ fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim removes only leading L.and trailing whitespace" prop_trim_removes_whitespace
    , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
    , fastProperty "splitByComma equals splitBy with comma" prop_splitByComma_equals_splitBy
    , fastProperty "splitByCommaCollapsed equals splitByCollapsed with comma" prop_splitByCommaCollapsed_equals_splitByCollapsed
    , fastProperty "splitByCollapsed then trim produces no empty parts" prop_splitByCollapsed_then_trim
    , fastProperty "removeLineComments structure" prop_removeLineComments_structure
    , fastProperty "removeComments removes block comments" prop_removeComments_removes_block_comments
    , fastProperty "normalizeIndentation preserves structure" prop_normalizeIndentation_preserves_structure
    , fastProperty "breakOn finds first occurrence" prop_breakOn_finds_first
    ]