{-# LANGUAGE TemplateHaskell #-}

-- | Core QuickCheck property tests for Utils module
module Test.Unit.UtilsCorePropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Property (property)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)

-- ============================================================================
-- Test Properties
-- ============================================================================

-- | trim should not change strings without leading/trailing whitespace
prop_trim_no_change :: String -> Property
prop_trim_no_change s = 
  let trimmed = trim s
      hasNoLeadingTrailing = null s || 
                             not (isSpace (L.head s)) && 
                             not (isSpace (last s))
  in if hasNoLeadingTrailing
     then trimmed === s
     else property True

-- | trim should be idempotent: trimming twice gives same result as once
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- | splitBy should be consistent with string L.length
prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length delim s = 
  let parts = splitBy delim s
      joinedLength = L.length s + L.length (L.filter (== delim) s)
      partsLength = L.sum (map L.length parts)
  in partsLength === L.length s

-- | splitByCollapsed should never produce empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s = 
  let parts = splitByCollapsed delim s
  in L.all (not . null) parts === True

-- | splitByComma should be equivalent to splitBy ','
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency s = 
  splitByComma s === splitBy ',' s

-- | removeLineComments should preserve content before comment marker
prop_removeLine_comments_preserve_prefix :: String -> String -> Property
prop_removeLine_comments_preserve_prefix prefix comment = 
  let input = prefix ++ "//" ++ comment
      result = removeLineComments input
  in prefix === result

-- | removeLineComments should handle strings without comments
prop_remove_line_comments_no_change :: String -> Property
prop_remove_line_comments_no_change s = 
  not ("//" `L.isPrefixOf` s) ==> removeLineComments s === s

-- | trim should handle empty strings gracefully
prop_trim_empty :: Property
prop_trim_empty = trim "" === ""

-- | splitBy should handle empty strings
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty delim = splitBy delim "" === [""]

-- | splitByCollapsed should handle empty strings
prop_splitByCollapsed_empty :: Char -> Property
prop_splitByCollapsed_empty delim = splitByCollapsed delim "" === []

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Utils Core Properties QuickCheck Tests"
  [ testProperty "trim: no change on non-whitespace strings" prop_trim_no_change
  , testProperty "trim: idempotent property" prop_trim_idempotent
  , testProperty "splitBy: L.length consistency" prop_splitBy_length
  , testProperty "splitByCollapsed: no empty strings" prop_splitByCollapsed_no_empty
  , testProperty "splitByComma: consistency with splitBy ','" prop_splitByComma_consistency
  , testProperty "removeLineComments: preserve prefix" prop_remove_line_comments_preserve_prefix
  , testProperty "removeLineComments: no change without comments" prop_remove_line_comments_no_change
  , testProperty "trim: empty string handling" prop_trim_empty
  , testProperty "splitBy: empty string handling" prop_splitBy_empty
  , testProperty "splitByCollapsed: empty string handling" prop_splitByCollapsed_empty
  ]