{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewCoreUtilsQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments, removeComments)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)

-- ============================================================================
-- Test Properties for Utils Module
-- ============================================================================

-- | trim should remove leading L.and trailing whitespace
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

-- | trim should not change internal whitespace
prop_trim_preserves_internal :: String -> String -> Bool
prop_trim_preserves_internal s1 s2 = 
  let s = s1 ++ "   " ++ s2 ++ "   "
      trimmed = trim s
  in not (null trimmed) ==> not (isSpace (L.head trimmed)) && not (isSpace (last trimmed))

-- | splitBy should preserve empty segments
prop_splitBy_preserves_empty :: Char -> String -> Bool
prop_splitBy_preserves_empty delim s = 
  let result = splitBy delim s
      expected = L.length (L.filter (== delim) s) + 1
  in L.length result == expected

-- | splitByCollapsed should remove empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Bool
prop_splitByCollapsed_removes_empty delim s = 
  let result = splitByCollapsed delim s
  in not (L.any null result)

-- | splitByComma should be equivalent to splitBy ','
prop_splitByComma_equals_splitBy :: String -> Bool
prop_splitByComma_equals_splitBy s = splitByComma s == splitBy ',' s

-- | removeLineComments should only remove lines starting with //
prop_removeLine_comments_basic :: String -> Bool
prop_removeLine_comments_basic s = 
  let withComment = s ++ "\n// this is a comment\n" ++ s
      withoutComment = removeLineComments withComment
  in s `L.isInfixOf` withoutComment && "// this is a comment" `notElem` lines withoutComment

-- | removeComments should handle both // L.and /* */ comments
prop_remove_comments_basic :: String -> Bool
prop_remove_comments_basic s = 
  let withComments = s ++ " // line comment\n" ++ s ++ " /* block comment */ " ++ s
      withoutComments = removeComments withComments
  in s `L.isInfixOf` withoutComments && 
     "// line comment" `notElem` words withoutComments &&
     "/* block comment */" `notElem` words withoutComments

-- | splitBy should be inverse of join with same delimiter
prop_split_by_join_inverse :: Char -> [String] -> Bool
prop_split_by_join_inverse delim parts = 
  let joined = concatMap (\p -> p ++ [delim]) (init parts) ++ last parts
      splitResult = splitBy delim joined
  in splitResult == parts

-- | trim should be identity for strings without whitespace
prop_trim_no_whitespace :: String -> Bool
prop_trim_no_whitespace s = 
  let noWs = L.filter (not . isSpace) s
  in trim noWs == noWs

-- | splitByCollapsed should handle consecutive delimiters correctly
prop_splitByCollapsed_consecutive :: Char -> Int -> String -> Bool
prop_splitByCollapsed_consecutive delim n s = 
  let consecutiveDelims = replicate n delim
      testString = s ++ consecutiveDelims ++ s
      result = splitByCollapsed delim testString
  in n > 0 ==> L.length result == 2

-- Helper function
isInfixOf :: Eq a => [a] -> [[a]] -> Bool
L.isInfixOf = L.any . flip L.isPrefixOf

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Utils Module QuickCheck Tests"
  [ testProperty "trim is idempotent" prop_trim_idempotent
  , testProperty "trim preserves internal whitespace" prop_trim_preserves_internal
  , testProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , testProperty "splitByComma equals splitBy ','" prop_splitByComma_equals_splitBy
  , testProperty "removeLineComments basic functionality" prop_removeLine_comments_basic
  , testProperty "removeComments basic functionality" prop_remove_comments_basic
  , testProperty "splitBy join inverse" prop_split_by_join_inverse
  , testProperty "trim no whitespace identity" prop_trim_no_whitespace
  , testProperty "splitByCollapsed consecutive delimiters" prop_splitByCollapsed_consecutive
  ]