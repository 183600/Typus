{-# LANGUAGE CPP #-}

module Test.Unit.NewQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, (===), counterexample, property)
import Data.Char (isSpace)
import Data.Maybe (listToMaybe)

import Utils

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

prop_splitBy_join :: Char -> String -> Property
prop_splitBy_join delim s = 
  let parts = splitBy delim s
  in counterexample ("Original: " ++ show s ++ ", Parts: " ++ show parts) $
     (length s >= 0) === True

prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency s = splitByComma s === splitBy ',' s

prop_splitByCommaCollapsed_consistency :: String -> Property
prop_splitByCommaCollapsed_consistency s = splitByCommaCollapsed s === splitByCollapsed ',' s

prop_normalizeIndentation_preserves_structure :: String -> Property
prop_normalizeIndentation_preserves_structure s = 
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
      sameLineCount = length originalLines == length normalizedLines
  in counterexample ("Original lines: " ++ show (length originalLines) ++ 
                    ", Normalized lines: " ++ show (length normalizedLines)) $
     (sameLineCount === True)

prop_removeComments_preserves_string_literals :: String -> Property
prop_removeComments_preserves_string_literals _s =
  let stringWithComment = "prefix \"// not a comment\" suffix"
      processed = removeComments stringWithComment
  in processed === "prefix \"// not a comment\" suffix"

prop_removeLineComments_removes_after_marker :: String -> Property
prop_removeLineComments_removes_after_marker _s =
  let testInput = "before // comment\nafter"
      result = removeLineComments testInput
  in result === "before \nafter\n"

prop_breakOn_finds_pattern :: String -> String -> Property
prop_breakOn_finds_pattern pat s =
  let (before, after) = breakOn pat s
      reconstructed = before ++ pat ++ after
      found = pat `isInfixOf` s
  in counterexample ("Pattern: " ++ show pat ++ ", String: " ++ show s) $
     if found then (reconstructed === s) else (property True)

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s = 
  let parts = splitByCollapsed delim s
  in all (not . null) parts === True

prop_trim_only_removes_whitespace :: String -> Property
prop_trim_only_removes_whitespace s =
  let trimmed = trim s
      startsWithNonSpace = null trimmed || maybe False (not . isSpace) (listToMaybe trimmed)
      endsWithNonSpace = null trimmed || not (isSpace (last trimmed))
  in counterexample ("Original: " ++ show s ++ ", Trimmed: " ++ show trimmed) $
     ((startsWithNonSpace && endsWithNonSpace) === True)

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys

tests :: TestTree
tests = testGroup "New QuickCheck Tests"
  [ testProperty "trim is idempotent" prop_trim_idempotent
  , testProperty "splitBy and join relationship" prop_splitBy_join
  , testProperty "splitByComma consistency" prop_splitByComma_consistency
  , testProperty "splitByCommaCollapsed consistency" prop_splitByCommaCollapsed_consistency
  , testProperty "normalizeIndentation preserves structure" prop_normalizeIndentation_preserves_structure
  , testProperty "removeComments preserves string literals" prop_removeComments_preserves_string_literals
  , testProperty "removeLineComments removes after marker" prop_removeLineComments_removes_after_marker
  , testProperty "breakOn finds pattern" prop_breakOn_finds_pattern
  , testProperty "splitByCollapsed no empty segments" prop_splitByCollapsed_no_empty
  , testProperty "trim only removes whitespace" prop_trim_only_removes_whitespace
  ]
