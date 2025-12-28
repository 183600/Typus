{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTypusUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Utils (trim, splitBy, normalizeIndentation, removeLineComments, fixIndentation)

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let content = prefix ++ "content" ++ suffix
      trimmed = trim content
      hasLeading = any isSpace prefix
      hasTrailing = any isSpace suffix
      noLeadingSpace = null trimmed || not (isSpace (head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

-- Property: splitBy preserves delimiter count
prop_splitBy_preserves_delimiter_count :: String -> String -> Property
prop_splitBy_preserves_delimiter_count content delimiter =
  let parts = splitBy content delimiter
      expectedCount = if null content then 1 else countOccurrences content delimiter + 1
      actualCount = length parts
  in classify (not (null delimiter)) "non-empty delimiter" $
     property $ actualCount === expectedCount

-- Property: normalizeIndentation preserves content structure
prop_normalize_indentation_preserves_structure :: String -> Property
prop_normalize_indentation_preserves_structure input =
  let lines = splitBy input "\n"
      normalized = normalizeIndentation input
      normalizedLines = splitBy normalized "\n"
      sameLineCount = length lines == length normalizedLines
  in classify (length lines > 1) "multiple lines" $
     property $ sameLineCount

-- Property: removeLineComments preserves code structure
prop_remove_comments_preserves_structure :: String -> String -> Property
prop_remove_comments_preserves_structure code comments =
  let codeWithComments = code ++ "\n// " ++ comments
      withoutComments = removeLineComments codeWithComments
      codeLines = length $ splitBy code "\n"
      commentLines = length $ splitBy comments "\n"
      resultLines = length $ splitBy withoutComments "\n"
  in classify (not (null comments)) "has comments" $
     property $ resultLines <= codeLines + commentLines

-- Property: fixIndentation handles mixed indentation
prop_fix_indentation_handles_mixed :: String -> Property
prop_fix_indentation_handles_mixed input =
  let mixedInput = addMixedIndentation input
      fixed = fixIndentation mixedInput
      hasConsistentTabs = not (any (\c -> c == ' ') fixed) || not (any (\c -> c == '\t') fixed)
  in classify (length input > 0) "non-empty input" $
     property $ hasConsistentTabs

-- Helper functions
isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

countOccurrences :: String -> String -> Int
countOccurrences _ [] = 0
countOccurrences str delim = if delim `isPrefixOf` str 
                            then 1 + countOccurrences (drop (length delim) str) delim
                            else countOccurrences (tail str) delim

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

addMixedIndentation :: String -> String
addMixedIndentation = unlines . map addIndent . splitBy "\n"
  where
    addIndent line = "  \t" ++ line

tests :: TestTree
tests = testGroup "New Typus Utils QuickCheck Tests"
  [ fastProperty "Trim removes whitespace" prop_trim_removes_whitespace
  , fastProperty "SplitBy preserves delimiter count" prop_splitBy_preserves_delimiter_count
  , fastProperty "Normalize indentation preserves structure" prop_normalize_indentation_preserves_structure
  , fastProperty "Remove comments preserves structure" prop_remove_comments_preserves_structure
  , fastProperty "Fix indentation handles mixed indentation" prop_fix_indentation_handles_mixed
  ]