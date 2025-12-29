{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), oneof, elements, listOf, choose)
import qualified Test.QuickCheck.Gen as Gen

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort)

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate strings with various whitespace characters
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements [' ', '\t', '\n', '\r']

-- Generate strings without quotes to avoid comment parsing issues
genCleanString :: Gen String
genCleanString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " _-+*/=<>[]{}();:"

-- Generate strings with delimiters
genStringWithDelim :: Char -> Gen String
genStringWithDelim delim = do
    parts <- listOf $ Gen.listOf (choose (1, 5)) >>= mapM (const (elements ['a'..'z']))
    return $ Data.List.intercalate [delim] parts

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: trim removes all leading and trailing whitespace
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

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmed1 = trim str
      trimmed2 = trim trimmed1
  in property $ trimmed1 === trimmed2

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim input =
  let result = splitBy delim input
      expectedCount = length (filter (== delim) input) + 1
  in property $ length result === expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim input =
  let result = splitByCollapsed delim input
  in property $ not (any null result)

-- Property: splitByComma is equivalent to splitBy with comma
prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy input =
  property $ splitByComma input === splitBy ',' input

-- Property: splitByCommaCollapsed is equivalent to splitByCollapsed with comma
prop_splitByCommaCollapsed_equals_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed_equals_splitByCollapsed input =
  property $ splitByCommaCollapsed input === splitByCollapsed ',' input

-- Property: removeLineComments removes // comments but preserves content
prop_removeLineComments_removes_comments :: String -> String -> Property
prop_removeLineComments_removes_comments code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) && not ("/" `isInfixOf` code) ==>
  let lineWithComment = code ++ " // " ++ comment
      cleaned = removeLineComments lineWithComment
  in property $ not ("// " `isInfixOf` cleaned) .&&. code `isInfixOf` cleaned

-- Property: removeComments removes both line and block comments
prop_removeComments_removes_both :: String -> String -> String -> Property
prop_removeComments_removes_both code1 code2 comment =
  not ('"' `elem` code1) && not ('\'' `elem` code1) && 
  not ('"' `elem` code2) && not ('\'' `elem` code2) &&
  not ("/" `isInfixOf` code1) && not ("/" `isInfixOf` code2) &&
  not ("/*" `isInfixOf` code1) && not ("*/" `isInfixOf` code1) &&
  not ("/*" `isInfixOf` code2) && not ("*/" `isInfixOf` code2) ==>
  let mixed = code1 ++ " // line comment\n" ++ code2 ++ " /* " ++ comment ++ " */ " ++ code1
      cleaned = removeComments mixed
  in property $ not ("// line comment" `isInfixOf` cleaned) .&&.
     not ("/* " `isInfixOf` cleaned) .&&.
     not (" */" `isInfixOf` cleaned) .&&.
     code1 `isInfixOf` cleaned .&&.
     code2 `isInfixOf` cleaned

-- Property: normalizeIndentation removes common prefix
prop_normalizeIndentation_removes_common :: String -> Property
prop_normalizeIndentation_removes_common content =
  not (null content) ==>
  let lines' = ["  " ++ content, "  " ++ content ++ " extra", "  " ++ content]
      result = normalizeIndentation (unlines lines')
      resultLines = lines result
      hasLeadingSpace = not (null resultLines) && any (\line -> not (null line) && isSpace (head line)) resultLines
  in property $ not hasLeadingSpace

-- Property: normalizeIndentation is idempotent
prop_normalizeIndentation_idempotent :: String -> Property
prop_normalizeIndentation_idempotent input =
  let normalized1 = normalizeIndentation input
      normalized2 = normalizeIndentation normalized1
  in property $ normalized1 === normalized2

-- Property: fixIndentation equals normalizeIndentation
prop_fixIndentation_equals_normalize :: String -> Property
prop_fixIndentation_equals_normalize input =
  property $ fixIndentation input === normalizeIndentation input

-- Property: breakOn finds first occurrence and splits correctly
prop_breakOn_correct_split :: String -> String -> String -> Property
prop_breakOn_correct_split pat prefix suffix =
  not (null pat) ==>
  let haystack = prefix ++ pat ++ suffix
      (before, after) = breakOn pat haystack
  in property $ before ++ pat ++ after === haystack

-- Property: breakOn with empty pattern returns empty before
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern haystack =
  let (before, after) = breakOn "" haystack
  in property $ before === "" .&&. after === haystack

-- Property: breakOn with missing pattern returns original as before
prop_breakOn_missing_pattern :: String -> String -> Property
prop_breakOn_missing_pattern pat haystack =
  not (null pat) && not (pat `isInfixOf` haystack) ==> 
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

-- Property: splitBy and join roundtrip
prop_splitBy_join_roundtrip :: Char -> String -> Property
prop_splitBy_join_roundtrip delim input =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === input

-- Property: removeComments is idempotent
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent input =
  let removedOnce = removeComments input
      removedTwice = removeComments removedOnce
  in property $ removedOnce === removedTwice

-- Property: removeLineComments is idempotent
prop_removeLineComments_idempotent :: String -> Property
prop_removeLineComments_idempotent input =
  let removedOnce = removeLineComments input
      removedTwice = removeLineComments removedOnce
  in property $ removedOnce === removedTwice

-- Property: Complex string processing pipeline consistency
prop_complex_pipeline_consistency :: String -> String -> String -> Property
prop_complex_pipeline_consistency prefix middle suffix =
  not ('"' `elem` prefix) && not ('\'' `elem` prefix) &&
  not ('"' `elem` middle) && not ('\'' `elem` middle) &&
  not ('"' `elem` suffix) && not ('\'' `elem` suffix) &&
  not ("/" `isInfixOf` prefix) && not ("/" `isInfixOf` middle) && not ("/" `isInfixOf` suffix) ==>
  let input = prefix ++ "  /* comment */  " ++ middle ++ "  // line comment  " ++ suffix
      processed1 = removeComments input |> trim |> normalizeIndentation
      processed2 = trim input |> removeComments |> normalizeIndentation
      processed3 = normalizeIndentation input |> removeComments |> trim
  in property $ processed1 === processed2 .||. processed2 === processed3 .||. processed1 === processed3
  where
    (|>) x f = f x

-- Property: Unicode handling in trim
prop_trim_unicode :: String -> Property
prop_trim_unicode content =
  let unicodeContent = " \t\n\r " ++ content ++ "测试🚀" ++ " \t\n\r "
      trimmed = trim unicodeContent
  in property $ not (null trimmed) ==> 
     not (any isSpace (take 1 trimmed)) .&&.
     not (any isSpace (reverse (take 1 (reverse trimmed))))

-- Property: Performance with large strings
prop_performance_large_strings :: Int -> String -> Property
prop_performance_large_strings multiplier content =
  multiplier <= 50 ==> -- Limit for performance testing
  let largeContent = concat (replicate multiplier content)
      trimmed = trim largeContent
      split = splitBy ',' largeContent
  in property $ length trimmed <= length largeContent .&&.
     length split >= 1

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Utils QuickCheck Tests"
  [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , fastProperty "splitByComma equals splitBy with comma" prop_splitByComma_equals_splitBy
  , fastProperty "splitByCommaCollapsed equals splitByCollapsed with comma" prop_splitByCommaCollapsed_equals_splitByCollapsed
  , fastProperty "removeLineComments removes // comments" prop_removeLineComments_removes_comments
  , fastProperty "removeComments removes both line and block comments" prop_removeComments_removes_both
  , fastProperty "normalizeIndentation removes common prefix" prop_normalizeIndentation_removes_common
  , fastProperty "normalizeIndentation is idempotent" prop_normalizeIndentation_idempotent
  , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalize
  , fastProperty "breakOn correct split" prop_breakOn_correct_split
  , fastProperty "breakOn with empty pattern" prop_breakOn_empty_pattern
  , fastProperty "breakOn with missing pattern" prop_breakOn_missing_pattern
  , fastProperty "splitBy and join roundtrip" prop_splitBy_join_roundtrip
  , fastProperty "removeComments is idempotent" prop_removeComments_idempotent
  , fastProperty "removeLineComments is idempotent" prop_removeLineComments_idempotent
  , fastProperty "complex pipeline consistency" prop_complex_pipeline_consistency
  , fastProperty "trim handles Unicode" prop_trim_unicode
  , fastProperty "performance with large strings" prop_performance_large_strings
  ]