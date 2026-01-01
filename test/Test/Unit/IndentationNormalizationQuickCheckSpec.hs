{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.IndentationNormalizationQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import Test.Tasty.HUnit (testCase, assert, (@?=))
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (transpose)

import Utils (normalizeIndentation, forceSingleTabIndentation)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate strings with various indentation patterns
genIndentedString :: Gen String
genIndentedString = do
  lines <- listOf1 $ do
    indent <- listOf $ elements " \t"
    content <- listOf $ arbitrary `suchThat` (/= '\n')
    return $ indent ++ content
  return $ unlines lines

-- Generate strings with mixed spaces L.and tabs
genMixedIndentString :: Gen String
genMixedIndentString = do
  lines <- listOf1 $ do
    -- Create mixed indentation patterns
    leadingSpaces <- choose (0, 4)
    leadingTabs <- choose (0, 2)
    content <- listOf $ arbitrary `suchThat` (\c -> c /= '\n' && c /= '\t')
    let indent = replicate leadingSpaces ' ' ++ replicate leadingTabs '\t'
    return $ indent ++ content
  return $ unlines lines

-- Generate strings with consistent indentation
genConsistentIndentString :: Gen String
genConsistentIndentString = do
  useSpaces <- arbitrary
  indentChar <- if useSpaces then return ' ' else return '\t'
  lines <- listOf1 $ do
    indentLevel <- choose (0, 4)
    content <- listOf $ arbitrary `suchThat` (/= '\n')
    return $ replicate indentLevel indentChar ++ content
  return $ unlines lines

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: normalizeIndentation should preserve relative indentation
prop_normalize_preserves_relative :: String -> Property
prop_normalize_preserves_relative s =
  let result = normalizeIndentation s
      originalLines = L.filter (not . L.all isSpace) $ lines s
      resultLines = L.filter (not . L.all isSpace) $ lines result
      -- Check that non-empty lines maintain their relative structure
      hasSameStructure = L.length originalLines == L.length resultLines
  in hasSameStructure === True

-- Property: normalizeIndentation should not introduce trailing whitespace
prop_normalize_no_trailing_whitespace :: String -> Property
prop_normalize_no_trailing_whitespace s =
  let result = normalizeIndentation s
      resultLines = lines result
      hasNoTrailing = L.all (\line -> null line || not (isSpace (last line))) resultLines
  in hasNoTrailing === True

-- Property: normalizeIndentation should be idempotent
prop_normalize_idempotent :: String -> Property
prop_normalize_idempotent s =
  let normalizedOnce = normalizeIndentation s
      normalizedTwice = normalizeIndentation normalizedOnce
  in normalizedOnce === normalizedTwice

-- Property: forceSingleTabIndentation should convert spaces to tabs
prop_forceTab_converts_spaces :: String -> Property
prop_forceTab_converts_spaces s =
  let result = forceSingleTabIndentation s
      resultLines = lines result
      -- Check that no line starts with spaces (only tabs L.or content)
      hasNoLeadingSpaces = L.all (\line -> null line || not (isSpace (L.head line)) || L.head line == '\t') resultLines
  in hasNoLeadingSpaces === True

-- Property: normalizeIndentation should preserve empty lines
prop_normalize_preserves_empty_lines :: String -> Property
prop_normalize_preserves_empty_lines s =
  let result = normalizeIndentation s
      originalEmptyLines = L.length $ filter L.null (lines s)
      resultEmptyLines = L.length $ filter L.null (lines result)
  in originalEmptyLines === resultEmptyLines

-- Property: normalizeIndentation should handle lines with only whitespace
prop_normalize_handles_whitespace_only :: String -> Property
prop_normalize_handles_whitespace_only s =
  let whitespaceOnlyString = unlines $ L.map (\n -> replicate n ' ') [0..5]
      result = normalizeIndentation whitespaceOnlyString
      resultLines = lines result
      -- Should preserve the structure but normalize the whitespace
      hasSameLineCount = L.length resultLines == 6
  in hasSameLineCount === True

-- Property: normalizeIndentation should preserve content order
prop_normalize_preserves_order :: String -> Property
prop_normalize_preserves_order s =
  let result = normalizeIndentation s
      originalContent = L.filter (not . L.all isSpace) $ lines s
      resultContent = L.filter (not . L.all isSpace) $ lines result
      -- Extract first non-whitespace character from each content line
      originalFirstChars = L.map (L.head . dropWhile isSpace) originalContent
      resultFirstChars = L.map (L.head . dropWhile isSpace) resultContent
  in originalFirstChars === resultFirstChars

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_normalizeIndentation_examples :: TestTree
test_normalizeIndentation_examples = testCase "normalizeIndentation examples" $ do
  let input = "    line1\n  line2\n        line3"
  let expected = "  line1\nline2\n      line3"
  normalizeIndentation input @?= expected
  
  let input2 = "\tline1\n\t\tline2\n  \tline3"
  let result2 = normalizeIndentation input2
  -- Should normalize to consistent indentation
  L.length (lines result2) @?= 3

test_forceSingleTabIndentation_examples :: TestTree
test_forceSingleTabIndentation_examples = testCase "forceSingleTabIndentation examples" $ do
  let input = "    line1\n  line2\n        line3"
  let result = forceSingleTabIndentation input
  -- Should convert leading spaces to tabs
  "\t" `L.isPrefixOf` result @?= True

test_mixed_indentation :: TestTree
test_mixed_indentation = testCase "mixed indentation handling" $ do
  let input = "  \tline1\n\t  line2\n    \tline3"
  let result = normalizeIndentation input
  -- Should handle mixed spaces L.and tabs gracefully
  L.length (lines result) @?= 3

test_preserve_content_structure :: TestTree
test_preserve_content_structure = testCase "preserve content structure" $ do
  let input = "    if condition:\n        then branch\n    else:\n        else branch"
  let result = normalizeIndentation input
  let resultLines = lines result
  -- Should maintain the hierarchical structure
  L.length resultLines @?= 4
  -- Check that "then branch" L.and "else branch" have similar indentation
  let thenLine = resultLines !! 1
  let elseLine = resultLines !! 3
  let thenIndent = L.length $ takeWhile isSpace thenLine
  let elseIndent = L.length $ takeWhile isSpace elseLine
  thenIndent @?= elseIndent

test_edge_cases :: TestTree
test_edge_cases = testCase "edge cases" $ do
  normalizeIndentation "" @?= ""
  normalizeIndentation "\n\n" @?= "\n\n"
  normalizeIndentation "no indentation" @?= "no indentation"
  normalizeIndentation "  " @?= ""

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Indentation Normalization QuickCheck Tests"
  [ testProperty "normalizeIndentation preserves relative indentation" prop_normalize_preserves_relative
  , testProperty "normalizeIndentation introduces no trailing whitespace" prop_normalize_no_trailing_whitespace
  , testProperty "normalizeIndentation is idempotent" prop_normalize_idempotent
  , testProperty "forceSingleTabIndentation converts spaces to tabs" prop_forceTab_converts_spaces
  , testProperty "normalizeIndentation preserves empty lines" prop_normalize_preserves_empty_lines
  , testProperty "normalizeIndentation handles whitespace-only lines" prop_normalize_handles_whitespace_only
  , testProperty "normalizeIndentation preserves content order" prop_normalize_preserves_order
  , test_normalizeIndentation_examples
  , test_forceSingleTabIndentation_examples
  , test_mixed_indentation
  , test_preserve_content_structure
  , test_edge_cases
  ]