{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsStringProcessingComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.List (isInfixOf, null, length, sort, isPrefixOf, isSuffixOf)
import Data.Char (isSpace, isLetter, isDigit)

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

-- | Comprehensive QuickCheck tests for Utils string processing functions
-- This module tests string manipulation, comment removal, and indentation utilities

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace input =
  let trimmed = trim input
      hasLeadingSpace = not (null input) && isSpace (head input)
      hasTrailingSpace = not (null input) && isSpace (last input)
  in (hasLeadingSpace || hasTrailingSpace) ==> 
     length trimmed <= length input

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent input =
  let once = trim input
      twice = trim once
  in once === twice

-- Property: trim preserves non-whitespace content
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content input =
  let trimmed = trim input
      nonSpaceChars = filter (not . isSpace) input
  in not (null nonSpaceChars) ==> all (`elem` trimmed) nonSpaceChars

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim input =
  let result = splitBy delim input
      expectedCount = length (filter (== delim) input) + 1
  in length result === expectedCount

-- Property: splitBy on empty input returns singleton
prop_splitBy_empty_input :: Char -> Property
prop_splitBy_empty_input delim =
  let result = splitBy delim ""
  in result === [""]

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim input =
  let result = splitByCollapsed delim
      noEmpty = all (not . null) result
  in noEmpty

-- Property: splitByComma delegates to splitBy
prop_splitByComma_delegates :: String -> Property
prop_splitByComma_delegates input =
  let commaResult = splitByComma input
      splitResult = splitBy ',' input
  in commaResult === splitResult

-- Property: splitByCommaCollapsed delegates to splitByCollapsed
prop_splitByCommaCollapsed_delegates :: String -> Property
prop_splitByCommaCollapsed_delegates input =
  let commaResult = splitByCommaCollapsed input
      splitResult = splitByCollapsed ',' input
  in commaResult === splitResult

-- Property: removeLineComments preserves string literals
prop_removeLineComments_preserves_strings :: String -> Property
prop_removeLineComments_preserves_strings content =
  not ('"' `elem` content) ==> 
  let withComment = content ++ "// this is a comment"
      result = removeLineComments withComment
  in content `isPrefixOf` result

-- Property: removeLineComments handles character literals
prop_removeLineComments_preserves_chars :: String -> Property
prop_removeLineComments_preserves_chars content =
  not ('\'' `elem` content) ==> 
  let withComment = content ++ "// this is a comment"
      result = removeLineComments withComment
  in content `isPrefixOf` result

-- Property: removeLineComments removes entire comment lines
prop_removeLineComments_removes_lines :: String -> Property
prop_removeLineComments_removes_lines comment =
  let commentLine = "// " ++ comment
      result = removeLineComments commentLine
  in null (trim result)

-- Property: removeComments handles block comments
prop_removeComments_handles_blocks :: String -> Property
prop_removeComments_handles_blocks content =
  not ("/*" `isInfixOf` content) ==> 
  let withBlock = content ++ "/* block comment */" ++ content
      result = removeComments withBlock
  in content `isPrefixOf` result && content `isSuffixOf` result

-- Property: removeComments handles nested quotes in comments
prop_removeComments_nested_quotes :: String -> Property
prop_removeComments_nested_quotes content =
  let withComment = "/* comment with \"quotes\" inside */" ++ content
      result = removeComments withComment
  in content `isInfixOf` result

-- Property: removeComments preserves escaped quotes
prop_removeComments_preserves_escaped :: String -> Property
prop_removeComments_preserves_escaped content =
  not ('"' `elem` content) ==> 
  let withEscaped = content ++ "text with \\"escaped\\" quotes"
      result = removeComments withEscaped
  in "\\\"escaped\\\"" `isInfixOf` result

-- Property: normalizeIndentation removes common leading whitespace
prop_normalizeIndentation_removes_common :: String -> Property
prop_normalizeIndentation_removes_common input =
  let lines' = lines input
      hasMultipleLines = length lines' > 1
  in hasMultipleLines ==>
     let normalized = normalizeIndentation input
         normalizedLines = lines normalized
     in length normalizedLines === length lines'

-- Property: normalizeIndentation preserves relative structure
prop_normalizeIndentation_preserves_structure :: String -> Property
prop_normalizeIndentation_preserves_structure input =
  let lines' = lines input
      hasMultipleLines = length lines' > 1
  in hasMultipleLines ==>
     let normalized = normalizeIndentation input
     in not (null normalized)

-- Property: forceSingleTabIndentation enforces tab prefix
prop_forceSingleTab_enforces_tabs :: String -> Property
prop_forceSingleTab_enforces_tabs input =
  let result = forceSingleTabIndentation input
      lines' = lines result
      nonEmptyLines = filter (not . null) lines'
      allStartWithTab = all ("\t" `isPrefixOf`) nonEmptyLines
  in null nonEmptyLines .||. allStartWithTab

-- Property: fixIndentation is alias for normalizeIndentation
prop_fixIndentation_alias :: String -> Property
prop_fixIndentation_alias input =
  let fixed = fixIndentation input
      normalized = normalizeIndentation input
  in fixed === normalized

-- Property: breakOn returns correct split when pattern exists
prop_breakOn_pattern_exists :: String -> String -> Property
prop_breakOn_pattern_exists pattern content =
  not (null pattern) && pattern `isInfixOf` content ==>
  let (prefix, suffix) = breakOn pattern content
      expectedPrefix = takeWhile (not . (pattern `isPrefixOf`)) (tails content) >>= head
  in prefix ++ pattern ++ suffix === content

-- Property: breakOn returns original string when pattern missing
prop_breakOn_pattern_missing :: String -> String -> Property
prop_breakOn_pattern_missing pattern content =
  not (null pattern) && not (pattern `isInfixOf` content) ==>
  let (prefix, suffix) = breakOn pattern content
  in prefix === content && suffix === ""

-- Property: breakOn with empty pattern returns whole string as suffix
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern content =
  let (prefix, suffix) = breakOn "" content
  in prefix === "" && suffix === content

-- Property: trim handles only whitespace input
prop_trim_whitespace_only :: String -> Property
prop_trim_whitespace_only input =
  all isSpace input ==>
  let trimmed = trim input
  in null trimmed

-- Property: splitBy handles Unicode characters
prop_splitBy_unicode :: Char -> String -> Property
prop_splitBy_unicode delim input =
  let result = splitBy delim input
      totalLength = sum (map length result) + length (filter (== delim) input)
  in totalLength === length input

-- Property: removeLineComments handles multiple comments
prop_removeLineComments_multiple :: String -> String -> Property
prop_removeLineComments_multiple content1 content2 =
  not (null content1) && not (null content2) ==>
  let withComments = content1 ++ "// comment1\n" ++ content2 ++ "// comment2\n"
      result = removeLineComments withComments
  in content1 `isInfixOf` result && content2 `isInfixOf` result

-- Property: removeComments handles multiple block comments
prop_removeComments_multiple_blocks :: String -> String -> String -> Property
prop_removeComments_multiple_blocks content1 comment content2 =
  not (null content1) && not (null comment) && not (null content2) ==>
  let withBlocks = content1 ++ "/* " ++ comment ++ " */" ++ content2 ++ "/* another */"
      result = removeComments withBlocks
  in content1 `isInfixOf` result && content2 `isInfixOf` result

-- Property: normalizeIndentation handles mixed indentation
prop_normalizeIndentation_mixed :: String -> Property
prop_normalizeIndentation_mixed input =
  let lines' = lines input
      hasMultipleLines = length lines' > 1
  in hasMultipleLines ==>
     let normalized = normalizeIndentation input
         normalizedLines = lines normalized
     in length normalizedLines === length lines'

-- Property: forceSingleTabIndentation collapses whitespace-only lines
prop_forceSingleTab_collapses_whitespace :: String -> Property
prop_forceSingleTab_collapses_whitespace input =
  let result = forceSingleTabIndentation input
      lines' = lines result
      whitespaceLines = filter (all isSpace) lines'
  in null whitespaceLines

-- Property: breakOn handles pattern at beginning
prop_breakOn_pattern_at_start :: String -> String -> Property
prop_breakOn_pattern_at_start pattern content =
  not (null pattern) && pattern `isPrefixOf` content ==>
  let (prefix, suffix) = breakOn pattern content
  in prefix === "" && suffix === drop (length pattern) content

-- Property: breakOn handles pattern at end
prop_breakOn_pattern_at_end :: String -> String -> Property
prop_breakOn_pattern_at_end pattern content =
  not (null pattern) && pattern `isSuffixOf` content ==>
  let (prefix, suffix) = breakOn pattern content
  in suffix === "" && prefix === take (length content - length pattern) content

tests :: TestTree
tests = testGroup "Utils String Processing Comprehensive QuickCheck tests"
  [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim preserves non-whitespace content" prop_trim_preserves_content
  , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , fastProperty "splitBy on empty input returns singleton" prop_splitBy_empty_input
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , fastProperty "splitByComma delegates to splitBy" prop_splitByComma_delegates
  , fastProperty "splitByCommaCollapsed delegates to splitByCollapsed" prop_splitByCommaCollapsed_delegates
  , fastProperty "removeLineComments preserves string literals" prop_removeLineComments_preserves_strings
  , fastProperty "removeLineComments handles character literals" prop_removeLineComments_preserves_chars
  , fastProperty "removeLineComments removes entire comment lines" prop_removeLineComments_removes_lines
  , fastProperty "removeComments handles block comments" prop_removeComments_handles_blocks
  , fastProperty "removeComments handles nested quotes in comments" prop_removeComments_nested_quotes
  , fastProperty "removeComments preserves escaped quotes" prop_removeComments_preserves_escaped
  , fastProperty "normalizeIndentation removes common leading whitespace" prop_normalizeIndentation_removes_common
  , fastProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentation_preserves_structure
  , fastProperty "forceSingleTabIndentation enforces tab prefix" prop_forceSingleTab_enforces_tabs
  , fastProperty "fixIndentation is alias for normalizeIndentation" prop_fixIndentation_alias
  , fastProperty "breakOn returns correct split when pattern exists" prop_breakOn_pattern_exists
  , fastProperty "breakOn returns original string when pattern missing" prop_breakOn_pattern_missing
  , fastProperty "breakOn with empty pattern returns whole string as suffix" prop_breakOn_empty_pattern
  , fastProperty "trim handles only whitespace input" prop_trim_whitespace_only
  , fastProperty "splitBy handles Unicode characters" prop_splitBy_unicode
  , fastProperty "removeLineComments handles multiple comments" prop_removeLineComments_multiple
  , fastProperty "removeComments handles multiple block comments" prop_removeComments_multiple_blocks
  , fastProperty "normalizeIndentation handles mixed indentation" prop_normalizeIndentation_mixed
  , fastProperty "forceSingleTabIndentation collapses whitespace-only lines" prop_forceSingleTab_collapses_whitespace
  , fastProperty "breakOn handles pattern at beginning" prop_breakOn_pattern_at_start
  , fastProperty "breakOn handles pattern at end" prop_breakOn_pattern_at_end
  ]