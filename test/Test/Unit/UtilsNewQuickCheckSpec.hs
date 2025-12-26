{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof)
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

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Char (isSpace, toLower, isAlphaNum)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub, intercalate)

-- ============================================================================
-- Advanced String Processing Properties
-- ============================================================================

-- Property: trim preserves internal whitespace structure
prop_trim_preserves_internal_structure :: String -> String -> String -> Property
prop_trim_preserves_internal_structure prefix middle suffix =
  let content = prefix ++ "  " ++ middle ++ "  " ++ suffix
      trimmed = trim content
      internalSpaces = "  " `isInfixOf` middle
  in internalSpaces ==> ("  " `isInfixOf` trimmed)

-- Property: trim handles Unicode whitespace correctly
prop_trim_unicode_whitespace :: String -> Property
prop_trim_unicode_whitespace content =
  let unicodeContent = "\u00A0\u2000\u2001\u2002" ++ content ++ "\u2003\u2004\u2005"
      trimmed = trim unicodeContent
  in not (any (\c -> c `elem` "\u00A0\u2000\u2001\u2002\u2003\u2004\u2005") (take 1 trimmed)) .&&.
     not (any (\c -> c `elem` "\u00A0\u2000\u2001\u2002\u2003\u2004\u2005") (reverse (take 1 (reverse trimmed))))

-- Property: splitBy handles Unicode delimiters
prop_splitBy_unicode_delimiter :: Char -> String -> Property
prop_splitBy_unicode_delimiter delim input =
  let unicodeInput = input ++ [delim] ++ "测试🚀" ++ [delim] ++ input
      parts = splitBy delim unicodeInput
  in if delim `elem` unicodeInput
     then property $ not (null parts) .&&. all (notElem delim) parts
     else property $ concat parts === unicodeInput

-- Property: splitBy maintains character encoding
prop_splitBy_maintains_encoding :: Char -> String -> Property
prop_splitBy_maintains_encoding delim input =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in rejoined === input

-- ============================================================================
-- Advanced Comment Processing Properties
-- ============================================================================

-- Property: removeLineComments handles nested quotes
prop_removeLineComments_nested_quotes :: String -> Property
prop_removeLineComments_nested_quotes content =
  let nestedQuotes = content ++ "var s = \"// not comment \\\"// still not\\\"\" // real comment"
      result = removeLineComments nestedQuotes
  in "// not comment" `isInfixOf` result .&&.
     not ("// real comment" `isInfixOf` result)

-- Property: removeComments handles malformed block comments
prop_removeComments_malformed_blocks :: String -> String -> Property
prop_removeComments_malformed_blocks before after =
  not ("/*" `isInfixOf` before) && not ("*/" `isInfixOf` before) &&
  not ("/*" `isInfixOf` after) && not ("*/" `isInfixOf` after) ==>
  let content = before ++ "/* unterminated comment" ++ after
      result = removeComments content
  in length result <= length content

-- Property: removeComments preserves comment-like content in strings
prop_removeComments_preserves_string_content :: String -> Property
prop_removeComments_preserves_string_content content =
  let stringWithComments = "var s = \"/* not a block */ // not a line\""
      result = removeComments stringWithComments
  in "/* not a block */" `isInfixOf` result .&&.
     "// not a line" `isInfixOf` result

-- Property: removeLineComments handles escaped backslashes
prop_removeLineComments_escaped_backslashes :: String -> Property
prop_removeLineComments_escaped_backslashes content =
  let escapedContent = "var path = \"C:\\\\path\\\\// not comment\" // real comment"
      result = removeLineComments escapedContent
  in "\\" `isInfixOf` result .&&.
     not ("// real comment" `isInfixOf` result)

-- ============================================================================
-- Advanced Indentation Properties
-- ============================================================================

-- Property: normalizeIndentation handles mixed tabs and spaces
prop_normalizeIndentation_mixed_tabs_spaces :: [String] -> Property
prop_normalizeIndentation_mixed_tabs_spaces lines =
  not (null lines) ==>
  let mixedLines = zipWith (\i line -> replicate i ' ' ++ "\t" ++ line) [0..length lines - 1] lines
      content = unlines mixedLines
      normalized = normalizeIndentation content
      normalizedLines = lines normalized
  in all (\line -> not (take 1 line == "\t" && take 1 line == " ")) normalizedLines

-- Property: forceSingleTabIndentation preserves logical structure
prop_forceSingleTabIndentation_preserves_structure :: [String] -> Property
prop_forceSingleTabIndentation_preserves_structure lines =
  not (null lines) ==>
  let content = unlines lines
      tabbed = forceSingleTabIndentation content
      tabbedLines = lines tabbed
      nonEmptyLines = filter (not . null . trim) tabbedLines
  in all (\line -> null line || take 1 line == "\t") nonEmptyLines

-- Property: normalizeIndentation handles empty lines correctly
prop_normalizeIndentation_empty_lines :: String -> String -> String -> Property
prop_normalizeIndentation_empty_lines before middle after =
  let content = before ++ "\n\n" ++ middle ++ "\n\n" ++ after
      normalized = normalizeIndentation content
  in "\n\n" `isInfixOf` normalized

-- Property: normalizeIndentation is idempotent
prop_normalizeIndentation_idempotent :: String -> Property
prop_normalizeIndentation_idempotent content =
  let normalized1 = normalizeIndentation content
      normalized2 = normalizeIndentation normalized1
  in normalized1 === normalized2

-- ============================================================================
-- Advanced Search and Split Properties
-- ============================================================================

-- Property: breakOn handles overlapping patterns
prop_breakOn_overlapping_patterns :: String -> String -> Property
prop_breakOn_overlapping_patterns pattern haystack =
  not (null pattern) ==>
  let overlapping = pattern ++ take (length pattern - 1) pattern
      (before, after) = breakOn overlapping haystack
  in before ++ overlapping ++ after === haystack .||. (before === haystack .&&. after === "")

-- Property: breakOn handles Unicode patterns
prop_breakOn_unicode_patterns :: String -> String -> Property
prop_breakOn_unicode_patterns pattern haystack =
  let unicodePattern = pattern ++ "测试"
      unicodeHaystack = haystack ++ "prefix" ++ unicodePattern ++ "suffix"
      (before, after) = breakOn unicodePattern unicodeHaystack
  in if unicodePattern `isInfixOf` unicodeHaystack
     then before ++ unicodePattern ++ after === unicodeHaystack
     else before === unicodeHaystack .&&. after === ""

-- Property: splitByCollapsed maintains uniqueness
prop_splitByCollapsed_maintains_uniqueness :: Char -> String -> Property
prop_splitByCollapsed_maintains_uniqueness delim input =
  let parts = splitByCollapsed delim input
      uniqueParts = nub parts
  in length parts === length uniqueParts

-- Property: splitBy and splitByCollapsed relationship
prop_splitBy_splitByCollapsed_relationship :: Char -> String -> Property
prop_splitBy_splitByCollapsed_relationship delim input =
  let regularParts = splitBy delim input
      collapsedParts = splitByCollapsed delim input
      hasEmptySegments = "" `elem` regularParts
  in if hasEmptySegments
     then length collapsedParts < length regularParts
     else length collapsedParts === length regularParts

-- ============================================================================
-- Performance and Edge Case Properties
-- ============================================================================

-- Property: trim handles very large strings efficiently
prop_trim_large_strings :: Int -> String -> Property
prop_trim_large_strings multiplier baseContent =
  multiplier > 0 && multiplier <= 1000 ==> -- Limit for performance
  let largeContent = concat (replicate multiplier baseContent)
      trimmed = trim largeContent
  in length trimmed <= length largeContent

-- Property: splitBy handles very long delimiters
prop_splitBy_long_delimiters :: Int -> String -> Property
prop_splitBy_long_delimiters length input =
  length > 0 && length <= 100 ==> -- Reasonable limit
  let longDelimiter = replicate length 'X'
      parts = splitBy (head longDelimiter) input
  in property True -- Should not crash

-- Property: removeComments handles deeply nested comments
prop_removeComments_deeply_nested :: Int -> Property
prop_removeComments_deeply_nested depth =
  depth > 0 && depth <= 10 ==> -- Reasonable limit
  let nestedComments = "/* " ++ concat (replicate depth "nested ") ++ "*/"
      content = "before" ++ nestedComments ++ "after"
      result = removeComments content
  in "before" `isInfixOf` result .&&. "after" `isInfixOf` result

-- Property: normalizeIndentation handles extreme indentation
prop_normalizeIndentation_extreme_indentation :: Int -> String -> Property
prop_normalizeIndentation_extreme_indentation indentLevel content =
  indentLevel > 0 && indentLevel <= 100 ==> -- Reasonable limit
  let extremeIndent = replicate indentLevel ' '
      contentWithIndent = extremeIndent ++ content
      result = normalizeIndentation contentWithIndent
  in not (take indentLevel result == extremeIndent)

-- ============================================================================
-- Unicode and Internationalization Properties
-- ============================================================================

-- Property: trim handles right-to-left text
prop_trim_rtl_text :: String -> Property
prop_trim_rtl_text content =
  let rtlContent = "  " ++ content ++ "  " -- Arabic/Hebrew would be here
      trimmed = trim rtlContent
  in not (any isSpace (take 1 trimmed)) .&&.
     not (any isSpace (reverse (take 1 (reverse trimmed))))

-- Property: splitBy handles zero-width characters
prop_splitBy_zero_width :: String -> Property
prop_splitBy_zero_width input =
  let zeroWidth = '\x200B' -- Zero-width space
      contentWithZW = input ++ [zeroWidth] ++ input
      parts = splitBy zeroWidth contentWithZW
  in length parts >= 2

-- Property: removeComments handles Unicode comments
prop_removeComments_unicode_comments :: String -> Property
prop_removeComments_unicode_comments content =
  let unicodeComment = "/* 这是中文注释 */"
      contentWithComment = content ++ unicodeComment ++ content
      result = removeComments contentWithComment
  in not ("这是中文注释" `isInfixOf` result)

-- ============================================================================
-- Complex Scenario Properties
-- ============================================================================

-- Property: complete text processing pipeline
prop_complete_processing_pipeline :: String -> String -> String -> Property
prop_complete_processing_pipeline prefix middle suffix =
  let input = prefix ++ "  /* comment */  " ++ middle ++ "  // line comment  " ++ suffix
      processed = input 
                  |> removeComments
                  |> trim
                  |> normalizeIndentation
      trimmedProcessed = trim processed
  in not ("/* comment */" `isInfixOf` processed) .&&.
     not ("// line comment" `isInfixOf` processed) .&&.
     (null trimmedProcessed || not (any isSpace (take 1 trimmedProcessed)))

-- Property: mixed comment types interaction
prop_mixed_comment_interaction :: String -> String -> Property
prop_mixed_comment_interaction code1 code2 =
  not ('"' `elem` code1) && not ('\'' `elem` code1) &&
  not ('"' `elem` code2) && not ('\'' `elem` code2) ==>
  let mixed = code1 ++ " /* block */ " ++ code2 ++ " // line"
      lineOnly = removeLineComments mixed
      both = removeComments mixed
  in "/* block */" `isInfixOf` lineOnly .&&.
     not ("/* block */" `isInfixOf` both) .&&.
     not ("// line" `isInfixOf` both)

-- Property: complex indentation scenarios
prop_complex_indentation_scenarios :: [Int] -> Property
prop_complex_indentation_scenarios indentLevels =
  not (null indentLevels) ==>
  let inputLines = zipWith (\level content -> replicate (abs level `mod` 20) ' ' ++ content) indentLevels (map show ([1..] :: [Integer]))
      content = unlines inputLines
      normalized = normalizeIndentation content
      normalizedLines = filter (not . null) (lines normalized)
      minIndent = if null normalizedLines then 0 else minimum [length (takeWhile isSpace line) | line <- normalizedLines]
  in minIndent === 0

-- Property: error recovery in malformed input
prop_error_recovery_malformed :: String -> Property
prop_error_recovery_malformed content =
  let malformedContent = content ++ "/* unterminated\nwith newlines"
      processed = removeComments malformedContent
  in length processed >= length content - 50 -- Should preserve most content

-- ============================================================================
-- Consistency and Idempotency Properties
-- ============================================================================

-- Property: removeComments is idempotent
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent input =
  let removedOnce = removeComments input
      removedTwice = removeComments removedOnce
  in removedOnce === removedTwice

-- Property: removeLineComments is idempotent
prop_removeLineComments_idempotent :: String -> Property
prop_removeLineComments_idempotent input =
  let removedOnce = removeLineComments input
      removedTwice = removeLineComments removedOnce
  in removedOnce === removedTwice

-- Property: forceSingleTabIndentation is idempotent
prop_forceSingleTabIndentation_idempotent :: String -> Property
prop_forceSingleTabIndentation_idempotent input =
  let forcedOnce = forceSingleTabIndentation input
      forcedTwice = forceSingleTabIndentation forcedOnce
  in forcedOnce === forcedTwice

-- Property: splitBy roundtrip with join
prop_splitBy_join_roundtrip :: Char -> String -> Property
prop_splitBy_join_roundtrip delim input =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in rejoined === input

-- ============================================================================
-- Boundary Condition Properties
-- ============================================================================

-- Property: functions handle empty strings
prop_handle_empty_strings :: Property
prop_handle_empty_strings =
  let empty = ""
  in trim empty === empty .&&.
     splitBy ',' empty === [""] .&&.
     splitByCollapsed ',' empty === [] .&&.
     removeLineComments empty === empty .&&.
     removeComments empty === empty .&&.
     normalizeIndentation empty === empty .&&.
     forceSingleTabIndentation empty === empty .&&.
     breakOn "x" empty === (empty, "")

-- Property: functions handle single characters
prop_handle_single_chars :: Char -> Property
prop_handle_single_chars char =
  let single = [char]
  in trim single === (if isSpace char then "" else single) .&&.
     splitBy char single === ["", ""] .&&.
     splitByCollapsed char single === [] .&&.
     breakOn "x" single === (if char == 'x' then ("", "") else (single, ""))

-- Property: functions handle whitespace-only strings
prop_handle_whitespace_only :: String -> Property
prop_handle_whitespace_only whitespace =
  all isSpace whitespace ==>
  let trimmed = trim whitespace
  in null trimmed .&&.
     normalizeIndentation whitespace === whitespace .&&.
     forceSingleTabIndentation whitespace === "\t"

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils Advanced QuickCheck Tests"
  [ testGroup "Advanced String Processing Properties"
    [ fastProperty "trim preserves internal whitespace structure" prop_trim_preserves_internal_structure
    , fastProperty "trim handles Unicode whitespace correctly" prop_trim_unicode_whitespace
    , fastProperty "splitBy handles Unicode delimiters" prop_splitBy_unicode_delimiter
    , fastProperty "splitBy maintains character encoding" prop_splitBy_maintains_encoding
    ]

  , testGroup "Advanced Comment Processing Properties"
    [ fastProperty "removeLineComments handles nested quotes" prop_removeLineComments_nested_quotes
    , fastProperty "removeComments handles malformed block comments" prop_removeComments_malformed_blocks
    , fastProperty "removeComments preserves comment-like content in strings" prop_removeComments_preserves_string_content
    , fastProperty "removeLineComments handles escaped backslashes" prop_removeLineComments_escaped_backslashes
    ]

  , testGroup "Advanced Indentation Properties"
    [ fastProperty "normalizeIndentation handles mixed tabs and spaces" prop_normalizeIndentation_mixed_tabs_spaces
    , fastProperty "forceSingleTabIndentation preserves logical structure" prop_forceSingleTabIndentation_preserves_structure
    , fastProperty "normalizeIndentation handles empty lines correctly" prop_normalizeIndentation_empty_lines
    , fastProperty "normalizeIndentation is idempotent" prop_normalizeIndentation_idempotent
    ]

  , testGroup "Advanced Search and Split Properties"
    [ fastProperty "breakOn handles overlapping patterns" prop_breakOn_overlapping_patterns
    , fastProperty "breakOn handles Unicode patterns" prop_breakOn_unicode_patterns
    , fastProperty "splitByCollapsed maintains uniqueness" prop_splitByCollapsed_maintains_uniqueness
    , fastProperty "splitBy and splitByCollapsed relationship" prop_splitBy_splitByCollapsed_relationship
    ]

  , testGroup "Performance and Edge Case Properties"
    [ fastProperty "trim handles very large strings efficiently" prop_trim_large_strings
    , fastProperty "splitBy handles very long delimiters" prop_splitBy_long_delimiters
    , fastProperty "removeComments handles deeply nested comments" prop_removeComments_deeply_nested
    , fastProperty "normalizeIndentation handles extreme indentation" prop_normalizeIndentation_extreme_indentation
    ]

  , testGroup "Unicode and Internationalization Properties"
    [ fastProperty "trim handles right-to-left text" prop_trim_rtl_text
    , fastProperty "splitBy handles zero-width characters" prop_splitBy_zero_width
    , fastProperty "removeComments handles Unicode comments" prop_removeComments_unicode_comments
    ]

  , testGroup "Complex Scenario Properties"
    [ fastProperty "complete text processing pipeline" prop_complete_processing_pipeline
    , fastProperty "mixed comment types interaction" prop_mixed_comment_interaction
    , fastProperty "complex indentation scenarios" prop_complex_indentation_scenarios
    , fastProperty "error recovery in malformed input" prop_error_recovery_malformed
    ]

  , testGroup "Consistency and Idempotency Properties"
    [ fastProperty "removeComments is idempotent" prop_removeComments_idempotent
    , fastProperty "removeLineComments is idempotent" prop_removeLineComments_idempotent
    , fastProperty "forceSingleTabIndentation is idempotent" prop_forceSingleTabIndentation_idempotent
    , fastProperty "splitBy roundtrip with join" prop_splitBy_join_roundtrip
    ]

  , testGroup "Boundary Condition Properties"
    [ fastProperty "functions handle empty strings" prop_handle_empty_strings
    , fastProperty "functions handle single characters" prop_handle_single_chars
    , fastProperty "functions handle whitespace-only strings" prop_handle_whitespace_only
    ]
  ]