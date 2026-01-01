{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewUtilsStringBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Char (isSpace, isControl, isAscii, isLetter, isDigit)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub)
import qualified Data.Text as T

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

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements " \t\n\r\f\v"

-- Generate strings with control characters
genControlCharString :: Gen String
genControlCharString = listOf $ elements $ map chr [0..31] ++ [chr 127]

-- Generate Unicode strings
genUnicodeString :: Gen String
genUnicodeString = listOf $ elements $ map chr [0..65535]

-- Generate strings with special characters
genSpecialCharString :: Gen String
genSpecialCharString = listOf $ elements "!@#$%^&*()[]{}|\\:;\"'<>?,./~`"

-- Generate strings with quote characters
genQuotedString :: Gen String
genQuotedString = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  let quoted = "\"" ++ content ++ "\""
  return quoted

-- Generate strings with comment-like patterns
genCommentLikeString :: Gen String
genCommentLikeString = do
  before <- listOf $ elements $ ['a'..'z'] ++ " "
  comment <- listOf $ elements $ ['a'..'z'] ++ " "
  return $ before ++ "// " ++ comment

-- Generate strings with block comment patterns
genBlockCommentString :: Gen String
genBlockCommentString = do
  before <- listOf $ elements $ ['a'..'z'] ++ " "
  comment <- listOf $ elements $ ['a'..'z'] ++ " "
  return $ before ++ "/* " ++ comment ++ " */"

-- Generate strings with mixed indentation
genMixedIndentation :: Gen String
genMixedIndentation = do
  lines <- listOf $ do
    spaces <- choose (0, 10)
    tabs <- choose (0, 3)
    content <- elements $ ['a'..'z'] ++ " "
    return $ replicate spaces ' ' ++ replicate tabs '\t' ++ content
  return $ unlines lines

-- Generate strings with various line endings
genLineEndingString :: Gen String
genLineEndingString = do
  lines <- listOf $ elements $ ['a'..'z'] ++ " "
  ending <- elements ["\n", "\r\n", "\r"]
  return $ concatMap (++ ending) lines

-- ============================================================================
-- Boundary Test Properties for trim
-- ============================================================================

-- Property: trim handles empty string
prop_trim_empty :: Property
prop_trim_empty =
  trim "" === ""

-- Property: trim handles only whitespace
prop_trim_only_whitespace :: String -> Property
prop_trim_only_whitespace ws =
  all isSpace ws ==> trim ws === ""

-- Property: trim handles no whitespace
prop_trim_no_whitespace :: String -> Property
prop_trim_no_whitespace s =
  not (L.any isSpace s) ==> trim s === s

-- Property: trim handles leading whitespace only
prop_trim_leading_only :: String -> String -> Property
prop_trim_leading_only ws content =
  all isSpace ws && not (L.any isSpace content) ==>
  let input = ws ++ content
  in trim input === content

-- Property: trim handles trailing whitespace only
prop_trim_trailing_only :: String -> String -> Property
prop_trim_trailing_only content ws =
  all isSpace ws && not (L.any isSpace content) ==>
  let input = content ++ ws
  in trim input === content

-- Property: trim handles mixed whitespace types
prop_trim_mixed_whitespace :: String -> String -> String -> Property
prop_trim_mixed_whitespace leading content trailing =
  all isSpace leading && L.all isSpace trailing && not (L.any isSpace content) ==>
  let input = leading ++ content ++ trailing
  in trim input === content

-- Property: trim handles control characters
prop_trim_control_chars :: String -> Property
prop_trim_control_chars s =
  let input = "\0\1\2" ++ s ++ "\30\31\127"
  in property $ not (L.any isControl (trim input))

-- Property: trim handles Unicode whitespace
prop_trim_unicode_whitespace :: String -> Property
prop_trim_unicode_whitespace s =
  -- Unicode whitespace characters
  let unicodeWs = "\x00A0\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200A\x2028\x2029\x202F\x205F\x3000"
      input = unicodeWs ++ s ++ unicodeWs
  in property $ not (L.any (`elem` unicodeWs) (trim input))

-- ============================================================================
-- Boundary Test Properties for splitBy
-- ============================================================================

-- Property: splitBy handles empty string
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty delim =
  splitBy delim "" === [""]

-- Property: splitBy handles delimiter not in string
prop_splitBy_delim_not_present :: Char -> String -> Property
prop_splitBy_delim_not_present delim s =
  delim `notElem` s ==> splitBy delim s === [s]

-- Property: splitBy handles string with only delimiters
prop_splitBy_only_delimiters :: Char -> Int -> Property
prop_splitBy_only_delimiters delim count =
  count > 0 && count <= 100 ==>
  let input = replicate count delim
      result = splitBy delim input
      expected = replicate (count + 1) ""
  in result === expected

-- Property: splitBy handles consecutive delimiters
prop_splitBy_consecutive :: Char -> Int -> String -> Property
prop_splitBy_consecutive delim count content =
  count > 0 && count <= 10 && delim `notElem` content ==>
  let consecutive = replicate count delim
      input = content ++ consecutive ++ content
      result = splitBy delim input
  in L.length result === count + 2 .&&. result !! (count + 1) === content

-- Property: splitBy handles Unicode delimiter
prop_splitBy_unicode_delim :: String -> Property
prop_splitBy_unicode_delim content =
  let delim = '€'  -- Unicode character
      input = content ++ [delim] ++ content
  in if delim `elem` content
     then property $ L.length (splitBy delim input) >= 2
     else property $ splitBy delim input === [input]

-- ============================================================================
-- Boundary Test Properties for splitByCollapsed
-- ============================================================================

-- Property: splitByCollapsed handles empty string
prop_splitByCollapsed_empty :: Char -> Property
prop_splitByCollapsed_empty delim =
  splitByCollapsed delim "" === []

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim input =
  let result = splitByCollapsed delim input
  in property $ not (L.any null result)

-- Property: splitByCollapsed handles only delimiters
prop_splitByCollapsed_only_delimiters :: Char -> Int -> Property
prop_splitByCollapsed_only_delimiters delim count =
  count > 0 && count <= 100 ==>
  let input = replicate count delim
      result = splitByCollapsed delim input
  in result === []

-- Property: splitByCollapsed handles consecutive delimiters
prop_splitByCollapsed_consecutive :: Char -> Int -> String -> Property
prop_splitByCollapsed_consecutive delim count content =
  count > 0 && count <= 10 && delim `notElem` content ==>
  let consecutive = replicate count delim
      input = content ++ consecutive ++ content
      result = splitByCollapsed delim input
  in result === [content, content]

-- ============================================================================
-- Boundary Test Properties for removeLineComments
-- ============================================================================

-- Property: removeLineComments handles empty string
prop_removeLineComments_empty :: Property
prop_removeLineComments_empty =
  removeLineComments "" === ""

-- Property: removeLineComments handles string without comments
prop_removeLineComments_no_comments :: String -> Property
prop_removeLineComments_no_comments s =
  "//" `notElem` (take 2 s) ==> removeLineComments s === s

-- Property: removeLineComments handles only comment
prop_removeLineComments_only_comment :: String -> Property
prop_removeLineComments_only_comment comment =
  let input = "// " ++ comment
  in property $ removeLineComments input === " "

-- Property: removeLineComments handles comment at start
prop_removeLineComments_comment_at_start :: String -> String -> Property
prop_removeLineComments_comment_at_start comment after =
  let input = "// " ++ comment ++ "\n" ++ after
      result = removeLineComments input
  in property $ not ("// " `L.isInfixOf` result) .&&. after `L.isInfixOf` result

-- Property: removeLineComments preserves comments in strings
prop_removeLineComments_preserves_string_comments :: String -> Property
prop_removeLineComments_preserves_string_comments content =
  let input = "var s = \"// not a comment " ++ content ++ "\"\n// real comment"
      result = removeLineComments input
  in property $ "// not a comment" `L.isInfixOf` result .&&.
             not ("// real comment" `L.isInfixOf` result)

-- Property: removeLineComments handles multiple comments
prop_removeLineComments_multiple :: String -> String -> String -> Property
prop_removeLineComments_multiple before middle after =
  let input = before ++ "// comment1\n" ++ middle ++ "// comment2\n" ++ after
      result = removeLineComments input
  in property $ not ("// comment1" `L.isInfixOf` result) .&&.
             not ("// comment2" `L.isInfixOf` result)

-- Property: removeLineComments handles escaped quotes
prop_removeLineComments_escaped_quotes :: String -> Property
prop_removeLineComments_escaped_quotes content =
  let input = "var s = \"// not comment \\\"escaped\\\" " ++ content ++ "\"\n// real comment"
      result = removeLineComments input
  in property $ "// not comment" `L.isInfixOf` result .&&.
             not ("// real comment" `L.isInfixOf` result)

-- ============================================================================
-- Boundary Test Properties for removeComments
-- ============================================================================

-- Property: removeComments handles empty string
prop_removeComments_empty :: Property
prop_removeComments_empty =
  removeComments "" === ""

-- Property: removeComments handles string without comments
prop_removeComments_no_comments :: String -> Property
prop_removeComments_no_comments s =
  "//" `notElem` take 2 s && "/*" `notElem` take 2 s ==> removeComments s === s

-- Property: removeComments handles only line comment
prop_removeComments_only_line_comment :: String -> Property
prop_removeComments_only_line_comment comment =
  let input = "// " ++ comment
  in property $ removeComments input === " "

-- Property: removeComments handles only block comment
prop_removeComments_only_block_comment :: String -> Property
prop_removeComments_only_block_comment comment =
  let input = "/* " ++ comment ++ " */"
  in property $ removeComments input === "  "

-- Property: removeComments preserves comments in strings
prop_removeComments_preserves_string_comments :: String -> String -> Property
prop_removeComments_preserves_string_comments comment1 comment2 =
  let input = "var s1 = \"// not comment1\"\nvar s2 = \"/* not comment2 */\"\n// real comment"
      result = removeComments input
  in property $ "// not comment1" `L.isInfixOf` result .&&.
             "/* not comment2 */" `L.isInfixOf` result .&&.
             not ("// real comment" `L.isInfixOf` result)

-- Property: removeComments handles nested block comments (C-style: first */ ends)
prop_removeComments_nested_block :: String -> String -> Property
prop_removeComments_nested_block outer inner =
  let input = "/* outer /* " ++ inner ++ " */ after */"
      result = removeComments input
  in property $ not ("/* outer" `L.isInfixOf` result) .&&.
             not ("/* " ++ inner `L.isInfixOf` result) .&&.
             " after */" `L.isInfixOf` result

-- Property: removeComments handles malformed block comments
prop_removeComments_malformed_block :: String -> Property
prop_removeComments_malformed_block content =
  let input = content ++ "/* unclosed comment"
      result = removeComments input
  in property $ not ("/*" `L.isInfixOf` result)

-- ============================================================================
-- Boundary Test Properties for normalizeIndentation
-- ============================================================================

-- Property: normalizeIndentation handles empty string
prop_normalizeIndentation_empty :: Property
prop_normalizeIndentation_empty =
  normalizeIndentation "" === ""

-- Property: normalizeIndentation handles single line
prop_normalizeIndentation_single_line :: String -> String -> Property
prop_normalizeIndentation_single_line indent content =
  all isSpace indent ==>
  let input = indent ++ content
      result = normalizeIndentation input
  in property $ not (L.any isSpace (take 1 result))

-- Property: normalizeIndentation handles no indentation
prop_normalizeIndentation_no_indent :: String -> Property
prop_normalizeIndentation_no_indent content =
  not ('\n' `elem` content) ==> normalizeIndentation content === content

-- Property: normalizeIndentation handles uniform indentation
prop_normalizeIndentation_uniform :: String -> String -> Property
prop_normalizeIndentation_uniform indent content =
  all isSpace indent ==>
  let lines' = [indent ++ "line1", indent ++ "line2", indent ++ content]
      input = unlines lines'
      result = normalizeIndentation input
      resultLines = lines result
  in property $ L.all (\line -> not (indent `L.isPrefixOf` line)) resultLines

-- Property: normalizeIndentation handles mixed whitespace
prop_normalizeIndentation_mixed_whitespace :: String -> String -> Property
prop_normalizeIndentation_mixed_whitespace content =
  not ('\n' `elem` content) ==>
  let mixed = " \t " ++ content ++ "\n\t  " ++ content
      result = normalizeIndentation mixed
  in property $ not (L.any isSpace (take 1 result))

-- Property: normalizeIndentation handles tabs L.and spaces
prop_normalizeIndentation_tabs_spaces :: String -> Property
prop_normalizeIndentation_tabs_spaces content =
  let input = "\t  " ++ content ++ "\n  \t" ++ content
      result = normalizeIndentation input
  in property $ not (L.any isSpace (take 1 result))

-- ============================================================================
-- Boundary Test Properties for forceSingleTabIndentation
-- ============================================================================

-- Property: forceSingleTabIndentation handles empty string
prop_forceSingleTabIndentation_empty :: Property
prop_forceSingleTabIndentation_empty =
  forceSingleTabIndentation "" === ""

-- Property: forceSingleTabIndentation handles empty lines
prop_forceSingleTabIndentation_empty_lines :: Int -> Property
prop_forceSingleTabIndentation_empty_lines count =
  count >= 0 && count <= 10 ==>
  let input = unlines (replicate count "")
      result = forceSingleTabIndentation input
  in property $ result === input

-- Property: forceSingleTabIndentation handles non-empty content
prop_forceSingleTabIndentation_non_empty :: String -> Property
prop_forceSingleTabIndentation_non_empty content =
  not (L.null (trim content)) && not ('\n' `elem` content) ==>
  let result = forceSingleTabIndentation content
      resultLines = lines result
  in property $ not (null resultLines) .&&.
             head resultLines `L.isPrefixOf` "\t"

-- Property: forceSingleTabIndentation handles already tabbed content
prop_forceSingleTabIndentation_already_tabbed :: String -> Property
prop_forceSingleTabIndentation_already_tabbed content =
  not (L.null (trim content)) ==>
  let tabbed = "\t" ++ trim content
      result = forceSingleTabIndentation tabbed
  in property $ "\t" `L.isPrefixOf` result

-- ============================================================================
-- Boundary Test Properties for breakOn
-- ============================================================================

-- Property: breakOn handles empty pattern
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern haystack =
  let (before, after) = breakOn "" haystack
  in property $ before === "" .&&. after === haystack

-- Property: breakOn handles pattern longer than haystack
prop_breakOn_pattern_too_long :: String -> String -> Property
prop_breakOn_pattern_too_long pat haystack =
  length pat > L.length haystack && not (null pat) ==>
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

-- Property: breakOn handles pattern not found
prop_breakOn_pattern_not_found :: String -> String -> Property
prop_breakOn_pattern_not_found pat haystack =
  not (null pat) && not (pat `L.isInfixOf` haystack) ==>
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

-- Property: breakOn handles pattern at start
prop_breakOn_pattern_at_start :: String -> String -> Property
prop_breakOn_pattern_at_start pat suffix =
  not (null pat) ==>
  let haystack = pat ++ suffix
      (before, after) = breakOn pat haystack
  in property $ before === "" .&&. after === suffix

-- Property: breakOn handles pattern at end
prop_breakOn_pattern_at_end :: String -> String -> Property
prop_breakOn_pattern_at_end pat prefix =
  not (null pat) && not (pat `L.isInfixOf` prefix) ==>
  let haystack = prefix ++ pat
      (before, after) = breakOn pat haystack
  in property $ before === prefix .&&. after === ""

-- Property: breakOn handles overlapping patterns
prop_breakOn_overlapping :: String -> String -> Property
prop_breakOn_overlapping pat haystack =
  not (null pat) ==>
  let overlapping = pat ++ take (L.length pat - 1) pat
      (before, after) = breakOn overlapping haystack
  in property $ before ++ overlapping ++ after === haystack .||.
             (before === haystack .&&. after === "")

-- ============================================================================
-- Performance L.and Stress Tests
-- ============================================================================

-- Property: Large string handling
prop_large_string_handling :: Int -> String -> Property
prop_large_string_handling multiplier base =
  multiplier >= 0 && multiplier <= 1000 ==>
  let large = L.concat (replicate multiplier base)
      trimmed = trim large
      split = splitBy ',' large
  in property $ L.length trimmed <= L.length large .&&.
             length split >= 1

-- Property: Deep nesting handling
prop_deep_nesting_comments :: Int -> Property
prop_deep_nesting_comments depth =
  depth >= 0 && depth <= 100 ==>
  let nested = L.concat (replicate depth "/*") ++ "content" ++ L.concat (replicate depth "*/")
      result = removeComments nested
  in property $ "content" `L.isInfixOf` result .||. null result

-- Property: Complex indentation scenarios
prop_complex_indentation :: Int -> Property
prop_complex_indentation complexity =
  complexity >= 0 && complexity <= 50 ==>
  let lines' = [[replicate n ' ' ++ "line"] | n <- [0..complexity]]
      input = unlines (L.concat lines')
      result = normalizeIndentation input
  in property $ not (null result)

-- ============================================================================
-- Unicode L.and Internationalization Tests
-- ============================================================================

-- Property: Unicode character handling
prop_unicode_handling :: String -> Property
prop_unicode_handling content =
  let unicodeContent = content ++ "café naïve résumé 测试 🚀"
      trimmed = trim unicodeContent
  in property $ "café" `L.isInfixOf` trimmed .&&.
             "naïve" `L.isInfixOf` trimmed .&&.
             "résumé" `L.isInfixOf` trimmed .&&.
             "测试" `L.isInfixOf` trimmed .&&.
             "🚀" `L.isInfixOf` trimmed

-- Property: Right-to-left text handling
prop_rtl_text_handling :: String -> Property
prop_rtl_text_handling content =
  let rtlContent = content ++ "العربية עברית"
      trimmed = trim rtlContent
  in property $ "العربية" `L.isInfixOf` trimmed .&&.
             "עברית" `L.isInfixOf` trimmed

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Utils String Boundary QuickCheck Tests"
  [ testGroup "trim Boundary Tests"
    [ fastProperty "trim empty" prop_trim_empty
    , fastProperty "trim only whitespace" prop_trim_only_whitespace
    , fastProperty "trim no whitespace" prop_trim_no_whitespace
    , fastProperty "trim leading only" prop_trim_leading_only
    , fastProperty "trim trailing only" prop_trim_trailing_only
    , fastProperty "trim mixed whitespace" prop_trim_mixed_whitespace
    , fastProperty "trim control chars" prop_trim_control_chars
    , fastProperty "trim unicode whitespace" prop_trim_unicode_whitespace
    ]

  , testGroup "splitBy Boundary Tests"
    [ fastProperty "splitBy empty" prop_splitBy_empty
    , fastProperty "splitBy delim not present" prop_splitBy_delim_not_present
    , fastProperty "splitBy only delimiters" prop_splitBy_only_delimiters
    , fastProperty "splitBy consecutive" prop_splitBy_consecutive
    , fastProperty "splitBy unicode delim" prop_splitBy_unicode_delim
    ]

  , testGroup "splitByCollapsed Boundary Tests"
    [ fastProperty "splitByCollapsed empty" prop_splitByCollapsed_empty
    , fastProperty "splitByCollapsed removes empty" prop_splitByCollapsed_removes_empty
    , fastProperty "splitByCollapsed only delimiters" prop_splitByCollapsed_only_delimiters
    , fastProperty "splitByCollapsed consecutive" prop_splitByCollapsed_consecutive
    ]

  , testGroup "removeLineComments Boundary Tests"
    [ fastProperty "removeLineComments empty" prop_removeLineComments_empty
    , fastProperty "removeLineComments no comments" prop_removeLineComments_no_comments
    , fastProperty "removeLineComments only comment" prop_removeLineComments_only_comment
    , fastProperty "removeLineComments comment at start" prop_removeLineComments_comment_at_start
    , fastProperty "removeLineComments preserves string comments" prop_removeLineComments_preserves_string_comments
    , fastProperty "removeLineComments multiple" prop_removeLineComments_multiple
    , fastProperty "removeLineComments escaped quotes" prop_removeLineComments_escaped_quotes
    ]

  , testGroup "removeComments Boundary Tests"
    [ fastProperty "removeComments empty" prop_removeComments_empty
    , fastProperty "removeComments no comments" prop_removeComments_no_comments
    , fastProperty "removeComments only line comment" prop_removeComments_only_line_comment
    , fastProperty "removeComments only block comment" prop_removeComments_only_block_comment
    , fastProperty "removeComments preserves string comments" prop_removeComments_preserves_string_comments
    , fastProperty "removeComments nested block" prop_removeComments_nested_block
    , fastProperty "removeComments malformed block" prop_removeComments_malformed_block
    ]

  , testGroup "normalizeIndentation Boundary Tests"
    [ fastProperty "normalizeIndentation empty" prop_normalizeIndentation_empty
    , fastProperty "normalizeIndentation single line" prop_normalizeIndentation_single_line
    , fastProperty "normalizeIndentation no indent" prop_normalizeIndentation_no_indent
    , fastProperty "normalizeIndentation uniform" prop_normalizeIndentation_uniform
    , fastProperty "normalizeIndentation mixed whitespace" prop_normalizeIndentation_mixed_whitespace
    , fastProperty "normalizeIndentation tabs spaces" prop_normalizeIndentation_tabs_spaces
    ]

  , testGroup "forceSingleTabIndentation Boundary Tests"
    [ fastProperty "forceSingleTabIndentation empty" prop_forceSingleTabIndentation_empty
    , fastProperty "forceSingleTabIndentation empty lines" prop_forceSingleTabIndentation_empty_lines
    , fastProperty "forceSingleTabIndentation non empty" prop_forceSingleTabIndentation_non_empty
    , fastProperty "forceSingleTabIndentation already tabbed" prop_forceSingleTabIndentation_already_tabbed
    ]

  , testGroup "breakOn Boundary Tests"
    [ fastProperty "breakOn empty pattern" prop_breakOn_empty_pattern
    , fastProperty "breakOn pattern too long" prop_breakOn_pattern_too_long
    , fastProperty "breakOn pattern not found" prop_breakOn_pattern_not_found
    , fastProperty "breakOn pattern at start" prop_breakOn_pattern_at_start
    , fastProperty "breakOn pattern at end" prop_breakOn_pattern_at_end
    , fastProperty "breakOn overlapping" prop_breakOn_overlapping
    ]

  , testGroup "Performance L.and Stress Tests"
    [ fastProperty "large string handling" prop_large_string_handling
    , fastProperty "deep nesting comments" prop_deep_nesting_comments
    , fastProperty "complex indentation" prop_complex_indentation
    ]

  , testGroup "Unicode L.and Internationalization Tests"
    [ fastProperty "unicode handling" prop_unicode_handling
    , fastProperty "rtl text handling" prop_rtl_text_handling
    ]
  ]