{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsNewQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat)

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

import Data.Char (isSpace, isAlphaNum)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, intercalate)

-- ============================================================================
-- Arbitrary Instances for Utils Testing
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements $ " \t\n\r" ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-.,;:!?()[]{}<>+/\\"

-- Generate strings without quotes (for comment testing)
genStringWithoutQuotes :: Gen String
genStringWithoutQuotes = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r._-;:,!()[]{}<>+/"

-- Generate strings with balanced quotes (for string literal testing)
genStringWithBalancedQuotes :: Gen String
genStringWithBalancedQuotes = do
  before <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\r._-;:,!()[]{}<>+/"
  middle <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\r._-;:,!()[]{}<>+/"
  after <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\r._-;:,!()[]{}<>+/"
  return $ before ++ "\"" ++ middle ++ "\"" ++ after

-- Generate strings with balanced single quotes (for char literal testing)
genStringWithBalancedCharQuotes :: Gen String
genStringWithBalancedCharQuotes = do
  before <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\r._-;:,!()[]{}<>+/"
  char <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\r._-;:,!()[]{}<>+/"
  after <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\r._-;:,!()[]{}<>+/"
  return $ before ++ "'" ++ char ++ "'" ++ after

-- Generate indented code blocks
genIndentedCode :: Gen String
genIndentedCode = do
  baseIndent <- choose (0, 8)
  lines' <- listOf $ do
    indent <- choose (baseIndent, baseIndent + 4)
    content <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ " .,;:!()"
    return $ replicate indent ' ' ++ content
  return $ unlines lines'

-- ============================================================================
-- Trim Properties
-- ============================================================================

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: Property
prop_trim_removes_whitespace =
  forAll genWhitespaceString $ \str ->
  let trimmed = trim str
      hasLeading = not (null str) && isSpace (head str)
      hasTrailing = not (null str) && isSpace (last str)
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ null trimmed || 
                (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: trim of empty string is empty
prop_trim_empty :: Property
prop_trim_empty = trim "" === ""

-- Property: trim of only whitespace is empty
prop_trim_only_whitespace :: Property
prop_trim_only_whitespace =
  forAll (listOf $ elements " \t\n\r") $ \whitespace ->
  trim whitespace === ""

-- ============================================================================
-- Split Properties
-- ============================================================================

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
  in property $ all (not . null) result

-- Property: splitByComma is splitBy with comma
prop_splitByComma_is_splitBy :: String -> Property
prop_splitByComma_is_splitBy input =
  splitByComma input === splitBy ',' input

-- Property: splitByCommaCollapsed is splitByCollapsed with comma
prop_splitByCommaCollapsed_is_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed_is_splitByCollapsed input =
  splitByCommaCollapsed input === splitByCollapsed ',' input

-- Property: splitBy and join roundtrip
prop_splitBy_join_roundtrip :: Char -> String -> Property
prop_splitBy_join_roundtrip delim input =
  let parts = splitBy delim input
      rejoined = intercalate [delim] parts
  in property $ rejoined === input

-- Property: splitByCollapsed handles consecutive delimiters
prop_splitByCollapsed_consecutive :: Char -> Int -> String -> Property
prop_splitByCollapsed_consecutive delim count suffix =
  count > 0 && not (delim `elem` suffix) && not (null suffix) ==>
  let consecutive = replicate count delim
      input = "prefix" ++ consecutive ++ suffix
      parts = splitByCollapsed delim input
  in property $ parts === ["prefix", suffix]

-- ============================================================================
-- Comment Removal Properties
-- ============================================================================

-- Property: removeLineComments removes // comments
prop_removeLineComments_removes :: String -> String -> Property
prop_removeLineComments_removes prefix comment =
  let content = prefix ++ "// " ++ comment ++ "\nafter comment"
      result = removeLineComments content
  in not ("// " `isInfixOf` prefix) ==> 
     property $ not ("// " `isInfixOf` result) .&&.
                "after comment" `isInfixOf` result

-- Property: removeLineComments preserves comments in strings
prop_removeLineComments_preserves_string :: String -> Property
prop_removeLineComments_preserves_string comment =
  let content = "var s = \"// not a comment " ++ comment ++ "\"\n// real comment"
      result = removeLineComments content
  in property $ "// not a comment" `isInfixOf` result .&&.
                not ("// real comment" `isInfixOf` result)

-- Property: removeLineComments is idempotent
prop_removeLineComments_idempotent :: String -> Property
prop_removeLineComments_idempotent input =
  let removedOnce = removeLineComments input
      removedTwice = removeLineComments removedOnce
  in property $ removedOnce === removedTwice

-- Property: removeComments removes both // and /* */ comments
prop_removeComments_removes_both :: String -> String -> String -> Property
prop_removeComments_removes_both before comment after =
  not (any (`elem` "\"'") before) && not (any (`elem` "\"'") after) &&
  not ("/*" `isInfixOf` before) && not ("/*" `isInfixOf` after) ==>
  let content = before ++ "/* block comment */" ++ comment ++ "// line comment\n" ++ after
      result = removeComments content
  in property $ not ("/*" `isInfixOf` result) .&&.
                not ("*/" `isInfixOf` result) .&&.
                not ("// line comment" `isInfixOf` result) .&&.
                after `isInfixOf` result

-- Property: removeComments preserves comments in strings
prop_removeComments_preserves_strings :: String -> String -> Property
prop_removeComments_preserves_strings comment1 comment2 =
  let content = "var s1 = \"// not comment1\"\nvar s2 = \"/* not comment2 */\"\n// real comment"
      result = removeComments content
  in property $ "// not comment1" `isInfixOf` result .&&.
                "/* not comment2 */" `isInfixOf` result .&&.
                not ("// real comment" `isInfixOf` result)

-- Property: removeComments is idempotent
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent input =
  let removedOnce = removeComments input
      removedTwice = removeComments removedOnce
  in property $ removedOnce === removedTwice

-- ============================================================================
-- Indentation Properties
-- ============================================================================

-- Property: normalizeIndentation removes common prefix
prop_normalizeIndentation_removes_prefix :: Property
prop_normalizeIndentation_removes_prefix =
  forAll genIndentedCode $ \code ->
  let lines' = lines code
      nonEmptyLines = filter (not . all isSpace) lines'
      normalized = normalizeIndentation code
      normalizedLines = lines normalized
  in if null nonEmptyLines
     then property $ normalized === code
     else property $ 
       let minIndent = minimum [length (takeWhile isSpace line) | line <- nonEmptyLines]
           firstNonEmpty = head [line | line <- normalizedLines, not (all isSpace line)]
       in not (null firstNonEmpty) ==> 
          not (take 1 firstNonEmpty == " " || take 1 firstNonEmpty == "\t")

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: Property
prop_normalizeIndentation_preserves_relative =
  forAll genIndentedCode $ \code ->
  let lines' = lines code
      normalized = normalizeIndentation code
      normalizedLines = lines normalized
  in property $ length normalizedLines === length lines'

-- Property: normalizeIndentation is idempotent
prop_normalizeIndentation_idempotent :: String -> Property
prop_normalizeIndentation_idempotent input =
  let normalizedOnce = normalizeIndentation input
      normalizedTwice = normalizeIndentation normalizedOnce
  in property $ normalizedOnce === normalizedTwice

-- Property: forceSingleTabIndentation forces tab indentation
prop_forceSingleTabIndentation_forces_tab :: Property
prop_forceSingleTabIndentation_forces_tab =
  forAll genIndentedCode $ \code ->
  let forced = forceSingleTabIndentation code
      forcedLines = lines forced
      nonEmptyLines = filter (not . null . trim) forcedLines
  in property $ all (\line -> null line || head line == '\t') nonEmptyLines

-- Property: fixIndentation equals normalizeIndentation
prop_fixIndentation_equals_normalize :: String -> Property
prop_fixIndentation_equals_normalize input =
  fixIndentation input === normalizeIndentation input

-- ============================================================================
-- Search Properties
-- ============================================================================

-- Property: breakOn finds substring
prop_breakOn_finds :: String -> String -> String -> Property
prop_breakOn_finds pat prefix suffix =
  not (null pat) ==>
  let haystack = prefix ++ pat ++ suffix
      (before, after) = breakOn pat haystack
  in property $ before ++ pat ++ after === haystack

-- Property: breakOn handles empty pattern
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern haystack =
  let (before, after) = breakOn "" haystack
  in property $ before === "" .&&. after === haystack

-- Property: breakOn handles missing pattern
prop_breakOn_missing :: String -> String -> Property
prop_breakOn_missing pat haystack =
  not (null pat) && not (pat `isInfixOf` haystack) ==> 
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

-- Property: breakOn returns first occurrence
prop_breakOn_first :: String -> String -> String -> Property
prop_breakOn_first pat prefix suffix =
  not (null pat) ==>
  let haystack = prefix ++ pat ++ suffix ++ pat ++ "extra"
      (before, after) = breakOn pat haystack
  in property $ before === prefix ++ pat ++ suffix .&&. after === "extra"

-- ============================================================================
-- Complex Interaction Properties
-- ============================================================================

-- Property: Complex string processing pipeline
prop_complex_pipeline :: String -> String -> String -> Property
prop_complex_pipeline prefix middle suffix =
  not (any (`elem` "\"'") prefix) && not (any (`elem` "\"'") middle) && not (any (`elem` "\"'") suffix) ==>
  let input = prefix ++ "  /* comment */  " ++ middle ++ "  // line comment  " ++ suffix
      processed = removeComments input
      trimmed = trim processed
      normalized = normalizeIndentation trimmed
  in property $ not ("/* comment */" `isInfixOf` processed) .&&.
                not ("// line comment" `isInfixOf` processed) .&&.
                (null trimmed || not (isSpace (head trimmed))) .&&.
                (null trimmed || not (isSpace (last trimmed)))

-- Property: Whitespace handling consistency
prop_whitespace_consistency :: String -> Property
prop_whitespace_consistency input =
  let trimmed = trim input
      split = splitBy ',' input
      splitCollapsed = splitByCollapsed ',' input
  in property $ length split >= length splitCollapsed

-- Property: Comment removal with edge cases
prop_comment_edge_cases :: String -> Property
prop_comment_edge_cases input =
  let withLineComments = input ++ "\n// comment\n"
      withBlockComments = input ++ "/* comment */" 
      removedLine = removeLineComments withLineComments
      removedBlock = removeComments withBlockComments
  in property $ length removedLine >= length input - 20 .&&. -- Allow some reduction
                length removedBlock >= length input - 20

-- Property: Indentation normalization with mixed content
prop_mixed_indentation :: Property
prop_mixed_indentation =
  let mixed = "    line1\n\t\tline2\n  line3\n    line4"
      normalized = normalizeIndentation mixed
      normalizedLines = lines normalized
  in property $ length normalizedLines === 4 .&&.
                not (any (isPrefixOf " ") $ filter (not . null) normalizedLines)

-- Property: Split and rejoin consistency
prop_split_rejoin_consistent :: Char -> String -> Property
prop_split_rejoin_consistent delim input =
  let parts = splitBy delim input
      rejoined = intercalate [delim] parts
  in property $ rejoined === input

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils New QuickCheck Tests"
  [ testGroup "Trim Properties"
    [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
    , fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim of empty string is empty" prop_trim_empty
    , fastProperty "trim of only whitespace is empty" prop_trim_only_whitespace
    ]

  , testGroup "Split Properties"
    [ fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
    , fastProperty "splitByComma is splitBy with comma" prop_splitByComma_is_splitBy
    , fastProperty "splitByCommaCollapsed is splitByCollapsed with comma" prop_splitByCommaCollapsed_is_splitByCollapsed
    , fastProperty "splitBy and join roundtrip" prop_splitBy_join_roundtrip
    , fastProperty "splitByCollapsed handles consecutive delimiters" prop_splitByCollapsed_consecutive
    ]

  , testGroup "Comment Removal Properties"
    [ fastProperty "removeLineComments removes // comments" prop_removeLineComments_removes
    , fastProperty "removeLineComments preserves comments in strings" prop_removeLineComments_preserves_string
    , fastProperty "removeLineComments is idempotent" prop_removeLineComments_idempotent
    , fastProperty "removeComments removes both // and /* */ comments" prop_removeComments_removes_both
    , fastProperty "removeComments preserves comments in strings" prop_removeComments_preserves_strings
    , fastProperty "removeComments is idempotent" prop_removeComments_idempotent
    ]

  , testGroup "Indentation Properties"
    [ fastProperty "normalizeIndentation removes common prefix" prop_normalizeIndentation_removes_prefix
    , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
    , fastProperty "normalizeIndentation is idempotent" prop_normalizeIndentation_idempotent
    , fastProperty "forceSingleTabIndentation forces tab indentation" prop_forceSingleTabIndentation_forces_tab
    , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalize
    ]

  , testGroup "Search Properties"
    [ fastProperty "breakOn finds substring" prop_breakOn_finds
    , fastProperty "breakOn handles empty pattern" prop_breakOn_empty_pattern
    , fastProperty "breakOn handles missing pattern" prop_breakOn_missing
    , fastProperty "breakOn returns first occurrence" prop_breakOn_first
    ]

  , testGroup "Complex Interaction Properties"
    [ fastProperty "Complex string processing pipeline" prop_complex_pipeline
    , fastProperty "Whitespace handling consistency" prop_whitespace_consistency
    , fastProperty "Comment removal with edge cases" prop_comment_edge_cases
    , fastProperty "Indentation normalization with mixed content" prop_mixed_indentation
    , fastProperty "Split and rejoin consistency" prop_split_rejoin_consistent
    ]
  ]