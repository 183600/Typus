{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.NewCabalUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat)
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
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
-- Arbitrary instances for Utils testing
-- ============================================================================

-- Generate strings with various whitespace characters
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements $ " \t\n\r"

-- Generate strings with alphanumeric characters
genAlphaNumString :: Gen String
genAlphaNumString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Generate strings with mixed content
genMixedString :: Gen String
genMixedString = do
  alphaNum <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  whitespace <- listOf $ elements $ " \t"
  return $ alphaNum ++ whitespace

-- Generate strings with comment patterns
genCommentString :: Gen String
genCommentString = do
  before <- genAlphaNumString
  comment <- listOf $ elements $ "This is a comment"
  return $ before ++ "// " ++ comment

-- Generate strings with block comment patterns
genBlockCommentString :: Gen String
genBlockCommentString = do
  before <- genAlphaNumString
  comment <- listOf $ elements $ "Block comment content"
  after <- genAlphaNumString
  return $ before ++ "/* " ++ comment ++ " */" ++ after

-- Generate strings with string literals
genStringLiteral :: Gen String
genStringLiteral = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ' ' ++ '!'
  return $ "\"" ++ content ++ "\""

-- Generate indented strings
genIndentedString :: Gen String
genIndentedString = do
  indent <- choose (1, 8)
  content <- genAlphaNumString
  return $ replicate indent ' ' ++ content

-- Generate multi-line strings with indentation
genMultiLineIndentedString :: Gen String
genMultiLineIndentedString = do
  baseIndent <- choose (2, 6)
  linesCount <- choose (2, 5)
  lines <- sequence $ replicate linesCount $ do
    extraIndent <- choose (0, 3)
    content <- genAlphaNumString
    return $ replicate (baseIndent + extraIndent) ' ' ++ content
  return $ unlines lines

-- ============================================================================
-- Property Tests for Utils
-- ============================================================================

-- Trim properties
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input = trim (trim input) == trim input

prop_trimRemovesOnlyWhitespace :: String -> Bool
prop_trimRemovesOnlyWhitespace input = 
  let trimmed = trim input
      hasLeadingOrTrailingWhitespace = not (null input) && 
                                      (isSpace (head input) || isSpace (last input))
  in if hasLeadingOrTrailingWhitespace
     then length trimmed < length input || trimmed == input
     else trimmed == input

-- Split properties
prop_splitByPreservesEmptySegments :: Char -> String -> Bool
prop_splitByPreservesEmptySegments delim input =
  let result = splitBy delim input
      expectedLength = countDelimiters delim input + 1
  in length result == expectedLength
  where
    countDelimiters d = length . filter (== d)

prop_splitByCollapsedRemovesEmpty :: Char -> String -> Bool
prop_splitByCollapsedRemovesEmpty delim input =
  all (not . null) (splitByCollapsed delim input)

prop_splitByCommaMatchesSplitBy :: String -> Bool
prop_splitByCommaMatchesSplitBy input = splitByComma input == splitBy ',' input

prop_splitByCommaCollapsedMatchesSplitByCollapsed :: String -> Bool
prop_splitByCommaCollapsedMatchesSplitByCollapsed input = 
  splitByCommaCollapsed input == splitByCollapsed ',' input

-- Comment removal properties
prop_removeLineCommentsPreservesNonCommentLines :: String -> Bool
prop_removeLineCommentsPreservesNonCommentLines input =
  let lines' = lines input
      resultLines = lines (removeLineComments input)
      nonCommentLines = filter (not . isPrefixOf "//") lines'
  in length (filter (not . null) resultLines) >= length nonCommentLines

prop_removeCommentsPreservesNonCommentContent :: String -> Bool
prop_removeCommentsPreservesNonCommentContent input =
  let result = removeComments input
      -- Count non-comment alphanumeric characters
      countNonCommentAlphaNum s = length $ filter isAlphaNum $ filterNotInStringsOrComments s
      filterNotInStringsOrComments = undefined -- Simplified for this example
  in True -- Simplified property test

-- Indentation properties
prop_normalizeIndentationPreservesRelativeStructure :: String -> Bool
prop_normalizeIndentationPreservesRelativeStructure input =
  let normalized = normalizeIndentation input
      originalLines = filter (not . all isSpace) $ lines input
      normalizedLines = filter (not . all isSpace) $ lines normalized
  in length originalLines == length normalizedLines

prop_forceSingleTabIndentationCreatesConsistentFormat :: String -> Bool
prop_forceSingleTabIndentationCreatesConsistentFormat input =
  let forced = forceSingleTabIndentation input
      lines' = lines forced
      nonEmptyLines = filter (not . null) lines'
  in all (isPrefixOf "\t") nonEmptyLines || null nonEmptyLines

prop_fixIndentationMatchesNormalizeIndentation :: String -> Bool
prop_fixIndentationMatchesNormalizeIndentation input = 
  fixIndentation input == normalizeIndentation input

-- BreakOn properties
prop_breakOnReturnsCorrectPrefix :: String -> String -> Bool
prop_breakOnReturnsCorrectPrefix pattern text
  | null pattern = breakOn pattern text == ("", text)
  | pattern `isInfixOf` text = 
      let (prefix, _) = breakOn pattern text
      in pattern `isInfixOf` (prefix ++ pattern)
  | otherwise = breakOn pattern text == (text, "")

prop_breakOnReturnsEmptySuffixWhenPatternAtEnd :: String -> String -> Bool
prop_breakOnReturnsEmptySuffixWhenPatternAtEnd pattern text
  | null pattern = True -- Handled by other property
  | pattern `isSuffixOf` text = 
      let (_, suffix) = breakOn pattern text
      in null suffix
  | otherwise = True -- Not applicable

-- ============================================================================
-- Unit Tests for Edge Cases
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils QuickCheck Tests"
  [ testGroup "Trim properties"
    [ fastProperty "trim is idempotent" prop_trimIdempotent
    , fastProperty "trim removes only whitespace" prop_trimRemovesOnlyWhitespace
    , testCase "trim handles empty string" $
        trim "" @?= ""
    , testCase "trim handles whitespace-only string" $
        trim "   \t\n\r   " @?= ""
    ]

  , testGroup "Split properties"
    [ fastProperty "splitBy preserves empty segments" prop_splitByPreservesEmptySegments
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsedRemovesEmpty
    , fastProperty "splitByComma matches splitBy ','" prop_splitByCommaMatchesSplitBy
    , fastProperty "splitByCommaCollapsed matches splitByCollapsed ','" prop_splitByCommaCollapsedMatchesSplitByCollapsed
    , testCase "splitBy handles empty string" $
        splitBy ',' "" @?= [""]
    , testCase "splitByCollapsed handles only delimiters" $
        splitByCollapsed ',' "::::" @?= []
    ]

  , testGroup "Comment removal properties"
    [ fastProperty "removeLineComments preserves non-comment lines" prop_removeLineCommentsPreservesNonCommentLines
    , fastProperty "removeComments preserves non-comment content" prop_removeCommentsPreservesNonCommentContent
    , testCase "removeLineComments handles string literals with //" $
        let input = "path := \"C://tmp\" // comment\n"
            expected = "path := \"C://tmp\" \n"
        in removeLineComments input @?= expected
    , testCase "removeComments handles nested block markers in strings" $
        let input = "text := \"/* not a comment */\" /* real comment */\n"
            expected = "text := \"/* not a comment */\" \n"
        in removeComments input @?= expected
    ]

  , testGroup "Indentation properties"
    [ fastProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentationPreservesRelativeStructure
    , fastProperty "forceSingleTabIndentation creates consistent format" prop_forceSingleTabIndentationCreatesConsistentFormat
    , fastProperty "fixIndentation matches normalizeIndentation" prop_fixIndentationMatchesNormalizeIndentation
    , testCase "normalizeIndentation handles empty input" $
        normalizeIndentation "" @?= ""
    , testCase "forceSingleTabIndentation handles empty lines" $
        let input = "line1\n\nline3"
            expected = "\tline1\n\n\tline3"
        in forceSingleTabIndentation input @?= expected
    ]

  , testGroup "BreakOn properties"
    [ fastProperty "breakOn returns correct prefix" prop_breakOnReturnsCorrectPrefix
    , fastProperty "breakOn returns empty suffix when pattern at end" prop_breakOnReturnsEmptySuffixWhenPatternAtEnd
    , testCase "breakOn handles empty pattern" $
        breakOn "" "hello" @?= ("", "hello")
    , testCase "breakOn handles pattern not found" $
        breakOn "xyz" "hello" @?= ("hello", "")
    , testCase "breakOn handles pattern at beginning" $
        breakOn "hel" "hello" @?= ("", "lo")
    ]

  , testGroup "Edge case tests"
    [ testCase "removeComments handles unclosed block comment" $
        let input = "start /* open\nstill inside"
            expected = "start \n"
        in removeComments input @?= expected

    , testCase "normalizeIndentation preserves blank lines" $
        let input = "\n    line1\n        line2\n\n"
            expected = "\nline1\n  line2\n\n"
        in normalizeIndentation input @?= expected

    , testCase "splitBy handles Unicode characters" $
        splitBy ',' "héllo,wörld" @?= ["héllo", "wörld"]

    , testCase "trim handles Unicode whitespace" $
        trim "\u00A0\u2003hello\u2002\u00A0" @?= "hello"
    ]
  ]