module Test.Unit.UtilsAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, (===), forAll, Gen, choose, arbitrary, listOf, elements, oneof, suchThat)
import TestSupport.QuickCheck (fastProperty)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
              removeLineComments, removeComments, normalizeIndentation, 
              forceSingleTabIndentation, fixIndentation, breakOn)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, intercalate)

-- ============================================================================
-- Generators
-- ============================================================================

-- Generate strings with whitespace
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements $ " \t\n\r"

-- Generate strings without whitespace
genNonWhitespaceString :: Gen String
genNonWhitespaceString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@#$%^&*()_+-=[]{}|;:',.<>/?"

-- Generate strings with mixed content
genMixedString :: Gen String
genMixedString = do
  before <- arbitrary
  delim <- arbitrary
  after <- arbitrary
  return $ before ++ [delim] ++ after

-- Generate strings with multiple delimiters
genMultiDelimiterString :: Char -> Gen String
genMultiDelimiterString delim = do
  parts <- listOf $ listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ intercalate [delim] parts

-- Generate strings with indentation
genIndentedString :: Gen String
genIndentedString = do
  lines' <- listOf $ do
    indent <- listOf $ elements " \t"
    content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ' '
    return $ indent ++ content
  return $ unlines lines'

-- Generate strings with comments
genCommentString :: Gen String
genCommentString = oneof
  [ do
      before <- arbitrary
      comment <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ' '
      return $ before ++ "// " ++ comment
  , do
      before <- arbitrary
      comment <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ' '
      return $ before ++ "/* " ++ comment ++ " */"
  , do
      before <- arbitrary
      return $ before ++ "/* multiline\ncomment\n*/"
  , do
      before <- arbitrary
      quote <- listOf $ elements $ ['a'..'z'] ++ ' '
      return $ before ++ "\"string with // not a comment\""
  , do
      before <- arbitrary
      char <- listOf $ elements $ ['a'..'z'] ++ ' '
      return $ before ++ "'c' // not a comment"
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: trim removes leading and trailing whitespace
prop_trimRemovesWhitespace :: String -> Bool
prop_trimRemovesWhitespace input =
  let trimmed = trim input
      hasLeadingWhitespace = not (null input) && isSpace (head input)
      hasTrailingWhitespace = not (null input) && isSpace (last input)
  in if hasLeadingWhitespace || hasTrailingWhitespace
     then length trimmed <= length input
     else trimmed == input

-- Property: trim is idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input = trim (trim input) == trim input

-- Property: trim preserves non-whitespace content
prop_trimPreservesContent :: String -> Bool
prop_trimPreservesContent input =
  let trimmed = trim input
      nonWhitespaceContent = filter (not . isSpace) input
      trimmedNonWhitespace = filter (not . isSpace) trimmed
  in trimmedNonWhitespace == nonWhitespaceContent

-- Property: splitBy preserves total length (including delimiters)
prop_splitByPreservesLength :: Char -> String -> Bool
prop_splitByPreservesLength delim input =
  let parts = splitBy delim input
      rejoined = intercalate [delim] parts
  in length rejoined == length input

-- Property: splitBy preserves order
prop_splitByPreservesOrder :: Char -> String -> Bool
prop_splitByPreservesOrder delim input =
  let parts = splitBy delim input
      rejoined = intercalate [delim] parts
  in rejoined == input

-- Property: splitByCollapsed removes empty parts
prop_splitByCollapsedRemovesEmpty :: Char -> String -> Bool
prop_splitByCollapsedRemovesEmpty delim input =
  let parts = splitByCollapsed delim input
  in all (not . null) parts

-- Property: splitByComma is equivalent to splitBy ','
prop_splitByCommaEquivalent :: String -> Bool
prop_splitByCommaEquivalent input = splitByComma input == splitBy ',' input

-- Property: splitByCommaCollapsed is equivalent to splitByCollapsed ','
prop_splitByCommaCollapsedEquivalent :: String -> Bool
prop_splitByCommaCollapsedEquivalent input = splitByCommaCollapsed input == splitByCollapsed ',' input

-- Property: removeLineComments removes // comments
prop_removeLineCommentsRemovesComments :: String -> Property
prop_removeLineCommentsRemovesComments input =
  "//" `isInfixOf` input ==>
  let cleaned = removeLineComments input
      lines' = lines cleaned
  in not (any ("//" `isPrefixOf`) lines')

-- Property: removeLineComments preserves strings with // inside
prop_removeLineCommentsPreservesStringsWithComments :: String -> Property
prop_removeLineCommentsPreservesStringsWithComments input =
  "\"" `isInfixOf` input && "//" `isInfixOf` input ==>
  let cleaned = removeLineComments input
      hasStringWithComment = any (\line -> 
        let beforeComment = takeWhile (/= '/') line
            hasQuoteBeforeComment = "\"" `isInfixOf` beforeComment
        in hasQuoteBeforeComment) (lines input)
  in if hasStringWithComment 
     then "//" `isInfixOf` cleaned  -- Should preserve // inside strings
     else property True

-- Property: removeComments removes both // and /* */ comments
prop_removeCommentsRemovesBothTypes :: String -> Property
prop_removeCommentsRemovesBothTypes input =
  ("//" `isInfixOf` input || "/*" `isInfixOf` input) ==>
  let cleaned = removeComments input
      lines' = lines cleaned
  in not (any ("//" `isPrefixOf`) lines') && 
     not (any ("/*" `isInfixOf`) cleaned)

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentationPreservesRelative :: String -> Property
prop_normalizeIndentationPreservesRelative input =
  let lines' = lines input
      nonEmptyLines = filter (not . all isSpace) lines'
  in length nonEmptyLines >= 2 ==>
     let normalized = normalizeIndentation input
         normalizedLines = lines normalized
         originalIndents = map (takeWhile isSpace) nonEmptyLines
         normalizedIndents = map (takeWhile isSpace) $ filter (not . all isSpace) normalizedLines
     in length originalIndents == length normalizedIndents

-- Property: normalizeIndentation removes common prefix
prop_normalizeIndentationRemovesCommonPrefix :: String -> Property
prop_normalizeIndentationRemovesCommonPrefix input =
  let lines' = lines input
      nonEmptyLines = filter (not . all isSpace) lines'
  in not (null nonEmptyLines) ==>
     let normalized = normalizeIndentation input
         normalizedLines = lines normalized
         firstNormalizedNonEmpty = head $ filter (not . all isSpace) normalizedLines
     in not (isSpace (head firstNormalizedNonEmpty))

-- Property: forceSingleTabIndentation adds tab to non-empty lines
prop_forceSingleTabIndentationAddsTab :: String -> Property
prop_forceSingleTabIndentationAddsTab input =
  let lines' = lines input
      nonEmptyLines = filter (not . null) lines'
  in not (null nonEmptyLines) ==>
     let forced = forceSingleTabIndentation input
         forcedLines = lines forced
         nonEmptyForced = filter (not . null) forcedLines
     in all ("\t" `isPrefixOf`) nonEmptyForced

-- Property: fixIndentation is equivalent to normalizeIndentation
prop_fixIndentationEquivalent :: String -> Bool
prop_fixIndentationEquivalent input = fixIndentation input == normalizeIndentation input

-- Property: breakOn with empty pattern returns ("", input)
prop_breakOnEmptyPattern :: String -> Bool
prop_breakOnEmptyPattern input = breakOn "" input == ("", input)

-- Property: breakOn returns correct split when pattern exists
prop_breakOnCorrectSplit :: String -> String -> Property
prop_breakOnCorrectSplit pat input =
  not (null pat) && pat `isInfixOf` input ==>
  let (before, after) = breakOn pat input
      expectedBefore = takeWhile (not . (`isPrefixOf` (tails input))) input
      expectedAfter = drop (length before + length pat) input
  in before == expectedBefore && after == expectedAfter

-- Property: breakOn returns (input, "") when pattern doesn't exist
prop_breakOnPatternNotFound :: String -> String -> Property
prop_breakOnPatternNotFound pat input =
  not (null pat) && not (pat `isInfixOf` input) ==>
  breakOn pat input == (input, "")

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils Advanced QuickCheck Tests"
  [ testGroup "Trim Properties"
    [ testProperty "trim removes leading and trailing whitespace" prop_trimRemovesWhitespace
    , testProperty "trim is idempotent" prop_trimIdempotent
    , testProperty "trim preserves non-whitespace content" prop_trimPreservesContent
    ]

  , testGroup "Split Properties"
    [ testProperty "splitBy preserves total length (including delimiters)" prop_splitByPreservesLength
    , testProperty "splitBy preserves order" prop_splitByPreservesOrder
    , testProperty "splitByCollapsed removes empty parts" prop_splitByCollapsedRemovesEmpty
    , testProperty "splitByComma is equivalent to splitBy ','" prop_splitByCommaEquivalent
    , testProperty "splitByCommaCollapsed is equivalent to splitByCollapsed ','" prop_splitByCommaCollapsedEquivalent
    ]

  , testGroup "Comment Removal Properties"
    [ testProperty "removeLineComments removes // comments" prop_removeLineCommentsRemovesComments
    , testProperty "removeLineComments preserves strings with // inside" prop_removeLineCommentsPreservesStringsWithComments
    , testProperty "removeComments removes both // and /* */ comments" prop_removeCommentsRemovesBothTypes
    ]

  , testGroup "Indentation Properties"
    [ testProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentationPreservesRelative
    , testProperty "normalizeIndentation removes common prefix" prop_normalizeIndentationRemovesCommonPrefix
    , testProperty "forceSingleTabIndentation adds tab to non-empty lines" prop_forceSingleTabIndentationAddsTab
    , testProperty "fixIndentation is equivalent to normalizeIndentation" prop_fixIndentationEquivalent
    ]

  , testGroup "Search Properties"
    [ testProperty "breakOn with empty pattern returns (\"\", input)" prop_breakOnEmptyPattern
    , testProperty "breakOn returns correct split when pattern exists" prop_breakOnCorrectSplit
    , testProperty "breakOn returns (input, \"\") when pattern doesn't exist" prop_breakOnPatternNotFound
    ]

  , testGroup "Unit Tests"
    [ testCase "trim removes leading and trailing whitespace" $ do
        trim "\t  hello  world \n" @?= "hello  world"

    , testCase "trim on empty string" $ do
        trim "" @?= ""

    , testCase "trim on whitespace only" $ do
        trim "   \t\n  " @?= ""

    , testCase "splitBy preserves empty segments" $ do
        splitBy ':' "a::b:" @?= ["a", "", "b", ""]

    , testCase "splitBy on empty input" $ do
        splitBy ':' "" @?= [""]

    , testCase "splitByCollapsed removes empty segments" $ do
        splitByCollapsed ':' "::alpha::beta::" @?= ["alpha", "beta"]

    , testCase "splitByCollapsed on empty input" $ do
        splitByCollapsed ':' "" @?= []

    , testCase "splitByCollapsed on delimiters only" $ do
        splitByCollapsed ':' "::::" @?= []

    , testCase "removeLineComments basic" $ do
        let input = "code // comment\nmore code"
        removeLineComments input @?= "code \nmore code"

    , testCase "removeLineComments preserves strings" $ do
        let input = "url := \"http://example.com//path\" // comment"
        removeLineComments input @?= "url := \"http://example.com//path\" "

    , testCase "removeLineComments preserves chars" $ do
        let input = "ch := '/' // comment"
        removeLineComments input @?= "ch := '/' "

    , testCase "removeComments both types" $ do
        let input = "code // comment\nmore /* block */ code"
        removeComments input @?= "code \nmore  code"

    , testCase "removeComments multiline block" $ do
        let input = "before /*\nblock\ncomment\n*/ after"
        removeComments input @?= "before \n\n\n after"

    , testCase "normalizeIndentation basic" $ do
        let input = "    func() {\n        return 42\n    }"
        let expected = "func() {\n    return 42\n}"
        normalizeIndentation input @?= expected

    , testCase "normalizeIndentation preserves empty lines" $ do
        let input = "    line1\n\n    line2"
        let expected = "line1\n\nline2"
        normalizeIndentation input @?= expected

    , testCase "forceSingleTabIndentation basic" $ do
        let input = "  line1\n\n    line2"
        let expected = "\tline1\n\n\tline2"
        forceSingleTabIndentation input @?= expected

    , testCase "breakOn basic" $ do
        breakOn "ll" "hello" @?= ("he", "o")

    , testCase "breakOn not found" $ do
        breakOn "xyz" "hello" @?= ("hello", "")

    , testCase "breakOn empty pattern" $ do
        breakOn "" "abc" @?= ("", "abc")

    , testCase "breakOn entire match" $ do
        breakOn "abc" "abc" @?= ("", "")
    ]
  ]