{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewUtilsBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, vectorOf, forAll, elements)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as T

import Utils
import TestSupport.QuickCheck (fastProperty)

-- | Test utility functions with boundary conditions
tests :: TestTree
tests =
  testGroup "New Utils Boundary Tests"
    [ testGroup "String trimming edge cases"
        [ testCase "trim handles empty string" $ do
            trim "" @?= ""

        , testCase "trim handles only whitespace" $ do
            trim "   \t\n\r  " @?= ""

        , testCase "trim preserves internal whitespace" $ do
            trim "  hello   world  " @?= "hello   world"

        , testCase "trim handles Unicode whitespace" $ do
            trim "\x2000\u3000hello\x2000" @?= "hello"

        , fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "trim never increases length" prop_trimNeverIncreasesLength
        ]

    , testGroup "String splitting edge cases"
        [ testCase "splitBy on empty string returns single empty" $ do
            splitBy ',' "" @?= [""]

        , testCase "splitBy with delimiter not in string returns single element" $ do
            splitBy ',' "hello" @?= ["hello"]

        , testCase "splitBy with only delimiters" $ do
            splitBy ',' ",,," @?= ["", "", "", ""]

        , testCase "splitByCollapsed on empty string returns empty" $ do
            splitByCollapsed ',' "" @?= []

        , testCase "splitByCollapsed removes all empty segments" $ do
            splitByCollapsed ',' ",a,,b,," @?= ["a", "b"]

        , testCase "splitByCollapsed with only delimiters returns empty" $ do
            splitByCollapsed ',' ",,," @?= []

        , fastProperty "splitBy length equals delimiter count + 1" prop_splitByLength
        , fastProperty "splitByCollapsed never has empty segments" prop_splitByCollapsedNoEmpty
        ]

    , testGroup "Comment removal edge cases"
        [ testCase "removeLineComments on empty string" $ do
            removeLineComments "" @?= ""

        , testCase "removeLineComments with no comments" $ do
            removeLineComments "hello\nworld" @?= "hello\nworld"

        , testCase "removeLineComments with only comments" $ do
            removeLineComments "// comment 1\n// comment 2" @?= "\n"

        , testCase "removeLineComments handles escaped quotes" $ do
            let input = "s := \"hello // not comment\" // real comment\n"
                expected = "s := \"hello // not comment\" \n"
            removeLineComments input @?= expected

        , testCase "removeComments on empty string" $ do
            removeComments "" @?= ""

        , testCase "removeComments with only block comments" $ do
            removeComments "/* comment */" @?= " "

        , testCase "removeComments handles nested quotes in comments" $ do
            let input = "text /* \"quoted\" */ more"
                expected = "text  more"
            removeComments input @?= expected

        , testCase "removeComments handles unterminated block comments" $ do
            let input = "start /* unterminated\nend"
                expected = "start \n"
            removeComments input @?= expected

        , fastProperty "removeLineComments preserves line count" prop_removeLineCommentsPreservesLines
        , fastProperty "removeComments never increases length" prop_removeCommentsNeverIncreasesLength
        ]

    , testGroup "Indentation handling edge cases"
        [ testCase "normalizeIndentation on empty string" $ do
            normalizeIndentation "" @?= ""

        , testCase "normalizeIndentation on only whitespace" $ do
            normalizeIndentation "   \n\t\n  \t\n" @?= "   \n\t\n  \t\n"

        , testCase "normalizeIndentation preserves empty lines" $ do
            let input = "\n\n  line\n\n"
                expected = "\n\nline\n\n"
            normalizeIndentation input @?= expected

        , testCase "forceSingleTabIndentation on empty string" $ do
            forceSingleTabIndentation "" @?= ""

        , testCase "forceSingleTabIndentation handles only whitespace lines" $ do
            let input = "  \n\t\n   \n"
                expected = "\n\n\n"
            forceSingleTabIndentation input @?= expected

        , testCase "forceSingleTabIndentation collapses leading whitespace" $ do
            let input = "    line\n\t\ttab\n  mixed\n"
                expected = "\tline\n\ttab\n\tmixed\n"
            forceSingleTabIndentation input @?= expected

        , fastProperty "normalizeIndentation preserves non-empty line count" prop_normalizeIndentationPreservesLineCount
        , fastProperty "forceSingleTabIndentation adds tab to non-empty lines" prop_forceSingleTabAddsTab
        ]

    , testGroup "Search helper edge cases"
        [ testCase "breakOn with empty pattern" $ do
            breakOn "" "hello" @?= ("", "hello")

        , testCase "breakOn with pattern not found" $ do
            breakOn "xyz" "hello" @?= ("hello", "")

        , testCase "breakOn with pattern at start" $ do
            breakOn "hel" "hello" @?= ("", "lo")

        , testCase "breakOn with pattern at end" $ do
            breakOn "lo" "hello" @?= ("hel", "")

        , testCase "breakOn with pattern longer than string" $ do
            breakOn "longer" "short" @?= ("short", "")

        , fastProperty "breakOn result concatenates to original" prop_breakOnConcatenates
        , fastProperty "breakOn with pattern in string splits correctly" prop_breakOnSplitsCorrectly
        ]

    , testGroup "Unicode and international string handling"
        [ testCase "trim handles Unicode characters" $ do
            trim "  \x4e2d\x6587  " @?= "\x4e2d\x6587"

        , testCase "splitBy with Unicode delimiters" $ do
            splitBy '\x3001' "a\x3001b\x3001c" @?= ["a", "b", "c"]

        , testCase "removeLineComments with Unicode comments" $ do
            let input = "hello // \x8bc4\x8bbc\nworld"
                expected = "hello \nworld"
            removeLineComments input @?= expected

        , fastProperty "trim handles Unicode whitespace correctly" prop_trimUnicodeWhitespace
        ]

    , testGroup "Performance and large inputs"
        [ fastProperty "trim handles large strings efficiently" prop_trimLargeString
        , fastProperty "splitBy handles large strings" prop_splitByLargeString
        , fastProperty "removeComments handles large inputs" prop_removeCommentsLargeString
        ]

    , testGroup "Robustness and error handling"
        [ testCase "functions handle null-like inputs gracefully" $ do
            -- These should not crash
            trim "" @?= ""
            splitBy ',' "" @?= [""]
            removeLineComments "" @?= ""
            removeComments "" @?= ""
            normalizeIndentation "" @?= ""

        , testCase "functions handle extreme inputs" $ do
            let veryLongLine = replicate 10000 'x'
                veryLongString = unlines (replicate 100 veryLongLine)
            -- Should not crash or cause stack overflow
            length (trim veryLongString) > 0 @?= True
            length (splitBy '\n' veryLongString) >= 100 @?= True
        ]
    ]

-- Property: trim is idempotent
prop_trimIdempotent :: String -> Property
prop_trimIdempotent input =
  let once = trim input
      twice = trim once
  in once == twice

-- Property: trim never increases length
prop_trimNeverIncreasesLength :: String -> Property
prop_trimNeverIncreasesLength input =
  length (trim input) <= length input

-- Property: splitBy length equals delimiter count + 1
prop_splitByLength :: String -> Char -> Property
prop_splitByLength input delim =
  let result = splitBy delim input
      delimCount = length (filter (== delim) input)
  in length result == delimCount + 1

-- Property: splitByCollapsed never has empty segments
prop_splitByCollapsedNoEmpty :: String -> Char -> Property
prop_splitByCollapsedNoEmpty input delim =
  let result = splitByCollapsed delim input
  in all (not . null) result

-- Property: removeLineComments preserves line count
prop_removeLineCommentsPreservesLines :: String -> Property
prop_removeLineCommentsPreservesLines input =
  let originalLines = length (lines input)
      processedLines = length (lines (removeLineComments input))
  in originalLines == processedLines

-- Property: removeComments never increases length
prop_removeCommentsNeverIncreasesLength :: String -> Property
prop_removeCommentsNeverIncreasesLength input =
  length (removeComments input) <= length input

-- Property: normalizeIndentation preserves non-empty line count
prop_normalizeIndentationPreservesLineCount :: String -> Property
prop_normalizeIndentationPreservesLineCount input =
  let originalNonEmpty = length (filter (not . all Char.isSpace) (lines input))
      processedNonEmpty = length (filter (not . all Char.isSpace) (lines (normalizeIndentation input)))
  in originalNonEmpty == processedNonEmpty

-- Property: forceSingleTabIndentation adds tab to non-empty lines
prop_forceSingleTabAddsTab :: String -> Property
prop_forceSingleTabAddsTab input =
  let processed = forceSingleTabIndentation input
      nonEmptyLines = filter (not . null) (lines processed)
  in all ("\t" `isPrefixOf`) nonEmptyLines
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- Property: breakOn result concatenates to original
prop_breakOnConcatenates :: String -> String -> Property
prop_breakOnConcatenates input pattern =
  let (prefix, suffix) = breakOn pattern input
  in if null pattern
     then prefix == "" && suffix == input
     else prefix ++ pattern ++ suffix == input

-- Property: breakOn with pattern in string splits correctly
prop_breakOnSplitsCorrectly :: String -> String -> Property
prop_breakOnSplitsCorrectly input pattern =
  not (null pattern) && pattern `isInfixOf` input ==> 
  let (prefix, suffix) = breakOn pattern input
  in pattern `isInfixOf` input && 
     prefix ++ pattern ++ suffix == input &&
     not (pattern `isInfixOf` prefix)

-- Property: trim handles Unicode whitespace correctly
prop_trimUnicodeWhitespace :: String -> Property
prop_trimUnicodeWhitespace input =
  let unicodeWhitespace = ['\x00A0', '\x2000', '\x3000']
      withUnicode = concat [unicodeWhitespace, input, unicodeWhitespace]
      trimmed = trim withUnicode
  in not (null input) ==> 
     trimmed `isSuffixOf` input && 
     trimmed `isPrefixOf` input

-- Property: trim handles large strings efficiently
prop_trimLargeString :: Positive Int -> Property
prop_trimLargeString (Positive n) =
  let largeString = replicate n ' ' ++ "content" ++ replicate n ' '
      trimmed = trim largeString
  in trimmed == "content"

-- Property: splitBy handles large strings
prop_splitByLargeString :: Positive Int -> Property
prop_splitByLargeString (Positive n) =
  let largeString = concat (replicate n "content,")
      result = splitBy ',' largeString
  in length result >= n

-- Property: removeComments handles large inputs
prop_removeCommentsLargeString :: Positive Int -> Property
prop_removeCommentsLargeString (Positive n) =
  let largeComment = "/* " ++ replicate n 'x' ++ " */"
      result = removeComments largeComment
  in length result < length largeComment

-- Helper wrapper for positive integers
newtype Positive a = Positive a
  deriving (Show, Eq)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> choose (1, 100)  -- Keep it reasonable for testing