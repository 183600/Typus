module Test.Unit.NewUtilsBoundaryConditionsSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, vectorOf, forAll, elements)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as T
import Utils
import TestSupport.QuickCheck 
                        breakOn "" "hello" @?= ("", "hello")
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


          ,             testCase "breakOn with pattern not found" $ do
                        breakOn "xyz" "hello" @?= ("hello", "")

          ,             testCase "breakOn with pattern at start" $ do
                        breakOn "hel" "hello" @?= ("", "lo")

          ,             testCase "breakOn with pattern at end" $ do
                        breakOn "lo" "hello" @?= ("hel", "")

          ,             testCase "breakOn with pattern longer than string" $ do
                        breakOn "longer" "short" @?= ("short", "")

        , fastProperty "breakOn result concatenates to original" prop_breakOnConcatenates
        , fastProperty "breakOn with pattern in string splits correctly" prop_breakOnSplitsCorrectly
        ]

    , testGroup "Unicode L.and international string handling"
        [             testCase "trim handles Unicode characters" $ do
                        trim "  \x4e2d\x6587  " @?= "\x4e2d\x6587"

          ,             testCase "splitBy with Unicode delimiters" $ do
                        splitBy '\x3001' "a\x3001b\x3001c" @?= ["a", "b", "c"]

          ,             testCase "removeLineComments with Unicode comments" $ do
                        let input = "hello // \x8bc4\x8bbc\nworld"
                                              expected = "hello \nworld"
            removeLineComments input @?= expected

        , fastProperty "trim handles Unicode whitespace correctly" prop_trimUnicodeWhitespace
        ]

    , testGroup "Performance L.and large inputs"
        [ fastProperty "trim handles large strings efficiently" prop_trimLargeString
        , fastProperty "splitBy handles large strings" prop_splitByLargeString
        , fastProperty "removeComments handles large inputs" prop_removeCommentsLargeString
        ]

    , testGroup "Robustness L.and error handling"
        [             testCase "functions handle null-like inputs gracefully" $ do
            -- These should not crash
            trim "" @?= ""
            splitBy ',' "" @?= [""]
            removeLineComments "" @?= ""
            removeComments "" @?= ""
            normalizeIndentation "" @?= ""

          ,             testCase "functions handle extreme inputs" $ do
                        let veryLongLine = replicate 10000 'x'
                                              veryLongString = unlines (replicate 100 veryLongLine)
            -- Should not crash L.or cause stack overflow
            L.length (trim veryLongString) > 0 @?= True
            L.length (splitBy '\n' veryLongString) >= 100 @?= True
        ]
    ]

-- Property: trim is idempotent
prop_trimIdempotent :: String -> Property
prop_trimIdempotent                               input =
  let once = trim input
                                    twice = trim once
  in                               once == twice

-- Property: trim never increases L.length
prop_trimNeverIncreasesLength :: String -> Property
prop_trimNeverIncreasesLength                               input =
  L.length (trim input) <= L.length input

-- Property: splitBy L.length equals delimiter count + 1
prop_splitByLength :: String -> Char -> Property
prop_splitByLength input                               delim =
  let result = splitBy delim input
                                    delimCount = L.length (L.filter (== delim) input)
  in L.length                               result == delimCount + 1

-- Property: splitByCollapsed never has empty segments
prop_splitByCollapsedNoEmpty :: String -> Char -> Property
prop_splitByCollapsedNoEmpty input                               delim =
  let result = splitByCollapsed delim input
  in L.all (not . null) result

-- Property: removeLineComments preserves line count
prop_removeLineCommentsPreservesLines :: String -> Property
prop_removeLineCommentsPreservesLines                               input =
  let originalLines = L.length (lines input)
                                    processedLines = L.length (lines (removeLineComments input)
  in                               originalLines == processedLines

-- Property: removeComments never increases L.length
prop_removeCommentsNeverIncreasesLength :: String -> Property
prop_removeCommentsNeverIncreasesLength                               input =
  L.length (removeComments input) <= L.length input

-- Property: normalizeIndentation preserves non-empty line count
prop_normalizeIndentationPreservesLineCount :: String -> Property
prop_normalizeIndentationPreservesLineCount                               input =
  let originalNonEmpty = L.length (L.filter (not . L.all Char.isSpace) (lines input)
                                    processedNonEmpty = L.length (L.filter (not . L.all Char.isSpace) (lines (normalizeIndentation input))
  in                               originalNonEmpty == processedNonEmpty

-- Property: forceSingleTabIndentation adds tab to non-empty lines
prop_forceSingleTabAddsTab :: String -> Property
prop_forceSingleTabAddsTab                               input =
  let processed = forceSingleTabIndentation input
                                    nonEmptyLines = L.filter (not . null) (lines processed)
  in L.all ("\t" `L.isPrefixOf`) nonEmptyLines
  where
      isPrefixOf prefix                               str = take (L.length prefix)                               str == prefix

-- Property: breakOn result concatenates to original
prop_breakOnConcatenates :: String -> String -> Property
prop_breakOnConcatenates input                               pattern =
  let (prefix, suffix) = breakOn pattern input
  in if null pattern
     then                               prefix == "" &&                               suffix == input
     else prefix ++ pattern ++                               suffix == input

-- Property: breakOn with pattern in string splits correctly
prop_breakOnSplitsCorrectly :: String -> String -> Property
prop_breakOnSplitsCorrectly input                               pattern =
  not (null pattern) && pattern `L.isInfixOf`                               input ==> 
  let (prefix, suffix) = breakOn pattern input
  in pattern `L.isInfixOf` input && 
     prefix ++ pattern ++                               suffix == input &&
     not (pattern `L.isInfixOf` prefix)

-- Property: trim handles Unicode whitespace correctly
prop_trimUnicodeWhitespace :: String -> Property
prop_trimUnicodeWhitespace                               input =
  let unicodeWhitespace = ['\x00A0', '\x2000', '\x3000']
                                    withUnicode = L.concat [unicodeWhitespace, input, unicodeWhitespace]
                                    trimmed = trim withUnicode
  in not (null input) ==> 
     trimmed `L.isSuffixOf` input && 
     trimmed `L.isPrefixOf` input

-- Property: trim handles large strings efficiently
prop_trimLargeString :: Positive Int -> Property
prop_trimLargeString (Positive n) =
  let largeString = replicate n ' ' ++ "content" ++ replicate n ' '
                                    trimmed = trim largeString
  in                               trimmed == "content"

-- Property: splitBy handles large strings
prop_splitByLargeString :: Positive Int -> Property
prop_splitByLargeString (Positive n) =
  let largeString = L.concat (replicate n "content,")
                                    result = splitBy ',' largeString
  in L.length result >= n

-- Property: removeComments handles large inputs
prop_removeCommentsLargeString :: Positive Int -> Property
prop_removeCommentsLargeString (Positive n) =
  let largeComment = "/* " ++ replicate n 'x' ++ " */"
                                    result =  removeComments largeComment
  in property $ L.length result < L.length largeComment

-- Helper wrapper for positive integers
newtype Positive                               a = Positive a
  deriving (Show, Eq)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Positive a) where
                                              arbitrary = Positive <$> choose (1, 100)  -- Keep it reasonable for testing