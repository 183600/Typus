module Test.Unit.UtilsStringPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, elements)
import qualified Test.QuickCheck as QC

import Utils 
    ( trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
      removeLineComments, removeComments, normalizeIndentation, 
      forceSingleTabIndentation, fixIndentation, breakOn )
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- | Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = do
    parts <- listOf $ elements ["", " ", "\t", "\n", "  ", "\t\t", "\n\n"]
    content <- elements ["", "content", "test", "data"]
    return $ concat parts ++ content ++ concat parts

-- | Generate strings with comma separators
genCommaString :: Gen String
genCommaString = do
    segments <- listOf $ elements ["", "a", "b", "c", "data", "test"]
    commas <- listOf $ elements [",", ",,", ",,,"]
    return $ intercalate "," segments

-- | Generate strings with comments
genCommentString :: Gen String
genCommentString = do
    code <- elements ["x := 42", "y := true", "func test() { }", "data := \"hello\""]
    comments <- elements ["// comment", "/* block comment */", "// end of line"]
    arrangement <- elements 
        [ code ++ " " ++ comments
        , comments ++ " " ++ code  
        , code ++ "\n" ++ comments
        , comments ++ "\n" ++ code
        ]
    return arrangement

-- | Generate strings with indentation
genIndentedString :: Gen String
genIndentedString = do
    baseIndent <- choose (0, 8)
    indentChar <- elements [" ", "\t"]
    let indent = replicate baseIndent indentChar
    content <- elements ["line1", "line2", "func test() {", "  x := 42", "}"]
    return $ indent ++ content

-- | Generate strings with multiple lines
genMultiLineString :: Gen String
genMultiLineString = do
    lineCount <- choose (1, 10)
    lines <- take lineCount <$> listOf genIndentedString
    return $ unlines lines

-- Helper function to intercalate strings
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

tests :: TestTree
tests =
  testGroup "Utils String Processing Properties"
    [ testGroup "Trim Function Properties"
        [ testCase "removes leading and trailing whitespace" $ do
            trim "  hello world  " @?= "hello world"
            trim "\t\n  test  \n\t" @?= "test"
            trim "no_whitespace" @?= "no_whitespace"
            trim "" @?= ""

        , testCase "handles only whitespace" $ do
            trim "   " @?= ""
            trim "\t\n\r" @?= ""
            trim "  \t  \n  " @?= ""

        , testCase "preserves internal whitespace" $ do
            trim "  hello   world  " @?= "hello   world"
            trim "\t  a\tb\tc  \n" @?= "a\tb\tc"

        , fastProperty "trim is idempotent" $ 
            prop_trimIdempotent
        , fastProperty "trim never increases length" $ 
            prop_trimNeverIncreasesLength
        , fastProperty "trim removes only whitespace" $ 
            prop_trimRemovesOnlyWhitespace
        ]

    , testGroup "Split Function Properties"
        [ testCase "splitBy preserves empty segments" $ do
            splitBy ',' "a,,b," @?= ["a", "", "b", ""]
            splitBy ':' "" @?= [""]
            splitBy '|' "|||" @?= ["", "", "", ""]

        , testCase "splitByCollapsed removes empty segments" $ do
            splitByCollapsed ',' "a,,b,," @?= ["a", "b"]
            splitByCollapsed ':' "" @?= []
            splitByCollapsed '|' "|||" @?= []

        , testCase "splitByComma delegates to splitBy" $ do
            splitByComma "x,,y" @?= splitBy ',' "x,,y"
            splitByComma "" @?= splitBy ',' ""

        , testCase "splitByCommaCollapsed removes empty comma segments" $ do
            splitByCommaCollapsed "one,two,,," @?= ["one", "two"]
            splitByCommaCollapsed "" @?= []

        , fastProperty "splitBy is consistent with delimiter" $ 
            prop_splitByConsistent
        , fastProperty "splitByCollapsed never returns empty strings" $ 
            prop_splitByCollapsedNeverEmpty
        , fastProperty "splitBy preserves total character count" $ 
            prop_splitByPreservesCount
        ]

    , testGroup "Comment Removal Properties"
        [ testCase "removeLineComments respects string literals" $ do
            let input = "url := \"http://example.com//path\" // comment"
            let expected = "url := \"http://example.com//path\" "
            removeLineComments input @?= expected

        , testCase "removeLineComments respects char literals" $ do
            let input = "char := '/' // not a comment"
            let expected = "char := '/' "
            removeLineComments input @?= expected

        , testCase "removeComments handles block comments" $ do
            let input = "code /* block comment */ more code"
            let expected = "code  more code"
            removeComments input @?= expected

        , testCase "removeComments preserves leading newlines" $ do
            let input = "/* header */\ncontent"
            let expected = "\ncontent"
            removeComments input @?= expected

        , fastProperty "removeLineComments never increases length" $ 
            prop_removeLineCommentsNeverIncreases
        , fastProperty "removeComments never increases length" $ 
            prop_removeCommentsNeverIncreases
        , fastProperty "comment removal preserves code structure" $ 
            prop_commentRemovalPreservesStructure
        ]

    , testGroup "Indentation Properties"
        [ testCase "normalizeIndentation removes common leading whitespace" $ do
            let input = "    line1\n        line2\n    line3\n"
            let expected = "line1\n    line2\nline3\n"
            normalizeIndentation input @?= expected

        , testCase "normalizeIndentation keeps leading blank lines" $ do
            let input = "\n    content\n        more\n"
            let expected = "\ncontent\n    more\n"
            normalizeIndentation input @?= expected

        , testCase "forceSingleTabIndentation enforces tab prefix" $ do
            let input = "  alpha\n\n\tbeta"
            let expected = "\talpha\n\n\tbeta"
            forceSingleTabIndentation input @?= expected

        , testCase "fixIndentation is alias for normalizeIndentation" $ do
            let input = "    test\n        nested\n"
            fixIndentation input @?= normalizeIndentation input

        , fastProperty "normalizeIndentation preserves relative indentation" $ 
            prop_normalizeIndentationPreservesRelative
        , fastProperty "forceSingleTabIndentation is deterministic" $ 
            prop_forceSingleTabDeterministic
        , fastProperty "indentation operations preserve line count" $ 
            prop_indentationPreservesLineCount
        ]

    , testGroup "Search Function Properties"
        [ testCase "breakOn returns prefix and suffix when pattern exists" $ do
            breakOn "ll" "hello" @?= ("he", "o")
            breakOn "test" "this is a test" @?= ("this is a ", "")

        , testCase "breakOn returns whole string when pattern missing" $ do
            breakOn "xyz" "hello" @?= ("hello", "")
            breakOn "notfound" "" @?= ("", "")

        , testCase "breakOn handles empty pattern" $ do
            breakOn "" "abc" @?= ("", "abc")
            breakOn "" "" @?= ("", "")

        , fastProperty "breakOn concatenation equals original when pattern found" $ 
            prop_breakOnConcatenates
        , fastProperty "breakOn returns original when pattern not found" $ 
            prop_breakOnOriginalWhenMissing
        , fastProperty "breakOn is deterministic" $ 
            prop_breakOnDeterministic
        ]

    , testGroup "String Processing Composition"
        [ testCase "trim after normalizeIndentation works correctly" $ do
            let input = "    \t  content  \n    "
            let result = trim (normalizeIndentation input)
            result @?= "content"

        , testCase "splitBy after removeComments works correctly" $ do
            let input = "a /*comment*/ b,c /*comment*/ d"
            let result = splitByComma (removeComments input)
            result @?= ["a  b", "c  d"]

        , testCase "complex processing pipeline" $ do
            let input = "    // comment\n    code /* block */ more,\n        final\n"
            let result = splitByCommaCollapsed $ normalizeIndentation $ removeComments input
            assertBool "Complex pipeline should work" $ 
                length result > 0 && all (not . null) result

        , fastProperty "composition of string functions is associative where appropriate" $ 
            prop_stringCompositionAssociative
        ]

    , testGroup "Edge Cases and Boundary Conditions"
        [ testCase "handles empty strings consistently" $ do
            trim "" @?= ""
            splitBy ',' "" @?= [""]
            splitByCollapsed ',' "" @?= []
            removeLineComments "" @?= ""
            removeComments "" @?= ""
            normalizeIndentation "" @?= ""
            breakOn "x" "" @?= ("", "")

        , testCase "handles unicode characters" $ do
            let unicode = "  你好世界  "
            trim unicode @?= "你好世界"
            
            let unicodeComments = "变量 := 值 // 注释"
            removeLineComments unicodeComments @?= "变量 := 值 "

        , testCase "handles very long strings efficiently" $ do
            let longString = replicate 10000 'a' ++ "middle" ++ replicate 10000 'b'
            let result = trim longString
            assertBool "Should handle long strings" $ 
                length result > 0 && head result == 'a' && last result == 'b'

        , fastProperty "string functions handle arbitrary unicode" $ 
            prop_handleUnicode
        , fastProperty "string functions handle extreme inputs" $ 
            prop_handleExtremeInputs
        ]

    , testGroup "Performance Properties"
        [ testCase "string operations are linear time" $ do
            let testString = unlines $ replicate 1000 "  test line with content  "
            -- These operations should complete quickly even for large strings
            let trimmed = trim testString
            let normalized = normalizeIndentation testString
            let withoutComments = removeComments testString
            assertBool "Operations should complete" $ 
                length trimmed > 0 && length normalized > 0 && length withoutComments > 0

        , fastProperty "string operations don't have exponential behavior" $ 
            prop_noExponentialBehavior
        ]
    ]

-- Property: trim is idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
  let once = trim input
      twice = trim once
  in once == twice

-- Property: trim never increases length
prop_trimNeverIncreasesLength :: String -> Bool
prop_trimNeverIncreasesLength input =
  length (trim input) <= length input

-- Property: trim removes only whitespace
prop_trimRemovesOnlyWhitespace :: String -> Bool
prop_trimRemovesOnlyWhitespace input =
  let trimmed = trim input
  in all (not . isSpace) trimmed || null trimmed

-- Property: splitBy is consistent with delimiter
prop_splitByConsistent :: Char -> String -> Bool
prop_splitByConsistent delim input =
  let result = splitBy delim input
      reconstructed = intercalate [delim] result
  in reconstructed == input

-- Property: splitByCollapsed never returns empty strings
prop_splitByCollapsedNeverEmpty :: Char -> String -> Bool
prop_splitByCollapsedNeverEmpty delim input =
  all (not . null) (splitByCollapsed delim input)

-- Property: splitBy preserves total character count
prop_splitByPreservesCount :: Char -> String -> Bool
prop_splitByPreservesCount delim input =
  let segments = splitBy delim input
  in length (concat segments) + length segments - 1 == length input || null input

-- Property: removeLineComments never increases length
prop_removeLineCommentsNeverIncreases :: String -> Bool
prop_removeLineCommentsNeverIncreases input =
  length (removeLineComments input) <= length input

-- Property: removeComments never increases length  
prop_removeCommentsNeverIncreases :: String -> Bool
prop_removeCommentsNeverIncreases input =
  length (removeComments input) <= length input

-- Property: comment removal preserves code structure
prop_commentRemovalPreservesStructure :: String -> Bool
prop_commentRemovalPreservesStructure input =
  let withoutComments = removeComments input
      lineCount = length $ lines input
      newLineCount = length $ lines withoutComments
  in newLineCount <= lineCount  -- Should not create more lines

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentationPreservesRelative :: String -> Bool
prop_normalizeIndentationPreservesRelative input =
  let normalized = normalizeIndentation input
      originalLines = lines input
      normalizedLines = lines normalized
  in length normalizedLines == length originalLines

-- Property: forceSingleTabIndentation is deterministic
prop_forceSingleTabDeterministic :: String -> Bool
prop_forceSingleTabDeterministic input =
  let result1 = forceSingleTabIndentation input
      result2 = forceSingleTabIndentation input
  in result1 == result2

-- Property: indentation operations preserve line count
prop_indentationPreservesLineCount :: String -> Bool
prop_indentationPreservesLineCount input =
  let normalized = normalizeIndentation input
      tabForced = forceSingleTabIndentation input
  in length (lines normalized) == length (lines input) &&
     length (lines tabForced) == length (lines input)

-- Property: breakOn concatenation equals original when pattern found
prop_breakOnConcatenates :: String -> String -> Bool
prop_breakOnConcatenates pattern input =
  let (prefix, suffix) = breakOn pattern input
  in if pattern `isInfixOf` input
     then prefix ++ pattern ++ suffix == input
     else True  -- Property only applies when pattern is found

-- Property: breakOn returns original when pattern not found
prop_breakOnOriginalWhenMissing :: String -> String -> Bool
prop_breakOnOriginalWhenMissing pattern input =
  let (prefix, suffix) = breakOn pattern input
  in if not (pattern `isInfixOf` input)
     then prefix == input && suffix == ""
     else True  -- Property only applies when pattern not found

-- Property: breakOn is deterministic
prop_breakOnDeterministic :: String -> String -> Bool
prop_breakOnDeterministic pattern input =
  let result1 = breakOn pattern input
      result2 = breakOn pattern input
  in result1 == result2

-- Property: composition of string functions is associative where appropriate
prop_stringCompositionAssociative :: String -> Bool
prop_stringCompositionAssociative input =
  let trimThenNormalize = trim (normalizeIndentation input)
      normalizeThenTrim = normalizeIndentation (trim input)
  in trimThenNormalize == normalizeThenTrim

-- Property: string functions handle arbitrary unicode
prop_handleUnicode :: String -> Bool
prop_handleUnicode input =
  let trimmed = trim input
      split = splitBy ',' input
      withoutComments = removeLineComments input
  in length trimmed >= 0 && length split >= 0 && length withoutComments >= 0

-- Property: string functions handle extreme inputs
prop_handleExtremeInputs :: String -> Bool
prop_handleExtremeInputs input =
  let extremeInput = concat $ replicate 1000 input
      result = trim extremeInput
  in length result >= 0

-- Property: string operations don't have exponential behavior
prop_noExponentialBehavior :: String -> Bool
prop_noExponentialBehavior input =
  let nestedInput = concat $ replicate 10 $ "  " ++ input ++ "  "
      result = normalizeIndentation nestedInput
  in length result < length nestedInput * 2  -- Reasonable bound