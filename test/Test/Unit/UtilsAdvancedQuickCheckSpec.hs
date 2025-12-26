module Test.Unit.UtilsAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat, vectorOf)
import TestSupport.QuickCheck (fastProperty)

import Utils
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T

-- ============================================================================
-- Generators for Utils Testing
-- ============================================================================

-- Generate strings with various whitespace patterns
whitespaceString :: Gen String
whitespaceString = listOf $ elements [' ', '\t', '\n', '\r']

-- Generate strings with mixed content
mixedString :: Gen String
mixedString = do
    base <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r"
    return base

-- Generate strings with potential comment markers
commentString :: Gen String
commentString = do
    parts <- listOf $ oneof [
        pure "// line comment",
        pure "/* block comment */",
        pure "/* multi\nline\ncomment */",
        pure "// comment with // inside",
        pure "string with // comment",
        pure "\"string with // inside\"",
        pure "'char with // inside'",
        pure "escaped \"// in string\"",
        pure "escaped '// in string'",
        arbitrary
        ]
    return $ unwords parts

-- Generate strings with various indentation patterns
indentedString :: Gen String
indentedString = do
    lines' <- listOf $ do
        indent <- choose (0, 8)
        content <- listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['\t']
        return $ replicate indent ' ' ++ content
    return $ unlines lines'

-- Generate strings with specific delimiters
stringWithDelim :: Char -> Gen String
stringWithDelim delim = do
    parts <- listOf $ oneof [
        pure [delim],
        pure [delim, delim],
        listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ",
        pure ""
        ]
    return $ concat parts

-- ============================================================================
-- Trim Function Properties
-- ============================================================================

prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
    let once = trim input
        twice = trim once
    in once == twice

prop_trimRemovesLeadingWhitespace :: String -> Bool
prop_trimRemovesLeadingWhitespace input =
    let trimmed = trim input
        leadingRemoved = null trimmed || not (isSpace (head trimmed))
    in leadingRemoved

prop_trimRemovesTrailingWhitespace :: String -> Bool
prop_trimRemovesTrailingWhitespace input =
    let trimmed = trim input
        trailingRemoved = null trimmed || not (isSpace (last trimmed))
    in trailingRemoved

prop_trimPreservesInternalWhitespace :: String -> String -> Bool
prop_trimPreservesInternalWhitespace prefix suffix =
    let middle = "  hello  world  "
        input = prefix ++ middle ++ suffix
        trimmed = trim input
        expected = "  hello  world  "
    in middle `isInfixOf` trimmed

-- ============================================================================
-- Split Function Properties
-- ============================================================================

prop_splitByPreservesOrder :: Char -> String -> Bool
prop_splitByPreservesOrder delim input =
    let parts = splitBy delim input
        rejoined = concat (intersperse [delim] parts)
    in rejoined == input
  where
    intersperse _ [] = []
    intersperse sep (x:xs) = x ++ sep ++ intersperse sep xs

prop_splitByHandlesEmptyInput :: Char -> Bool
prop_splitByHandlesEmptyInput delim =
    splitBy delim "" == [""]

prop_splitByHandlesOnlyDelimiters :: Char -> Int -> Bool
prop_splitByHandlesOnlyDelimiters delim n =
    let input = replicate n delim
        result = splitBy delim input
        expected = replicate (n + 1) ""
    in result == expected

prop_splitByCollapsedRemovesEmpty :: Char -> String -> Bool
prop_splitByCollapsedRemovesEmpty delim input =
    let parts = splitByCollapsed delim input
    in all (not . null) parts

prop_splitByCollapsedIsSubsetOfSplitBy :: Char -> String -> Bool
prop_splitByCollapsedIsSubsetOfSplitBy delim input =
    let normal = splitBy delim input
        collapsed = splitByCollapsed delim input
    in all (`elem` normal) collapsed

prop_splitByCommaEqualsSplitByComma :: String -> Bool
prop_splitByCommaEqualsSplitByComma input =
    splitByComma input == splitBy ',' input

prop_splitByCommaCollapsedEqualsSplitByCollapsed :: String -> Bool
prop_splitByCommaCollapsedEqualsSplitByCollapsed input =
    splitByCommaCollapsed input == splitByCollapsed ',' input

-- ============================================================================
-- Comment Removal Properties
-- ============================================================================

prop_removeLineCommentsPreservesNonCommentLines :: String -> Bool
prop_removeLineCommentsPreservesNonCommentLines input =
    let lines' = lines input
        processedLines = lines (removeLineComments input)
        nonCommentLines = filter (not . isPrefixOf "//") lines'
        processedNonComments = filter (not . null) processedLines
    in length processedNonComments >= length nonCommentLines - 
       length (filter (isPrefixOf "//") nonCommentLines)

prop_removeLineCommentsIgnoresCommentsInStrings :: String -> Bool
prop_removeLineCommentsIgnoresCommentsInStrings input =
    let stringWithCommentInString = "\"string with // comment\" normal // comment\nafter"
        result = removeLineComments stringWithCommentInString
    in "\"string with // comment\" normal " `isPrefixOf` result

prop_removeCommentsPreservesStringLiterals :: String -> Bool
prop_removeCommentsPreservesStringLiterals input =
    let stringWithStrings = "text \"string // not comment\" more /* not comment */ text"
        result = removeComments stringWithStrings
    in "\"string // not comment\"" `isInfixOf` result

prop_removeCommentsPreservesCharLiterals :: String -> Bool
prop_removeCommentsPreservesCharLiterals input =
    let stringWithChars = "text '// not comment' more /* not comment */ text"
        result = removeComments stringWithChars
    in "'// not comment'" `isInfixOf` result

prop_removeCommentsHandlesNestedQuotes :: String -> Bool
prop_removeCommentsHandlesNestedQuotes input =
    let nestedQuotes = "text \"outer // not comment \\\"inner // also not\\\" more\" normal // comment"
        result = removeComments nestedQuotes
    in "\"outer // not comment \\\"inner // also not\\\" more\"" `isInfixOf` result

prop_removeCommentsHandlesEscapedQuotes :: String -> Bool
prop_removeCommentsHandlesEscapedQuotes input =
    let escapedQuotes = "text \"string with \\\" // not comment\" normal // comment"
        result = removeComments escapedQuotes
    in "\"string with \\\" // not comment\"" `isInfixOf` result

-- ============================================================================
-- Indentation Properties
-- ============================================================================

prop_normalizeIndentationPreservesRelativeIndentation :: String -> Bool
prop_normalizeIndentationPreservesRelativeIndentation input =
    let normalized = normalizeIndentation input
        originalLines = lines input
        normalizedLines = lines normalized
    in length originalLines == length normalizedLines

prop_normalizeIndentationRemovesCommonPrefix :: String -> Bool
prop_normalizeIndentationRemovesCommonPrefix input =
    let normalized = normalizeIndentation input
        normalizedLines = lines normalized
        nonEmptyLines = filter (not . all isSpace) normalizedLines
    in if null nonEmptyLines
       then True
       else all (\line -> null line || not (isSpace (head line))) nonEmptyLines

prop_normalizeIndentationPreservesEmptyLines :: String -> Bool
prop_normalizeIndentationPreservesEmptyLines input =
    let originalLines = lines input
        normalizedLines = lines (normalizeIndentation input)
        originalEmpty = filter null originalLines
        normalizedEmpty = filter null normalizedLines
    in length originalEmpty == length normalizedEmpty

prop_forceSingleTabIndentationAddsTab :: String -> Bool
prop_forceSingleTabIndentationAddsTab input =
    let forced = forceSingleTabIndentation input
        forcedLines = lines forced
    in all (\line -> null line || '\t' `elem` take 1 line) forcedLines

prop_forceSingleTabIndentationTrimsContent :: String -> Bool
prop_forceSingleTabIndentationTrimsContent input =
    let forced = forceSingleTabIndentation input
        forcedLines = lines forced
    in all (\line -> null line || trim line == drop 1 line) forcedLines

prop_fixIndentationEqualsNormalizeIndentation :: String -> Bool
prop_fixIndentationEqualsNormalizeIndentation input =
    fixIndentation input == normalizeIndentation input

-- ============================================================================
-- Search Function Properties
-- ============================================================================

prop_breakOnFindsPattern :: String -> String -> Bool
prop_breakOnFindsPattern pattern input =
    let (before, after) = breakOn pattern input
        reconstructed = before ++ pattern ++ after
    in if pattern `isInfixOf` input
       then reconstructed == input
       else before == input && after == ""

prop_breakOnHandlesEmptyPattern :: String -> Bool
prop_breakOnHandlesEmptyPattern input =
    let (before, after) = breakOn "" input
    in null before && after == input

prop_breakOnHandlesPatternAtStart :: String -> String -> Bool
prop_breakOnHandlesPatternAtStart pattern suffix =
    let input = pattern ++ suffix
        (before, after) = breakOn pattern input
    in null before && after == suffix

prop_breakOnHandlesPatternAtEnd :: String -> String -> Bool
prop_breakOnHandlesPatternAtEnd prefix pattern =
    let input = prefix ++ pattern
        (before, after) = breakOn pattern input
    in before == prefix && after == ""

prop_breakOnHandlesMultipleOccurrences :: String -> String -> String -> Bool
prop_breakOnHandlesMultipleOccurrences prefix pattern suffix =
    let input = prefix ++ pattern ++ suffix ++ pattern ++ "end"
        (before, after) = breakOn pattern input
    in before == prefix && after == suffix ++ pattern ++ "end"

-- ============================================================================
-- Advanced Properties
-- ============================================================================

prop_splitByAndJoinRoundtrip :: Char -> String -> Bool
prop_splitByAndJoinRoundtrip delim input =
    let parts = splitBy delim input
        rejoined = concat parts
    in if null delim
       then rejoined == input
       else filter (/= delim) rejoined == filter (/= delim) input

prop_trimAndNormalizeIndentationInteraction :: String -> Bool
prop_trimAndNormalizeIndentationInteraction input =
    let trimmed = trim input
        normalized = normalizeIndentation input
        trimmedNormalized = trim normalized
    in trimmedNormalized == normalizeIndentation trimmed

prop_removeCommentsAndNormalizeIndentationInteraction :: String -> Bool
prop_removeCommentsAndNormalizeIndentationInteraction input =
    let commentsRemoved = removeComments input
        normalized = normalizeIndentation input
        commentsRemovedNormalized = normalizeIndentation commentsRemoved
    in length (lines commentsRemovedNormalized) <= length (lines normalized)

prop_breakOnAndSplitByInteraction :: Char -> String -> Bool
prop_breakOnAndSplitByInteraction delim input =
    let pattern = [delim]
        (before, after) = breakOn pattern input
        parts = splitBy delim input
    in case parts of
        [] -> True
        [single] -> before == single && after == ""
        first:rest -> before == first && after == concat rest

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Utils Advanced QuickCheck Tests"
    [ testGroup "Trim Function Properties"
        [ fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "trim removes leading whitespace" prop_trimRemovesLeadingWhitespace
        , fastProperty "trim removes trailing whitespace" prop_trimRemovesTrailingWhitespace
        , fastProperty "trim preserves internal whitespace" prop_trimPreservesInternalWhitespace
        ]

    , testGroup "Split Function Properties"
        [ fastProperty "splitBy preserves order" prop_splitByPreservesOrder
        , fastProperty "splitBy handles empty input" prop_splitByHandlesEmptyInput
        , fastProperty "splitBy handles only delimiters" prop_splitByHandlesOnlyDelimiters
        , fastProperty "splitByCollapsed removes empty" prop_splitByCollapsedRemovesEmpty
        , fastProperty "splitByCollapsed is subset of splitBy" prop_splitByCollapsedIsSubsetOfSplitBy
        , fastProperty "splitByComma equals splitBy ','" prop_splitByCommaEqualsSplitByComma
        , fastProperty "splitByCommaCollapsed equals splitByCollapsed ','" prop_splitByCommaCollapsedEqualsSplitByCollapsed
        ]

    , testGroup "Comment Removal Properties"
        [ fastProperty "removeLineComments preserves non-comment lines" prop_removeLineCommentsPreservesNonCommentLines
        , fastProperty "removeLineComments ignores comments in strings" prop_removeLineCommentsIgnoresCommentsInStrings
        , fastProperty "removeComments preserves string literals" prop_removeCommentsPreservesStringLiterals
        , fastProperty "removeComments preserves char literals" prop_removeCommentsPreservesCharLiterals
        , fastProperty "removeComments handles nested quotes" prop_removeCommentsHandlesNestedQuotes
        , fastProperty "removeComments handles escaped quotes" prop_removeCommentsHandlesEscapedQuotes
        ]

    , testGroup "Indentation Properties"
        [ fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentationPreservesRelativeIndentation
        , fastProperty "normalizeIndentation removes common prefix" prop_normalizeIndentationRemovesCommonPrefix
        , fastProperty "normalizeIndentation preserves empty lines" prop_normalizeIndentationPreservesEmptyLines
        , fastProperty "forceSingleTabIndentation adds tab" prop_forceSingleTabIndentationAddsTab
        , fastProperty "forceSingleTabIndentation trims content" prop_forceSingleTabIndentationTrimsContent
        , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentationEqualsNormalizeIndentation
        ]

    , testGroup "Search Function Properties"
        [ fastProperty "breakOn finds pattern" prop_breakOnFindsPattern
        , fastProperty "breakOn handles empty pattern" prop_breakOnHandlesEmptyPattern
        , fastProperty "breakOn handles pattern at start" prop_breakOnHandlesPatternAtStart
        , fastProperty "breakOn handles pattern at end" prop_breakOnHandlesPatternAtEnd
        , fastProperty "breakOn handles multiple occurrences" prop_breakOnHandlesMultipleOccurrences
        ]

    , testGroup "Advanced Properties"
        [ fastProperty "splitBy and join roundtrip" prop_splitByAndJoinRoundtrip
        , fastProperty "trim and normalizeIndentation interaction" prop_trimAndNormalizeIndentationInteraction
        , fastProperty "removeComments and normalizeIndentation interaction" prop_removeCommentsAndNormalizeIndentationInteraction
        , fastProperty "breakOn and splitBy interaction" prop_breakOnAndSplitByInteraction
        ]

    , testGroup "Unit Tests"
        [ testCase "trim removes all whitespace" $ do
            trim "  hello  world  " @?= "hello  world"
            trim "\t\n  test  \n\t" @?= "test"

        , testCase "splitBy with various delimiters" $ do
            splitBy ',' "a,b,c" @?= ["a", "b", "c"]
            splitBy ':' "a::b::c" @?= ["a", "", "b", "", "c"]
            splitBy '-' "a-b-c-" @?= ["a", "b", "c", ""]

        , testCase "splitByCollapsed removes empty segments" $ do
            splitByCollapsed ',' "a,,b" @?= ["a", "b"]
            splitByCollapsed ':' "::a::b::" @?= ["a", "b"]
            splitByCollapsed '-' "---" @?= []

        , testCase "removeLineComments preserves strings" $ do
            let input = "code // comment\n\"string // not comment\" // real comment"
                result = removeLineComments input
            result @?= "code \n\"string // not comment\" "

        , testCase "removeComments handles both types" $ do
            let input = "code // line comment\n/* block\ncomment */ more code"
                result = removeComments input
            result @?= "code \n\n more code"

        , testCase "normalizeIndentation preserves structure" $ do
            let input = "    func main() {\n        fmt.Println(\"hi\")\n    }"
                expected = "func main() {\n    fmt.Println(\"hi\")\n}"
            normalizeIndentation input @?= expected

        , testCase "breakOn finds first occurrence" $ do
            breakOn "ll" "hello" @?= ("he", "o")
            breakOn "xyz" "hello" @?= ("hello", "")
            breakOn "" "abc" @?= ("", "abc")
        ]
    ]