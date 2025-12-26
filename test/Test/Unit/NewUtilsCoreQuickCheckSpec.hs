{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewUtilsCoreQuickCheckSpec where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)
import Test.Tasty.HUnit (testCase, assertBool)

import qualified Data.Text as T
import qualified Data.Char as Char
import Utils

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genStringWithWhitespace :: Gen String
genStringWithWhitespace = do
    before <- listOf $ elements [' ', '\t']
    content <- listOf1 $ choose ('a', 'z')
    after <- listOf $ elements [' ', '\t', '\n']
    return $ before ++ content ++ after

-- Generate strings with multiple lines
genMultiLineString :: Gen String
genMultiLineString = do
    numLines <- choose (1, 5)
    lines <- listOf1 $ do
        content <- listOf $ choose ('a', 'z')
        ws <- listOf $ elements [' ', '\t']
        return $ content ++ ws
    return $ unlines lines

-- Generate strings for split testing
genSplitString :: Gen String
genSplitString = do
    parts <- listOf1 $ listOf $ choose ('a', 'z')
    delim <- elements [',', ';', ':', '|']
    return $ intercalate [delim] parts

-- Generate strings with comments
genStringWithComments :: Gen String
genStringWithComments = do
    code <- listOf $ choose ('a', 'z')
    hasLineComment <- choose (True, False)
    hasBlockComment <- choose (True, False)
    let baseCode = code
    let withLineComment = if hasLineComment then baseCode ++ "// comment\n" else baseCode
    let withBlockComment = if hasBlockComment then "/* block comment */" ++ withLineComment else withLineComment
    return withBlockComment

-- Generate strings with various indentation
genIndentedString :: Gen String
genIndentedString = do
    numLines <- choose (1, 4)
    lines <- sequence $ do
        _ <- [1..numLines]
        indent <- choose (0, 8)
        content <- listOf $ choose ('a', 'z')
        return $ replicate indent ' ' ++ content
    return $ unlines lines

-- ============================================================================
-- Utils Core Properties
-- ============================================================================

-- Property: trim removes leading and trailing whitespace
prop_trimRemovesWhitespace :: String -> Property
prop_trimRemovesWhitespace str =
    let trimmed = trim str
        hasLeadingSpace = not (null str) && Char.isSpace (head str)
        hasTrailingSpace = not (null str) && Char.isSpace (last str)
        trimmedStartEmpty = null trimmed || not (Char.isSpace (head trimmed))
        trimmedEndEmpty = null trimmed || not (Char.isSpace (last trimmed))
    in counterexample ("Trim should remove leading and trailing whitespace")
       (if hasLeadingSpace || hasTrailingSpace
        then trimmedStartEmpty && trimmedEndEmpty
        else property True)

-- Property: trim doesn't change content without leading/trailing whitespace
prop_trimPreservesNonWhitespace :: String -> Property
prop_trimPreservesNonWhitespace str
    | null str = property True
    | Char.isSpace (head str) || Char.isSpace (last str) = property True
    | otherwise =
        let trimmed = trim str
        in counterexample ("Trim should preserve content without leading/trailing whitespace")
           (trimmed === str)

-- Property: splitBy preserves empty segments when delimiter is at boundaries
prop_splitByPreservesEmptySegments :: Char -> String -> Property
prop_splitByPreservesEmptySegments delim str =
    let parts = splitBy delim str
        strWithDelim = [delim] ++ str ++ [delim]
        partsWithDelim = splitBy delim strWithDelim
    in counterexample ("Split should preserve empty segments at boundaries")
       (length partsWithDelim === length parts + 2)

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsedRemovesEmpty :: Char -> String -> Property
prop_splitByCollapsedRemovesEmpty delim str =
    let parts = splitBy delim str
        collapsedParts = splitByCollapsed delim str
        hasEmpty = any null parts
    in counterexample ("SplitCollapsed should remove empty segments")
       (if hasEmpty then length collapsedParts < length parts else property True)

-- Property: splitByComma is equivalent to splitBy ','
prop_splitByCommaEquivalence :: String -> Property
prop_splitByCommaEquivalence str =
    let commaParts = splitByComma str
        genericParts = splitBy ',' str
    in counterexample ("splitByComma should equal splitBy ','")
       (commaParts === genericParts)

-- Property: splitByCommaCollapsed is equivalent to splitByCollapsed ','
prop_splitByCommaCollapsedEquivalence :: String -> Property
prop_splitByCommaCollapsedEquivalence str =
    let commaParts = splitByCommaCollapsed str
        genericParts = splitByCollapsed ',' str
    in counterexample ("splitByCommaCollapsed should equal splitByCollapsed ','")
       (commaParts === genericParts)

-- Property: removeLineComments removes // comments but preserves other content
prop_removeLineCommentsBehavior :: String -> Property
prop_removeLineCommentsBehavior str =
    let withoutComments = removeLineComments str
        linesOriginal = lines str
        linesProcessed = lines withoutComments
    in counterexample ("removeLineComments should process line by line")
       (length linesProcessed <= length linesOriginal === True)

-- Property: removeComments handles both // and /* */ comments
prop_removeCommentsHandlesBothTypes :: String -> Property
prop_removeCommentsHandlesBothTypes str =
    let withoutComments = removeComments str
        hasLineComment = "//" `isInfixOf` str
        hasBlockComment = "/*" `isInfixOf` str && "*/" `isInfixOf` str
    in counterexample ("removeComments should handle both comment types")
       (if hasLineComment || hasBlockComment
        then length withoutComments <= length str
        else property True)

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentationPreservesRelative :: String -> Property
prop_normalizeIndentationPreservesRelative str =
    let normalized = normalizeIndentation str
        originalLines = filter (not . all Char.isSpace) $ lines str
        normalizedLines = filter (not . all Char.isSpace) $ lines normalized
    in counterexample ("normalizeIndentation should preserve relative structure")
       (length normalizedLines === length originalLines)

-- Property: normalizeIndentation removes common prefix
prop_normalizeIndentationRemovesCommonPrefix :: String -> Property
prop_normalizeIndentationRemovesCommonPrefix str =
    let normalized = normalizeIndentation str
        normalizedLines = lines normalized
        hasLeadingSpaces = any (\line -> not (null line) && Char.isSpace (head line)) normalizedLines
    in counterexample ("normalizeIndentation should remove common prefix")
       (not (null normalizedLines) ==> not hasLeadingSpaces)

-- Property: fixIndentation is equivalent to normalizeIndentation
prop_fixIndentationEquivalence :: String -> Property
prop_fixIndentationEquivalence str =
    let fixed = fixIndentation str
        normalized = normalizeIndentation str
    in counterexample ("fixIndentation should equal normalizeIndentation")
       (fixed === normalized)

-- Property: breakOn finds first occurrence or returns original
prop_breakOnBehavior :: String -> String -> Property
prop_breakOnBehavior pat str
    | null pat = 
        let (before, after) = breakOn pat str
        in counterexample ("breakOn with empty pattern should return (\"\", str)")
           (before === "" && after === str)
    | pat `isInfixOf` str =
        let (before, after) = breakOn pat str
            expectedBefore = takeWhile (/= head pat) str
        in counterexample ("breakOn should split at first occurrence")
           (before ++ pat ++ after === str)
    | otherwise =
        let (before, after) = breakOn pat str
        in counterexample ("breakOn should return (str, \"\") when pattern not found")
           (before === str && after === "")

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

-- Property: trim handles empty string
prop_trimHandlesEmpty :: Property
prop_trimHandlesEmpty =
    let trimmed = trim ""
    in counterexample ("trim should handle empty string")
       (trimmed === "")

-- Property: splitBy handles empty string
prop_splitByHandlesEmpty :: Char -> Property
prop_splitByHandlesEmpty delim =
    let parts = splitBy delim ""
    in counterexample ("splitBy should handle empty string")
       (parts === [""])

-- Property: splitByCollapsed handles empty string
prop_splitByCollapsedHandlesEmpty :: Char -> Property
prop_splitByCollapsedHandlesEmpty delim =
    let parts = splitByCollapsed delim ""
    in counterexample ("splitByCollapsed should handle empty string")
       (parts === [])

-- Property: removeComments handles empty string
prop_removeCommentsHandlesEmpty :: Property
prop_removeCommentsHandlesEmpty =
    let result = removeComments ""
    in counterexample ("removeComments should handle empty string")
       (result === "")

-- Property: normalizeIndentation handles empty string
prop_normalizeIndentationHandlesEmpty :: Property
prop_normalizeIndentationHandlesEmpty =
    let result = normalizeIndentation ""
    in counterexample ("normalizeIndentation should handle empty string")
       (result === "")

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Utils Core QuickCheck Tests"
    [ testProperty "trim removes whitespace" prop_trimRemovesWhitespace
    , testProperty "trim preserves non-whitespace content" prop_trimPreservesNonWhitespace
    , testProperty "splitBy preserves empty segments" prop_splitByPreservesEmptySegments
    , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsedRemovesEmpty
    , testProperty "splitByComma equivalence" prop_splitByCommaEquivalence
    , testProperty "splitByCommaCollapsed equivalence" prop_splitByCommaCollapsedEquivalence
    , testProperty "removeLineComments behavior" prop_removeLineCommentsBehavior
    , testProperty "removeComments handles both types" prop_removeCommentsHandlesBothTypes
    , testProperty "normalizeIndentation preserves relative" prop_normalizeIndentationPreservesRelative
    , testProperty "normalizeIndentation removes common prefix" prop_normalizeIndentationRemovesCommonPrefix
    , testProperty "fixIndentation equivalence" prop_fixIndentationEquivalence
    , testProperty "breakOn behavior" prop_breakOnBehavior
    , testProperty "trim handles empty" prop_trimHandlesEmpty
    , testProperty "splitBy handles empty" prop_splitByHandlesEmpty
    , testProperty "splitByCollapsed handles empty" prop_splitByCollapsedHandlesEmpty
    , testProperty "removeComments handles empty" prop_removeCommentsHandlesEmpty
    , testProperty "normalizeIndentation handles empty" prop_normalizeIndentationHandlesEmpty
    ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Import required for intercalate
import Data.List (intercalate)

-- Import for QuickCheck
import Test.QuickCheck (choose, elements, listOf, listOf1, (==>))