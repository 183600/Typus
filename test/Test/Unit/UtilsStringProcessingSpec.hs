module Test.Unit.UtilsStringProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf1, elements, suchThat)
import qualified Data.Text as T
import Utils

-- | QuickCheck tests for Utils string processing functions
tests :: TestTree
tests =
  testGroup "Utils string processing properties"
    [ testGroup "String splitting properties"
        [ fastProperty "splitBy preserves total L.length when delimiter not present" prop_splitByNoDelimiter
        , fastProperty "splitBy L.length increases with more delimiters" prop_splitByDelimiterCount
        , fastProperty "splitByCollapsed never produces empty strings" prop_splitByCollapsedNoEmpty
        , fastProperty "splitByComma is splitBy with ','" prop_splitByCommaEquivalence
        , fastProperty "splitByCommaCollapsed is splitByCollapsed with ','" prop_splitByCommaCollapsedEquivalence
        ]

    , testGroup "String trimming properties"
        [ fastProperty "trim removes only leading/trailing whitespace" prop_trimOnlyEdges
        , fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "trim never increases string L.length" prop_trimNonIncreasing
        , fastProperty "trim preserves non-whitespace characters" prop_trimPreservesContent
        ]

    , testGroup "Comment removal properties"
        [ fastProperty "removeLineComments preserves line count" prop_removeLineCommentsPreservesLines
        , fastProperty "removeLineComments removes comment markers" prop_removeLineCommentsRemovesMarkers
        , fastProperty "removeComments preserves string literals" prop_removeCommentsPreservesStrings
        , fastProperty "removeComments preserves character literals" prop_removeCommentsPreserveChars
        , fastProperty "removeComments handles nested block comments correctly" prop_removeCommentsBlockHandling
        ]

    , testGroup "Indentation properties"
        [ fastProperty "normalizeIndentation preserves relative structure" prop_normalizePreservesStructure
        , fastProperty "normalizeIndentation doesn't increase indentation" prop_normalizeNonIncreasing
        , fastProperty "forceSingleTabIndentation creates consistent tabs" prop_forceTabConsistency
        , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentationEquivalence
        ]

    , testGroup "Search L.and split properties"
        [ fastProperty "breakOn finds first occurrence" prop_breakOnFirstOccurrence
        , fastProperty "breakOn handles empty pattern" prop_breakOnEmptyPattern
        , fastProperty "breakOn returns original string when pattern not found" prop_breakOnNotFound
        ]
    ]

-- ============================================================================
-- Helper generators
-- ============================================================================

genNonEmptyString :: Gen String
genNonEmptyString = listOf1 $ elements ['a'..'z']

genStringWithDelim :: Char -> Gen String
genStringWithDelim delim = do
    parts <- listOf1 genNonEmptyString
    return $ L.concat $ intersperse [delim] parts
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x ++ sep : intersperse sep (y:xs)

genStringWithWhitespace :: Gen String
genStringWithWhitespace = do
    content <- genNonEmptyString
    leading <- listOf $ elements " \t\n"
    trailing <- listOf $ elements " \t\n"
    return $ leading ++ content ++ trailing

genStringWithLineComments :: Gen String
genStringWithLineComments = do
    lines <- listOf1 $ do
        content <- genNonEmptyString
        hasComment <- arbitrary
        if hasComment
            then return $ content ++ " // comment"
            else return content
    return $ unlines lines

genStringWithBlockComments :: Gen String
genStringWithBlockComments = do
    before <- genNonEmptyString
    comment <- genNonEmptyString
    after <- genNonEmptyString
    return $ before ++ "/* " ++ comment ++ " */" ++ after

genMultilineString :: Gen String
genMultilineString = do
    lines <- listOf1 genNonEmptyString
    return $ unlines lines

-- ============================================================================
-- String splitting properties
-- ============================================================================

prop_splitByNoDelimiter :: Char -> String -> Property
prop_splitByNoDelimiter delim str =
    delim `notElem` str ==> L.length (splitBy delim str) == 1

prop_splitByDelimiterCount :: Char -> String -> Property
prop_splitByDelimiterCount delim str =
    let delimCount = L.length $ L.filter (== delim) str
        resultCount = L.length $ splitBy delim str
    in resultCount == delimCount + 1

prop_splitByCollapsedNoEmpty :: Char -> String -> Bool
prop_splitByCollapsedNoEmpty delim str =
    L.all (not . null) (splitByCollapsed delim str)

prop_splitByCommaEquivalence :: String -> Bool
prop_splitByCommaEquivalence str =
    splitByComma str == splitBy ',' str

prop_splitByCommaCollapsedEquivalence :: String -> Bool
prop_splitByCommaCollapsedEquivalence str =
    splitByCommaCollapsed str == splitByCollapsed ',' str

-- ============================================================================
-- String trimming properties
-- ============================================================================

prop_trimOnlyEdges :: String -> Property
prop_trimOnlyEdges str =
    let trimmed = trim str
        hasInternalWhitespace = L.any isSpace (init (safeTail trimmed))
    in not hasInternalWhitespace || null trimmed
  where
    isSpace c = c `elem` " \t\n\r"
    safeTail [] = []
    safeTail xs = L.tail xs

prop_trimIdempotent :: String -> Bool
prop_trimIdempotent str =
    let once = trim str
        twice = trim once
    in once == twice

prop_trimNonIncreasing :: String -> Bool
prop_trimNonIncreasing str =
    L.length (trim str) <= L.length str

prop_trimPreservesContent :: String -> Property
prop_trimPreservesContent str =
    let trimmed = trim str
        nonSpaceChars = L.filter (not . isSpace) str
        trimmedNonSpace = L.filter (not . isSpace) trimmed
    in null nonSpaceChars ==> trimmedNonSpace == nonSpaceChars
  where
    isSpace c = c `elem` " \t\n\r"

-- ============================================================================
-- Comment removal properties
-- ============================================================================

prop_removeLineCommentsPreservesLines :: String -> Bool
prop_removeLineCommentsPreservesLines str =
    L.length (lines str) == L.length (lines (removeLineComments str))

prop_removeLineCommentsRemovesMarkers :: String -> Property
prop_removeLineCommentsRemovesMarkers str =
    "//" `L.isInfixOf` str ==> not ("//" `L.isInfixOf` removeLineComments str)

prop_removeCommentsPreservesStrings :: String -> Property
prop_removeCommentsPreservesStrings str =
    let hasStringLiteral = "\"" `L.isInfixOf` str
        result = removeComments str
    in hasStringLiteral ==> countStringLiterals str == countStringLiterals result
  where
    countStringLiterals = L.length . L.filter (== '"')

prop_removeCommentsPreserveChars :: String -> Property
prop_removeCommentsPreserveChars str =
    let hasCharLiteral = "'" `L.isInfixOf` str
        result = removeComments str
    in hasCharLiteral ==> countCharLiterals str == countCharLiterals result
  where
    countCharLiterals = L.length . L.filter (== '\'')

prop_removeCommentsBlockHandling :: String -> String -> Property
prop_removeCommentsBlockHandling before after =
    let input = before ++ "/* comment */" ++ after
        result = removeComments input
    in before `L.isInfixOf` result && after `L.isInfixOf` result

-- ============================================================================
-- Indentation properties
-- ============================================================================

prop_normalizePreservesStructure :: String -> Property
prop_normalizePreservesStructure str =
    let originalLines = lines str
        normalizedLines = lines (normalizeIndentation str)
        originalLineCount = L.length originalLines
        normalizedLineCount = L.length normalizedLines
    in originalLineCount == normalizedLineCount

prop_normalizeNonIncreasing :: String -> Bool
prop_normalizeNonIncreasing str =
    let normalized = normalizeIndentation str
        originalIndentation = getIndentation str
        normalizedIndentation = getIndentation normalized
    in normalizedIndentation <= originalIndentation
  where
    getIndentation = L.minimum . L.map (L.length . takeWhile isSpace) . L.filter (not . L.all isSpace) . lines
    isSpace c = c `elem` " \t"

prop_forceTabConsistency :: String -> Bool
prop_forceTabConsistency str =
    let result = forceSingleTabIndentation str
        lines' = lines result
        nonEmptyLines = L.filter (not . null) lines'
    in L.all (\line -> null line || L.head line == '\t') nonEmptyLines

prop_fixIndentationEquivalence :: String -> Bool
prop_fixIndentationEquivalence str =
    fixIndentation str == normalizeIndentation str

-- ============================================================================
-- Search L.and split properties
-- ============================================================================

prop_breakOnFirstOccurrence :: String -> String -> Property
prop_breakOnFirstOccurrence pat str =
    not (null pat) && pat `L.isInfixOf` str ==> 
    let (before, after) = breakOn pat str
        combined = before ++ pat ++ after
    in combined == str

prop_breakOnEmptyPattern :: String -> Bool
prop_breakOnEmptyPattern str =
    let (before, after) = breakOn "" str
    in null before && after == str

prop_breakOnNotFound :: String -> String -> Property
prop_breakOnNotFound pat str =
    not (null pat) && pat `notElem` str ==> 
    let (before, after) = breakOn pat str
    in before == str && null after

-- ============================================================================
-- Helper functions
-- ============================================================================

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = L.any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'
