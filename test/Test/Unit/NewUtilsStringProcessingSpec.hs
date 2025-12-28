module Test.Unit.NewUtilsStringProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, elements, suchThat)
import Utils
import Data.Char (isSpace)

-- | 新的字符串处理QuickCheck测试
tests :: TestTree
tests =
  testGroup "New Utils String Processing Tests"
    [ testGroup "String splitting properties"
        [ fastProperty "splitBy and splitByCollapsed relationship" prop_splitByRelationship
        , fastProperty "splitBy preserves total length" prop_splitByPreservesLength
        , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsedRemovesEmpty
        , fastProperty "splitBy comma behavior" prop_splitByCommaBehavior
        ]

    , testGroup "String trimming properties"
        [ fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "trim removes only whitespace" prop_trimRemovesOnlyWhitespace
        , fastProperty "trim preserves non-whitespace content" prop_trimPreservesContent
        ]

    , testGroup "Comment removal properties"
        [ fastProperty "removeLineComments preserves non-comment content" prop_removeLineCommentsPreservesContent
        , fastProperty "removeComments shrinks or preserves length" prop_removeCommentsShrinksOrPreserves
        , fastProperty "removeComments handles nested patterns" prop_removeCommentsNested
        ]

    , testGroup "Indentation properties"
        [ fastProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentationPreservesStructure
        , fastProperty "forceSingleTabIndentation creates consistent format" prop_forceSingleTabCreatesConsistentFormat
        ]

    , testGroup "Search properties"
        [ fastProperty "breakOn correctness" prop_breakOnCorrectness
        , fastProperty "breakOn with empty pattern" prop_breakOnEmptyPattern
        ]
    ]

-- ============================================================================
-- Arbitrary instances for test data
-- ============================================================================

instance Arbitrary Char where
    arbitrary = oneof
        [ elements ['a'..'z']
        , elements ['A'..'Z']
        , elements ['0'..'9']
        , elements " \t\n\r.,;:!?()[]{}<>+-*/=&|%^~"
        , elements "\"'\\/@#$`_"
        ]

-- Generate strings with various characters
genStringWithDelims :: Char -> Gen String
genStringWithDelims delim = listOf $ oneof
    [ arbitrary `suchThat` (/= delim)
    , return delim
    ]

-- Generate strings with whitespace
genStringWithWhitespace :: Gen String
genStringWithWhitespace = listOf $ oneof
    [ arbitrary `suchThat` (not . isSpace)
    , elements " \t\n\r"
    ]

-- Generate strings with comment patterns
genStringWithComments :: Gen String
genStringWithComments = listOf $ oneof
    [ arbitrary `suchThat` (`notElem` "/\"\\")
    , return '/'
    , return '*'
    , return '"'
    , return '\\'
    , return '\n'
    ]

-- ============================================================================
-- Properties for splitBy functions
-- ============================================================================

prop_splitByRelationship :: Char -> String -> Bool
prop_splitByRelationship delim input =
    splitByCollapsed delim input == filter (not . null) (splitBy delim input)

prop_splitByPreservesLength :: Char -> String -> Bool
prop_splitByPreservesLength delim input =
    let segments = splitBy delim input
        reconstructed = concatMap (\s -> s ++ [delim]) (init segments) ++ last segments
    in length input == length reconstructed

prop_splitByCollapsedRemovesEmpty :: Char -> String -> Bool
prop_splitByCollapsedRemovesEmpty delim input =
    all (not . null) (splitByCollapsed delim input)

prop_splitByCommaBehavior :: String -> Bool
prop_splitByCommaBehavior input =
    splitByComma input == splitBy ',' input &&
    splitByCommaCollapsed input == splitByCollapsed ',' input

-- ============================================================================
-- Properties for trim
-- ============================================================================

prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
    let once = trim input
        twice = trim once
    in once == twice

prop_trimRemovesOnlyWhitespace :: String -> Bool
prop_trimRemovesOnlyWhitespace input =
    let trimmed = trim input
        hasLeadingWhitespace = not (null input) && isSpace (head input)
        hasTrailingWhitespace = not (null input) && isSpace (last input)
    in if hasLeadingWhitespace || hasTrailingWhitespace
       then length trimmed < length input
       else trimmed == input

prop_trimPreservesContent :: String -> Bool
prop_trimPreservesContent input =
    let trimmed = trim input
        core = dropWhile isSpace (reverse (dropWhile isSpace (reverse input)))
    in trimmed == core

-- ============================================================================
-- Properties for comment removal
-- ============================================================================

prop_removeLineCommentsPreservesContent :: String -> Bool
prop_removeLineCommentsPreservesContent input =
    let withoutComments = removeLineComments input
        linesInput = lines input
        linesOutput = lines withoutComments
    in length linesInput == length linesOutput

prop_removeCommentsShrinksOrPreserves :: String -> Bool
prop_removeCommentsShrinksOrPreserves input =
    let withoutComments = removeComments input
    in length withoutComments <= length input

prop_removeCommentsNested :: String -> Bool
prop_removeCommentsNested input =
    let withNested = input ++ "/* outer /* inner */ still outer */ end"
        withoutComments = removeComments withNested
    in not ("/*" `isInfixOf` withoutComments) && not ("*/" `isInfixOf` withoutComments)
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]

-- ============================================================================
-- Properties for indentation
-- ============================================================================

prop_normalizeIndentationPreservesStructure :: String -> Bool
prop_normalizeIndentationPreservesStructure input =
    let normalized = normalizeIndentation input
        originalLines = lines input
        normalizedLines = lines normalized
    in length originalLines == length normalizedLines

prop_forceSingleTabCreatesConsistentFormat :: String -> Bool
prop_forceSingleTabCreatesConsistentFormat input =
    let formatted = forceSingleTabIndentation input
        nonEmptyLines = filter (not . null) (lines formatted)
    in all (\line -> take 1 line == "\t") nonEmptyLines

-- ============================================================================
-- Properties for search functions
-- ============================================================================

prop_breakOnCorrectness :: String -> String -> Bool
prop_breakOnCorrectness pattern text
    | null pattern = breakOn pattern text == ("", text)
    | pattern `isInfixOf` text = 
        let (before, after) = breakOn pattern text
        in before ++ pattern ++ after == text
    | otherwise = breakOn pattern text == (text, "")
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]

prop_breakOnEmptyPattern :: String -> Bool
prop_breakOnEmptyPattern text = breakOn "" text == ("", text)
