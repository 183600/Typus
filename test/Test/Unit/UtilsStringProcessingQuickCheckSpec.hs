{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.UtilsStringProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, listOf, elements)
import Test.Tasty.HUnit (testCase, (@?=))

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
              removeLineComments, removeComments, normalizeIndentation, breakOn)
import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary Char where
    arbitrary = oneof 
        [ elements ['a'..'z']
        , elements ['A'..'Z']
        , elements ['0'..'9']
        , elements " \t\n\r,;./\\[]{}()<>+-=*!@#$%^&*_~`|?\"'"
        ]

-- Generate strings with various whitespace patterns
genStringWithWhitespace :: Gen String
genStringWithWhitespace = listOf arbitrary

-- Generate strings specifically for split testing
genSplitTestString :: Gen String
genSplitTestString = do
    parts <- listOf $ listOf $ elements ['a'..'z']
    delim <- elements [',', ';', ':', '|']
    return $ concat $ intersperse delim parts
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse d (x:xs) = x : d : intersperse d xs

-- Generate strings with comments
genCommentString :: Gen String
genCommentString = do
    code <- listOf $ elements ['a'..'z']
    comment <- listOf $ elements ['A'..'Z']
    return $ concat code ++ "//" ++ comment

-- Generate strings with block comments
genBlockCommentString :: Gen String
genBlockCommentString = do
    before <- listOf $ elements ['a'..'z']
    inside <- listOf $ elements ['A'..'Z']
    after <- listOf $ elements ['0'..'9']
    return $ concat before ++ "/*" ++ concat inside ++ "*/" ++ concat after

-- ============================================================================
-- Properties for trim function
-- ============================================================================

prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_trim_no_leading_trailing_whitespace :: String -> Bool
prop_trim_no_leading_trailing_whitespace s = 
    let trimmed = trim s
    in null trimmed || 
       (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

prop_trim_preserves_internal_whitespace :: String -> Bool
prop_trim_preserves_internal_whitespace s =
    let trimmed = trim s
        originalInternal = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse s
    in length (filter isSpace trimmed) == length (filter isSpace originalInternal)

-- ============================================================================
-- Properties for splitBy function
-- ============================================================================

prop_split_by_preserves_content :: Char -> String -> Bool
prop_split_by_preserves_content delim s = 
    concat (splitBy delim s) == s

prop_split_by_empty_segments :: Char -> String -> Bool
prop_split_by_empty_segments delim s =
    let parts = splitBy delim s
        expectedEmpties = length $ filter (== delim) $ delim : s
    in length (filter null parts) == expectedEmpties

prop_split_by_single_char :: Char -> Bool
prop_split_by_single_char delim = splitBy delim [delim] == ["", ""]

-- ============================================================================
-- Properties for splitByCollapsed function
-- ============================================================================

prop_split_by_collapsed_no_empty :: Char -> String -> Bool
prop_split_by_collapsed_no_empty delim s = 
    all (not . null) (splitByCollapsed delim s)

prop_split_by_collapsed_correct_count :: Char -> String -> Bool
prop_split_by_collapsed_correct_count delim s =
    let normalParts = splitBy delim s
        collapsedParts = splitByCollapsed delim s
        nonEmptyNormal = filter (not . null) normalParts
    in length collapsedParts == length nonEmptyNormal

-- ============================================================================
-- Properties for removeLineComments function
-- ============================================================================

prop_remove_line_comments_preserves_non_comment_lines :: String -> Bool
prop_remove_line_comments_preserves_non_comment_lines s =
    let linesWithoutComments = lines $ removeLineComments s
        originalLines = lines s
        nonCommentOriginal = filter (not . ("//" `isPrefixOf`)) originalLines
    in length linesWithoutComments == length nonCommentOriginal

prop_remove_line_comments_no_comment_markers :: String -> Bool
prop_remove_line_comments_no_comment_markers s =
    let result = removeLineComments s
    in "//" `notElem` (words result)

-- ============================================================================
-- Properties for removeComments function
-- ============================================================================

prop_remove_comments_no_block_comments :: String -> Bool
prop_remove_comments_no_block_comments s =
    "/*" `notElem` (words $ removeComments s)

prop_remove_comments_preserves_line_structure :: String -> Bool
prop_remove_comments_preserves_line_structure s =
    let originalLines = length $ lines s
        resultLines = length $ lines $ removeComments s
    in resultLines <= originalLines  -- Comments may remove lines but never add

-- ============================================================================
-- Properties for normalizeIndentation function
-- ============================================================================

prop_normalize_indentation_preserves_relative_structure :: String -> Bool
prop_normalize_indentation_preserves_relative_structure s =
    let originalLines = lines s
        normalizedLines = lines $ normalizeIndentation s
        originalIndents = map (length . takeWhile isSpace) originalLines
        normalizedIndents = map (length . takeWhile isSpace) normalizedLines
        minOriginal = minimum $ 0 : filter (>0) originalIndents
        adjustedOriginal = map (\i -> if i > 0 then i - minOriginal else i) originalIndents
    in normalizedIndents == adjustedOriginal

prop_normalize_indentation_idempotent :: String -> Bool
prop_normalize_indentation_idempotent s = 
    normalizeIndentation (normalizeIndentation s) == normalizeIndentation s

-- ============================================================================
-- Properties for breakOn function
-- ============================================================================

prop_break_on_empty_pattern :: String -> Bool
prop_break_on_empty_pattern s =
    breakOn "" s == ("", s)

prop_break_on_found :: String -> String -> Bool
prop_break_on_found pat s =
    let (before, after) = breakOn pat s
    in if pat `isPrefixOf` s
       then before ++ pat ++ after == s
       else before == s && after == ""

prop_break_on_not_found :: String -> String -> Bool
prop_break_on_not_found pat s =
    not (pat `isInfixOf` s) ==> breakOn pat s == (s, "")

-- ============================================================================
-- Unit Tests for Edge Cases
-- ============================================================================

test_trim_empty :: IO ()
test_trim_empty = trim "" @?= ""

test_trim_all_whitespace :: IO ()
test_trim_all_whitespace = trim "   \t\n\r   " @?= ""

test_trim_preserves_single_word :: IO ()
test_trim_preserves_single_word = trim "hello" @?= "hello"

test_split_by_empty_string :: IO ()
test_split_by_empty_string = splitBy ',' "" @?= [""]

test_split_by_no_delimiter :: IO ()
test_split_by_no_delimiter = splitBy ',' "hello" @?= ["hello"]

test_remove_comments_empty :: IO ()
test_remove_comments_empty = removeComments "" @?= ""

test_remove_line_comments_only :: IO ()
test_remove_line_comments_only = removeLineComments "// This is a comment\n" @?= "\n"

test_break_on_empty_string :: IO ()
test_break_on_empty_string = breakOn "hello" "" @?= ("", "")

test_normalize_indentation_empty :: IO ()
test_normalize_indentation_empty = normalizeIndentation "" @?= ""

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils String Processing QuickCheck Tests"
    [ testGroup "trim function tests"
        [ testProperty "trim is idempotent" prop_trim_idempotent
        , testProperty "trim removes leading/trailing whitespace" prop_trim_no_leading_trailing_whitespace
        , testProperty "trim preserves internal whitespace" prop_trim_preserves_internal_whitespace
        , testCase "trim empty string" test_trim_empty
        , testCase "trim all whitespace" test_trim_all_whitespace
        , testCase "trim preserves single word" test_trim_preserves_single_word
        ]
    , testGroup "splitBy function tests"
        [ testProperty "splitBy preserves content" prop_split_by_preserves_content
        , testProperty "splitBy creates empty segments" prop_split_by_empty_segments
        , testProperty "splitBy single character" prop_split_by_single_char
        , testCase "splitBy empty string" test_split_by_empty_string
        , testCase "splitBy no delimiter" test_split_by_no_delimiter
        ]
    , testGroup "splitByCollapsed function tests"
        [ testProperty "splitByCollapsed has no empty segments" prop_split_by_collapsed_no_empty
        , testProperty "splitByCollapsed correct count" prop_split_by_collapsed_correct_count
        ]
    , testGroup "removeLineComments function tests"
        [ testProperty "removeLineComments preserves non-comment lines" prop_remove_line_comments_preserves_non_comment_lines
        , testProperty "removeLineComments removes comment markers" prop_remove_line_comments_no_comment_markers
        , testCase "removeLineComments only comments" test_remove_line_comments_only
        ]
    , testGroup "removeComments function tests"
        [ testProperty "removeComments removes block comments" prop_remove_comments_no_block_comments
        , testProperty "removeComments preserves line structure" prop_remove_comments_preserves_line_structure
        , testCase "removeComments empty string" test_remove_comments_empty
        ]
    , testGroup "normalizeIndentation function tests"
        [ testProperty "normalizeIndentation preserves relative structure" prop_normalize_indentation_preserves_relative_structure
        , testProperty "normalizeIndentation is idempotent" prop_normalize_indentation_idempotent
        , testCase "normalizeIndentation empty string" test_normalize_indentation_empty
        ]
    , testGroup "breakOn function tests"
        [ testProperty "breakOn empty pattern" prop_break_on_empty_pattern
        , testProperty "breakOn found pattern" prop_break_on_found
        , testProperty "breakOn not found pattern" prop_break_on_not_found
        , testCase "breakOn empty string" test_break_on_empty_string
        ]
    ]