{-# LANGUAGE CPP #-}
module Test.Unit.UtilsStringBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, 
                        Property, (===), forAll, counterexample)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
             removeLineComments, removeComments, normalizeIndentation, breakOn)

-- ============================================================================
-- Test data generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements [' ', '\t', '\n', '\r']

-- Generate strings with mixed content
genMixedString :: Gen String
genMixedString = do
  whitespace <- genWhitespaceString
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".,;:!@#$%^&*()[]{}<>+-*/="
  moreWhitespace <- genWhitespaceString
  return $ whitespace ++ content ++ moreWhitespace

-- Generate strings with comment patterns
genCommentString :: Gen String
genCommentString = do
  before <- listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['\n']
  commentType <- elements ["//", "/*"]
  comment <- listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['\n']
  after <- if commentType == "//"
           then listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['\n']
           else do
               afterContent <- listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['\n']
               endComment <- elements ["*/", ""]  -- Sometimes missing end comment
               return $ afterContent ++ endComment
  return $ before ++ commentType ++ comment ++ after

-- Generate strings with indentation
genIndentedString :: Gen String
genIndentedString = do
  numLines <- choose (1, 5)
  lines <- sequence $ replicate numLines $ do
    indent <- choose (0, 10)
    content <- listOf $ elements $ ['a'..'z'] ++ [' ']
    return $ replicate indent ' ' ++ content
  return $ unlines lines

-- ============================================================================
-- Properties for trim function
-- ============================================================================

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

prop_trim_no_leading_trailing_whitespace :: String -> Property
prop_trim_no_leading_trailing_whitespace s =
  let trimmed = trim s
  in counterexample ("Result: " ++ show trimmed) $
     not (null trimmed) ==> 
     (head trimmed /= ' ' && head trimmed /= '\t' && head trimmed /= '\n' && head trimmed /= '\r') &&
     (last trimmed /= ' ' && last trimmed /= '\t' && last trimmed /= '\n' && last trimmed /= '\r')

-- ============================================================================
-- Properties for splitBy functions
-- ============================================================================

prop_split_by_preserves_empty_segments :: Char -> String -> Property
prop_split_by_preserves_empty_segments delim s =
  let result = splitBy delim s
      reconstructed = concatMap (\seg -> seg ++ [delim]) (init result) ++ last result
  in length result > 1 ==> reconstructed === s

prop_split_by_collapsed_no_empty_segments :: Char -> String -> Property
prop_split_by_collapsed_no_empty_segments delim s =
  let result = splitByCollapsed delim s
  in all (not . null) result

-- ============================================================================
-- Properties for comment removal
-- ============================================================================

prop_remove_line_comments_preserves_newlines :: String -> Property
prop_remove_line_comments_preserves_newlines s =
  let originalLines = lines s
      processedLines = lines $ removeLineComments s
  in length processedLines === length originalLines

prop_remove_comments_no_comment_markers :: String -> Property
prop_remove_comments_no_comment_markers s =
  let noCommentString = filter (`notElem` "/") s
  in removeComments noCommentString === noCommentString

-- ============================================================================
-- Properties for indentation normalization
-- ============================================================================

prop_normalize_indentation_preserves_relative_structure :: Property
prop_normalize_indentation_preserves_relative_structure =
  forAll genIndentedString $ \s ->
    let normalized = normalizeIndentation s
        originalLines = lines s
        normalizedLines = lines normalized
        -- Check that non-empty lines maintain their relative indentation
        originalIndents = [length $ takeWhile isSpace line | line <- originalLines, not (all isSpace line)]
        normalizedIndents = [length $ takeWhile isSpace line | line <- normalizedLines, not (all isSpace line)]
    in case (originalIndents, normalizedIndents) of
         ([], []) -> property True
         (orig, norm) -> 
           if length orig == length norm
           then let minOrig = minimum orig
                    minNorm = minimum norm
                    adjustedOrig = map (\x -> x - minOrig) orig
                    adjustedNorm = map (\x -> x - minNorm) norm
                in adjustedOrig === adjustedNorm
           else property False
  where
    isSpace c = c == ' ' || c == '\t'

-- ============================================================================
-- Properties for breakOn function
-- ============================================================================

prop_break_on_empty_pattern :: String -> Property
prop_break_on_empty_pattern s =
  breakOn "" s === ("", s)

prop_break_on_pattern_not_found :: String -> String -> Property
prop_break_on_pattern_not_found pat s =
  not (pat `isInfixOf` s) ==> breakOn pat s === (s, "")

prop_break_on_roundtrip :: String -> String -> Property
prop_break_on_roundtrip pat s =
  pat `isInfixOf` s ==> 
  let (before, after) = breakOn pat s
  in before ++ pat ++ after === s
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils String Boundary QuickCheck Tests"
  [ testGroup "trim function properties"
    [ fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim removes leading/trailing whitespace" prop_trim_no_leading_trailing_whitespace
    ]
  , testGroup "splitBy function properties"
    [ fastProperty "splitBy preserves empty segments" prop_split_by_preserves_empty_segments
    , fastProperty "splitByCollapsed removes empty segments" prop_split_by_collapsed_no_empty_segments
    ]
  , testGroup "comment removal properties"
    [ fastProperty "removeLineComments preserves newlines" prop_remove_line_comments_preserves_newlines
    , fastProperty "removeComments handles strings without comments" prop_remove_comments_no_comment_markers
    ]
  , testGroup "indentation normalization properties"
    [ fastProperty "normalizeIndentation preserves relative structure" prop_normalize_indentation_preserves_relative_structure
    ]
  , testGroup "breakOn function properties"
    [ fastProperty "breakOn handles empty pattern" prop_break_on_empty_pattern
    , fastProperty "breakOn handles pattern not found" prop_break_on_pattern_not_found
    , fastProperty "breakOn roundtrip property" prop_break_on_roundtrip
    ]
  ]