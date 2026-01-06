{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewStringProcessingBoundarySpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (property) as QC
import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

-- ============================================================================
-- String Processing Boundary Condition QuickCheck Tests
-- ============================================================================

-- | Test that trim handles whitespace-only strings correctly
prop_trim_whitespace_only :: String -> Bool
prop_trim_whitespace_only s = 
    let whitespaceOnly = filter isSpace s
        trimmed = trim whitespaceOnly
    in null trimmed

-- | Test that trim preserves non-whitespace characters
prop_trim_preserves_non_whitespace :: String -> Bool
prop_trim_preserves_non_whitespace s = 
    let nonWhitespace = L.filter (not . isSpace) s
        trimmed = trim s
        trimmedNonWhitespace = L.filter (not . isSpace) trimmed
    in trimmedNonWhitespace == nonWhitespace

-- | Test that splitBy handles delimiter-only strings correctly
prop_splitBy_delimiter_only :: Char -> Bool
prop_splitBy_delimiter_only delim = 
    let delimOnly = replicate 5 delim
        result = splitBy delim delimOnly
    in L.length result == 6 && L.all null result

-- | Test that splitByCollapsed handles delimiter-only strings correctly
prop_splitByCollapsed_delimiter_only :: Char -> Bool
prop_splitByCollapsed_delimiter_only delim = 
    let delimOnly = replicate 5 delim
        result = splitByCollapsed delim delimOnly
    in null result

-- | Test that splitBy handles strings without delimiter
prop_splitBy_no_delimiter :: Char -> String -> Bool
prop_splitBy_no_delimiter delim s = 
    not (delim `elem` s) ==> 
    splitBy delim s == [s]

-- | Test that splitByCollapsed handles strings without delimiter
prop_splitByCollapsed_no_delimiter :: Char -> String -> Bool
prop_splitByCollapsed_no_delimiter delim s = 
    not (delim `elem` s) ==> 
    splitByCollapsed delim s == [s]

-- | Test that removeLineComments handles strings without comments
prop_removeLineComments_no_comments :: String -> Bool
prop_removeLineComments_no_comments s = 
    not ("//" `L.isPrefixOf` s) ==> 
    removeLineComments s == s

-- | Test that removeLineComments removes lines starting with //
prop_removeLineComments_removes_comment_lines :: String -> String -> Bool
prop_removeLineComments_removes_comment_lines prefix suffix = 
    let commentLine = prefix ++ "//" ++ suffix
        result = removeLineComments commentLine
    in not ("//" `L.isInfixOf` result)

-- | Test that removeComments handles strings without block comments
prop_removeComments_no_block_comments :: String -> Bool
prop_removeComments_no_block_comments s = 
    not ("/*" `L.isInfixOf` s) ==> 
    removeComments s == s

-- | Test that removeComments removes block comments
prop_removeComments_removes_block_comments :: String -> String -> Bool
prop_removeComments_removes_block_comments before after = 
    let withComment = before ++ "/*" ++ "comment" ++ "*/" ++ after
        result = removeComments withComment
    in not ("/*" `L.isInfixOf` result) && not ("*/" `L.isInfixOf` result)

-- | Test that trim is idempotent on boundary cases
prop_trim_boundary_idempotent :: String -> Bool
prop_trim_boundary_idempotent s = 
    let trimmed = trim s
        doubleTrimmed = trim trimmed
    in trimmed == doubleTrimmed

-- | Test that splitBy preserves string L.length (including delimiters)
prop_splitBy_length_preservation :: Char -> String -> Bool
prop_splitBy_length_preservation delim s = 
    let parts = splitBy delim s
        reconstructed = L.concat parts ++ replicate (L.length (L.filter (== delim) s)) [delim]
    in L.length reconstructed == L.length s

-- | Test that splitByCollapsed reduces L.or maintains L.length
prop_splitByCollapsed_length_reduction :: Char -> String -> Bool
prop_splitByCollapsed_length_reduction delim s = 
    let originalParts = splitBy delim s
        collapsedParts = splitByCollapsed delim s
    in L.length collapsedParts <= L.length originalParts

-- | Test that trim handles Unicode whitespace correctly
prop_trim_unicode_whitespace :: String -> Bool
prop_trim_unicode_whitespace s = 
    let withUnicode = s ++ "\x00A0\x2000\x3000" ++ s  -- Add various Unicode whitespace
        trimmed = trim withUnicode
    in not (L.any isSpace (take 1 trimmed)) && not (L.any isSpace (take 1 (L.reverse trimmed)))

-- | Test that removeLineComments preserves non-comment content
prop_removeLineComments_preserves_content :: String -> String -> Bool
prop_removeLineComments_preserves_content before after = 
    let line = before ++ " code " ++ after
        result = removeLineComments line
    in before `L.isInfixOf` result && after `L.isInfixOf` result

-- | Test that removeComments handles nested block comments gracefully
prop_removeComments_nested :: String -> String -> String -> Bool
prop_removeComments_nested outer inner content = 
    let nested = "/* outer " ++ "/* inner " ++ content ++ " */" ++ " */"
        result = removeComments nested
    in not ("/*" `L.isInfixOf` result) && not ("*/" `L.isInfixOf` result)

-- | Test that trim handles empty string correctly
prop_trim_empty_string :: Bool
prop_trim_empty_string = trim "" == ""

-- | Test that splitBy handles empty string correctly
prop_splitBy_empty_string :: Char -> Bool
prop_splitBy_empty_string delim = splitBy delim "" == [""]

-- | Test that splitByCollapsed handles empty string correctly
prop_splitByCollapsed_empty_string :: Char -> Bool
prop_splitByCollapsed_empty_string delim = splitByCollapsed delim "" == []

-- ============================================================================
-- Test Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "String Processing Boundary Condition QuickCheck Tests"
  [ QC.testProperty "trim handles whitespace-only strings correctly" prop_trim_whitespace_only
  , QC.testProperty "trim preserves non-whitespace characters" prop_trim_preserves_non_whitespace
  , QC.testProperty "splitBy handles delimiter-only strings correctly" prop_splitBy_delimiter_only
  , QC.testProperty "splitByCollapsed handles delimiter-only strings correctly" prop_splitByCollapsed_delimiter_only
  , QC.testProperty "splitBy handles strings without delimiter" prop_splitBy_no_delimiter
  , QC.testProperty "splitByCollapsed handles strings without delimiter" prop_splitByCollapsed_no_delimiter
  , QC.testProperty "removeLineComments handles strings without comments" prop_removeLineComments_no_comments
  , QC.testProperty "removeLineComments removes lines starting with //" prop_removeLineComments_removes_comment_lines
  , QC.testProperty "removeComments handles strings without block comments" prop_removeComments_no_block_comments
  , QC.testProperty "removeComments removes block comments" prop_removeComments_removes_block_comments
  , QC.testProperty "trim is idempotent on boundary cases" prop_trim_boundary_idempotent
  , QC.testProperty "splitBy L.length preservation" prop_splitBy_length_preservation
  , QC.testProperty "splitByCollapsed reduces L.or maintains L.length" prop_splitByCollapsed_length_reduction
  , QC.testProperty "trim handles Unicode whitespace correctly" prop_trim_unicode_whitespace
  , QC.testProperty "removeLineComments preserves non-comment content" prop_removeLineComments_preserves_content
  , QC.testProperty "removeComments handles nested block comments gracefully" prop_removeComments_nested
  , QC.testProperty "trim handles empty string correctly" prop_trim_empty_string
  , QC.testProperty "splitBy handles empty string correctly" prop_splitBy_empty_string
  , QC.testProperty "splitByCollapsed handles empty string correctly" prop_splitByCollapsed_empty_string
  ]