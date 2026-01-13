{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.UtilsEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.List as L

-- ============================================================================
-- Utils Module QuickCheck Tests
-- ============================================================================

-- | Test trim function properties
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_trim_no_leading_trailing_spaces :: String -> Bool
prop_trim_no_leading_trailing_spaces s = 
  let trimmed = trim s
  in null trimmed || 
     (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

prop_trim_preserves_internal_spaces :: String -> Bool
prop_trim_preserves_internal_spaces s =
  let trimmed = trim s
      internalSpaces = filter isSpace (take (length trimmed - 1) (drop 1 trimmed))
  in length internalSpaces == length (filter isSpace (take (length s - 1) (drop 1 s)))

-- | Test splitBy function properties
prop_splitBy_consistency :: Char -> String -> Bool
prop_splitBy_consistency delim s = 
  concat (splitBy delim s) == filter (/= delim) s

prop_splitBy_empty_segments :: Char -> Property
prop_splitBy_empty_segments delim = 
  forAll (listOf (elements [delim])) $ \s ->
    length (splitBy delim s) == length s + 1

prop_splitBy_single_char :: Char -> Char -> Property
prop_splitBy_single_char delim c = 
  c /= delim ==> 
    splitBy delim [c] == [[c]]

prop_splitBy_preserves_order :: Char -> String -> Bool
prop_splitBy_preserves_order delim s = 
  let parts = splitBy delim
      original = filter (/= delim) s
      reconstructed = concat parts
  in reconstructed == original

-- | Test splitByCollapsed function properties
prop_splitByCollapsed_no_empty_segments :: Char -> String -> Bool
prop_splitByCollapsed_no_empty_segments delim s = 
  all (not . null) (splitByCollapsed delim s)

prop_splitByCollapsed_relationship :: Char -> String -> Bool
prop_splitByCollapsed_relationship delim s = 
  splitByCollapsed delim s == filter (not . null) (splitBy delim s)

-- | Test splitByComma function properties
prop_splitByComma_equivalence :: String -> Bool
prop_splitByComma_equivalence s = splitByComma s == splitBy ',' s

-- | Test splitByCommaCollapsed function properties
prop_splitByCommaCollapsed_equivalence :: String -> Bool
prop_splitByCommaCollapsed_equivalence s = 
  splitByCommaCollapsed s == splitByCollapsed ',' s

-- | Test removeLineComments function properties
prop_removeLineComments_no_comments :: String -> Bool
prop_removeLineComments_no_comments s = 
  not ("//" `L.isInfixOf` s) ==> removeLineComments s == s

prop_removeLineComments_preserves_non_comment_parts :: String -> Property
prop_removeLineComments_preserves_non_comment_parts = 
  forAll (listOf $ elements $ ['a'..'z'] ++ " \t\n") $ \s ->
    let withoutComments = removeLineComments s
        linesWithoutComments = lines withoutComments
        originalLines = lines s
        nonCommentLines = filter (not . ("//" `L.isPrefixOf`)) originalLines
    in length linesWithoutComments == length nonCommentLines

prop_removeLineComments_handles_strings :: String -> Property
prop_removeLineComments_handles_strings = 
  forAll (listOf $ elements $ ['a'..'z'] ++ "\"/ \t\n") $ \s ->
    let result = removeLineComments s
        hasString = "\"" `L.isInfixOf` s
    in hasString ==> "//" `L.isInfixOf` result || not ("//" `L.isInfixOf` s)

-- | Test removeComments function properties
prop_removeComments_no_comments :: String -> Bool
prop_removeComments_no_comments s = 
  not ("//" `L.isInfixOf` s) && not ("/*" `L.isInfixOf` s) ==> 
  removeComments s == s

prop_removeComments_removes_line_comments :: String -> Property
prop_removeComments_removes_line_comments = 
  forAll (listOf $ elements $ ['a'..'z'] ++ "/ \n") $ \s ->
    let hasLineComment = "//" `L.isInfixOf` s
        result = removeComments s
    in hasLineComment ==> not ("//" `L.isInfixOf` result)

prop_removeComments_removes_block_comments :: String -> Property
prop_removeComments_removes_block_comments = 
  forAll (listOf $ elements $ ['a'..'z'] ++ "* \n") $ \s ->
    let hasBlockComment = "/*" `L.isInfixOf` s && "*/" `L.isInfixOf` s
        result = removeComments s
    in hasBlockComment ==> not ("/*" `L.isInfixOf` result) && not ("*/" `L.isInfixOf` result)

-- | Test normalizeIndentation function properties
prop_normalizeIndentation_preserves_line_count :: String -> Bool
prop_normalizeIndentation_preserves_line_count s = 
  let originalLines = lines s
      normalizedLines = lines (normalizeIndentation s)
  in length originalLines == length normalizedLines

prop_normalizeIndentation_idempotent :: String -> Bool
prop_normalizeIndentation_idempotent s = 
  normalizeIndentation (normalizeIndentation s) == normalizeIndentation s

prop_normalizeIndentation_preserves_relative_indentation :: String -> Property
prop_normalizeIndentation_preserves_relative_indentation = 
  forAll (listOf $ listOf $ elements $ ' ' : ['a'..'z']) $ \l ->
    let s = unlines l
        result = normalizeIndentation s
        resultLines = lines result
    in length resultLines == length l

-- | Test forceSingleTabIndentation function properties
prop_forceSingleTabIndentation_adds_tab :: String -> Property
prop_forceSingleTabIndentation_adds_tab = 
  forAll (listOf $ elements $ ['a'..'z'] ++ " \t\n") $ \s ->
    let result = forceSingleTabIndentation s
        resultLines = lines result
    in all (\line -> null line || '\t' `elem` take 1 line) resultLines

-- | Test fixIndentation function properties
prop_fixIndentation_equivalence :: String -> Bool
prop_fixIndentation_equivalence s = fixIndentation s == normalizeIndentation s

-- | Test breakOn function properties
prop_breakOn_empty_pattern :: String -> Bool
prop_breakOn_empty_pattern s = breakOn "" s == ("", s)

prop_breakOn_pattern_not_found :: String -> String -> Property
prop_breakOn_pattern_not_found pat s = 
  not (pat `L.isInfixOf` s) ==> breakOn pat s == (s, "")

prop_breakOn_pattern_found :: String -> String -> Property
prop_breakOn_pattern_found pat s = 
  pat `L.isInfixOf` s ==> 
    let (before, after) = breakOn pat s
    in pat `L.isPrefixOf` (before ++ pat ++ after) && 
       before ++ pat ++ after == s

prop_breakOn_preserves_total_content :: String -> String -> Bool
prop_breakOn_preserves_total_content pat s = 
  let (before, after) = breakOn pat s
  in before ++ pat ++ after == s

-- | Test safeProcessString function properties
prop_safeProcessString_removes_control_chars :: String -> Property
prop_safeProcessString_removes_control_chars = 
  forAll (listOf $ elements $ ['a'..'z'] ++ "\x00\x01\x02") $ \s ->
    case safeProcessString s of
      Left _ -> property True
      Right result -> all (\c -> c >= ' ' || c == '\n' || c == '\r' || c == '\t') result

prop_safeProcessString_preserves_valid_chars :: String -> Property
prop_safeProcessString_preserves_valid_chars = 
  forAll (listOf $ elements $ ['a'..'z'] ++ " \t\n\r") $ \s ->
    case safeProcessString s of
      Left _ -> property False
      Right result -> result == s

-- | Test isValidChar function properties
prop_isValidChar_valid_chars :: Char -> Property
prop_isValidChar_valid_chars c = 
  (c >= ' ' || c == '\n' || c == '\r' || c == '\t') ==> isValidChar c

prop_isValidChar_invalid_chars :: Char -> Property
prop_isValidChar_invalid_chars c = 
  (c < ' ' && c /= '\n' && c /= '\r' && c /= '\t') ==> not (isValidChar c)

-- | Test combined properties
prop_trim_splitBy_interaction :: Char -> String -> Bool
prop_trim_splitBy_interaction delim s = 
  let parts = splitBy delim s
      trimmedParts = map trim parts
  in concat trimmedParts == filter (/= delim) (map (\c -> if isSpace c then ' ' else c) s)

prop_removeComments_normalizeIndentation_interaction :: String -> Property
prop_removeComments_normalizeIndentation_interaction = 
  forAll (listOf $ elements $ ['a'..'z'] ++ " \t\n/*/") $ \s ->
    let withoutComments = removeComments s
        normalized = normalizeIndentation withoutComments
        normalizedOriginal = normalizeIndentation s
        withoutCommentsNormalized = removeComments normalizedOriginal
    in lines normalized == lines withoutCommentsNormalized

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Utils Module QuickCheck Properties"
  [ -- Trim function tests
    testProperty "trim is idempotent" prop_trim_idempotent,
    testProperty "trim removes leading and trailing spaces" prop_trim_no_leading_trailing_spaces,
    testProperty "trim preserves internal spaces" prop_trim_preserves_internal_spaces,
    
    -- SplitBy function tests
    testProperty "splitBy is consistent" prop_splitBy_consistency,
    testProperty "splitBy creates empty segments" prop_splitBy_empty_segments,
    testProperty "splitBy handles single character" prop_splitBy_single_char,
    testProperty "splitBy preserves order" prop_splitBy_preserves_order,
    
    -- SplitByCollapsed function tests
    testProperty "splitByCollapsed has no empty segments" prop_splitByCollapsed_no_empty_segments,
    testProperty "splitByCollapsed relationship with splitBy" prop_splitByCollapsed_relationship,
    
    -- Comma split functions tests
    testProperty "splitByComma is equivalent to splitBy ','" prop_splitByComma_equivalence,
    testProperty "splitByCommaCollapsed is equivalent to splitByCollapsed ','" prop_splitByCommaCollapsed_equivalence,
    
    -- Comment removal tests
    testProperty "removeLineComments preserves strings without comments" prop_removeLineComments_no_comments,
    testProperty "removeLineComments preserves non-comment parts" prop_removeLineComments_preserves_non_comment_parts,
    testProperty "removeLineComments handles strings correctly" prop_removeLineComments_handles_strings,
    
    testProperty "removeComments preserves strings without comments" prop_removeComments_no_comments,
    testProperty "removeComments removes line comments" prop_removeComments_removes_line_comments,
    testProperty "removeComments removes block comments" prop_removeComments_removes_block_comments,
    
    -- Indentation tests
    testProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_line_count,
    testProperty "normalizeIndentation is idempotent" prop_normalizeIndentation_idempotent,
    testProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative_indentation,
    
    testProperty "forceSingleTabIndentation adds tab to non-empty lines" prop_forceSingleTabIndentation_adds_tab,
    testProperty "fixIndentation is equivalent to normalizeIndentation" prop_fixIndentation_equivalence,
    
    -- BreakOn function tests
    testProperty "breakOn handles empty pattern" prop_breakOn_empty_pattern,
    testProperty "breakOn handles pattern not found" prop_breakOn_pattern_not_found,
    testProperty "breakOn handles pattern found" prop_breakOn_pattern_found,
    testProperty "breakOn preserves total content" prop_breakOn_preserves_total_content,
    
    -- Safe processing tests
    testProperty "safeProcessString removes control characters" prop_safeProcessString_removes_control_chars,
    testProperty "safeProcessString preserves valid characters" prop_safeProcessString_preserves_valid_chars,
    
    -- Character validation tests
    testProperty "isValidChar validates valid characters" prop_isValidChar_valid_chars,
    testProperty "isValidChar rejects invalid characters" prop_isValidChar_invalid_chars,
    
    -- Interaction tests
    testProperty "trim and splitBy interaction" prop_trim_splitBy_interaction,
    testProperty "removeComments and normalizeIndentation interaction" prop_removeComments_normalizeIndentation_interaction
  ]