{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.BoundaryConditionsEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  , safeProcessString
  , isValidChar
  )
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , spanBetween
  , mergeSpans
  , isValidSpan
  )
import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  )
import Data.Char (isSpace, isControl)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T

-- Arbitrary instances for testing
instance Arbitrary SourcePos where
  arbitrary = do
    line <- arbitrary `suchThat` (> 0)
    column <- arbitrary `suchThat` (> 0)
    offset <- arbitrary `suchThat` (>= 0)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ spanBetween start end

-- ============================================================================
-- Boundary Conditions QuickCheck Tests
-- ============================================================================

-- | Test empty and single-character inputs for Utils
prop_utils_trim_empty :: Bool
prop_utils_trim_empty = trim "" == ""

prop_utils_trim_single_char :: Char -> Bool
prop_utils_trim_single_char c = 
  let result = trim [c]
  in if isSpace c then result == "" else result == [c]

prop_utils_splitBy_empty_input :: Char -> Bool
prop_utils_splitBy_empty_input delim = splitBy delim "" == []

prop_utils_splitBy_single_char :: Char -> Char -> Bool
prop_utils_splitBy_single_char delim c = 
  if c == delim 
  then splitBy delim [c] == ["", ""]
  else splitBy delim [c] == [[c]]

prop_utils_splitByCollapsed_empty_input :: Char -> Bool
prop_utils_splitByCollapsed_empty_input delim = splitByCollapsed delim "" == []

prop_utils_removeComments_empty :: Bool
prop_utils_removeComments_empty = removeComments "" == ""

prop_utils_removeLineComments_empty :: Bool
prop_utils_removeLineComments_empty = removeLineComments "" == ""

prop_utils_normalizeIndentation_empty :: Bool
prop_utils_normalizeIndentation_empty = normalizeIndentation "" == ""

prop_utils_breakOn_empty_pattern :: String -> Bool
prop_utils_breakOn_empty_pattern s = breakOn "" s == ("", s)

prop_utils_breakOn_empty_input :: String -> Property
prop_utils_breakOn_empty_input pat = 
  not (null pat) ==> breakOn pat "" == ("", "")

prop_utils_safeProcessString_empty :: Bool
prop_utils_safeProcessString_empty = 
  case safeProcessString "" of
    Left _ -> False
    Right result -> result == ""

-- | Test large inputs for Utils
prop_utils_large_input_trim :: Int -> Property
prop_utils_large_input_trim n = 
  n >= 0 && n <= 1000 ==>
    let largeInput = replicate n ' ' ++ "content" ++ replicate n ' '
        result = trim largeInput
    in result == "content"

prop_utils_large_input_split :: Int -> Char -> Property
prop_utils_large_input_split n delim = 
  n >= 0 && n <= 1000 ==>
    let largeInput = replicate n delim
        result = splitBy delim largeInput
    in length result == n + 1 && all (== "") result

prop_utils_large_input_comments :: Int -> Property
prop_utils_large_input_comments n = 
  n >= 0 && n <= 1000 ==>
    let largeContent = concat (replicate n "content\n")
        withComments = largeContent ++ concat (replicate n "// comment\n")
        result = removeComments withComments
    in "content" `isInfixOf` result

-- | Test boundary values for SourceLocation
prop_sourcelocation_zero_positions :: Bool
prop_sourcelocation_zero_positions = 
  let pos = SourcePos 0 0 0
  in posLine pos == 0 && posColumn pos == 0 && posOffset pos == 0

prop_sourcelocation_negative_positions :: Int -> Int -> Int -> Bool
prop_sourcelocation_negative_positions line column offset = 
  let pos = SourcePos line column offset
  in posLine pos == line && posColumn pos == column && posOffset pos == offset

prop_sourcelocation_large_positions :: Int -> Property
prop_sourcelocation_large_positions n = 
  property $ n >= 0 && n <= 1000000 ==>
    let pos = SourcePos n n n
    in posLine pos == n && posColumn pos == n && posOffset pos == n

prop_sourcelocation_span_same_position :: SourcePos -> Bool
prop_sourcelocation_span_same_position pos = 
  let span = spanBetween pos pos
  in spanStart span == pos && spanEnd span == pos

prop_sourcelocation_merge_empty_spans :: SourcePos -> SourcePos -> Bool
prop_sourcelocation_merge_empty_spans pos1 pos2 = 
  let span1 = spanBetween pos1 pos1
      span2 = spanBetween pos2 pos2
      merged = mergeSpans span1 span2
  in spanStart merged == spanStart span1 || spanStart merged == spanStart span2

-- | Test boundary values for Parser
prop_parser_empty_input :: Property
prop_parser_empty_input = 
  let result = parseTypus ""
  in case result of
       Left _ -> property True
       Right typusFile -> property (tfBlocks typusFile == [])

prop_parser_whitespace_only :: String -> Property
prop_parser_whitespace_only s = 
  all isSpace s ==> 
    let result = parseTypus s
    in case result of
         Left _ -> property True
         Right typusFile -> property (tfBlocks typusFile == [])

prop_parser_very_long_line :: Int -> Property
prop_parser_very_long_line n = 
  n >= 0 && n <= 10000 ==>
    let longLine = replicate n 'a'
        result = parseTypus longLine
    in case result of
         Left _ -> property True
         Right typusFile ->
           let blocks = tfBlocks typusFile
           in not (null blocks) ==> 
              let firstBlock = head blocks
                  blockContent = cbContent firstBlock
              in length blockContent >= n

prop_parser_many_directives :: Int -> Property
prop_parser_many_directives n = 
  n >= 0 && n <= 100 ==>
    let directives = concat (replicate n "// build: tag\n")
        result = parseTypus directives
    in case result of
         Left _ -> property True
         Right typusFile -> 
           let buildTags = tfBuildTags typusFile
           in property (length buildTags == n)

-- | Test special characters and edge cases
prop_utils_special_characters_trim :: String -> Property
prop_utils_special_characters_trim s = 
  let specialChars = "\t\n\r"
      withSpecial = specialChars ++ s ++ specialChars
      result = trim withSpecial
  in property $ not (null result) ==> 
      not (isSpace (head result)) && not (isSpace (last result))

prop_utils_unicode_characters :: String -> Property
prop_utils_unicode_characters s = 
  let unicode = "中文测试ñáéíóú"
      withUnicode = s ++ unicode
      result = trim withUnicode
  in property $ unicode `isInfixOf` result

prop_utils_control_characters :: String -> Property
prop_utils_control_characters s = 
  let controlChars = "\x00\x01\x02\x03\x04"
      withControl = s ++ controlChars
      result = safeProcessString withControl
  in case result of
    Left _ -> property True
    Right processed -> property (not (any (\c -> c < ' ' && c /= '\n' && c /= '\r' && c /= '\t') processed))

prop_sourcelocation_tab_positions :: Int -> Property
prop_sourcelocation_tab_positions n = 
  property $ n >= 0 && n <= 100 ==>
    let pos = SourcePos 1 (n * 8) 0
        newPos = posAfter '\t' pos
        expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
    in posColumn newPos == expectedColumn

-- | Test error conditions
prop_utils_invalid_split_char :: Char -> String -> Bool
prop_utils_invalid_split_char delim s = 
  let result = splitBy delim s
      reconstructed = concat result
  in reconstructed == filter (/= delim) s

prop_sourcelocation_invalid_span :: SourcePos -> SourcePos -> Bool
prop_sourcelocation_invalid_span start end = 
  let span = spanBetween end start  -- Swapped order
      valid = isValidSpan span
  in if start > end then not valid else valid  -- Should be invalid if start > end

prop_parser_malformed_directives :: String -> Property
prop_parser_malformed_directives directive = 
  let malformed = "// " ++ directive ++ "\n"  -- Missing ": value"
      result = parseTypus malformed
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
             buildTags = tfBuildTags typusFile
         in property (not (null blocks) || null buildTags)  -- Either has blocks or no build tags

-- | Test performance boundary conditions
prop_utils_large_string_processing :: Int -> Property
prop_utils_large_string_processing n = 
  n >= 0 && n <= 5000 ==>
    let largeString = concat (replicate n "test ")
        trimmed = trim largeString
        parts = splitBy ' ' largeString
    in length parts >= n && head trimmed == 't'

prop_sourcelocation_large_span_operations :: Int -> Property
prop_sourcelocation_large_span_operations n = 
  n >= 0 && n <= 1000 ==>
    let start = SourcePos 1 1 0
        end = SourcePos n n (n * n)
        span = spanBetween start end
        merged = mergeSpans span span
    in spanStart merged == start && spanEnd merged == end

-- | Test consistency under stress
prop_utils_consistency_under_stress :: String -> String -> Bool
prop_utils_consistency_under_stress s1 s2 = 
  let combined = s1 ++ s2
      trimmed1 = trim s1
      trimmed2 = trim s2
      trimmedCombined = trim combined
  in trimmedCombined == trim (trimmed1 ++ trimmed2)

prop_sourcelocation_consistency_under_stress :: SourcePos -> SourcePos -> SourcePos -> Bool
prop_sourcelocation_consistency_under_stress pos1 pos2 pos3 = 
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in spanStart merged1 == spanStart merged2 && spanEnd merged1 == spanEnd merged2

-- | Test boundary conditions for character validation
prop_utils_character_validation_boundary :: Char -> Bool
prop_utils_character_validation_boundary c = 
  let isValid = isValidChar c
      isControlChar = isControl c && c /= '\n' && c /= '\r' && c /= '\t'
  in if isControlChar then not isValid else isValid || c < ' '

-- | Test extreme indentation scenarios
prop_utils_extreme_indentation :: Int -> Property
prop_utils_extreme_indentation n = 
  n >= 0 && n <= 1000 ==>
    let indented = concat (replicate n " ") ++ "content\n" ++ concat (replicate (n `div` 2) " ") ++ "more\n"
        normalized = normalizeIndentation indented
        lines' = lines normalized
    in all (\line -> not (null line) && not (all isSpace line)) lines'

-- | Test comment edge cases
prop_utils_comment_edge_cases :: String -> Property
prop_utils_comment_edge_cases s = 
  let withLineComment = s ++ "// comment"
      withBlockComment = s ++ "/* comment */"
      withoutLine = removeLineComments withLineComment
      withoutBlock = removeComments withBlockComment
  in property $ s `isInfixOf` withoutLine && s `isInfixOf` withoutBlock

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Boundary Conditions Enhanced QuickCheck Properties"
  [ -- Empty and single-character inputs for Utils
    testProperty "trim empty string" prop_utils_trim_empty,
    testProperty "trim single character" prop_utils_trim_single_char,
    testProperty "splitBy empty input" prop_utils_splitBy_empty_input,
    testProperty "splitBy single character" prop_utils_splitBy_single_char,
    testProperty "splitByCollapsed empty input" prop_utils_splitByCollapsed_empty_input,
    testProperty "removeComments empty input" prop_utils_removeComments_empty,
    testProperty "removeLineComments empty input" prop_utils_removeLineComments_empty,
    testProperty "normalizeIndentation empty input" prop_utils_normalizeIndentation_empty,
    testProperty "breakOn empty pattern" prop_utils_breakOn_empty_pattern,
    testProperty "breakOn empty input" prop_utils_breakOn_empty_input,
    testProperty "safeProcessString empty input" prop_utils_safeProcessString_empty,
    
    -- Large inputs for Utils
    testProperty "large input trim" prop_utils_large_input_trim,
    testProperty "large input split" prop_utils_large_input_split,
    testProperty "large input comments" prop_utils_large_input_comments,
    
    -- Boundary values for SourceLocation
    testProperty "zero positions" prop_sourcelocation_zero_positions,
    testProperty "negative positions" prop_sourcelocation_negative_positions,
    testProperty "large positions" prop_sourcelocation_large_positions,
    testProperty "span with same position" prop_sourcelocation_span_same_position,
    testProperty "merge empty spans" prop_sourcelocation_merge_empty_spans,
    
    -- Boundary values for Parser
    testProperty "parser empty input" prop_parser_empty_input,
    testProperty "parser whitespace only" prop_parser_whitespace_only,
    testProperty "parser very long line" prop_parser_very_long_line,
    testProperty "parser many directives" prop_parser_many_directives,
    
    -- Special characters and edge cases
    testProperty "special characters trim" prop_utils_special_characters_trim,
    testProperty "unicode characters" prop_utils_unicode_characters,
    testProperty "control characters" prop_utils_control_characters,
    testProperty "tab positions" prop_sourcelocation_tab_positions,
    
    -- Error conditions
    testProperty "invalid split char" prop_utils_invalid_split_char,
    testProperty "invalid span" prop_sourcelocation_invalid_span,
    testProperty "malformed directives" prop_parser_malformed_directives,
    
    -- Performance boundary conditions
    testProperty "large string processing" prop_utils_large_string_processing,
    testProperty "large span operations" prop_sourcelocation_large_span_operations,
    
    -- Consistency under stress
    testProperty "utils consistency under stress" prop_utils_consistency_under_stress,
    testProperty "sourcelocation consistency under stress" prop_sourcelocation_consistency_under_stress,
    
    -- Character validation boundary
    testProperty "character validation boundary" prop_utils_character_validation_boundary,
    
    -- Extreme indentation scenarios
    testProperty "extreme indentation" prop_utils_extreme_indentation,
    
    -- Comment edge cases
    testProperty "comment edge cases" prop_utils_comment_edge_cases
  ]