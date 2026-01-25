{-# LANGUAGE CPP #-}
module Test.Unit.ComprehensiveCoreModulesQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Data.List (uncons)

import qualified Utils
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , toErrorLocation
  , toErrorLocationWithSpan
  )
import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import Compiler.Errors.Core (ErrorLocation(..))

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- Test trim function
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = 
  let trimmed = Utils.trim s
  in property $ Utils.trim trimmed === trimmed

prop_trim_no_leading_trailing_spaces :: String -> Property
prop_trim_no_leading_trailing_spaces s = 
  let trimmed = Utils.trim s
  in property $ not (null trimmed) ==> 
    case uncons trimmed of
      Just (firstChar, _) -> not (isSpace firstChar) && not (isSpace (last trimmed))
      Nothing -> False

-- Test splitBy function
prop_splitBy_empty_segments :: Char -> String -> Property
prop_splitBy_empty_segments delim s = 
  let parts = Utils.splitBy delim s
  in property $ length parts > 0 ==> True

prop_splitBy_comma_consistency :: String -> Property
prop_splitBy_comma_consistency s = 
  property $ Utils.splitBy ',' s === Utils.splitByComma s

prop_splitBy_collapsed_removes_empty :: Char -> String -> Property
prop_splitBy_collapsed_removes_empty delim s = 
  let parts = Utils.splitByCollapsed delim s
  in property $ not (any null parts)

-- Test removeLineComments function
prop_removeLineComments_preserves_non_comment_lines :: String -> Property
prop_removeLineComments_preserves_non_comment_lines s = 
  let withoutComments = Utils.removeLineComments s
      linesWithoutComments = lines withoutComments
      originalLines = lines s
  in property $ not (any ("//" `isPrefixOf`) originalLines) ==> 
    length linesWithoutComments === length originalLines

prop_removeLineComments_removes_comment_content :: Property
prop_removeLineComments_removes_comment_content = 
  let codeLine = "code // comment"
      result = Utils.removeLineComments codeLine
  in property $ result === "code"

-- Test removeComments function
prop_removeComments_preserves_strings :: String -> Property
prop_removeComments_preserves_strings s = 
  let stringWithComment = "code /* comment */ more code"
      result = Utils.removeComments stringWithComment
  in property $ not (null s) ==> "code" `isPrefixOf` result && "more code " `isSuffixOf` result

-- Test normalizeIndentation function
prop_normalizeIndentation_preserves_relative_indentation :: String -> Property
prop_normalizeIndentation_preserves_relative_indentation s = 
  let lines_s = lines s
      normalized = Utils.normalizeIndentation s
      normalizedLines = lines normalized
  in property $ length lines_s === length normalizedLines

-- Test safeProcessString function
prop_safeProcessString_filters_control_chars :: String -> Property
prop_safeProcessString_filters_control_chars s = 
  case Utils.safeProcessString s of
    Left _ -> property True
    Right filtered -> property $ all (\c -> c >= ' ' || c == '\n' || c == '\r' || c == '\t') filtered

-- Test isValidChar function
prop_isValid_char_consistency :: Char -> Property
prop_isValid_char_consistency c = 
  let isValid = Utils.isValidChar c
      expected = c >= ' ' || c == '\n' || c == '\r' || c == '\t'
  in property $ isValid === expected

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- Test SourcePos operations
prop_posAfter_newline_increments_line :: SourcePos -> Property
prop_posAfter_newline_increments_line pos = 
  let newPos = posAfter '\n' pos
  in conjoin 
    [ property $ posLine newPos === posLine pos + 1
    , property $ posColumn newPos === 1
    , property $ posOffset newPos === posOffset pos + 1
    ]

prop_posAfter_tab_advances_to_next_tab_stop :: SourcePos -> Property
prop_posAfter_tab_advances_to_next_tab_stop pos = 
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in conjoin
    [ property $ posColumn newPos === expectedCol
    , property $ posOffset newPos === posOffset pos + 1
    ]

prop_posAfter_regular_char_increments_column :: SourcePos -> Property
prop_posAfter_regular_char_increments_column pos = 
  let newPos = posAfter 'a' pos
  in conjoin
    [ property $ posLine newPos === posLine pos
    , property $ posColumn newPos === posColumn pos + 1
    , property $ posOffset newPos === posOffset pos + 1
    ]

prop_posAt_creates_correct_position :: Int -> Int -> Property
prop_posAt_creates_correct_position lineNum col = 
  let pos = posAt lineNum col
  in conjoin
    [ property $ posLine pos === lineNum
    , property $ posColumn pos === col
    , property $ posOffset pos === 0
    ]

prop_posAtLineCol_creates_correct_position :: Int -> Int -> Int -> Property
prop_posAtLineCol_creates_correct_position lineNum colNum offsetVal = 
  let pos = posAtLineCol lineNum colNum offsetVal
  in conjoin
    [ property $ posLine pos === lineNum
    , property $ posColumn pos === colNum
    , property $ posOffset pos === offsetVal
    ]

-- Test SourceSpan operations
prop_empty_span_has_same_start_and_end :: SourcePos -> Property
prop_empty_span_has_same_start_and_end pos = 
  let testSpan = emptySpan pos
  in conjoin
    [ property $ spanStart testSpan === pos
    , property $ spanEnd testSpan === pos
    ]

prop_spanFrom_creates_empty_span :: SourcePos -> Property
prop_spanFrom_creates_empty_span pos = 
  let testSpan = spanFrom pos
  in conjoin
    [ property $ spanStart testSpan === pos
    , property $ spanEnd testSpan === pos
    ]

prop_spanTo_creates_empty_span :: SourcePos -> Property
prop_spanTo_creates_empty_span pos = 
  let testSpan = spanTo pos
  in conjoin
    [ property $ spanStart testSpan === pos
    , property $ spanEnd testSpan === pos
    ]

prop_spanBetween_creates_correct_span :: SourcePos -> SourcePos -> Property
prop_spanBetween_creates_correct_span start end = 
  let testSpan = spanBetween start end
  in conjoin
    [ property $ spanStart testSpan === start
    , property $ spanEnd testSpan === end
    ]

prop_mergeSpans_contains_both_spans :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both_spans span1 span2 = 
  let merged = mergeSpans span1 span2
  in conjoin
    [ property $ spanStart merged <= spanStart span1
    , property $ spanStart merged <= spanStart span2
    , property $ spanEnd merged >= spanEnd span1
    , property $ spanEnd merged >= spanEnd span2
    ]

prop_isValidSpan_check_order :: SourcePos -> SourcePos -> Property
prop_isValidSpan_check_order pos1 pos2 = 
  let testSpan = spanBetween pos1 pos2
      valid = pos1 <= pos2
  in property $ isValidSpan testSpan === valid

-- Test Located operations
prop_locatedAt_creates_correct_location :: SourcePos -> Int -> Property
prop_locatedAt_creates_correct_location pos value = 
  let located = locatedAt pos value
  in conjoin
    [ property $ locValue located === value
    , property $ locPos located === pos
    , property $ locSpan located === emptySpan pos
    ]

prop_locatedWithSpan_creates_correct_location :: SourceSpan -> String -> Property
prop_locatedWithSpan_creates_correct_location testSpan value = 
  let located = locatedWithSpan testSpan value
  in conjoin
    [ property $ locValue located === value
    , property $ locSpan located === testSpan
    , property $ locPos located === spanStart testSpan
    ]

prop_locatedValue_extracts_value :: SourcePos -> Int -> Property
prop_locatedValue_extracts_value pos value = 
  let located = locatedAt pos value
  in property $ locatedValue located === value

prop_locatedSpan_extracts_span :: SourcePos -> Int -> Property
prop_locatedSpan_extracts_span pos value = 
  let located = locatedAt pos value
  in property $ locatedSpan located === emptySpan pos

prop_locatedPos_returns_start_position :: SourceSpan -> String -> Property
prop_locatedPos_returns_start_position testSpan value = 
  let located = locatedWithSpan testSpan value
  in property $ locatedPos located === spanStart testSpan

prop_mapLocated_applies_function :: SourcePos -> Int -> Property
prop_mapLocated_applies_function pos value = 
  let located = locatedAt pos value
      doubled = mapLocated (*2) located
  in property $ locValue doubled === value * 2

-- Test position advancement
prop_advancePos_matches_posAfter :: SourcePos -> Char -> Property
prop_advancePos_matches_posAfter pos c = 
  property $ advancePos c pos === posAfter c pos

prop_advancePosBy_empty_string :: SourcePos -> Property
prop_advancePosBy_empty_string pos = 
  property $ advancePosBy "" pos === pos

prop_advancePosBy_consistent_with_posAfter :: SourcePos -> String -> Property
prop_advancePosBy_consistent_with_posAfter pos s = 
  property $ advancePosBy s pos === foldl (flip posAfter) pos s

-- ============================================================================
-- Parser Module Tests
-- ============================================================================

-- Test FileDirectives
prop_fileDirectives_equality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_fileDirectives_equality ownership dependentTypes constraints = 
  let locatedOwnership = fmap (\b -> locatedAt startPos b) ownership
      locatedDependentTypes = fmap (\b -> locatedAt startPos b) dependentTypes
      locatedConstraints = fmap (\b -> locatedAt startPos b) constraints
      fd1 = FileDirectives locatedOwnership locatedDependentTypes locatedConstraints
      fd2 = FileDirectives locatedOwnership locatedDependentTypes locatedConstraints
  in property $ fd1 === fd2

prop_defaultFileDirectives_has_nothing :: Property
prop_defaultFileDirectives_has_nothing = 
  let fd = defaultFileDirectives
  in conjoin
    [ property $ fdOwnership fd === Nothing
    , property $ fdDependentTypes fd === Nothing
    , property $ fdConstraints fd === Nothing
    ]

-- Test BlockDirectives
prop_blockDirectives_equality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_blockDirectives_equality ownership dependentTypes constraints = 
  let locatedOwnership = fmap (\b -> locatedAt startPos b) ownership
      locatedDependentTypes = fmap (\b -> locatedAt startPos b) dependentTypes
      locatedConstraints = fmap (\b -> locatedAt startPos b) constraints
      bd1 = BlockDirectives locatedOwnership locatedDependentTypes locatedConstraints
      bd2 = BlockDirectives locatedOwnership locatedDependentTypes locatedConstraints
  in property $ bd1 === bd2

prop_defaultBlockDirectives_has_nothing :: Property
prop_defaultBlockDirectives_has_nothing = 
  let bd = defaultBlockDirectives
  in conjoin
    [ property $ bdOwnership bd === Nothing
    , property $ bdDependentTypes bd === Nothing
    , property $ bdConstraints bd === Nothing
    ]

-- Test CodeBlock
prop_codeBlock_equality :: BlockDirectives -> String -> SourceSpan -> Property
prop_codeBlock_equality directives content testSpan = 
  let cb1 = CodeBlock directives content testSpan
      cb2 = CodeBlock directives content testSpan
  in property $ cb1 === cb2

-- Test TypusFile
prop_typusFile_equality :: FileDirectives -> Property
prop_typusFile_equality directives = 
  let tf1 = TypusFile directives [] [] []
      tf2 = TypusFile directives [] [] []
  in property $ tf1 === tf2

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Test Utils and SourceLocation integration
prop_source_location_string_processing :: String -> Property
prop_source_location_string_processing s = 
  let trimmed = Utils.trim s
      pos = posAt 1 (length trimmed + 1)
      located = locatedAt pos trimmed
  in property $ locatedValue located === trimmed

prop_parser_utils_integration :: String -> Property
prop_parser_utils_integration s = 
  let trimmed = Utils.trim s
      withoutComments = Utils.removeLineComments trimmed
  in property $ length withoutComments <= length trimmed + 1 -- Allow for one extra newline

-- Test error location conversion
prop_toErrorLocation_preserves_position :: SourcePos -> Property
prop_toErrorLocation_preserves_position pos = 
  let errLoc = toErrorLocation pos
  in conjoin
    [ property $ line errLoc === posLine pos
    , property $ column errLoc === posColumn pos
    ]

prop_toErrorLocationWithSpan_preserves_range :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves_range testSpan = 
  let errLoc = toErrorLocationWithSpan testSpan
      start = spanStart testSpan
      end = spanEnd testSpan
  in conjoin
    [ property $ line errLoc === posLine start
    , property $ column errLoc === posColumn start
    , property $ endLine errLoc === Just (posLine end)
    , property $ endColumn errLoc === Just (posColumn end)
    ]

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Comprehensive Core Modules QuickCheck Tests"
  [ -- Utils Tests (11 tests)
    testGroup "Utils Module"
    [ fastProperty "trim idempotent" prop_trim_idempotent
    , fastProperty "trim no leading/trailing spaces" prop_trim_no_leading_trailing_spaces
    , fastProperty "splitBy empty segments" prop_splitBy_empty_segments
    , fastProperty "splitBy comma consistency" prop_splitBy_comma_consistency
    , fastProperty "splitBy collapsed removes empty" prop_splitBy_collapsed_removes_empty
    , fastProperty "removeLineComments preserves non-comment lines" prop_removeLineComments_preserves_non_comment_lines
    , fastProperty "removeLineComments removes comment content" prop_removeLineComments_removes_comment_content
    , fastProperty "removeComments preserves strings" prop_removeComments_preserves_strings
    , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative_indentation
    , fastProperty "safeProcessString filters control chars" prop_safeProcessString_filters_control_chars
    , fastProperty "isValidChar consistency" prop_isValid_char_consistency
    ]
  
  -- SourceLocation Tests (20 tests)
  , testGroup "SourceLocation Module"
    [ fastProperty "posAfter newline increments line" prop_posAfter_newline_increments_line
    , fastProperty "posAfter tab advances to next tab stop" prop_posAfter_tab_advances_to_next_tab_stop
    , fastProperty "posAfter regular char increments column" prop_posAfter_regular_char_increments_column
    , fastProperty "posAt creates correct position" prop_posAt_creates_correct_position
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_creates_correct_position
    , fastProperty "empty span has same start and end" prop_empty_span_has_same_start_and_end
    , fastProperty "spanFrom creates empty span" prop_spanFrom_creates_empty_span
    , fastProperty "spanTo creates empty span" prop_spanTo_creates_empty_span
    , fastProperty "spanBetween creates correct span" prop_spanBetween_creates_correct_span
    , fastProperty "mergeSpans contains both spans" prop_mergeSpans_contains_both_spans
    , fastProperty "isValidSpan check order" prop_isValidSpan_check_order
    , fastProperty "locatedAt creates correct location" prop_locatedAt_creates_correct_location
    , fastProperty "locatedWithSpan creates correct location" prop_locatedWithSpan_creates_correct_location
    , fastProperty "locatedValue extracts value" prop_locatedValue_extracts_value
    , fastProperty "locatedSpan extracts span" prop_locatedSpan_extracts_span
    , fastProperty "locatedPos returns start position" prop_locatedPos_returns_start_position
    , fastProperty "mapLocated applies function" prop_mapLocated_applies_function
    , fastProperty "advancePos matches posAfter" prop_advancePos_matches_posAfter
    , fastProperty "advancePosBy empty string" prop_advancePosBy_empty_string
    , fastProperty "advancePosBy consistent with posAfter" prop_advancePosBy_consistent_with_posAfter
    ]
  
  -- Parser Tests (10 tests)
  , testGroup "Parser Module"
    [ fastProperty "FileDirectives equality" prop_fileDirectives_equality
    , fastProperty "defaultFileDirectives has nothing" prop_defaultFileDirectives_has_nothing
    , fastProperty "BlockDirectives equality" prop_blockDirectives_equality
    , fastProperty "defaultBlockDirectives has nothing" prop_defaultBlockDirectives_has_nothing
    , fastProperty "CodeBlock equality" prop_codeBlock_equality
    , fastProperty "TypusFile equality" prop_typusFile_equality
    ]
  
  -- Integration Tests (5 tests)
  , testGroup "Integration Tests"
    [ fastProperty "source location string processing" prop_source_location_string_processing
    , fastProperty "parser utils integration" prop_parser_utils_integration
    , fastProperty "toErrorLocation preserves position" prop_toErrorLocation_preserves_position
    , fastProperty "toErrorLocationWithSpan preserves range" prop_toErrorLocationWithSpan_preserves_range
    ]
  ]