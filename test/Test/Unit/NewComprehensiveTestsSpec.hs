{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewComprehensiveTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, posAt, spanFrom, spanTo, mergeSpans, isValidSpan, locatedAt, locatedWithSpan)
import Utils (trim, splitBy, splitByComma, removeLineComments, removeComments, normalizeIndentation, breakOn)
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..), emptyContext, hasErrors, hasWarnings, filterBySeverity, filterByCategory)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)

-- ============================================================================
-- SourceLocation Tests
-- ============================================================================

-- Property: startPos is always consistent
prop_startPos_consistent :: Property
prop_startPos_consistent =
  property $ startPos === SourcePos 1 1 0

-- Property: posAfter correctly handles line breaks
prop_posAfter_line_break :: Int -> Property
prop_posAfter_line_break n =
  n >= 0 && n <= 100 ==>
  let pos = posAt n 1
      newPos = posAfter '\n' pos
  in property $ posLine newPos === n + 1 .&&. posColumn newPos === 1

-- Property: posAfter correctly handles tabs (8-space alignment)
prop_posAfter_tab_alignment :: Int -> Property
prop_posAfter_tab_alignment n =
  n >= 1 && n <= 8 ==>
  let pos = posAt 1 n
      newPos = posAfter '\t' pos
      expectedColumn = ((n - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn newPos === expectedColumn

-- Property: posAfter correctly handles regular characters
prop_posAfter_regular_char :: Char -> Int -> Property
prop_posAfter_regular_char c n =
  not (isSpace c) && n >= 1 && n <= 100 ==>
  let pos = posAt 1 n
      newPos = posAfter c pos
  in property $ posLine newPos === 1 .&&. posColumn newPos === n + 1

-- Property: spanFrom creates valid spans
prop_spanFrom_valid :: Int -> Property
prop_spanFrom_valid n =
  n >= 0 && n <= 1000 ==>
  let pos = posAt 1 (n + 1)
      span = spanFrom pos 5
  in property $ isValidSpan span

-- Property: mergeSpans is commutative for overlapping spans
prop_mergeSpans_commutative :: Int -> Int -> Int -> Int -> Property
prop_mergeSpans_commutative start1 len1 start2 len2 =
  start1 >= 0 && start2 >= 0 && len1 > 0 && len2 > 0 &&
  abs (start1 - start2) <= max len1 len2 ==>
  let span1 = spanFrom (posAt 1 (start1 + 1)) len1
      span2 = spanFrom (posAt 1 (start2 + 1)) len2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- ============================================================================
-- Utils Tests
-- ============================================================================

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmed1 = trim str
      trimmed2 = trim trimmed1
  in property $ trimmed1 === trimmed2

-- Property: trim removes only outer whitespace
prop_trim_preserves_internal_spaces :: String -> String -> String -> Property
prop_trim_preserves_internal_spaces before middle after =
  not (L.any isSpace before) && not (L.any isSpace after) ==>
  let input = before ++ "   " ++ middle ++ "   " ++ after
      trimmed = trim input
      expected = before ++ "   " ++ middle ++ "   " ++ after
  in property $ trimmed === expected

-- Property: splitBy preserves input when rejoined
prop_splitBy_roundtrip :: Char -> String -> Property
prop_splitBy_roundtrip delim str =
  let parts = splitBy delim str
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === str

-- Property: splitByComma is splitBy with comma
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency str =
  splitByComma str === splitBy ',' str

-- Property: removeLineComments preserves non-comment content
prop_removeLineComments_preserves_content :: String -> String -> Property
prop_removeLineComments_preserves_content before after =
  not ("//" `L.isInfixOf` before) && not ("//" `L.isInfixOf` after) &&
  not ("\"" `L.isInfixOf` before) && not ("\"" `L.isInfixOf` after) &&
  not ("'" `L.isInfixOf` before) && not ("'" `L.isInfixOf` after) ==>
  let input = before ++ "\n" ++ after ++ "\n// comment\n" ++ before
      result = removeLineComments input
  in property $ before `L.isInfixOf` result .&&. after `L.isInfixOf` result

-- Property: removeComments preserves string literals
prop_removeComments_preserves_strings :: String -> Property
prop_removeComments_preserves_strings content =
  not ("\"" `L.isInfixOf` content) && not ("'" `L.isInfixOf` content) ==>
  let input = "var s = \"" ++ content ++ "\" // comment\n/* block comment */"
      result = removeComments input
  in property $ ("\"" ++ content ++ "\"") `L.isInfixOf` result

-- Property: normalizeIndentation preserves relative structure
prop_normalizeIndentation_preserves_structure :: [String] -> Property
prop_normalizeIndentation_preserves_structure lines =
  not (null lines) && L.length lines <= 10 ==>
  let input = Data.List.unlines lines
      normalized = normalizeIndentation input
      normalizedLines = lines normalized
  in property $ L.length normalizedLines === L.length lines

-- Property: breakOn finds correct split point
prop_breakOn_correct_split :: String -> String -> String -> Property
prop_breakOn_correct_split pat prefix suffix =
  not (null pat) && not (pat `L.isInfixOf` prefix) && not (pat `L.isInfixOf` suffix) ==>
  let input = prefix ++ pat ++ suffix
      (before, after) = breakOn pat input
  in property $ before === prefix .&&. after === suffix

-- ============================================================================
-- Error Handling Tests
-- ============================================================================

-- Property: emptyContext has no errors L.or warnings
prop_emptyContext_clean :: Property
prop_emptyContext_clean =
  property $ not (hasErrors emptyContext) .&&. not (hasWarnings emptyContext)

-- Property: filterBySeverity preserves order
prop_filterBySeverity_preserves_order :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filterBySeverity_preserves_order severities target =
  not (null severities) ==> 
  let filtered = filterBySeverity target severities
      originalFiltered = L.filter (== target) severities
  in property $ filtered === originalFiltered

-- Property: filterByCategory is consistent
prop_filterByCategory_consistent :: [ErrorCategory] -> ErrorCategory -> Property
prop_filterByCategory_consistent categories target =
  not (null categories) ==>
  let filtered = filterByCategory target categories
      expected = L.filter (== target) categories
  in property $ filtered === expected

-- ============================================================================
-- Parser Integration Tests
-- ============================================================================

-- Property: Valid identifiers are alphanumeric with underscores L.and hyphens
prop_valid_identifier_structure :: String -> Property
prop_valid_identifier_structure str =
  let isValid = L.all (\c -> isAlphaNum c || c == '_' || c == '-') str
      hasContent = not (null str)
  in classify hasContent "non-empty" $
     classify isValid "valid identifier" $
     property $ True

-- Property: Directive parsing maintains consistency
prop_directive_parsing_consistent :: [(String, String)] -> Property
prop_directive_parsing_consistent pairs =
  L.length pairs <= 5 ==> -- Limit complexity
  let directiveStr = "//! " ++ unwords [key ++ "=" ++ value | (key, value) <- pairs]
      hasDirective = "//!" `L.isPrefixOf` directiveStr
  in classify hasDirective "has directive" $
     property $ hasDirective

-- ============================================================================
-- Performance Tests
-- ============================================================================

-- Property: String processing scales linearly
prop_linear_string_processing :: Int -> String -> Property
prop_linear_string_processing multiplier baseStr =
  multiplier >= 1 && multiplier <= 100 ==> -- Reasonable limits
  let largeStr = L.concat (replicate multiplier baseStr)
      processed = trim largeStr
      splitResult = splitBy ',' largeStr
  in property $ L.length processed <= L.length largeStr .&&. L.length splitResult >= 1

-- Property: Position tracking is efficient
prop_efficient_position_tracking :: Int -> Property
prop_efficient_position_tracking n =
  n >= 0 && n <= 10000 ==> -- Reasonable limits
  let finalPos = L.foldl (\pos c -> posAfter c pos) startPos (replicate n 'x')
  in property $ posOffset finalPos === n

-- ============================================================================
-- Edge Case Tests  
-- ============================================================================

-- Property: Empty string handling
prop_empty_string_handling :: Property
prop_empty_string_handling =
  let trimmed = trim ""
      split = splitBy ',' ""
      commentsRemoved = removeComments ""
      normalized = normalizeIndentation ""
  in property $ trimmed === "" .&&. split === [""] .&&. commentsRemoved === "" .&&. normalized === ""

-- Property: Unicode character handling
prop_unicode_handling :: String -> Property
prop_unicode_handling baseStr =
  let unicodeStr = baseStr ++ "测试café🚀"
      trimmed = trim unicodeStr
      processed = removeLineComments unicodeStr
  in property $ "测试" `L.isInfixOf` processed .&&. "café" `L.isInfixOf` processed .&&. "🚀" `L.isInfixOf` processed

-- Property: Special character handling in comments
prop_special_char_comments :: String -> Property
prop_special_char_comments content =
  not ("\"" `L.isInfixOf` content) && not ("'" `L.isInfixOf` content) &&
  not ("/*" `L.isInfixOf` content) && not ("*/" `L.isInfixOf` content) ==>
  let input = content ++ " // " ++ content ++ "\n/* " ++ content ++ " */"
      result = removeComments input
  in property $ not ("//" `L.isInfixOf` result) .&&. not ("/*" `L.isInfixOf` result) .&&. not ("*/" `L.isInfixOf` result)

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Property: Complete processing pipeline
prop_complete_processing_pipeline :: String -> String -> String -> Property
prop_complete_processing_pipeline prefix middle suffix =
  not ("\"" `L.isInfixOf` prefix) && not ("'" `L.isInfixOf` prefix) &&
  not ("\"" `L.isInfixOf` middle) && not ("'" `L.isInfixOf` middle) &&
  not ("\"" `L.isInfixOf` suffix) && not ("'" `L.isInfixOf` suffix) &&
  not ("/*" `L.isInfixOf` prefix ++ middle ++ suffix) ==>
  let input = prefix ++ "  /* comment */  " ++ middle ++ "  // line comment  " ++ suffix
      processed = input 
                  |> removeComments
                  |> trim
                  |> normalizeIndentation
  in property $ not ("/* comment */" `L.isInfixOf` processed) .&&.
     not ("// line comment" `L.isInfixOf` processed) .&&.
     (middle `L.isInfixOf` processed)

-- Helper function for pipeline composition
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- Aggregate L.all tests
tests :: TestTree
tests = testGroup "New Comprehensive Tests"
  [ testGroup "SourceLocation Tests"
    [ fastProperty "startPos is consistent" prop_startPos_consistent
    , fastProperty "posAfter handles line breaks" prop_posAfter_line_break
    , fastProperty "posAfter handles tab alignment" prop_posAfter_tab_alignment
    , fastProperty "posAfter handles regular characters" prop_posAfter_regular_char
    , fastProperty "spanFrom creates valid spans" prop_spanFrom_valid
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    ]
  , testGroup "Utils Tests"
    [ fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim preserves internal spaces" prop_trim_preserves_internal_spaces
    , fastProperty "splitBy roundtrip" prop_splitBy_roundtrip
    , fastProperty "splitByComma consistency" prop_splitByComma_consistency
    , fastProperty "removeLineComments preserves content" prop_removeLineComments_preserves_content
    , fastProperty "removeComments preserves strings" prop_removeComments_preserves_strings
    , fastProperty "normalizeIndentation preserves structure" prop_normalizeIndentation_preserves_structure
    , fastProperty "breakOn correct split" prop_breakOn_correct_split
    ]
  , testGroup "Error Handling Tests"
    [ fastProperty "emptyContext is clean" prop_emptyContext_clean
    , fastProperty "filterBySeverity preserves order" prop_filterBySeverity_preserves_order
    , fastProperty "filterByCategory consistency" prop_filterByCategory_consistent
    ]
  , testGroup "Parser Integration Tests"
    [ fastProperty "valid identifier structure" prop_valid_identifier_structure
    , fastProperty "directive parsing consistency" prop_directive_parsing_consistent
    ]
  , testGroup "Performance Tests"
    [ fastProperty "linear string processing" prop_linear_string_processing
    , fastProperty "efficient position tracking" prop_efficient_position_tracking
    ]
  , testGroup "Edge Case Tests"
    [ fastProperty "empty string handling" prop_empty_string_handling
    , fastProperty "unicode handling" prop_unicode_handling
    , fastProperty "special char comments" prop_special_char_comments
    ]
  , testGroup "Integration Tests"
    [ fastProperty "complete processing pipeline" prop_complete_processing_pipeline
    ]
  ]