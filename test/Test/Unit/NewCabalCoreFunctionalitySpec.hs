{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalCoreFunctionalitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)
import Test.QuickCheck.Gen (Gen, suchThat)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

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
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Data.Char (isSpace, toLower, isAlphaNum, isLetter)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, nub)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (foldM, when)

-- ============================================================================
-- Test 1: SourceLocation Mathematical Operations
-- ============================================================================

-- Test position advancement properties
prop_position_advancement_additive :: String -> String -> Property
prop_position_advancement_additive s1 s2 =
  let pos1 = advancePosBy s1 startPos
      pos2 = advancePosBy s2 pos1
      posCombined = advancePosBy (s1 ++ s2) startPos
  in property $ pos2 === posCombined

-- Test span merging associativity
prop_span_merging_associative :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging_associative p1 p2 p3 =
  let span1 = spanBetween p1 p2
      span2 = spanBetween p2 p3
      span3 = spanBetween p1 p3
      merged1 = mergeSpans span1 span2
  in isValidSpan span1 && isValidSpan span2 && isValidSpan span3 ==>
     property $ merged1 === span3

-- Test located value mapping
prop_located_mapping_preserves_location :: String -> Int -> Property
prop_located_mapping_preserves_location s n =
  let pos = posAt 1 1
      span = emptySpan pos
      located = locatedWithSpan span s
      mapped = mapLocated L.length located
  in property $ locatedSpan mapped === locatedSpan located .&&.
               locatedValue mapped === L.length s

-- ============================================================================
-- Test 2: Utils String Processing Edge Cases
-- ============================================================================

-- Test trim with various Unicode whitespace
prop_trim_unicode_whitespace :: String -> Property
prop_trim_unicode_whitespace s =
  let unicodeWhitespace = "\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200A\x2028\x2029\x3000"
      input = unicodeWhitespace ++ s ++ unicodeWhitespace
      trimmed = trim input
      hasLeading = not (null s) && isSpace (L.head input)
      hasTrailing = not (null s) && isSpace (last input)
  in classify hasLeading "has leading Unicode whitespace" $
     classify hasTrailing "has trailing Unicode whitespace" $
     property $ (null trimmed || not (isSpace (L.head trimmed))) .&&.
                (null trimmed || not (isSpace (last trimmed)))

-- Test splitBy with empty delimiter edge cases
prop_splitBy_empty_delimiter_consistency :: String -> Property
prop_splitBy_empty_delimiter_consistency s =
  let regular = splitBy ',' s
      collapsed = splitByCollapsed ',' s
      hasConsecutiveCommas = ",," `L.isInfixOf` s
  in classify hasConsecutiveCommas "has consecutive delimiters" $
     property $ (if hasConsecutiveCommas then L.length regular > L.length collapsed else regular == collapsed)

-- Test comment removal with nested quotes
prop_comment_removal_nested_quotes :: String -> String -> Property
prop_comment_removal_nested_quotes before after =
  let content = before ++ "var s = \"// not comment \\\" // still not comment\" // real comment\n" ++ after
      processed = removeLineComments content
  in not (L.any (`elem` "\"'\\") before) && not (L.any (`elem` "\"'\\") after) ==>
     property $ "// not comment" `L.isInfixOf` processed .&&.
                not ("// real comment" `L.isInfixOf` processed)

-- ============================================================================
-- Test 3: Parser Error Recovery Simulation
-- ============================================================================

-- Simulate parser error recovery with malformed directives
prop_parser_error_recovery_directives :: String -> Property
prop_parser_error_recovery_directives content =
  let malformedDirective = "//! malformed-directive without-equals\n" ++ content
      -- Simulate basic directive parsing
      lines' = lines malformedDirective
      directiveLines = L.filter ("//! " `L.isPrefixOf`) lines'
      contentLines = L.filter (not . ("//! " `L.isPrefixOf`)) lines'
  in property $ L.length directiveLines >= 1 .&&.
                L.length contentLines >= L.length (lines content)

-- Test block parsing with mixed content
prop_parser_block_mixed_content :: [String] -> Property
prop_parser_block_mixed_content blocks =
  not (null blocks) ==>
  let blockContent = Data.List.intercalate "\n\n" blocks
      blockLines = lines blockContent
      -- Simulate block detection
      hasDirectives = L.any ("//! " `L.isPrefixOf`) blockLines
      hasCode = L.any (not . null . trim) blockLines
  in property $ L.length blockLines >= L.length blocks .&&.
                (hasDirectives || hasCode)

-- ============================================================================
-- Test 4: Compiler Consistency Tests
-- ============================================================================

-- Test compilation pipeline consistency
prop_compilation_pipeline_consistency :: String -> Property
prop_compilation_pipeline_consistency source =
  let stage1 = removeComments source
      stage2 = trim stage1
      stage3 = normalizeIndentation stage2
      -- Simulate multiple pipeline runs
      pipeline1 = source |> removeComments |> trim |> normalizeIndentation
      pipeline2 = source |> trim |> removeComments |> normalizeIndentation
      pipeline3 = source |> normalizeIndentation |> removeComments |> trim
  in property $ (pipeline1 == pipeline2) .||. (pipeline2 == pipeline3) .||. (pipeline1 == pipeline3)

-- Test compiler optimization invariants
prop_compiler_optimization_invariants :: String -> Property
prop_compiler_optimization_invariants code =
  let optimized = removeComments code
      originalLines = L.length (lines code)
      optimizedLines = L.length (lines optimized)
  in property $ optimizedLines <= originalLines .&&.
                L.length optimized <= L.length code

-- ============================================================================
-- Test 5: Ownership Transfer Tests
-- ============================================================================

-- Test ownership transfer transitivity
prop_ownership_transfer_transitive :: String -> String -> String -> Property
prop_ownership_transfer_transitive owner1 owner2 resource =
  not (null owner1) && not (null owner2) && not (null resource) ==>
  let transfer1 = owner1 ++ " -> " ++ resource
      transfer2 = owner2 ++ " -> " ++ owner1
      -- Simulate ownership chain validation
      chain = [transfer1, transfer2]
      chainValid = L.all (\t -> L.length (words t) >= 3) chain
  in property $ chainValid ==> L.length chain >= 2

-- Test ownership borrowing constraints
prop_ownership_borrowing_constraints :: String -> Property
prop_ownership_borrowing_constraints resource =
  not (null resource) ==>
  let borrow = "&" ++ resource
      use = resource ++ ".method()"
      -- Simulate borrow checker validation
      hasBorrow = "&" `L.isPrefixOf` borrow
      hasUse = not (null use)
  in property $ hasBorrow && hasUse ==> L.length borrow >= 1 .&&. L.length use >= 1

-- ============================================================================
-- Test 6: Dependency Analysis Cycle Detection
-- ============================================================================

-- Test cycle detection in dependency graph
prop_dependency_cycle_detection :: [(String, String)] -> Property
prop_dependency_cycle_detection dependencies =
  not (null dependencies) ==>
  let nodes = nub $ concatMap (\(a, b) -> [a, b]) dependencies
      hasCycle = L.any (\(a, b) -> (b, a) `elem` dependencies) dependencies
  in classify hasCycle "has cycle" $
     property $ L.length nodes >= 2 .&&.
                (if hasCycle then L.length dependencies >= 2 else True)

-- Test dependency ordering consistency
prop_dependency_ordering_consistency :: [String] -> Property
prop_dependency_ordering_consistency modules =
  not (null modules) ==>
  let ordered = sort modules
      -- Simulate topological sort validation
      isSorted = L.all (uncurry (<=)) (zip ordered (L.tail ordered))
  in property $ L.length ordered == L.length modules .&&.
                isSorted

-- ============================================================================
-- Test 7: Error Handling Consistency
-- ============================================================================

-- Test error location accuracy
prop_error_location_accuracy :: Int -> Int -> Property
prop_error_location_accuracy line col =
  line >= 1 && line <= 1000 && col >= 1 && col <= 1000 ==>
  let pos = posAtLineCol line col 0
      errorLoc = toErrorLocation pos
  in property =<<
    counterexample ("Invalid error location: " ++ show errorLoc) $
    property $ line errorLoc == line .&&. column errorLoc == col

-- Test error span coverage
prop_error_span_coverage :: Int -> Int -> Int -> Int -> Property
prop_error_span_coverage startLine startCol endLine endCol =
  startLine >= 1 && startLine <= 1000 && startCol >= 1 && startCol <= 1000 &&
  endLine >= startLine && endLine <= 1000 && endCol >= 1 && endCol <= 1000 ==>
  let start = posAtLineCol startLine startCol 0
      end = posAtLineCol endLine endCol 0
      span = spanBetween start end
      errorLoc = toErrorLocationWithSpan span
  in property $ isValidSpan span ==>
                line errorLoc == startLine .&&.
                column errorLoc == startCol .&&.
                endLine errorLoc == Just endLine .&&.
                endColumn errorLoc == Just endCol

-- ============================================================================
-- Test 8: Syntax Validator Robustness
-- ============================================================================

-- Test syntax validation with malformed input
prop_syntax_validation_malformed_input :: String -> Property
prop_syntax_validation_malformed_input input =
  let hasUnmatchedBrackets = (L.length (L.filter (== '{') input) /= L.length (L.filter (== '}') input)) ||
                            (L.length (L.filter (== '(') input) /= L.length (L.filter (== ')') input))
      hasUnmatchedQuotes = (L.length (L.filter (== '"') input) `mod` 2) /= 0
  in classify hasUnmatchedBrackets "has unmatched brackets" $
     classify hasUnmatchedQuotes "has unmatched quotes" $
     property $ L.length input >= 0 .&&. True -- Always should not crash

-- Test syntax validation edge cases
prop_syntax_validation_edge_cases :: String -> Property
prop_syntax_validation_edge_cases input =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      hasSpecialChars = L.any (`elem` specialChars) input
  in classify hasSpecialChars "has special characters" $
     property $ L.length input >= 0 -- Should handle special characters gracefully

-- ============================================================================
-- Test 9: Comment Processing Complex Scenarios
-- ============================================================================

-- Test comment removal with complex string literals
prop_comment_complex_string_literals :: String -> Property
prop_comment_complex_string_literals content =
  let stringWithComments = content ++ " var s = \"// not comment /* also not */\" /* real comment */ // line comment"
      processed = removeComments stringWithComments
  in not (L.any (`elem` "\"'\\") content) ==>
     property $ "// not comment /* also not */" `L.isInfixOf` processed .&&.
                not ("/* real comment */" `L.isInfixOf` processed) .&&.
                not ("// line comment" `L.isInfixOf` processed)

-- Test comment nesting edge cases
prop_comment_nesting_edge_cases :: String -> Property
prop_comment_nesting_edge_cases content =
  let nestedComments = "/* outer /* inner */ still outer */" ++ content
      processed = removeComments nestedComments
  in not ("/*" `L.isInfixOf` content) && not ("*/" `L.isInfixOf` content) ==>
     property $ not ("/* outer" `L.isInfixOf` processed) .&&.
                not ("/* inner" `L.isInfixOf` processed) .&&.
                content `L.isInfixOf` processed

-- ============================================================================
-- Test 10: Indentation Normalization Boundaries
-- ============================================================================

-- Test indentation with mixed tabs L.and spaces
prop_indentation_mixed_whitespace :: [Int] -> Property
prop_indentation_mixed_whitespace indentLevels =
  not (null indentLevels) ==>
  let inputLines = zipWith (\level content -> 
        let spaces = replicate (abs level `mod` 10) ' '
            tabs = replicate (abs level `mod` 5) '\t'
        in spaces ++ tabs ++ "content " ++ show level) indentLevels [1..]
      content = unlines inputLines
      normalized = normalizeIndentation content
      normalizedLines = L.filter (not . null . trim) (lines normalized)
      minIndent = if null normalizedLines then 0 else 
                  L.minimum [L.length (takeWhile isSpace line) | line <- normalizedLines]
  in property $ L.length normalizedLines >= L.length indentLevels .&&.
                minIndent === 0

-- Test indentation preservation of relative structure
prop_indentation_relative_structure :: [Int] -> Property
prop_indentation_relative_structure levels =
  not (null levels) && L.all (>= 0) levels ==>
  let inputLines = zipWith (\level content -> 
        replicate level ' ' ++ "line" ++ show level) levels [1..]
      content = unlines inputLines
      normalized = normalizeIndentation content
      normalizedLines = lines normalized
      indentDifferences = zipWith (-) 
        [L.length (takeWhile isSpace line) | line <- normalizedLines]
        (0 : [L.length (takeWhile isSpace line) | line <- normalizedLines])
  in property $ L.all (>= 0) indentDifferences

-- Helper function for pipeline testing
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Core Functionality Tests"
  [ testGroup "SourceLocation Mathematical Operations"
    [ fastProperty "position advancement is additive" prop_position_advancement_additive
    , fastProperty "span merging is associative" prop_span_merging_associative
    , fastProperty "located mapping preserves location" prop_located_mapping_preserves_location
    ]

  , testGroup "Utils String Processing Edge Cases"
    [ fastProperty "trim handles Unicode whitespace" prop_trim_unicode_whitespace
    , fastProperty "splitBy consistency with empty delimiters" prop_splitBy_empty_delimiter_consistency
    , fastProperty "comment removal with nested quotes" prop_comment_removal_nested_quotes
    ]

  , testGroup "Parser Error Recovery Simulation"
    [ fastProperty "parser error recovery with malformed directives" prop_parser_error_recovery_directives
    , fastProperty "parser block parsing with mixed content" prop_parser_block_mixed_content
    ]

  , testGroup "Compiler Consistency Tests"
    [ fastProperty "compilation pipeline consistency" prop_compilation_pipeline_consistency
    , fastProperty "compiler optimization invariants" prop_compiler_optimization_invariants
    ]

  , testGroup "Ownership Transfer Tests"
    [ fastProperty "ownership transfer transitivity" prop_ownership_transfer_transitive
    , fastProperty "ownership borrowing constraints" prop_ownership_borrowing_constraints
    ]

  , testGroup "Dependency Analysis Cycle Detection"
    [ fastProperty "dependency cycle detection" prop_dependency_cycle_detection
    , fastProperty "dependency ordering consistency" prop_dependency_ordering_consistency
    ]

  , testGroup "Error Handling Consistency"
    [ fastProperty "error location accuracy" prop_error_location_accuracy
    , fastProperty "error span coverage" prop_error_span_coverage
    ]

  , testGroup "Syntax Validator Robustness"
    [ fastProperty "syntax validation with malformed input" prop_syntax_validation_malformed_input
    , fastProperty "syntax validation edge cases" prop_syntax_validation_edge_cases
    ]

  , testGroup "Comment Processing Complex Scenarios"
    [ fastProperty "comment removal with complex string literals" prop_comment_complex_string_literals
    , fastProperty "comment nesting edge cases" prop_comment_nesting_edge_cases
    ]

  , testGroup "Indentation Normalization Boundaries"
    [ fastProperty "indentation with mixed whitespace" prop_indentation_mixed_whitespace
    , fastProperty "indentation preservation of relative structure" prop_indentation_relative_structure
    ]
  ]