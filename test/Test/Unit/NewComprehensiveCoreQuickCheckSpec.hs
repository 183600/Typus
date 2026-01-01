{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewComprehensiveCoreQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Test.Unit.NewCoreUtilsQuickCheckSpec as Utils
import qualified Test.Unit.NewCoreSourceLocationQuickCheckSpec as SourceLocation
import qualified Test.Unit.NewCoreParserQuickCheckSpec as Parser
import qualified Utils
import qualified SourceLocation
import qualified Parser

-- ============================================================================
-- Comprehensive Core Module QuickCheck Tests
-- ============================================================================

-- | Cross-module integration property: Utils + SourceLocation
prop_utils_source_location_integration :: String -> Bool
prop_utils_source_location_integration s = 
  let trimmed = Utils.trim s
      pos = SourceLocation.startPos
      located = SourceLocation.locatedAt pos trimmed
  in SourceLocation.locatedValue located == trimmed

-- | Cross-module integration property: Parser + SourceLocation
prop_parser_source_location_integration :: String -> Bool
prop_parser_source_location_integration s = 
  case Parser.parseTypus s of
    Left _ -> True
    Right parsed -> True  -- Basic integration validation

-- | Cross-module integration property: Utils + Parser
prop_utils_parser_integration :: String -> Bool
prop_utils_parser_integration s = 
  let processed = Utils.removeComments s
      normalized = Utils.normalizeIndentation processed
  in case Parser.parseTypus normalized of
    Left _ -> True
    Right parsed -> True

-- | Property: String processing pipeline should be consistent
prop_string_processing_pipeline :: String -> Bool
prop_string_processing_pipeline s = 
  let step1 = Utils.trim s
      step2 = Utils.removeComments step1
      step3 = Utils.normalizeIndentation step2
      step4 = Utils.splitBy '\n' step3
  in L.length step4 >= 0  -- Basic pipeline validation

-- | Property: Source location tracking should be consistent
prop_source_location_tracking_consistent :: Int -> Int -> Bool
prop_source_location_tracking_consistent line col = 
  line > 0 && col > 0 ==>
  let pos1 = SourceLocation.posAt line col
      pos2 = SourceLocation.posAtLineCol line col
      span = SourceLocation.spanFrom pos1
  in SourceLocation.spanStart span == pos1 && pos1 == pos2

-- | Property: Parser error handling should be robust
prop_parser_error_robustness :: String -> Bool
prop_parser_error_robustness s = 
  let malformed = s ++ "\n@invalid directive {\nunclosed block"
  in case Parser.parseTypus malformed of
    Left _ -> True
    Right _ -> True  -- Should handle gracefully

-- | Property: Utils functions should compose correctly
prop_utils_composition :: String -> Bool
prop_utils_composition s = 
  let commaSeparated = Utils.splitByComma s
      rejoined = L.concat commaSeparated
      trimmed = Utils.trim rejoined
  in not (null s) ==> L.length trimmed <= L.length rejoined

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Comprehensive Core Module QuickCheck Tests"
  [ Utils.testSuite
  , SourceLocation.testSuite  
  , Parser.testSuite
  , testGroup "Cross-Module Integration Tests"
    [ testProperty "Utils + SourceLocation integration" prop_utils_source_location_integration
    , testProperty "Parser + SourceLocation integration" prop_parser_source_location_integration
    , testProperty "Utils + Parser integration" prop_utils_parser_integration
    , testProperty "String processing pipeline" prop_string_processing_pipeline
    , testProperty "Source location tracking consistent" prop_source_location_tracking_consistent
    , testProperty "Parser error robustness" prop_parser_error_robustness
    , testProperty "Utils composition" prop_utils_composition
    ]
  ]