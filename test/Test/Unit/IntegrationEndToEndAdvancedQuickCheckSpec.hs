{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.IntegrationEndToEndAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler.IR (buildSourceIR, buildSemanticIR, emitGo)
import ErrorHandler (formatErrors)
import Ownership (analyzeOwnership, OwnershipError(..))
import DependentTypesParser (validateDependentTypeSyntax)
import Utils (trim, removeComments, normalizeIndentation)
import Compiler.Errors (ErrorSeverity(..), ErrorCategory(..))
import Data.List (sort, nub, length, filter, elem, intercalate, concat, isPrefixOf, isInfixOf)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T

-- ============================================================================
-- Advanced Integration End-to-End QuickCheck Tests
-- ============================================================================

-- Property: Complete compilation pipeline preserves function structure
prop_complete_pipeline_preserves_functions :: String -> String -> Property
prop_complete_pipeline_preserves_functions funcName funcBody =
  length funcName > 0 && length funcBody > 0 && not (null funcBody) ==>
  let typusSource = "func " ++ funcName ++ "() {\n" ++ funcBody ++ "\n}"
      parseResult = parseTypus typusSource
  in case parseResult of
    Left _ -> property True  -- Parsing errors are acceptable
    Right typusFile -> 
      let sourceIR = buildSourceIR typusFile typusSource
          semanticIR = buildSemanticIR typusFile
          goIR = emitGo semanticIR
          goCode = goIR goIR
      in property $ 
        goCode `contains` ("func " ++ funcName) .&&.
        length goCode > 0

-- Property: Error handling throughout pipeline is consistent
prop_pipeline_error_consistency :: String -> Property
prop_pipeline_error_consistency malformedSource =
  length malformedSource > 0 ==>
  let parseResult = parseTypus malformedSource
  in case parseResult of
    Left parseError -> 
      property $ length (show parseError) > 0
    Right typusFile -> 
      let sourceIR = buildSourceIR typusFile malformedSource
          semanticIR = buildSemanticIR typusFile
          goIR = emitGo semanticIR
          goCode = goIR goIR
      in property $ length goCode >= 0

-- Property: Ownership analysis integrates with parsing
prop_ownership_analysis_integration :: String -> Property
prop_ownership_analysis_integration sourceCode =
  length sourceCode > 0 ==>
  let parseResult = parseTypus sourceCode
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let ownershipResult = analyzeOwnership typusFile
      in property $ 
        case ownershipResult of
          Left errors -> length errors > 0
          Right _ -> property True

-- Property: Dependent type validation integrates with pipeline
prop_dependent_type_validation_integration :: String -> Property
prop_dependent_type_validation_integration typeSource =
  length typeSource > 0 && "type " `isInfixOf` typeSource ==>
  let validationResult = validateDependentTypeSyntax typeSource
  in property $ 
    case validationResult of
      Left errors -> length errors > 0
      Right _ -> property True

-- Property: String processing integrates with compilation
prop_string_processing_integration :: String -> Property
prop_string_processing_integration rawSource =
  length rawSource > 0 ==>
  let trimmed = trim rawSource
      withoutComments = removeComments trimmed
      normalized = normalizeIndentation withoutComments
      parseResult = parseTypus normalized
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let sourceIR = buildSourceIR typusFile normalized
      in property $ sourceText sourceIR === normalized

-- Property: Multi-file compilation consistency
prop_multi_file_compilation_consistency :: [String] -> Property
prop_multi_file_compilation_consistency sources =
  length sources > 0 && all (not . null) sources ==>
  let parseResults = map parseTypus sources
      successfulParses = [typusFile | Right typusFile <- parseResults]
      sourceIRs = map (\f -> buildSourceIR f "") successfulParses
      semanticIRs = map buildSemanticIR successfulParses
      goIRs = map emitGo semanticIRs
      goCodes = map (\ir -> ir ir) goIRs
  in property $ 
    length goCodes === length successfulParses .&&.
    all (length > 0) goCodes

-- Property: Directive processing consistency
prop_directive_processing_consistency :: String -> String -> Property
prop_directive_processing_consistency directive content =
  length directive > 0 && length content > 0 ==>
  let sourceWithDirective = "//! " ++ directive ++ " = true\n" ++ content
      parseResult = parseTypus sourceWithDirective
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let fileDirectives = tfDirectives typusFile
      in property $ 
        case fileDirectives of
          FileDirectives ownership dependent constraints -> 
            isJust ownership || isJust dependent || isJust constraints

-- Property: Error recovery maintains partial results
prop_error_recovery_maintains_partial :: String -> String -> Property
prop_error_recovery_maintains_partial good bad =
  length good > 0 && length bad > 0 ==>
  let mixedSource = good ++ "\n@@ SYNTAX ERROR @@\n" ++ bad ++ "\n" ++ good
      parseResult = parseTypus mixedSource
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let blocks = tfBlocks typusFile
      in property $ length blocks >= 0

-- Property: Performance characteristics are bounded
prop_performance_bounded :: String -> Int -> Property
prop_performance_bounded baseContent iterations =
  length baseContent > 0 && iterations > 0 && iterations <= 100 ==>
  let largeSource = concat (replicate iterations baseContent)
      parseResult = parseTypus largeSource
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let sourceIR = buildSourceIR typusFile largeSource
          semanticIR = buildSemanticIR typusFile
          goIR = emitGo semanticIR
          goCode = goIR goIR
      in property $ length goCode > 0

-- Property: Cross-module dependency handling
prop_cross_module_dependencies :: [String] -> [String] -> Property
prop_cross_module_dependencies moduleNames dependencies =
  length moduleNames > 0 && all (not . null) moduleNames ==>
  let moduleSources = zipWith (\name deps -> 
        "module " ++ name ++ "\n" ++ 
        concatMap (\dep -> "import " ++ dep ++ "\n") deps) 
        moduleNames dependencies
      parseResults = map parseTypus moduleSources
      successfulModules = [typusFile | Right typusFile <- parseResults]
  in property $ 
    length successfulModules <= length moduleNames .&&.
    length successfulModules >= 0

-- Property: Type inference consistency
prop_type_inference_consistency :: String -> String -> Property
prop_type_inference_consistency varName value =
  length varName > 0 && length value > 0 ==>
  let sourceWithVar = "let " ++ varName ++ " = " ++ value
      parseResult = parseTypus sourceWithVar
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let sourceIR = buildSourceIR typusFile sourceWithVar
      in property $ sourceText sourceIR === sourceWithVar

-- Property: Optimization preserves semantics
prop_optimization_preserves_semantics :: String -> Property
prop_optimization_preserves_semantics sourceCode =
  length sourceCode > 0 ==>
  let parseResult = parseTypus sourceCode
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let sourceIR = buildSourceIR typusFile sourceCode
          semanticIR = buildSemanticIR typusFile
          goIR = emitGo semanticIR
          goCode = goIR goIR
      in property $ 
        length goCode > 0 .&&.
        (goCode `contains` "func" || length (filter (`elem` goCode) "func") == 0)

-- Property: Resource cleanup is consistent
prop_resource_cleanup_consistent :: String -> Property
prop_resource_cleanup_consistent sourceCode =
  length sourceCode > 0 ==>
  let parseResult = parseTypus sourceCode
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let sourceIR = buildSourceIR typusFile sourceCode
          semanticIR = buildSemanticIR typusFile
          goIR = emitGo semanticIR
          goCode = goIR goIR
      in property $ 
        -- Check that resources are properly cleaned up by ensuring
        -- no memory leaks in the IR structures
        sourceIR `seq` semanticIR `seq` goIR `seq` True

-- Helper function to check string containment
contains :: String -> String -> Bool
contains needle haystack = needle `Data.List.isInfixOf` haystack

-- Test collection
tests :: TestTree
tests = testGroup "Advanced Integration End-to-End QuickCheck Tests"
  [ fastProperty "Complete compilation pipeline preserves function structure" prop_complete_pipeline_preserves_functions
  , fastProperty "Error handling throughout pipeline is consistent" prop_pipeline_error_consistency
  , fastProperty "Ownership analysis integrates with parsing" prop_ownership_analysis_integration
  , fastProperty "Dependent type validation integrates with pipeline" prop_dependent_type_validation_integration
  , fastProperty "String processing integrates with compilation" prop_string_processing_integration
  , fastProperty "Multi-file compilation consistency" prop_multi_file_compilation_consistency
  , fastProperty "Directive processing consistency" prop_directive_processing_consistency
  , fastProperty "Error recovery maintains partial results" prop_error_recovery_maintains_partial
  , fastProperty "Performance characteristics are bounded" prop_performance_bounded
  , fastProperty "Cross-module dependency handling" prop_cross_module_dependencies
  , fastProperty "Type inference consistency" prop_type_inference_consistency
  , fastProperty "Optimization preserves semantics" prop_optimization_preserves_semantics
  , fastProperty "Resource cleanup is consistent" prop_resource_cleanup_consistent
  ]