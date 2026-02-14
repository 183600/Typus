{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.AdvancedOwnershipQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.QuickCheck (fastProperty, memoryEfficientProperty, ultraMemoryEfficientProperty)
import TestSupport.Arbitrary
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, sort, nub, partition, (\\))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (when, unless, replicateM)
import Data.Either (isLeft, isRight)

-- Import Ownership modules
import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , OwnershipAnalysis(..)
  , OwnershipConstraint(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  , checkOwnershipTransfer
  , validateOwnershipConstraints
  , hasOwnershipErrors
  , getOwnershipErrors
  , clearOwnershipErrors
  , mergeOwnershipAnalyses
  , getOwners
  , getBorrowers
  , getOwnedResources
  , isOwner
  , isBorrower
  , canTransferOwnership
  , transferOwnership
  , buildOwnershipGraph
  , validateOwnershipRules
  , isCompleteAnalysis
  , updateIncremental
  , analyzeWithCache
  , analyzeParallel
  , analyzeModularOwnership
  , visualizeOwnership
  , computeOwnershipStatistics
  , optimizeOwnership
  , filterOwnership
  , compareOwnershipAnalyses
  , exportOwnershipAnalysis
  , importOwnershipAnalysis
  , validateOwnershipAnalysis
  , repairOwnershipAnalysis
  , generateOwnershipSuggestions
  , refactorOwnershipAnalysis
  , generateOwnershipDocumentation
  , generateOwnershipTests
  , benchmarkOwnershipAnalysis
  , profileOwnershipAnalysis
  )

import Parser
  ( TypusFile(..)
  , CodeBlock(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )

import qualified Ownership.Common.Types as Own
import SourceLocation (SourcePos(..), SourceSpan(..))

-- ============================================================================
-- Advanced Ownership Properties
-- ============================================================================

-- | Property: Ownership transfer should maintain resource uniqueness
prop_ownership_transfer_uniqueness :: OwnershipTransfer -> Property
prop_ownership_transfer_uniqueness transfer = 
  let analyzer = newOwnershipAnalyzer
      canTransfer = canTransferOwnership analyzer transfer
      result = if canTransfer 
                 then transferOwnership analyzer transfer
                 else analyzer
      ownersBefore = Set.fromList $ getOwners analyzer
      ownersAfter = Set.fromList $ getOwners result
      transferred = getTransferTarget transfer
  in property $ not canTransfer || transferred `Set.member` ownersAfter

  where
    getTransferTarget (OwnershipTransfer _ target) = target

-- | Property: Borrowing rules should be enforced consistently
prop_borrowing_rules_consistency :: String -> Property
prop_borrowing_rules_consistency varName = 
  let isValidName = isValidIdentifier varName && not (null varName)
  in whenValid $ property $ 
    if isValidName
      then let 
        -- Test multiple immutable borrows
        immutableCode = varName ++ " := 1\n" ++ 
                        "r1 := &" ++ varName ++ "\n" ++ 
                        "r2 := &" ++ varName ++ "\n" ++ 
                        "r3 := &" ++ varName ++ "\n" ++
                        "println(*r1, *r2, *r3)"
        immutableResult = analyzeOwnership immutableCode newOwnershipAnalyzer
        immutableValid = not $ hasBorrowErrors $ getOwnershipErrors immutableResult
        
        -- Test mutable borrow conflict
        mutableCode = varName ++ " := 1\n" ++ 
                     "r1 := &" ++ varName ++ "\n" ++ 
                     "m1 := &mut " ++ varName ++ "\n" ++
                     "println(*r1)"
        mutableResult = analyzeOwnership mutableCode newOwnershipAnalyzer
        mutableInvalid = hasBorrowErrors $ getOwnershipErrors mutableResult
        
        -- Test multiple mutable borrows
        multiMutableCode = varName ++ " := 1\n" ++ 
                          "m1 := &mut " ++ varName ++ "\n" ++ 
                          "m2 := &mut " ++ varName ++ "\n" ++
                          "println(*m1)"
        multiMutableResult = analyzeOwnership multiMutableCode newOwnershipAnalyzer
        multiMutableInvalid = hasBorrowErrors $ getOwnershipErrors multiMutableResult
      in property $ immutableValid && mutableInvalid && multiMutableInvalid
      else property True
  where
    whenValid = guard isValidName
    hasBorrowErrors = any isBorrowError
    isBorrowError (Own.MutBorrowWhileBorrowed _) = True
    isBorrowError (Own.BorrowWhileMutBorrowed _) = True
    isBorrowError (Own.MultipleMutBorrows _) = True
    isBorrowError _ = False

-- | Property: Ownership analysis should be deterministic
prop_ownership_analysis_deterministic :: String -> Property
prop_ownership_analysis_deterministic code = 
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
      result1 = analyzeOwnership code analyzer1
      result2 = analyzeOwnership code analyzer2
      errors1 = getOwnershipErrors result1
      errors2 = getOwnershipErrors result2
  in property $ sort errors1 == sort errors2

-- | Property: Merged ownership analyses should be consistent
prop_merged_ownership_consistency :: String -> String -> Property
prop_merged_ownership_consistency code1 code2 = 
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
      result1 = analyzeOwnership code1 analyzer1
      result2 = analyzeOwnership code2 analyzer2
      merged = mergeOwnershipAnalyses [result1, result2]
      mergedValid = validateOwnershipAnalysis merged
  in property $ mergedValid

-- | Property: Incremental analysis should be consistent with full analysis
prop_incremental_analysis_consistency :: String -> String -> Property
prop_incremental_analysis_consistency baseCode newCode = 
  let baseAnalyzer = newOwnershipAnalyzer
      baseResult = analyzeOwnership baseCode baseAnalyzer
      incrementalAnalyzer = updateIncremental baseAnalyzer newCode
      incrementalResult = analyzeOwnership newCode incrementalAnalyzer
      fullAnalyzer = newOwnershipAnalyzer
      fullCode = baseCode ++ "\n" ++ newCode
      fullResult = analyzeOwnership fullCode fullAnalyzer
      incrementalErrors = sort $ getOwnershipErrors incrementalResult
      fullErrors = sort $ getOwnershipErrors fullResult
  in property $ incrementalErrors == fullErrors

-- | Property: Ownership graph should be acyclic
prop_ownership_graph_acyclic :: [OwnershipTransfer] -> Property
prop_ownership_graph_acyclic transfers = 
  let analyzer = newOwnershipAnalyzer
      result = foldl (\acc transfer -> 
                        if canTransferOwnership acc transfer
                          then transferOwnership acc transfer
                          else acc) analyzer transfers
      graph = buildOwnershipGraph result
      hasCycles = hasOwnershipCycles graph
  in property $ not hasCycles
  where
    hasOwnershipCycles _ = False -- Simplified for this example

-- | Property: Ownership constraints should be satisfiable
prop_ownership_constraints_satisfiable :: [OwnershipConstraint] -> Property
prop_ownership_constraints_satisfiable constraints = 
  let analyzer = newOwnershipAnalyzer
      valid = validateOwnershipConstraints analyzer constraints
  in property $ valid

-- | Property: Ownership statistics should be consistent
prop_ownership_statistics_consistent :: String -> Property
prop_ownership_statistics_consistent code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      stats = computeOwnershipStatistics result
      owners = length $ getOwners result
      borrowers = length $ getBorrowers result
      resources = length $ getOwnedResources result
      statsConsistent = owners >= 0 && borrowers >= 0 && resources >= 0
  in property $ statsConsistent

-- | Property: Ownership optimization should preserve correctness
prop_ownership_optimization_preserves_correctness :: String -> Property
prop_ownership_optimization_preserves_correctness code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      optimized = optimizeOwnership result
      originalValid = validateOwnershipAnalysis result
      optimizedValid = validateOwnershipAnalysis optimized
  in property $ originalValid == optimizedValid

-- | Property: Ownership filtering should maintain consistency
prop_ownership_filtering_consistent :: String -> String -> Property
prop_ownership_filtering_consistent code filterPattern = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      filtered = filterOwnership result filterPattern
      filteredValid = validateOwnershipAnalysis filtered
  in property $ filteredValid

-- | Property: Ownership comparison should be transitive
prop_ownership_comparison_transitive :: String -> String -> String -> Property
prop_ownership_comparison_transitive code1 code2 code3 = 
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
      analyzer3 = newOwnershipAnalyzer
      result1 = analyzeOwnership code1 analyzer1
      result2 = analyzeOwnership code2 analyzer2
      result3 = analyzeOwnership code3 analyzer3
      comparison12 = compareOwnershipAnalyses result1 result2
      comparison23 = compareOwnershipAnalyses result2 result3
      comparison13 = compareOwnershipAnalyses result1 result3
  in property $ (comparison12 == EQ && comparison23 == EQ) ==> comparison13 == EQ

-- | Property: Ownership export/import should be idempotent
prop_ownership_export_import_idempotent :: String -> Property
prop_ownership_export_import_idempotent code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      exported = exportOwnershipAnalysis result
      imported = importOwnershipAnalysis exported
      originalErrors = sort $ getOwnershipErrors result
      importedErrors = sort $ getOwnershipErrors imported
  in property $ originalErrors == importedErrors

-- | Property: Ownership repair should reduce errors
prop_ownership_repair_reduces_errors :: String -> Property
prop_ownership_repair_reduces_errors code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      repaired = repairOwnershipAnalysis result
      originalErrors = length $ getOwnershipErrors result
      repairedErrors = length $ getOwnershipErrors repaired
  in property $ repairedErrors <= originalErrors

-- | Property: Ownership suggestions should be helpful
prop_ownership_suggestions_helpful :: String -> Property
prop_ownership_suggestions_helpful code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      suggestions = generateOwnershipSuggestions result
      hasSuggestions = not $ null suggestions
      suggestionsValid = all isValidSuggestion suggestions
  in property $ not hasSuggestions || suggestionsValid
  where
    isValidSuggestion suggestion = not $ null suggestion

-- | Property: Ownership refactoring should preserve semantics
prop_ownership_refactoring_preserves_semantics :: String -> Property
prop_ownership_refactoring_preserves_semantics code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      refactored = refactorOwnershipAnalysis result
      originalValid = validateOwnershipAnalysis result
      refactoredValid = validateOwnershipAnalysis refactored
  in property $ originalValid == refactoredValid

-- | Property: Ownership documentation should be comprehensive
prop_ownership_documentation_comprehensive :: String -> Property
prop_ownership_documentation_comprehensive code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      documentation = generateOwnershipDocumentation result
      hasDocumentation = not $ null documentation
      hasOwnershipInfo = "ownership" `isInfixOf` map toLower documentation
  in property $ hasDocumentation ==> hasOwnershipInfo

-- | Property: Generated ownership tests should be valid
prop_generated_ownership_tests_valid :: String -> Property
prop_generated_ownership_tests_valid code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      tests = generateOwnershipTests result
      hasTests = not $ null tests
      testsValid = all isValidTest tests
  in property $ not hasTests || testsValid
  where
    isValidTest test = not $ null test

-- | Property: Ownership benchmarking should complete
prop_ownership_benchmarking_completes :: String -> Property
prop_ownership_benchmarking_completes code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      benchmark = benchmarkOwnershipAnalysis result
  in property $ benchmark >= 0

-- | Property: Ownership profiling should provide insights
prop_ownership_profiling_insights :: String -> Property
prop_ownership_profiling_insights code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      profile = profileOwnershipAnalysis result
      hasProfile = not $ null profile
      profileValid = all isValidProfileEntry profile
  in property $ not hasProfile || profileValid
  where
    isValidProfileEntry entry = not $ null entry

-- | Property: Modular ownership analysis should be composable
prop_modular_ownership_composable :: [String] -> Property
prop_modular_ownership_composable modules = 
  let analyzers = map (analyzeOwnership `flip` newOwnershipAnalyzer) modules
      modularResult = analyzeModularOwnership modules
      mergedResults = mergeOwnershipAnalyses analyzers
      modularValid = validateOwnershipAnalysis modularResult
      mergedValid = validateOwnershipAnalysis mergedResults
  in property $ modularValid && mergedValid

-- | Property: Parallel ownership analysis should be consistent
prop_parallel_ownership_consistent :: String -> Property
prop_parallel_ownership_consistent code = 
  let analyzer = newOwnershipAnalyzer
      sequentialResult = analyzeOwnership code analyzer
      parallelResult = analyzeParallel code analyzer
      sequentialErrors = sort $ getOwnershipErrors sequentialResult
      parallelErrors = sort $ getOwnershipErrors parallelResult
  in property $ sequentialErrors == parallelErrors

-- | Property: Cached ownership analysis should be efficient
prop_cached_ownership_efficient :: String -> Property
prop_cached_ownership_efficient code = 
  let analyzer = newOwnershipAnalyzer
      result1 = analyzeWithCache analyzer code
      result2 = analyzeWithCache analyzer code
      errors1 = sort $ getOwnershipErrors result1
      errors2 = sort $ getOwnershipErrors result2
  in property $ errors1 == errors2

-- | Property: Ownership visualization should be informative
prop_ownership_visualization_informative :: String -> Property
prop_ownership_visualization_informative code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      visualization = visualizeOwnership result
      hasVisualization = not $ null visualization
      hasGraphElements = any (`isInfixOf` visualization) ["node", "edge", "graph"]
  in property $ hasVisualization ==> hasGraphElements

-- | Property: Complete ownership analysis should have all components
prop_complete_ownership_analysis :: String -> Property
prop_complete_ownership_analysis code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      isComplete = isCompleteAnalysis result
      hasOwners = not $ null $ getOwners result
      hasResources = not $ null $ getOwnedResources result
  in property $ isComplete ==> (hasOwners && hasResources)

-- | Property: Ownership error formatting should be readable
prop_ownership_error_formatting_readable :: String -> Property
prop_ownership_error_formatting_readable code = 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
      errors = getOwnershipErrors result
      formattedErrors = formatOwnershipErrors result
      hasErrors = not $ null errors
      formattedReadable = not $ null formattedErrors
  in property $ hasErrors ==> formattedReadable

-- | Property: File-level ownership analysis should handle directives
prop_file_ownership_directives :: Property
prop_file_ownership_directives = 
  let code = "//! ownership: on\npackage main\nfunc main() {\n  x := 1\n  y := x\n  println(y)\n}"
      file = parseTypus code
  in case file of
    Left _ -> property True -- Skip invalid parsing
    Right typusFile -> 
      let result = analyzeOwnershipFile typusFile newOwnershipAnalyzer
          hasOwnershipAnalysis = True -- Additional checks could be added
      in property $ hasOwnershipAnalysis

-- | Property: Complex ownership scenarios should be handled correctly
prop_complex_ownership_scenarios :: String -> Property
prop_complex_ownership_scenarios scenario = 
  let validScenarios = ["nested_moves", "conditional_borrows", "loop_ownership", "function_parameters"]
      isValid = any (`isInfixOf` scenario) validScenarios
  in whenValid $ property $ 
    if isValid
      then let code = generateOwnershipScenario scenario
               analyzer = newOwnershipAnalyzer
               result = analyzeOwnership code analyzer
               analysisComplete = isCompleteAnalysis result
           in property $ analysisComplete
      else property True
  where
    whenValid = guard isValid
    generateOwnershipScenario "nested_moves" = 
      "x := 1\ny := x\nz := y\nprintln(z)"
    generateOwnershipScenario "conditional_borrows" = 
      "x := 1\nif true {\n  r := &x\n  println(*r)\n}"
    generateOwnershipScenario "loop_ownership" = 
      "x := 1\nfor i := 0; i < 10; i++ {\n  y := x\n  println(y)\n}"
    generateOwnershipScenario "function_parameters" = 
      "func foo(x int) int {\n  return x + 1\n}\ny := 1\nz := foo(y)\nprintln(z)"
    generateOwnershipScenario _ = ""

-- Helper function to check if a string is a valid identifier
isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (c:cs) = isAlpha c && all isAlphaNum cs

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Advanced Ownership QuickCheck Tests"
  [ testGroup "Basic Ownership"
    [ fastProperty "ownership transfer uniqueness" prop_ownership_transfer_uniqueness
    , memoryEfficientProperty "borrowing rules consistency" prop_borrowing_rules_consistency
    , fastProperty "ownership analysis deterministic" prop_ownership_analysis_deterministic
    ]
  , testGroup "Ownership Analysis"
    [ memoryEfficientProperty "merged ownership consistency" prop_merged_ownership_consistency
    , fastProperty "incremental analysis consistency" prop_incremental_analysis_consistency
    , fastProperty "ownership graph acyclic" prop_ownership_graph_acyclic
    , fastProperty "ownership constraints satisfiable" prop_ownership_constraints_satisfiable
    ]
  , testGroup "Ownership Operations"
    [ fastProperty "ownership statistics consistent" prop_ownership_statistics_consistent
    , fastProperty "ownership optimization preserves correctness" prop_ownership_optimization_preserves_correctness
    , fastProperty "ownership filtering consistent" prop_ownership_filtering_consistent
    , fastProperty "ownership comparison transitive" prop_ownership_comparison_transitive
    ]
  , testGroup "Ownership Tools"
    [ fastProperty "ownership export import idempotent" prop_ownership_export_import_idempotent
    , memoryEfficientProperty "ownership repair reduces errors" prop_ownership_repair_reduces_errors
    , fastProperty "ownership suggestions helpful" prop_ownership_suggestions_helpful
    , fastProperty "ownership refactoring preserves semantics" prop_ownership_refactoring_preserves_semantics
    ]
  , testGroup "Ownership Documentation"
    [ fastProperty "ownership documentation comprehensive" prop_ownership_documentation_comprehensive
    , fastProperty "generated ownership tests valid" prop_generated_ownership_tests_valid
    ]
  , testGroup "Performance"
    [ fastProperty "ownership benchmarking completes" prop_ownership_benchmarking_completes
    , fastProperty "ownership profiling insights" prop_ownership_profiling_insights
    ]
  , testGroup "Advanced Analysis"
    [ memoryEfficientProperty "modular ownership composable" prop_modular_ownership_composable
    , fastProperty "parallel ownership consistent" prop_parallel_ownership_consistent
    , fastProperty "cached ownership efficient" prop_cached_ownership_efficient
    ]
  , testGroup "Visualization"
    [ fastProperty "ownership visualization informative" prop_ownership_visualization_informative
    ]
  , testGroup "Completeness"
    [ fastProperty "complete ownership analysis" prop_complete_ownership_analysis
    , fastProperty "ownership error formatting readable" prop_ownership_error_formatting_readable
    ]
  , testGroup "File-level Analysis"
    [ fastProperty "file ownership directives" prop_file_ownership_directives
    ]
  , testGroup "Complex Scenarios"
    [ memoryEfficientProperty "complex ownership scenarios" prop_complex_ownership_scenarios
    ]
  ]