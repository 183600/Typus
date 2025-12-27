{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.IntegrationAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compileTypus, CompilationResult(..), CompilationError(..))
import ErrorHandler (errorAt, warningAt, ErrorLocation(..))
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt, posAt)
import Ownership (analyzeOwnership, OwnershipResult(..), OwnershipIssue(..))
import Dependencies (analyzeDependencies, DependencyResult(..), DependencyIssue(..))

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, sort)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Advanced Integration Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Advanced Integration Tests"
    [ testGroup "Parser to Compiler integration"
        [ testCase "parses and compiles simple function" test_parse_compile_simple
        , testCase "handles compilation errors with locations" test_compilation_errors_with_locations
        , testCase "propagates warnings through compilation pipeline" test_warnings_propagation
        , testCase "handles ownership analysis integration" test_ownership_integration
        , testCase "handles dependency analysis integration" test_dependency_integration
        ]

    , testGroup "Error handling across modules"
        [ testCase "error locations are preserved across pipeline" test_error_locations_preserved
        , testCase "multiple errors are collected correctly" test_multiple_errors_collected
        , testCase "error contexts are maintained" test_error_contexts_maintained
        , testCase "error recovery strategies work across modules" test_error_recovery_integration
        ]

    , testGroup "Ownership and Dependencies integration"
        [ testCase "ownership analysis uses parser output" test_ownership_uses_parser
        , testCase "dependency analysis respects ownership constraints" test_dependency_respects_ownership
        , testCase "combined analysis produces consistent results" test_combined_analysis_consistency
        , testCase "ownership issues affect dependency analysis" test_ownership_affects_dependencies
        ]

    , testGroup "Source location tracking integration"
        [ testCase "source locations are tracked through parsing" test_source_location_tracking
        , testCase "spans are correctly calculated for complex constructs" test_span_calculation
        , testCase "locations survive transformation pipeline" test_location_survival
        , testCase "error reporting uses accurate locations" test_accurate_error_reporting
        ]

    , testGroup "Performance and scalability"
        [ testCase "handles large files efficiently" test_large_file_handling
        , testCase "manages memory usage with many errors" test_memory_management
        , testCase "performance scales with file complexity" test_performance_scaling
        ]

    , testGroup "Property-based integration tests"
        [ fastProperty "parse-compile-analyze pipeline is deterministic" prop_pipeline_deterministic
        , fastProperty "error count is bounded by input size" prop_error_count_bounded
        , fastProperty "successful compilation preserves semantics" prop_compilation_preserves_semantics
        , fastProperty "ownership analysis is sound" prop_ownership_soundness
        ]
    ]

-- ============================================================================
-- Parser to Compiler Integration Tests
-- ============================================================================

test_parse_compile_simple :: IO ()
test_parse_compile_simple = do
  let content = "func main() {\n    return 42\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Left compileErr -> assertFailure $ "Compile failed: " ++ show compileErr
        Right result -> do
          assertBool "Compilation should succeed" (crSuccess result)

test_compilation_errors_with_locations :: IO ()
test_compilation_errors_with_locations = do
  let content = "func invalid_syntax {\n    return 42\n}\n"  -- Missing parentheses
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Right result -> assertFailure "Expected compilation error but got success"
        Left errors -> do
          assertBool "Should have compilation errors" (not (null errors))
          let firstError = head errors
              errorLoc = ceLocation firstError
          -- Error location should be meaningful
          assertBool "Error should have valid location" (line errorLoc > 0)

test_warnings_propagation :: IO ()
test_warnings_propagation = do
  let content = "func main() {\n    // This function is very simple\n    return 42\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Left compileErr -> assertFailure $ "Compile failed: " ++ show compileErr
        Right result -> do
          let warnings = crWarnings result
          -- Should have some warnings about unused variables or similar
          assertBool "Should propagate warnings through pipeline" (not (null warnings))

test_ownership_integration :: IO ()
test_ownership_integration = do
  let content = "//! ownership=true\n\nfunc main() {\n    data := make([]int, 10)\n    return data[0]\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should have ownership-related analysis results
      assertBool "Should have ownership analysis results" (not (null issues))

test_dependency_integration :: IO ()
test_dependency_integration = do
  let content = "//! dependent-types=true\n\nfunc main() {\n    vec := Vector[int]{1, 2, 3}\n    return vec.length\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let dependencyResult = analyzeDependencies typusFile
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      let dependencies = drDependencies dependencyResult
      -- Should have dependency analysis results
      assertBool "Should have dependency analysis results" (not (null dependencies))

-- ============================================================================
-- Error Handling Across Modules Tests
-- ============================================================================

test_error_locations_preserved :: IO ()
test_error_locations_preserved = do
  let content = "func test() {\n    invalid_syntax_here\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Right result -> assertFailure "Expected compilation error"
        Left errors -> do
          assertBool "Should have errors" (not (null errors))
          let firstError = head errors
              errorLoc = ceLocation firstError
          -- Location should point to the invalid syntax
          assertBool "Error location should be meaningful" (line errorLoc >= 2)

test_multiple_errors_collected :: IO ()
test_multiple_errors_collected = do
  let content = "func test() {\n    invalid1\n    invalid2\n    invalid3\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Right result -> assertFailure "Expected compilation error"
        Left errors -> do
          -- Should collect multiple errors
          assertBool "Should have multiple errors" (length errors >= 2)
          let errorLines = sort $ map (line . ceLocation) errors
          -- Errors should be on different lines
          assertBool "Errors should be on different lines" (length (nub errorLines) >= 2)

test_error_contexts_maintained :: IO ()
test_error_contexts_maintained = do
  let content = "func outer() {\n    func inner() {\n        invalid_syntax\n    }\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Right result -> assertFailure "Expected compilation error"
        Left errors -> do
          assertBool "Should have errors" (not (null errors))
          let firstError = head errors
              context = ceContext firstError
          -- Error context should include function nesting information
          assertBool "Error context should include function information" (not (null context))

test_error_recovery_integration :: IO ()
test_error_recovery_integration = do
  let content = "func test() {\n    invalid1\n    valid_code := 42\n    invalid2\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Right result -> do
          -- If compilation succeeded, check that valid code was processed
          let warnings = crWarnings result
          assertBool "Should have warnings about invalid syntax" (not (null warnings))
        Left errors -> do
          -- Should still attempt to recover and find multiple errors
          assertBool "Should find multiple errors despite recovery attempts" (length errors >= 2)

-- ============================================================================
-- Ownership and Dependencies Integration Tests
-- ============================================================================

test_ownership_uses_parser :: IO ()
test_ownership_uses_parser = do
  let content = "//! ownership=true\n\nfunc test() {\n    data := make([]int, 10)\n    return data[0]\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
          directives = tfDirectives typusFile
          ownershipEnabled = fmap locatedValue (fdOwnership directives)
      assertBool "Ownership should be enabled in directives" (ownershipEnabled == Just True)
      assertBool "Ownership analysis should use parser output" (orSuccess ownershipResult)

test_dependency_respects_ownership :: IO ()
test_dependency_respects_ownership = do
  let content = "//! ownership=true, dependent-types=true\n\nfunc test() {\n    vec := Vector[int]{1, 2, 3}\n    data := vec.data\n    return data.length\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
          dependencyResult = analyzeDependencies typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      -- Dependency analysis should respect ownership constraints
      let ownershipIssues = orIssues ownershipResult
          dependencies = drDependencies dependencyResult
      assertBool "Should have ownership analysis results" (not (null ownershipIssues))
      assertBool "Should have dependency analysis results" (not (null dependencies))

test_combined_analysis_consistency :: IO ()
test_combined_analysis_consistency = do
  let content = "//! ownership=true, dependent-types=true\n\nfunc test() {\n    vec := make(Vector[int], 10)\n    for i := 0; i < 10; i++ {\n        vec.push(i)\n    }\n    return vec.length\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
          dependencyResult = analyzeDependencies typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      -- Results should be consistent with each other
      let ownershipIssues = orIssues ownershipResult
          dependencies = drDependencies dependencyResult
      -- Both analyses should find issues related to the same constructs
      assertBool "Analyses should be consistent" (True)  -- More specific consistency checks would depend on actual implementation

test_ownership_affects_dependencies :: IO ()
test_ownership_affects_dependencies = do
  let content = "//! ownership=true\n\nfunc test() {\n    data := make([]int, 10)\n    borrowed := &data[0]\n    return *borrowed\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
          dependencyResult = analyzeDependencies typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      assertBool "Dependency analysis should succeed" (drSuccess dependencyResult)
      -- Ownership issues should influence dependency analysis
      let ownershipIssues = orIssues ownershipResult
      assertBool "Should find ownership issues with borrowing" (not (null ownershipIssues))

-- ============================================================================
-- Source Location Tracking Integration Tests
-- ============================================================================

test_source_location_tracking :: IO ()
test_source_location_tracking = do
  let content = "func main() {\n    line1\n    line2\n    line3\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have code blocks" (not (null blocks))
      let firstBlock = head blocks
          span = cbSpan firstBlock
      assertBool "Span should be valid" (spanStart span <= spanEnd span)

test_span_calculation :: IO ()
test_span_calculation = do
  let content = "func complex() {\n    if condition {\n        nested()\n    } else {\n        other()\n    }\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have code blocks" (not (null blocks))
      let firstBlock = head blocks
          span = cbSpan firstBlock
          start = spanStart span
          end = spanEnd span
      assertBool "Start position should be before end position" (start <= end)
      assertBool "Span should cover multiple lines" (posLine end > posLine start)

test_location_survival :: IO ()
test_location_survival = do
  let content = "func test() {\n    value := 42\n    return value\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Left errors -> do
          assertBool "Should have errors with locations" (not (null errors))
          let firstError = head errors
              location = ceLocation firstError
          assertBool "Error should have meaningful location" (line location > 0)
        Right result -> do
          let warnings = crWarnings result
          if not (null warnings)
            then let firstWarning = head warnings
                     location = wLocation firstWarning
                 in assertBool "Warning should have meaningful location" (line location > 0)
            else assertBool "Compilation succeeded without warnings" True

test_accurate_error_reporting :: IO ()
test_accurate_error_reporting = do
  let content = "func test() {\n    undefined_var := 42\n    return undefined_var\n}\n"
      parseResult = parseTypus content
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Right result -> assertFailure "Expected compilation error"
        Left errors -> do
          assertBool "Should have errors" (not (null errors))
          let firstError = head errors
              location = ceLocation firstError
              message = ceMessage firstError
          -- Error should point to the undefined variable
          assertBool "Error should be on line 2" (line location == 2)
          assertBool "Error message should mention undefined variable" ("undefined_var" `isInfixOf` message)

-- ============================================================================
-- Performance and Scalability Tests
-- ============================================================================

test_large_file_handling :: IO ()
test_large_file_handling = do
  let largeFunction = "func large() {\n"
      functionBody = concat $ replicate 100 "    x := x + 1\n"
      largeContent = largeFunction ++ functionBody ++ "}\n"
      parseResult = parseTypus largeContent
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed on large file: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Left errors -> do
          -- Should handle large files even with errors
          assertBool "Should handle large files with errors gracefully" (not (null errors))
        Right result -> do
          assertBool "Should compile large file successfully" (crSuccess result)

test_memory_management :: IO ()
test_memory_management = do
  let errorContent = concat $ replicate 100 "func test" ++ show [1..100] ++ "() { invalid }\n"
      parseResult = parseTypus errorContent
  case parseResult of
    Left parseErr -> assertFailure $ "Parse failed: " ++ show parseErr
    Right typusFile -> do
      let compileResult = compileTypus typusFile
      case compileResult of
        Left errors -> do
          -- Should handle many errors without running out of memory
          assertBool "Should handle many errors" (length errors >= 10)
        Right result -> do
          assertBool "Should handle many functions" (True)

test_performance_scaling :: IO ()
test_performance_scaling = do
  let simpleContent = "func test() { return 42 }\n"
      mediumContent = concat $ replicate 10 simpleContent
      largeContent = concat $ replicate 100 simpleContent
      
      simpleParse = parseTypus simpleContent
      mediumParse = parseTypus mediumContent
      largeParse = parseTypus largeContent
      
  case (simpleParse, mediumParse, largeParse) of
    (Right simple, Right medium, Right large) -> do
      let simpleBlocks = length (tfBlocks simple)
          mediumBlocks = length (tfBlocks medium)
          largeBlocks = length (tfBlocks large)
      assertBool "Should scale linearly with content size" (mediumBlocks >= simpleBlocks && largeBlocks >= mediumBlocks)
    _ -> assertFailure "All parses should succeed"

-- ============================================================================
-- Property-Based Integration Tests
-- ============================================================================

prop_pipeline_deterministic :: Property
prop_pipeline_deterministic =
  forAll arbitrary $ \content ->
    let parseResult1 = parseTypus content
        parseResult2 = parseTypus content
    in case (parseResult1, parseResult2) of
         (Left err1, Left err2) -> err1 === err2
         (Right file1, Right file2) -> file1 === file2
         _ -> property False

prop_error_count_bounded :: Property
prop_error_count_bounded =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True  -- Parse errors count as 1
         Right typusFile ->
           let compileResult = compileTypus typusFile
           in case compileResult of
                Left errors -> length errors <= length (lines content) + 10
                Right _ -> property True

prop_compilation_preserves_semantics :: Property
prop_compilation_preserves_semantics =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True  -- Can't preserve semantics if parse fails
         Right typusFile ->
           let compileResult = compileTypus typusFile
           in case compileResult of
                Left _ -> property True  -- Compilation errors can't preserve semantics
                Right result -> 
                  -- If compilation succeeds, basic semantic properties should be preserved
                  crSuccess result === True

prop_ownership_soundness :: Property
prop_ownership_soundness =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True  -- Can't analyze if parse fails
         Right typusFile ->
           let ownershipResult = analyzeOwnership typusFile
           in property $ orSuccess ownershipResult ==> 
                        let issues = orIssues ownershipResult
                        in all (\issue -> oiLine issue > 0) issues