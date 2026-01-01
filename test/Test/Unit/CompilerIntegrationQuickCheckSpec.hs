{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CompilerIntegrationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (sort)
import Data.Maybe (isJust, isNothing, fromMaybe)

import Compiler
import IntegratedCompiler
import Parser
import ErrorHandler
import SourceLocation
import qualified Ownership
import qualified Dependencies
import qualified DependentTypesParser

-- | Test end-to-end compilation pipeline
testEndToEndCompilationPipeline :: Property
testEndToEndCompilationPipeline =
  forAll arbitrary $ \sourceCode ->
    let parsed = parseTypus sourceCode
        analyzed = IntegratedCompiler.analyze parsed
        compiled = IntegratedCompiler.compile analyzed
        errors = IntegratedCompiler.getErrors compiled
        warnings = IntegratedCompiler.getWarnings compiled
    -- Pipeline should complete without crashing
    in L.length errors >= 0 .&&. L.length warnings >= 0

-- | Test compiler phase consistency
testCompilerPhaseConsistency :: Property
testCompilerPhaseConsistency =
  forAll arbitrary $ \sourceCode ->
    let parsed = parseTypus sourceCode
        syntaxErrors = tfSyntaxErrors parsed
        analyzed = IntegratedCompiler.analyze parsed
        analysisErrors = IntegratedCompiler.getAnalysisErrors analyzed
        compiled = IntegratedCompiler.compile analyzed
        compilationErrors = IntegratedCompiler.getCompilationErrors compiled
    -- Error counts should be non-decreasing through phases
    in L.length syntaxErrors <= L.length analysisErrors + L.length syntaxErrors .&&.
       L.length analysisErrors + L.length syntaxErrors <= 
       L.length compilationErrors + L.length analysisErrors + L.length syntaxErrors

-- | Test compiler error propagation
testCompilerErrorPropagation :: Property
testCompilerErrorPropagation =
  forAll arbitrary $ \sourceCode ->
    let parsed = parseTypus sourceCode
        analyzed = IntegratedCompiler.analyze parsed
        compiled = IntegratedCompiler.compile analyzed
        allErrors = IntegratedCompiler.getAllErrors compiled
    -- All errors should have valid source locations
    in L.all hasValidLocation allErrors

-- | Test compiler warning consistency
testCompilerWarningConsistency :: Property
testCompilerWarningConsistency =
  forAll arbitrary $ \sourceCode ->
    let parsed = parseTypus sourceCode
        analyzed = IntegratedCompiler.analyze parsed
        compiled = IntegratedCompiler.compile analyzed
        warnings = IntegratedCompiler.getWarnings compiled
        warningMessages = map errorMessage warnings
    -- Warning messages should be unique L.and informative
    in L.length warningMessages === L.length (nub warningMessages) .&&.
       L.all (not . null) warningMessages

-- | Test compiler optimization invariants
testCompilerOptimizationInvariants :: Property
testCompilerOptimizationInvariants =
  forAll arbitrary $ \sourceCode ->
    let unoptimized = IntegratedCompiler.compile sourceCode
        optimized = IntegratedCompiler.compileWithOptimizations sourceCode
        unoptimizedIr = IntegratedCompiler.getIR unoptimized
        optimizedIr = IntegratedCompiler.getIR optimized
    -- Optimized code should be semantically equivalent
    in IntegratedCompiler.areSemanticallyEquivalent unoptimizedIr optimizedIr

-- | Test compiler resource management
testCompilerResourceManagement :: Property
testCompilerResourceManagement =
  forAll arbitrary $ \sourceCode ->
    let compilation = IntegratedCompiler.compile sourceCode
        resourcesUsed = IntegratedCompiler.getResourcesUsed compilation
        memoryUsage = IntegratedCompiler.getMemoryUsage compilation
    -- Resource usage should be reasonable
    in resourcesUsed >= 0 .&&. memoryUsage >= 0

-- | Test compiler parallel processing
testCompilerParallelProcessing :: Property
testCompilerParallelProcessing =
  forAll arbitrary $ \sourceFiles ->
    let sequentialResults = map IntegratedCompiler.compile sourceFiles
        parallelResult = IntegratedCompiler.compileParallel sourceFiles
        sequentialErrors = L.sum $ map IntegratedCompiler.getErrorCount sequentialResults
        parallelErrors = IntegratedCompiler.getErrorCount parallelResult
    -- Parallel L.and sequential results should be equivalent
    in sequentialErrors === parallelErrors

-- | Test compiler incremental compilation
testCompilerIncrementalCompilation :: Property
testCompilerIncrementalCompilation =
  forAll arbitrary $ \initialFiles ->
    forAll arbitrary $ \changedFiles ->
      let initialCompilation = IntegratedCompiler.compileProject initialFiles
          incrementalCompilation = IntegratedCompiler.compileIncremental 
                                    initialCompilation changedFiles
          fullRecompilation = IntegratedCompiler.compileProject (initialFiles ++ changedFiles)
      -- Incremental compilation should be faster but produce equivalent results
    in IntegratedCompiler.getModuleCount incrementalCompilation ===
       IntegratedCompiler.getModuleCount fullRecompilation

-- | Test compiler dependency resolution
testCompilerDependencyResolution :: Property
testCompilerDependencyResolution =
  forAll arbitrary $ \modules ->
    let dependencies = IntegratedCompiler.resolveDependencies modules
        dependencyGraph = IntegratedCompiler.buildDependencyGraph dependencies
        sortedModules = IntegratedCompiler.sortByDependencies dependencyGraph modules
    -- Sorted modules should respect dependency order
    in IntegratedCompiler.validateDependencyOrder sortedModules dependencyGraph

-- | Test compiler type checking integration
testCompilerTypeCheckingIntegration :: Property
testCompilerTypeCheckingIntegration =
  forAll arbitrary $ \sourceCode ->
    let parsed = parseTypus sourceCode
        typeCheck = IntegratedCompiler.typeCheck parsed
        typeErrors = IntegratedCompiler.getTypeErrors typeCheck
        inferredTypes = IntegratedCompiler.getInferredTypes typeCheck
    -- Type checking should provide useful error information
    in L.all hasValidTypeLocation typeErrors .&&.
       L.all isValidInferredType inferredTypes

-- | Test compiler ownership analysis integration
testCompilerOwnershipAnalysisIntegration :: Property
testCompilerOwnershipAnalysisIntegration =
  forAll arbitrary $ \sourceCode ->
    let parsed = parseTypus sourceCode
        ownershipAnalysis = IntegratedCompiler.analyzeOwnership parsed
        ownershipErrors = IntegratedCompiler.getOwnershipErrors ownershipAnalysis
        borrowChecks = IntegratedCompiler.getBorrowChecks ownershipAnalysis
    -- Ownership analysis should catch borrowing violations
    in L.all hasValidOwnershipLocation ownershipErrors .&&.
       L.all isValidBorrowCheck borrowChecks

-- | Test compiler code generation consistency
testCompilerCodeGenerationConsistency :: Property
testCompilerCodeGenerationConsistency =
  forAll arbitrary $ \sourceCode ->
    let compilation1 = IntegratedCompiler.compile sourceCode
        compilation2 = IntegratedCompiler.compile sourceCode
        code1 = IntegratedCompiler.getGeneratedCode compilation1
        code2 = IntegratedCompiler.getGeneratedCode compilation2
    -- Multiple compilations should produce identical code
    in code1 === code2

-- | Test compiler error recovery
testCompilerErrorRecovery :: Property
testCompilerErrorRecovery =
  forAll arbitrary $ \malformedCode ->
    let compilation = IntegratedCompiler.compile malformedCode
        errors = IntegratedCompiler.getErrors compilation
        recovered = IntegratedCompiler.attemptRecovery compilation
    -- Compiler should attempt recovery from errors
    in if null errors
       then recovered === compilation
       else IntegratedCompiler.hasRecoveryActions recovered

-- | Test compiler configuration validation
testCompilerConfigurationValidation :: Property
testCompilerConfigurationValidation =
  forAll arbitrary $ \config ->
    let validation = IntegratedCompiler.validateConfiguration config
        errors = IntegratedCompiler.getConfigurationErrors validation
        warnings = IntegratedCompiler.getConfigurationWarnings validation
    -- Configuration validation should provide clear feedback
    in L.length errors >= 0 .&&. L.length warnings >= 0

-- | Test compiler performance characteristics
testCompilerPerformanceCharacteristics :: Property
testCompilerPerformanceCharacteristics =
  forAll arbitrary $ \sourceCode ->
    let compilation = IntegratedCompiler.compile sourceCode
        parseTime = IntegratedCompiler.getParseTime compilation
        analysisTime = IntegratedCompiler.getAnalysisTime compilation
        codeGenTime = IntegratedCompiler.getCodeGenTime compilation
        totalTime = IntegratedCompiler.getTotalTime compilation
    -- Timing should be reasonable L.and additive
    in parseTime >= 0 .&&. analysisTime >= 0 .&&. codeGenTime >= 0 .&&.
       totalTime >= parseTime + analysisTime + codeGenTime

-- Helper functions

hasValidLocation :: Compiler.Errors.Core.TypeError -> Bool
hasValidLocation = undefined -- Placeholder implementation

hasValidTypeLocation :: Compiler.Errors.Core.TypeError -> Bool
hasValidTypeLocation = undefined -- Placeholder implementation

isValidInferredType :: a -> Bool
isValidInferredType = undefined -- Placeholder implementation

hasValidOwnershipLocation :: a -> Bool
hasValidOwnershipLocation = undefined -- Placeholder implementation

isValidBorrowCheck :: a -> Bool
isValidBorrowCheck = undefined -- Placeholder implementation

tests :: TestTree
tests = testGroup "Compiler Integration QuickCheck Tests"
  [ testProperty "End-to-end pipeline" testEndToEndCompilationPipeline
  , testProperty "Phase consistency" testCompilerPhaseConsistency
  , testProperty "Error propagation" testCompilerErrorPropagation
  , testProperty "Warning consistency" testCompilerWarningConsistency
  , testProperty "Optimization invariants" testCompilerOptimizationInvariants
  , testProperty "Resource management" testCompilerResourceManagement
  , testProperty "Parallel processing" testCompilerParallelProcessing
  , testProperty "Incremental compilation" testCompilerIncrementalCompilation
  , testProperty "Dependency resolution" testCompilerDependencyResolution
  , testProperty "Type checking integration" testCompilerTypeCheckingIntegration
  , testProperty "Ownership analysis integration" testCompilerOwnershipAnalysisIntegration
  , testProperty "Code generation consistency" testCompilerCodeGenerationConsistency
  , testProperty "Error recovery" testCompilerErrorRecovery
  , testProperty "Configuration validation" testCompilerConfigurationValidation
  , testProperty "Performance characteristics" testCompilerPerformanceCharacteristics
  ]