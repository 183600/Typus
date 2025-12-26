{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.DependencyAnalysisQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (nub, sort, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Set (Set)

import Dependencies
import qualified Dependencies.TypeSystem as Dep
import qualified Dependencies.AST as DepAST
import qualified Dependencies.Analyzer as DepAn
import Compiler.Errors.Core
import SourceLocation

-- | Test dependency graph construction
testDependencyGraphConstruction :: Property
testDependencyGraphConstruction =
  forAll arbitrary $ \modules ->
    let graph = DepAn.buildDependencyGraph modules
        nodes = DepAn.getGraphNodes graph
        edges = DepAn.getGraphEdges graph
        moduleCount = length modules
    in length nodes >= moduleCount .&&.
       length edges >= 0

-- | Test circular dependency detection
testCircularDependencyDetection :: Property
testCircularDependencyDetection =
  forAll arbitrary $ \dependencies ->
    let graph = DepAn.buildGraphFromDependencies dependencies
        cycles = DepAn.findCycles graph
        hasCycles = not (null cycles)
        isAcyclic = DepAn.isAcyclic graph
    in hasCycles === not isAcyclic

-- | Test dependency resolution order
testDependencyResolutionOrder :: Property
testDependencyResolutionOrder =
  forAll arbitrary $ \dependencies ->
    let graph = DepAn.buildGraphFromDependencies dependencies
        resolution = DepAn.topologicalSort graph
        dependencyCount = length dependencies
    in if DepAn.isAcyclic graph
       then length resolution >= dependencyCount
       else isNothing resolution

-- | Test type dependency inference
testTypeDependencyInference :: Property
testTypeDependencyInference =
  forAll arbitrary $ \ast ->
    let typeDeps = DepAn.inferTypeDependencies ast
        allTypes = DepAn.extractTypes ast
        depTypes = DepAn.getDependentTypes typeDeps
    in all (`elem` allTypes) depTypes .&&.
       length typeDeps >= 0

-- | Test module dependency consistency
testModuleDependencyConsistency :: Property
testModuleDependencyConsistency =
  forAll arbitrary $ \modules ->
    let dependencies = DepAn.analyzeModuleDependencies modules
        moduleNames = map DepAn.getModuleName modules
        dependencyNames = DepAn.getDependencyNames dependencies
    in all (`elem` moduleNames) dependencyNames .&&.
       length dependencies >= 0

-- | Test incremental dependency analysis
testIncrementalDependencyAnalysis :: Property
testIncrementalDependencyAnalysis =
  forAll arbitrary $ \initialModules ->
    forAll arbitrary $ \changedModules ->
      let initialAnalysis = DepAn.analyzeDependencies initialModules
          updatedAnalysis = DepAn.incrementalUpdate initialAnalysis changedModules
          initialGraph = DepAn.getDependencyGraph initialAnalysis
          updatedGraph = DepAn.getDependencyGraph updatedAnalysis
      in length (DepAn.getGraphNodes updatedGraph) >= 
         length (DepAn.getGraphNodes initialGraph)

-- | Test dependency pruning
testDependencyPruning :: Property
testDependencyPruning =
  forAll arbitrary $ \dependencies ->
    forAll arbitrary $ \entryPoints ->
      let pruned = DepAn.pruneDependencies dependencies entryPoints
        originalCount = length dependencies
        prunedCount = length pruned
    in prunedCount <= originalCount .&&.
       all (isReachable entryPoints dependencies) pruned

-- | Test dependency cycle breaking
testDependencyCycleBreaking :: Property
testDependencyCycleBreaking =
  forAll arbitrary $ \dependencies ->
    let cycles = DepAn.findDependencyCycles dependencies
        broken = DepAn.breakCycles dependencies
        brokenCycles = DepAn.findDependencyCycles broken
    in if null cycles
       then broken === dependencies
       else length brokenCycles < length cycles

-- | Test dependency version compatibility
testDependencyVersionCompatibility :: Property
testDependencyVersionCompatibility =
  forAll arbitrary $ \dependencies ->
    let versions = DepAn.extractVersions dependencies
        conflicts = DepAn.findVersionConflicts versions
        compatible = DepAn.checkCompatibility dependencies
    in if null conflicts
       then compatible
       else property True

-- | Test transitive dependency closure
testTransitiveDependencyClosure :: Property
testTransitiveDependencyClosure =
  forAll arbitrary $ \dependencies ->
    let closure = DepAn.transitiveClosure dependencies
        directDeps = DepAn.getDirectDependencies dependencies
        transitiveDeps = DepAn.getTransitiveDependencies closure
    in all (`elem` transitiveDeps) directDeps .&&.
       length transitiveDeps >= length directDeps

-- | Test dependency impact analysis
testDependencyImpactAnalysis :: Property
testDependencyImpactAnalysis =
  forAll arbitrary $ \dependencies ->
    forAll arbitrary $ \changedModule ->
      let impact = DepAn.analyzeImpact dependencies changedModule
        affectedModules = DepAn.getAffectedModules impact
        allModules = DepAn.getAllModules dependencies
      in all (`elem` allModules) affectedModules .&&.
         length affectedModules >= 0

-- | Test dependency optimization
testDependencyOptimization :: Property
testDependencyOptimization =
  forAll arbitrary $ \dependencies ->
    let optimized = DepAn.optimizeDependencies dependencies
        originalCount = length dependencies
        optimizedCount = length optimized
    in optimizedCount <= originalCount .&&.
       DepAn.preservesSemantics dependencies optimized

-- | Test dependency validation
testDependencyValidation :: Property
testDependencyValidation =
  forAll arbitrary $ \dependencies ->
    let validation = DepAn.validateDependencies dependencies
        errors = DepAn.getValidationErrors validation
        warnings = DepAn.getValidationWarnings validation
    in length errors >= 0 .&&.
       length warnings >= 0 .&&.
       (if null errors then DepAn.isValid validation else property True)

-- | Test dependency merging
testDependencyMerging :: Property
testDependencyMerging =
  forAll arbitrary $ \deps1 ->
    forAll arbitrary $ \deps2 ->
      let merged = DepAn.mergeDependencies deps1 deps2
        mergedCount = length merged
        totalCount = length deps1 + length deps2
        uniqueCount = length $ nub $ deps1 ++ deps2
    in mergedCount <= totalCount .&&.
       mergedCount >= uniqueCount

-- | Test dependency graph properties
testDependencyGraphProperties :: Property
testDependencyGraphProperties =
  forAll arbitrary $ \graph ->
    let nodes = DepAn.getGraphNodes graph
        edges = DepAn.getGraphEdges graph
        isDag = DepAn.isAcyclic graph
        hasCycles = not isDag
        stronglyConnected = DepAn.getStronglyConnectedComponents graph
    in length nodes >= 0 .&&.
       length edges >= 0 .&&.
       length stronglyConnected >= 0 .&&.
       (if hasCycles then length stronglyConnected > 1 else property True)

-- Helper functions

isReachable :: [String] -> [a] -> a -> Bool
isReachable = undefined -- Placeholder implementation

tests :: TestTree
tests = testGroup "Dependency Analysis QuickCheck Tests"
  [ testProperty "Graph construction" testDependencyGraphConstruction
  , testProperty "Circular dependency detection" testCircularDependencyDetection
  , testProperty "Resolution order" testDependencyResolutionOrder
  , testProperty "Type dependency inference" testTypeDependencyInference
  , testProperty "Module dependency consistency" testModuleDependencyConsistency
  , testProperty "Incremental analysis" testIncrementalDependencyAnalysis
  , testProperty "Dependency pruning" testDependencyPruning
  , testProperty "Cycle breaking" testDependencyCycleBreaking
  , testProperty "Version compatibility" testDependencyVersionCompatibility
  , testProperty "Transitive closure" testTransitiveDependencyClosure
  , testProperty "Impact analysis" testDependencyImpactAnalysis
  , testProperty "Optimization" testDependencyOptimization
  , testProperty "Validation" testDependencyValidation
  , testProperty "Merging" testDependencyMerging
  , testProperty "Graph properties" testDependencyGraphProperties
  ]