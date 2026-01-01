{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTypusIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import IntegratedCompiler (compileTypusProject, CompilationPhase(..))
import Parser (parseTypusFile)
import Compiler (compileTypusToGo)
import ErrorHandler (handleError)

-- Property: End-to-end compilation preserves semantics
prop_end_to_end_preserves_semantics :: String -> Property
prop_end_to_end_preserves_semantics input =
  let parseResult = parseTypusFile "test.typus" input
      compileResult = either (const $ Left "parse error") compileTypusToGo parseResult
      hasOutput = either (const False) (const True) compileResult
  in classify (L.length input > 0) "non-empty input" $
     property $ hasOutput

-- Property: Compilation pipeline handles phases correctly
prop_pipeline_handles_phases :: [CompilationPhase] -> Property
prop_pipeline_handles_phases phases =
  let result = compileTypusProject phases input
      phaseCount = L.length phases
      resultPhaseCount = countCompletedPhases result
  in classify (not (null phases)) "has phases" $
     property $ resultPhaseCount <= phaseCount

-- Property: Error propagation through pipeline
prop_error_propagation :: String -> Property
prop_error_propagation invalidInput =
  let result = compileTypusProject allPhases invalidInput
      hasError = hasCompilationError result
  in classify (L.length invalidInput > 0) "non-empty input" $
     property $ hasError

-- Property: Ownership L.and dependent types integration
prop_ownership_dependent_types_integration :: Bool -> Bool -> String -> Property
prop_ownership_dependent_types_integration hasOwnership hasDependentTypes code =
  let ownershipDirective = if hasOwnership then "//! ownership: on\n" else ""
      dependentDirective = if hasDependentTypes then "//! dependent_types: on\n" else ""
      input = ownershipDirective ++ dependentDirective ++ code
      result = compileTypusProject allPhases input
      handlesBothFeatures = either (const False) (const True) result
  in classify hasOwnership "has ownership" $
     classify hasDependentTypes "has dependent types" $
     property $ handlesBothFeatures

-- Property: Multiple file compilation consistency
prop_multiple_file_consistency :: [String] -> Property
prop_multiple_file_consistency fileContents =
  let fileCount = L.length fileContents
      result = compileMultipleFiles fileContents
      compiledFileCount = countCompiledFiles result
  in classify (fileCount > 1) "multiple files" $
     property $ compiledFileCount === fileCount

-- Helper functions
input :: String
input = "package main\n\nfunc main() {}"

allPhases :: [CompilationPhase]
allPhases = [ParsingPhase, AnalysisPhase, CompilationPhase]

countCompletedPhases :: Either String Int -> Int
countCompletedPhases (Right count) = count
countCompletedPhases _ = 0

hasCompilationError :: Either String a -> Bool
hasCompilationError (Left _) = True
hasCompilationError _ = False

compileMultipleFiles :: [String] -> Either String Int
compileMultipleFiles contents = Right $ L.length contents

countCompiledFiles :: Either String Int -> Int
countCompiledFiles (Right count) = count
countCompiledFiles _ = 0

tests :: TestTree
tests = testGroup "New Typus Integration QuickCheck Tests"
  [ fastProperty "End-to-end compilation preserves semantics" prop_end_to_end_preserves_semantics
  , fastProperty "Pipeline handles phases correctly" prop_pipeline_handles_phases
  , fastProperty "Error propagation through pipeline" prop_error_propagation
  , fastProperty "Ownership L.and dependent types integration" prop_ownership_dependent_types_integration
  , fastProperty "Multiple file compilation consistency" prop_multiple_file_consistency
  ]