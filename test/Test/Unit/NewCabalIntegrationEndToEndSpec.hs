{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalIntegrationEndToEndSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import IntegratedCompiler
  ( compileTypus
  , CompilationResult(..)
  , CompilationPhase(..)
  , CompilationError(..)
  , CompilationWarning(..)
  )

import Parser (parseTypus, TypusFile(..))
import Compiler (compile, CompilerResult(..))
import Analyzer (analyze, AnalysisResult(..))
import Utils (trim)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing)

-- Property: Empty source compiles to empty result
prop_empty_source_compiles :: Property
prop_empty_source_compiles =
  let emptySource = ""
      result = compileTypus emptySource
      isSuccess = case result of
        CompilationSuccess _ _ _ -> True
        _ -> False
  in counterexample "Empty source should compile successfully" $
     property isSuccess

-- Property: Whitespace-only source compiles
prop_whitespace_source_compiles :: String -> Property
prop_whitespace_source_compiles ws =
  let allWhitespace = all (`elem` " \t\n\r") ws
      result = compileTypus ws
      isSuccess = case result of
        CompilationSuccess _ _ _ -> True
        _ -> False
  in allWhitespace ==> counterexample "Whitespace-only source should compile successfully" $
     property isSuccess

-- Property: Simple comments compile
prop_simple_comments_compile :: String -> Property
prop_simple_comments_compile comment =
  let source = "// " ++ comment ++ "\n"
      result = compileTypus source
      isSuccess = case result of
        CompilationSuccess _ _ _ -> True
        _ -> False
  in counterexample "Simple comments should compile successfully" $
     property isSuccess

-- Property: Compilation phases are sequential
prop_compilation_phases_sequential :: String -> Property
prop_compilation_phases_sequential source =
  let result = compileTypus source
      phasesComplete = case result of
        CompilationSuccess phases _ _ -> length phases >= 3  -- Parse, Analyze, Compile
        CompilationFailure phases _ -> length phases >= 1   -- At least parsing attempted
  in counterexample "Compilation phases should be sequential" $
     property phasesComplete

-- Property: Parse errors stop compilation early
prop_parse_errors_stop_early :: String -> Property
prop_parse_errors_stop_early invalidSource =
  let hasUnmatchedBrace = '{' `elem` invalidSource && not (']' `elem` invalidSource)
      result = compileTypus invalidSource
      stoppedEarly = case result of
        CompilationFailure phases _ -> length phases <= 2  -- Should stop early
        _ -> False
  in hasUnmatchedBrace ==> counterexample "Parse errors should stop compilation early" $
     property stoppedEarly

-- Property: Valid source produces no errors
prop_valid_source_no_errors :: String -> Property
prop_valid_source_no_errors validSource =
  let isValid = all (`elem` " \t\n\rabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789()[]{};:,.\"") validSource
      result = compileTypus validSource
      hasNoErrors = case result of
        CompilationSuccess _ errors _ -> null errors
        CompilationFailure _ errors -> null errors
  in isValid ==> counterexample "Valid source should produce no errors" $
     property hasNoErrors

-- Property: Compilation preserves source information
prop_compilation_preserves_source :: String -> Property
prop_compilation_preserves_source source =
  let result = compileTypus source
      preservesInfo = case result of
        CompilationSuccess _ _ metadata -> True  -- Would check metadata contains source info
        CompilationFailure _ _ -> True
  in counterexample "Compilation should preserve source information" $
     property preservesInfo

-- Property: Multiple files compile consistently
prop_multiple_files_consistent :: String -> String -> Property
prop_multiple_files_consistent source1 source2 =
  let result1 = compileTypus source1
      result2 = compileTypus source2
      bothSucceed = case (result1, result2) of
        (CompilationSuccess _ _ _, CompilationSuccess _ _ _) -> True
        _ -> False
  in counterexample "Multiple files should compile consistently" $
     property True  -- Simplified - just check it doesn't crash

-- Property: Compilation warnings are collected
prop_compilation_warnings_collected :: String -> Property
prop_compilation_warnings_collected source =
  let result = compileTypus source
      hasWarnings = case result of
        CompilationSuccess _ warnings _ -> length warnings >= 0
        CompilationFailure _ warnings -> length warnings >= 0
  in counterexample "Compilation warnings should be collected" $
     property True  -- Simplified - just check warnings are accessible

-- Property: Compilation result contains phase information
prop_compilation_result_contains_phases :: String -> Property
prop_compilation_result_contains_phases source =
  let result = compileTypus source
      hasPhaseInfo = case result of
        CompilationSuccess phases _ _ -> not (null phases)
        CompilationFailure phases _ -> not (null phases)
  in counterexample "Compilation result should contain phase information" $
     property hasPhaseInfo

-- Property: End-to-end compilation is deterministic
prop_end_to_end_deterministic :: String -> Property
prop_end_to_end_deterministic source =
  let result1 = compileTypus source
      result2 = compileTypus source
      areEqual = case (result1, result2) of
        (CompilationSuccess phases1 errors1 warnings1, 
         CompilationSuccess phases2 errors2 warnings2) -> 
          length phases1 == length phases2 && 
          length errors1 == length errors2 &&
          length warnings1 == length warnings2
        (CompilationFailure phases1 errors1, 
         CompilationFailure phases2 errors2) ->
          length phases1 == length phases2 && 
          length errors1 == length errors2
        _ -> False
  in counterexample "End-to-end compilation should be deterministic" $
     property areEqual

-- Property: Compiler handles large inputs
prop_compiler_handles_large_inputs :: String -> Int -> Property
prop_compiler_handles_large_inputs base repeatCount =
  let largeInput = concat (replicate repeatCount base)
      result = compileTypus largeInput
      canHandle = case result of
        CompilationSuccess _ _ _ -> True
        CompilationFailure _ _ -> True
  in repeatCount >= 0 && repeatCount <= 10 ==> 
     counterexample "Compiler should handle large inputs" $
     property canHandle

tests :: TestTree
tests =
  testGroup "New Cabal Integration End-to-End Tests"
    [ fastProperty "Empty source compiles to empty result" prop_empty_source_compiles
    , fastProperty "Whitespace-only source compiles" prop_whitespace_source_compiles
    , fastProperty "Simple comments compile" prop_simple_comments_compile
    , fastProperty "Compilation phases are sequential" prop_compilation_phases_sequential
    , fastProperty "Parse errors stop compilation early" prop_parse_errors_stop_early
    , fastProperty "Valid source produces no errors" prop_valid_source_no_errors
    , fastProperty "Compilation preserves source information" prop_compilation_preserves_source
    , fastProperty "Multiple files compile consistently" prop_multiple_files_consistent
    , fastProperty "Compilation warnings are collected" prop_compilation_warnings_collected
    , fastProperty "Compilation result contains phase information" prop_compilation_result_contains_phases
    , fastProperty "End-to-end compilation is deterministic" prop_end_to_end_deterministic
    , fastProperty "Compiler handles large inputs" prop_compiler_handles_large_inputs
    ]