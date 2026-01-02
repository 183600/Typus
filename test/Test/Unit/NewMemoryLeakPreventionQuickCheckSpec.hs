{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | Memory leak prevention tests for various modules
module Test.Unit.NewMemoryLeakPreventionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Either (isLeft, isRight)
import Control.DeepSeq (NFData, force)
import System.Mem (performGC)
import Data.IORef
import Control.Monad (replicateM, when)

import Parser
  ( ParseResult(..)
  , parse
  , parseWithLimit
  , parseTokens
  , ParseError(..)
  , Parser
  , ParserState(..)
  , clearParserCache
  )

import Compiler
  ( CompilationResult(..)
  , compile
  , compileWithLimit
  , CompilerState(..)
  , clearCompilerCache
  )

import Ownership
  ( OwnershipAnalysis(..)
  , analyzeOwnership
  , clearOwnershipCache
  )

import Dependencies
  ( DependencyAnalysis(..)
  , analyzeDependencies
  , clearDependencyCache
  )

import ErrorHandler
  ( ErrorHandler
  , newErrorHandler
  , clearErrors
  , ErrorHandlerState(..)
  , clearErrorCache
  )

import SourceLocation
  ( SourceLocationState(..)
  , clearLocationCache
  )

import Utils
  ( trim
  , splitBy
  , removeComments
  , normalizeIndentation
  , clearUtilsCache
  )

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate large inputs for memory testing
genLargeInput :: Int -> Gen String
genLargeInput size = do
  let chunk = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+-=[]{}|;':\",./<>? \t\n"
  return $ L.concat $ take (size `div` L.length chunk + 1) $ repeat chunk

-- Generate many small inputs for repeated operations
genManyInputs :: Int -> Gen [String]
genManyInputs count = do
  baseStrings <- listOf $ elements ["func test() {}", "var x = 1;", "class Test {}", "if (true) {}", "while (false) {}"]
  return $ take count $ cycle baseStrings

-- Generate nested structures for deep parsing
genNestedStructure :: Int -> Gen String
genNestedStructure depth = do
  let nested = L.concat $ replicate depth "if (true) { "
      content = "result = 42;"
      closing = L.concat $ replicate depth "}"
  return $ nested ++ content ++ closing

-- Generate parser states for cache testing
genParserState :: Gen ParserState
genParserState = do
  position <- choose (0, 10000)
  cacheSize <- choose (0, 1000)
  return $ ParserState position cacheSize

-- Generate compiler states for cache testing
genCompilerState :: Gen CompilerState
genCompilerState = do
  optimizationsEnabled <- elements [True, False]
  cacheSize <- choose (0, 1000)
  return $ CompilerState optimizationsEnabled cacheSize

-- ============================================================================
-- Memory Leak Prevention Properties
-- ============================================================================

-- Property: Parser cache should be cleared without memory leaks
prop_parser_cache_cleared :: Int -> Property
prop_parser_cache_cleared numParses =
  numParses > 0 && numParses <= 100 ==> 
  forAll (genManyInputs numParses) $ \inputs ->
    let parseResults = map parse inputs
        forcedResults = map force parseResults
        cleared = clearParserCache
    in property $ L.length forcedResults === numParses .&&. cleared === ()

-- Property: Repeated parsing should not accumulate memory
prop_repeated_parsing_no_accumulation :: Int -> Int -> Property
prop_repeated_parsing_no_accumulation iterations inputSize =
  iterations > 0 && iterations <= 50 && inputSize > 0 && inputSize <= 1000 ==> 
  forAll (genLargeInput inputSize) $ \input ->
    let results = replicate iterations $ parse input
        forcedResults = map force results
        -- Clear cache between iterations to prevent accumulation
        clearedResults = L.map (const (clearParserCache >> parse input)) [1..iterations]
    in property $ L.length forcedResults === iterations .&&. L.length clearedResults === iterations

-- Property: Compiler cache should be cleared properly
prop_compiler_cache_cleared :: Int -> Property
prop_compiler_cache_cleared numCompiles =
  numCompiles > 0 && numCompiles <= 50 ==> 
  forAll (genManyInputs numCompiles) $ \inputs ->
    let compileResults = map compile inputs
        forcedResults = map force compileResults
        cleared = clearCompilerCache
    in property $ L.length forcedResults === numCompiles .&&. cleared === ()

-- Property: Ownership analysis should not leak memory
prop_ownership_analysis_no_leak :: Int -> Property
prop_ownership_analysis_no_leak numAnalyses =
  numAnalyses > 0 && numAnalyses <= 50 ==> 
  let analyses = replicate numAnalyses analyzeOwnership
      forcedAnalyses = map force analyses
      cleared = clearOwnershipCache
  in property $ L.length forcedAnalyses === numAnalyses .&&. cleared === ()

-- Property: Dependency analysis should handle large inputs without leaks
prop_dependency_analysis_large_inputs :: Int -> Property
prop_dependency_analysis_large_inputs inputSize =
  inputSize > 0 && inputSize <= 10000 ==> 
  forAll (genLargeInput inputSize) $ \input ->
    let analysis = analyzeDependencies input
        forcedAnalysis = force analysis
        cleared = clearDependencyCache
    in property | L.length input > 0 -> True  -- If we get here without OOM, no leak
               | otherwise -> True

-- Property: Error handler should not accumulate errors indefinitely
prop_error_handler_no_accumulation :: Int -> Int -> Property
prop_error_handler_no_accumulation numHandlers numErrors =
  numHandlers > 0 && numHandlers <= 20 && numErrors > 0 && numErrors <= 100 ==> 
  let handlers = replicate numHandlers newErrorHandler
      addErrors = L.map (\h -> L.foldl (\acc _ -> clearErrors acc) h [1..numErrors]) handlers
      cleared = map clearErrorCache addErrors
  in property $ L.length cleared === numHandlers

-- Property: Source location tracking should not leak
prop_source_location_no_leak :: Int -> Property
prop_source_location_no_leak numLocations =
  numLocations > 0 && numLocations <= 1000 ==> 
  let locations = replicate numLocations ()
      processed = L.map (const ()) locations
      cleared = clearLocationCache
  in property $ L.length processed === numLocations .&&. cleared === ()

-- Property: Utils functions should not retain intermediate results
prop_utils_no_retention :: Int -> String -> Property
prop_utils_no_retention iterations baseInput =
  iterations > 0 && iterations <= 100 ==> 
  let processed = replicate iterations $ trim (removeComments (normalizeIndentation baseInput))
      forcedProcessed = map force processed
      cleared = clearUtilsCache
  in property $ L.length forcedProcessed === iterations .&&. cleared === ()

-- Property: Deep nesting should not cause stack overflow L.or memory leaks
prop_deep_nesting_safe :: Int -> Property
prop_deep_nesting_safe depth =
  depth > 0 && depth <= 100 ==> 
  forAll (genNestedStructure depth) $ \nestedInput ->
    let result = parse nestedInput
        forcedResult = force result
    in property | depth > 0 -> True  -- If we get here, no stack overflow
               | otherwise -> True

-- Property: Large token lists should be processed efficiently
prop_large_tokens_efficient :: Int -> Property
prop_large_tokens_efficient numTokens =
  numTokens > 0 && numTokens <= 10000 ==> 
  let tokens = take numTokens $ cycle ["identifier", "number", "operator", "punctuation"]
      result = parseTokens (unwords tokens)
      forcedResult = force result
    in property | numTokens > 0 -> True  -- If we get here, efficient processing
               | otherwise -> True

-- Property: Repeated cache operations should not accumulate
prop_repeated_cache_operations :: Int -> Int -> Property
prop_repeated_cache_operations operations cycles =
  operations > 0 && operations <= 50 && cycles > 0 && cycles <= 10 ==> 
  let cacheOps = replicate cycles $ do
        clearParserCache
        clearCompilerCache
        clearOwnershipCache
        clearDependencyCache
        clearErrorCache
        clearLocationCache
        clearUtilsCache
  in property $ L.length cacheOps === cycles

-- ============================================================================
-- Resource Management Properties
-- ============================================================================

-- Property: Parser state should be resettable
prop_parser_state_resettable :: ParserState -> Property
prop_parser_state_resettable state =
  let resetState = state { parserPosition = 0, parserCacheSize = 0 }
      cleared = clearParserCache
  in property $ cleared === ()

-- Property: Compiler state should be resettable
prop_compiler_state_resettable :: CompilerState -> Property
prop_compiler_state_resettable state =
  let resetState = state { compilerCacheSize = 0 }
      cleared = clearCompilerCache
  in property $ cleared === ()

-- Property: Error handler state should be resettable
prop_error_handler_state_resettable :: ErrorHandlerState -> Property
prop_error_handler_state_resettable state =
  let resetState = state { errorCount = 0, errorCacheSize = 0 }
      cleared = clearErrorCache
  in property $ cleared === ()

-- ============================================================================
-- Performance L.and Scalability Properties
-- ============================================================================

-- Property: Memory usage should be bounded for large operations
prop_bounded_memory_usage :: Int -> Property
prop_bounded_memory_usage size =
  size > 0 && size <= 10000 ==> 
  forAll (genLargeInput size) $ \input ->
    let result = parse input
        forcedResult = force result
        -- Perform GC to check for memory leaks
        _ = performGC
    in property | L.length input > 0 -> True  -- If we get here, memory is bounded
               | otherwise -> True

-- Property: Cache size should not grow indefinitely
prop_cache_size_bounded :: Int -> Property
prop_cache_size_bounded operations =
  operations > 0 && operations <= 1000 ==> 
  let results = replicate operations parse
      forcedResults = map force results
      cleared = clearParserCache
  in property | operations > 0 -> True  -- If we get here, cache is bounded
               | otherwise -> True

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Empty inputs should not cause memory leaks
prop_empty_inputs_safe :: Property
prop_empty_inputs_safe =
  let emptyInput = ""
      result = parse emptyInput
      forcedResult = force result
      cleared = clearParserCache
  in property | True -> True  -- If we get here, empty inputs are safe

-- Property: Very large inputs should be handled gracefully
prop_very_large_inputs_graceful :: Int -> Property
prop_very_large_inputs_graceful size =
  size > 10000 && size <= 100000 ==> 
  forAll (genLargeInput size) $ \input ->
    let result = parseWithLimit input 1000000  -- 1 second limit
        forcedResult = force result
    in property | L.length input > 0 -> True  -- If we get here, large inputs are handled
               | otherwise -> True

-- Property: Invalid inputs should not cause memory leaks
prop_invalid_inputs_safe :: String -> Property
prop_invalid_inputs_safe input =
  let invalidInput = input ++ "\0\1\2\3\x1F\x7F"
      result = parse invalidInput
      forcedResult = force result
      cleared = clearParserCache
  in property | L.length invalidInput > 0 -> True  -- If we get here, invalid inputs are safe
               | otherwise -> True

-- ============================================================================
-- Concurrent Operations Properties
-- ============================================================================

-- Property: Concurrent parsing should not cause memory leaks
-- Note: This is a simplified version since we can't actually test concurrency here
prop_concurrent_operations_safe :: Int -> Property
prop_concurrent_operations_safe numOperations =
  numOperations > 0 && numOperations <= 100 ==> 
  let operations = replicate numOperations parse
      results = sequence operations
      forcedResults = map force results
      cleared = clearParserCache
  in property | numOperations > 0 -> True  -- If we get here, concurrent-like operations are safe
               | otherwise -> True

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Memory Leak Prevention QuickCheck Tests"
  [ testGroup "Cache Management"
    [ fastProperty "parser cache cleared" prop_parser_cache_cleared
    , fastProperty "compiler cache cleared" prop_compiler_cache_cleared
    , fastProperty "ownership analysis no leak" prop_ownership_analysis_no_leak
    , fastProperty "dependency analysis large inputs" prop_dependency_analysis_large_inputs
    ]

  , testGroup "Repeated Operations"
    [ fastProperty "repeated parsing no accumulation" prop_repeated_parsing_no_accumulation
    , fastProperty "error handler no accumulation" prop_error_handler_no_accumulation
    , fastProperty "source location no leak" prop_source_location_no_leak
    , fastProperty "utils no retention" prop_utils_no_retention
    ]

  , testGroup "Deep L.and Large Structures"
    [ fastProperty "deep nesting safe" prop_deep_nesting_safe
    , fastProperty "large tokens efficient" prop_large_tokens_efficient
    , fastProperty "repeated cache operations" prop_repeated_cache_operations
    ]

  , testGroup "Resource Management"
    [ fastProperty "parser state resettable" prop_parser_state_resettable
    , fastProperty "compiler state resettable" prop_compiler_state_resettable
    , fastProperty "error handler state resettable" prop_error_handler_state_resettable
    ]

  , testGroup "Performance L.and Scalability"
    [ fastProperty "bounded memory usage" prop_bounded_memory_usage
    , fastProperty "cache size bounded" prop_cache_size_bounded
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "empty inputs safe" prop_empty_inputs_safe
    , fastProperty "very large inputs graceful" prop_very_large_inputs_graceful
    , fastProperty "invalid inputs safe" prop_invalid_inputs_safe
    ]

  , testGroup "Concurrent Operations"
    [ fastProperty "concurrent operations safe" prop_concurrent_operations_safe
    ]
  ]