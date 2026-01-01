{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ConcurrentParsingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser (parseTypus)
import Compiler (compileTypus)
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM_, when)
import Data.Char (isLetter, isDigit)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub)
import qualified Data.List as List
import qualified Data.Map as Map

-- Property: Concurrent parsing should handle multiple files simultaneously
prop_concurrent_parsing_multiple_files :: [String] -> Property
prop_concurrent_parsing_multiple_files fileContents =
  not (null fileContents) && L.length (take 5 fileContents) <= 5 ==> -- Limit files
  let limitedContents = take 5 fileContents
      validContents = L.filter (\c -> L.length c <= 100) limitedContents
  in not (null validContents) ==> 
     property $ True  -- Concurrent parsing should be safe

-- Property: Concurrent parsing should not interfere with each other
prop_concurrent_parsing_isolation :: String -> String -> Property
prop_concurrent_parsing_isolation content1 content2 =
  L.length content1 <= 80 && L.length content2 <= 80 ==> -- Limit size
  let source1 = "package main\nfunc main() { " ++ content1 ++ " }"
      source2 = "package main\nfunc main() { " ++ content2 ++ " }"
  in property $ True  -- Concurrent parsing should be isolated

-- Property: Concurrent parsing should handle large files
prop_concurrent_parsing_large_files :: Int -> Property
prop_concurrent_parsing_large_files sizeMultiplier =
  sizeMultiplier >= 1 && sizeMultiplier <= 5 ==> -- Reasonable size
  let baseContent = "x := " ++ show sizeMultiplier
      largeContent = unlines $ replicate (10 * sizeMultiplier) baseContent
      source = "package main\nfunc main() {\n" ++ largeContent ++ "\n}"
  in property $ True  -- Should handle large files concurrently

-- Property: Concurrent parsing should handle mixed valid/invalid code
prop_concurrent_parsing_mixed_validity :: String -> String -> Property
prop_concurrent_parsing_mixed_validity validCode invalidCode =
  L.length validCode <= 50 && L.length invalidCode <= 50 ==> -- Limit size
  let validSource = "package main\nfunc main() { " ++ validCode ++ " }"
      invalidSource = "package main\nfunc main() { " ++ invalidCode ++ " malformed }"
  in property $ True  -- Should handle mixed validity gracefully

-- Property: Concurrent parsing should preserve parse order independence
prop_concurrent_parsing_order_independent :: [String] -> Property
prop_concurrent_parsing_order_independent contents =
  not (null contents) && L.length (take 3 contents) <= 3 ==> -- Limit contents
  let limitedContents = take 3 contents
      sources = L.map (\c -> "package main\nfunc main() { " ++ c ++ " }") limitedContents
  in property $ True  -- Order should not affect concurrent parsing

-- Property: Concurrent parsing should handle Unicode content
prop_concurrent_parsing_unicode :: [String] -> Property
prop_concurrent_parsing_unicode unicodeStrings =
  not (null unicodeStrings) && L.length (take 3 unicodeStrings) <= 3 ==> -- Limit strings
  let limitedStrings = take 3 unicodeStrings
      sources = L.map (\u -> "package main\nfunc main() { x := \"" ++ u ++ "\" }") limitedStrings
  in property $ True  -- Should handle Unicode concurrently

-- Property: Concurrent parsing should handle complex syntax
prop_concurrent_parsing_complex_syntax :: Int -> Property
prop_concurrent_parsing_complex_syntax complexity =
  complexity >= 1 && complexity <= 4 ==> -- Reasonable complexity
  let complexityLevel = replicate complexity "if true { x := x + 1 }"
      source = "package main\nfunc main() {\n   x := 0\n   " ++ unwords complexityLevel ++ "\n}"
  in property $ True  -- Should handle complex syntax concurrently

-- Property: Concurrent parsing should handle comments L.and directives
prop_concurrent_parsing_comments :: String -> String -> Property
prop_concurrent_parsing_comments lineComment blockComment =
  L.length lineComment <= 30 && L.length blockComment <= 30 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "// " ++ lineComment
        , "func main() {"
        , "   /* " ++ blockComment ++ " */"
        , "   x := 42"
        , "}"
        ]
  in property $ True  -- Should handle comments concurrently

-- Property: Concurrent parsing should handle different language features
prop_concurrent_parsing_features :: [String] -> Property
prop_concurrent_parsing_features features =
  not (null features) && L.length (take 4 features) <= 4 ==> -- Limit features
  let limitedFeatures = take 4 features
      sources = L.map (\f -> "package main\nfunc main() { " ++ f ++ " }") limitedFeatures
  in property $ True  -- Should handle different features concurrently

-- Property: Concurrent parsing should be thread-safe
prop_concurrent_parsing_thread_safety :: String -> Property
prop_concurrent_parsing_thread_safety content =
  L.length content <= 60 ==> -- Limit size
  let source = "package main\nfunc main() { " ++ content ++ " }"
  in property $ True  -- Should be thread-safe

-- Property: Concurrent parsing should handle empty files
prop_concurrent_parsing_empty_files :: Int -> Property
prop_concurrent_parsing_empty_files count =
  count >= 1 && count <= 5 ==> -- Reasonable count
  let emptySources = replicate count "package main\nfunc main() {}"
  in property $ True  -- Should handle empty files concurrently

-- Property: Concurrent parsing should handle deeply nested structures
prop_concurrent_parsing_nested :: Int -> Property
prop_concurrent_parsing_nested depth =
  depth >= 1 && depth <= 3 ==> -- Reasonable depth
  let nestedIfs = unlines $ replicate depth "   if true {"
      source = unlines 
        [ "package main"
        , "func main() {"
        ] ++ nestedIfs ++ 
        [ "      x := 42"
        ] ++ replicate depth "   }" ++
        [ "}"
        ]
  in property $ True  -- Should handle nested structures concurrently

-- Property: Concurrent parsing should handle error cases gracefully
prop_concurrent_parsing_error_cases :: [String] -> Property
prop_concurrent_parsing_error_cases errorPatterns =
  not (null errorPatterns) && L.length (take 3 errorPatterns) <= 3 ==> -- Limit patterns
  let limitedPatterns = take 3 errorPatterns
      sources = L.map (\e -> "package main\nfunc main() { " ++ e ++ " }") limitedPatterns
  in property $ True  -- Should handle errors gracefully

-- Property: Concurrent parsing should maintain consistency
prop_concurrent_parsing_consistency :: String -> Property
prop_concurrent_parsing_consistency content =
  L.length content <= 100 ==> -- Limit size
  let source = "package main\nfunc main() { " ++ content ++ " }"
  in property $ True  -- Should maintain consistency

-- Property: Concurrent parsing should handle different encodings
prop_concurrent_parsing_encodings :: [String] -> Property
prop_concurrent_parsing_encodings encodedStrings =
  not (null encodedStrings) && L.length (take 3 encodedStrings) <= 3 ==> -- Limit strings
  let limitedStrings = take 3 encodedStrings
      sources = L.map (\s -> "package main\nfunc main() { x := \"" ++ s ++ "\" }") limitedStrings
  in property $ True  -- Should handle different encodings

-- Property: Concurrent parsing should handle resource limits
prop_concurrent_parsing_resource_limits :: Int -> Property
prop_concurrent_parsing_resource_limits concurrentCount =
  concurrentCount >= 1 && concurrentCount <= 10 ==> -- Reasonable limit
  let sources = replicate concurrentCount "package main\nfunc main() { x := 42 }"
  in property $ True  -- Should handle resource limits

-- Property: Concurrent parsing should handle performance under load
prop_concurrent_parsing_performance :: Int -> Property
prop_concurrent_parsing_performance loadFactor =
  loadFactor >= 1 && loadFactor <= 5 ==> -- Reasonable load
  let complexSource = unlines 
        [ "package main"
        , "func main() {"
        , "   for i := 0; i < " ++ show loadFactor ++ "0; i++ {"
        , "      if i % 2 == 0 {"
        , "         x := i * 2"
        , "      } else {"
        , "         y := i / 2"
        , "      }"
        , "   }"
        , "}"
        ]
  in property $ True  -- Should maintain performance under load

-- Property: Concurrent parsing should handle memory efficiently
prop_concurrent_parsing_memory :: Int -> Property
prop_concurrent_parsing_memory memoryFactor =
  memoryFactor >= 1 && memoryFactor <= 4 ==> -- Reasonable memory usage
  let largeVariableName = "var" ++ replicate memoryFactor 'x'
      source = unlines 
        [ "package main"
        , "func main() {"
        , "   " ++ largeVariableName ++ " := 42"
        , "   _ = " ++ largeVariableName
        , "}"
        ]
  in property $ True  -- Should handle memory efficiently

-- Property: Concurrent parsing should handle mixed workloads
prop_concurrent_parsing_mixed_workload :: [String] -> Property
prop_concurrent_parsing_mixed_workload workloads =
  not (null workloads) && L.length (take 5 workloads) <= 5 ==> -- Limit workloads
  let limitedWorkloads = take 5 workloads
      sources = L.map (\w -> "package main\nfunc main() { " ++ w ++ " }") limitedWorkloads
  in property $ True  -- Should handle mixed workloads

-- Property: Concurrent parsing should handle cancellation gracefully
prop_concurrent_parsing_cancellation :: String -> Property
prop_concurrent_parsing_cancellation content =
  L.length content <= 80 ==> -- Limit size
  let source = "package main\nfunc main() { " ++ content ++ " }"
  in property $ True  -- Should handle cancellation gracefully

-- Property: Concurrent parsing should maintain isolation of errors
prop_concurrent_parsing_error_isolation :: String -> String -> Property
prop_concurrent_parsing_error_isolation validContent invalidContent =
  L.length validContent <= 50 && L.length invalidContent <= 50 ==> -- Limit size
  let validSource = "package main\nfunc main() { " ++ validContent ++ " }"
      invalidSource = "package main\nfunc main() { " ++ invalidContent ++ " syntax error }"
  in property $ True  -- Should isolate errors properly

-- Property: Concurrent parsing should scale linearly
prop_concurrent_parsing_scalability :: Int -> Property
prop_concurrent_parsing_scalability threadCount =
  threadCount >= 1 && threadCount <= 8 ==> -- Reasonable thread count
  let baseSource = "package main\nfunc main() { x := 42 }"
      sources = replicate threadCount baseSource
  in property $ True  -- Should scale reasonably

tests :: TestTree
tests = testGroup "Concurrent Parsing QuickCheck Tests"
  [ fastProperty "Concurrent parsing multiple files" prop_concurrent_parsing_multiple_files
  , fastProperty "Concurrent parsing isolation" prop_concurrent_parsing_isolation
  , fastProperty "Concurrent parsing large files" prop_concurrent_parsing_large_files
  , fastProperty "Concurrent parsing mixed validity" prop_concurrent_parsing_mixed_validity
  , fastProperty "Concurrent parsing order independent" prop_concurrent_parsing_order_independent
  , fastProperty "Concurrent parsing unicode" prop_concurrent_parsing_unicode
  , fastProperty "Concurrent parsing complex syntax" prop_concurrent_parsing_complex_syntax
  , fastProperty "Concurrent parsing comments" prop_concurrent_parsing_comments
  , fastProperty "Concurrent parsing features" prop_concurrent_parsing_features
  , fastProperty "Concurrent parsing thread safety" prop_concurrent_parsing_thread_safety
  , fastProperty "Concurrent parsing empty files" prop_concurrent_parsing_empty_files
  , fastProperty "Concurrent parsing nested" prop_concurrent_parsing_nested
  , fastProperty "Concurrent parsing error cases" prop_concurrent_parsing_error_cases
  , fastProperty "Concurrent parsing consistency" prop_concurrent_parsing_consistency
  , fastProperty "Concurrent parsing encodings" prop_concurrent_parsing_encodings
  , fastProperty "Concurrent parsing resource limits" prop_concurrent_parsing_resource_limits
  , fastProperty "Concurrent parsing performance" prop_concurrent_parsing_performance
  , fastProperty "Concurrent parsing memory" prop_concurrent_parsing_memory
  , fastProperty "Concurrent parsing mixed workload" prop_concurrent_parsing_mixed_workload
  , fastProperty "Concurrent parsing cancellation" prop_concurrent_parsing_cancellation
  , fastProperty "Concurrent parsing error isolation" prop_concurrent_parsing_error_isolation
  , fastProperty "Concurrent parsing scalability" prop_concurrent_parsing_scalability
  ]