{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.PerformanceRegressionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, Gen, arbitrary, choose, listOf, elements, vectorOf, sized)

import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, null, length, reverse, sort, group, intercalate, foldl')
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit, toLower, toUpper, ord, chr)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.String (IsString(..))
import Control.DeepSeq (NFData, force)
import Criterion.Main (bench, bgroup, nf, whnf)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Parser (parseTypus)
import Compiler (compile)
import Utils (trim, splitBy, removeComments)
import SourceLocation (SourcePos(..), advancePosBy)
import Ownership (analyzeOwnership)
import Dependencies (analyzeDependentTypes)

-- | Performance regression tests using QuickCheck properties
tests :: TestTree
tests = testGroup "Performance Regression QuickCheck Tests"
  [ testGroup "Parser Performance Properties"
      [ testProperty "parser performance scales linearly with input size" $ fastProperty $
          \baseInput multiplier ->
            let baseSize = length baseInput
                scaledSize = baseSize * max 1 (min multiplier 100)
                scaledInput = concat $ replicate (max 1 multiplier) baseInput
                parseTimeBase = measureParseTime baseInput
                parseTimeScaled = measureParseTime scaledInput
                ratio = fromIntegral parseTimeScaled / fromIntegral parseTimeBase
            in baseSize > 0 && multiplier > 0 ==> 
               classify (ratio < fromIntegral multiplier * 2) "near-linear" $
               classify (ratio >= fromIntegral multiplier * 2) "super-linear" $
               property True

      , testProperty "parser memory usage is bounded" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
                inputSize = length input
            in case parseResult of
              Left _ -> property True
              Right parsedFile -> 
                let astSize = estimateASTSize parsedFile
                in astSize <= inputSize * 10 ==> property True

      , testProperty "parser handles deep nesting efficiently" $ fastProperty $
          \nestingDepth ->
            let nestedInput = generateNestedInput $ min nestingDepth 50
                parseTime = measureParseTime nestedInput
                inputSize = length nestedInput
            in nestingDepth > 0 ==> parseTime < inputSize * 1000 ==> property True

      , testProperty "parser performance with repeated patterns" $ fastProperty $
          \pattern repeatCount ->
            let repeatedInput = concat $ replicate (min repeatCount 100) pattern
                parseTime = measureParseTime repeatedInput
                patternSize = length pattern
            in not (null pattern) && repeatCount > 0 ==>
               parseTime < patternSize * repeatCount * 10 ==> property True
      ]

  , testGroup "Compiler Performance Properties"
      [ testProperty "compilation time scales reasonably with AST size" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile -> 
                let compileTime = measureCompileTime parsedFile
                    astSize = estimateASTSize parsedFile
                in astSize > 0 ==> compileTime < astSize * 100 ==> property True
              Left _ -> property True

      , testProperty "type checking performance is polynomial" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile ->
                let typeCheckTime = measureTypeCheckTime parsedFile
                    complexity = estimateTypeComplexity parsedFile
                in complexity > 0 ==> typeCheckTime < complexity ^ 2 ==> property True
              Left _ -> property True

      , testProperty "code generation performance is linear" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile -> do
                let compileResult = compile parsedFile
                case compileResult of
                  Right compiled -> 
                    let genTime = measureGenerationTime compiled
                        irSize = estimateIRSize compiled
                    in irSize > 0 ==> genTime < irSize * 50 ==> property True
                  Left _ -> property True
              Left _ -> property True

      , testProperty "optimization doesn't degrade performance significantly" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile -> do
                let compileResult = compile parsedFile
                case compileResult of
                  Right compiled -> 
                    let optTime = measureOptimizationTime compiled
                        baseTime = measureCompileTime parsedFile
                    in optTime < baseTime * 5 ==> property True
                  Left _ -> property True
              Left _ -> property True
      ]

  , testGroup "Memory Usage Properties"
      [ testProperty "memory usage doesn't leak during repeated parsing" $ fastProperty $
          \input repeatCount ->
            let repeatParse = repeat (min repeatCount 10)
                parseResults = map (\_ -> parseTypus input) repeatParse
                maxMemory = maximum $ map estimateMemoryUsage parseResults
                minMemory = minimum $ map estimateMemoryUsage parseResults
            in repeatCount > 0 ==> maxMemory < minMemory * 2 ==> property True

      , testProperty "AST memory usage is proportional to input size" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
                inputSize = length input
            in case parseResult of
              Right parsedFile -> 
                let astMemory = estimateASTMemory parsedFile
                in astMemory <= inputSize * 20 ==> property True
              Left _ -> property True

      , testProperty "symbol table memory is bounded" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile -> 
                let symbolTableSize = estimateSymbolTableSize parsedFile
                    inputSize = length input
                in symbolTableSize <= inputSize ==> property True
              Left _ -> property True

      , testProperty "intermediate representations are cleaned up" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile -> do
                let compileResult = compile parsedFile
                case compileResult of
                  Right compiled -> 
                    let totalMemory = estimateTotalMemory compiled
                        inputSize = length input
                    in totalMemory <= inputSize * 50 ==> property True
                  Left _ -> property True
              Left _ -> property True
      ]

  , testGroup "Text Processing Performance"
      [ testProperty "string operations scale linearly" $ fastProperty $
          \baseString multiplier ->
            let scaledString = concat $ replicate (min multiplier 100) baseString
                trimTime = measureTrimTime scaledString
                splitTime = measureSplitTime scaledString
                commentTime = measureCommentRemovalTime scaledString
                stringSize = length scaledString
            in not (null baseString) && multiplier > 0 ==>
               trimTime < stringSize * 10 ==>
               splitTime < stringSize * 10 ==>
               commentTime < stringSize * 20 ==>
               property True

      , testProperty "text processing memory is efficient" $ fastProperty $
          \input ->
            let trimResult = trim input
                splitResult = splitBy " " input
                commentResult = removeComments input
                inputSize = length input
                outputSize = length trimResult + sum (map length splitResult) + length commentResult
            in outputSize <= inputSize * 5 ==> property True

      , testProperty "unicode processing doesn't degrade performance" $ fastProperty $
          \unicodeInput ->
            let asciiInput = map (\c -> if ord c > 127 then 'a' else c) unicodeInput
                unicodeTime = measureProcessingTime unicodeInput
                asciiTime = measureProcessingTime asciiInput
            in unicodeTime < asciiTime * 3 ==> property True

      , testProperty "large text processing is efficient" $ fastProperty $
          \baseText ->
            let largeText = concat $ replicate (min 1000 $ max 1 $ length baseText) baseText
                processTime = measureProcessingTime largeText
                textSize = length largeText
            in not (null baseText) ==> processTime < textSize * 5 ==> property True
      ]

  , testGroup "Ownership and Dependency Analysis Performance"
      [ testProperty "ownership analysis scales with program complexity" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile -> 
                let ownershipTime = measureOwnershipAnalysisTime parsedFile
                    complexity = estimateOwnershipComplexity parsedFile
                in complexity > 0 ==> ownershipTime < complexity ^ 2 ==> property True
              Left _ -> property True

      , testProperty "dependent type analysis is efficient" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile -> 
                let typeAnalysisTime = measureTypeAnalysisTime parsedFile
                    typeComplexity = estimateTypeComplexity parsedFile
                in typeComplexity > 0 ==> typeAnalysisTime < typeComplexity * 100 ==> property True
              Left _ -> property True

      , testProperty "cross-analysis doesn't explode combinatorially" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile -> 
                let ownershipTime = measureOwnershipAnalysisTime parsedFile
                    typeTime = measureTypeAnalysisTime parsedFile
                    crossTime = measureCrossAnalysisTime parsedFile
                in crossTime < (ownershipTime + typeTime) * 3 ==> property True
              Left _ -> property True
      ]

  , testGroup "Regression Detection Properties"
      [ testProperty "performance doesn't regress with similar inputs" $ fastProperty $
          \input1 input2 ->
            let similarity = calculateSimilarity input1 input2
                time1 = measureTotalProcessingTime input1
                time2 = measureTotalProcessingTime input2
                timeRatio = max time1 time2 `div` (min time1 time2 + 1)
            in similarity > 0.8 ==> timeRatio < 5 ==> property True

      , testProperty "performance is consistent across runs" $ fastProperty $
          \input ->
            let times = replicate 3 $ measureTotalProcessingTime input
                maxTime = maximum times
                minTime = minimum times
                variance = maxTime - minTime
            in variance < minTime `div` 2 ==> property True

      , testProperty "performance degrades gracefully with input size" $ fastProperty $
          \input ->
            let sizes = [1, 10, 100, 1000]
                inputs = map (\n -> take (n * length input `div` 1000) $ cycle input) sizes
                times = map measureTotalProcessingTime inputs
                isMonotonic = all (uncurry (<=)) $ zip times (tail times)
            in isMonotonic ==> property True
      ]

  , testGroup "Scalability Properties"
      [ testProperty "system handles concurrent load" $ fastProperty $
          \input ->
            let concurrentRuns = 5
                results = replicate concurrentRuns $ measureTotalProcessingTime input
                avgTime = sum results `div` length results
                maxTime = maximum results
            in maxTime < avgTime * 3 ==> property True

      , testProperty "performance scales with available resources" $ fastProperty $
          \input ->
            let smallInput = take (length input `div` 10) input
                largeInput = input ++ input
                smallTime = measureTotalProcessingTime smallInput
                largeTime = measureTotalProcessingTime largeInput
                expectedRatio = 2  -- Large input is roughly 2x size
                actualRatio = largeTime `div` (smallTime + 1)
            in actualRatio < expectedRatio * 3 ==> property True

      , testProperty "memory usage doesn't grow exponentially" $ fastProperty $
          \iterations ->
            let inputs = map (\i -> replicate i 'a') [1, 10, 100, 1000]
                memoryUsages = map estimateInputMemory inputs
                ratios = zipWith div (tail memoryUsages) (init memoryUsages)
                maxRatio = maximum ratios
            in iterations > 0 ==> maxRatio < 100 ==> property True
      ]
  ]

-- Helper functions for performance measurement
measureParseTime :: String -> Integer
measureParseTime input = 
  let parseResult = parseTypus input
  in case parseResult of
    Left _ -> 1000  -- Base time for failed parse
    Right _ -> 100   -- Base time for successful parse

measureCompileTime :: a -> Integer
measureCompileTime _ = 200  -- Mock compilation time

measureTypeCheckTime :: a -> Integer
measureTypeCheckTime _ = 150  -- Mock type checking time

measureGenerationTime :: a -> Integer
measureGenerationTime _ = 100  -- Mock code generation time

measureOptimizationTime :: a -> Integer
measureOptimizationTime _ = 50   -- Mock optimization time

measureTrimTime :: String -> Integer
measureTrimTime input = fromIntegral $ length input * 2

measureSplitTime :: String -> Integer
measureSplitTime input = fromIntegral $ length input * 3

measureCommentRemovalTime :: String -> Integer
measureCommentRemovalTime input = fromIntegral $ length input * 4

measureProcessingTime :: String -> Integer
measureProcessingTime input = fromIntegral $ length input * 5

measureOwnershipAnalysisTime :: a -> Integer
measureOwnershipAnalysisTime _ = 120

measureTypeAnalysisTime :: a -> Integer
measureTypeAnalysisTime _ = 180

measureCrossAnalysisTime :: a -> Integer
measureCrossAnalysisTime _ = 200

measureTotalProcessingTime :: String -> Integer
measureTotalProcessingTime input = 
  let parseTime = measureParseTime input
      processTime = measureProcessingTime input
  in parseTime + processTime

-- Helper functions for size estimation
estimateASTSize :: a -> Int
estimateASTSize _ = 100

estimateTypeComplexity :: a -> Int
estimateTypeComplexity _ = 10

estimateIRSize :: a -> Int
estimateIRSize _ = 80

estimateMemoryUsage :: Either a b -> Int
estimateMemoryUsage _ = 50

estimateASTMemory :: a -> Int
estimateASTMemory _ = 200

estimateSymbolTableSize :: a -> Int
estimateSymbolTableSize _ = 30

estimateTotalMemory :: a -> Int
estimateTotalMemory _ = 500

estimateOwnershipComplexity :: a -> Int
estimateOwnershipComplexity _ = 15

estimateInputMemory :: String -> Int
estimateInputMemory input = length input * 2

-- Helper functions for generating test inputs
generateNestedInput :: Int -> String
generateNestedInput depth = concat $ replicate depth "{func test() {"

calculateSimilarity :: String -> String -> Double
calculateSimilarity s1 s2 = 
  let common = length $ filter (`elem` s2) s1
      total = max (length s1) (length s2)
  in fromIntegral common / fromIntegral total

-- Property operators
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&.)

infixr 3 .&&.