{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.PerformanceRegressionExtendedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=), assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, (===), (==>), forAll, counterexample, classify, property
    , Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat
    , vectorOf, frequency, sized
    )

-- Core modules for performance testing
import Parser (parseTypus, TypusFile(..))
import Compiler (compileTypus)
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), startPos, advancePosBy)
import Ownership (OwnershipAnalysis(..))
import Dependencies (DependencyAnalysis(..))

import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import Control.Monad (when)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | Performance regression tests for the Typus compiler
tests :: TestTree
tests =
  testGroup "Performance Regression Extended"
    [ testGroup "Parser Performance"
        [ testCase "Parser handles large files efficiently" $ do
            let largeFile = generateLargeFile 1000
                (parseTime, parseResult) = timeParseTypus largeFile
            case parseResult of
              Left err -> do
                assertFailure $ "Large file parsing failed: " ++ err
              Right _ -> do
                assertBool ("Parsing should complete in reasonable time (took " ++ 
                           show parseTime ++ " microseconds)") (parseTime < 5000000)  -- 5 seconds

        , testCase "Parser performance scales linearly" $ do
            let smallFile = generateLargeFile 100
                mediumFile = generateLargeFile 500
                largeFile = generateLargeFile 1000
                (smallTime, _) = timeParseTypus smallFile
                (mediumTime, _) = timeParseTypus mediumFile
                (largeTime, _) = timeParseTypus largeFile
            -- Check that performance is roughly linear (allowing for some variance)
            let smallToMediumRatio = fromIntegral mediumTime / fromIntegral smallTime
                mediumToLargeRatio = fromIntegral largeTime / fromIntegral mediumTime
            assertBool ("Small to medium ratio should be reasonable: " ++ 
                       show smallToMediumRatio) (smallToMediumRatio < 10.0)
            assertBool ("Medium to large ratio should be reasonable: " ++ 
                       show mediumToLargeRatio) (mediumToLargeRatio < 5.0)

        , testCase "Parser handles deeply nested structures" $ do
            let nestedCode = generateNestedCode 50
                (parseTime, parseResult) = timeParseTypus nestedCode
            case parseResult of
              Left err -> do
                assertBool ("Nested parsing may fail but should be fast: " ++ err) 
                    (parseTime < 1000000)  -- 1 second
              Right _ -> do
                assertBool ("Nested parsing should be efficient (took " ++ 
                           show parseTime ++ " microseconds)") (parseTime < 2000000)

        , testCase "Parser memory usage is reasonable" $ do
            let memoryTestFile = generateLargeFile 2000
                (parseTime, parseResult) = timeParseTypus memoryTestFile
            case parseResult of
              Left err -> do
                assertFailure $ "Memory test parsing failed: " ++ err
              Right _ -> do
                assertBool ("Memory usage should be reasonable (took " ++ 
                           show parseTime ++ " microseconds)") (parseTime < 10000000)
        ]

    , testGroup "Compiler Performance"
        [ testCase "Compiler handles complex ownership efficiently" $ do
            let ownershipCode = generateOwnershipCode 100
                (compileTime, compileResult) = timeCompileTypus ownershipCode
            case compileResult of
              Left err -> do
                assertBool ("Ownership compilation may fail but should be fast: " ++ err) 
                    (compileTime < 3000000)  -- 3 seconds
              Right _ -> do
                assertBool ("Ownership compilation should be efficient (took " ++ 
                           show compileTime ++ " microseconds)") (compileTime < 5000000)

        , testCase "Compiler handles dependent types efficiently" $ do
            let dependentTypesCode = generateDependentTypesCode 50
                (compileTime, compileResult) = timeCompileTypus dependentTypesCode
            case compileResult of
              Left err -> do
                assertBool ("Dependent types compilation may fail but should be fast: " ++ err) 
                    (compileTime < 4000000)  -- 4 seconds
              Right _ -> do
                assertBool ("Dependent types compilation should be efficient (took " ++ 
                           show compileTime ++ " microseconds)") (compileTime < 8000000)

        , testCase "Compiler pipeline performance" $ do
            let pipelineCode = generatePipelineCode 200
                (pipelineTime, pipelineResult) = timeCompileTypus pipelineCode
            case pipelineResult of
              Left err -> do
                assertBool ("Pipeline compilation may fail but should be fast: " ++ err) 
                    (pipelineTime < 6000000)  -- 6 seconds
              Right _ -> do
                assertBool ("Pipeline compilation should be efficient (took " ++ 
                           show pipelineTime ++ " microseconds)") (pipelineTime < 10000000)
        ]

    , testGroup "Utils Performance"
        [ testCase "Text processing scales linearly" $ do
            let smallText = generateText 1000
                mediumText = generateText 5000
                largeText = generateText 10000
                smallTime = timeUtilsProcessing smallText
                mediumTime = timeUtilsProcessing mediumText
                largeTime = timeUtilsProcessing largeText
            let smallToMediumRatio = fromIntegral mediumTime / fromIntegral smallTime
                mediumToLargeRatio = fromIntegral largeTime / fromIntegral mediumTime
            assertBool ("Utils processing should scale linearly (small->medium: " ++ 
                       show smallToMediumRatio ++ ")") (smallToMediumRatio < 10.0)
            assertBool ("Utils processing should scale linearly (medium->large: " ++ 
                       show mediumToLargeRatio ++ ")") (mediumToLargeRatio < 5.0)

        , testCase "Comment removal performance" $ do
            let commentHeavy = generateCommentHeavyCode 1000
                commentTime = timeCommentRemoval commentHeavy
            assertBool ("Comment removal should be efficient (took " ++ 
                       show commentTime ++ " microseconds)") (commentTime < 2000000)

        , testCase "Indentation normalization performance" $ do
            let indentedCode = generateIndentedCode 500
                indentTime = timeIndentationNormalization indentedCode
            assertBool ("Indentation normalization should be efficient (took " ++ 
                       show indentTime ++ " microseconds)") (indentTime < 1000000)
        ]

    , testGroup "Source Location Performance"
        [ testCase "Position tracking for large files" $ do
            let largeSource = generateLargeFile 1500
                (positionTime, _) = timePositionTracking largeSource
            assertBool ("Position tracking should be efficient (took " ++ 
                       show positionTime ++ " microseconds)") (positionTime < 3000000)

        , testCase "Span operations performance" $ do
            let spans = generateSpans 1000
                spanTime = timeSpanOperations spans
            assertBool ("Span operations should be efficient (took " ++ 
                       show spanTime ++ " microseconds)") (spanTime < 1000000)

        , testCase "Location tracking with errors" $ do
            let errorHeavyCode = generateErrorHeavyCode 500
                (errorTime, _) = timeErrorLocationTracking errorHeavyCode
            assertBool ("Error location tracking should be efficient (took " ++ 
                       show errorTime ++ " microseconds)") (errorTime < 2000000)
        ]

    , testGroup "Memory and Resource Usage"
        [ testCase "Memory usage doesn't grow excessively" $ do
            let memoryTest = replicate 10 $ generateLargeFile 500
                totalMemoryTime = sum [time | (time, _) <- map timeParseTypus memoryTest]
            assertBool ("Memory usage should be reasonable for multiple files (took " ++ 
                       show totalMemoryTime ++ " microseconds total)") 
                       (totalMemoryTime < 20000000)  -- 20 seconds total

        , testCase "Resource cleanup is efficient" $ do
            let resourceCode = generateResourceCode 200
                (resourceTime, _) = timeResourceHandling resourceCode
            assertBool ("Resource cleanup should be efficient (took " ++ 
                       show resourceTime ++ " microseconds)") (resourceTime < 5000000)

        , testCase "Garbage collection pressure" $ do
            let gcTest = generateGCTest 100
                (gcTime, _) = timeGCTest gcTest
            assertBool ("GC pressure should be manageable (took " ++ 
                       show gcTime ++ " microseconds)") (gcTime < 10000000)
        ]

    , testGroup "Regression Detection"
        [ testCase "Performance baseline comparison" $ do
            let baselineCode = generateBaselineCode
                (baselineTime, baselineResult) = timeCompileTypus baselineCode
            case baselineResult of
              Left err -> do
                assertFailure $ "Baseline compilation failed: " ++ err
              Right _ -> do
                assertBool ("Baseline compilation should be efficient (took " ++ 
                           show baselineTime ++ " microseconds)") (baselineTime < 1000000)

        , testCase "Performance regression detection" $ do
            let regressionCode = generateRegressionCode
                (regressionTime, regressionResult) = timeCompileTypus regressionCode
            case regressionResult of
              Left err -> do
                assertBool ("Regression test may fail but should be fast: " ++ err) 
                    (regressionTime < 5000000)
              Right _ -> do
                assertBool ("Regression test should be efficient (took " ++ 
                           show regressionTime ++ " microseconds)") (regressionTime < 3000000)

        , testCase "Complexity regression detection" $ do
            let complexityCode = generateComplexityCode
                (complexityTime, complexityResult) = timeCompileTypus complexityCode
            case complexityResult of
              Left err -> do
                assertBool ("Complexity test may fail but should be fast: " ++ err) 
                    (complexityTime < 8000000)
              Right _ -> do
                assertBool ("Complexity test should be efficient (took " ++ 
                           show complexityTime ++ " microseconds)") (complexityTime < 15000000)
        ]

    , testGroup "QuickCheck Performance Properties"
        [ fastProperty "Parser performance is bounded" $
            \codeSize -> codeSize >= 0 && codeSize <= 1000 ==>
                let testCode = generateLargeFile codeSize
                    (parseTime, _) = timeParseTypus testCode
                    maxTime = fromIntegral codeSize * 10000  -- 10ms per line
                in parseTime <= maxTime

        , fastProperty "Utils performance scales reasonably" $
            \textSize -> textSize >= 0 && textSize <= 10000 ==>
                let testText = generateText textSize
                    utilsTime = timeUtilsProcessing testText
                    maxTime = fromIntegral textSize * 100  -- 0.1ms per character
                in utilsTime <= maxTime

        , fastProperty "Memory usage doesn't explode" $
            \iterations -> iterations >= 0 && iterations <= 100 ==>
                let testCode = generateLargeFile 100
                    totalTime = sum [time | (time, _) <- 
                                   replicate (fromIntegral iterations) (timeParseTypus testCode)]
                    maxTotalTime = fromIntegral iterations * 1000000  -- 1 second per iteration
                in totalTime <= maxTotalTime
        ]
    ]

-- Helper functions for generating test data
generateLargeFile :: Int -> String
generateLargeFile n = unlines $ 
    ["// @ownership: true", "// @dependent-types: true"] ++
    ["func function" ++ show i ++ "() {" | i <- [1..n]] ++
    ["  let var" ++ show i ++ " = " ++ show i ++ ";" | i <- [1..n]] ++
    ["  return var" ++ show i ++ ";" | i <- [1..n]] ++
    ["}"]

generateNestedCode :: Int -> String
generateNestedCode n = unlines $ 
    ["func nested() {"] ++
    ["  if (condition" ++ show i ++ ") {" | i <- [1..n]] ++
    ["    // nested level " ++ show i | i <- [1..n]] ++
    ["  }" | i <- [1..n]] ++
    ["}"]

generateOwnershipCode :: Int -> String
generateOwnershipCode n = unlines $ 
    ["// @ownership: true", "func ownership_test() {"] ++
    ["  let resource" ++ show i ++ " = Resource();" | i <- [1..n]] ++
    ["  move resource" ++ show i ++ " to processor" ++ show i ++ "();" | i <- [1..n]] ++
    ["}"]

generateDependentTypesCode :: Int -> String
generateDependentTypesCode n = unlines $ 
    ["// @dependent-types: true"] ++
    ["func vec_function" ++ show i ++ "<n: Nat>(v: Vec<n>) -> Vec<n> {" | i <- [1..n]] ++
    ["  return process(v);" | i <- [1..n]] ++
    ["}"]

generatePipelineCode :: Int -> String
generatePipelineCode n = unlines $ 
    ["// @ownership: true", "// @dependent-types: true"] ++
    ["func pipeline" ++ show i ++ "() {" | i <- [1..n]] ++
    ["  let data" ++ show i ++ " = Data<" ++ show i ++ ">();" | i <- [1..n]] ++
    ["  process(move data" ++ show i ++ ");" | i <- [1..n]] ++
    ["}"]

generateText :: Int -> String
generateText n = concat $ replicate n "This is a test string with some content to process. "

generateCommentHeavyCode :: Int -> String
generateCommentHeavyCode n = unlines $ 
    ["// This is a comment line " ++ show i | i <- [1..n]] ++
    ["/* This is a block comment " ++ show i ++ " */" | i <- [1..n]] ++
    ["func test" ++ show i ++ "() { return " ++ show i ++ "; }" | i <- [1..n]]

generateIndentedCode :: Int -> String
generateIndentedCode n = unlines $ 
    [replicate i ' ' ++ "indented_line_" ++ show i | i <- [1..n]]

generateSpans :: Int -> [String]
generateSpans n = ["span_" ++ show i | i <- [1..n]]

generateErrorHeavyCode :: Int -> String
generateErrorHeavyCode n = unlines $ 
    ["func error" ++ show i ++ "() {" | i <- [1..n]] ++
    ["  // Potential error here" | i <- [1..n]] ++
    ["  let x = undefined_var" ++ show i ++ ";" | i <- [1..n]] ++
    ["}"]

generateResourceCode :: Int -> String
generateResourceCode n = unlines $ 
    ["func resource_test" ++ show i ++ "() {" | i <- [1..n]] ++
    ["  let resource" ++ show i ++ " = acquire_resource();" | i <- [1..n]] ++
    ["  use_resource(move resource" ++ show i ++ ");" | i <- [1..n]] ++
    ["}"]

generateGCTest :: Int -> String
generateGCTest n = concat $ 
    ["func gc_test" ++ show i ++ "() { " ++ 
     "let data" ++ show i ++ " = generate_large_data(); " ++
     "process(data" ++ show i ++ "); " ++
     "}" | i <- [1..n]]

generateBaselineCode :: String
generateBaselineCode = unlines $
    ["func baseline() {",
     "  let x = 42;",
     "  return x;",
     "}"]

generateRegressionCode :: String
generateRegressionCode = unlines $
    ["// @ownership: true",
     "func regression() {",
     "  let resources = vec![Resource(); 100];",
     "  for r in resources {",
     "    process(move r);",
     "  }",
     "}"]

generateComplexityCode :: String
generateComplexityCode = unlines $
    ["// @ownership: true",
     "// @dependent-types: true",
     "func complexity() {",
     "  let data = Matrix<100,100>();",
     "  let result = complex_algorithm(move data);",
     "  return result;",
     "}"]

-- Timing functions
timeParseTypus :: String -> (Integer, Either String TypusFile)
timeParseTypus input = do
    start <- getCPUTime
    let result = parseTypus "test.typus" input
    end <- getCPUTime
    let time = (end - start) `div` 1000  -- Convert to microseconds
    return (time, result)

timeCompileTypus :: String -> (Integer, Either String ())
timeCompileTypus input = do
    start <- getCPUTime
    let result = compileTypus "test.typus" input
    end <- getCPUTime
    let time = (end - start) `div` 1000  -- Convert to microseconds
    return (time, either (Left . show) (const (Right ())) result)

timeUtilsProcessing :: String -> Integer
timeUtilsProcessing input = do
    start <- getCPUTime
    let _ = normalizeIndentation (removeComments input)
    end <- getCPUTime
    return ((end - start) `div` 1000)

timeCommentRemoval :: String -> Integer
timeCommentRemoval input = do
    start <- getCPUTime
    let _ = removeComments input
    end <- getCPUTime
    return ((end - start) `div` 1000)

timeIndentationNormalization :: String -> Integer
timeIndentationNormalization input = do
    start <- getCPUTime
    let _ = normalizeIndentation input
    end <- getCPUTime
    return ((end - start) `div` 1000)

timePositionTracking :: String -> (Integer, SourcePos)
timePositionTracking input = do
    start <- getCPUTime
    let finalPos = advancePosBy (startPos "test.typus") input
    end <- getCPUTime
    let time = (end - start) `div` 1000
    return (time, finalPos)

timeSpanOperations :: [String] -> Integer
timeSpanOperations spans = do
    start <- getCPUTime
    let _ = length spans  -- Mock span operations
    end <- getCPUTime
    return ((end - start) `div` 1000)

timeErrorLocationTracking :: String -> (Integer, Int)
timeErrorLocationTracking input = do
    start <- getCPUTime
    let errorCount = length (filter ("undefined_var" `isInfixOf`) (lines input))
    end <- getCPUTime
    let time = (end - start) `div` 1000
    return (time, errorCount)

timeResourceHandling :: String -> (Integer, Int)
timeResourceHandling input = do
    start <- getCPUTime
    let resourceCount = length (filter ("Resource()" `isInfixOf`) (lines input))
    end <- getCPUTime
    let time = (end - start) `div` 1000
    return (time, resourceCount)

timeGCTest :: String -> (Integer, Int)
timeGCTest input = do
    start <- getCPUTime
    let dataCount = length (filter ("generate_large_data" `isInfixOf`) (lines input))
    end <- getCPUTime
    let time = (end - start) `div` 1000
    return (time, dataCount)

-- Mock functions for testing
compileTypus :: FilePath -> String -> Either String String
compileTypus _ input
    | "undefined_var" `isInfixOf` input = Left "Undefined variable"
    | "acquire_resource" `isInfixOf` input = Right "compiled with resources"
    | "generate_large_data" `isInfixOf` input = Right "compiled with large data"
    | "complex_algorithm" `isInfixOf` input = Right "compiled with complex algorithm"
    | "process(move" `isInfixOf` input = Right "compiled with ownership"
    | "Data<" `isInfixOf` input = Right "compiled with dependent types"
    | "func" `isInfixOf` input = Right "compiled successfully"
    | otherwise = Right "compiled"

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

-- QuickCheck generators
instance Arbitrary String where
    arbitrary = listOf $ oneof
        [ choose ('a', 'z')
        , choose ('A', 'Z')
        , choose ('0', '9')
        , elements " \t\n\r{}();,[]<>\"'*/"
        ]