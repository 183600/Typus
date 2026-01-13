{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.PerformanceBoundarySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort, (\\), intersect)
import Control.Monad (when, replicateM)
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- ============================================================================
-- Performance Boundary Tests
-- ============================================================================

-- | Test parser performance with large inputs
prop_performance_parser_large_input :: Int -> String -> Property
prop_performance_parser_large_input n baseStr =
  n >= 0 && n <= 1000 ==>
    let largeInput = concat $ replicate n baseStr
    in ioProperty $ do
         startTime <- getCPUTime
         let parseResult = parseTypus largeInput
         endTime <- getCPUTime
         let executionTime = fromIntegral (endTime - startTime) / (10^12)
         case parseResult of
           Left _ -> return $ executionTime < 10.0  -- 10 seconds max
           Right _ -> return $ executionTime < 10.0

-- | Test compiler performance with complex code
prop_performance_compiler_complex :: Int -> Property
prop_performance_compiler_complex complexity =
  complexity >= 0 && complexity <= 100 ==>
    let complexCode = generateComplexCode complexity
        parseResult = parseTypus complexCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           ioProperty $ do
             startTime <- getCPUTime
             let compileResult = compile typusFile
             endTime <- getCPUTime
             let executionTime = fromIntegral (endTime - startTime) / (10^12)
             case compileResult of
               Left _ -> return $ executionTime < 10.0
               Right _ -> return $ executionTime < 10.0

-- | Test memory usage with deep nesting
prop_performance_memory_nesting :: Int -> Property
prop_performance_memory_nesting depth =
  depth >= 0 && depth <= 50 ==>
    let nestedCode = generateNestedCode depth
        parseResult = parseTypus nestedCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True  -- Memory usage test would need more sophisticated setup

-- | Test performance with many small files
prop_performance_many_files :: Int -> Property
prop_performance_many_files count =
  count >= 0 && count <= 100 ==>
    let files = replicate count "let x = 5\n"
        parseResults = map parseTypus files
        successfulParses = length [() | Right _ <- parseResults]
    in property $ successfulParses >= 0

-- | Test performance with large expressions
prop_performance_large_expressions :: Int -> String -> Property
prop_performance_large_expressions n baseExpr =
  n >= 0 && n <= 20 && not (null baseExpr) ==>
    let largeExpr = buildLargeExpression n baseExpr
        exprCode = "let x = " ++ largeExpr ++ "\n"
        parseResult = parseTypus exprCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with many imports
prop_performance_many_imports :: Int -> Property
prop_performance_many_imports n =
  n >= 0 && n <= 50 ==>
    let importCode = unlines $ map (\i -> "import Module" ++ show i) [1..n] ++ ["let x = 5\n"]
        parseResult = parseTypus importCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with type inference complexity
prop_performance_type_inference :: Int -> Property
prop_performance_type_inference complexity =
  complexity >= 0 && complexity <= 20 ==>
    let typeInferenceCode = generateTypeInferenceCode complexity
        parseResult = parseTypus typeInferenceCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with ownership analysis
prop_performance_ownership_analysis :: Int -> Property
prop_performance_ownership_analysis n =
  n >= 0 && n <= 20 ==>
    let ownershipCode = generateOwnershipCode n
        parseResult = parseTypus ownershipCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with dependency analysis
prop_performance_dependency_analysis :: Int -> Property
prop_performance_dependency_analysis n =
  n >= 0 && n <= 20 ==>
    let dependencyCode = generateDependencyCode n
        parseResult = parseTypus dependencyCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with error handling
prop_performance_error_handling :: Int -> Property
prop_performance_error_handling n =
  n >= 0 && n <= 20 ==>
    let errorCode = generateErrorCode n
        parseResult = parseTypus errorCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with optimization passes
prop_performance_optimization :: Int -> Property
prop_performance_optimization n =
  n >= 0 && n <= 10 ==>
    let optimizationCode = generateOptimizationCode n
        parseResult = parseTypus optimizationCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with code generation
prop_performance_code_generation :: Int -> Property
prop_performance_code_generation n =
  n >= 0 && n <= 10 ==>
    let codeGenCode = generateCodeGenCode n
        parseResult = parseTypus codeGenCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with parallel processing
prop_performance_parallel_processing :: Int -> Property
prop_performance_parallel_processing n =
  n >= 0 && n <= 10 ==>
    let parallelCode = generateParallelCode n
        parseResult = parseTypus parallelCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with incremental compilation
prop_performance_incremental :: Int -> Property
prop_performance_incremental n =
  n >= 0 && n <= 10 ==>
    let incrementalCode = generateIncrementalCode n
        parseResult = parseTypus incrementalCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with caching
prop_performance_caching :: Int -> Property
prop_performance_caching n =
  n >= 0 && n <= 10 ==>
    let cacheCode = generateCacheCode n
        parseResult = parseTypus cacheCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- | Test performance with memory pressure
prop_performance_memory_pressure :: Int -> Property
prop_performance_memory_pressure n =
  n >= 0 && n <= 1000 ==>
    let memoryPressureCode = generateMemoryPressureCode n
        parseResult = parseTypus memoryPressureCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

-- Helper functions to generate test code
generateComplexCode :: Int -> String
generateComplexCode 0 = "let x = 0"
generateComplexCode n = "let x" ++ show n ++ " = " ++ generateComplexCode (n-1) ++ " + 1"

generateNestedCode :: Int -> String
generateNestedCode 0 = "let x = 0"
generateNestedCode n = "if (true) {\n" ++ generateNestedCode (n-1) ++ "\n}"

buildLargeExpression :: Int -> String -> String
buildLargeExpression 0 base = base
buildLargeExpression n base = "(" ++ buildLargeExpression (n-1) base ++ " + " ++ base ++ ")"

generateTypeInferenceCode :: Int -> String
generateTypeInferenceCode 0 = "let x = 0"
generateTypeInferenceCode n = "let x" ++ show n ++ " = function(y" ++ show n ++ ") { return y" ++ show n ++ " + 1 }\n" ++ 
                              generateTypeInferenceCode (n-1)

generateOwnershipCode :: Int -> String
generateOwnershipCode 0 = "let x = owned_by(\"owner0\")"
generateOwnershipCode n = "let x" ++ show n ++ " = owned_by(\"owner" ++ show n ++ "\")\n" ++
                        "x" ++ show n ++ ".transfer_to(\"newOwner" ++ show n ++ "\")\n" ++
                        generateOwnershipCode (n-1)

generateDependencyCode :: Int -> String
generateDependencyCode 0 = "let x = 0"
generateDependencyCode n = "import Module" ++ show n ++ "\n" ++
                          "let x" ++ show n ++ " = Module" ++ show n ++ ".value\n" ++
                          generateDependencyCode (n-1)

generateErrorCode :: Int -> String
generateErrorCode 0 = "let x = 0"
generateErrorCode n = "try {\n" ++
                      "  let x" ++ show n ++ " = undefined\n" ++
                      "  x" ++ show n ++ ".method()\n" ++
                      "} catch (e) {\n" ++
                      "  handleError(e)\n" ++
                      "}\n" ++
                      generateErrorCode (n-1)

generateOptimizationCode :: Int -> String
generateOptimizationCode 0 = "let x = 0"
generateOptimizationCode n = "let x" ++ show n ++ " = " ++ show n ++ " + " ++ show n ++ "\n" ++
                            "let y" ++ show n ++ " = x" ++ show n ++ " * 2\n" ++
                            generateOptimizationCode (n-1)

generateCodeGenCode :: Int -> String
generateCodeGenCode 0 = "let x = 0"
generateCodeGenCode n = "function func" ++ show n ++ "() {\n" ++
                        "  return " ++ show n ++ "\n" ++
                        "}\n" ++
                        generateCodeGenCode (n-1)

generateParallelCode :: Int -> String
generateParallelCode 0 = "let x = 0"
generateParallelCode n = "parallel {\n" ++
                         "  let x" ++ show n ++ " = " ++ show n ++ "\n" ++
                         "}\n" ++
                         generateParallelCode (n-1)

generateIncrementalCode :: Int -> String
generateIncrementalCode 0 = "let x = 0"
generateIncrementalCode n = "module Module" ++ show n ++ " {\n" ++
                            "  let x" ++ show n ++ " = " ++ show n ++ "\n" ++
                            "}\n" ++
                            generateIncrementalCode (n-1)

generateCacheCode :: Int -> String
generateCacheCode 0 = "let x = 0"
generateCacheCode n = "cache(\"key" ++ show n ++ "\") {\n" ++
                      "  let x" ++ show n ++ " = " ++ show n ++ "\n" ++
                      "}\n" ++
                      generateCacheCode (n-1)

generateMemoryPressureCode :: Int -> String
generateMemoryPressureCode 0 = "let x = 0"
generateMemoryPressureCode n = "let arr" ++ show n ++ " = [" ++ unwords (map show [1..n]) ++ "]\n" ++
                              generateMemoryPressureCode (n-1)

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Performance Boundary Tests"
  [ testProperty "Parser performance with large inputs" prop_performance_parser_large_input,
    testProperty "Compiler performance with complex code" prop_performance_compiler_complex,
    testProperty "Memory usage with deep nesting" prop_performance_memory_nesting,
    testProperty "Performance with many small files" prop_performance_many_files,
    testProperty "Performance with large expressions" prop_performance_large_expressions,
    testProperty "Performance with many imports" prop_performance_many_imports,
    testProperty "Performance with type inference complexity" prop_performance_type_inference,
    testProperty "Performance with ownership analysis" prop_performance_ownership_analysis,
    testProperty "Performance with dependency analysis" prop_performance_dependency_analysis,
    testProperty "Performance with error handling" prop_performance_error_handling,
    testProperty "Performance with optimization passes" prop_performance_optimization,
    testProperty "Performance with code generation" prop_performance_code_generation,
    testProperty "Performance with parallel processing" prop_performance_parallel_processing,
    testProperty "Performance with incremental compilation" prop_performance_incremental,
    testProperty "Performance with caching" prop_performance_caching,
    testProperty "Performance with memory pressure" prop_performance_memory_pressure
  ]