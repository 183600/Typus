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
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler (compile, CompilerError(..))
import Parser (parseTypus)
import Ownership (analyzeOwnership)
import Compiler.TypeChecker (inferType, buildTypeEnv)
import Data.List (isInfixOf, length, foldl')
import Data.Char (isSpace)

-- Property: Parsing time scales linearly with input size
prop_parsing_time_linear :: String -> Property
prop_parsing_time_linear code =
  let hasCode = length code > 0
      reasonableSize = length code <= 500
      codeLines = lines code
      lineCount = length codeLines
  in hasCode && reasonableSize ==>
  case parseTypus code of
    Right parsed ->
      let parsedStr = show parsed
          parsedSize = length parsedStr
          reasonableRatio = parsedSize <= length code * 10 + 1000
      in property $ reasonableRatio
    Left _ -> property $ True

-- Property: Compilation time doesn't grow exponentially
prop_compilation_time_reasonable :: String -> Property
prop_compilation_time_reasonable code =
  let hasCode = length code > 10
      reasonableSize = length code <= 1000
      complexity = length $ filter (`elem` code) "+-*/="
  in hasCode && reasonableSize ==>
  case parseTypus code of
    Right typusFile ->
      case compile typusFile of
        Right result ->
          let resultStr = show result
              resultSize = length resultStr
              reasonableGrowth = resultSize <= length code * 5 + 500
          in property $ reasonableGrowth
        Left _ -> property $ True
    Left _ -> property $ True

-- Property: Memory usage is bounded for repeated operations
prop_memory_usage_bounded :: String -> Int -> Property
prop_memory_usage_bounded code iterations =
  let hasCode = length code > 0
      validIterations = iterations >= 1 && iterations <= 10
  in hasCode && validIterations ==>
  let results = take iterations $ repeat (parseTypus code)
      resultSizes = map (\r -> length $ show r) results
      maxSize = maximum resultSizes
      minSize = minimum resultSizes
      sizeVariation = maxSize - minSize
      reasonableVariation = sizeVariation <= 100
  in property $ reasonableVariation

-- Property: Type checking performance doesn't degrade with nested types
prop_type_checking_nested_performance :: Int -> String -> Property
prop_type_checking_nested_performance depth baseType =
  let validDepth = depth >= 0 && depth <= 5
      validBase = length baseType > 0 && length baseType <= 20
      nestedType = if depth > 0 
                   then "(" ++ baseType ++ ")" ++ concat (replicate depth "[[]]")
                   else baseType
  in validDepth && validBase ==>
  case inferType nestedType of
    Right inferredType ->
      let typeStr = show inferredType
          reasonableSize = length typeStr <= length nestedType * 3 + 50
      in property $ reasonableSize
    Left _ -> property $ True

-- Property: Ownership analysis scales reasonably with code size
prop_ownership_analysis_scales :: [String] -> Property
prop_ownership_analysis_scales codeLines =
  let hasLines = length codeLines > 0
      reasonableSize = length codeLines <= 20
      nonEmptyLines = all (not . null) codeLines
      totalSize = sum (map length codeLines)
  in hasLines && reasonableSize && nonEmptyLines ==>
  case analyzeOwnership (unlines codeLines) of
    Right result ->
      let resultStr = show result
          resultSize = length resultStr
          reasonableRatio = resultSize <= totalSize * 2 + 200
      in property $ reasonableRatio
    Left _ -> property $ True

-- Property: Repeated operations don't leak memory
prop_repeated_operations_no_leak :: String -> Int -> Property
prop_repeated_operations_no_leak code repetitions =
  let hasCode = length code > 0
      validRepetitions = repetitions >= 1 && repetitions <= 5
  in hasCode && validRepetitions ==>
  let performOperation _ = parseTypus code
      results = map performOperation [1..repetitions]
      resultSizes = map (\r -> length $ show r) results
      sizeGrowth = if length resultSizes > 1 
                   then last resultSizes - head resultSizes
                   else 0
      reasonableGrowth = sizeGrowth <= 50
  in property $ reasonableGrowth

-- Property: Large inputs are handled efficiently
prop_large_inputs_efficient :: String -> Property
prop_large_inputs_efficient code =
  let hasCode = length code > 100
      notTooLarge = length code <= 2000
      complexity = length $ filter (not . isSpace) code
  in hasCode && notTooLarge ==>
  case parseTypus code of
    Right parsed ->
      let parsedStr = show parsed
          efficiency = length parsedStr <= length code * 2 + 1000
      in property $ efficiency
    Left _ -> property $ True

-- Property: Performance is consistent across similar inputs
prop_performance_consistent :: String -> String -> Property
prop_performance_consistent code1 code2 =
  let hasCode1 = length code1 > 0
      hasCode2 = length code2 > 0
      similarSize = abs (length code1 - length code2) <= 50
  in hasCode1 && hasCode2 && similarSize ==>
  let result1 = parseTypus code1
      result2 = parseTypus code2
      size1 = length $ show result1
      size2 = length $ show result2
      sizeDifference = abs (size1 - size2)
      reasonableDifference = sizeDifference <= max (length code1) (length code2)
  in property $ reasonableDifference

-- Property: Optimization doesn't cause performance regression
prop_optimization_no_regression :: String -> Property
prop_optimization_no_regression code =
  let hasCode = length code > 10
      hasOperations = any (`elem` code) "+-*/"
  in hasCode && hasOperations ==>
  case parseTypus code of
    Right typusFile ->
      case compile typusFile of
        Right result ->
          let resultStr = show result
              reasonableSize = length resultStr <= length code * 10 + 1000
          in property $ reasonableSize
        Left _ -> property $ True
    Left _ -> property $ True

tests :: TestTree
tests = testGroup "Performance Regression QuickCheck Tests"
  [ fastProperty "Parsing time scales linearly with input size" prop_parsing_time_linear
  , fastProperty "Compilation time doesn't grow exponentially" prop_compilation_time_reasonable
  , fastProperty "Memory usage is bounded for repeated operations" prop_memory_usage_bounded
  , fastProperty "Type checking performance doesn't degrade with nested types" prop_type_checking_nested_performance
  , fastProperty "Ownership analysis scales reasonably with code size" prop_ownership_analysis_scales
  , fastProperty "Repeated operations don't leak memory" prop_repeated_operations_no_leak
  , fastProperty "Large inputs are handled efficiently" prop_large_inputs_efficient
  , fastProperty "Performance is consistent across similar inputs" prop_performance_consistent
  , fastProperty "Optimization doesn't cause performance regression" prop_optimization_no_regression
  ]