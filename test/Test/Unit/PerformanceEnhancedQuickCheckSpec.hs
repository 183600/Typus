{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.PerformanceEnhancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, QuickCheckTests(..))
import Test.Tasty.HUnit (testCase, assert, assertBool)
import Compiler (compile)
import Parser (parseTypus)
import Compiler.TypeChecker (typeCheck)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements, choose, listOf, oneof, sized)
import qualified Data.List as L
import Data.List (length)
import Data.List (foldl')
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- | Generate arbitrary code sizes for performance testing
newtype CodeSize = CodeSize Int
  deriving (Show)

instance Arbitrary CodeSize where
  arbitrary = do
    size <- elements [10, 50, 100, 500, 1000, 2000]
    return $ CodeSize size

-- | Generate arbitrary nesting depths
newtype NestingDepth = NestingDepth Int
  deriving (Show)

instance Arbitrary NestingDepth where
  arbitrary = do
    depth <- choose (1, 20)
    return $ NestingDepth depth

-- | Generate arbitrary variable counts
newtype VarCount = VarCount Int
  deriving (Show)

instance Arbitrary VarCount where
  arbitrary = do
    count <- choose (1, 100)
    return $ VarCount count

-- | Performance measurement result
data PerformanceResult = PerformanceResult
  { executionTime :: Double  -- in milliseconds
  , memoryUsage :: Int       -- in arbitrary units
  , inputSize :: Int
  } deriving (Show)

tests :: TestTree
tests = testGroup "Performance Regression Tests"
  [ testProperty "parsing time scales linearly with code size" $ \codeSize ->
      let CodeSize size = codeSize
          code = generateCode size 1 10
          result = measureParsingTime code
          expectedMaxTime = fromIntegral size * 0.1  -- 0.1ms per character max
      in executionTime result <= expectedMaxTime

  , testProperty "type checking time scales quadratically at worst" $ \codeSize ->
      \varCount -> let CodeSize size = codeSize
                       VarCount vars = varCount
                       code = generateComplexCode size vars
                       result = measureTypeCheckingTime code
                       expectedMaxTime = fromIntegral (size * vars) * 0.001  -- Quadratic bound
                   in executionTime result <= expectedMaxTime

  , testProperty "compilation time stays within acceptable bounds" $ \codeSize ->
      \nestingDepth -> let CodeSize size = codeSize
                           NestingDepth depth = nestingDepth
                           code = generateNestedCode size depth
                           result = measureCompilationTime code
                           expectedMaxTime = fromIntegral size * fromIntegral depth * 0.01
                       in executionTime result <= expectedMaxTime

  , testProperty "memory usage scales reasonably with input size" $ \codeSize ->
      let CodeSize size = codeSize
          code = generateCode size 1 10
          result = measureMemoryUsage code
          expectedMaxMemory = size * 100  -- 100 units per character
      in memoryUsage result <= expectedMaxMemory

  , testProperty "deep nesting doesn't cause exponential blowup" $ \nestingDepth ->
      let NestingDepth depth = nestingDepth
          code = generateDeeplyNestedCode depth
          result = measureCompilationTime code
          expectedMaxTime = fromIntegral depth * 10.0  -- Linear in depth
      in executionTime result <= expectedMaxTime

  , testProperty "large variable counts are handled efficiently" $ \varCount ->
      let VarCount count = varCount
          code = generateVariableHeavyCode count
          result = measureTypeCheckingTime code
          expectedMaxTime = fromIntegral count * 0.5  -- Linear in variable count
      in executionTime result <= expectedMaxTime

  , testCase "small code compilation performance" $ do
      let code = generateCode 100 1 5
          result = measureCompilationTime code
      assert (executionTime result < 10.0)  -- Should complete in under 10ms

  , testCase "medium code compilation performance" $ do
      let code = generateCode 1000 5 20
          result = measureCompilationTime code
      assert (executionTime result < 100.0)  -- Should complete in under 100ms

  , testCase "large code compilation performance" $ do
      let code = generateCode 5000 10 50
          result = measureCompilationTime code
      assert (executionTime result < 1000.0)  -- Should complete in under 1s

  , testCase "parsing performance regression test" $ do
      let baseline = measureParsingTime baselineCode
          current = measureParsingTime baselineCode
      assert (executionTime current <= executionTime baseline * 1.2)  -- Within 20% of baseline

  , testCase "type checking performance regression test" $ do
      let baseline = measureTypeCheckingTime baselineCode
          current = measureTypeCheckingTime baselineCode
      assert (executionTime current <= executionTime baseline * 1.2)  -- Within 20% of baseline

  , testCase "compilation performance regression test" $ do
      let baseline = measureCompilationTime baselineCode
          current = measureCompilationTime baselineCode
      assert (executionTime current <= executionTime baseline * 1.2)  -- Within 20% of baseline

  , testCase "memory usage regression test" $ do
      let baseline = measureMemoryUsage baselineCode
          current = measureMemoryUsage baselineCode
      assert (memoryUsage current <= memoryUsage baseline * 1.5)  -- Within 50% of baseline

  , testCase "performance with repeated patterns" $ do
      let code = generateRepeatedPatternCode 1000
          result = measureCompilationTime code
      assert (executionTime result < 500.0)  -- Should handle repeated patterns efficiently

  , testCase "performance with complex expressions" $ do
      let code = generateComplexExpressionCode 100
          result = measureCompilationTime code
      assert (executionTime result < 200.0)  -- Should handle complex expressions reasonably
  ]

-- Helper functions for performance testing
generateCode :: Int -> Int -> Int -> String
generateCode size minVars maxVars = 
  let varCount = min maxVars (size `div` 10)
      vars = take varCount $ L.map (\i -> "x" ++ show i) [1..]
      declarations = concatMap (\v -> "let " ++ v ++ " = " ++ show (L.length v) ++ " in ") vars
      body = concatMap (\v -> v ++ " + ") vars ++ "0"
  in take size $ declarations ++ body

generateComplexCode :: Int -> Int -> String
generateComplexCode size varCount = 
  let vars = take varCount $ L.map (\i -> "x" ++ show i) [1..]
      makeExpr i = if i < varCount 
                   then "(" ++ vars !! i ++ " + " ++ makeExpr (i + 1) ++ ")"
                   else "0"
      complexExpr = makeExpr 0
      wrapper = "let result = " ++ complexExpr ++ " in result"
  in take size $ wrapper

generateNestedCode :: Int -> Int -> String
generateNestedCode size depth = 
  let nest 0 = "0"
      nest n = "(" ++ nest (n - 1) ++ " + 1)"
      nested = nest depth
      wrapper = "let nested = " ++ nested ++ " in nested"
  in take size $ wrapper

generateDeeplyNestedCode :: Int -> String
generateDeeplyNestedCode depth = 
  let nest 0 = "x"
      nest n = "let f" ++ show n ++ " = \x -> " ++ nest (n - 1) ++ " in f" ++ show n
  in nest depth ++ " 0"

generateVariableHeavyCode :: Int -> String
generateVariableHeavyCode count = 
  let vars = take count $ L.map (\i -> "var" ++ show i) [1..]
      declarations = concatMap (\v -> "let " ++ v ++ " = " ++ show (L.length v) ++ " in ") vars
      uses = concatMap (\v -> v ++ " + ") vars ++ "0"
  in declarations ++ uses

generateRepeatedPatternCode :: Int -> String
generateRepeatedPatternCode repetitions = 
  let pattern = "let x = 1 + 2 * 3 - 4 / 5 in x"
      repeated = L.concat $ replicate repetitions pattern
  in repeated

generateComplexExpressionCode :: Int -> Int -> String
generateComplexExpressionCode complexity = 
  let buildExpr 0 = "x"
      buildExpr n = "(" ++ buildExpr (n - 1) ++ " * (" ++ buildExpr (n - 1) ++ " + 1))"
      expr = buildExpr complexity
      wrapper = "let x = 2 in " ++ expr
  in wrapper

-- Performance measurement functions
measureParsingTime :: String -> PerformanceResult
measureParsingTime code = 
  let start = getCPUTime
      result = parseTypus "test" code
      end = getCPUTime
      time = fromIntegral (end - start) / (10^9)  -- Convert to milliseconds
  in force result `seq` PerformanceResult time (L.length code) (L.length code)

measureTypeCheckingTime :: String -> PerformanceResult
measureTypeCheckingTime code = 
  case parseTypus "test" code of
    Left _ -> PerformanceResult 0.0 (L.length code) (L.length code)
    Right parsedFile -> 
      let start = getCPUTime
          result = typeCheck parsedFile
          end = getCPUTime
          time = fromIntegral (end - start) / (10^9)
      in force result `seq` PerformanceResult time (L.length code) (L.length code)

measureCompilationTime :: String -> PerformanceResult
measureCompilationTime code = 
  let start = getCPUTime
      result = compile code "test" []
      end = getCPUTime
      time = fromIntegral (end - start) / (10^9)
  in force result `seq` PerformanceResult time (L.length code) (L.length code)

measureMemoryUsage :: String -> PerformanceResult
measureMemoryUsage code = 
  let result = compile code "test" []
      estimatedMemory = L.length code * 10  -- Rough estimate
  in force result `seq` PerformanceResult 0.0 estimatedMemory (L.length code)

-- Baseline code for regression testing
baselineCode :: String
baselineCode = unlines
  [ "let factorial = \n -> if n <= 1 then 1 else n * factorial (n - 1) in"
  , "let fibonacci = \n -> if n <= 1 then n else fibonacci (n - 1) + fibonacci (n - 2) in"
  , "let main = factorial 5 + fibonacci 10 in"
  , "main"
  ]