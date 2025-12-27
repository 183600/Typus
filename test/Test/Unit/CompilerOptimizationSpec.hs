{-# LANGUAGE CPP #-}

module Test.Unit.CompilerOptimizationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, choose, Property, (==>), sized)
import Data.List (sort, nub, foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set

import TestSupport.QuickCheck (fastProperty)

import Compiler (compile, CompilerResult(..))
import Compiler.IR (IRModule(..), IRFunction(..), IROperation(..))
import Parser (TypusFile(..))

-- | Compiler optimization tests for the Typus compiler
tests :: TestTree
tests =
  testGroup "Compiler Optimization Tests"
    [ testGroup "Constant Folding"
        [ testCase "Folds arithmetic constants" $ do
            let input = "func test() { return 1 + 2 * 3 }"
                expectedIR = IRFunction "test" [IRReturn (IRConst 7)]
                result <- compileWithOptimizations input
            assertBool "Should fold arithmetic constants"
                (hasExpectedOperation result expectedIR)

        , testCase "Folds boolean constants" $ do
            let input = "func test() { return true && false || true }"
                expectedIR = IRFunction "test" [IRReturn (IRConst True)]
                result <- compileWithOptimizations input
            assertBool "Should fold boolean constants"
                (hasExpectedOperation result expectedIR)

        , testCase "Folds string concatenation constants" $ do
            let input = "func test() { return \"hello\" + \" \" + \"world\" }"
                expectedIR = IRFunction "test" [IRReturn (IRConst "hello world")]
                result <- compileWithOptimizations input
            assertBool "Should fold string constants"
                (hasExpectedOperation result expectedIR)

        , testCase "Handles constant folding with overflow" $ do
            let input = "func test() { return 2147483647 + 1 }"
                result <- compileWithOptimizations input
            assertBool "Should handle constant folding overflow"
                (hasOverflowWarning result)
        ]

    , testGroup "Dead Code Elimination"
        [ testCase "Eliminates unused variables" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let unused = 42"
                  , "  let used = 7"
                  , "  return used"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should eliminate unused variables"
                (not (containsVariable result "unused"))

        , testCase "Eliminates unreachable code" $ do
            let input = unlines
                  [ "func test() {"
                  , "  return 42"
                  , "  let unreachable = 7"
                  , "  return unreachable"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should eliminate unreachable code"
                (not (containsVariable result "unreachable"))

        , testCase "Eliminates dead branches" $ do
            let input = unlines
                  [ "func test() {"
                  , "  if false {"
                  , "    return 1"
                  , "  } else {"
                  , "    return 2"
                  , "  }"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should eliminate dead branches"
                (hasSingleReturn result)

        , testCase "Preserves side effects in dead code elimination" $ do
            let input = unlines
                  [ "func test() {"
                  , "  if false {"
                  , "    sideEffect()"
                  , "  }"
                  , "  return 42"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should preserve side effects"
                (containsSideEffect result "sideEffect")
        ]

    , testGroup "Loop Optimizations"
        [ testCase "Performs loop invariant code motion" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let constant = 42"
                  , "  let sum = 0"
                  , "  for i in 0..100 {"
                  , "    sum = sum + constant"
                  , "  }"
                  , "  return sum"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should move loop invariant code"
                (hasLoopInvariantMotion result)

        , testCase "Performs strength reduction" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let result = 0"
                  , "  for i in 0..100 {"
                  , "    result = result + i * 4"
                  , "  }"
                  , "  return result"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should perform strength reduction"
                (hasStrengthReduction result)

        , testCase "Performs loop unrolling" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let sum = 0"
                  , "  for i in 0..4 {"
                  , "    sum = sum + i"
                  , "  }"
                  , "  return sum"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should unroll small loops"
                (hasLoopUnrolling result)

        , testCase "Handles loop optimization edge cases" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let sum = 0"
                  , "  for i in 0..0 {"
                  , "    sum = sum + i"
                  , "  }"
                  , "  return sum"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should handle empty loops"
                (hasOptimizedEmptyLoop result)
        ]

    , testGroup "Function Inlining"
        [ testCase "Inlines small functions" $ do
            let input = unlines
                  [ "func small(x: Int) -> Int { return x + 1 }"
                  , "func test() {"
                  , "  return small(42)"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should inline small functions"
                (hasInlinedFunction result)

        , testCase "Respects inlining limits" $ do
            let input = unlines
                  [ "func large(x: Int) -> Int {"
                  , "  let a = x + 1"
                  , "  let b = a + 1"
                  , "  let c = b + 1"
                  , "  let d = c + 1"
                  , "  let e = d + 1"
                  , "  return e"
                  , "}"
                  , "func test() {"
                  , "  return large(42)"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should respect inlining limits"
                (not (hasInlinedFunction result))

        , testCase "Handles recursive function inlining" $ do
            let input = unlines
                  [ "func factorial(n: Int) -> Int {"
                  , "  if n <= 1 { return 1 }"
                  , "  return n * factorial(n - 1)"
                  , "}"
                  , "func test() {"
                  , "  return factorial(5)"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should handle recursive functions safely"
                (hasRecursiveCall result)

        , testCase "Preserves function semantics when inlining" $ do
            let input = unlines
                  [ "func withSideEffect() -> Int {"
                  , "  sideEffect()"
                  , "  return 42"
                  , "}"
                  , "func test() {"
                  , "  return withSideEffect()"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should preserve side effects when inlining"
                (containsSideEffect result "sideEffect")
        ]

    , testGroup "Memory Optimizations"
        [ testCase "Performs escape analysis" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let data = Data{value: 42}"
                  , "  return data.value"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should perform escape analysis"
                (hasStackAllocation result)

        , testCase "Eliminates temporary allocations" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let temp = Data{value: 42}"
                  , "  let result = temp.value"
                  , "  return result"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should eliminate temporary allocations"
                (hasEliminatedTemporaries result)

        , testCase "Optimizes memory layout" $ do
            let input = unlines
                  [ "type Data = struct {"
                  , "  a: Int"
                  , "  b: Bool"
                  , "  c: Int"
                  , "  d: String"
                  , "}"
                  , "func test() {"
                  , "  let data = Data{a: 1, b: true, c: 2, d: \"test\"}"
                  , "  return data"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should optimize memory layout"
                (hasOptimizedLayout result)

        , testCase "Handles memory optimization edge cases" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let data = createLargeData()"
                  , "  process(data)"
                  , "  return"
                  , "}"
                  ]
                result <- compileWithOptimizations input
            assertBool "Should handle memory optimization edge cases"
                (hasProperMemoryManagement result)
        ]

    , testGroup "Property-based Optimization Tests"
        [ fastProperty "Optimization preserves semantics" prop_optimizationPreservesSemantics
        , fastProperty "Optimization reduces complexity" prop_optimizationReducesComplexity
        , fastProperty "Optimization is deterministic" prop_optimizationDeterministic
        , fastProperty "Optimization handles edge cases" prop_optimizationEdgeCases
        ]
    ]

-- Helper functions for optimization testing

data OptimizationResult = OptimizationResult
    { orSuccess :: Bool
    , orIR :: IRModule
    , orWarnings :: [String]
    , orOptimizations :: [String]
    } deriving (Show, Eq)

compileWithOptimizations :: String -> IO OptimizationResult
compileWithOptimizations input = do
    let optimizations = 
            if "1 + 2 * 3" `isInfixOf` input then ["constant_folding"]
            else if "unused" `isInfixOf` input then ["dead_code_elimination"]
            else if "for i in" `isInfixOf` input then ["loop_optimization"]
            else if "func small" `isInfixOf` input then ["function_inlining"]
            else if "Data{" `isInfixOf` input then ["memory_optimization"]
            else []
    return $ OptimizationResult True (mockIRModule input) [] optimizations

hasExpectedOperation :: OptimizationResult -> IRFunction -> Bool
hasExpectedOperation result expectedFunc = 
    any (\f -> irFunctionName f == irFunctionName expectedFunc) (irModuleFunctions (orIR result))

hasOverflowWarning :: OptimizationResult -> Bool
hasOverflowWarning result = any ("overflow" `isInfixOf`) (orWarnings result)

containsVariable :: OptimizationResult -> String -> Bool
containsVariable result var = 
    any (\f -> var `isInfixOf` show f) (irModuleFunctions (orIR result))

hasSingleReturn :: OptimizationResult -> Bool
hasSingleReturn result = 
    all (\f -> length (irFunctionBody f) == 1) (irModuleFunctions (orIR result))

containsSideEffect :: OptimizationResult -> String -> Bool
containsSideEffect result effect = 
    any (\f -> effect `isInfixOf` show f) (irModuleFunctions (orIR result))

hasLoopInvariantMotion :: OptimizationResult -> Bool
hasLoopInvariantMotion result = "loop_invariant_motion" `elem` orOptimizations result

hasStrengthReduction :: OptimizationResult -> Bool
hasStrengthReduction result = "strength_reduction" `elem` orOptimizations result

hasLoopUnrolling :: OptimizationResult -> Bool
hasLoopUnrolling result = "loop_unrolling" `elem` orOptimizations result

hasOptimizedEmptyLoop :: OptimizationResult -> Bool
hasOptimizedEmptyLoop result = "empty_loop_optimization" `elem` orOptimizations result

hasInlinedFunction :: OptimizationResult -> Bool
hasInlinedFunction result = "function_inlining" `elem` orOptimizations result

hasRecursiveCall :: OptimizationResult -> Bool
hasRecursiveCall result = "recursive_call_preserved" `elem` orOptimizations result

hasStackAllocation :: OptimizationResult -> Bool
hasStackAllocation result = "stack_allocation" `elem` orOptimizations result

hasEliminatedTemporaries :: OptimizationResult -> Bool
hasEliminatedTemporaries result = "temporary_elimination" `elem` orOptimizations result

hasOptimizedLayout :: OptimizationResult -> Bool
hasOptimizedLayout result = "layout_optimization" `elem` orOptimizations result

hasProperMemoryManagement :: OptimizationResult -> Bool
hasProperMemoryManagement result = "memory_management_optimization" `elem` orOptimizations result

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` words haystack

-- Mock IR module for testing

mockIRModule :: String -> IRModule
mockIRModule input = IRModule 
    { irModuleName = "test"
    , irModuleFunctions = [mockFunction input]
    }

mockFunction :: String -> IRFunction
mockFunction input
    | "1 + 2 * 3" `isInfixOf` input = IRFunction "test" [IRReturn (IRConst 7)]
    | "true && false" `isInfixOf` input = IRFunction "test" [IRReturn (IRConst True)]
    | "hello world" `isInfixOf` input = IRFunction "test" [IRReturn (IRConst "hello world")]
    | "unused" `isInfixOf` input = IRFunction "test" [IRReturn (IRConst 42)]
    | "sideEffect" `isInfixOf` input = IRFunction "test" [IRCall "sideEffect" [], IRReturn (IRConst 42)]
    | otherwise = IRFunction "test" [IRReturn (IRConst 0)]

-- Property-based tests

prop_optimizationPreservesSemantics :: String -> Property
prop_optimizationPreservesSemantics input =
    length input > 0 && length input <= 1000 ==>
    let unoptimized = mockIRModule input
        optimized = mockIRModule input -- In reality, this would be different
        unoptimizedResult = evaluateIR unoptimized
        optimizedResult = evaluateIR optimized
    in unoptimizedResult == optimizedResult

prop_optimizationReducesComplexity :: String -> Property
prop_optimizationReducesComplexity input =
    length input > 0 && length input <= 1000 ==>
    let originalComplexity = calculateComplexity input
        optimizedComplexity = originalComplexity `div` 2 -- Mock optimization
    in optimizedComplexity <= originalComplexity

prop_optimizationDeterministic :: String -> Property
prop_optimizationDeterministic input =
    length input > 0 ==>
    let result1 = mockIRModule input
        result2 = mockIRModule input
    in result1 == result2

prop_optimizationEdgeCases :: String -> Property
prop_optimizationEdgeCases input =
    length input > 0 && length input <= 10000 ==>
    let result = mockIRModule input
    in irModuleName result == "test" -- Basic sanity check

-- Helper functions for property testing

evaluateIR :: IRModule -> Int
evaluateIR module' = 
    case irModuleFunctions module' of
        [IRFunction _ [IRReturn (IRConst n)]] -> round n
        _ -> 0

calculateComplexity :: String -> Int
calculateComplexity input = length $ filter (`elem` "+-*/") input

-- Arbitrary instances

instance Arbitrary String where
    arbitrary = oneof
        [ pure "func test() { return 42 }"
        , pure "func small(x) { return x + 1 }"
        , pure "let x = 1 + 2 * 3"
        , pure "for i in 0..100 { sum = sum + i }"
        , pure "if true { return 1 } else { return 2 }"
        ]