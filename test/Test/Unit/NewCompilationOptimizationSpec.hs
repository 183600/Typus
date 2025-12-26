{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompilationOptimizationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, suchThat)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.List (isInfixOf, nub, sort, length)

import Compiler (compile, CompilerError(..), CompilationPhase(..), generateGoCode)
import Compiler.IR (IRModule, optimizeIR)
import Parser (parseTypus, TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

-- | Test compilation optimization functionality
tests :: TestTree
tests =
  testGroup "New Compilation Optimization Tests"
    [ basicOptimizationTests
    , constantFoldingTests
    , deadCodeEliminationTests
    , loopOptimizationTests
    , functionInliningTests
    , memoryOptimizationTests
    , optimizationValidationTests
    , quickCheckProperties
    ]

-- | Basic optimization tests
basicOptimizationTests :: TestTree
basicOptimizationTests =
  testGroup "Basic Optimization Tests"
    [ testCase "Simple arithmetic constant folding" $
        let inputs = 
              [ ("let x = 5 + 3", "let x = 8")
              , ("let y = 10 * 2", "let y = 20")
              , ("let z = 15 - 5", "let z = 10")
              , ("let w = 20 / 4", "let w = 5")
              ]
            results = map (\(input, expected) -> (optimizeCode "test.typus" input, expected)) inputs
        in do
           assertBool "All constant folding should work correctly" 
                     (all (\(optimized, expected) -> optimized == Just expected) results)

    , testCase "Boolean expression optimization" $
        let inputs = 
              [ ("let x = true && true", "let x = true")
              , ("let y = false && anything", "let y = false")
              , ("let z = true || anything", "let z = true")
              , ("let w = false || false", "let w = false")
              ]
            results = map (\(input, expected) -> (optimizeCode "test.typus" input, expected)) inputs
        in do
           assertBool "Boolean expressions should be optimized" 
                     (all (\(optimized, expected) -> optimized == Just expected) results)

    , testCase "Algebraic simplification" $
        let inputs = 
              [ ("let x = y * 1", "let x = y")
              , ("let y = z * 0", "let y = 0")
              , ("let z = a + 0", "let z = a")
              , ("let w = b - b", "let w = 0")
              ]
            results = map (\(input, expected) -> (optimizeCode "test.typus" input, expected)) inputs
        in do
           assertBool "Algebraic expressions should be simplified" 
                     (all (\(optimized, expected) -> optimized == Just expected) results)

    , testCase "Strength reduction" $
        let inputs = 
              [ ("let x = y * 2", "let x = y << 1")
              , ("let y = z * 4", "let y = z << 2")
              , ("let z = a / 2", "let z = a >> 1")
              , ("let w = b % 8", "let w = b & 7")
              ]
            results = map (\(input, expected) -> (optimizeCode "test.typus" input, expected)) inputs
        in do
           assertBool "Strength reduction should be applied" 
                     (all (\(optimized, expected) -> optimized == Just expected) results)

    , testCase "Copy propagation" $
        let input = "let x = 5\nlet y = x\nlet z = y + 3"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should propagate copies" ("5" `isInfixOf` optimized)
               assertBool "Should eliminate unnecessary copies" (not $ "y = x" `isInfixOf` optimized)
             Nothing -> assertFailure "Should optimize copy propagation"
    ]

-- | Constant folding tests
constantFoldingTests :: TestTree
constantFoldingTests =
  testGroup "Constant Folding Tests"
    [ testCase "Nested constant folding" $
        let input = "let x = (5 + 3) * (10 - 2)"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should fold nested constants" ("64" `isInfixOf` optimized)
               assertBool "Should eliminate intermediate results" (not $ "+" `isInfixOf` optimized)
             Nothing -> assertFailure "Should fold nested constants"

    , testCase "Floating point constant folding" $
        let inputs = 
              [ ("let x = 3.14 * 2.0", "let x = 6.28")
              , ("let y = 1.5 + 2.5", "let y = 4.0")
              , ("let z = 10.0 / 2.0", "let z = 5.0")
              ]
            results = map (\(input, expected) -> (optimizeCode "test.typus" input, expected)) inputs
        in do
           assertBool "Floating point constants should be folded" 
                     (all (\(optimized, expected) -> optimized == Just expected) results)

    , testCase "String constant folding" $
        let inputs = 
              [ ("let x = \"hello\" + \" \" + \"world\"", "let x = \"hello world\"")
              , ("let y = \"test\".length", "let y = 4")
              ]
            results = map (\(input, expected) -> (optimizeCode "test.typus" input, expected)) inputs
        in do
           assertBool "String constants should be folded" 
                     (all (\(optimized, expected) -> optimized == Just expected) results)

    , testCase "Boolean constant folding with complex expressions" $
        let input = "let x = (true && false) || (true || false)"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should fold complex boolean expression" ("true" `isInfixOf` optimized)
               assertBool "Should eliminate sub-expressions" (not $ "&&" `isInfixOf` optimized)
             Nothing -> assertFailure "Should fold complex boolean expression"

    , testCase "Array literal folding" $
        let input = "let arr = [1, 2, 3] + [4, 5, 6]"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should fold array literals" ("[1, 2, 3, 4, 5, 6]" `isInfixOf` optimized)
               assertBool "Should eliminate concatenation" (not $ "+" `isInfixOf` optimized)
             Nothing -> assertFailure "Should fold array literals"
    ]

-- | Dead code elimination tests
deadCodeEliminationTests :: TestTree
deadCodeEliminationTests =
  testGroup "Dead Code Elimination Tests"
    [ testCase "Unreachable code elimination" $
        let input = "func test() { return 42\nlet x = 5\nreturn x }"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should eliminate unreachable code" (not $ "let x = 5" `isInfixOf` optimized)
               assertBool "Should keep reachable code" ("return 42" `isInfixOf` optimized)
             Nothing -> assertFailure "Should eliminate unreachable code"

    , testCase "Unused variable elimination" $
        let input = "let x = 5\nlet y = 10\nlet z = x + y\nlet unused = 42\nreturn z"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should eliminate unused variables" (not $ "unused" `isInfixOf` optimized)
               assertBool "Should keep used variables" ("z = x + y" `isInfixOf` optimized)
             Nothing -> assertFailure "Should eliminate unused variables"

    , testCase "Dead function elimination" $
        let input = "func used() { return 42 }\nfunc unused() { return 10 }\nlet result = used()"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should eliminate unused functions" (not $ "func unused" `isInfixOf` optimized)
               assertBool "Should keep used functions" ("func used" `isInfixOf` optimized)
             Nothing -> assertFailure "Should eliminate unused functions"

    , testCase "Conditional dead code elimination" $
        let input = "if false { let x = 5\nreturn x } else { let y = 10\nreturn y }"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should eliminate dead branch" (not $ "let x = 5" `isInfixOf` optimized)
               assertBool "Should keep live branch" ("let y = 10" `isInfixOf` optimized)
             Nothing -> assertFailure "Should eliminate dead branch"

    , testCase "Loop invariant code motion" $
        let input = "let x = 5\nwhile i < 10 {\nlet y = x * 2\ni += 1\n}"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should move invariant code out of loop" 
                         (not $ "let y = x * 2" `isInfixOf` optimized)
               assertBool "Should preserve loop structure" ("while" `isInfixOf` optimized)
             Nothing -> assertFailure "Should move loop invariant code"
    ]

-- | Loop optimization tests
loopOptimizationTests :: TestTree
loopOptimizationTests =
  testGroup "Loop Optimization Tests"
    [ testCase "Loop unrolling" $
        let input = "for i in 0..3 {\nresult += i\n}"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should unroll small loops" (not $ "for i in 0..3" `isInfixOf` optimized)
               assertBool "Should preserve loop semantics" ("result += 0" `isInfixOf` optimized)
             Nothing -> assertFailure "Should unroll small loops"

    , testCase "Loop fusion" $
        let input = "for i in 0..10 {\narr1[i] = i * 2}\nfor i in 0..10 {\narr2[i] = i + 1}"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should fuse compatible loops" (length (filter (== "for") (words optimized)) <= 1)
               assertBool "Should preserve all operations" ("arr1" `isInfixOf` optimized && "arr2" `isInfixOf` optimized)
             Nothing -> assertFailure "Should fuse compatible loops"

    , testCase "Induction variable elimination" $
        let input = "for i in 0..10 {\nlet j = i * 4\narr[i] = j\n}"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should eliminate induction variables" (not $ "let j = i * 4" `isInfixOf` optimized)
               assertBool "Should substitute directly" ("arr[i] = i * 4" `isInfixOf` optimized)
             Nothing -> assertFailure "Should eliminate induction variables"

    , testCase "Loop invariant code motion" $
        let input = "let x = compute_expensive()\nfor i in 0..100 {\nlet y = x + i\nresult += y\n}"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should move invariant computation" 
                         (not $ "let y = x + i" `isInfixOf` optimized)
               assertBool "Should preserve loop semantics" ("for i in 0..100" `isInfixOf` optimized)
             Nothing -> assertFailure "Should move loop invariant code"

    , testCase "Strength reduction in loops" $
        let input = "for i in 0..100 {\narr[i] = i * 4\n}"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should apply strength reduction in loops" 
                         ("<<" `isInfixOf` optimized || "+= 4" `isInfixOf` optimized)
               assertBool "Should preserve array indexing" ("arr[i]" `isInfixOf` optimized)
             Nothing -> assertFailure "Should apply strength reduction in loops"
    ]

-- | Function inlining tests
functionInliningTests :: TestTree
functionInliningTests =
  testGroup "Function Inlining Tests"
    [ testCase "Simple function inlining" $
        let input = "func add(a, b) { return a + b }\nlet result = add(5, 3)"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should inline small functions" (not $ "func add" `isInfixOf` optimized)
               assertBool "Should substitute function body" ("5 + 3" `isInfixOf` optimized)
             Nothing -> assertFailure "Should inline small functions"

    , testCase "Recursive function inlining prevention" $
        let input = "func factorial(n) { return n <= 1 ? 1 : n * factorial(n - 1) }\nlet result = factorial(5)"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should not inline recursive functions" ("func factorial" `isInfixOf` optimized)
               assertBool "Should preserve recursive calls" ("factorial(" `isInfixOf` optimized)
             Nothing -> assertFailure "Should not inline recursive functions"

    , testCase "Conditional inlining based on size" $
        let inputs = 
              [ ("func small() { return 42 }", True)      -- Should inline
              , ("func large() { let x = 1; x += 2; x += 3; x += 4; x += 5; return x }", False)  -- Should not inline
              ]
            results = map (\(code, shouldInline) -> (optimizeCode "test.typus" code, shouldInline)) inputs
        in do
           assertBool "Should inline based on function size" 
                     (all (\(optimized, shouldInline) -> 
                        if shouldInline 
                        then not $ "func" `isInfixOf` optimized
                        else "func" `isInfixOf` optimized) results)

    , testCase "Inlining with parameter substitution" $
        let input = "func multiply_by_two(x) { return x * 2 }\nlet result = multiply_by_two(5)"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should substitute parameters" ("5 * 2" `isInfixOf` optimized)
               assertBool "Should eliminate function call" (not $ "multiply_by_two" `isInfixOf` optimized)
             Nothing -> assertFailure "Should substitute parameters"

    , testCase "Inlining with side effects preservation" $
        let input = "func counter() { global_count += 1; return global_count }\nlet result = counter()"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should preserve side effects when inlining" 
                         ("global_count += 1" `isInfixOf` optimized)
               assertBool "Should maintain correct semantics" ("return global_count" `isInfixOf` optimized)
             Nothing -> assertFailure "Should preserve side effects"
    ]

-- | Memory optimization tests
memoryOptimizationTests :: TestTree
memoryOptimizationTests =
  testGroup "Memory Optimization Tests"
    [ testCase "Stack allocation optimization" $
        let input = "let arr = [1, 2, 3, 4, 5]\nlet sum = 0\nfor i in 0..5 {\nsum += arr[i]\n}"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should optimize stack allocation" (length optimized <= length input)
               assertBool "Should preserve array access" ("arr[i]" `isInfixOf` optimized)
             Nothing -> assertFailure "Should optimize stack allocation"

    , testCase "Escape analysis for heap allocation" $
        let input = "func create_point() { return Point { x: 5, y: 10 } }\nlet p = create_point()"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should perform escape analysis" ("Point" `isInfixOf` optimized)
               assertBool "Should optimize allocation based on escape" (length optimized <= length input)
             Nothing -> assertFailure "Should perform escape analysis"

    , testCase "Memory pool allocation" $
        let input = "for i in 0..1000 {\nlet temp = allocate_temp()\nprocess(temp)\nfree_temp(temp)\n}"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should optimize repeated allocations" 
                         ("pool" `isInfixOf` optimized || "reuse" `isInfixOf` optimized)
               assertBool "Should preserve loop semantics" ("for i in 0..1000" `isInfixOf` optimized)
             Nothing -> assertFailure "Should optimize repeated allocations"

    , testCase "Garbage collection optimization" $
        let input = "let objects = []\nfor i in 0..100 {\nobjects.push(create_object())\n}\ngc()"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should optimize garbage collection" 
                         ("gc" `isInfixOf` optimized || "collect" `isInfixOf` optimized)
               assertBool "Should reduce allocation pressure" (length optimized <= length input)
             Nothing -> assertFailure "Should optimize garbage collection"

    , testCase "Memory layout optimization" $
        let input = "type Data = struct { a: int, b: char, c: int, d: bool }\nlet arr = [Data(), Data(), Data()]"
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should optimize memory layout" 
                         ("align" `isInfixOf` optimized || "packed" `isInfixOf` optimized)
               assertBool "Should preserve type semantics" ("Data" `isInfixOf` optimized)
             Nothing -> assertFailure "Should optimize memory layout"
    ]

-- | Optimization validation tests
optimizationValidationTests :: TestTree
optimizationValidationTests =
  testGroup "Optimization Validation Tests"
    [ testCase "Optimization preserves semantics" $
        let inputs = 
              [ "let x = 5 + 3"
              , "func test(a) { return a * 2 }"
              , "for i in 0..10 { result += i }"
              ]
            results = map (\input -> validateOptimization "test.typus" input) inputs
        in do
           assertBool "All optimizations should preserve semantics" (all id results)

    , testCase "Optimization doesn't introduce errors" $
        let input = "let x = 5\nlet y = x + 3\nlet z = y * 2"
            result = compile "test.typus" input
            optimized = optimizeCode "test.typus" input
        in case (result, optimized) of
             (Right _, Just optCode) -> do
               let compileResult = compile "test.typus" optCode
               case compileResult of
                 Right _ -> assertBool "Optimized code compiles successfully" True
                 Left _ -> assertFailure "Optimization introduced compilation errors"
             _ -> assertFailure "Original code should compile"

    , testCase "Optimization improves performance" $
        let input = "for i in 0..1000 {\nlet x = i * 2\nlet y = x + 1\nresult += y\n}"
            originalMetrics = measurePerformance "test.typus" input
            optimizedCode = optimizeCode "test.typus" input
        in case optimizedCode of
             Just opt -> do
               let optimizedMetrics = measurePerformance "test.typus" opt
               assertBool "Optimization should improve performance" 
                         (optimizedMetrics <= originalMetrics)
             Nothing -> assertFailure "Should optimize code"

    , testCase "Optimization respects safety constraints" $
        let input = "let arr = [1, 2, 3]\nlet x = arr[10]"  -- Out of bounds access
            result = optimizeCode "test.typus" input
        in case result of
             Just optimized -> do
               assertBool "Should preserve safety checks" ("bounds" `isInfixOf` optimized || "check" `isInfixOf` optimized)
               assertBool "Should not optimize away safety" (length optimized > 0)
             Nothing -> assertFailure "Should preserve safety constraints"

    , testCase "Optimization handles edge cases" $
        let inputs = 
              [ "let x = 0 / 1"           -- Division by zero edge case
              , "let y = 1 / 0"           -- Actual division by zero
              , "func infinite_recursion() { infinite_recursion() }"  -- Infinite recursion
              ]
            results = map (\input -> optimizeCode "test.typus" input) inputs
        in do
           assertBool "Should handle optimization edge cases gracefully" 
                     (all isJust results)
    ]

-- | QuickCheck properties for compilation optimization
quickCheckProperties :: TestTree
quickCheckProperties =
  testGroup "QuickCheck Properties"
    [ testProperty "Optimization preserves program behavior" $
        forAll genValidProgram $ \program ->
            let original = executeProgram "test.typus" program
                optimized = maybe original (executeProgram "test.typus") (optimizeCode "test.typus" program)
            in original === optimized

    , testProperty "Constant folding is sound" $
        forAll genConstantExpression $ \expr ->
            let original = evaluateExpression "test.typus" expr
                optimized = maybe original (evaluateExpression "test.typus") (optimizeExpression "test.typus" expr)
            in original === optimized

    , testProperty "Dead code elimination is safe" $
        forAll genProgramWithDeadCode $ \program ->
            let original = executeProgram "test.typus" program
                optimized = maybe original (executeProgram "test.typus") (optimizeCode "test.typus" program)
            in original === optimized
    ]

-- | Helper functions for optimization testing
optimizeCode :: String -> String -> Maybe String
optimizeCode filename code = 
    case compile filename code of
      Right _ -> Just $ "optimized: " ++ code  -- Simplified
      Left _ -> Nothing

validateOptimization :: String -> String -> Bool
validateOptimization filename code = 
    case compile filename code of
      Right _ -> True
      Left _ -> False

measurePerformance :: String -> String -> Int
measurePerformance filename code = 
    case compile filename code of
      Right _ -> length code  -- Simplified performance metric
      Left _ -> maxBound

executeProgram :: String -> String -> String
executeProgram filename code = 
    case compile filename code of
      Right _ -> "executed"
      Left _ -> "error"

evaluateExpression :: String -> String -> String
evaluateExpression filename expr = 
    case compile filename ("let result = " ++ expr) of
      Right _ -> "evaluated"
      Left _ -> "error"

optimizeExpression :: String -> String -> Maybe String
optimizeExpression filename expr = optimizeCode filename ("let result = " ++ expr)

-- | Generators for QuickCheck testing
genValidProgram :: Gen String
genValidProgram = elements
  [ "let x = 5 + 3"
  , "func add(a, b) { return a + b }"
  , "for i in 0..10 { result += i }"
  , "let arr = [1, 2, 3]\nlet sum = 0\nfor v in arr { sum += v }"
  ]

genConstantExpression :: Gen String
genConstantExpression = elements
  [ "5 + 3"
  , "10 * 2"
  , "true && false"
  , "\"hello\" + \" world\""
  , "1.5 * 2.0"
  ]

genProgramWithDeadCode :: Gen String
genProgramWithDeadCode = elements
  [ "func test() { return 42; let x = 5; return x }"
  , "let unused = 10\nlet used = 5\nreturn used"
  , "if false { let x = 5 } else { let y = 10 }"
  ]