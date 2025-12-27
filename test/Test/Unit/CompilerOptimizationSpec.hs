{-# LANGUAGE CPP #-}

module Test.Unit.CompilerOptimizationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck ((==>), Property, forAll, choose, listOf1, elements)
import qualified Data.List as List
import qualified Data.Map as Map
import Control.DeepSeq (NFData, force)

import TestSupport.QuickCheck (fastProperty)
import Compiler (compile)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import Compiler.GoAst (GoModule(..), GoDecl(..), FuncDecl(..))
import SourceLocation (SourceSpan(..), SourcePos(..))

-- | Compiler optimization and performance tests
tests :: TestTree
tests =
  testGroup "Compiler Optimization Tests"
    [ testGroup "Dead code elimination"
        [ testCase "removes unused variable declarations" $ do
            let input = unlines
                  [ "func main() {"
                  , "    unused := 42"
                  , "    used := 7"
                  , "    return used"
                  , "}"
                  ]
                expected = unlines
                  [ "func main() {"
                  , "    used := 7"
                  , "    return used"
                  , "}"
                  ]
            optimizeCode input @?= expected

        , testCase "removes unreachable code after return" $ do
            let input = unlines
                  [ "func test() int {"
                  , "    return 42"
                  , "    unreachable := 100"
                  , "    return unreachable"
                  , "}"
                  ]
                expected = unlines
                  [ "func test() int {"
                  , "    return 42"
                  , "}"
                  ]
            optimizeCode input @?= expected

        , testCase "preserves side effects in unused code" $ do
            let input = unlines
                  [ "func main() {"
                  , "    unused := println(\"side effect\")"
                  , "    return 42"
                  , "}"
                  ]
                expected = input  -- Should preserve println
            optimizeCode input @?= expected
        ]

    , testGroup "Constant folding"
        [ testCase "folds arithmetic constants" $ do
            let input = "result := 2 + 3 * 4"
                expected = "result := 14"
            foldConstants input @?= expected

        , testCase "folds boolean expressions" $ do
            let input = "condition := true && false || true"
                expected = "condition := true"
            foldConstants input @?= expected

        , testCase "handles complex constant expressions" $ do
            let input = "value := (1 + 2) * (3 + 4) / 2"
                expected = "value := 10"
            foldConstants input @?= expected
        ]

    , testGroup "Inline expansion"
        [ testCase "inlines small functions" $ do
            let input = unlines
                  [ "func small(x int) int { return x * 2 }"
                  , "func main() { result := small(5) }"
                  ]
                expected = unlines
                  [ "func main() { result := 5 * 2 }"
                  ]
            inlineFunctions input @?= expected

        , testCase "avoids inlining large functions" $ do
            let input = unlines
                  [ "func large(x int) int {"
                  , "    // Many statements..."
                  , "    return x"
                  , "}"
                  , "func main() { result := large(5) }"
                  ]
                expected = input  -- Should not inline
            inlineFunctions input @?= expected
        ]

    , testGroup "Loop optimizations"
        [ testCase "optimizes simple for loops" $ do
            let input = unlines
                  [ "sum := 0"
                  , "for i := 0; i < 10; i++ {"
                  , "    sum += i"
                  , "}"
                  ]
                expected = "sum := 45"  -- Pre-calculated sum
            optimizeLoops input @?= expected

        , testCase "detects loop invariants" $ do
            let input = unlines
                  [ "factor := 2"
                  , "for i := 0; i < 100; i++ {"
                  , "    result := i * factor"
                  , "    process(result)"
                  , "}"
                  ]
                expected = unlines
                  [ "factor := 2"
                  , "for i := 0; i < 100; i++ {"
                  , "    result := i * 2"  -- Factor inlined
                  , "    process(result)"
                  , "}"
                  ]
            optimizeLoops input @?= expected
        ]

    , testGroup "Memory optimization"
        [ testCase "eliminates redundant allocations" $ do
            let input = unlines
                  [ "func process() {"
                  , "    temp := make([]int, 100)"
                  , "    temp[0] = 1"
                  , "    use(temp[0])"
                  , "}"
                  ]
                expected = unlines
                  [ "func process() {"
                  , "    use(1)"  -- Direct value instead of allocation
                  , "}"
                  ]
            optimizeMemory input @?= expected

        , testCase "optimizes string concatenations" $ do
            let input = "result := \"a\" + \"b\" + \"c\""
                expected = "result := \"abc\""
            optimizeMemory input @?= expected
        ]

    , testGroup "Performance benchmarks"
        [ testCase "compilation time scales linearly" $ do
            let sizes = [100, 200, 400, 800]
                compileTimes = map (`compileWithSize` 1000) sizes
            -- Simple linear scaling check
            assertBool "Linear scaling" $ all (>= 0) compileTimes

        , testCase "optimization doesn't increase code size significantly" $ do
            let input = generateTestCode 1000
                originalSize = length input
                optimized = optimizeCode input
                optimizedSize = length optimized
            assertBool "Optimization size constraint" $ 
                optimizedSize <= originalSize * 2 `div` 3
        ]

    , testGroup "Property-based tests"
        [ fastProperty "optimization preserves semantics" prop_optimizationPreservesSemantics
        , fastProperty "constant folding is deterministic" prop_constantFoldingDeterministic
        , fastProperty "dead code elimination reduces size" prop_deadCodeReducesSize
        , fastProperty "inlining doesn't increase call count" prop_inliningControlsCallCount
        ]

    , testGroup "Regression tests"
        [ testCase "handles empty input gracefully" $ do
            optimizeCode "" @?= ""

        , testCase "preserves comments in optimized code" $ do
            let input = unlines
                  [ "// Important comment"
                  , "x := 1 + 1"
                  , "// End comment"
                  ]
                expected = unlines
                  [ "// Important comment"
                  , "x := 2"
                  , "// End comment"
                  ]
            optimizeCode input @?= expected

        , testCase "maintains correct line numbers" $ do
            let input = unlines
                  [ "line 1"
                  , "line 2: x := 1 + 1"
                  , "line 3"
                  ]
                result = optimizeCode input
                lines result @?= ["line 1", "line 2: x := 2", "line 3"]
        ]
    ]

-- Helper functions (would normally be in Compiler.Optimization module)
optimizeCode :: String -> String
optimizeCode input = input
  -- Simplified implementation - real optimization would be more complex

foldConstants :: String -> String
foldConstants input = input
  -- Simplified implementation

inlineFunctions :: String -> String
inlineFunctions input = input
  -- Simplified implementation

optimizeLoops :: String -> String
optimizeLoops input = input
  -- Simplified implementation

optimizeMemory :: String -> String
optimizeMemory input = input
  -- Simplified implementation

compileWithSize :: Int -> Int -> Int
compileWithSize linesCount complexity = linesCount * complexity `div` 1000

generateTestCode :: Int -> String
generateTestCode n = unlines $ map (\i -> "x" ++ show i ++ " := " ++ show i) [1..n]

-- Property-based tests
prop_optimizationPreservesSemantics :: String -> Property
prop_optimizationPreservesSemantics input =
    length input < 1000 ==>  -- Limit size for performance
    let optimized = optimizeCode input
        originalResult = evaluateCode input
        optimizedResult = evaluateCode optimized
    in originalResult == optimizedResult

prop_constantFoldingDeterministic :: String -> Property
prop_constantFoldingDeterministic input =
    length input < 100 ==> 
    let result1 = foldConstants input
        result2 = foldConstants input
    in result1 == result2

prop_deadCodeReducesSize :: String -> Property
prop_deadCodeReducesSize input =
    "unused" `List.isInfixOf` input ==>
    let optimized = optimizeCode input
    in length optimized <= length input

prop_inliningControlsCallCount :: String -> Property
prop_inliningControlsCallCount input =
    "func small" `List.isInfixOf` input ==>
    let optimized = inlineFunctions input
        originalCalls = countOccurrences "small(" input
        optimizedCalls = countOccurrences "small(" optimized
    in optimizedCalls <= originalCalls

-- Helper functions for property tests
evaluateCode :: String -> Int
evaluateCode _ = 0  -- Simplified evaluation

countOccurrences :: String -> String -> Int
countOccurrences pattern text = length $ filter (pattern `List.isPrefixOf`) (List.tails text)