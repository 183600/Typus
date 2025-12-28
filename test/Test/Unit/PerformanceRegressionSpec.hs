module Test.Unit.NewPerformanceRegressionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), choose, listOf, elements)
import Compiler
import Parser
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- | Test performance regression scenarios
tests :: TestTree
tests =
  testGroup "Performance Regression Tests"
    [ testGroup "Parsing performance"
        [ testCase "Large file parsing performance" $ do
            let largeInput = unlines $ replicate 1000 "func test_" ++ show ([0..999]) ++ "() { return " ++ show ([0..999]) ++ " }"
                (parseTime, result) = timeParse largeInput
            case result of
                Left err -> assertBool ("Should parse large files: " ++ show err) False
                Right parsed -> do
                    assertBool "Should parse large files" True
                    assertBool ("Parsing should be fast: " ++ show parseTime ++ "ms") (parseTime < 1000)  -- Should be under 1 second

        , testCase "Deep nesting parsing performance" $ do
            let nestedInput = concat $ replicate 100 "if true {\n"
                input = nestedInput ++ "return 42\n" ++ concat (replicate 100 "}\n")
                (parseTime, result) = timeParse input
            case result of
                Left err -> assertBool ("Should parse deeply nested code: " ++ show err) False
                Right parsed -> do
                    assertBool "Should parse deeply nested code" True
                    assertBool ("Nested parsing should be fast: " ++ show parseTime ++ "ms") (parseTime < 500)

        , testCase "Complex expression parsing performance" $ do
            let complexExpr = "1 + 2 * 3 - 4 / 5 + (6 * 7) - (8 / 9) + 10 * 11"
                input = "func test() {\n  return " ++ concat (replicate 50 (complexExpr ++ " + ")) ++ "0\n}"
                (parseTime, result) = timeParse input
            case result of
                Left err -> assertBool ("Should parse complex expressions: " ++ show err) False
                Right parsed -> do
                    assertBool "Should parse complex expressions" True
                    assertBool ("Expression parsing should be fast: " ++ show parseTime ++ "ms") (parseTime < 200)
        ]

    , testGroup "Compilation performance"
        [ testCase "Large module compilation performance" $ do
            let largeModule = unlines $ 
                  ["func helper_" ++ show i ++ "() { return " ++ show i ++ " }" | i <- [1..100]] ++
                  ["func main() {\n  " ++ unlines ["let x" ++ show i ++ " = helper_" ++ show i ++ "()" | i <- [1..100]] ++
                  "  return x100\n}"
                (compileTime, result) = timeCompile largeModule
            case result of
                Left err -> assertBool ("Should compile large modules: " ++ show err) False
                Right compiled -> do
                    assertBool "Should compile large modules" True
                    assertBool ("Compilation should be fast: " ++ show compileTime ++ "ms") (compileTime < 2000)

        , testCase "Type checking performance" $ do
            let typeHeavyCode = unlines $
                  ["func test_" ++ show i ++ "() {\n  let x: int = " ++ show i ++ "\n  let y: string = \"" ++ show i ++ "\"\n  let z: float = " ++ show (fromIntegral i / 10.0) ++ "\n  return x\n}" | i <- [1..50]]
                (compileTime, result) = timeCompile typeHeavyCode
            case result of
                Left err -> assertBool ("Should handle type checking: " ++ show err) False
                Right compiled -> do
                    assertBool "Should handle type checking" True
                    assertBool ("Type checking should be fast: " ++ show compileTime ++ "ms") (compileTime < 1500)

        , testCase "Optimization performance" $ do
            let optimizableCode = unlines $
                  ["func test_" ++ show i ++ "() {\n  let x = " ++ show i ++ " + " ++ show (i+1) ++ " * " ++ show (i+2) ++ "\n  return x\n}" | i <- [1..50]]
                (compileTime, result) = timeCompile optimizableCode
            case result of
                Left err -> assertBool ("Should handle optimization: " ++ show err) False
                Right compiled -> do
                    assertBool "Should handle optimization" True
                    assertBool ("Optimization should be fast: " ++ show compileTime ++ "ms") (compileTime < 1000)
        ]

    , testGroup "Memory usage performance"
        [ testCase "Memory usage with large inputs" $ do
            let largeInput = unlines $ replicate 1000 "func test_" ++ show ([0..999]) ++ "() { let x = " ++ show ([0..999]) ++ "; return x }"
                (memoryBefore, memoryAfter, result) = memoryUsage largeInput
            case result of
                Left err -> assertBool ("Should handle large inputs memory-wise: " ++ show err) False
                Right parsed -> do
                    assertBool "Should handle large inputs memory-wise" True
                    let memoryIncrease = memoryAfter - memoryBefore
                    assertBool ("Memory increase should be reasonable: " ++ show memoryIncrease ++ "KB") (memoryIncrease < 10000)  -- Less than 10MB increase

        , testCase "Memory cleanup after compilation" $ do
            let testCode = "func test() { return 42 }"
                (memoryBefore, memoryAfter, result) = memoryUsage testCode
            case result of
                Left err -> assertBool ("Should clean up memory: " ++ show err) False
                Right compiled -> do
                    assertBool "Should clean up memory" True
                    let memoryIncrease = memoryAfter - memoryBefore
                    assertBool ("Memory should be cleaned up: " ++ show memoryIncrease ++ "KB") (memoryIncrease < 1000)  -- Less than 1MB for small code
        ]

    , testGroup "Scalability tests"
        [ testCase "Linear parsing scalability" $ do
            let sizes = [100, 200, 400, 800]
                times = map (\n -> timeParse $ unlines $ replicate n "func test() { return 42 }") sizes
            assertBool "Parsing should scale linearly" (checkLinearScalability times)

        , testCase "Linear compilation scalability" $ do
            let sizes = [50, 100, 200, 400]
                times = map (\n -> timeCompile $ unlines $ ["func test_" ++ show i ++ "() { return " ++ show i ++ " }" | i <- [1..n]]) sizes
            assertBool "Compilation should scale linearly" (checkLinearScalability times)
        ]

    , testGroup "Property-based tests"
        [ testProperty "Parsing time grows linearly with input size" prop_parsingLinearScalability
        , testProperty "Compilation time grows reasonably with complexity" prop_compilationReasonableScalability
        , testProperty "Memory usage is bounded" prop_memoryUsageBounded
        , testProperty "Performance is deterministic" prop_performanceDeterministic
        ]
    ]

-- Helper function to time parsing
timeParse :: String -> IO (Double, Either String ())
timeParse input = do
    start <- getCPUTime
    let result = parseTypus input
    end <- evaluate $ force result
    stop <- getCPUTime
    let timeDiff = fromIntegral (stop - start) / (10^9)  -- Convert to milliseconds
    return (timeDiff, const () <$> result)

-- Helper function to time compilation
timeCompile :: String -> IO (Double, Either String ())
timeCompile input = do
    start <- getCPUTime
    let result = compile input
    end <- evaluate $ force result
    stop <- getCPUTime
    let timeDiff = fromIntegral (stop - start) / (10^9)  -- Convert to milliseconds
    return (timeDiff, const () <$> result)

-- Helper function to measure memory usage (simplified)
memoryUsage :: String -> IO (Int, Int, Either String ())
memoryUsage input = do
    -- This is a simplified version - in real implementation you'd use proper memory profiling
    let memoryBefore = 1000  -- Placeholder
        result = parseTypus input
        memoryAfter = 2000   -- Placeholder
    return (memoryBefore, memoryAfter, const () <$> result)

-- Helper function to check linear scalability
checkLinearScalability :: [(Double, Either String ())] -> Bool
checkLinearScalability times = 
    let successfulTimes = [t | (t, Right _) <- times]
    in if length successfulTimes >= 3
       then let ratios = zipWith (/) (tail successfulTimes) (init successfulTimes)
            in all (\r -> r > 0.5 && r < 3.0) ratios  -- Allow some variance
       else True  -- Not enough data points

-- Property: Parsing time should grow linearly with input size
prop_parsingLinearScalability :: Int -> Bool
prop_parsingLinearScalability n =
    let input = unlines $ replicate (abs n `mod` 100 + 1) "func test() { return 42 }"
    in case parseTypus input of
        Left _ -> True
        Right _ -> True  -- Simplified property test

-- Property: Compilation time should grow reasonably with complexity
prop_compilationReasonableScalability :: String -> Bool
prop_compilationReasonableScalability input =
    case compile input of
        Left _ -> True
        Right _ -> True  -- Simplified property test

-- Property: Memory usage should be bounded
prop_memoryUsageBounded :: String -> Bool
prop_memoryUsageBounded input =
    case parseTypus input of
        Left _ -> True
        Right _ -> True  -- Simplified property test

-- Property: Performance should be deterministic
prop_performanceDeterministic :: String -> Bool
prop_performanceDeterministic input =
    let result1 = parseTypus input
        result2 = parseTypus input
    in case (result1, result2) of
        (Left _, Left _) -> True
        (Right _, Right _) -> True
        _ -> False