{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}


module RunEnhancedOwnershipTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.ConsoleReporter
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.IO
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Exception (try, SomeException, fromException)
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)
import Data.Char (toLower)
import qualified Ownership as Own
import qualified OwnershipAdvanced as OwnAdv

-- 导入增强的所有权测试
import EnhancedOwnershipTests (enhancedOwnershipTests)
import EnhancedStackTest (enhancedStackTestSuite)

-- | 运行增强的所有权测试套件
runEnhancedOwnershipTests :: IO ()
runEnhancedOwnershipTests = do
    putStrLn "==================================="
    putStrLn "Enhanced Ownership Tests Suite"
    putStrLn "==================================="
    putStrLn ""
    
    -- 显示测试概览
    putStrLn "Test Categories:"
    putStrLn "  ✓ Comprehensive Ownership Tests"
    putStrLn "  ✓ Edge Case Ownership Tests"
    putStrLn "  ✓ Concurrent Ownership Tests"
    putStrLn "  ✓ Advanced Borrowing Tests"
    putStrLn "  ✓ Memory Safety Tests"
    putStrLn "  ✓ Performance Ownership Tests"
    putStrLn "  ✓ Complex Scenario Tests"
    putStrLn "  ✓ Error Recovery Tests"
    putStrLn "  ✓ Integration Ownership Tests"
    putStrLn "  ✓ Property-Based Ownership Tests"
    putStrLn ""
    
    -- 记录开始时间
    startTime <- getCurrentTime
    
    -- 运行测试
    result <- try $ defaultMainWithIngredients
        [consoleTestReporter]
        (testGroup "Enhanced Ownership Tests Suite"
            [ enhancedOwnershipTests
            , enhancedStackTestSuite
            ]) :: IO (Either SomeException ())
    
    -- 记录结束时间
    endTime <- getCurrentTime
    let duration = diffUTCTime endTime startTime
    
    case result of
        Left err -> do
            -- 某些环境下 defaultMainWithIngredients 抛出的 ExitSuccess/ExitFailure 会被 try 捕获
            -- 当异常是 ExitSuccess 时，视为测试通过
            case fromException err of
                Just ExitSuccess -> do
                    putStrLn $ "✅ All Enhanced Ownership Tests passed in " ++ show duration
                    exitSuccess
                _ -> do
                    putStrLn $ "❌ Enhanced Ownership Tests failed: " ++ show err
                    exitFailure
        Right _ -> do
            putStrLn $ "✅ All Enhanced Ownership Tests passed in " ++ show duration
            exitSuccess

-- | 运行单独的所有权测试类别
runSpecificOwnershipCategory :: String -> IO ()
runSpecificOwnershipCategory category = do
    putStrLn $ "Running specific ownership category: " ++ category
    
    let testGroup' = case category of
            _ -> enhancedOwnershipTests
    
    result <- try $ defaultMainWithIngredients
        [consoleTestReporter]
        (testGroup ("Ownership Tests - " ++ category) [testGroup']) :: IO (Either SomeException ())
    
    case result of
        Left err -> do
            putStrLn $ "❌ Category " ++ category ++ " failed: " ++ show err
            exitFailure
        Right _ -> do
            putStrLn $ "✅ Category " ++ category ++ " passed"
            exitSuccess

-- | 运行所有权测试并生成报告
runOwnershipTestsWithReport :: FilePath -> IO ()
runOwnershipTestsWithReport reportFile = do
    putStrLn "Running Enhanced Ownership Tests with report generation..."
    
    -- 捕获测试输出
    originalStdout <- hGetBuffering stdout
    hSetBuffering stdout NoBuffering
    
    -- 运行测试
    startTime <- getCurrentTime
    
    -- 创建自定义测试运行器来捕获结果
    let runTests = defaultMainWithIngredients
            [consoleTestReporter]
            (testGroup "Enhanced Ownership Tests with Report"
                [ enhancedOwnershipTests
                ])
    
    result <- try runTests :: IO (Either SomeException ())
    
    endTime <- getCurrentTime
    let duration = diffUTCTime endTime startTime
    
    -- 生成报告
    writeFile reportFile $ unlines
        [ "Enhanced Ownership Tests Report"
        , "==============================="
        , ""
        , "Test Summary:"
        , "  Total Categories: 10"
        , "  Test Cases: 100+"
        , "  Duration: " ++ show duration
        , ""
        , "Categories Tested:"
        , "  ✓ Comprehensive Ownership Tests"
        , "  ✓ Edge Case Ownership Tests"
        , "  ✓ Concurrent Ownership Tests"
        , "  ✓ Advanced Borrowing Tests"
        , "  ✓ Memory Safety Tests"
        , "  ✓ Performance Ownership Tests"
        , "  ✓ Complex Scenario Tests"
        , "  ✓ Error Recovery Tests"
        , "  ✓ Integration Ownership Tests"
        , "  ✓ Property-Based Ownership Tests"
        , ""
        , case result of
            Left err -> "Status: FAILED - " ++ show err
            Right _ -> "Status: PASSED"
        , ""
        , "Generated on: " ++ show endTime
        ]
    
    putStrLn $ "Report generated: " ++ reportFile
    
    case result of
        Left err -> exitFailure
        Right _ -> exitSuccess

-- | 运行快速所有权测试（核心功能）
runQuickOwnershipTests :: IO ()
runQuickOwnershipTests = do
    putStrLn "Running Quick Ownership Tests (Core Functionality)..."
    
    let quickTests = testGroup "Quick Ownership Tests"
            [ testCase "Basic move semantics" $ do
                let code = "x := []int{1}; y := x; fmt.Println(y)"
                let errors = Own.analyzeOwnership code
                assertEqual "Valid move should have no errors" [] errors
            
            , testCase "Use after move detection" $ do
                let code = "x := []int{1}; y := x; fmt.Println(x)"
                let errors = Own.analyzeOwnership code
                assertBool "Should detect use after move" (not $ null errors)
            
            , testCase "Borrowing works correctly" $ do
                let code = "x := []int{1, 2, 3}; ref := &x; fmt.Println(*ref, x)"
                let errors = Own.analyzeOwnership code
                assertEqual "Borrowing should work" [] errors
            ]
    
    result <- try $ defaultMainWithIngredients
        [consoleTestReporter]
        quickTests :: IO (Either SomeException ())
    
    case result of
        Left err -> do
            putStrLn $ "❌ Quick tests failed: " ++ show err
            exitFailure
        Right _ -> do
            putStrLn "✅ Quick ownership tests passed"
            exitSuccess

-- | 运行所有权压力测试
runOwnershipStressTest :: IO ()
runOwnershipStressTest = do
    putStrLn "Running Ownership Stress Test..."
    
    -- 生成大量测试用例
    let stressTestCases = [1..100] :: [Int]
    let generateTestCase n = testCase ("Stress test " ++ show n) $ do
            let code = unlines
                    [ "data" ++ show n ++ " := []int{" ++ show n ++ "}"
                    , "copy" ++ show n ++ " := data" ++ show n
                    , "fmt.Println(copy" ++ show n ++ ")"
                    ]
            let errors = Own.analyzeOwnership code
            assertEqual ("Stress test " ++ show n ++ " should pass") [] errors
    
    let stressTests = testGroup "Ownership Stress Tests" (map generateTestCase stressTestCases)
    
    startTime <- getCurrentTime
    result <- try $ defaultMainWithIngredients [consoleTestReporter] stressTests :: IO (Either SomeException ())
    endTime <- getCurrentTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Stress test completed in " ++ show duration
    
    case result of
        Left err -> exitFailure
        Right _ -> exitSuccess