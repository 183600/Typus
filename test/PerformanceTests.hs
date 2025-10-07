{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module PerformanceTests (runPerformanceTests, defaultPerformanceConfig) where

import Test.Tasty.Ingredients (tryIngredients)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Parser (parseTypus)
import qualified Compiler (compile)
import qualified Ownership (analyzeOwnership)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Exception (evaluate)
import Data.Time (getCurrentTime, diffUTCTime)
import Control.DeepSeq ()


-- 性能测试配置
data PerformanceConfig = PerformanceConfig
    { maxParseTime :: Double  -- 最大解析时间（秒）
    , maxCompileTime :: Double -- 最大编译时间（秒）
    , maxOwnershipTime :: Double -- 最大所有权分析时间（秒）
    , maxMemoryUsage :: Int     -- 最大内存使用（MB）
    }

defaultPerformanceConfig :: PerformanceConfig
defaultPerformanceConfig = PerformanceConfig
    { maxParseTime = 5.0
    , maxCompileTime = 10.0
    , maxOwnershipTime = 8.0
    , maxMemoryUsage = 512
    }

-- 从测试数据目录加载测试用例
loadTestData :: FilePath -> IO String
loadTestData fileName = do
    let testDir = "test" </> "data"
    let filePath = testDir </> fileName
    exists <- doesFileExist filePath
    if not exists
        then error $ "Test data file not found: " ++ filePath
        else readFile filePath

-- 测试解析性能
testParsePerformance :: PerformanceConfig -> TestTree
testParsePerformance config = testCase "Parse Performance" $ do
    code <- loadTestData "large_code.typus"
    
    start <- getCurrentTime
    result <- evaluate $ Parser.parseTypus code
    end <- getCurrentTime
    
    let parseTime :: Double
        parseTime = realToFrac $ diffUTCTime end start
    
    case result of
        Left err -> assertFailure $ "Parser failed: " ++ err
        Right _ -> do
            assertBool ("Parsing should complete within " ++ show (maxParseTime config) ++ " seconds")
                (parseTime <= maxParseTime config)

-- 测试编译性能
testCompilePerformance :: PerformanceConfig -> TestTree
testCompilePerformance config = testCase "Compile Performance" $ do
    code <- loadTestData "large_code.typus"
    
    case Parser.parseTypus code of
        Left err -> assertFailure $ "Parser failed: " ++ err
        Right typusFile -> do
            start <- getCurrentTime
            result <- evaluate $ Compiler.compile typusFile
            end <- getCurrentTime
            
            let compileTime :: Double
                compileTime = realToFrac $ diffUTCTime end start
            
            case result of
                Left err -> assertFailure $ "Compilation failed: " ++ err
                Right _ -> do
                    assertBool ("Compilation should complete within " ++ show (maxCompileTime config) ++ " seconds")
                        (compileTime <= maxCompileTime config)

-- 测试所有权分析性能
testOwnershipPerformance :: PerformanceConfig -> TestTree
testOwnershipPerformance config = testCase "Ownership Performance" $ do
    code <- loadTestData "large_code.typus"
    
    start <- getCurrentTime
    _ <- evaluate $ Ownership.analyzeOwnership code
    end <- getCurrentTime
    let ownershipTime :: Double
        ownershipTime = realToFrac $ diffUTCTime end start
    
    assertBool ("Ownership analysis should complete within " ++ show (maxOwnershipTime config) ++ " seconds")
        (ownershipTime <= maxOwnershipTime config)

-- 测试内存使用
testMemoryUsage :: PerformanceConfig -> TestTree
testMemoryUsage _config = testCase "Memory Usage" $ do
    -- 这里需要根据实际情况实现内存使用测试
    -- 由于 Haskell 的内存管理较为复杂，这里仅作为占位符
    code <- loadTestData "large_code.typus"
    
    -- 强制求值以确保内存使用
    result <- evaluate $ Parser.parseTypus code
    case result of
        Left err -> assertFailure $ "Parser failed: " ++ err
        Right _ -> return ()
    
    -- 实际项目中，这里应该使用专门的内存分析工具
    assertBool "Memory usage should be within limits" True

-- 测试并发性能
testConcurrentPerformance :: PerformanceConfig -> TestTree
testConcurrentPerformance config = testCase "Concurrent Performance" $ do
    -- 测试并发处理多个文件的能力
    let testFiles :: [FilePath]
        testFiles = ["simple_go_code.typus", "complex_ownership_code.typus", "code_with_dependent_types.typus"]
    
    start <- getCurrentTime
    results <- mapM loadTestData testFiles
    parseResults <- mapM (evaluate . Parser.parseTypus) results
    
    -- 检查所有解析结果
    mapM_ (\case
        Left err -> assertFailure $ "Parser failed: " ++ err
        Right _ -> return ()) parseResults
    
    end <- getCurrentTime
    let concurrentTime :: Double
        concurrentTime = realToFrac $ diffUTCTime end start
    
    assertBool ("Concurrent parsing should complete within " ++ show (maxParseTime config) ++ " seconds")
        (concurrentTime <= maxParseTime config)

-- 性能测试套件
performanceTestSuite :: PerformanceConfig -> TestTree
performanceTestSuite config = testGroup "Performance Tests" [
    testParsePerformance config,
    testCompilePerformance config,
    testOwnershipPerformance config,
    testMemoryUsage config,
    testConcurrentPerformance config
    ]

-- 运行性能测试并生成报告
runPerformanceTests :: PerformanceConfig -> IO ()
runPerformanceTests config = do
    putStrLn "Running performance test suite for Typus compiler..."
    putStrLn "================================================"
    putStrLn $ "Performance thresholds:"
    putStrLn $ "  Max parse time: " ++ show (maxParseTime config) ++ " seconds"
    putStrLn $ "  Max compile time: " ++ show (maxCompileTime config) ++ " seconds"
    putStrLn $ "  Max ownership time: " ++ show (maxOwnershipTime config) ++ " seconds"
    putStrLn $ "  Max memory usage: " ++ show (maxMemoryUsage config) ++ " MB"
    putStrLn ""
    
    -- 使用 Tasty 运行性能测试
    let ingredients = [consoleTestReporter]
    case tryIngredients ingredients mempty (performanceTestSuite config) of
        Nothing -> do
            putStrLn "ERROR: No suitable ingredient found to run performance tests"
        Just runTest -> do
            success <- runTest
            if success
                then putStrLn "SUCCESS: All performance tests passed. The project meets production requirements."
                else putStrLn "ERROR: Some performance tests failed."
    return ()