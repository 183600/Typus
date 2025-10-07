module BenchmarkTests (runBenchmarks) where

import Criterion.Main
import Criterion.Types

import qualified Parser (parseTypus)
import qualified Compiler (compile)
import qualified Ownership (analyzeOwnership)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Exception (evaluate)
import Data.List (isInfixOf)

-- 基准测试套件
benchmarkSuite :: Benchmark
benchmarkSuite = bgroup "Compiler Benchmarks" [
    bgroup "Parser" [
        bench "parse simple code" $ whnfIO $ loadAndParse "simple_go_code.typus",
        bench "parse complex code" $ whnfIO $ loadAndParse "complex_ownership_code.typus",
        bench "parse dependent types" $ whnfIO $ loadAndParse "code_with_dependent_types.typus",
        bench "parse large code" $ whnfIO $ loadAndParse "large_code.typus"
    ],
    bgroup "Compiler" [
        bench "compile simple code" $ whnfIO $ loadAndCompile "simple_go_code.typus",
        bench "compile complex code" $ whnfIO $ loadAndCompile "complex_ownership_code.typus",
        bench "compile dependent types" $ whnfIO $ loadAndCompile "code_with_dependent_types.typus",
        bench "compile large code" $ whnfIO $ loadAndCompile "large_code.typus"
    ],
    bgroup "Ownership Analysis" [
        bench "analyze simple code" $ whnfIO $ loadAndAnalyzeOwnership "simple_go_code.typus",
        bench "analyze complex code" $ whnfIO $ loadAndAnalyzeOwnership "complex_ownership_code.typus",
        bench "analyze dependent types" $ whnfIO $ loadAndAnalyzeOwnership "code_with_dependent_types.typus",
        bench "analyze large code" $ whnfIO $ loadAndAnalyzeOwnership "large_code.typus"
    ],
    bgroup "End-to-End" [
        bench "full pipeline simple" $ whnfIO $ fullPipeline "simple_go_code.typus",
        bench "full pipeline complex" $ whnfIO $ fullPipeline "complex_ownership_code.typus",
        bench "full pipeline dependent types" $ whnfIO $ fullPipeline "code_with_dependent_types.typus"
    ]
    ]

-- 加载测试数据
loadTestData :: FilePath -> IO String
loadTestData fileName = do
    let testDir = "test" </> "data"
    let filePath = testDir </> fileName
    exists <- doesFileExist filePath
    if not exists
        then error $ "Test data file not found: " ++ filePath
        else readFile filePath

-- 加载并解析
loadAndParse :: FilePath -> IO ()
loadAndParse fileName = do
    code <- loadTestData fileName
    _ <- evaluate $ Parser.parseTypus code
    return ()

-- 加载并编译
loadAndCompile :: FilePath -> IO ()
loadAndCompile fileName = do
    code <- loadTestData fileName
    case Parser.parseTypus code of
        Left err -> error $ "Parser failed: " ++ err
        Right typusFile -> do
            _ <- evaluate $ Compiler.compile typusFile
            return ()

-- 加载并进行所有权分析
loadAndAnalyzeOwnership :: FilePath -> IO ()
loadAndAnalyzeOwnership fileName = do
    code <- loadTestData fileName
    _ <- evaluate $ Ownership.analyzeOwnership code
    return ()

-- 完整的编译管道
fullPipeline :: FilePath -> IO ()
fullPipeline fileName = do
    code <- loadTestData fileName
    case Parser.parseTypus code of
        Left err -> error $ "Parser failed: " ++ err
        Right typusFile -> do
            case Compiler.compile typusFile of
                Left err -> error $ "Compilation failed: " ++ err
                Right goCode -> do
                    _ <- evaluate $ length goCode
                    _ <- evaluate $ "package main" `isInfixOf` goCode
                    return ()

-- 运行基准测试
runBenchmarks :: IO ()
runBenchmarks = do
    putStrLn "Running compiler benchmarks..."
    putStrLn "================================"
    
    let config = defaultConfig {
        -- 设置更合理的基准测试配置
        timeLimit = 30,  -- 每个基准测试最多30秒
        resamples = 10,  -- 重采样次数
        verbosity = Normal
    }
    
    defaultMainWith config [benchmarkSuite]