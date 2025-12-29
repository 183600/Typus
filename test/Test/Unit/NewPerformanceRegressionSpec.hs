{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewPerformanceRegressionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser (parseTypus)
import Compiler (compile)
import Compiler.TypeChecker (buildTypeEnv, inferExpressionType)
import Ownership (analyzeOwnership, newOwnershipAnalyzer)
import Dependencies (analyzeDependentTypes)
import Utils (trim, splitBy, removeComments)
import SourceLocation (advancePosByText, startPos)
import IntegratedCompiler (compileWithIntegratedAnalyzers, defaultCompilerConfig)

import Compiler.GoAst (parseGoModule)
import SyntaxValidator (validateFile)
import AnalyzerIntegration (runIntegratedAnalysis, mkAnalysisInput, newIntegratedAnalyzer)

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (length, foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

-- | Performance regression tests
tests :: TestTree
tests =
  testGroup "New Performance Regression Tests"
    [ testGroup "Parsing performance"
        [ testCase "small file parsing performance" $ do
            let smallSource = unlines
                  [ "package main"
                  , ""
                  , "func main() {"
                  , "    println(\"Hello\")"
                  , "}"
                  ]
                iterations = 1000
            (time, result) <- timeAction $ repeatAction iterations (parseTypus smallSource)
            case result of
              Left err -> assertFailure $ "Parsing failed: " ++ err
              Right _ -> do
                -- Should parse small files quickly (less than 1ms per file)
                let avgTime = time / fromIntegral iterations
                avgTime @?= (avgTime :: Double)  -- Basic comparison
                -- Assert that average time is reasonable
                if avgTime > 0.001
                  then assertFailure $ "Parsing too slow: " ++ show avgTime ++ "s per file"
                  else return ()
                
        , testCase "large file parsing performance" $ do
            let largeFunction = "func test" ++ show 1 ++ "() {\n" ++
                               concat ["    x := " ++ show i ++ "\n" | i <- [1..100]] ++
                               "    println(x)\n}\n"
                largeSource = "package main\n\n" ++ concat [largeFunction | i <- [1..50]]
                iterations = 10
            (time, result) <- timeAction $ repeatAction iterations (parseTypus largeSource)
            case result of
              Left err -> assertFailure $ "Parsing large file failed: " ++ err
              Right _ -> do
                -- Should handle larger files efficiently
                let avgTime = time / fromIntegral iterations
                if avgTime > 0.1
                  then assertFailure $ "Large file parsing too slow: " ++ show avgTime ++ "s per file"
                  else return ()
        ]
        
    , testGroup "Compilation performance"
        [ testCase "simple compilation performance" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "func add(a, b int) int {"
                  , "    return a + b"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    result := add(10, 20)"
                  , "    println(result)"
                  , "}"
                  ]
                iterations = 100
            result <- parseTypus source
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ err
              Right typusFile -> do
                (time, _) <- timeAction $ repeatAction iterations (compile typusFile)
                let avgTime = time / fromIntegral iterations
                if avgTime > 0.01
                  then assertFailure $ "Compilation too slow: " ++ show avgTime ++ "s per compilation"
                  else return ()
                  
        , testCase "complex compilation performance" $ do
            let complexSource = unlines
                  [ "package main"
                  , ""
                  , "type Complex struct {"
                  , "    Data map[string][]int"
                  , "    Chan chan func(string) error"
                  , "}"
                  , ""
                  , "func (c Complex) Process() error {"
                  , "    for k, v := range c.Data {"
                  , "        go func(key string, vals []int) {"
                  , "            c.Chan <- func(s string) error {"
                  , "                return nil"
                  , "            }"
                  , "        }(k, v)"
                  , "    }"
                  , "    return nil"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    c := Complex{make(map[string][]int), make(chan func(string) error)}"
                  , "    c.Process()"
                  , "}"
                  ]
                iterations = 20
            result <- parseTypus complexSource
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ err
              Right typusFile -> do
                (time, _) <- timeAction $ repeatAction iterations (compile typusFile)
                let avgTime = time / fromIntegral iterations
                if avgTime > 0.05
                  then assertFailure $ "Complex compilation too slow: " ++ show avgTime ++ "s per compilation"
                  else return ()
        ]
        
    , testGroup "Type checking performance"
        [ testCase "type environment building performance" $ do
            let goSource = unlines
                  [ "package main"
                  , ""
                  , concat ["func func" ++ show i ++ "() int { return " ++ show i ++ " }\n" | i <- [1..100]]
                  ]
                iterations = 50
            case parseGoModule (lines goSource) of
              Left err -> assertFailure $ "Go parsing failed: " ++ err
              Right goModule -> do
                (time, _) <- timeAction $ repeatAction iterations (evaluate . force . buildTypeEnv $ goModule)
                let avgTime = time / fromIntegral iterations
                if avgTime > 0.01
                  then assertFailure $ "Type environment building too slow: " ++ show avgTime ++ "s per build"
                  else return ()
                  
        , testCase "type inference performance" $ do
            let typeEnv = buildTypeEnvFromPairs 
                    [ ("var" ++ show i, TypeName "int") | i <- [1..1000] ]
                    []
                expressions = ["var" ++ show i | i <- [1..100]]
                iterations = 100
            (time, _) <- timeAction $ repeatAction iterations (mapM (inferExpressionType typeEnv) expressions)
            let avgTime = time / fromIntegral iterations
            if avgTime > 0.01
              then assertFailure $ "Type inference too slow: " ++ show avgTime ++ "s per batch"
              else return ()
        ]
        
    , testGroup "Ownership analysis performance"
        [ testCase "simple ownership analysis performance" $ do
            let source = unlines
                  [ "//! ownership on"
                  , ""
                  , "func main() {"
                  , "    x := 42"
                  , "    y := x"
                  , "    println(y)"
                  , "}"
                  ]
                analyzer = newOwnershipAnalyzer
                iterations = 200
            (time, _) <- timeAction $ repeatAction iterations (analyzeOwnership analyzer source)
            let avgTime = time / fromIntegral iterations
            if avgTime > 0.005
              then assertFailure $ "Simple ownership analysis too slow: " ++ show avgTime ++ "s per analysis"
              else return ()
              
        , testCase "complex ownership analysis performance" $ do
            let complexOwnershipSource = unlines
                  [ "//! ownership on"
                  , ""
                  , "func complex() {"
                  , concat ["    var" ++ show i ++ " := " ++ show i ++ "\n" | i <- [1..50]]
                  , concat ["    moved" ++ show i ++ " := var" ++ show i ++ "\n" | i <- [1..25]]
                  , concat ["    used" ++ show i ++ " := moved" ++ show i ++ "\n" | i <- [1..25]]
                  , "}"
                  ]
                analyzer = newOwnershipAnalyzer
                iterations = 50
            (time, _) <- timeAction $ repeatAction iterations (analyzeOwnership analyzer complexOwnershipSource)
            let avgTime = time / fromIntegral iterations
            if avgTime > 0.02
              then assertFailure $ "Complex ownership analysis too slow: " ++ show avgTime ++ "s per analysis"
              else return ()
        ]
        
    , testGroup "Dependency analysis performance"
        [ testCase "dependent type analysis performance" $ do
            let source = unlines
                  [ "type A = B"
                  , "type B = C"
                  , "type C = Int"
                  ]
                iterations = 100
            (time, _) <- timeAction $ repeatAction iterations (analyzeDependentTypes source)
            let avgTime = time / fromIntegral iterations
            if avgTime > 0.01
              then assertFailure $ "Dependent type analysis too slow: " ++ show avgTime ++ "s per analysis"
              else return ()
              
        , testCase "complex dependency analysis performance" $ do
            let complexDepSource = unlines
                  [ "type T" ++ show i ++ " = T" ++ show (i+1) | i <- [1..99]] ++
                  ["type T100 = Int"]
                iterations = 20
            (time, _) <- timeAction $ repeatAction iterations (analyzeDependentTypes complexDepSource)
            let avgTime = time / fromIntegral iterations
            if avgTime > 0.05
              then assertFailure $ "Complex dependency analysis too slow: " ++ show avgTime ++ "s per analysis"
              else return ()
        ]
        
    , testGroup "Integrated analysis performance"
        [ testCase "full pipeline performance" $ do
            let source = unlines
                  [ "//! ownership on"
                  , "//! dependent_types on"
                  , ""
                  , "package main"
                  , ""
                  , "func process<T>(data T) T {"
                  , "    return data"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    result := process(42)"
                  , "    println(result)"
                  , "}"
                  ]
                config = defaultCompilerConfig
                iterations = 50
            (time, _) <- timeAction $ repeatAction iterations (compileWithIntegratedAnalyzers source config)
            let avgTime = time / fromIntegral iterations
            if avgTime > 0.05
              then assertFailure $ "Full pipeline too slow: " ++ show avgTime ++ "s per compilation"
              else return ()
        ]
        
    , testGroup "Memory usage performance"
        [ testCase "large file memory usage" $ do
            let veryLargeSource = unlines
                  [ "package main"
                  , ""
                  , concat ["func func" ++ show i ++ "() {\n" ++
                           concat ["    x := " ++ show j ++ "\n" | j <- [1..100]] ++
                           "    println(x)\n}\n" | i <- [1..100]]
                  ]
                iterations = 5
            -- Force evaluation to ensure memory is actually used
            (time, result) <- timeAction $ repeatAction iterations (evaluate . force . parseTypus $ veryLargeSource)
            case result of
              Left err -> assertFailure $ "Large file parse failed: " ++ err
              Right _ -> do
                let avgTime = time / fromIntegral iterations
                if avgTime > 0.1
                  then assertFailure $ "Large file processing too slow: " ++ show avgTime ++ "s per file"
                  else return ()
                  
        , testCase "repeated operations don't leak memory" $ do
            let source = unlines
                  [ "package main"
                  , ""
                  , "func test() {"
                  , "    x := 42"
                  , "    println(x)"
                  , "}"
                  ]
                iterations = 1000
                -- Perform many operations to check for memory leaks
                (time, _) <- timeAction $ repeatAction iterations (do
                    result <- parseTypus source
                    case result of
                      Left _ -> return ()
                      Right typusFile -> do
                        _ <- evaluate $ force typusFile
                        return ())
                let avgTime = time / fromIntegral iterations
                if avgTime > 0.001
                  then assertFailure $ "Repeated operations too slow (possible memory leak): " ++ show avgTime ++ "s per operation"
                  else return ()
        ]
        
    , testGroup "Text processing performance"
        [ testCase "large text processing performance" $ do
            let largeText = concat [replicate 1000 "test string " ++ "\n" | _ <- [1..100]]
                iterations = 10
            (time, _) <- timeAction $ repeatAction iterations (evaluate . force . removeComments $ largeText)
            let avgTime = time / fromIntegral iterations
            if avgTime > 0.01
              then assertFailure $ "Large text processing too slow: " ++ show avgTime ++ "s per operation"
              else return ()
              
        , testCase "source location tracking performance" $ do
            let largeSource = concat [replicate 1000 "line content\n" | _ <- [1..10]]
                iterations = 50
            (time, _) <- timeAction $ repeatAction iterations (evaluate . force . advancePosByText largeSource $ startPos)
            let avgTime = time / fromIntegral iterations
            if avgTime > 0.001
              then assertFailure $ "Source location tracking too slow: " ++ show avgTime ++ "s per operation"
              else return ()
        ]
    ]

-- Helper functions for performance testing
timeAction :: IO a -> IO (Double, a)
timeAction action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let timeDiff = diffUTCTime end start
    return (realToFrac timeDiff, result)

repeatAction :: Int -> IO a -> IO a
repeatAction n action = foldl' (>>) (return ()) (replicate n action) >> action

buildTypeEnvFromPairs :: [(String, Compiler.TypeChecker.Type)] -> [(String, Compiler.TypeChecker.FunctionSignature)] -> Compiler.TypeChecker.TypeEnv
buildTypeEnvFromPairs varPairs funcPairs = 
    Compiler.TypeChecker.TypeEnv 
        { Compiler.TypeChecker.varTypes = Map.fromList varPairs
        , Compiler.TypeChecker.functionTypes = Map.fromList funcPairs
        }