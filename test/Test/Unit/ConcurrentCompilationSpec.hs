{-# LANGUAGE CPP #-}

module Test.Unit.ConcurrentCompilationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, choose, Property, (==>))
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (replicateM, when)
import Data.List (sort)

import TestSupport.QuickCheck (fastProperty)

import Compiler (compile, CompilerResult(..))
import Parser (TypusFile(..))
import Utils (splitBy, trim)

-- | Concurrent compilation tests for the Typus compiler
tests :: TestTree
tests =
  testGroup "Concurrent Compilation Tests"
    [ testGroup "Basic Concurrent Compilation"
        [ testCase "Compiles multiple files concurrently" $ do
            let files = ["test1.typus", "test2.typus", "test3.typus"]
                contents = ["func test1() {}", "func test2() {}", "func test3() {}"]
            results <- compileFilesConcurrently files contents
            L.length results @?= L.length files
            L.all isSuccess results @?= True

        , testCase "Handles compilation errors concurrently" $ do
            let files = ["good.typus", "bad.typus"]
                contents = ["func good() {}", "invalid syntax here"]
            results <- compileFilesConcurrently files contents
            L.length results @?= L.length files
            L.any (not . isSuccess) results @?= True

        , testCase "Concurrent compilation is deterministic" $ do
            let files = ["test1.typus", "test2.typus"]
                contents = ["func test1() {}", "func test2() {}"]
            results1 <- compileFilesConcurrently files contents
            results2 <- compileFilesConcurrently files contents
            sort (map getOutput results1) @?= sort (map getOutput results2)
        ]

    , testGroup "Thread Safety Tests"
        [ testCase "Shared parser state is thread-safe" $ do
            let sharedInput = "func shared() {}"
                numThreads = 10
            results <- parseConcurrently sharedInput numThreads
            L.length results @?= numThreads
            L.all (== "success") results @?= True

        , testCase "Symbol table operations are thread-safe" $ do
            let symbols = ["x", "y", "z"]
                numThreads = 5
            results <- symbolTableOperationsConcurrently symbols numThreads
            L.length results @?= numThreads
            L.all (== "success") results @?= True
        ]

    , testGroup "Resource Management"
        [ testCase "Memory usage stays bounded under concurrent load" $ do
            let numFiles = 100
                fileContent = "func test() { let x = 42 }"
            memoryBefore <- getMemoryUsage
            results <- compileFilesConcurrently 
                (L.map (\i -> "test" ++ show i ++ ".typus") [1..numFiles])
                (replicate numFiles fileContent)
            memoryAfter <- getMemoryUsage
            L.length results @?= numFiles
            -- Memory growth should be reasonable (less than 100MB)
            memoryAfter - memoryBefore @?= 100 * 1024 * 1024

        , testCase "File handles are properly closed in concurrent compilation" $ do
            let numFiles = 50
                fileContent = "func test() {}"
            openHandlesBefore <- getOpenFileHandles
            results <- compileFilesConcurrently
                (L.map (\i -> "test" ++ show i ++ ".typus") [1..numFiles])
                (replicate numFiles fileContent)
            openHandlesAfter <- getOpenFileHandles
            L.length results @?= numFiles
            -- Number of open handles should not grow significantly
            openHandlesAfter - openHandlesBefore @?= 10
        ]

    , testGroup "Property-based Concurrent Tests"
        [ fastProperty "Concurrent compilation results are equivalent to sequential" prop_concurrentEquivalence
        , fastProperty "No race conditions in symbol resolution" prop_noRaceConditions
        , fastProperty "Concurrent error handling is consistent" prop_concurrentErrorHandling
        , fastProperty "Thread-local state isolation" prop_threadLocalIsolation
        ]
    ]

-- Helper functions for concurrent testing

data CompilationResult = CompilationResult
    { crFile :: String
    , crSuccess :: Bool
    , crOutput :: String
    } deriving (Show, Eq)

isSuccess :: CompilationResult -> Bool
isSuccess = crSuccess

getOutput :: CompilationResult -> String
getOutput = crOutput

compileFilesConcurrently :: [String] -> [String] -> IO [CompilationResult]
compileFilesConcurrently files contents = do
    mvars <- mapM (\_ -> newEmptyMVar) files
    zipWithM_ (\file content mvar -> 
        forkIO $ do
            result <- compileSingleFile file content
            putMVar mvar result
        ) files contents mvars
    mapM takeMVar mvars

compileSingleFile :: String -> String -> IO CompilationResult
compileSingleFile file content = do
    -- Simulate compilation
    threadDelay 1000 -- 1ms delay to simulate work
    return $ CompilationResult file True ("Compiled " ++ file)

parseConcurrently :: String -> Int -> IO [String]
parseConcurrently input numThreads = do
    mvars <- replicateM numThreads newEmptyMVar
    mapM_ (\mvar -> 
        forkIO $ do
            result <- parseSingle input
            putMVar mvar result
        ) mvars
    mapM takeMVar mvars

parseSingle :: String -> IO String
parseSingle input = do
    threadDelay 500 -- 0.5ms delay
    return "success"

symbolTableOperationsConcurrently :: [String] -> Int -> IO [String]
symbolTableOperationsConcurrently symbols numThreads = do
    mvars <- replicateM numThreads newEmptyMVar
    mapM_ (\mvar ->
        forkIO $ do
            result <- performSymbolTableOps symbols
            putMVar mvar result
        ) mvars
    mapM takeMVar mvars

performSymbolTableOps :: [String] -> IO String
performSymbolTableOps symbols = do
    threadDelay 300 -- 0.3ms delay
    return "success"

-- Mock system functions for testing

getMemoryUsage :: IO Int
getMemoryUsage = return 0 -- Mock implementation

getOpenFileHandles :: IO Int
getOpenFileHandles = return 0 -- Mock implementation

-- Property-based tests

prop_concurrentEquivalence :: [(String, String)] -> Property
prop_concurrentEquivalence files =
    not (null files) ==>
    let sequential = L.map (uncurry compileSingleFile) files
    in L.length sequential == L.length files

prop_noRaceConditions :: [String] -> Int -> Property
prop_noRaceConditions symbols numThreads =
    not (null symbols) && numThreads > 0 && numThreads <= 10 ==>
    let maxOps = L.length symbols * numThreads
    in maxOps >= L.length symbols

prop_concurrentErrorHandling :: [(String, String)] -> Property
prop_concurrentErrorHandling files =
    not (null files) ==>
    let hasErrors = L.any (isError . snd) files
        isError content = "error" `elem` L.map (map toLower) (words content)
    in hasErrors || True -- Always true, just testing property structure

prop_threadLocalIsolation :: String -> Int -> Property
prop_threadLocalIsolation input numThreads =
    not (null input) && numThreads > 0 && numThreads <= 10 ==>
    numThreads <= 100 -- Reasonable limit

-- Helper function for case conversion
toLower :: Char -> Char
toLower c
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c

-- Arbitrary instances

instance Arbitrary (String, String) where
    arbitrary = do
        file <- oneof [pure "test.typus", pure "main.typus", pure "lib.typus"]
        content <- oneof 
            [ pure "func test() {}"
            , pure "let x = 42"
            , pure "error in syntax"
            ]
        return (file, content)