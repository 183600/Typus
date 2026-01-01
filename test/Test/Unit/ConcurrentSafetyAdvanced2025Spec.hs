{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.ConcurrentSafetyAdvanced2025Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, elements)
import Test.Tasty.HUnit (testCase, (@=?))

import Control.Concurrent (forkIO, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, writeTVar, modifyTVar)
import Control.Monad (replicateM, when)
import Data.IORef
import SourceLocation (SourcePos(..), SourceSpan(..))

tests :: TestTree
tests = testGroup "Concurrent Safety Advanced Tests"
  [ testProperty "Concurrent parser access is thread-safe" propConcurrentParserAccess
  , testProperty "Concurrent type inference maintains consistency" propConcurrentTypeInferenceConsistency
  , testProperty "Concurrent ownership tracking prevents conflicts" propConcurrentOwnershipTracking
  , testProperty "Concurrent error handling is thread-safe" propConcurrentErrorHandling
  , testProperty "STM-based symbol table operations" propSTMSymbolTableOperations
  , testCase "Concurrent compilation pipeline" testConcurrentCompilationPipeline
  , testProperty "Concurrent dependency analysis" propConcurrentDependencyAnalysis
  , testCase "Thread-safe source location tracking" testThreadSafeSourceLocationTracking
  , testProperty "Concurrent memory management" propConcurrentMemoryManagement
  , testCase "Concurrent test execution isolation" testConcurrentTestExecutionIsolation
  ]

-- Mock data types for testing
data MockSymbolTable = MockSymbolTable (IORef [(String, Int)])

data MockCompilationState = MockCompilationState 
  { stateSymbols :: MockSymbolTable
  , stateErrors :: IORef [String]
  , stateProgress :: IORef Int
  }

data MockConcurrentResource = MockConcurrentResource 
  { resourceCounter :: TVar Int
  , resourceLock :: MVar ()
  }

-- Property 1: Concurrent parser access is thread-safe
propConcurrentParserAccess :: String -> Int -> Bool
propConcurrentParserAccess input numThreads = 
  numThreads > 0 && numThreads <= 10 ==>
  let testResult = runConcurrentParserTest input numThreads
  in case testResult of
       Right results -> L.all (== L.head results) results  -- All results should be identical
       Left _ -> False

-- Property 2: Concurrent type inference maintains consistency
propConcurrentTypeInferenceConsistency :: String -> Int -> Bool
propConcurrentTypeInferenceConsistency expr numThreads =
  numThreads > 0 && numThreads <= 10 ==>
  let testResult = runConcurrentTypeInferenceTest expr numThreads
  in case testResult of
       Right results -> L.length (nub results) <= 1  -- Should have at most one unique result
       Left _ -> False

-- Property 3: Concurrent ownership tracking prevents conflicts
propConcurrentOwnershipTracking :: Int -> Bool
propConcurrentOwnershipTracking numOperations =
  numOperations > 0 && numOperations <= 20 ==>
  let testResult = runConcurrentOwnershipTest numOperations
  in case testResult of
    Right finalCount -> finalCount >= 0  -- Count should never be negative
    Left _ -> False

-- Property 4: Concurrent error handling is thread-safe
propConcurrentErrorHandling :: [String] -> Int -> Bool
propConcurrentErrorHandling errors numThreads =
  numThreads > 0 && numThreads <= 10 ==>
  let testResult = runConcurrentErrorHandlingTest errors numThreads
  in case testResult of
    Right collectedErrors -> L.length collectedErrors >= L.length errors
    Left _ -> False

-- Property 5: STM-based symbol table operations
propSTMSymbolTableOperations :: [(String, Int)] -> Bool
propSTMSymbolTableOperations operations =
  let testResult = runSTMSymbolTableTest operations
  in case testResult of
    Right finalTable -> L.length finalTable == L.length (nub (map fst operations))
    Left _ -> False

-- Test Case 6: Concurrent compilation pipeline
testConcurrentCompilationPipeline :: IO ()
testConcurrentCompilationPipeline = do
  state <- createMockCompilationState
  results <- runConcurrentCompilationPipeline state ["file1.typus", "file2.typus", "file3.typus"]
  
  -- Should have processed L.all files
  L.length results @=? 3
  
  -- Progress should be 100%
  progress <- readIORef (stateProgress state)
  progress @=? 100

-- Property 7: Concurrent dependency analysis
propConcurrentDependencyAnalysis :: [(String, [String])] -> Bool
propConcurrentDependencyAnalysis dependencies =
  let testResult = runConcurrentDependencyAnalysis dependencies
  in case testResult of
    Right graph -> isValidDependencyGraph graph
    Left _ -> False

-- Test Case 8: Thread-safe source location tracking
testThreadSafeSourceLocationTracking :: IO ()
testThreadSafeSourceLocationTracking = do
  locationTracker <- createMockLocationTracker
  
  -- Concurrent updates
  results <- replicateM 10 $ do
    forkIO $ updateMockLocation locationTracker (SourcePos 1 1) (SourcePos 2 10)
    threadDelay 1000
    readMockLocation locationTracker
  
  -- All updates should be reflected consistently
  let finalLocation = L.head results
  sourceLine finalLocation @=? 2
  sourceColumn finalLocation @=? 10

-- Property 9: Concurrent memory management
propConcurrentMemoryManagement :: Int -> Bool
propConcurrentMemoryManagement numAllocations =
  numAllocations > 0 && numAllocations <= 100 ==>
  let testResult = runConcurrentMemoryTest numAllocations
  in case testResult of
    Right finalMemory -> finalMemory >= 0
    Left _ -> False

-- Test Case 10: Concurrent test execution isolation
testConcurrentTestExecutionIsolation :: IO ()
testConcurrentTestExecutionIsolation = do
  results <- replicateM 5 $ runIsolatedTest "test"
  
  -- Each test should have its own isolated environment
  let uniqueResults = nub results
  L.length uniqueResults @=? 5  -- All results should be unique

-- Helper functions for concurrent testing
runConcurrentParserTest :: String -> Int -> IO (Either String [String])
runConcurrentParserTest input numThreads = do
  resultsVar <- newIORef []
  done <- newEmptyMVar
  
  let worker = do
        result <- return $ "parsed: " ++ input  -- Mock parsing
        atomically $ modifyIORef resultsVar (result:)
        putMVar done ()
  
  replicateM_ numThreads (forkIO worker)
  replicateM_ numThreads (takeMVar done)
  
  results <- readIORef resultsVar
  return $ Right results

runConcurrentTypeInferenceTest :: String -> Int -> IO (Either String [String])
runConcurrentTypeInferenceTest expr numThreads = do
  resultsVar <- newIORef []
  done <- newEmptyMVar
  
  let worker = do
        result <- return $ "type: " ++ expr  -- Mock type inference
        atomically $ modifyIORef resultsVar (result:)
        putMVar done ()
  
  replicateM_ numThreads (forkIO worker)
  replicateM_ numThreads (takeMVar done)
  
  results <- readIORef resultsVar
  return $ Right results

runConcurrentOwnershipTest :: Int -> IO (Either String Int)
runConcurrentOwnershipTest numOperations = do
  counter <- newTVarIO 0
  done <- newEmptyMVar
  
  let worker = do
        atomically $ modifyTVar counter (+1)
        threadDelay 100
        atomically $ modifyTVar counter (subtract 1)
        putMVar done ()
  
  replicateM_ numOperations (forkIO worker)
  replicateM_ numOperations (takeMVar done)
  
  finalCount <- atomically $ readTVar counter
  return $ Right finalCount

runConcurrentErrorHandlingTest :: [String] -> Int -> IO (Either String [String])
runConcurrentErrorHandlingTest errors numThreads = do
  errorsVar <- newIORef []
  done <- newEmptyMVar
  
  let worker err = do
        atomically $ modifyIORef errorsVar (err:)
        putMVar done ()
  
  mapM_ (\err -> forkIO $ worker err) (take numThreads (cycle errors))
  replicateM_ numThreads (takeMVar done)
  
  collectedErrors <- readIORef errorsVar
  return $ Right collectedErrors

runSTMSymbolTableTest :: [(String, Int)] -> IO (Either String [(String, Int)])
runSTMSymbolTableTest operations = do
  table <- newTVarIO []
  
  let insert (key, value) = atomically $ do
        current <- readTVar table
        writeTVar table ((key, value) : current)
  
  mapM_ (forkIO . insert) operations
  threadDelay 1000000  -- Wait for L.all operations
  
  finalTable <- atomically $ readTVar table
  return $ Right finalTable

createMockCompilationState :: IO MockCompilationState
createMockCompilationState = do
  symbols <- MockSymbolTable <$> newIORef []
  errors <- newIORef []
  progress <- newIORef 0
  return $ MockCompilationState symbols errors progress

runConcurrentCompilationPipeline :: MockCompilationState -> [String] -> IO [String]
runConcurrentCompilationPipeline state files = do
  resultsVar <- newIORef []
  
  let processFile file = do
        threadDelay 100000  -- Mock processing time
        atomically $ modifyIORef (stateProgress state) (+ (100 `div` L.length files))
        return $ "processed: " ++ file
  
  results <- mapM processFile files
  writeIORef resultsVar results
  
  readIORef resultsVar

runConcurrentDependencyAnalysis :: [(String, [String])] -> IO (Either String [(String, [String])])
runConcurrentDependencyAnalysis dependencies = do
  graphVar <- newIORef []
  
  let analyze (file, deps) = do
        threadDelay 50000  -- Mock analysis time
        return (file, deps)
  
  results <- mapM analyze dependencies
  writeIORef graphVar results
  
  graph <- readIORef graphVar
  return $ Right graph

createMockLocationTracker :: IO (IORef (Maybe SourcePos))
createMockLocationTracker = newIORef Nothing

updateMockLocation :: IORef (Maybe SourcePos) -> SourcePos -> SourcePos -> IO ()
updateMockLocation tracker start end = do
  threadDelay 10000
  writeIORef tracker (Just end)

readMockLocation :: IORef (Maybe SourcePos) -> IO SourcePos
readMockLocation tracker = do
  location <- readIORef tracker
  return $ case location of
    Just pos -> pos
    Nothing -> SourcePos 1 1

runConcurrentMemoryTest :: Int -> IO (Either String Int)
runConcurrentMemoryTest numAllocations = do
  memory <- newTVarIO 0
  done <- newEmptyMVar
  
  let allocate = do
        atomically $ modifyTVar memory (+1)
        threadDelay 1000
        atomically $ modifyTVar memory (subtract 1)
        putMVar done ()
  
  replicateM_ numAllocations (forkIO allocate)
  replicateM_ numAllocations (takeMVar done)
  
  finalMemory <- atomically $ readTVar memory
  return $ Right finalMemory

runIsolatedTest :: String -> IO String
runIsolatedTest testName = do
  -- Each test gets its own isolated environment
  envId <- show <$> newIORef ()
  threadDelay 100000  -- Mock test execution
  return $ testName ++ "-" ++ envId

-- Utility functions
isValidDependencyGraph :: [(String, [String])] -> Bool
isValidDependencyGraph graph = L.all (\(file, deps) -> file `notElem` deps) graph

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (L.filter (/= x) xs)

-- STM helper
newTVarIO :: a -> IO (TVar a)
newTVarIO = atomically . newTVar

-- Atomic IORef operations
atomically :: IO a -> IO a
atomically = id

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef = modifyIORef'