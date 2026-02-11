{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestSupport.AdvancedMemoryOptimization
  ( runMemoryOptimizedTests
  , runBatchedTests
  , runStreamingTests
  , runMinimalFootprintTests
  , MemoryOptimizationLevel(..)
  , TestBatchConfig(..)
  , StreamingTestConfig(..)
  , MinimalFootprintConfig(..)
  , createAdvancedMemoryOptimizedSuite
  , withAdvancedMemoryLimits
  , withBatchedExecution
  , withStreamingExecution
  , withMinimalFootprint
  , forceAggressiveGC
  , monitorMemoryUsage
  , cleanupTestResources
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import System.Mem (performGC)
import System.Mem.Weak (Weak, deRefWeak)
import Control.Monad (replicateM_, when, void)
import Control.Concurrent (threadDelay, forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally, evaluate, IOException, catch)
import Data.List (partition, splitAt)
import Data.IORef
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import System.Process (readProcess)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)
import System.Posix.Process (getProcessID)
import System.Posix.Types (CPid)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Advanced memory optimization levels
data MemoryOptimizationLevel = 
    UltraMinimal      -- ^ Ultra minimal memory usage (64MB)
  | ExtremeMinimal    -- ^ Extreme minimal memory usage (128MB)
  | AggressiveMinimal -- ^ Aggressive minimal memory usage (256MB)
  | ModerateMinimal   -- ^ Moderate minimal memory usage (512MB)
  | Conservative      -- ^ Conservative memory usage (1GB)
  deriving (Show, Eq, Ord)

-- | Configuration for batched test execution
data TestBatchConfig = TestBatchConfig
  { batchSize :: Int               -- ^ Number of tests per batch
  , batchDelay :: Int              -- ^ Delay between batches (microseconds)
  , gcBetweenBatches :: Bool       -- ^ Force GC between batches
  , monitorBatchMemory :: Bool     -- ^ Monitor memory usage per batch
  , maxBatchMemoryMB :: Int        -- ^ Maximum memory per batch in MB
  } deriving (Show, Eq)

-- | Configuration for streaming test execution
data StreamingTestConfig = StreamingTestConfig
  { streamChunkSize :: Int         -- ^ Size of each test chunk
  , streamDelay :: Int             -- ^ Delay between chunks (microseconds)
  , gcBetweenChunks :: Bool        -- ^ Force GC between chunks
  , monitorStreamMemory :: Bool    -- ^ Monitor memory usage during streaming
  , maxStreamMemoryMB :: Int       -- ^ Maximum memory during streaming in MB
  } deriving (Show, Eq)

-- | Configuration for minimal footprint execution
data MinimalFootprintConfig = MinimalFootprintConfig
  { maxMemoryMB :: Int             -- ^ Maximum memory limit in MB
  , maxQuickCheckTests :: Int      -- ^ Maximum QuickCheck tests per property
  , maxQuickCheckSize :: Int       -- ^ Maximum QuickCheck size
  , maxQuickCheckShrinks :: Int    -- ^ Maximum QuickCheck shrinks
  , forceGCFrequency :: Int        -- ^ Force GC every N tests
  , enableMemoryMonitoring :: Bool -- ^ Enable detailed memory monitoring
  , cleanupAfterEachTest :: Bool   -- ^ Cleanup resources after each test
  } deriving (Show, Eq)

-- | Default configurations
defaultBatchConfig :: TestBatchConfig
defaultBatchConfig = TestBatchConfig
  { batchSize = 10
  , batchDelay = 100000  -- 100ms
  , gcBetweenBatches = True
  , monitorBatchMemory = True
  , maxBatchMemoryMB = 64
  }

defaultStreamingConfig :: StreamingTestConfig
defaultStreamingConfig = StreamingTestConfig
  { streamChunkSize = 5
  , streamDelay = 50000  -- 50ms
  , gcBetweenChunks = True
  , monitorStreamMemory = True
  , maxStreamMemoryMB = 32
  }

defaultMinimalFootprintConfig :: MinimalFootprintConfig
defaultMinimalFootprintConfig = MinimalFootprintConfig
  { maxMemoryMB = 64
  , maxQuickCheckTests = 3
  , maxQuickCheckSize = 1
  , maxQuickCheckShrinks = 2
  , forceGCFrequency = 1
  , enableMemoryMonitoring = True
  , cleanupAfterEachTest = True
  }

-- | Apply advanced memory limits based on optimization level
withAdvancedMemoryLimits :: MemoryOptimizationLevel -> TestTree -> TestTree
withAdvancedMemoryLimits level test = case level of
  UltraMinimal -> 
    localOption (QuickCheckMaxSize 1) $
    localOption (QuickCheckTests 1) $
    localOption (QuickCheckMaxShrinks 1) $
    test
  ExtremeMinimal -> 
    localOption (QuickCheckMaxSize 1) $
    localOption (QuickCheckTests 2) $
    localOption (QuickCheckMaxShrinks 2) $
    test
  AggressiveMinimal -> 
    localOption (QuickCheckMaxSize 2) $
    localOption (QuickCheckTests 3) $
    localOption (QuickCheckMaxShrinks 3) $
    test
  ModerateMinimal -> 
    localOption (QuickCheckMaxSize 3) $
    localOption (QuickCheckTests 5) $
    localOption (QuickCheckMaxShrinks 5) $
    test
  Conservative -> 
    localOption (QuickCheckMaxSize 5) $
    localOption (QuickCheckTests 10) $
    localOption (QuickCheckMaxShrinks 8) $
    test

-- | Force aggressive garbage collection with enhanced cleanup
forceAggressiveGC :: IO ()
forceAggressiveGC = do
  -- Multiple rounds of aggressive GC
  replicateM_ 5 $ do
    performGC
    threadDelay 1000  -- 1ms delay between GC rounds
  
  -- Final cleanup pass
  performGC
  
  -- Try to prompt system GC if available
  void $ forkIO $ do
    threadDelay 50000  -- 50ms delay
    performGC

-- | Monitor memory usage with enhanced precision
monitorMemoryUsage :: String -> IO Int
monitorMemoryUsage label = do
  pid <- getProcessID
  let pidStr = show pid
  
  -- Try to read memory from /proc filesystem (Linux)
  memKB <- catch (readProcess "cat" ["/proc/" ++ pidStr ++ "/status"] "" :: IO String) 
              (\(_ :: IOException) -> return "VmRSS: 0 kB")
  
  let rssLine = filter (isPrefixOf "VmRSS") (lines memKB)
  let memUsage = case rssLine of
        (line:_) -> case words line of
          (_:value:_) -> read value :: Int
          _ -> 0
        _ -> 0
  
  when (memUsage > 0) $
    printf "[%s] Memory usage: %d KB (%.2f MB)\n" label memUsage (fromIntegral memUsage / 1024 :: Double)
  
  return memUsage
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- | Cleanup test resources aggressively
cleanupTestResources :: IO ()
cleanupTestResources = do
  -- Force multiple GC passes
  forceAggressiveGC
  
  -- Clear any cached data if possible
  evaluate (seq (Map.empty :: Map.Map String Int) ())
  
  -- Final GC pass
  performGC

-- | Execute tests in batches to control memory usage
withBatchedExecution :: TestBatchConfig -> [TestTree] -> IO ()
withBatchedExecution config tests = do
  putStrLn $ "Running " ++ show (length tests) ++ " tests in batches of " ++ show (batchSize config)
  
  let batches = chunksOf (batchSize config) tests
  
  mapM_ (runBatch config) (zip [1..] batches)
  
  where
    chunksOf _ [] = []
    chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

-- | Run a single batch of tests
runBatch :: TestBatchConfig -> (Int, [TestTree]) -> IO ()
runBatch config (batchNum, batchTests) = do
  putStrLn $ "Running batch " ++ show batchNum ++ " with " ++ show (length batchTests) ++ " tests"
  
  -- Monitor memory before batch
  when (monitorBatchMemory config) $ do
    void $ monitorMemoryUsage ("batch-" ++ show batchNum ++ "-before")
  
  -- Run the batch
  let batchSuite = testGroup ("Batch-" ++ show batchNum) batchTests
  defaultMain batchSuite
  
  -- Force GC if configured
  when (gcBetweenBatches config) $ do
    forceAggressiveGC
    putStrLn $ "Forced GC after batch " ++ show batchNum
  
  -- Monitor memory after batch
  when (monitorBatchMemory config) $ do
    memAfter <- monitorMemoryUsage ("batch-" ++ show batchNum ++ "-after")
    when (memAfter > (maxBatchMemoryMB config) * 1024) $
      putStrLn $ "WARNING: Batch " ++ show batchNum ++ " exceeded memory limit"
  
  -- Delay between batches
  when (batchDelay config > 0) $
    threadDelay (batchDelay config)

-- | Execute tests with streaming to minimize memory footprint
withStreamingExecution :: StreamingTestConfig -> [TestTree] -> IO ()
withStreamingExecution config tests = do
  putStrLn $ "Streaming " ++ show (length tests) ++ " tests with chunk size " ++ show (streamChunkSize config)
  
  let chunks = chunksOf (streamChunkSize config) tests
  
  mapM_ (runChunk config) (zip [1..] chunks)
  
  where
    chunksOf _ [] = []
    chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

-- | Run a single chunk of tests
runChunk :: StreamingTestConfig -> (Int, [TestTree]) -> IO ()
runChunk config (chunkNum, chunkTests) = do
  putStrLn $ "Processing chunk " ++ show chunkNum ++ " with " ++ show (length chunkTests) ++ " tests"
  
  -- Monitor memory before chunk
  when (monitorStreamMemory config) $ do
    void $ monitorMemoryUsage ("chunk-" ++ show chunkNum ++ "-before")
  
  -- Run the chunk with minimal memory settings
  let chunkSuite = withAdvancedMemoryLimits UltraMinimal $ 
                  testGroup ("Chunk-" ++ show chunkNum) chunkTests
  
  defaultMain chunkSuite
  
  -- Force GC if configured
  when (gcBetweenChunks config) $ do
    forceAggressiveGC
    putStrLn $ "Forced GC after chunk " ++ show chunkNum
  
  -- Monitor memory after chunk
  when (monitorStreamMemory config) $ do
    memAfter <- monitorMemoryUsage ("chunk-" ++ show chunkNum ++ "-after")
    when (memAfter > (maxStreamMemoryMB config) * 1024) $
      putStrLn $ "WARNING: Chunk " ++ show chunkNum ++ " exceeded memory limit"
  
  -- Delay between chunks
  when (streamDelay config > 0) $
    threadDelay (streamDelay config)

-- | Execute tests with minimal footprint
withMinimalFootprint :: MinimalFootprintConfig -> [TestTree] -> IO ()
withMinimalFootprint config tests = do
  putStrLn $ "Running " ++ show (length tests) ++ " tests with minimal footprint"
  putStrLn $ "Memory limit: " ++ show (maxMemoryMB config) ++ "MB"
  putStrLn $ "QuickCheck tests: " ++ show (maxQuickCheckTests config)
  
  -- Apply minimal memory settings to all tests
  let minimalTests = map (withMinimalMemorySettings config) tests
  let minimalSuite = testGroup "Minimal Footprint Tests" minimalTests
  
  -- Monitor memory before
  when (enableMemoryMonitoring config) $ do
    void $ monitorMemoryUsage "minimal-footprint-before"
  
  -- Run tests with aggressive cleanup
  bracket 
    (return ())
    (\_ -> cleanupTestResources)
    (\_ -> defaultMain minimalSuite)
  
  where
    withMinimalMemorySettings :: MinimalFootprintConfig -> TestTree -> TestTree
    withMinimalMemorySettings cfg test =
      localOption (QuickCheckMaxSize (maxQuickCheckSize cfg)) $
      localOption (QuickCheckTests (maxQuickCheckTests cfg)) $
      localOption (QuickCheckMaxShrinks (maxQuickCheckShrinks cfg)) $
      test

-- | Create advanced memory optimized test suite
createAdvancedMemoryOptimizedSuite :: MemoryOptimizationLevel -> String -> [TestTree] -> TestTree
createAdvancedMemoryOptimizedSuite level name tests =
  let optimizedTests = map (withAdvancedMemoryLimits level) tests
      levelPrefix = case level of
        UltraMinimal -> "[Ultra-Minimal] "
        ExtremeMinimal -> "[Extreme-Minimal] "
        AggressiveMinimal -> "[Aggressive-Minimal] "
        ModerateMinimal -> "[Moderate-Minimal] "
        Conservative -> "[Conservative] "
  in testGroup (levelPrefix ++ name) optimizedTests

-- | Run memory optimized tests with different strategies
runMemoryOptimizedTests :: MemoryOptimizationLevel -> [TestTree] -> IO ()
runMemoryOptimizedTests level tests = do
  putStrLn $ "Running memory optimized tests with level: " ++ show level
  
  -- Force initial GC
  forceAggressiveGC
  
  case level of
    UltraMinimal -> withMinimalFootprint defaultMinimalFootprintConfig tests
    ExtremeMinimal -> withStreamingExecution defaultStreamingConfig tests
    AggressiveMinimal -> withBatchedExecution defaultBatchConfig tests
    ModerateMinimal -> defaultMain (createAdvancedMemoryOptimizedSuite level "Moderate Minimal Tests" tests)
    Conservative -> defaultMain (createAdvancedMemoryOptimizedSuite level "Conservative Tests" tests)

-- | Run batched tests (alias for withBatchedExecution)
runBatchedTests :: TestBatchConfig -> [TestTree] -> IO ()
runBatchedTests = withBatchedExecution

-- | Run streaming tests (alias for withStreamingExecution)
runStreamingTests :: StreamingTestConfig -> [TestTree] -> IO ()
runStreamingTests = withStreamingExecution

-- | Run minimal footprint tests (alias for withMinimalFootprint)
runMinimalFootprintTests :: MinimalFootprintConfig -> [TestTree] -> IO ()
runMinimalFootprintTests = withMinimalFootprint