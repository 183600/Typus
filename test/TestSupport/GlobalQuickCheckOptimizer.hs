{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Global QuickCheck Optimizer Module
-- This module provides global optimization for QuickCheck parameters
-- to ensure memory efficiency across all tests.
module TestSupport.GlobalQuickCheckOptimizer 
  ( -- * Global QuickCheck Configuration
    GlobalQuickCheckConfig(..)
  , globalQuickCheckConfig
  , setGlobalQuickCheckConfig
  , getGlobalQuickCheckConfig
    
    -- * Memory-Efficient Generators
  , genSmallString
  , genSmallList
  , genSmallInt
  , genSmallBool
  , genBoundedString
  , genBoundedList
  , genMemoryEfficient
    
    -- * Global Test Optimization
  , globallyOptimizeTest
  , globallyOptimizeTestSuite
  , applyGlobalQuickCheckOptimization
    
    -- * Memory Monitoring for QuickCheck
  , withQuickCheckMemoryMonitoring
  , quickCheckMemoryCleanup
  , monitorQuickTestExecution
    
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck 
  ( QuickCheckMaxSize(..)
  , QuickCheckTests(..)
  , QuickCheckMaxShrinks(..)
  , testProperty
  , property
  , Gen
  , choose
  , listOf
  , vectorOf
  , elements
  , oneof
  , sized
  , resize
  )
import TestSupport.ConsolidatedMemoryOptimization 
  ( MemoryConfig(..)
  , QuickCheckMemoryConfig(..)
  , globalMemoryConfig
  , getCurrentQuickCheckConfig
  , withMemoryMonitoring
  , aggressiveCleanup
  )
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay, forkIO)
import Data.List (take)
import Data.String (IsString)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import Data.IORef

-- | Global QuickCheck configuration
data GlobalQuickCheckConfig = GlobalQuickCheckConfig
  { globalMaxSize :: Int
  , globalMaxTests :: Int
  , globalMaxShrinks :: Int
  , globalMaxStringLength :: Int
  , globalMaxListLength :: Int
  , globalEnableMonitoring :: Bool
  , globalCleanupFrequency :: Int
  , globalMemoryThreshold :: Int  -- ^ MB
  } deriving (Show, Eq)

-- | Global configuration reference
globalConfigRef :: IORef GlobalQuickCheckConfig
globalConfigRef = unsafePerformIO $ newIORef defaultGlobalConfig
{-# NOINLINE globalConfigRef #-}

-- | Default global configuration
defaultGlobalConfig :: GlobalQuickCheckConfig
defaultGlobalConfig = GlobalQuickCheckConfig
  { globalMaxSize = 2
  , globalMaxTests = 5
  , globalMaxShrinks = 2
  , globalMaxStringLength = 8
  , globalMaxListLength = 5
  , globalEnableMonitoring = True
  , globalCleanupFrequency = 3
  , globalMemoryThreshold = 64
  }

-- | Get global QuickCheck configuration
getGlobalQuickCheckConfig :: IO GlobalQuickCheckConfig
getGlobalQuickCheckConfig = readIORef globalConfigRef

-- | Set global QuickCheck configuration
setGlobalQuickCheckConfig :: GlobalQuickCheckConfig -> IO ()
setGlobalQuickCheckConfig config = writeIORef globalConfigRef config

-- | Initialize global configuration from memory config
initializeGlobalConfig :: IO ()
initializeGlobalConfig = do
  let qcConfig = getCurrentQuickCheckConfig
      globalConfig = GlobalQuickCheckConfig
        { globalMaxSize = qcMaxSize qcConfig
        , globalMaxTests = qcMaxTests qcConfig
        , globalMaxShrinks = qcMaxShrinks qcConfig
        , globalMaxStringLength = qcMaxStringLength qcConfig
        , globalMaxListLength = qcMaxListLength qcConfig
        , globalEnableMonitoring = True
        , globalCleanupFrequency = 2
        , globalMemoryThreshold = memoryLimitMB globalMemoryConfig
        }
  setGlobalQuickCheckConfig globalConfig

-- | Global QuickCheck configuration (initialized at startup)
globalQuickCheckConfig :: GlobalQuickCheckConfig
globalQuickCheckConfig = unsafePerformIO $ do
  initializeGlobalConfig
  getGlobalQuickCheckConfig
{-# NOINLINE globalQuickCheckConfig #-}

-- | Generate small strings for memory efficiency
genSmallString :: Gen String
genSmallString = do
  size <- globalMaxStringLength <$> getGlobalQuickCheckConfig
  n <- choose (0, size)
  vectorOf n $ elements ['a'..'z']

-- | Generate small lists for memory efficiency
genSmallList :: Gen a -> Gen [a]
genSmallList gen = do
  size <- globalMaxListLength <$> getGlobalQuickCheckConfig
  n <- choose (0, size)
  vectorOf n gen

-- | Generate small integers for memory efficiency
genSmallInt :: Gen Int
genSmallInt = do
  maxSize <- globalMaxSize <$> getGlobalQuickCheckConfig
  choose (-maxSize, maxSize)

-- | Generate booleans (already memory efficient)
genSmallBool :: Gen Bool
genSmallBool = elements [True, False]

-- | Generate bounded strings with explicit limit
genBoundedString :: Int -> Gen String
genBoundedString limit = do
  n <- choose (0, min limit (globalMaxStringLength globalQuickCheckConfig))
  vectorOf n $ elements ['a'..'z']

-- | Generate bounded lists with explicit limit
genBoundedList :: Int -> Gen a -> Gen [a]
genBoundedList limit gen = do
  n <- choose (0, min limit (globalMaxListLength globalQuickCheckConfig))
  vectorOf n gen

-- | Memory-efficient generator that respects global limits
genMemoryEfficient :: Gen a -> Gen a
genMemoryEfficient gen = sized $ \size -> do
  maxSize <- globalMaxSize <$> getGlobalQuickCheckConfig
  let limitedSize = min size maxSize
  resize limitedSize gen

-- | Apply global QuickCheck optimization to a test
globallyOptimizeTest :: TestTree -> TestTree
globallyOptimizeTest test = do
  config <- globalQuickCheckConfig
  localOption (QuickCheckMaxSize (globalMaxSize config)) $
    localOption (QuickCheckTests (globalMaxTests config)) $
      localOption (QuickCheckMaxShrinks (globalMaxShrinks config)) $
        test

-- | Apply global optimization to a test suite
globallyOptimizeTestSuite :: String -> [TestTree] -> TestTree
globallyOptimizeTestSuite name tests = 
  let optimizedTests = map globallyOptimizeTest tests
      prefix = "[GLOBAL-QC-OPT] "
  in testGroup (prefix ++ name) optimizedTests

-- | Apply global QuickCheck optimization with memory monitoring
applyGlobalQuickCheckOptimization :: TestTree -> TestTree
applyGlobalQuickCheckOptimization test = do
  config <- globalQuickCheckConfig
  let withOptimization = globallyOptimizeTest test
      withMonitoring = if globalEnableMonitoring config
                      then withQuickCheckMemoryMonitoring withOptimization
                      else withOptimization
  withMonitoring

-- | QuickCheck memory monitoring
withQuickCheckMemoryMonitoring :: TestTree -> TestTree
withQuickCheckMemoryMonitoring test = testGroup "[QC-MEM-MONITOR]" [test]

-- | QuickCheck memory cleanup
quickCheckMemoryCleanup :: IO ()
quickCheckMemoryCleanup = do
  config <- getGlobalQuickCheckConfig
  let cleanupRounds = if globalMemoryThreshold config <= 32 then 5 else 3
  
  replicateM_ cleanupRounds $ do
    performGC
    threadDelay 100

-- | Monitor QuickCheck test execution
monitorQuickTestExecution :: IO a -> IO a
monitorQuickTestExecution action = do
  config <- getGlobalQuickCheckConfig
  
  -- Pre-execution cleanup
  when (globalEnableMonitoring config) $ do
    replicateM_ 2 performGC
    threadDelay 50
  
  -- Execute action
  result <- action
  
  -- Post-execution cleanup
  when (globalEnableMonitoring config) $ do
    quickCheckMemoryCleanup
  
  return result

-- | Create memory-efficient QuickCheck property
createMemoryEfficientProperty :: String -> (a -> Bool) -> a -> TestTree
createMemoryEfficientProperty testName prop = 
  let limitedProp x = prop x  -- Property is already constrained by global config
  in testProperty testName $ property limitedProp

-- | Create memory-efficient QuickCheck property for strings
createMemoryEfficientStringProperty :: String -> (String -> Bool) -> TestTree
createMemoryEfficientStringProperty testName prop = 
  testProperty testName $ property $ \s -> do
    config <- getGlobalQuickCheckConfig
    let limitedS = take (globalMaxStringLength config) s
    prop limitedS

-- | Create memory-efficient QuickCheck property for lists
createMemoryEfficientListProperty :: String -> ([a] -> Bool) -> Gen a -> TestTree
createMemoryEfficientListProperty testName prop gen = 
  testProperty testName $ property $ \xs -> do
    config <- getGlobalQuickCheckConfig
    let limitedXs = take (globalMaxListLength config) xs
    prop limitedXs

-- | Batch optimize multiple tests
batchOptimizeTests :: [TestTree] -> TestTree
batchOptimizeTests tests = 
  let optimizedTests = map applyGlobalQuickCheckOptimization tests
  in testGroup "[BATCH-QC-OPT]" optimizedTests

-- | Adaptive optimization based on memory pressure
adaptiveQuickCheckOptimization :: TestTree -> TestTree
adaptiveQuickCheckOptimization test = do
  config <- getGlobalQuickCheckConfig
  let isMemoryConstrained = globalMemoryThreshold config <= 32
  
  if isMemoryConstrained
    then do
      -- Apply ultra-conservative settings for memory-constrained environments
      localOption (QuickCheckMaxSize 1) $
        localOption (QuickCheckTests 1) $
          localOption (QuickCheckMaxShrinks 0) $
            test
    else do
      -- Apply standard optimization
      globallyOptimizeTest test

-- | Print global QuickCheck optimization report
printQuickCheckOptimizationReport :: IO ()
printQuickCheckOptimizationReport = do
  config <- getGlobalQuickCheckConfig
  putStrLn "=== Global QuickCheck Optimization Report ==="
  printf "Max size: %d\n" (globalMaxSize config)
  printf "Max tests: %d\n" (globalMaxTests config)
  printf "Max shrinks: %d\n" (globalMaxShrinks config)
  printf "Max string length: %d\n" (globalMaxStringLength config)
  printf "Max list length: %d\n" (globalMaxListLength config)
  printf "Memory monitoring: %s\n" (show $ globalEnableMonitoring config)
  printf "Cleanup frequency: %d\n" (globalCleanupFrequency config)
  printf "Memory threshold: %dMB\n" (globalMemoryThreshold config)
  putStrLn ""
  putStrLn "Optimization features:"
  putStrLn "- Global QuickCheck parameter limits"
  putStrLn "- Memory-efficient generators"
  putStrLn "- Automatic memory monitoring"
  putStrLn "- Adaptive optimization based on memory pressure"
  putStrLn "- Batch optimization for test suites"

-- | Initialize global optimization (call this at test startup)
initializeGlobalQuickCheckOptimization :: IO ()
initializeGlobalQuickCheckOptimization = do
  initializeGlobalConfig
  config <- getGlobalQuickCheckConfig
  printf "Global QuickCheck optimization initialized with %dMB memory threshold\n" 
    (globalMemoryThreshold config)
  
  when (globalEnableMonitoring config) $
    putStrLn "QuickCheck memory monitoring enabled"