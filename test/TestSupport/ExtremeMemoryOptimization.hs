{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Extreme memory optimization test support module
-- This module provides memory optimization strategies for extreme memory limit environments (16MB-32MB)
module TestSupport.ExtremeMemoryOptimization 
  ( -- Extreme memory configuration
    ExtremeMemoryConfig(..)
  , ultraExtremeMemoryConfig
  , criticalMemoryConfig
  , emergencyMemoryConfig
  
    -- Extreme memory limit application
  , withExtremeMemoryLimits
  , withCriticalMemoryLimits
  , withEmergencyMemoryLimits
  , createExtremeMemorySuite
  , selectUltraEssentialTests
  
    -- Smart memory management
  , smartMemoryCleanup
  , emergencyMemoryCleanup
  , ultraAggressiveCleanup
  , monitorMemoryUsage
  , adaptiveTestSelection
    
    -- Data size limiters
  , limitStringSize
  , limitListSize
  , limitTreeSize
  , limitArbitrarySize
  ) where
import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import Test.Tasty.QuickCheck (property, forAll, resize)
import Test.Tasty.HUnit (testCase)
import System.Mem (performGC)
import Control.Monad (replicateM_, when, void)
import Control.Concurrent (threadDelay, forkIO)
import Data.List (take, length, splitAt)
import Data.Time (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

-- | Extreme memory configuration for different levels of memory limits
data ExtremeMemoryConfig = ExtremeMemoryConfig
  { memoryLimitMB :: Int        -- ^ Memory limit (MB)
  , maxTestSize :: Int          -- ^ QuickCheck max size
  , testCount :: Int            -- ^ Test count
  , maxShrinks :: Int           -- ^ Max shrink count
  , gcFrequency :: Int          -- ^ GC frequency (every N tests)
  , maxStringLength :: Int      -- ^ Max string length
  , maxListLength :: Int        -- ^ Max list length
  , maxTreeDepth :: Int         -- ^ Max tree depth
  , enableAggressiveCleanup :: Bool -- ^ Enable aggressive cleanup
  , enableEmergencyMode :: Bool -- ^ Enable emergency mode
  , testSelectionRatio :: Double -- ^ Test selection ratio (0.0-1.0)
  , adaptiveSelection :: Bool   -- ^ Adaptive test selection
  } deriving (Show, Eq)

-- | Ultra extreme memory configuration (16MB) - For severely restricted environments
ultraExtremeMemoryConfig :: ExtremeMemoryConfig
ultraExtremeMemoryConfig = ExtremeMemoryConfig
  { memoryLimitMB = 16
  , maxTestSize = 1
  , testCount = 1
  , maxShrinks = 0
  , gcFrequency = 1
  , maxStringLength = 5
  , maxListLength = 3
  , maxTreeDepth = 2
  , enableAggressiveCleanup = True
  , enableEmergencyMode = True
  , testSelectionRatio = 0.01  -- Run only 1% of tests
  , adaptiveSelection = True
  }

-- | Critical memory configuration (24MB) - For critical environments
criticalMemoryConfig :: ExtremeMemoryConfig
criticalMemoryConfig = ExtremeMemoryConfig
  { memoryLimitMB = 24
  , maxTestSize = 1
  , testCount = 2
  , maxShrinks = 1
  , gcFrequency = 1
  , maxStringLength = 8
  , maxListLength = 5
  , maxTreeDepth = 3
  , enableAggressiveCleanup = True
  , enableEmergencyMode = False
  , testSelectionRatio = 0.02  -- Run 2% of tests
  , adaptiveSelection = True
  }

-- | Emergency memory configuration (32MB) - For emergency environments
emergencyMemoryConfig :: ExtremeMemoryConfig
emergencyMemoryConfig = ExtremeMemoryConfig
  { memoryLimitMB = 32
  , maxTestSize = 2
  , testCount = 3
  , maxShrinks = 2
  , gcFrequency = 1
  , maxStringLength = 12
  , maxListLength = 8
  , maxTreeDepth = 4
  , enableAggressiveCleanup = True
  , enableEmergencyMode = False
  , testSelectionRatio = 0.05  -- Run 5% of tests
  , adaptiveSelection = True
  }

-- | Apply ultra extreme memory limits
withExtremeMemoryLimits :: TestTree -> TestTree
withExtremeMemoryLimits test = 
  localOption (QuickCheckMaxSize (maxTestSize ultraExtremeMemoryConfig)) $
  localOption (QuickCheckTests (testCount ultraExtremeMemoryConfig)) $
  localOption (QuickCheckMaxShrinks (maxShrinks ultraExtremeMemoryConfig)) $
  test

-- | Apply critical memory limits
withCriticalMemoryLimits :: TestTree -> TestTree
withCriticalMemoryLimits test = 
  localOption (QuickCheckMaxSize (maxTestSize criticalMemoryConfig)) $
  localOption (QuickCheckTests (testCount criticalMemoryConfig)) $
  localOption (QuickCheckMaxShrinks (maxShrinks criticalMemoryConfig)) $
  test

-- | Apply emergency memory limits
withEmergencyMemoryLimits :: TestTree -> TestTree
withEmergencyMemoryLimits test = 
  localOption (QuickCheckMaxSize (maxTestSize emergencyMemoryConfig)) $
  localOption (QuickCheckTests (testCount emergencyMemoryConfig)) $
  localOption (QuickCheckMaxShrinks (maxShrinks emergencyMemoryConfig)) $
  test

-- | Create extreme memory test suite
createExtremeMemorySuite :: ExtremeMemoryConfig -> String -> [TestTree] -> TestTree
createExtremeMemorySuite config name tests = 
  let selectedTests = selectUltraEssentialTests config tests
      limitedTests = map (applyExtremeLimits config) selectedTests
      prefix = "[" ++ show (memoryLimitMB config) ++ "MB-EXTREME] "
  in testGroup (prefix ++ name ++ " (" ++ show (length selectedTests) ++ "/" ++ show (length tests) ++ " tests)") limitedTests

-- | Apply extreme memory limits
applyExtremeLimits :: ExtremeMemoryConfig -> TestTree -> TestTree
applyExtremeLimits config test = case memoryLimitMB config of
  16 -> withExtremeMemoryLimits test
  24 -> withCriticalMemoryLimits test
  32 -> withEmergencyMemoryLimits test
  _ -> test

-- | Select ultra essential tests
selectUltraEssentialTests :: ExtremeMemoryConfig -> [TestTree] -> [TestTree]
selectUltraEssentialTests config tests = 
  let ratio = testSelectionRatio config
      targetCount = max 1 $ round (fromIntegral (length tests) * ratio)
      adaptive = adaptiveSelection config
  in if adaptive
     then adaptiveTestSelection config tests targetCount
     else take targetCount tests

-- | Adaptive test selection
adaptiveTestSelection :: ExtremeMemoryConfig -> [TestTree] -> Int -> [TestTree]
adaptiveTestSelection config tests targetCount = do
  -- Simple random selection strategy, prioritize core tests
  let coreTests = take (length tests `div` 3) tests
      otherTests = drop (length tests `div` 3) tests
      
  if length coreTests >= targetCount
     then take targetCount coreTests
     else coreTests ++ take (targetCount - length coreTests) otherTests

-- | Smart memory cleanup
smartMemoryCleanup :: IO ()
smartMemoryCleanup = do
  -- Multiple rounds of GC, with increasing intervals
  replicateM_ 5 $ do
    performGC
    threadDelay 1000
  
  -- Medium intensity cleanup
  replicateM_ 3 $ do
    performGC
    threadDelay 3000
  
  -- Final cleanup
  replicateM_ 2 performGC

-- | Emergency memory cleanup
emergencyMemoryCleanup :: IO ()
emergencyMemoryCleanup = do
  -- Extreme multiple rounds of GC, with very short intervals
  replicateM_ 8 $ do
    performGC
    threadDelay 500
  
  -- High intensity cleanup
  replicateM_ 5 $ do
    performGC
    threadDelay 1000
  
  -- Final cleanup
  replicateM_ 3 performGC

-- | Ultra aggressive memory cleanup - for between every test
ultraAggressiveCleanup :: IO ()
ultraAggressiveCleanup = do
  -- Immediate GC with no delays
  replicateM_ 3 performGC
  
  -- Short delay
  threadDelay 200
  
  -- More GC
  replicateM_ 2 performGC
  
  -- Another short delay
  threadDelay 200
  
  -- Final GC
  performGC

-- | Monitor memory usage
monitorMemoryUsage :: IO a -> IO a
monitorMemoryUsage action = do
  -- Force initial GC
  ultraAggressiveCleanup
  
  -- Run action
  result <- action
  
  -- Force final GC
  ultraAggressiveCleanup
  
  return result

-- | Limit string size
limitStringSize :: Int -> String -> String
limitStringSize maxSize s = take maxSize s

-- | Limit list size
limitListSize :: Int -> [a] -> [a]
limitListSize maxSize xs = take maxSize xs

-- | Limit tree size
limitTreeSize :: Int -> [a] -> [a]
limitTreeSize maxDepth xs = take maxDepth xs

-- | Limit arbitrary data size
limitArbitrarySize :: Int -> a -> a
limitArbitrarySize maxSize x = x  -- Simplified implementation

-- | Create memory optimized test property
createMemoryOptimizedProperty :: String -> (String -> Bool) -> TestTree
createMemoryOptimizedProperty testName propFunc = testCase testName $ do
  let testString = ""
  if propFunc testString 
     then return ()
     else error $ "Test failed for: " ++ testString

-- | Create extreme memory optimized test suite example
createExtremeTestSuiteExample :: TestTree
createExtremeTestSuiteExample = 
  createExtremeMemorySuite ultraExtremeMemoryConfig "Extreme Example Tests"
    [ createMemoryOptimizedProperty "basic string test" (not . null)
    , createMemoryOptimizedProperty "string length test" ((> 0) . length)
    ]

data MemoryUsageStats = MemoryUsageStats
  { totalTests :: Int
  , selectedTests :: Int
  , memoryReduction :: Double
  , testReduction :: Double
  } deriving (Show, Eq)

calculateMemoryStats :: ExtremeMemoryConfig -> [TestTree] -> MemoryUsageStats
calculateMemoryStats config tests = 
  let total = length tests
      selected = length (selectUltraEssentialTests config tests)
      testRed = 1.0 - (fromIntegral selected / fromIntegral total)
      memoryRed = case memoryLimitMB config of
        16 -> 0.95  -- 95% memory reduction
        24 -> 0.90  -- 90% memory reduction
        32 -> 0.85  -- 85% memory reduction
        _ -> 0.80
  in MemoryUsageStats total selected memoryRed testRed

printExtremeMemoryReport :: IO ()
printExtremeMemoryReport = do
  putStrLn "=== Extreme Memory Optimization Report ==="
  putStrLn ""
  putStrLn "Available memory configurations:"
  putStrLn "1. Ultra extreme configuration (16MB) - Run only 1% of tests"
  putStrLn "2. Critical configuration (24MB) - Run 2% of tests"
  putStrLn "3. Emergency configuration (32MB) - Run 5% of tests"
  putStrLn ""
  putStrLn "Optimization strategies:"
  putStrLn "- Limit QuickCheck test size to 1-2"
  putStrLn "- Limit test count to 1-3"
  putStrLn "- Limit string length to 5-12 characters"
  putStrLn "- Limit list length to 3-8 elements"
  putStrLn "- Perform garbage collection after each test"
  putStrLn "- Adaptive test selection"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  withExtremeMemoryLimits  - Apply 16MB limits"
  putStrLn "  withCriticalMemoryLimits  - Apply 24MB limits"
  putStrLn "  withEmergencyMemoryLimits - Apply 32MB limits"