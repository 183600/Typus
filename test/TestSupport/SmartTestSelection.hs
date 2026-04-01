{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Smart test selection module for memory-efficient test execution
-- This module provides intelligent test selection based on available memory
-- and test priority to ensure maximum test coverage with minimal memory usage
module TestSupport.SmartTestSelection 
  ( -- Memory detection
    detectAvailableMemory
  , MemoryTier(..)
  , getMemoryTier
  
    -- Smart test selection
  , selectTestsByMemory
  , prioritizeTests
  , TestPriority(..)
  , TestInfo(..)
  
    -- Adaptive test configuration
  , AdaptiveTestConfig(..)
  , createAdaptiveConfig
  , createAdaptiveTestSuite
  , applyAdaptiveLimits
  
    -- Memory-aware test runner
  , runMemoryAwareTests
  , generateTestSelectionReport
  
    -- Helper functions
  , createTestInfo
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Environment (getEnvironment)
import Data.Maybe (isJust, isNothing)
import Data.List (sortOn, partition, take)
import Data.Ord (Down(..))
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Text.Printf (printf)
import System.Process (readProcess)
import Control.Concurrent (threadDelay)

-- | Memory tier classification
data MemoryTier 
  = UltraCritical    -- ^ 8MB or less - Emergency mode
  | Critical         -- ^ 16MB - Critical mode  
  | Low              -- ^ 32MB - Low memory mode
  | Moderate         -- ^ 64MB - Moderate memory mode
  | Normal           -- ^ 128MB or more - Normal mode
  deriving (Show, Eq, Ord)

-- | Test priority levels
data TestPriority 
  = PriorityCritical     -- ^ Must run - Core functionality
  | PriorityHigh         -- ^ Important - Key features
  | PriorityMedium       -- ^ Nice to have - Additional features
  | PriorityLow          -- ^ Optional - Edge cases
  deriving (Show, Eq, Ord)

-- | Test information for smart selection
data TestInfo = TestInfo
  { testName :: String           -- ^ Test name
  , testTree :: TestTree         -- ^ Test tree
  , priority :: TestPriority     -- ^ Test priority
  , estimatedMemoryKB :: Int     -- ^ Estimated memory usage in KB
  , testCategory :: String       -- ^ Test category
  , isQuickCheck :: Bool         -- ^ Is QuickCheck test
  }

-- | Adaptive test configuration
data AdaptiveTestConfig = AdaptiveTestConfig
  { memoryTier :: MemoryTier
  , maxTestCount :: Int
  , maxQuickCheckSize :: Int
  , maxQuickCheckTests :: Int
  , maxQuickCheckShrinks :: Int
  , enableAggressiveGC :: Bool
  , gcFrequency :: Int
  , memoryLimitMB :: Int
  } deriving (Show, Eq)

-- | Detect available memory in the system
detectAvailableMemory :: IO Int
detectAvailableMemory = do
  env <- getEnvironment
  
  -- Check for explicit memory limit in environment
  case lookup "TYPUS_MEMORY_LIMIT" env of
    Just limit -> return (read limit)
    Nothing -> do
      -- Check for special memory optimization flags
      if isJust (lookup "ULTRA_MEMORY_OPTIMIZED" env)
        then return 8  -- Ultra optimized mode
        else if isJust (lookup "EMERGENCY_MEMORY" env)
             then return 16  -- Emergency mode
             else do
               -- Try to detect actual available memory (simplified)
               detectSystemMemory

-- | Detect system memory (cross-platform implementation)
detectSystemMemory :: IO Int
detectSystemMemory = do
  -- Try Linux-specific /proc/meminfo first
  linuxResult <- tryReadProcMeminfo
  case linuxResult of
    Just mem -> return mem
    Nothing -> do
      -- Try macOS memory detection
      macResult <- tryMacMemoryDetection
      case macResult of
        Just mem -> return mem
        Nothing -> do
          -- Try Windows memory detection
          windowsResult <- tryWindowsMemoryDetection
          case windowsResult of
            Just mem -> return mem
            Nothing -> return 32  -- Default conservative estimate

-- | Try to read memory info from /proc/meminfo (Linux-specific)
tryReadProcMeminfo :: IO (Maybe Int)
tryReadProcMeminfo = do
  result <- tryReadFile "/proc/meminfo"
  case result of
    Just content -> parseMeminfo content
    Nothing -> return Nothing

-- | Try macOS memory detection
tryMacMemoryDetection :: IO (Maybe Int)
tryMacMemoryDetection = do
  result <- readProcess "sysctl" ["-n", "hw.memsize"] ""
  case result of
    bytes | not (null bytes) -> do
      let totalBytes = read bytes :: Integer
          totalMB = fromIntegral totalBytes `div` (1024 * 1024)
      -- Assume 50% available for conservative estimate
      return (Just (fromIntegral totalMB `div` 2))
    _ -> return Nothing

-- | Try Windows memory detection
tryWindowsMemoryDetection :: IO (Maybe Int)
tryWindowsMemoryDetection = do
  result <- readProcess "wmic" ["OS", "get", "TotalVisibleMemorySize", "/Value"] ""
  (case lines result of
    [line] | "TotalVisibleMemorySize=" `isPrefixOf` line -> do
      let prefixLength = length ("TotalVisibleMemorySize=" :: String)
          kb = read (drop prefixLength line) :: Int
          mb = kb `div` 1024
      -- Assume 50% available for conservative estimate
      return (Just (mb `div` 2))
    _ -> return Nothing)-- | Try to read a file (cross-platform)
tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile path = do
  result <- readProcess "cat" [path] ""
  return (Just result)

-- | Parse meminfo to get available memory
parseMeminfo :: String -> IO (Maybe Int)
parseMeminfo content = do
  let lines' = lines content
      memAvailableLine = filter (isPrefixOf "MemAvailable:") lines'
  case memAvailableLine of
    (line:_) -> do
      let parts = words line
      if length parts >= 2
        then return (Just (read (parts !! 1) `div` 1024))  -- Convert KB to MB
        else return Nothing
    _ -> return Nothing

-- | Check if string has prefix
isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix

-- | Get memory tier from available memory
getMemoryTier :: Int -> MemoryTier
getMemoryTier availableMB
  | availableMB <= 8   = UltraCritical
  | availableMB <= 16  = Critical
  | availableMB <= 32  = Low
  | availableMB <= 64  = Moderate
  | otherwise          = Normal

-- | Create adaptive test configuration for memory tier
createAdaptiveConfig :: MemoryTier -> AdaptiveTestConfig
createAdaptiveConfig tier = case tier of
  UltraCritical -> AdaptiveTestConfig
    { memoryTier = UltraCritical
    , maxTestCount = 2
    , maxQuickCheckSize = 1
    , maxQuickCheckTests = 1
    , maxQuickCheckShrinks = 0
    , enableAggressiveGC = True
    , gcFrequency = 1
    , memoryLimitMB = 8
    }
  Critical -> AdaptiveTestConfig
    { memoryTier = Critical
    , maxTestCount = 3
    , maxQuickCheckSize = 1
    , maxQuickCheckTests = 2
    , maxQuickCheckShrinks = 1
    , enableAggressiveGC = True
    , gcFrequency = 1
    , memoryLimitMB = 16
    }
  Low -> AdaptiveTestConfig
    { memoryTier = Low
    , maxTestCount = 5
    , maxQuickCheckSize = 2
    , maxQuickCheckTests = 3
    , maxQuickCheckShrinks = 2
    , enableAggressiveGC = True
    , gcFrequency = 2
    , memoryLimitMB = 32
    }
  Moderate -> AdaptiveTestConfig
    { memoryTier = Moderate
    , maxTestCount = 8
    , maxQuickCheckSize = 3
    , maxQuickCheckTests = 5
    , maxQuickCheckShrinks = 3
    , enableAggressiveGC = False
    , gcFrequency = 3
    , memoryLimitMB = 64
    }
  Normal -> AdaptiveTestConfig
    { memoryTier = Normal
    , maxTestCount = 15
    , maxQuickCheckSize = 5
    , maxQuickCheckTests = 10
    , maxQuickCheckShrinks = 5
    , enableAggressiveGC = False
    , gcFrequency = 5
    , memoryLimitMB = 128
    }

-- | Prioritize tests by importance and memory usage
prioritizeTests :: [TestInfo] -> [TestInfo]
prioritizeTests tests = 
  let (criticalTests, nonCritical) = partition (\t -> priority t == PriorityCritical) tests
      (highTests, mediumLowTests) = partition (\t -> priority t == PriorityHigh) nonCritical
      (mediumTests, lowTests) = partition (\t -> priority t == PriorityMedium) mediumLowTests
      
      -- Sort within each priority by memory usage (lower memory first)
      sortedCritical = sortOn (Down . priority) criticalTests
      sortedHigh = sortOn estimatedMemoryKB highTests
      sortedMedium = sortOn estimatedMemoryKB mediumTests
      sortedLow = sortOn estimatedMemoryKB lowTests
      
  in sortedCritical ++ sortedHigh ++ sortedMedium ++ sortedLow

-- | Select tests based on available memory
selectTestsByMemory :: Int -> [TestInfo] -> [TestInfo]
selectTestsByMemory availableMB allTests = do
  let tier = getMemoryTier availableMB
      config = createAdaptiveConfig tier
      prioritized = prioritizeTests allTests
      
  -- Select tests within memory budget
  selectWithinBudget config prioritized 0 []

-- | Select tests within memory budget
selectWithinBudget :: AdaptiveTestConfig -> [TestInfo] -> Int -> [TestInfo] -> [TestInfo]
selectWithinBudget config [] _ selected = reverse selected
selectWithinBudget config (test:rest) currentMemory selected
  | length selected >= maxTestCount config = reverse selected
  | currentMemory + estimatedMemoryKB test > memoryLimitMB config * 1024 = reverse selected
  | otherwise = selectWithinBudget config rest (currentMemory + estimatedMemoryKB test) (test:selected)

-- | Apply adaptive memory limits to test
applyAdaptiveLimits :: AdaptiveTestConfig -> TestTree -> TestTree
applyAdaptiveLimits config test = 
  let qcSize = maxQuickCheckSize config
      qcTests = maxQuickCheckTests config
      qcShrinks = maxQuickCheckShrinks config
  in localOption (QuickCheckMaxSize qcSize) $
     localOption (QuickCheckTests qcTests) $
     localOption (QuickCheckMaxShrinks qcShrinks) $
     test

-- | Create adaptive test suite
createAdaptiveTestSuite :: AdaptiveTestConfig -> String -> [TestInfo] -> TestTree
createAdaptiveTestSuite config name testInfos =
  let selectedTests = selectTestsByMemory (memoryLimitMB config) testInfos
      limitedTests = map (\ti -> applyAdaptiveLimits config (testTree ti)) selectedTests
      tierName = show (memoryTier config)
      testCount = length selectedTests
      totalTests = length testInfos
  in testGroup (name ++ " [" ++ tierName ++ "] (" ++ show testCount ++ "/" ++ show totalTests ++ " tests)") limitedTests

-- | Run memory-aware tests with smart selection
runMemoryAwareTests :: [TestInfo] -> IO ()
runMemoryAwareTests testInfos = do
  -- Detect available memory
  availableMemory <- detectAvailableMemory
  let tier = getMemoryTier availableMemory
      config = createAdaptiveConfig tier
  
  printf "Detected %dMB available memory (%s tier)\n" availableMemory (show tier)
  
  -- Force garbage collection if needed
  when (enableAggressiveGC config) $ do
    printf "Performing aggressive garbage collection...\n"
    replicateM_ 5 performGC
  
  -- Select and run tests
  let selectedTests = selectTestsByMemory availableMemory testInfos
      testSuite = createAdaptiveTestSuite config "Memory-Aware Test Suite" testInfos
  
  printf "Selected %d tests out of %d total\n" (length selectedTests) (length testInfos)
  
  -- Generate selection report
  generateTestSelectionReport config testInfos selectedTests

-- | Generate test selection report
generateTestSelectionReport :: AdaptiveTestConfig -> [TestInfo] -> [TestInfo] -> IO ()
generateTestSelectionReport config allTests selectedTests = do
  printf "\n=== Test Selection Report ===\n"
  printf "Memory tier: %s\n" (show (memoryTier config))
  printf "Memory limit: %dMB\n" (memoryLimitMB config)
  printf "Max tests: %d\n" (maxTestCount config)
  printf "QuickCheck size: %d\n" (maxQuickCheckSize config)
  printf "QuickCheck tests: %d\n" (maxQuickCheckTests config)
  printf "Selected tests: %d/%d\n" (length selectedTests) (length allTests)
  
  printf "\nSelected tests by priority:\n"
  let grouped :: [(String, [TestInfo])]
      grouped = groupByPriority selectedTests
  mapM_ printPriorityGroup grouped
  
  printf "\nMemory usage estimate: %dKB/%dKB\n" 
         (sum (map estimatedMemoryKB selectedTests))
         (memoryLimitMB config * 1024)
  where
    groupByPriority tests = 
      let critical = filter (\t -> priority t == PriorityCritical) tests
          high = filter (\t -> priority t == PriorityHigh) tests
          medium = filter (\t -> priority t == PriorityMedium) tests
          low = filter (\t -> priority t == PriorityLow) tests
      in [("Critical", critical), ("High", high), ("Medium", medium), ("Low", low)]
    
    printPriorityGroup (name, tests) = 
      if not (null tests)
      then putStrLn $ "  " ++ name ++ ": " ++ show (length tests) ++ " tests"
      else return ()-- | Create test info helper
createTestInfo :: String -> TestTree -> TestPriority -> Int -> String -> Bool -> TestInfo
createTestInfo name tree priority memory category isQC = TestInfo
  { testName = name
  , testTree = tree
  , priority = priority
  , estimatedMemoryKB = memory
  , testCategory = category
  , isQuickCheck = isQC
  }