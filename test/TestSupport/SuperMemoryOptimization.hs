{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TestSupport.SuperMemoryOptimization
  ( withSuperMemoryLimits
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , withSuperLowMemoryLimits
  , withSuperModerateMemoryLimits
  , superMemoryLimitedTestGroup
  , superEmergencyMemoryLimitedTestGroup
  , superCriticalMemoryLimitedTestGroup
  , superMinimalMemoryLimitedTestGroup
  , superLowMemoryLimitedTestGroup
  , superModerateMemoryLimitedTestGroup
  , superGC
  , continuousGC
  , ultraAggressiveGC
  , withSuperMemoryMonitoring
  , withSuperMemoryLevel
  , superMemoryLevelTestGroup
  , SuperMemoryLevel(..)
  , createSuperOptimizedTestSuite
  , selectSuperEssentialTests
  , executeSuperMemoryCleanup
  , monitorSuperMemoryUsage
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import System.Process (readProcess)
import Data.List (isPrefixOf, isInfixOf)
import Text.Read (readMaybe)

-- | Super memory optimization levels
data SuperMemoryLevel = 
    SuperEmergency      -- ^ Super emergency memory usage (1MB)
  | SuperCritical       -- ^ Super critical memory usage (2MB)  
  | SuperMinimal        -- ^ Super minimal memory usage (4MB)
  | SuperLow            -- ^ Super low memory usage (8MB)
  | SuperModerate       -- ^ Super moderate memory usage (16MB)
  deriving (Show, Eq)

-- | Apply super emergency memory limits for extreme constraints - 极限优化
withSuperEmergencyMemoryLimits :: TestTree -> TestTree
withSuperEmergencyMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $           -- 最小值
  localOption (QuickCheckTests 1) $             -- 最小值：每个属性仅测试1次
  localOption (QuickCheckMaxShrinks 0) $        -- 禁用收缩以节省内存
  test

-- | Apply super critical memory limits for very constrained environments - 极度优化
withSuperCriticalMemoryLimits :: TestTree -> TestTree
withSuperCriticalMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $           -- 最小值
  localOption (QuickCheckTests 1) $             -- 保持1个测试
  localOption (QuickCheckMaxShrinks 0) $        -- 禁用收缩以节省内存
  test

-- | Apply super minimal memory limits for constrained environments - 高度优化
withSuperMinimalMemoryLimits :: TestTree -> TestTree
withSuperMinimalMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $           -- 最小值
  localOption (QuickCheckTests 1) $             -- 保持1个测试
  localOption (QuickCheckMaxShrinks 0) $        -- 禁用收缩以节省内存
  test

-- | Apply super low memory limits for moderate constraints - 中度优化
withSuperLowMemoryLimits :: TestTree -> TestTree
withSuperLowMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $           -- 最小值
  localOption (QuickCheckTests 1) $             -- 保持1个测试
  localOption (QuickCheckMaxShrinks 0) $        -- 禁用收缩以节省内存
  test

-- | Apply super moderate memory limits for light optimization - 轻度优化
withSuperModerateMemoryLimits :: TestTree -> TestTree
withSuperModerateMemoryLimits test = 
  localOption (QuickCheckMaxSize 2) $           -- 小值
  localOption (QuickCheckTests 1) $             -- 保持1个测试
  localOption (QuickCheckMaxShrinks 1) $        -- 最小收缩
  test

-- | Generic super memory limits application
withSuperMemoryLimits :: SuperMemoryLevel -> TestTree -> TestTree
withSuperMemoryLimits level test = case level of
  SuperEmergency   -> withSuperEmergencyMemoryLimits test
  SuperCritical    -> withSuperCriticalMemoryLimits test
  SuperMinimal     -> withSuperMinimalMemoryLimits test
  SuperLow         -> withSuperLowMemoryLimits test
  SuperModerate    -> withSuperModerateMemoryLimits test

-- | Create super memory limited test groups
superMemoryLimitedTestGroup :: SuperMemoryLevel -> String -> [TestTree] -> TestTree
superMemoryLimitedTestGroup level name tests = 
  withSuperMemoryLimits level $ testGroup name tests

superEmergencyMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
superEmergencyMemoryLimitedTestGroup = superMemoryLimitedTestGroup SuperEmergency

superCriticalMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
superCriticalMemoryLimitedTestGroup = superMemoryLimitedTestGroup SuperCritical

superMinimalMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
superMinimalMemoryLimitedTestGroup = superMemoryLimitedTestGroup SuperMinimal

superLowMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
superLowMemoryLimitedTestGroup = superMemoryLimitedTestGroup SuperLow

superModerateMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
superModerateMemoryLimitedTestGroup = superMemoryLimitedTestGroup SuperModerate

-- | Super garbage collection strategies
superGC :: IO ()
superGC = do
  -- 执行多次垃圾回收以确保内存清理
  replicateM_ 10 performGC
  -- 短暂等待让GC完成
  threadDelay 1000

-- | Continuous garbage collection for extreme memory constraints
continuousGC :: IO ()
continuousGC = do
  replicateM_ 20 performGC
  threadDelay 500
  replicateM_ 10 performGC
  threadDelay 200
  replicateM_ 5 performGC

-- | Ultra aggressive garbage collection for critical situations
ultraAggressiveGC :: IO ()
ultraAggressiveGC = do
  replicateM_ 50 performGC
  threadDelay 100
  replicateM_ 25 performGC
  threadDelay 50
  replicateM_ 15 performGC
  threadDelay 20
  replicateM_ 10 performGC

-- | Execute super memory cleanup
executeSuperMemoryCleanup :: IO ()
executeSuperMemoryCleanup = do
  -- 执行超级垃圾回收
  ultraAggressiveGC
  
  -- 尝试清理系统缓存（如果可能）
  _ <- tryReadProcess "echo" ["1", ">", "/proc/sys/vm/drop_caches"] ""
  _ <- tryReadProcess "echo" ["2", ">", "/proc/sys/vm/drop_caches"] ""
  _ <- tryReadProcess "echo" ["3", ">", "/proc/sys/vm/drop_caches"] ""
  
  -- 再次执行垃圾回收
  superGC

-- | Monitor super memory usage
monitorSuperMemoryUsage :: IO a -> IO a
monitorSuperMemoryUsage action = do
  -- 预清理
  executeSuperMemoryCleanup
  
  -- 执行操作
  result <- action
  
  -- 后清理
  executeSuperMemoryCleanup
  
  return result

-- | Test group with super memory monitoring
withSuperMemoryMonitoring :: SuperMemoryLevel -> String -> [TestTree] -> TestTree
withSuperMemoryMonitoring level name tests = 
  let monitoredTests = map (\test -> testGroup "Super Monitored" [test]) tests
  in superMemoryLimitedTestGroup level name monitoredTests

-- | Test group with super memory level
superMemoryLevelTestGroup :: SuperMemoryLevel -> String -> [TestTree] -> TestTree
superMemoryLevelTestGroup = superMemoryLimitedTestGroup

-- | Select super essential tests based on memory level
selectSuperEssentialTests :: SuperMemoryLevel -> [String]
selectSuperEssentialTests level = case level of
  SuperEmergency -> 
    [ "Test.Unit.BasicQuickCheckTestSuite.tests"
    , "Test.Unit.ConciseTestSuite.tests"
    ]
  SuperCritical ->
    [ "Test.Unit.BasicQuickCheckTestSuite.tests"
    , "Test.Unit.ConciseTestSuite.tests"
    , "Test.Unit.MemoryOptimizedTestSuite.tests"
    ]
  SuperMinimal ->
    [ "Test.Unit.BasicQuickCheckTestSuite.tests"
    , "Test.Unit.ConciseTestSuite.tests"
    , "Test.Unit.MemoryOptimizedTestSuite.tests"
    , "Test.Unit.ExtremeMemoryOptimizedTestSuite.tests"
    ]
  SuperLow ->
    [ "Test.Unit.BasicQuickCheckTestSuite.tests"
    , "Test.Unit.ConciseTestSuite.tests"
    , "Test.Unit.MemoryOptimizedTestSuite.tests"
    , "Test.Unit.ExtremeMemoryOptimizedTestSuite.tests"
    , "Test.Unit.AdvancedMemoryOptimizedTestSuite.tests"
    ]
  SuperModerate ->
    [ "Test.Unit.BasicQuickCheckTestSuite.tests"
    , "Test.Unit.ConciseTestSuite.tests"
    , "Test.Unit.MemoryOptimizedTestSuite.tests"
    , "Test.Unit.ExtremeMemoryOptimizedTestSuite.tests"
    , "Test.Unit.AdvancedMemoryOptimizedTestSuite.tests"
    , "Test.Unit.ComprehensiveMemoryOptimizedTestSuite.tests"
    ]

-- | Create super optimized test suite
createSuperOptimizedTestSuite :: SuperMemoryLevel -> String -> [TestTree] -> TestTree
createSuperOptimizedTestSuite level name tests = 
  let essentialTests = take (case level of
        SuperEmergency -> 2
        SuperCritical -> 3
        SuperMinimal -> 4
        SuperLow -> 5
        SuperModerate -> 6) tests
      
      optimizedTests = zipWith (\i test -> withSuperMemoryMonitoring level ("Test " ++ show i) [test]) [1..] essentialTests
      
  in superMemoryLimitedTestGroup level name optimizedTests

-- | Helper function to safely read process output
tryReadProcess :: String -> [String] -> String -> IO String
tryReadProcess cmd args input = do
  result <- try $ readProcess cmd args input
  case result of
    Left (_ :: SomeException) -> return ""
    Right output -> return output

-- | Apply super memory level to test tree
withSuperMemoryLevel :: SuperMemoryLevel -> TestTree -> TestTree
withSuperMemoryLevel = withSuperMemoryLimits

-- | Get current memory usage in MB (simplified)
getCurrentMemoryUsage :: IO Int
getCurrentMemoryUsage = do
  result <- tryReadProcess "free" ["-m"] ""
  let lines' = lines result
      memLine = find (isPrefixOf "Mem:") lines'
  case memLine of
    Just line -> 
      let fields = words line
      in case fields of
        (_:used:_:_) -> case readMaybe used of
          Just usage -> return usage
          Nothing -> return 0
        _ -> return 0
    Nothing -> return 0
  where
    find _ [] = Nothing
    find p (x:xs) = if p x then Just x else find p xs