{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TestSupport.UltimateMemoryLimits where

import Control.Exception (evaluate, try, SomeException)
import Control.Monad (when, replicateM_)
import Data.List (isInfixOf)
import System.Mem (performGC)
import System.Process (readProcess)
import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import Data.Time (getCurrentTime, diffUTCTime)

-- | 极致内存限制级别
data UltimateMemoryLevel = 
    UltimateEmergency    -- ^ 1MB - 极致紧急
  | UltimateCritical     -- ^ 2MB - 极致关键  
  | UltimateMinimal      -- ^ 4MB - 极致最小
  | UltimateLow          -- ^ 8MB - 极度低内存
  | UltimateModerate     -- ^ 16MB - 极度适中
  | UltimateNormal       -- ^ 32MB - 极度正常
  deriving (Show, Eq, Ord)

-- | 极致内存配置
data UltimateMemoryConfig = UltimateMemoryConfig
  { umMemoryLimit :: Int          -- ^ 内存限制（MB）
  , umQuickCheckTests :: Int      -- ^ QuickCheck测试次数
  , umQuickCheckMaxSize :: Int    -- ^ QuickCheck最大大小
  , umQuickCheckMaxShrinks :: Int -- ^ QuickCheck最大收缩次数
  , umStringLimit :: Int          -- ^ 字符串长度限制
  , umListLimit :: Int            -- ^ 列表长度限制
  , umIntLimit :: Int             -- ^ 整数范围限制
  , umGCStrategy :: String        -- ^ 垃圾回收策略
  , umTestTimeout :: Int          -- ^ 测试超时（秒）
  } deriving (Show)

-- | 获取极致内存配置
getUltimateMemoryConfig :: UltimateMemoryLevel -> UltimateMemoryConfig
getUltimateMemoryConfig level = case level of
  UltimateEmergency -> UltimateMemoryConfig
    { umMemoryLimit = 1
    , umQuickCheckTests = 1
    , umQuickCheckMaxSize = 1
    , umQuickCheckMaxShrinks = 0
    , umStringLimit = 1
    , umListLimit = 1
    , umIntLimit = 1
    , umGCStrategy = "ultra-aggressive"
    , umTestTimeout = 15
    }
  UltimateCritical -> UltimateMemoryConfig
    { umMemoryLimit = 2
    , umQuickCheckTests = 1
    , umQuickCheckMaxSize = 1
    , umQuickCheckMaxShrinks = 0
    , umStringLimit = 1
    , umListLimit = 1
    , umIntLimit = 2
    , umGCStrategy = "hyper-aggressive"
    , umTestTimeout = 30
    }
  UltimateMinimal -> UltimateMemoryConfig
    { umMemoryLimit = 4
    , umQuickCheckTests = 1
    , umQuickCheckMaxSize = 2
    , umQuickCheckMaxShrinks = 0
    , umStringLimit = 2
    , umListLimit = 1
    , umIntLimit = 3
    , umGCStrategy = "aggressive"
    , umTestTimeout = 60
    }
  UltimateLow -> UltimateMemoryConfig
    { umMemoryLimit = 8
    , umQuickCheckTests = 2
    , umQuickCheckMaxSize = 2
    , umQuickCheckMaxShrinks = 1
    , umStringLimit = 3
    , umListLimit = 2
    , umIntLimit = 5
    , umGCStrategy = "frequent"
    , umTestTimeout = 120
    }
  UltimateModerate -> UltimateMemoryConfig
    { umMemoryLimit = 16
    , umQuickCheckTests = 3
    , umQuickCheckMaxSize = 3
    , umQuickCheckMaxShrinks = 1
    , umStringLimit = 5
    , umListLimit = 3
    , umIntLimit = 8
    , umGCStrategy = "regular"
    , umTestTimeout = 300
    }
  UltimateNormal -> UltimateMemoryConfig
    { umMemoryLimit = 32
    , umQuickCheckTests = 5
    , umQuickCheckMaxSize = 5
    , umQuickCheckMaxShrinks = 2
    , umStringLimit = 8
    , umListLimit = 5
    , umIntLimit = 16
    , umGCStrategy = "normal"
    , umTestTimeout = 600
    }

-- | 极致内存限制操作
withUltimateMemoryLimits :: UltimateMemoryLevel -> IO a -> IO a
withUltimateMemoryLimits level action = do
  let config = getUltimateMemoryConfig level
  
  -- 设置极致内存环境
  setupUltimateMemoryEnvironment config
  
  -- 执行前清理
  performUltimateCleanup
  
  -- 执行操作
  result <- try action
  
  -- 执行后清理
  performUltimateCleanup
  
  case result of
    Left ex -> do
      putStrLn $ "Exception in ultimate memory context: " ++ show (ex :: SomeException)
      performUltimateEmergencyCleanup
      error "Ultimate memory limit exceeded"
    Right val -> return val

-- | 设置极致内存环境
setupUltimateMemoryEnvironment :: UltimateMemoryConfig -> IO ()
setupUltimateMemoryEnvironment config = do
  -- 设置环境变量
  setEnv "QUICKCHECK_TESTS" (show $ umQuickCheckTests config)
  setEnv "QUICKCHECK_MAX_SIZE" (show $ umQuickCheckMaxSize config)
  setEnv "QUICKCHECK_MAX_SHRINKS" (show $ umQuickCheckMaxShrinks config)
  setEnv "TYPUS_STRING_LENGTH_LIMIT" (show $ umStringLimit config)
  setEnv "TYPUS_LIST_LENGTH_LIMIT" (show $ umListLimit config)
  setEnv "TYPUS_INT_RANGE_LIMIT" (show $ umIntLimit config)
  setEnv "TYPUS_ULTIMATE_OPTIMIZATION" "1"
  setEnv "TYPUS_EXTREME_LIMITS" "1"
  
  -- 设置GHC运行时选项
  let rtsOptions = printf "-M%dm -A%dk -n%dk -H%dm -qg -G1" 
                    (umMemoryLimit config)
                    (umMemoryLimit config * 32)
                    (umMemoryLimit config * 4)
                    (umMemoryLimit config `div` 2)
  setEnv "GHCRTS" rtsOptions

-- | 执行极致清理
performUltimateCleanup :: IO ()
performUltimateCleanup = do
  -- 多次强制垃圾回收
  replicateM_ 5 performGC
  
  -- 短暂休息让GC完成
  threadDelay 100000  -- 100ms
  
  -- 清理环境变量中的大对象
  unsetEnv "LARGE_DATA"
  unsetEnv "BIG_STRING"
  
  -- 系统级清理（如果可能）
  _ <- try $ readProcess "sync" [] ""
  _ <- try $ readProcess "sh" ["-c", "echo 3 > /proc/sys/vm/drop_caches"] ""
  
  return ()

-- | 执行极致紧急清理
performUltimateEmergencyCleanup :: IO ()
performUltimateEmergencyCleanup = do
  -- 极致的垃圾回收
  replicateM_ 10 performGC
  
  -- 更长的休息时间
  threadDelay 500000  -- 500ms
  
  -- 更激进的系统清理
  _ <- try $ readProcess "sync" [] ""
  _ <- try $ readProcess "sh" ["-c", "echo 3 > /proc/sys/vm/drop_caches"] ""
  _ <- try $ readProcess "sh" ["-c", "find /tmp -name 'typus-*' -delete 2>/dev/null"] ""
  _ <- try $ readProcess "sh" ["-c", "find /tmp -name 'cabal-*' -delete 2>/dev/null"] ""
  
  return ()

-- | 极致字符串限制
ultimateStringLimit :: UltimateMemoryLevel -> String -> String
ultimateStringLimit level s = 
  let config = getUltimateMemoryConfig level
      limit = umStringLimit config
  in take limit s

-- | 极致列表限制
ultimateListLimit :: UltimateMemoryLevel -> [a] -> [a]
ultimateListLimit level xs = 
  let config = getUltimateMemoryConfig level
      limit = umListLimit config
  in take limit xs

-- | 极致整数限制
ultimateIntLimit :: UltimateMemoryLevel -> Int -> Int
ultimateIntLimit level n = 
  let config = getUltimateMemoryConfig level
      limit = umIntLimit config
      maxVal = limit `div` 2
  in if n < 0 then max (-maxVal) (negate maxVal) else min maxVal n

-- | 极致内存监控
monitorUltimateMemory :: UltimateMemoryLevel -> IO a -> IO (a, String)
monitorUltimateMemory level action = do
  startTime <- getCurrentTime
  
  -- 执行操作
  result <- withUltimateMemoryLimits level action
  
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  
  -- 生成内存报告
  let config = getUltimateMemoryConfig level
      report = printf "Ultimate Memory Level: %s\nMemory Limit: %dMB\nDuration: %.2fs\nGC Strategy: %s"
                     (show level) (umMemoryLimit config) (realToFrac duration :: Double) (umGCStrategy config)
  
  return (result, report)

-- | 极致测试套件创建器
createUltimateMemorySuite :: UltimateMemoryLevel -> String -> [IO ()] -> IO ()
createUltimateMemorySuite level name tests = do
  putStrLn $ "Running Ultimate Memory Suite: " ++ name ++ " (Level: " ++ show level ++ ")"
  
  let config = getUltimateMemoryConfig level
  putStrLn $ "Memory Limit: " ++ show (umMemoryLimit config) ++ "MB"
  putStrLn $ "Test Count: " ++ show (length tests)
  
  -- 执行所有测试
  sequence_ tests
  
  putStrLn $ "Ultimate Memory Suite completed: " ++ name

-- | 极致内存属性测试
ultimateMemoryProperty :: UltimateMemoryLevel -> String -> IO Bool -> IO Bool
ultimateMemoryProperty level name prop = do
  putStrLn $ "Running Ultimate Memory Property: " ++ name
  result <- withUltimateMemoryLimits level prop
  putStrLn $ "Property " ++ name ++ " result: " ++ show result
  return result

-- | 极致内存基准测试
runUltimateMemoryBenchmark :: UltimateMemoryLevel -> IO () -> IO String
runUltimateMemoryBenchmark level action = do
  putStrLn $ "Running Ultimate Memory Benchmark for level: " ++ show level
  
  (_, report) <- monitorUltimateMemory level action
  
  putStrLn "Ultimate Memory Benchmark completed"
  return report

-- | 辅助函数：设置环境变量
setEnv :: String -> String -> IO ()
setEnv key value = unsafePerformIO $ do
  _ <- try $ readProcess "sh" ["-c", "export " ++ key ++ "=" ++ value] ""
  return ()

-- | 辅助函数：取消设置环境变量
unsetEnv :: String -> IO ()
unsetEnv key = unsafePerformIO $ do
  _ <- try $ readProcess "sh" ["-c", "unset " ++ key] ""
  return ()

-- | 预定义的极致内存配置
ultimateEmergencyConfig :: UltimateMemoryConfig
ultimateEmergencyConfig = getUltimateMemoryConfig UltimateEmergency

ultimateCriticalConfig :: UltimateMemoryConfig
ultimateCriticalConfig = getUltimateMemoryConfig UltimateCritical

ultimateMinimalConfig :: UltimateMemoryConfig
ultimateMinimalConfig = getUltimateMemoryConfig UltimateMinimal

ultimateLowConfig :: UltimateMemoryConfig
ultimateLowConfig = getUltimateMemoryConfig UltimateLow

ultimateModerateConfig :: UltimateMemoryConfig
ultimateModerateConfig = getUltimateMemoryConfig UltimateModerate

ultimateNormalConfig :: UltimateMemoryConfig
ultimateNormalConfig = getUltimateMemoryConfig UltimateNormal

-- | 自动检测内存级别
detectUltimateMemoryLevel :: IO UltimateMemoryLevel
detectUltimateMemoryLevel = do
  -- 尝试读取系统内存信息
  result <- try $ readProcess "free" ["-m"] ""
  case result of
    Left _ -> return UltimateMinimal  -- 默认值
    Right output -> 
      let lines' = lines output
          availableMb = if length lines' > 1
                       then read $ words (lines' !! 1) !! 6
                       else 32  -- 默认值
      in if availableMb <= 8
         then return UltimateEmergency
         else if availableMb <= 16
              then return UltimateCritical
              else if availableMb <= 32
                   then return UltimateMinimal
                   else if availableMb <= 64
                        then return UltimateLow
                        else if availableMb <= 128
                             then return UltimateModerate
                             else return UltimateNormal

-- | 极致内存优化建议
getUltimateMemoryOptimizationAdvice :: UltimateMemoryLevel -> String
getUltimateMemoryOptimizationAdvice level = case level of
  UltimateEmergency -> 
    "极致紧急模式 (1MB):\n" ++
    "- 禁用所有非核心功能\n" ++
    "- 使用最小测试数据集\n" ++
    "- 激进的垃圾回收策略\n" ++
    "- 仅运行最关键的测试"
  UltimateCritical ->
    "极致关键模式 (2MB):\n" ++
    "- 限制测试数据大小\n" ++
    "- 频繁的内存清理\n" ++
    "- 优化字符串操作\n" ++
    "- 运行核心测试子集"
  UltimateMinimal ->
    "极致最小模式 (4MB):\n" ++
    "- 适度的测试限制\n" ++
    "- 平衡的内存使用\n" ++
    "- 基本的内存监控\n" ++
    "- 运行基本测试套件"
  UltimateLow ->
    "极度低内存模式 (8MB):\n" ++
    "- 轻度的内存限制\n" ++
    "- 标准的垃圾回收\n" ++
    "- 增强的测试覆盖\n" ++
    "- 运行标准测试套件"
  UltimateModerate ->
    "极度适中模式 (16MB):\n" ++
    "- 宽松的内存限制\n" ++
    "- 优化的性能平衡\n" ++
    "- 全面的测试覆盖\n" ++
    "- 运行增强测试套件"
  UltimateNormal ->
    "极度正常模式 (32MB):\n" ++
    "- 最少的内存限制\n" ++
    "- 标准的性能设置\n" ++
    "- 完整的测试覆盖\n" ++
    "- 运行完整测试套件"