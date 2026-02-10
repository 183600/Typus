{-# LANGUAGE CPP #-}

module TestSupport.AdaptiveMemoryConfig 
  ( -- 自适应内存配置
    AdaptiveMemoryConfig(..)
  , defaultAdaptiveConfig
  , -- 自适应策略
    selectAdaptiveMemoryLevel
  , adaptMemoryBasedOnHistory
  , -- 内存历史记录
    MemoryHistory(..)
  , recordMemoryUsage
  , getOptimalMemoryLevel
  , -- 动态测试选择
    selectTestsAdaptively
  , prioritizeTestsByMemoryProfile
  ) where

import Data.List (sortOn, nub, find)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime, addUTCTime, diffUTCTime)
import qualified Data.Map as Map
import Text.Printf (printf)

-- | 自适应内存配置
data AdaptiveMemoryConfig = AdaptiveMemoryConfig
  { baseMemoryLevel :: String           -- ^ 基础内存级别
  , enableAdaptation :: Bool           -- ^ 启用自适应
  , historyRetentionDays :: Int        -- ^ 历史记录保留天数
  , memoryThresholdMB :: Int           -- ^ 内存阈值 (MB)
  , failureTolerance :: Int            -- ^ 失败容忍度
  , adaptiveStepUp :: Bool             -- ^ 是否允许增加内存
  , adaptiveStepDown :: Bool           -- ^ 是否允许减少内存
  , minMemoryLevel :: String           -- ^ 最小内存级别
  , maxMemoryLevel :: String           -- ^ 最大内存级别
  , testSelectionStrategy :: String    -- ^ 测试选择策略
  } deriving (Show, Eq)

-- | 默认自适应配置
defaultAdaptiveConfig :: AdaptiveMemoryConfig
defaultAdaptiveConfig = AdaptiveMemoryConfig
  { baseMemoryLevel = "moderate"
  , enableAdaptation = True
  , historyRetentionDays = 7
  , memoryThresholdMB = 400
  , failureTolerance = 2
  , adaptiveStepUp = True
  , adaptiveStepDown = True
  , minMemoryLevel = "ultra-low"
  , maxMemoryLevel = "normal"
  , testSelectionStrategy = "balanced"
  }

-- | 内存历史记录
data MemoryHistory = MemoryHistory
  { memoryLevel :: String              -- ^ 使用的内存级别
  , peakMemoryMB :: Int                -- ^ 峰值内存使用
  , testCount :: Int                   -- ^ 测试数量
  , successCount :: Int                -- ^ 成功测试数
  , failureCount :: Int                -- ^ 失败测试数
  , durationSeconds :: Double          -- ^ 运行时间
  , timestamp :: String                -- ^ 时间戳
  } deriving (Show, Eq)

-- | 内存级别定义
memoryLevels :: [(String, Int)]
memoryLevels = 
  [ ("ultra-low", 128)
  , ("very-low", 192)
  , ("low", 256)
  , ("moderate", 384)
  , ("normal", 512)
  ]

-- | 获取内存级别的MB值
getMemoryMB :: String -> Int
getMemoryMB level = fromMaybe 256 (lookup level memoryLevels)

-- | 获取下一个内存级别
getNextMemoryLevel :: String -> Bool -> String
getNextMemoryLevel currentLevel stepUp = 
  let levels = map fst memoryLevels
      currentIndex = fromMaybe 0 (findIndex (== currentLevel) levels)
      nextIndex = if stepUp 
                  then min (length levels - 1) (currentIndex + 1)
                  else max 0 (currentIndex - 1)
  in levels !! nextIndex
  where
    findIndex _ [] = Nothing
    findIndex p (x:xs) = if p x then Just 0 else fmap (+1) (findIndex p xs)

-- | 选择自适应内存级别
selectAdaptiveMemoryLevel :: AdaptiveMemoryConfig -> [MemoryHistory] -> String
selectAdaptiveMemoryLevel config history = 
  if not (enableAdaptation config)
     then baseMemoryLevel config
     else adaptMemoryBasedOnHistory config history

-- | 基于历史记录调整内存
adaptMemoryBasedOnHistory :: AdaptiveMemoryConfig -> [MemoryHistory] -> String
adaptMemoryBasedOnHistory config history = 
  let recentHistory = take 3 (reverse (sortOn timestamp history))  -- 最近3次运行
      avgMemory = if null recentHistory 
                  then 0
                  else sum (map peakMemoryMB recentHistory) `div` length recentHistory
      totalFailures = sum (map failureCount recentHistory)
      currentLevel = baseMemoryLevel config
  in if avgMemory > memoryThresholdMB config || totalFailures > failureTolerance config
     then if adaptiveStepUp config
          then getNextMemoryLevel currentLevel True
          else currentLevel
     else if totalFailures == 0 && avgMemory < memoryThresholdMB config `div` 2
          then if adaptiveStepDown config
               then getNextMemoryLevel currentLevel False
               else currentLevel
          else currentLevel

-- | 记录内存使用情况
recordMemoryUsage :: AdaptiveMemoryConfig -> String -> Int -> Int -> Int -> Int -> Double -> IO MemoryHistory
recordMemoryUsage config level peakMem testCount successCount failureCount duration = do
  currentTime <- getCurrentTime
  let timestamp = show currentTime
  return MemoryHistory
    { memoryLevel = level
    , peakMemoryMB = peakMem
    , testCount = testCount
    , successCount = successCount
    , failureCount = failureCount
    , durationSeconds = duration
    , timestamp = timestamp
    }

-- | 获取最优内存级别
getOptimalMemoryLevel :: AdaptiveMemoryConfig -> [MemoryHistory] -> String
getOptimalMemoryLevel config history = 
  let levelPerformance = Map.fromListWith combinePerformance 
        $ map (\h -> (memoryLevel h, calculatePerformance h)) history
      bestLevel = fst $ head $ sortOn (negate . snd) (Map.toList levelPerformance)
  in if Map.null levelPerformance
     then baseMemoryLevel config
     else bestLevel
  where
    combinePerformance (count1, score1) (count2, score2) = 
      (count1 + count2, (score1 * fromIntegral count1 + score2 * fromIntegral count2) / fromIntegral (count1 + count2))
    
    calculatePerformance h = 
      let successRate = fromIntegral (successCount h) / fromIntegral (testCount h)
          memoryEfficiency = fromIntegral (memoryThresholdMB config - min (peakMemoryMB h) (memoryThresholdMB config)) 
                           / fromIntegral (memoryThresholdMB config)
          speedFactor = 10.0 / max 1.0 (durationSeconds h)  -- 假设10秒为基准
      in successRate * 0.4 + memoryEfficiency * 0.3 + speedFactor * 0.3

-- | 自适应测试选择
selectTestsAdaptively :: AdaptiveMemoryConfig -> String -> [a] -> [a]
selectTestsAdaptively config level tests = 
  let memoryMB = getMemoryMB level
      strategy = testSelectionStrategy config
  in case strategy of
       "minimal" -> take (max 1 (length tests `div` 10)) tests
       "conservative" -> take (max 2 (length tests `div` 6)) tests
       "balanced" -> take (max 3 (length tests `div` 4)) tests
       "comprehensive" -> take (max 5 (length tests `div` 2)) tests
       "full" -> tests
       _ -> take (max 3 (length tests `div` 4)) tests

-- | 基于内存配置文件优先选择测试
prioritizeTestsByMemoryProfile :: AdaptiveMemoryConfig -> [MemoryHistory] -> [a] -> [a]
prioritizeTestsByMemoryProfile config history tests = 
  let optimalLevel = getOptimalMemoryLevel config history
      selectedTests = selectTestsAdaptively config optimalLevel tests
  in selectedTests