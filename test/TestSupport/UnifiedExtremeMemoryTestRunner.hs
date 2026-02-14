{-# LANGUAGE OverloadedStrings #-}

-- | 统一的极度内存优化测试运行器
module TestSupport.UnifiedExtremeMemoryTestRunner 
  ( runExtremeMemoryTests
  , ExtremeTestConfig(..)
  ) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck 
  ( QuickCheckMaxSize(..)
  , QuickCheckTests(..)
  , QuickCheckMaxShrinks(..)
  )
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import System.Environment (getEnvironment, lookupEnv)
import System.IO (hFlush, stdout)
import Data.Maybe (isJust, fromMaybe)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import qualified Data.Map.Strict as Map

-- 导入内存优化模块
import TestSupport.ExtremeQuickCheckMemoryOptimization 
  ( ExtremeMemoryConfig(..)
  , criticalMemoryConfig
  , minimalMemoryConfig
  , ultraMemoryConfig
  , withExtremeMemoryOptimization
  , extremeMemoryCleanup
  , getCurrentMemoryConfig
  )
import TestSupport.UnifiedAdaptiveMemoryOptimization 
  ( AdaptiveMemoryConfig(..)
  , MemoryTier(..)
  , detectSystemResources
  , createAdaptiveConfig
  , withResourceMonitoring
  , profileMemoryUsage
  , getRecommendedMemoryConfig
  )
import TestSupport.MemoryLimits 
  ( MemoryLevel(..)
  , withMemoryLimits
  , gcBetweenTests
  )
import TestSupport.EnhancedMemoryOptimization 
  ( enhancedMemoryCleanup
  , strategicMemoryCleanup
  , withEnhancedMemoryControl
  )

-- | 极度测试配置
data ExtremeTestConfig = ExtremeTestConfig
  { testBatchSize :: Int
  , maxMemoryMB :: Int
  } deriving (Show, Eq)

-- | 运行极度内存测试
runExtremeMemoryTests :: IO ()
runExtremeMemoryTests = do
  performGC
  putStrLn "Running extreme memory tests"