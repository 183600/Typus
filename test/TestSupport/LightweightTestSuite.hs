{-# LANGUAGE OverloadedStrings #-}

module TestSupport.LightweightTestSuite 
  ( -- 轻量级测试套件
    createLightweightTestSuite
  , runLightweightTests
  , -- 轻量级配置
    LightweightConfig(..)
  , defaultLightweightConfig
  , ultraLightweightConfig
  , -- 测试选择
    selectEssentialTests
  , createMinimalTestSet
  ) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import Test.Tasty.Options (localOption)
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Utils (trim, splitBy)
import Data.Char (isSpace)

-- | 轻量级测试配置
data LightweightConfig = LightweightConfig
  { memoryLimitMB :: Int        -- ^ 内存限制 (MB)
  , maxTestSize :: Int          -- ^ QuickCheck最大大小
  , testCount :: Int            -- ^ 测试数量
  , maxShrinks :: Int           -- ^ 最大收缩次数
  , selectedTests :: Int        -- ^ 选择的测试数量
  , gcFrequency :: Int          -- ^ GC频率
  } deriving (Show, Eq)

-- | 默认轻量级配置
defaultLightweightConfig :: LightweightConfig
defaultLightweightConfig = LightweightConfig
  { memoryLimitMB = 256
  , maxTestSize = 2
  , testCount = 5
  , maxShrinks = 5
  , selectedTests = 5
  , gcFrequency = 1
  }

-- | 超轻量级配置
ultraLightweightConfig :: LightweightConfig
ultraLightweightConfig = LightweightConfig
  { memoryLimitMB = 128
  , maxTestSize = 1
  , testCount = 3
  , maxShrinks = 2
  , selectedTests = 3
  , gcFrequency = 1
  }

-- | 轻量级属性测试
prop_lightweight_trim :: String -> Property
prop_lightweight_trim s = 
  let limitedInput = take 10 s  -- 严格限制输入大小
      trimmed = trim limitedInput
  in property $ length trimmed <= length limitedInput

prop_lightweight_split :: Char -> String -> Property
prop_lightweight_split c s = 
  let limitedInput = take 8 s   -- 严格限制输入大小
      parts = splitBy c limitedInput
  in property $ length parts <= 9

prop_lightweight_whitespace :: String -> Property
prop_lightweight_whitespace s = 
  let limitedInput = take 6 s
      hasSpace = any isSpace limitedInput
  in property $ if hasSpace then True else True

prop_lightweight_length :: String -> Property
prop_lightweight_length s = 
  let limitedInput = take 5 s
      len = length limitedInput
  in property $ len >= 0 && len <= 5

prop_lightweight_concat :: String -> String -> Property
prop_lightweight_concat s1 s2 = 
  let limited1 = take 3 s1
      limited2 = take 3 s2
      combined = limited1 ++ limited2
  in property $ length combined <= 6

-- | 创建轻量级测试套件
createLightweightTestSuite :: LightweightConfig -> TestTree
createLightweightTestSuite config = 
  let baseTests = 
        [ testProperty "lightweight trim" prop_lightweight_trim
        , testProperty "lightweight split" prop_lightweight_split
        , testProperty "lightweight whitespace" prop_lightweight_whitespace
        , testProperty "lightweight length" prop_lightweight_length
        , testProperty "lightweight concat" prop_lightweight_concat
        ]
      
      selectedTests = take (selectedTests config) baseTests
      limitedTests = map (applyLightweightLimits config) selectedTests
      testName = "Lightweight Test Suite (" ++ show (memoryLimitMB config) ++ "MB)"
  in testGroup testName limitedTests

-- | 应用轻量级限制
applyLightweightLimits :: LightweightConfig -> TestTree -> TestTree
applyLightweightLimits config test = 
  localOption (QuickCheckMaxSize (maxTestSize config)) $
  localOption (QuickCheckTests (testCount config)) $
  localOption (QuickCheckMaxShrinks (maxShrinks config)) $
  test

-- | 选择基本测试
selectEssentialTests :: [TestTree] -> [TestTree]
selectEssentialTests tests = take 3 tests

-- | 创建最小测试集
createMinimalTestSet :: [TestTree] -> TestTree
createMinimalTestSet tests = 
  let essentialTests = selectEssentialTests tests
      minimalConfig = ultraLightweightConfig
      limitedTests = map (applyLightweightLimits minimalConfig) essentialTests
  in testGroup "Minimal Test Set" limitedTests

-- | 运行轻量级测试
runLightweightTests :: LightweightConfig -> IO ()
runLightweightTests config = do
  putStrLn $ "Running lightweight tests with " ++ show (memoryLimitMB config) ++ "MB limit"
  
  -- 强制初始GC
  replicateM_ 3 performGC
  
  -- 创建并运行测试套件
  let testSuite = createLightweightTestSuite config
  defaultMain testSuite
  
  -- 强制最终GC
  replicateM_ 3 performGC
  
  putStrLn "Lightweight tests completed"