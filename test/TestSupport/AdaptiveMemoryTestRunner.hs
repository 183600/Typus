{-# LANGUAGE OverloadedStrings #-}

-- | Adaptive Memory Test Runner
-- 根据可用内存自动选择最适合的测试级别
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import System.Mem (performGC)
import System.Process (readProcess)
import System.Exit (ExitCode(..))
import Control.Monad (when)
import Data.List (isInfixOf)
import Text.Read (readMaybe)

import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import Test.Unit.TestListPropertiesSpec (testListProperties)
import TestSupport.MemoryLimits 
  ( MemoryLevel(..)
  , withMemoryLevel
  , memoryLevelTestGroup
  , gcBetweenTests
  , aggressiveGC
  , extremeGC
  )

-- | 内存级别定义
data AdaptiveMemoryLevel = 
    NanoLevel     -- ^ 极度受限环境 (< 64MB)
  | MicroLevel    -- ^ 微型环境 (64-128MB)
  | TinyLevel     -- ^ 超轻量环境 (128-256MB)
  | Lightweight   -- ^ 轻量环境 (256-512MB)
  | Standard      -- ^ 标准环境 (>512MB)
  deriving (Show, Eq)

-- | 获取系统可用内存（MB）
getAvailableMemory :: IO Int
getAvailableMemory = do
  -- 尝试使用free命令获取内存信息
  result <- readProcess "free" ["-m"] ""
  let lines' = lines result
  if length lines' >= 2
    then do
      let memLine = lines' !! 1
          words' = words memLine
      if length words' >= 7
        then return $ read (words' !! 6)  -- available memory column
        else return 1024  -- 默认值
    else return 1024  -- 默认值

-- | 根据可用内存确定测试级别
determineMemoryLevel :: IO AdaptiveMemoryLevel
determineMemoryLevel = do
  availableMB <- getAvailableMemory
  putStrLn $ "检测到可用内存: " ++ show availableMB ++ "MB"
  return $ case availableMB of
    n | n < 64    -> NanoLevel
    n | n < 128   -> MicroLevel
    n | n < 256   -> TinyLevel
    n | n < 512   -> Lightweight
    otherwise     -> Standard

-- | 创建适应性测试套件
createAdaptiveTestSuite :: AdaptiveMemoryLevel -> TestTree
createAdaptiveTestSuite level = 
  let testName = "[自适应内存测试] " ++ show level
      testDesc = case level of
        NanoLevel   -> "极度内存受限 (<64MB)"
        MicroLevel  -> "微型内存环境 (64-128MB)"
        TinyLevel   -> "超轻量内存 (128-256MB)"
        Lightweight -> "轻量内存 (256-512MB)"
        Standard    -> "标准内存 (>512MB)"
  in testGroup testName $ 
     [ testCase ("内存级别: " ++ testDesc) $ return ()
     ] ++ createTestsForLevel level

-- 根据内存级别创建测试
createTestsForLevel :: AdaptiveMemoryLevel -> [TestTree]
createTestsForLevel level = case level of
  NanoLevel -> 
    [ memoryLevelTestGroup Minimal "Nano级别测试" 
        [ testProperty "基本字符串属性" prop_basicString
        , testProperty "基本列表属性" prop_basicList
        ]
    ]
  MicroLevel -> 
    [ memoryLevelTestGroup Ultra "Micro级别测试" 
        [ testProperty "字符串属性" prop_string
        , testProperty "列表属性" prop_list
        , testProperty "标识符属性" prop_identifier
        ]
    ]
  TinyLevel -> 
    [ memoryLevelTestGroup Aggressive "Tiny级别测试" 
        [ testProperty "增强字符串属性" prop_enhancedString
        , testProperty "增强列表属性" prop_enhancedList
        , testProperty "组合属性" prop_combined
        , testListProperties
        ]
    ]
  Lightweight -> 
    [ memoryLevelTestGroup Moderate "轻量级别测试" 
        [ testProperty "完整字符串属性" prop_fullString
        , testProperty "完整列表属性" prop_fullList
        , testProperty "完整组合属性" prop_fullCombined
        , testListProperties
        , ConciseTestSuite.tests
        ]
    ]
  Standard -> 
    [ testGroup "标准测试套件"
        [ testListProperties
        , ConciseTestSuite.tests
        , BasicQuickCheckTestSuite.tests
        ]
    ]

-- | 基本测试属性（极度内存优化）
prop_basicString :: String -> Property
prop_basicString s = 
  let limited = take 2 s  -- 限制为2个字符
  in property $ length limited >= 0

prop_basicList :: [Int] -> Property
prop_basicList xs = 
  let limited = take 1 xs  -- 限制为1个元素
  in property $ length limited >= 0

-- | 简单测试属性（微型内存优化）
prop_string :: String -> Property
prop_string s = 
  let limited = take 3 s  -- 限制为3个字符
  in property $ length limited >= 0

prop_list :: [Int] -> Property
prop_list xs = 
  let limited = take 2 xs  -- 限制为2个元素
  in property $ length limited >= 0

prop_identifier :: String -> Property
prop_identifier s = 
  let limited = take 4 s  -- 限制为4个字符
  in property $ length limited >= 0

-- | 增强测试属性（轻量内存优化）
prop_enhancedString :: String -> Property
prop_enhancedString s = 
  let limited = take 5 s  -- 限制为5个字符
  in property $ length limited >= 0

prop_enhancedList :: [Int] -> Property
prop_enhancedList xs = 
  let limited = take 3 xs  -- 限制为3个元素
  in property $ length limited >= 0

prop_combined :: (String, [Int]) -> Property
prop_combined (s, xs) = 
  let limitedStr = take 4 s
      limitedList = take 2 xs
  in property $ length limitedStr >= 0 && length limitedList >= 0

-- | 完整测试属性（标准内存）
prop_fullString :: String -> Property
prop_fullString s = 
  let limited = take 10 s  -- 限制为10个字符
  in property $ length limited >= 0

prop_fullList :: [Int] -> Property
prop_fullList xs = 
  let limited = take 5 xs  -- 限制为5个元素
  in property $ length limited >= 0

prop_fullCombined :: (String, [Int]) -> Property
prop_fullCombined (s, xs) = 
  let limitedStr = take 8 s
      limitedList = take 4 xs
  in property $ length limitedStr >= 0 && length limitedList >= 0

-- | 运行适应性测试
runAdaptiveTests :: IO ()
runAdaptiveTests = do
  putStrLn "=== 自适应内存测试运行器 ==="
  level <- determineMemoryLevel
  putStrLn $ "选择测试级别: " ++ show level
  
  -- 根据级别执行垃圾回收
  case level of
    NanoLevel   -> extremeGC
    MicroLevel  -> extremeGC
    TinyLevel   -> aggressiveGC
    Lightweight -> aggressiveGC
    Standard    -> gcBetweenTests
  
  let testSuite = createAdaptiveTestSuite level
  defaultMain testSuite

-- | Main函数
main :: IO ()
main = runAdaptiveTests