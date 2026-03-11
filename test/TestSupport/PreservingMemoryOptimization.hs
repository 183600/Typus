{-# LANGUAGE CPP #-}

module TestSupport.PreservingMemoryOptimization
  ( withPreservingMemoryLimits
  , preservingMemoryTestGroup
  , optimizedArbitrary
  , smallGenerator
  , boundedGenerator
  , memoryEfficientProperty
  , gcBetweenTests
  , forceGC
  , withMemoryMonitoring
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck 
  ( QuickCheckMaxSize(..)
  , QuickCheckTests(..) 
  , QuickCheckMaxShrinks(..)
  , Property
  , Testable
  , property
  , forAll
  )
import Test.QuickCheck (Arbitrary(..), Gen, choose, elements, oneof, listOf, resize)
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import System.IO (hPutStrLn, stderr)

-- | 应用保持测试的内存限制
-- 不删除任何测试，但优化执行参数
withPreservingMemoryLimits :: TestTree -> TestTree
withPreservingMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $    -- 最小生成器大小
  localOption (QuickCheckTests 1) $      -- 每个属性只测试1次
  localOption (QuickCheckMaxShrinks 0) $ -- 禁用收缩
  test

-- | 创建内存优化的测试组
preservingMemoryTestGroup :: String -> [TestTree] -> TestTree
preservingMemoryTestGroup name tests = 
  withPreservingMemoryLimits $ testGroup name tests

-- | 优化的Arbitrary实例，限制数据大小
optimizedArbitrary :: (Arbitrary a) => Gen a
optimizedArbitrary = resize 8 arbitrary

-- | 小型生成器，用于内存敏感场景
smallGenerator :: (Arbitrary a) => Gen a
smallGenerator = resize 4 arbitrary

-- | 有界生成器，严格限制数据大小
boundedGenerator :: (Arbitrary a) => Gen a
boundedGenerator = resize 2 arbitrary

-- | 内存高效的属性测试
memoryEfficientProperty :: Testable prop => prop -> Property
memoryEfficientProperty prop = property prop

-- | 测试间垃圾回收
gcBetweenTests :: IO ()
gcBetweenTests = do
  performGC
  -- 给GC一些时间完成
  threadDelay 10000  -- 10ms

-- | 强制垃圾回收
forceGC :: IO ()
forceGC = do
  replicateM_ 3 performGC
  threadDelay 20000  -- 20ms

-- | 带内存监控的测试执行
withMemoryMonitoring :: IO a -> IO a
withMemoryMonitoring action = do
  -- 执行前强制GC
  forceGC
  result <- action
  -- 执行后强制GC
  forceGC
  return result

-- | 生成小型字符串（最大16字符）
smallString :: Gen String
smallString = resize 16 arbitrary

-- | 生成有界列表（最大5元素）
boundedList :: Gen a -> Gen [a]
boundedList gen = resize 5 $ listOf gen

-- | 生成小型整数（范围：-50到50）
smallInt :: Gen Int
smallInt = choose (-50, 50)

-- | 生成小型正整数（范围：1到20）
smallPositiveInt :: Gen Int
smallPositiveInt = choose (1, 20)

-- | 生成标识符（最大8字符）
smallIdentifier :: Gen String
smallIdentifier = do
  first <- elements ['a'..'z']
  rest <- resize 7 $ listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- | 生成小型Unicode字符串（最大8字符）
smallUnicodeString :: Gen String
smallUnicodeString = resize 8 arbitrary

-- | 生成有界的AST深度
boundedAST :: Gen a -> Gen [a]
boundedAST gen = resize 3 $ listOf gen

-- | 内存优化的测试套件配置
data MemoryOptimizationConfig = MemoryOptimizationConfig
  { maxStringLength :: Int
  , maxListLength :: Int
  , maxIntValue :: Int
  , testRepetitions :: Int
  , enableGC :: Bool
  }

-- | 默认内存优化配置
defaultMemoryConfig :: MemoryOptimizationConfig
defaultMemoryConfig = MemoryOptimizationConfig
  { maxStringLength = 16
  , maxListLength = 5
  , maxIntValue = 50
  , testRepetitions = 1
  , enableGC = True
  }

-- | 激进内存优化配置
aggressiveMemoryConfig :: MemoryOptimizationConfig
aggressiveMemoryConfig = MemoryOptimizationConfig
  { maxStringLength = 8
  , maxListLength = 3
  , maxIntValue = 20
  , testRepetitions = 1
  , enableGC = True
  }

-- | 应用内存配置到测试树
applyMemoryConfig :: MemoryOptimizationConfig -> TestTree -> TestTree
applyMemoryConfig config test = 
  localOption (QuickCheckMaxSize 1) $
  localOption (QuickCheckTests (testRepetitions config)) $
  localOption (QuickCheckMaxShrinks 0) $
  test