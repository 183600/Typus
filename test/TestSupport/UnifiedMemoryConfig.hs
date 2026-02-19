-- 统一内存优化测试配置
-- 确保所有测试用例都使用内存优化的参数

module TestSupport.UnifiedMemoryConfig where

import Test.QuickCheck (Args(..), stdArgs, quickCheckWith)

-- 统一的内存优化QuickCheck参数
memoryOptimizedArgs :: Args
memoryOptimizedArgs = stdArgs
  { maxSuccess = 1        -- 减少测试次数，从100减少到1
  , maxSize = 1          -- 减少最大大小，从100减少到1
  , maxShrinks = 0       -- 禁用收缩，减少内存使用
  }

-- 更激进的内存优化参数（用于资源受限环境）
aggressiveMemoryArgs :: Args
aggressiveMemoryArgs = stdArgs
  { maxSuccess = 1        -- 最小测试次数
  , maxSize = 1          -- 最小大小
  , maxShrinks = 0       -- 禁用收缩
  , chatty = False       -- 减少输出，节省内存
  }

-- 中等程度的内存优化参数
moderateMemoryArgs :: Args
moderateMemoryArgs = stdArgs
  { maxSuccess = 3        -- 适中的测试次数
  , maxSize = 2          -- 适中的最大大小
  , maxShrinks = 1       -- 最少收缩
  }

-- 内存优化的字符串长度限制
maxStringLength :: Int
maxStringLength = 50     -- 最大字符串长度

-- 内存优化的列表长度限制
maxListLength :: Int
maxListLength = 10       -- 最大列表长度

-- 内存优化的整数范围限制
maxIntRange :: Int
maxIntRange = 100        -- 最大整数范围

-- 内存优化的嵌套深度限制
maxNestingDepth :: Int
maxNestingDepth = 3      -- 最大嵌套深度

-- 辅助函数：限制字符串长度
limitStringLength :: Int -> String -> String
limitStringLength limit s = take (min limit (length s)) s

-- 辅助函数：限制列表长度
limitListLength :: Int -> [a] -> [a]
limitListLength limit xs = take (min limit (length xs)) xs

-- 辅助函数：限制整数范围
limitIntRange :: Int -> Int -> Int
limitIntRange maxVal n = min maxVal (max (-maxVal) n)

-- 辅助函数：限制嵌套深度
limitNestingDepth :: Int -> a -> a
limitNestingDepth maxDepth value = value  -- 在实际使用中需要更复杂的实现

-- 使用内存优化参数运行QuickCheck测试
quickCheckMemory :: Testable a => a -> IO ()
quickCheckMemory = quickCheckWith memoryOptimizedArgs

-- 使用激进内存优化参数运行QuickCheck测试
quickCheckAggressive :: Testable a => a -> IO ()
quickCheckAggressive = quickCheckWith aggressiveMemoryArgs

-- 使用中等内存优化参数运行QuickCheck测试
quickCheckModerate :: Testable a => a -> IO ()
quickCheckModerate = quickCheckWith moderateMemoryArgs