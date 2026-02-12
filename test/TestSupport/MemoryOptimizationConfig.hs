{-# LANGUAGE OverloadedStrings #-}

-- | 内存优化配置文件
module TestSupport.MemoryOptimizationConfig where

-- | 默认内存限制（MB）
defaultMemoryLimit :: Int
defaultMemoryLimit = 32

-- | 最小内存限制（MB）
minimalMemoryLimit :: Int
minimalMemoryLimit = 16

-- | 最大测试大小
maxTestSize :: Int
maxTestSize = 2

-- | 测试数量
testCount :: Int
testCount = 5

-- | 垃圾回收频率
gcFrequency :: Int
gcFrequency = 1
