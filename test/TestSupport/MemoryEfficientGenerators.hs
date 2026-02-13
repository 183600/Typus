{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Memory-efficient test data generators for Typus test suites
-- This module provides optimized data generators that minimize memory usage
module TestSupport.MemoryEfficientGenerators
  ( -- Memory-efficient string generators
    generateMicroStrings
  , generateUltraLightStrings
  , generateEnhancedStrings
  , -- Memory-efficient list generators
    generateMicroLists
  , generateUltraLightLists
  , generateEnhancedLists
  , -- Memory-efficient tree generators
    generateMicroTrees
  , generateUltraLightTrees
  , generateEnhancedTrees
  , -- Memory-efficient AST generators
    generateMiniAST
  , generateCompactAST
  , generateEfficientAST
  , -- Utility functions
    limitStringSize
  , limitListSize
  , limitRecursion
  , createMemoryEfficientArbitrary
  , -- Generator configurations
    GeneratorConfig(..)
  , microGeneratorConfig
  , ultraLightGeneratorConfig
  , enhancedGeneratorConfig
  ) where

import Test.Tasty.QuickCheck (Gen, Arbitrary(..), sized, scale, oneof, elements, arbitrary)
import Control.Monad (replicateM)

-- | Configuration for memory-efficient generators
data GeneratorConfig = GeneratorConfig
  { maxStringLength :: Int        -- ^ Maximum string length
  , maxListLength :: Int          -- ^ Maximum list length
  , maxTreeDepth :: Int           -- ^ Maximum tree depth
  , maxASTNodes :: Int            -- ^ Maximum AST nodes
  , usePrintableChars :: Bool     -- ^ Use only printable characters
  , useSimpleTypes :: Bool        -- ^ Use simple types only
  , enableLazyGeneration :: Bool  -- ^ Enable lazy generation
  , maxRecursionDepth :: Int      -- ^ Maximum recursion depth
  } deriving (Show, Eq)

-- | Micro generator configuration (minimal memory usage) - 进一步优化
microGeneratorConfig :: GeneratorConfig
microGeneratorConfig = GeneratorConfig
  { maxStringLength = 1
  , maxListLength = 1
  , maxTreeDepth = 1
  , maxASTNodes = 1
  , usePrintableChars = True
  , useSimpleTypes = True
  , enableLazyGeneration = False  -- 禁用惰性生成以减少内存持有
  , maxRecursionDepth = 1
  }

-- | Ultra light generator configuration (very low memory usage)
ultraLightGeneratorConfig :: GeneratorConfig
ultraLightGeneratorConfig = GeneratorConfig
  { maxStringLength = 2
  , maxListLength = 2
  , maxTreeDepth = 2
  , maxASTNodes = 2
  , usePrintableChars = True
  , useSimpleTypes = True
  , enableLazyGeneration = True
  , maxRecursionDepth = 2
  }

-- | Enhanced generator configuration (low memory usage)
enhancedGeneratorConfig :: GeneratorConfig
enhancedGeneratorConfig = GeneratorConfig
  { maxStringLength = 3
  , maxListLength = 2
  , maxTreeDepth = 2
  , maxASTNodes = 3
  , usePrintableChars = True
  , useSimpleTypes = True
  , enableLazyGeneration = True
  , maxRecursionDepth = 2
  }

-- | Standard generator configuration (balanced memory usage)
standardGeneratorConfig :: GeneratorConfig
standardGeneratorConfig = GeneratorConfig
  { maxStringLength = 4
  , maxListLength = 3
  , maxTreeDepth = 3
  , maxASTNodes = 4
  , usePrintableChars = True
  , useSimpleTypes = False
  , enableLazyGeneration = True
  , maxRecursionDepth = 3
  }

-- | Simple data types for testing
data SimpleTree = Leaf String | Node SimpleTree SimpleTree
  deriving (Show, Eq)

data SimpleAST = Literal String | Var String | Op SimpleAST SimpleAST
  deriving (Show, Eq)

-- | Generate micro-sized strings (1 char max) - 内存优化版本
generateMicroStrings :: Gen String
generateMicroStrings = sized $ \n -> do
  let size = min 1 (max 0 n)
      -- 使用更小的字符集，减少内存占用
      microChars = ['a', 'b', 'c']  -- 仅使用3个字符
  replicateM size $ elements microChars

-- | Generate ultra light strings (2 chars max)
generateUltraLightStrings :: Gen String
generateUltraLightStrings = sized $ \n -> do
  let size = min 2 (max 0 n)
  replicateM size $ elements $ take 16 ['a'..'z']

-- | Generate enhanced strings (3 chars max)
generateEnhancedStrings :: Gen String
generateEnhancedStrings = sized $ \n -> do
  let size = min 3 (max 0 n)
  replicateM size $ elements $ take 24 ['a'..'z']

-- | Generate micro-sized lists (0-1 elements max) - 内存优化版本
generateMicroLists :: Gen a -> Gen [a]
generateMicroLists gen = sized $ \n -> do
  let size = min 1 (max 0 n)
  -- 使用严格的列表生成，避免惰性求值的内存开销
  if size == 0
  then return []
  else do
    elem <- gen
    return [elem]  -- 明确返回单元素列表

-- | Generate ultra light lists (0-2 elements max)
generateUltraLightLists :: Gen a -> Gen [a]
generateUltraLightLists gen = sized $ \n -> do
  let size = min 2 (max 0 n)
  replicateM size gen

-- | Generate enhanced lists (0-2 elements max)
generateEnhancedLists :: Gen a -> Gen [a]
generateEnhancedLists gen = sized $ \n -> do
  let size = min 2 (max 0 n)
  replicateM size gen

-- | Generate micro-sized trees (depth 1, leaf only)
generateMicroTrees :: Gen SimpleTree
generateMicroTrees = 
  Leaf <$> generateMicroStrings

-- | Generate ultra light trees (depth 1-2, mostly leaves)
generateUltraLightTrees :: Gen SimpleTree
generateUltraLightTrees = oneof
  [ Leaf <$> generateMicroStrings
  , Leaf <$> generateUltraLightStrings
  , Node <$> generateMicroTrees <*> generateMicroTrees
  ]

-- | Generate enhanced trees (depth 2, conservative)
generateEnhancedTrees :: Gen SimpleTree
generateEnhancedTrees = oneof
  [ Leaf <$> generateMicroStrings
  , Leaf <$> generateUltraLightStrings
  , Node <$> generateMicroTrees <*> generateMicroTrees
  ]

-- | Generate mini AST (1 node max)
generateMiniAST :: Gen SimpleAST
generateMiniAST = oneof
  [ Literal <$> generateMicroStrings
  , Var <$> generateMicroStrings
  ]

-- | Generate compact AST (1-2 nodes max)
generateCompactAST :: Gen SimpleAST
generateCompactAST = oneof
  [ Literal <$> generateMicroStrings
  , Var <$> generateMicroStrings
  , Literal <$> generateUltraLightStrings
  ]

-- | Generate efficient AST (1-2 nodes max)
generateEfficientAST :: Gen SimpleAST
generateEfficientAST = oneof
  [ Literal <$> generateMicroStrings
  , Var <$> generateMicroStrings
  , Literal <$> generateUltraLightStrings
  , Var <$> generateUltraLightStrings
  ]

-- | Limit string size based on configuration
limitStringSize :: GeneratorConfig -> String -> String
limitStringSize config s = take (maxStringLength config) s

-- | Limit list size based on configuration
limitListSize :: GeneratorConfig -> [a] -> [a]
limitListSize config xs = take (maxListLength config) xs

-- | Limit recursion based on configuration
limitRecursion :: GeneratorConfig -> Int -> Int
limitRecursion config depth = min depth (maxRecursionDepth config)

-- | Create memory-efficient Arbitrary instance
createMemoryEfficientArbitrary :: GeneratorConfig -> Gen a -> Gen a
createMemoryEfficientArbitrary config gen = 
  if enableLazyGeneration config
  then sized $ \_ -> scale (const (maxTreeDepth config)) gen
  else scale (const (maxTreeDepth config)) gen

-- | 极端内存优化的字符串生成器（仅使用单个字符）
generateExtremeStrings :: Gen String
generateExtremeStrings = elements ["a", "b", "c"]  -- 预定义的极小字符串集合

-- | 极端内存优化的列表生成器（空列表或单元素）
generateExtremeLists :: Gen a -> Gen [a]
generateExtremeLists gen = oneof [return [], fmap return gen]

-- | 极端内存优化的树生成器（仅叶子节点）
generateExtremeTrees :: Gen SimpleTree
generateExtremeTrees = Leaf <$> generateExtremeStrings

-- | 极端内存优化的AST生成器（仅字面量）
generateExtremeAST :: Gen SimpleAST
generateExtremeAST = Literal <$> generateExtremeStrings

-- | 极端内存配置（用于极度受限的环境）
extremeGeneratorConfig :: GeneratorConfig
extremeGeneratorConfig = GeneratorConfig
  { maxStringLength = 1
  , maxListLength = 1
  , maxTreeDepth = 1
  , maxASTNodes = 1
  , usePrintableChars = True
  , useSimpleTypes = True
  , enableLazyGeneration = False
  , maxRecursionDepth = 1
  }

-- | Memory-efficient Arbitrary instances
instance Arbitrary SimpleTree where
  arbitrary = generateMicroTrees

instance Arbitrary SimpleAST where
  arbitrary = generateMiniAST

-- | Note: We don't redefine Arbitrary String or [a] since QuickCheck already provides them.
-- Use generateMicroStrings, generateMicroLists directly if you need memory-efficient generation.

-- | Utility functions for common test data types