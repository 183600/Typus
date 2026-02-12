{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

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

-- | Micro generator configuration (minimal memory usage)
microGeneratorConfig :: GeneratorConfig
microGeneratorConfig = GeneratorConfig
  { maxStringLength = 2
  , maxListLength = 1
  , maxTreeDepth = 1
  , maxASTNodes = 2
  , usePrintableChars = True
  , useSimpleTypes = True
  , enableLazyGeneration = True
  , maxRecursionDepth = 1
  }

-- | Ultra light generator configuration (very low memory usage)
ultraLightGeneratorConfig :: GeneratorConfig
ultraLightGeneratorConfig = GeneratorConfig
  { maxStringLength = 3
  , maxListLength = 2
  , maxTreeDepth = 2
  , maxASTNodes = 3
  , usePrintableChars = True
  , useSimpleTypes = True
  , enableLazyGeneration = True
  , maxRecursionDepth = 2
  }

-- | Enhanced generator configuration (low memory usage)
enhancedGeneratorConfig :: GeneratorConfig
enhancedGeneratorConfig = GeneratorConfig
  { maxStringLength = 4
  , maxListLength = 3
  , maxTreeDepth = 3
  , maxASTNodes = 5
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

-- | Generate micro-sized strings (2 chars max)
generateMicroStrings :: Gen String
generateMicroStrings = sized $ \n -> do
  let size = min 2 (max 1 n)
  replicateM size $ elements $ take 32 ['a'..'z']

-- | Generate ultra light strings (3 chars max)
generateUltraLightStrings :: Gen String
generateUltraLightStrings = sized $ \n -> do
  let size = min 3 (max 1 n)
  replicateM size $ elements $ take 64 (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

-- | Generate enhanced strings (4 chars max)
generateEnhancedStrings :: Gen String
generateEnhancedStrings = sized $ \n -> do
  let size = min 4 (max 1 n)
  replicateM size $ elements $ take 96 (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " _-")

-- | Generate micro-sized lists (1 element max)
generateMicroLists :: Gen a -> Gen [a]
generateMicroLists gen = sized $ \n -> do
  let size = min 1 (max 1 n)
  replicateM size gen

-- | Generate ultra light lists (2 elements max)
generateUltraLightLists :: Gen a -> Gen [a]
generateUltraLightLists gen = sized $ \n -> do
  let size = min 2 (max 1 n)
  replicateM size gen

-- | Generate enhanced lists (3 elements max)
generateEnhancedLists :: Gen a -> Gen [a]
generateEnhancedLists gen = sized $ \n -> do
  let size = min 3 (max 1 n)
  replicateM size gen

-- | Generate micro-sized trees (depth 1)
generateMicroTrees :: Gen SimpleTree
generateMicroTrees = oneof
  [ Leaf <$> generateMicroStrings
  ]

-- | Generate ultra light trees (depth 2)
generateUltraLightTrees :: Gen SimpleTree
generateUltraLightTrees = oneof
  [ Leaf <$> generateUltraLightStrings
  , Node <$> generateMicroTrees <*> generateMicroTrees
  ]

-- | Generate enhanced trees (depth 3)
generateEnhancedTrees :: Gen SimpleTree
generateEnhancedTrees = oneof
  [ Leaf <$> generateEnhancedStrings
  , Node <$> generateUltraLightTrees <*> generateUltraLightTrees
  ]

-- | Generate mini AST (2 nodes max)
generateMiniAST :: Gen SimpleAST
generateMiniAST = oneof
  [ Literal <$> generateMicroStrings
  , Var <$> generateMicroStrings
  ]

-- | Generate compact AST (3 nodes max)
generateCompactAST :: Gen SimpleAST
generateCompactAST = oneof
  [ Literal <$> generateUltraLightStrings
  , Var <$> generateUltraLightStrings
  , Op <$> generateMiniAST <*> generateMiniAST
  ]

-- | Generate efficient AST (5 nodes max)
generateEfficientAST :: Gen SimpleAST
generateEfficientAST = oneof
  [ Literal <$> generateEnhancedStrings
  , Var <$> generateEnhancedStrings
  , Op <$> generateCompactAST <*> generateCompactAST
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

-- | Memory-efficient Arbitrary instances
instance Arbitrary SimpleTree where
  arbitrary = generateMicroTrees

instance Arbitrary SimpleAST where
  arbitrary = generateMiniAST

-- | Utility functions for common test data types