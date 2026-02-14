{-# LANGUAGE OverloadedStrings #-}

-- | Memory-optimized QuickCheck configurations
-- This module provides standardized QuickCheck configurations optimized
-- for different memory constraints to ensure consistent memory usage across all test suites
module TestSupport.MemoryOptimizedQuickCheck 
  ( -- Memory-optimized QuickCheck configurations
    QuickCheckMemoryConfig(..)
  , emergencyMemoryConfig
  , ultraLowMemoryConfig
  , criticalMemoryConfig
  , lowMemoryConfig
  , moderateMemoryConfig
  , normalMemoryConfig
  
    -- Configuration application
  , applyQuickCheckMemoryConfig
  , withQuickCheckMemoryConfig
  , createMemoryOptimizedProperty
  
    -- Memory-optimized generators
  , genSmallString
  , genSmallList
  , genSmallInt
  , genSmallBool
  , genLimitedChar
  , genLimitedEither
  , genLimitedMaybe
  
    -- Test property helpers
  , limitPropertySize
  , withMemoryLimit
  , withTestCountLimit
  , withShrinkLimit
  
    -- Memory-optimized properties
  , memoryOptimizedStringProperty
  , memoryOptimizedListProperty
  , memoryOptimizedIntProperty
  , memoryOptimizedBoolProperty
  
    -- Memory configuration helpers
  , getConfigForMemory
  ) where

import Test.Tasty (TestTree, localOption)
import Test.Tasty.QuickCheck 
  ( testProperty
  , Property
  , QuickCheckMaxSize(..)
  , QuickCheckTests(..)
  , QuickCheckMaxShrinks(..)
  , Gen
  , property
  , forAll
  , resize
  , suchThat
  , elements
  , listOf
  , oneof
  , frequency
  , choose
  , arbitrary
  , (.&&.)
  )
import Test.Tasty.HUnit (testCase)
import Data.Char (isLetter, isDigit, isSpace)
import Data.List (take)
import Control.Monad (replicateM)

-- | QuickCheck memory configuration
data QuickCheckMemoryConfig = QuickCheckMemoryConfig
  { maxTestSize :: Int           -- ^ Maximum test size
  , testCount :: Int             -- ^ Number of test cases
  , maxShrinks :: Int            -- ^ Maximum number of shrinks
  , maxStringLength :: Int       -- ^ Maximum string length
  , maxListLength :: Int         -- ^ Maximum list length
  , maxIntRange :: Int           -- ^ Maximum integer value
  , enableSizeLimiting :: Bool   -- ^ Enable size limiting
  , enableCountLimiting :: Bool  -- ^ Enable count limiting
  , enableShrinkLimiting :: Bool -- ^ Enable shrink limiting
  } deriving (Show, Eq)

-- | Emergency memory configuration (4MB) - Extreme emergency mode
emergencyMemoryConfig :: QuickCheckMemoryConfig
emergencyMemoryConfig = QuickCheckMemoryConfig
  { maxTestSize = 1
  , testCount = 1
  , maxShrinks = 0
  , maxStringLength = 1
  , maxListLength = 1
  , maxIntRange = 1
  , enableSizeLimiting = True
  , enableCountLimiting = True
  , enableShrinkLimiting = True
  }

-- | Ultra low memory configuration (8MB) - Emergency mode
ultraLowMemoryConfig :: QuickCheckMemoryConfig
ultraLowMemoryConfig = QuickCheckMemoryConfig
  { maxTestSize = 1
  , testCount = 1
  , maxShrinks = 0
  , maxStringLength = 1
  , maxListLength = 1
  , maxIntRange = 2
  , enableSizeLimiting = True
  , enableCountLimiting = True
  , enableShrinkLimiting = True
  }

-- | Critical memory configuration (16MB) - Critical mode
criticalMemoryConfig :: QuickCheckMemoryConfig
criticalMemoryConfig = QuickCheckMemoryConfig
  { maxTestSize = 1
  , testCount = 1
  , maxShrinks = 0
  , maxStringLength = 2
  , maxListLength = 1
  , maxIntRange = 3
  , enableSizeLimiting = True
  , enableCountLimiting = True
  , enableShrinkLimiting = True
  }

-- | Low memory configuration (32MB) - Low memory mode
lowMemoryConfig :: QuickCheckMemoryConfig
lowMemoryConfig = QuickCheckMemoryConfig
  { maxTestSize = 1
  , testCount = 2
  , maxShrinks = 0
  , maxStringLength = 3
  , maxListLength = 2
  , maxIntRange = 5
  , enableSizeLimiting = True
  , enableCountLimiting = True
  , enableShrinkLimiting = True
  }

-- | Moderate memory configuration (64MB) - Moderate mode
moderateMemoryConfig :: QuickCheckMemoryConfig
moderateMemoryConfig = QuickCheckMemoryConfig
  { maxTestSize = 2
  , testCount = 3
  , maxShrinks = 1
  , maxStringLength = 5
  , maxListLength = 3
  , maxIntRange = 10
  , enableSizeLimiting = True
  , enableCountLimiting = True
  , enableShrinkLimiting = True
  }

-- | Normal memory configuration (128MB) - Normal mode
normalMemoryConfig :: QuickCheckMemoryConfig
normalMemoryConfig = QuickCheckMemoryConfig
  { maxTestSize = 3
  , testCount = 5
  , maxShrinks = 2
  , maxStringLength = 8
  , maxListLength = 5
  , maxIntRange = 20
  , enableSizeLimiting = True
  , enableCountLimiting = True
  , enableShrinkLimiting = True
  }

-- | Apply QuickCheck memory configuration to a test tree
applyQuickCheckMemoryConfig :: QuickCheckMemoryConfig -> TestTree -> TestTree
applyQuickCheckMemoryConfig config test = 
  let sizeLimit = if enableSizeLimiting config then Just (maxTestSize config) else Nothing
      countLimit = if enableCountLimiting config then Just (testCount config) else Nothing
      shrinkLimit = if enableShrinkLimiting config then Just (maxShrinks config) else Nothing
      
      applySizeLimit t = case sizeLimit of
        Just size -> localOption (QuickCheckMaxSize size) t
        Nothing -> t
      
      applyCountLimit t = case countLimit of
        Just count -> localOption (QuickCheckTests count) t
        Nothing -> t
      
      applyShrinkLimit t = case shrinkLimit of
        Just shrinks -> localOption (QuickCheckMaxShrinks shrinks) t
        Nothing -> t
  in applySizeLimit $ applyCountLimit $ applyShrinkLimit test

-- | Create a test with QuickCheck memory configuration
withQuickCheckMemoryConfig :: Show a => QuickCheckMemoryConfig -> String -> (a -> Property) -> Gen a -> TestTree
withQuickCheckMemoryConfig config testName prop gen = 
  let limitedGen = limitGenerator config gen
      testProperty' = testProperty testName
      propertyWithGen = forAll limitedGen prop
      testTree = testProperty' propertyWithGen
  in applyQuickCheckMemoryConfig config testTree

-- | Create a memory-optimized property
createMemoryOptimizedProperty :: QuickCheckMemoryConfig -> String -> Property -> TestTree
createMemoryOptimizedProperty config testName prop = 
  let testTree = testProperty testName prop
  in applyQuickCheckMemoryConfig config testTree

-- | Limit generator based on memory configuration
limitGenerator :: QuickCheckMemoryConfig -> Gen a -> Gen a
limitGenerator config gen = resize (maxTestSize config) gen

-- | Generate small strings with memory limits
genSmallString :: QuickCheckMemoryConfig -> Gen String
genSmallString config = do
  size <- choose (0, maxStringLength config)
  replicateM size (genLimitedChar config)

-- | Generate small lists with memory limits
genSmallList :: QuickCheckMemoryConfig -> Gen a -> Gen [a]
genSmallList config gen = do
  size <- choose (0, maxListLength config)
  replicateM size gen

-- | Generate small integers with memory limits
genSmallInt :: QuickCheckMemoryConfig -> Gen Int
genSmallInt config = choose (-maxIntRange config, maxIntRange config)

-- | Generate small booleans (always just Bool)
genSmallBool :: Gen Bool
genSmallBool = elements [True, False]

-- | Generate limited characters
genLimitedChar :: QuickCheckMemoryConfig -> Gen Char
genLimitedChar config = 
  let safeChars = filter (\c -> isLetter c || isDigit c || isSpace c) [' '..'~']
  in elements (take (min 32 (maxStringLength config * 2)) safeChars)

-- | Generate limited Either values
genLimitedEither :: QuickCheckMemoryConfig -> Gen a -> Gen b -> Gen (Either a b)
genLimitedEither config genA genB = 
  oneof [fmap Left (limitGenerator config genA), fmap Right (limitGenerator config genB)]

-- | Generate limited Maybe values
genLimitedMaybe :: QuickCheckMemoryConfig -> Gen a -> Gen (Maybe a)
genLimitedMaybe config gen = 
  frequency [(1, return Nothing), (3, fmap Just (limitGenerator config gen))]

-- | Limit property size for memory optimization
-- Note: Property size limiting is handled by applyQuickCheckMemoryConfig via QuickCheckMaxSize
limitPropertySize :: QuickCheckMemoryConfig -> Property -> Property
limitPropertySize _ prop = prop

-- | Apply memory limit to a property
withMemoryLimit :: QuickCheckMemoryConfig -> (a -> Property) -> a -> Property
withMemoryLimit config prop x = limitPropertySize config (prop x)

-- | Apply test count limit to a property
-- Note: Test count limiting is handled by applyQuickCheckMemoryConfig via QuickCheckTests
withTestCountLimit :: QuickCheckMemoryConfig -> Property -> Property
withTestCountLimit _ prop = prop

-- | Apply shrink limit to a property
-- Note: Shrink limiting is handled by applyQuickCheckMemoryConfig via QuickCheckMaxShrinks
withShrinkLimit :: QuickCheckMemoryConfig -> Property -> Property
withShrinkLimit _ prop = prop

-- | Memory-optimized string property
memoryOptimizedStringProperty :: QuickCheckMemoryConfig -> String -> (String -> Bool) -> TestTree
memoryOptimizedStringProperty config testName propFunc = 
  withQuickCheckMemoryConfig config testName 
    (property . propFunc . take (maxStringLength config))
    (genSmallString config)

-- | Memory-optimized list property
memoryOptimizedListProperty :: Show a => QuickCheckMemoryConfig -> String -> ([a] -> Bool) -> Gen a -> TestTree
memoryOptimizedListProperty config testName propFunc gen = 
  withQuickCheckMemoryConfig config testName
    (property . propFunc . take (maxListLength config))
    (genSmallList config gen)

-- | Memory-optimized integer property
memoryOptimizedIntProperty :: QuickCheckMemoryConfig -> String -> (Int -> Bool) -> TestTree
memoryOptimizedIntProperty config testName propFunc = 
  withQuickCheckMemoryConfig config testName
    (property . propFunc . (`mod` (maxIntRange config)))
    (genSmallInt config)

-- | Memory-optimized boolean property
memoryOptimizedBoolProperty :: QuickCheckMemoryConfig -> String -> (Bool -> Bool) -> TestTree
memoryOptimizedBoolProperty config testName propFunc = 
  withQuickCheckMemoryConfig config testName
    (property . propFunc)
    genSmallBool

-- | Get configuration based on available memory
getConfigForMemory :: Int -> QuickCheckMemoryConfig
getConfigForMemory availableMB
  | availableMB <= 8 = emergencyMemoryConfig
  | availableMB <= 16 = ultraLowMemoryConfig
  | availableMB <= 32 = criticalMemoryConfig
  | availableMB <= 64 = lowMemoryConfig
  | availableMB <= 128 = moderateMemoryConfig
  | otherwise = normalMemoryConfig

-- | Apply automatic memory optimization based on available memory
withAutomaticMemoryOptimization :: Show a => Int -> String -> (a -> Property) -> Gen a -> IO TestTree
withAutomaticMemoryOptimization availableMB testName propFunc gen = do
  let config = getConfigForMemory availableMB
  return $ withQuickCheckMemoryConfig config testName propFunc gen