{-# LANGUAGE OverloadedStrings #-}

-- | Universal Memory Optimization Module for QuickCheck Tests
-- This module provides a simple, universal interface for applying memory optimization
-- to any QuickCheck test file with minimal code changes.
module TestSupport.UniversalMemoryOptimization 
  ( -- * Universal memory optimization
    optimizeTestSuite
  , optimizeProperty
  , optimizeTestGroup
    
    -- * Pre-configured memory levels
  , emergencyLevel
  , lowLevel
  , moderateLevel
    
    -- * Memory-efficient generators
  , genSafeString
  , genSafeList
  , genSafeInt
    
    -- * Property helpers
  , limitString
  , limitList
  , limitInt
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryOptimizedQuickCheck
import Data.List (take)

-- | Pre-configured memory levels
emergencyLevel :: QuickCheckMemoryConfig
emergencyLevel = emergencyMemoryConfig

lowLevel :: QuickCheckMemoryConfig
lowLevel = lowMemoryConfig

moderateLevel :: QuickCheckMemoryConfig
moderateLevel = moderateMemoryConfig

-- | Universal test suite optimization - just wrap your tests with this
optimizeTestSuite :: QuickCheckMemoryConfig -> TestTree -> TestTree
optimizeTestSuite = applyQuickCheckMemoryConfig

-- | Universal property optimization
optimizeProperty :: QuickCheckMemoryConfig -> String -> Property -> TestTree
optimizeProperty config name prop = 
  applyQuickCheckMemoryConfig config $ testProperty name prop

-- | Universal test group optimization
optimizeTestGroup :: QuickCheckMemoryConfig -> String -> [TestTree] -> TestTree
optimizeTestGroup config name tests = 
  applyQuickCheckMemoryConfig config $ testGroup name tests

-- | Memory-efficient generators
genSafeString :: Gen String
genSafeString = genSmallString lowLevel

genSafeList :: Gen a -> Gen [a]
genSafeList = genSmallList lowLevel

genSafeInt :: Gen Int
genSafeInt = genSmallInt lowLevel

-- | Property helpers for automatic limiting
limitString :: Int -> String -> String
limitString = take

limitList :: Int -> [a] -> [a]
limitList = take

limitInt :: Int -> Int -> Int
limitInt maxVal x = if x > maxVal then maxVal else if x < -maxVal then -maxVal else x

-- | Quick optimization function for common patterns
quickOptimize :: TestTree -> TestTree
quickOptimize = optimizeTestSuite lowLevel

-- | Memory optimization with automatic string limiting
autoLimitStringProperty :: String -> (String -> Property) -> TestTree
autoLimitStringProperty name prop = 
  optimizeProperty lowLevel name $ \s -> prop (limitString 5 s)

-- | Memory optimization with automatic list limiting  
autoLimitListProperty :: Show a => String -> ([a] -> Property) -> TestTree
autoLimitListProperty name prop = 
  optimizeProperty lowLevel name $ \xs -> prop (limitList 3 xs)

-- | Memory optimization with automatic int limiting
autoLimitIntProperty :: String -> (Int -> Property) -> TestTree
autoLimitIntProperty name prop = 
  optimizeProperty lowLevel name $ \n -> prop (limitInt 10 n)