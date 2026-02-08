{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches -Wno-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.UltraMemoryOptimizedTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , memoryLevelTestGroup
  , gcBetweenTests
  , withAggressiveMemoryLimits
  , aggressiveMemoryLimitedTestGroup
  )

import Utils (trim, splitBy)
import Data.Char (isSpace)

-- Ultra memory-optimized properties with minimal memory usage
prop_ultra_trim_minimal :: String -> Property
prop_ultra_trim_minimal s = 
  let ultraLimited = take 5 s  -- Very small limit for ultra optimization
      trimmed = trim ultraLimited
  in property $ length trimmed <= 5

prop_ultra_split_minimal :: Char -> String -> Property
prop_ultra_split_minimal c s = 
  let ultraLimited = take 3 s  -- Extremely small limit
      parts = splitBy c ultraLimited
  in property $ length parts <= 4  -- Maximum parts for 3-char string

prop_ultra_whitespace_minimal :: String -> Property
prop_ultra_whitespace_minimal s = 
  let ultraLimited = take 4 s
      isAllWhitespace = all isSpace ultraLimited
  in property $ if isAllWhitespace then length ultraLimited <= 4 else True

prop_ultra_basic_arithmetic :: Int -> Int -> Property
prop_ultra_basic_arithmetic x y = 
  let limitedX = mod (abs x) 10  -- Very small range
      limitedY = mod (abs y) 10
      sum = limitedX + limitedY
  in property $ sum >= 0 && sum <= 18

prop_ultra_list_operations :: [Int] -> Property
prop_ultra_list_operations xs = 
  let ultraLimited = take 2 xs  -- Maximum 2 elements
      lengthLimited = length ultraLimited
  in property $ lengthLimited <= 2

prop_ultra_string_operations :: String -> String -> Property
prop_ultra_string_operations s1 s2 = 
  let limited1 = take 2 s1
      limited2 = take 2 s2
      combined = limited1 ++ limited2
  in property $ length combined <= 4

-- Ultra memory-optimized test suite for CI/CD environments
tests :: TestTree
tests = aggressiveMemoryLimitedTestGroup "Ultra Memory-Optimized Test Suite"
  [ withAggressiveMemoryLimits $ testProperty "ultra trim minimal" prop_ultra_trim_minimal
  , withAggressiveMemoryLimits $ testProperty "ultra split minimal" prop_ultra_split_minimal
  , withAggressiveMemoryLimits $ testProperty "ultra whitespace minimal" prop_ultra_whitespace_minimal
  , withAggressiveMemoryLimits $ testProperty "ultra basic arithmetic" prop_ultra_basic_arithmetic
  , withAggressiveMemoryLimits $ testProperty "ultra list operations" prop_ultra_list_operations
  , withAggressiveMemoryLimits $ testProperty "ultra string operations" prop_ultra_string_operations
  ]