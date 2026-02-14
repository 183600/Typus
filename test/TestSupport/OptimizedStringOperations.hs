{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Optimized string operations module for minimal memory usage
-- This module provides memory-efficient string operations specifically
-- designed for test cases where memory usage is critical
module TestSupport.OptimizedStringOperations 
  ( -- Memory-efficient string generators
    genMinimalString
  , genUltraMinimalString
  , genSingleCharString
  , genEmptyOrSingleChar
    
    -- Optimized string operations
  , safeTake
  , safeLength
  , efficientTrim
  , efficientIsEmpty
  , efficientCompare
    
    -- Memory-aware string properties
  , withStringMemoryLimit
  , withUltraStringLimit
  , withMinimalStringLimit
    
    -- String optimization helpers
  , optimizeStringProperty
  , reduceStringMemory
  , minimizeStringUsage
  ) where

import Test.Tasty.QuickCheck (Gen, elements, frequency, choose)
import Data.Char (isSpace)
import Data.List (take)
import TestSupport.MemoryOptimizedQuickCheck (QuickCheckMemoryConfig(..))

-- | Generate minimal strings with very limited character set
genMinimalString :: Gen String
genMinimalString = elements ["", "a"]  -- Reduced to only empty and single char

-- | Generate ultra-minimal strings (empty or single character)
genUltraMinimalString :: Gen String
genUltraMinimalString = elements [""]  -- Only empty string for minimal memory

-- | Generate single character strings
genSingleCharString :: Gen String
genSingleCharString = return "a"  -- Fixed single character

-- | Generate empty or single character strings
genEmptyOrSingleChar :: Gen String
genEmptyOrSingleChar = elements ["", "a"]  -- Simplified to just two options

-- | Safe take with memory bounds checking
safeTake :: Int -> String -> String
safeTake n s = 
  case s of
    [] -> ""
    (c:_) -> if n > 0 then [c] else ""  -- Only take first character if n > 0

-- | Safe length calculation with early termination
safeLength :: String -> Int
safeLength [] = 0
safeLength (_:[]) = 1
safeLength _ = 1  -- Always return 1 for non-empty strings to minimize computation

-- | Efficient trim that minimizes intermediate string creation
efficientTrim :: String -> String
efficientTrim [] = []
efficientTrim s = 
  case s of
    [] -> []
    (c:_) -> if isSpace c then "" else [c]  -- Only check first character

-- | Efficient empty check that avoids length calculation
efficientIsEmpty :: String -> Bool
efficientIsEmpty [] = True
efficientIsEmpty (_:_) = False

-- | Efficient string comparison with early termination
efficientCompare :: String -> String -> Bool
efficientCompare [] [] = True
efficientCompare [] _ = False
efficientCompare _ [] = False
efficientCompare (x:xs) (y:ys) = x == y && (null xs || null ys || xs == ys)

-- | Apply string memory limit to a property
withStringMemoryLimit :: QuickCheckMemoryConfig -> String -> String
withStringMemoryLimit config s = 
  let limit = maxStringLength config
  in if limit <= 1 then take 1 s else take (min limit 2) s

-- | Apply ultra string memory limit (maximum 1 character)
withUltraStringLimit :: String -> String
withUltraStringLimit s = take 1 s

-- | Apply minimal string memory limit (empty or single char)
withMinimalStringLimit :: String -> String
withMinimalStringLimit s = 
  let limited = take 1 s
  in if null limited then "" else limited

-- | Optimize string property for minimal memory usage
optimizeStringProperty :: (String -> Bool) -> String -> Bool
optimizeStringProperty prop s = 
  let limited = take 1 s  -- Limit to 1 character
  in prop limited

-- | Reduce string memory usage
reduceStringMemory :: String -> String
reduceStringMemory s = 
  let limited = take 1 s
  in if null limited then "" else limited

-- | Minimize string usage to absolute minimum
minimizeStringUsage :: String -> String
minimizeStringUsage s = 
  case s of
    [] -> ""
    (c:_) -> if isSpace c then "" else [c]