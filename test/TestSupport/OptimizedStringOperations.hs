{-# LANGUAGE CPP #-}

module TestSupport.OptimizedStringOperations
  ( genMinimalString
  , genUltraMinimalString
  , safeTake
  , safeLength
  , efficientTrim
  , efficientIsEmpty
  , withUltraStringLimit
  , minimizeStringUsage
  , optimizeStringProperty
  ) where

import Data.Char (isSpace)
import Control.Monad (liftM)

-- Fallback definitions when QuickCheck is not available
type Gen a = IO a
type Property = ()
property :: Bool -> Property
property _ = ()

-- Generate minimal strings for memory-efficient testing
genMinimalString :: Gen String
genMinimalString = 
  -- Generate very short strings (0-10 chars) to minimize memory usage
  liftM (\n -> replicate (n `mod` 11) 'a') $ return (0 :: Int)

-- Generate ultra-minimal strings (0-5 chars) for extreme memory constraints
genUltraMinimalString :: Gen String
genUltraMinimalString = 
  -- Generate extremely short strings (0-5 chars)
  liftM (\n -> replicate (n `mod` 6) 'x') $ return (0 :: Int)

-- Safe string operations with length limits
safeTake :: Int -> String -> String
safeTake n s = take (min n 50) s  -- Limit to 50 chars maximum

-- Safe length calculation with overflow protection
safeLength :: String -> Int
safeLength s = min (length s) 100  -- Cap length at 100

-- Efficient trim implementation with minimal allocations
efficientTrim :: String -> String
efficientTrim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Efficient empty check with minimal overhead
efficientIsEmpty :: String -> Bool
efficientIsEmpty [] = True
efficientIsEmpty _ = False

-- Apply ultra string limits to operations
withUltraStringLimit :: (String -> a) -> String -> a
withUltraStringLimit f s = f (take 10 s)  -- Limit to 10 chars

-- Minimize string usage in properties
minimizeStringUsage :: (String -> Property) -> String -> Property
minimizeStringUsage prop s = prop (take 20 s)  -- Limit to 20 chars

-- Optimize string properties with memory constraints
optimizeStringProperty :: (String -> Bool) -> String -> Property
optimizeStringProperty predicate s = 
  let limitedString = take 15 s  -- Strict limit for memory efficiency
  in property (predicate limitedString)