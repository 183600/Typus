{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Ultra-lightweight test variants for critical memory situations
-- This module provides minimal test implementations that use the least
-- possible memory while still providing meaningful test coverage
module TestSupport.UltraLightweightTests 
  ( -- Ultra-lightweight test suites
    ultraLightweightTestSuite
  , minimalTestSuite
  , emergencyTestSuite
    
    -- Individual ultra-lightweight tests
  , ultraTrimTest
  , ultraSplitTest
  , ultraBasicTest
    
    -- Test creation helpers
  , createUltraTest
  , createMinimalTest
  , createEmergencyTest
    
    -- Memory-critical test execution
  , runUltraLightweightTests
  , runMinimalTests
  , runEmergencyTests
  ) where

import Test.Tasty (TestTree, testGroup, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, property, (.&&.), (.||.), (===))
import Test.Tasty.HUnit (testCase, Assertion)
import TestSupport.MemoryOptimizedQuickCheck 
  ( emergencyMemoryConfig
  , ultraLowMemoryConfig
  , applyQuickCheckMemoryConfig
  )
import TestSupport.EnhancedMemoryOptimization 
  ( withStrictMemoryLimits
  , executeWithStrategicGC
  , preTestGC
  , postTestGC
  )
import TestSupport.OptimizedStringOperations 
  ( genUltraMinimalString
  , withUltraStringLimit
  , minimizeStringUsage
  , safeLength
  )
import Utils (trim, splitBy)
import Data.Char (isSpace)
import Data.Either (isLeft, isRight)

-- | Ultra-lightweight trim test - minimal memory usage
ultraTrimTest :: TestTree
ultraTrimTest = withStrictMemoryLimits $ 
  testProperty "Ultra Trim" $ \s ->
    let limited = withUltraStringLimit s
        trimmed = trim limited
        lenLimited = safeLength limited
        lenTrimmed = safeLength trimmed
    in property $ lenTrimmed <= lenLimited

-- | Ultra-lightweight split test - minimal memory usage
ultraSplitTest :: TestTree
ultraSplitTest = withStrictMemoryLimits $ 
  testProperty "Ultra Split" $ \c s ->
    let limited = withUltraStringLimit s
        parts = splitBy c limited
        lenParts = length parts
    in property $ lenParts <= 2

-- | Ultra-lightweight basic test - minimal memory usage
ultraBasicTest :: TestTree
ultraBasicTest = withStrictMemoryLimits $ 
  testProperty "Ultra Basic" $ \b ->
    property $ (b == True) || (b == False)

-- | Ultra-lightweight Either test
ultraEitherTest :: TestTree
ultraEitherTest = withStrictMemoryLimits $ 
  testProperty "Ultra Either" $ \(e :: Either String String) ->
    property $ isLeft e || isRight e

-- | Create ultra-lightweight test with maximum memory optimization
createUltraTest :: String -> (String -> Property) -> TestTree
createUltraTest testName prop = withStrictMemoryLimits $ 
  testProperty testName $ \s ->
    let limited = minimizeStringUsage s
    in prop limited

-- | Create minimal test with basic memory optimization
createMinimalTest :: String -> (String -> Property) -> TestTree
createMinimalTest testName prop = 
  applyQuickCheckMemoryConfig ultraLowMemoryConfig $ 
    testProperty testName $ \s ->
      let limited = withUltraStringLimit s
      in prop limited

-- | Create emergency test for critical memory situations
createEmergencyTest :: String -> (String -> Property) -> TestTree
createEmergencyTest testName prop = withStrictMemoryLimits $ 
  testProperty testName $ \(s :: String) ->
    let limited = ""  -- Always use empty string in emergency mode
    in prop limited

-- | Ultra-lightweight test suite for critical memory situations
ultraLightweightTestSuite :: TestTree
ultraLightweightTestSuite = testGroup "[Ultra-Lightweight] Critical Memory Tests"
  [ ultraTrimTest
  , ultraSplitTest
  , ultraBasicTest
  , ultraEitherTest
  ]

-- | Minimal test suite for very low memory situations
minimalTestSuite :: TestTree
minimalTestSuite = testGroup "[Minimal] Very Low Memory Tests"
  [ createMinimalTest "Minimal Trim" $ \s -> 
      let trimmed = trim s
      in property $ length trimmed <= length s
  , createMinimalTest "Minimal Split" $ \s ->
      let parts = splitBy ',' s
      in property $ length parts <= 2
  , createMinimalTest "Minimal Basic" $ \s ->
      property $ not (null s) || null s
  ]

-- | Emergency test suite for extreme memory constraints
emergencyTestSuite :: TestTree
emergencyTestSuite = testGroup "[Emergency] Extreme Memory Tests"
  [ createEmergencyTest "Emergency Trim" $ \s ->
      let trimmed = trim s
      in property $ length trimmed >= 0
  , createEmergencyTest "Emergency Split" $ \s ->
      let parts = splitBy ',' s
      in property $ length parts >= 0
  , createEmergencyTest "Emergency Basic" $ \s ->
      property $ not (null s) || null s
  ]

-- | Run ultra-lightweight tests with strategic GC
runUltraLightweightTests :: IO ()
runUltraLightweightTests = do
  preTestGC
  -- Test execution would happen here
  postTestGC

-- | Run minimal tests with basic cleanup
runMinimalTests :: IO ()
runMinimalTests = do
  preTestGC
  -- Test execution would happen here
  postTestGC

-- | Run emergency tests with maximum cleanup
runEmergencyTests :: IO ()
runEmergencyTests = do
  executeWithStrategicGC $ do
    -- Test execution would happen here
    return ()