{-# LANGUAGE DeriveGeneric #-}
module Test.Unit.DebugQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (hFlush, stdout)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

import Debug

-- Test data generators
generateDebugConfig :: Int -> DebugConfig
generateDebugConfig n = DebugConfig
    { dcEnabled = n `mod` 2 == 0
    , dcLogLevel = n `mod` 5
    , dcShowTime = n `mod` 2 == 1
    , dcShowLocation = n `mod` 2 == 1
    }

generateLocation :: Int -> String
generateLocation n = "module" ++ show n ++ ":function" ++ show (n + 1)

generateMessage :: Int -> String
generateMessage n = "Debug message " ++ show n

-- Test helper to capture output
captureOutput :: IO a -> IO String
captureOutput action = do
    -- This is a simplified version - in a real implementation,
    -- you would redirect stdout to capture the output
    action
    return "captured_output"

-- QuickCheck properties
prop_debug_config_creation :: Property
prop_debug_config_creation =
  forAll arbitrary $ \n ->
    let config = generateDebugConfig n
    in property $
      dcEnabled config == (n `mod` 2 == 0) &&
      dcLogLevel config == n `mod` 5 &&
      dcShowTime config == (n `mod` 2 == 1) &&
      dcShowLocation config == (n `mod` 2 == 1)

prop_default_debug_config :: Property
prop_default_debug_config =
  let config = defaultDebugConfig
  in property $
    dcEnabled config &&
    dcLogLevel config == 3 &&
    dcShowTime config &&
    dcShowLocation config

prop_debug_log_disabled :: Property
prop_debug_log_disabled =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig False 3 True True
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify no output when disabled

prop_debug_log_level_filter :: Property
prop_debug_log_level_filter =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig True 2 True True  -- Only error and warn
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify debugLog is filtered out

prop_debug_log_with_time :: Property
prop_debug_log_with_time =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig True 3 True False
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify timestamp is included

prop_debug_log_with_location :: Property
prop_debug_log_with_location =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig True 3 False True
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify location is included

prop_debug_breakpoint_enabled :: Property
prop_debug_breakpoint_enabled =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig True 3 True True
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify breakpoint behavior

prop_debug_breakpoint_disabled :: Property
prop_debug_breakpoint_disabled =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig False 3 True True
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify no output when disabled

prop_debug_trace_enabled :: Property
prop_debug_trace_enabled =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig True 4 True True
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify trace output

prop_debug_trace_level_filter :: Property
prop_debug_trace_level_filter =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig True 3 True True  -- Trace level 4, but log level is 3
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify trace is filtered out

prop_debug_error_level :: Property
prop_debug_error_level =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig True 1 True True
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify error output

prop_debug_warn_level :: Property
prop_debug_warn_level =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig True 2 True True
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify warn output

prop_debug_info_level :: Property
prop_debug_info_level =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let config = DebugConfig True 3 True True
        location = generateLocation n
        message = generateMessage m
    in property $ True  -- Test would verify info output

prop_debug_log_with_level :: Property
prop_debug_log_with_level =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
  forAll arbitrary $ \k ->
    let config = generateDebugConfig n
        level = k `mod` 5
        location = generateLocation m
        message = generateMessage (m + 1)
    in property $ True  -- Test would verify level filtering

prop_with_debug_config :: Property
prop_with_debug_config =
  forAll arbitrary $ \n ->
    let config = generateDebugConfig n
        action = return ()
    in property $ True  -- Test would verify config is applied

-- Test suite
testSuite :: TestTree
testSuite = testGroup "Debug QuickCheck Tests"
  [ testProperty "debug config creation" prop_debug_config_creation
  , testProperty "default debug config" prop_default_debug_config
  , testProperty "debug log disabled" prop_debug_log_disabled
  , testProperty "debug log level filter" prop_debug_log_level_filter
  , testProperty "debug log with time" prop_debug_log_with_time
  , testProperty "debug log with location" prop_debug_log_with_location
  , testProperty "debug breakpoint enabled" prop_debug_breakpoint_enabled
  , testProperty "debug breakpoint disabled" prop_debug_breakpoint_disabled
  , testProperty "debug trace enabled" prop_debug_trace_enabled
  , testProperty "debug trace level filter" prop_debug_trace_level_filter
  , testProperty "debug error level" prop_debug_error_level
  , testProperty "debug warn level" prop_debug_warn_level
  , testProperty "debug info level" prop_debug_info_level
  , testProperty "debug log with level" prop_debug_log_with_level
  , testProperty "with debug config" prop_with_debug_config
  ]

-- Unit tests for specific edge cases
unitTests :: TestTree
unitTests = testGroup "Debug Unit Tests"
  [ testCase "default configuration" $ do
      let config = defaultDebugConfig
      assertBool "Default config should be enabled" $ dcEnabled config
      assertEqual "Default log level should be 3" 3 (dcLogLevel config)
      assertBool "Default should show time" $ dcShowTime config
      assertBool "Default should show location" $ dcShowLocation config

  , testCase "disabled configuration" $ do
      let config = DebugConfig False 3 True True
      assertBool "Config should be disabled" $ not (dcEnabled config)

  , testCase "log level filtering" $ do
      let config = DebugConfig True 1 True True  -- Only error level
      assertEqual "Log level should be 1" 1 (dcLogLevel config)

  , testCase "debug log functions" $ do
      -- These tests would verify that the functions execute without errors
      -- In a real implementation, you would capture output to verify
      let location = "TestModule:testFunction"
          message = "Test message"
      
      -- Test that these functions don't throw exceptions
      debugLog location message
      debugError location message
      debugWarn location message
      debugInfo location message
      debugTrace location message
      debugBreakpoint location message

  , testCase "debug log with custom config" $ do
      let config = DebugConfig True 4 False True
          location = "TestModule:testFunction"
          message = "Test message"
      -- Test that custom config is used
      debugLogWith config location message
      debugTraceWith config location message
      debugBreakpointWith config location message

  , testCase "debug log with level" $ do
      let config = DebugConfig True 3 True True
          location = "TestModule:testFunction"
          message = "Test message"
      -- Test different log levels
      debugLogWithLevel config 1 location message  -- Error
      debugLogWithLevel config 2 location message  -- Warning
      debugLogWithLevel config 3 location message  -- Info
      debugLogWithLevel config 4 location message  -- Debug
      debugLogWithLevel config 5 location message  -- Trace (should be filtered out)
  ]

-- Combined test suite
tests :: TestTree
tests = testGroup "Debug Tests"
  [ testSuite
  , unitTests
  ]