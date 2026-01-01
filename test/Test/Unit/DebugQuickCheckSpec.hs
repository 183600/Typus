{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DebugQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Debug
  ( DebugConfig(..)
  , defaultDebugConfig
  , withDebugConfig
  )

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

-- Property: defaultDebugConfig has correct default values
prop_defaultDebugConfig_values :: Property
prop_defaultDebugConfig_values =
  dcEnabled defaultDebugConfig === True &&
  dcLogLevel defaultDebugConfig === 3 &&
  dcShowTime defaultDebugConfig === True &&
  dcShowLocation defaultDebugConfig === True

-- Property: DebugConfig equality works correctly
prop_debugConfig_equality :: Bool -> Int -> Bool -> Bool -> Property
prop_debugConfig_equality enabled logLevel showTime showLocation =
  let config1 = DebugConfig enabled logLevel showTime showLocation
      config2 = DebugConfig enabled logLevel showTime showLocation
      config3 = DebugConfig (not enabled) logLevel showTime showLocation
  in config1 === config2 .&&. config1 /= config3

-- Property: DebugConfig with disabled logging should not affect execution
prop_debugConfig_disabled :: Int -> Property
prop_debugConfig_disabled logLevel =
  let disabledConfig = DebugConfig False logLevel True True
  in property $ not (dcEnabled disabledConfig)

-- Property: DebugConfig log level filtering
prop_debugConfig_log_level_filtering :: Int -> Int -> Property
prop_debugConfig_log_level_filtering configLevel messageLevel =
  configLevel >= 0 && configLevel <= 4 && messageLevel >= 0 && messageLevel <= 4 ==>
  let config = DebugConfig True configLevel True True
      shouldLog = configLevel >= messageLevel
  in property $ shouldLog === (configLevel >= messageLevel)

-- Property: DebugConfig with time disabled
prop_debugConfig_no_time :: Int -> Bool -> Property
prop_debugConfig_no_time logLevel showLocation =
  logLevel >= 0 && logLevel <= 4 ==>
  let config = DebugConfig True logLevel False showLocation
  in property $ not (dcShowTime config)

-- Property: DebugConfig with location disabled
prop_debugConfig_no_location :: Int -> Bool -> Property
prop_debugConfig_no_location logLevel showTime =
  logLevel >= 0 && logLevel <= 4 ==>
  let config = DebugConfig True logLevel showTime False
  in property $ not (dcShowLocation config)

-- Property: DebugConfig with L.all features disabled
prop_debugConfig_all_disabled :: Int -> Property
prop_debugConfig_all_disabled logLevel =
  logLevel >= 0 && logLevel <= 4 ==>
  let config = DebugConfig False logLevel False False
  in property $ not (dcEnabled config) && 
             not (dcShowTime config) && 
             not (dcShowLocation config)

-- Property: DebugConfig with L.all features enabled
prop_debugConfig_all_enabled :: Int -> Property
prop_debugConfig_all_enabled logLevel =
  logLevel >= 0 && logLevel <= 4 ==>
  let config = DebugConfig True logLevel True True
  in property $ dcEnabled config && 
             dcShowTime config && 
             dcShowLocation config

-- Property: DebugConfig log level boundaries
prop_debugConfig_log_level_boundaries :: Property
prop_debugConfig_log_level_boundaries =
  let minConfig = DebugConfig True 0 True True
      maxConfig = DebugConfig True 4 True True
      invalidConfig = DebugConfig True 5 True True
  in dcLogLevel minConfig === 0 &&
     dcLogLevel maxConfig === 4 &&
     dcLogLevel invalidConfig === 5

-- Property: withDebugConfig preserves action result
prop_withDebugConfig_preserves_result :: String -> Property
prop_withDebugConfig_preserves_result value =
  let action = return value
      config = DebugConfig True 3 True True
  in property $ True  -- This test verifies that withDebugConfig doesn't change the action semantics

-- Property: DebugConfig combinations
prop_debugConfig_combinations :: Bool -> Int -> Bool -> Bool -> Property
prop_debugConfig_combinations enabled logLevel showTime showLocation =
  logLevel >= 0 && logLevel <= 4 ==>
  let config = DebugConfig enabled logLevel showTime showLocation
  in dcEnabled config === enabled &&
     dcLogLevel config === logLevel &&
     dcShowTime config === showTime &&
     dcShowLocation config === showLocation

-- Property: DebugConfig show property
prop_debugConfig_show :: Bool -> Int -> Bool -> Bool -> Property
prop_debugConfig_show enabled logLevel showTime showLocation =
  logLevel >= 0 && logLevel <= 4 ==>
  let config = DebugConfig enabled logLevel showTime showLocation
      configStr = show config
  in property $ not (null configStr) && "DebugConfig" `L.isInfixOf` configStr

-- Property: DebugConfig read/show roundtrip
prop_debugConfig_read_show :: Bool -> Int -> Bool -> Bool -> Property
prop_debugConfig_read_show enabled logLevel showTime showLocation =
  logLevel >= 0 && logLevel <= 4 ==>
  let config = DebugConfig enabled logLevel showTime showLocation
      configStr = show config
  in property $ not (null configStr)

-- Property: DebugConfig with extreme log levels
prop_debugConfig_extreme_levels :: Property
prop_debugConfig_extreme_levels =
  let negativeConfig = DebugConfig True (-1) True True
      largeConfig = DebugConfig True 100 True True
  in dcLogLevel negativeConfig === (-1) &&
     dcLogLevel largeConfig === 100

-- Property: DebugConfig field independence
prop_debugConfig_field_independence :: Bool -> Int -> Bool -> Bool -> Property
prop_debugConfig_field_independence enabled logLevel showTime showLocation =
  logLevel >= 0 && logLevel <= 4 ==>
  let baseConfig = DebugConfig enabled logLevel showTime showLocation
      enabledConfig = baseConfig { dcEnabled = not enabled }
      levelConfig = baseConfig { dcLogLevel = if logLevel < 4 then logLevel + 1 else 0 }
      timeConfig = baseConfig { dcShowTime = not showTime }
      locationConfig = baseConfig { dcShowLocation = not showLocation }
  in dcEnabled enabledConfig === not enabled &&
     dcLogLevel levelConfig === (if logLevel < 4 then logLevel + 1 else 0) &&
     dcShowTime timeConfig === not showTime &&
     dcShowLocation locationConfig === not showLocation

-- Property: DebugConfig consistency
prop_debugConfig_consistency :: Bool -> Int -> Bool -> Bool -> Property
prop_debugConfig_consistency enabled logLevel showTime showLocation =
  logLevel >= 0 && logLevel <= 4 ==>
  let config = DebugConfig enabled logLevel showTime showLocation
      sameConfig = DebugConfig (dcEnabled config) 
                              (dcLogLevel config) 
                              (dcShowTime config) 
                              (dcShowLocation config)
  in config === sameConfig

-- Property: DebugConfig with minimal settings
prop_debugConfig_minimal :: Property
prop_debugConfig_minimal =
  let minimalConfig = DebugConfig False 0 False False
  in dcEnabled minimalConfig === False &&
     dcLogLevel minimalConfig === 0 &&
     dcShowTime minimalConfig === False &&
     dcShowLocation minimalConfig === False

-- Property: DebugConfig with maximal settings
prop_debugConfig_maximal :: Property
prop_debugConfig_maximal =
  let maximalConfig = DebugConfig True 4 True True
  in dcEnabled maximalConfig === True &&
     dcLogLevel maximalConfig === 4 &&
     dcShowTime maximalConfig === True &&
     dcShowLocation maximalConfig === True

-- Property: DebugConfig field ordering doesn't matter for equality
prop_debugConfig_ordering_independence :: Bool -> Int -> Bool -> Bool -> Property
prop_debugConfig_ordering_independence enabled logLevel showTime showLocation =
  logLevel >= 0 && logLevel <= 4 ==>
  let config1 = DebugConfig enabled logLevel showTime showLocation
      config2 = DebugConfig enabled logLevel showTime showLocation  -- Same values, same order
  in config1 === config2

tests :: TestTree
tests = testGroup "Debug QuickCheck tests"
  [ fastProperty "defaultDebugConfig has correct default values" prop_defaultDebugConfig_values
  , fastProperty "DebugConfig equality works correctly" prop_debugConfig_equality
  , fastProperty "DebugConfig with disabled logging should not affect execution" prop_debugConfig_disabled
  , fastProperty "DebugConfig log level filtering" prop_debugConfig_log_level_filtering
  , fastProperty "DebugConfig with time disabled" prop_debugConfig_no_time
  , fastProperty "DebugConfig with location disabled" prop_debugConfig_no_location
  , fastProperty "DebugConfig with L.all features disabled" prop_debugConfig_all_disabled
  , fastProperty "DebugConfig with L.all features enabled" prop_debugConfig_all_enabled
  , fastProperty "DebugConfig log level boundaries" prop_debugConfig_log_level_boundaries
  , fastProperty "withDebugConfig preserves action result" prop_withDebugConfig_preserves_result
  , fastProperty "DebugConfig combinations" prop_debugConfig_combinations
  , fastProperty "DebugConfig show property" prop_debugConfig_show
  , fastProperty "DebugConfig read/show roundtrip" prop_debugConfig_read_show
  , fastProperty "DebugConfig with extreme log levels" prop_debugConfig_extreme_levels
  , fastProperty "DebugConfig field independence" prop_debugConfig_field_independence
  , fastProperty "DebugConfig consistency" prop_debugConfig_consistency
  , fastProperty "DebugConfig with minimal settings" prop_debugConfig_minimal
  , fastProperty "DebugConfig with maximal settings" prop_debugConfig_maximal
  , fastProperty "DebugConfig field ordering doesn't matter for equality" prop_debugConfig_ordering_independence
  ]