{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.DebugCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, vectorOf, elements )
import Control.Monad (replicateM, when)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isDigit, isAlpha)

import Debug
  ( DebugConfig(..)
  , defaultDebugConfig
  , withDebugConfig
  )

-- Property: Debug config maintains logical consistency
prop_debug_config_consistency :: Bool -> Bool -> Bool -> Bool -> Property
prop_debug_config_consistency enabled showTime showLocation logLevel =
  let config = DebugConfig enabled logLevel showTime showLocation
      validLogLevel = logLevel >= 0 && logLevel <= 4
  in property $ validLogLevel ==> config === DebugConfig enabled logLevel showTime showLocation

-- Property: Default debug config has expected properties
prop_default_debug_config_properties :: Property
prop_default_debug_config_properties =
  let config = defaultDebugConfig
  in property $ dcEnabled config === True .&&.
     dcLogLevel config === 3 .&&.
     dcShowTime config === True .&&.
     dcShowLocation config === True

-- Property: Debug config equality is reflexive
prop_debug_config_reflexive :: DebugConfig -> Property
prop_debug_config_reflexive config =
  property $ config === config

-- Property: Debug config equality is symmetric
prop_debug_config_symmetric :: DebugConfig -> DebugConfig -> Property
prop_debug_config_symmetric config1 config2 =
  (config1 === config2) ==> (config2 === config1)

-- Property: Debug config modifications affect only intended fields
prop_debug_config_modification_isolation :: DebugConfig -> Bool -> Bool -> Bool -> Int -> Property
prop_debug_config_modification_isolation original enabled showTime showLocation logLevel =
  let validLogLevel = logLevel >= 0 && logLevel <= 4
      modified = original { dcEnabled = enabled, dcShowTime = showTime, dcShowLocation = showLocation, dcLogLevel = logLevel }
  in validLogLevel ==> 
     property $ dcEnabled modified === enabled .&&.
     dcShowTime modified === showTime .&&.
     dcShowLocation modified === showLocation .&&.
     dcLogLevel modified === logLevel

-- Property: Debug config log level bounds
prop_debug_config_log_level_bounds :: Int -> Property
prop_debug_config_log_level_bounds level =
  let config = DebugConfig True level True True
      validLevel = level >= 0 && level <= 4
  in property $ if validLevel 
     then dcLogLevel config === level
     else property $ dcLogLevel config >= 0 .&&. dcLogLevel config <= 4

-- Property: Debug config maintains field independence
prop_debug_config_field_independence :: Bool -> Int -> Bool -> Bool -> Bool -> Int -> Bool -> Bool -> Property
prop_debug_config_field_independence enabled1 logLevel1 showTime1 showLocation1 enabled2 logLevel2 showTime2 showLocation2 =
  let validLogLevel1 = logLevel1 >= 0 && logLevel1 <= 4
      validLogLevel2 = logLevel2 >= 0 && logLevel2 <= 4
      config1 = DebugConfig enabled1 logLevel1 showTime1 showLocation1
      config2 = DebugConfig enabled2 logLevel2 showTime2 showLocation2
  in (validLogLevel1 && validLogLevel2) ==> 
     (enabled1 /= enabled2 || logLevel1 /= logLevel2 || showTime1 /= showTime2 || showLocation1 /= showLocation2) ==>
     property $ config1 /= config2

-- Property: Debug config show L.and read consistency (basic)
prop_debug_config_show_read_consistency :: DebugConfig -> Property
prop_debug_config_show_read_consistency config =
  let configStr = show config
      hasExpectedFields = "DebugConfig" `L.isInfixOf` configStr
  in property $ hasExpectedFields === True

-- Property: Debug config handles extreme values
prop_debug_config_extreme_values :: Property
prop_debug_config_extreme_values =
  let minConfig = DebugConfig False 0 False False
      maxConfig = DebugConfig True 4 True True
  in property $ (dcLogLevel minConfig === 0 .&&. not (dcEnabled minConfig) .&&. not (dcShowTime minConfig) .&&. not (dcShowLocation minConfig)) .&&.
     (dcLogLevel maxConfig === 4 .&&. dcEnabled maxConfig .&&. dcShowTime maxConfig .&&. dcShowLocation maxConfig)

-- Property: Debug config sequence of modifications
prop_debug_config_modification_sequence :: DebugConfig -> [Bool] -> [Int] -> Property
prop_debug_config_modification_sequence initial enableds logLevels =
  let validLevels = L.filter (\l -> l >= 0 && l <= 4) logLevels
      modifyConfig config (enabled, level) = config { dcEnabled = enabled, dcLogLevel = level }
      finalConfig = foldl modifyConfig initial (zip enableds validLevels)
      lastEnabled = if null enableds then dcEnabled initial else last enableds
      lastLevel = if null validLevels then dcLogLevel initial else last validLevels
  in not (null validLevels) ==> 
     property $ dcEnabled finalConfig === lastEnabled .&&.
     dcLogLevel finalConfig === lastLevel

tests :: TestTree
tests = testGroup "Debug Core QuickCheck Tests"
  [ fastProperty "debug config consistency" prop_debug_config_consistency
  , fastProperty "default debug config properties" prop_default_debug_config_properties
  , fastProperty "debug config reflexive" prop_debug_config_reflexive
  , fastProperty "debug config symmetric" prop_debug_config_symmetric
  , fastProperty "debug config modification isolation" prop_debug_config_modification_isolation
  , fastProperty "debug config log level bounds" prop_debug_config_log_level_bounds
  , fastProperty "debug config field independence" prop_debug_config_field_independence
  , fastProperty "debug config show read consistency" prop_debug_config_show_read_consistency
  , fastProperty "debug config extreme values" prop_debug_config_extreme_values
  , fastProperty "debug config modification sequence" prop_debug_config_modification_sequence
  ]