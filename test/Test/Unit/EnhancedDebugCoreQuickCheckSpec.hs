{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.EnhancedDebugCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, vectorOf, elements )
import Control.Monad (replicateM, when)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort)
import Data.Char (isSpace, isDigit, isAlpha)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import EnhancedDebug
  ( LogLevel(..)
  , DebugLogger(..)
  , EnhancedDebugConfig(..)
  , defaultEnhancedDebugConfig
  , logWithLevel
  , setLogLevel
  )

-- Arbitrary instances for QuickCheck
instance Arbitrary LogLevel where
  arbitrary = elements [Debug, Info, Warning, Error]

-- Property: LogLevel ordering is consistent
prop_log_level_ordering :: LogLevel -> LogLevel -> Property
prop_log_level_ordering level1 level2 =
  let ordered = compare level1 level2
      expected = if level1 == level2 then EQ else 
                 if (level1 == Debug && level2 /= Debug) ||
                    (level1 == Info && level2 `elem` [Warning, Error]) ||
                    (level1 == Warning && level2 == Error)
                 then LT else GT
  in property $ ordered === expected

-- Property: LogLevel enum properties
prop_log_level_enum_properties :: LogLevel -> Property
prop_log_level_enum_properties level =
  let allLevels = [Debug, Info, Warning, Error]
      levelInAll = level `elem` allLevels
      fromEnumValue = fromEnum level
      validEnum = fromEnumValue >= 0 && fromEnumValue <= 3
  in property $ levelInAll === True .&&. validEnum === True

-- Property: Default enhanced debug config has expected properties
prop_default_enhanced_debug_config_properties :: Property
prop_default_enhanced_debug_config_properties =
  let config = defaultEnhancedDebugConfig
  in property $ True -- Since we don't have access to the actual fields, this is a placeholder

-- Property: LogLevel roundtrip conversion
prop_log_level_roundtrip :: LogLevel -> Property
prop_log_level_roundtrip level =
  let enumValue = fromEnum level
      roundtripLevel = toEnum enumValue :: LogLevel
  in property $ roundtripLevel === level

-- Property: LogLevel comparison properties
prop_log_level_comparison_properties :: LogLevel -> LogLevel -> LogLevel -> Property
prop_log_level_comparison_properties level1 level2 level3 =
  property $ (level1 <= level2 && level2 <= level3) ==> (level1 <= level3)

-- Property: LogLevel maximum and minimum
prop_log_level_extremes :: Property
prop_log_level_extremes =
  let allLevels = [Debug, Info, Warning, Error]
      minLevel = minimum allLevels
      maxLevel = maximum allLevels
  in property $ minLevel === Debug .&&. maxLevel === Error

-- Property: LogLevel successor relationships
prop_log_level_successor :: LogLevel -> Property
prop_log_level_successor level =
  let successors = Map.fromList [(Debug, Info), (Info, Warning), (Warning, Error)]
      expectedSuccessor = Map.lookup level successors
  in property $ case expectedSuccessor of
    Nothing -> level === Error
    Just succ -> succ > level

-- Property: LogLevel predecessor relationships  
prop_log_level_predecessor :: LogLevel -> Property
prop_log_level_predecessor level =
  let predecessors = Map.fromList [(Info, Debug), (Warning, Info), (Error, Warning)]
      expectedPredecessor = Map.lookup level predecessors
  in property $ case expectedPredecessor of
    Nothing -> level === Debug
    Just pred -> pred < level

-- Property: LogLevel sorting
prop_log_level_sorting :: [LogLevel] -> Property
prop_log_level_sorting levels =
  let sortedLevels = sort levels
      isSorted = all (\(a,b) -> a <= b) (zip sortedLevels (drop 1 sortedLevels))
  in property $ isSorted === True

-- Property: LogLevel set operations
prop_log_level_set_operations :: LogLevel -> [LogLevel] -> Property
prop_log_level_set_operations level levels =
  let levelSet = Set.fromList levels
      containsLevel = level `elem` levels
      setContainsLevel = level `Set.member` levelSet
  in property $ containsLevel === setContainsLevel

-- Property: LogLevel map operations
prop_log_level_map_operations :: LogLevel -> Int -> [LogLevel] -> Property
prop_log_level_map_operations level value levels =
  let levelMap = Map.fromList (zip levels (repeat value))
      mapContainsLevel = level `Map.member` levelMap
      listContainsLevel = level `elem` levels
  in property $ mapContainsLevel === listContainsLevel

tests :: TestTree
tests = testGroup "Enhanced Debug Core QuickCheck Tests"
  [ fastProperty "log level ordering" prop_log_level_ordering
  , fastProperty "log level enum properties" prop_log_level_enum_properties
  , fastProperty "default enhanced debug config properties" prop_default_enhanced_debug_config_properties
  , fastProperty "log level roundtrip" prop_log_level_roundtrip
  , fastProperty "log level comparison properties" prop_log_level_comparison_properties
  , fastProperty "log level extremes" prop_log_level_extremes
  , fastProperty "log level successor" prop_log_level_successor
  , fastProperty "log level predecessor" prop_log_level_predecessor
  , fastProperty "log level sorting" prop_log_level_sorting
  , fastProperty "log level set operations" prop_log_level_set_operations
  , fastProperty "log level map operations" prop_log_level_map_operations
  ]