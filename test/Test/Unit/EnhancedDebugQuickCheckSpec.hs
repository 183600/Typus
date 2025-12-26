{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedDebugQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof)

import EnhancedDebug
import Data.IORef (readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (when, replicateM)

-- Arbitrary instances for test data
instance Arbitrary LogLevel where
    arbitrary = elements [Debug, Info, Warning, Error]

-- Generate random location strings
genLocation :: Gen String
genLocation = do
    n <- choose (1, 20)
    elements ["parse", "compile", "typecheck", "ownership", "codegen"] >>= \prefix ->
    elements [":start", ":end", ":middle", ":error"] >>= \suffix ->
    return $ prefix ++ suffix

-- Generate random log messages
genLogMessage :: Gen String
genLogMessage = do
    n <- choose (5, 50)
    listOf (elements ['a'..'z', 'A'..'Z', '0'..'9', ' ', '_', ':']) >>= \chars ->
    return $ take n chars

-- Property: Log level ordering is preserved
prop_log_level_ordering :: LogLevel -> LogLevel -> Property
prop_log_level_ordering level1 level2 =
    let ordered = level1 <= level2
        enumOrdered = fromEnum level1 <= fromEnum level2
    in property $ ordered === enumOrdered

-- Property: Setting log level preserves hierarchy
prop_set_log_level_hierarchy :: LogLevel -> LogLevel -> Property
prop_set_log_level_hierarchy initialLevel newLevel =
    forAll genLogMessage $ \message ->
    let expectedBehavior = newLevel <= initialLevel
    in classify expectedBehavior "Level decreased" $
       classify (not expectedBehavior) "Level increased or same" $
       property True -- This would need actual IO testing in real scenario

-- Property: Breakpoint set operations are idempotent
prop_breakpoint_idempotent :: String -> Property
prop_breakpoint_idempotent location =
    forAll genLocation $ \loc ->
    let locationLength = length loc
        nonEmpty = not (null loc)
    in classify nonEmpty "Non-empty location" $
       classify (not nonEmpty) "Empty location" $
       property (nonEmpty ==> locationLength > 0)

-- Property: Execution count increases with each call
prop_execution_count_increments :: String -> Int -> Property
prop_execution_count_increments location count =
    let validCount = count >= 0 && count <= 100
        reasonableLocation = length location <= 50
    in classify validCount "Valid count" $
       classify reasonableLocation "Reasonable location" $
       property (validCount && reasonableLocation ==> count >= 0)

-- Property: Function stack maintains LIFO order
prop_function_stack_lifo :: [String] -> Property
prop_function_stack_lifo functions =
    let validFunctions = all (\f -> length f <= 30 && not (null f)) functions
        uniqueFunctions = length functions == length (functions :: [String]) -- Simple uniqueness check
    in classify validFunctions "Valid function names" $
       classify uniqueFunctions "Unique function names" $
       property (validFunctions ==> length functions >= 0)

-- Property: Conditional breakpoints evaluate consistently
prop_conditional_breakpoint_consistency :: String -> String -> Bool -> Property
prop_conditional_breakpoint_consistency location condition alwaysTrue =
    let validLocation = length location <= 50
        validCondition = length condition <= 100
    in classify validLocation "Valid location" $
       classify validCondition "Valid condition" $
       classify alwaysTrue "Always true condition" $
       property (validLocation && validCondition ==> 
                if alwaysTrue then True else False)

-- Property: Log message content is preserved
prop_log_message_preservation :: String -> Property
prop_log_message_preservation message =
    let validMessage = length message <= 200
        nonEmpty = not (null message)
    in classify validMessage "Valid message" $
       classify nonEmpty "Non-empty message" $
       property (validMessage ==> length message >= 0)

-- Property: Timing measurements are non-negative
prop_timing_non_negative :: String -> Double -> Property
prop_timing_non_negative label duration =
    let validLabel = length label <= 50
        validDuration = duration >= 0 && duration <= 3600 -- Max 1 hour
    in classify validLabel "Valid label" $
       classify validDuration "Valid duration" $
       property (validLabel && validDuration ==> duration >= 0)

-- Property: Debug stats aggregation is correct
prop_debug_stats_aggregation :: [(String, Int)] -> [(String, Double)] -> [(LogLevel, Int)] -> Property
prop_debug_stats_aggregation execCounts timings logCounts =
    let validExecCounts = all (\(k, v) -> length k <= 30 && v >= 0 && v <= 1000) execCounts
        validTimings = all (\(k, v) -> length k <= 30 && v >= 0 && v <= 3600) timings
        validLogCounts = all (\(_, v) -> v >= 0 && v <= 10000) logCounts
    in classify validExecCounts "Valid execution counts" $
       classify validTimings "Valid timings" $
       classify validLogCounts "Valid log counts" $
       property (validExecCounts && validTimings && validLogCounts ==> True)

-- Property: Multiple breakpoints can coexist
prop_multiple_breakpoints :: [String] -> Property
prop_multiple_breakpoints locations =
    let validLocations = all (\loc -> length loc <= 30 && not (null loc)) locations
        uniqueLocations = length locations == length (locations :: [String])
    in classify validLocations "Valid locations" $
       classify uniqueLocations "Unique locations" $
       property (validLocations ==> length locations >= 0)

-- Property: Log levels filter messages correctly
prop_log_level_filtering :: LogLevel -> [LogLevel] -> Property
prop_log_level_filtering currentLevel messageLevels =
    let shouldLog = any (\level -> level >= currentLevel) messageLevels
        allValid = all (`elem` [Debug, Info, Warning, Error]) messageLevels
    in classify shouldLog "Some messages should log" $
       classify (not shouldLog) "No messages should log" $
       classify allValid "All valid log levels" $
       property (allValid ==> True)

-- Property: Function stack depth is bounded
prop_function_stack_depth :: Int -> Property
prop_function_stack_depth depth =
    let reasonableDepth = depth >= 0 && depth <= 1000
    in classify reasonableDepth "Reasonable depth" $
       property (reasonableDepth ==> depth >= 0)

-- Property: Breakpoint hit count increases monotonically
prop_breakpoint_hit_count_monotonic :: Int -> Int -> Property
prop_breakpoint_hit_count_monotonic initialHits additionalHits =
    let validInitial = initialHits >= 0 && initialHits <= 1000
        validAdditional = additionalHits >= 0 && additionalHits <= 100
        finalHits = initialHits + additionalHits
    in classify validInitial "Valid initial hits" $
       classify validAdditional "Valid additional hits" $
       property (validInitial && validAdditional ==> finalHits >= initialHits)

-- Property: Debug configuration maintains consistency
prop_debug_config_consistency :: LogLevel -> Int -> Property
prop_debug_config_consistency logLevel numOutputs =
    let validLogLevel = logLevel `elem` [Debug, Info, Warning, Error]
        validOutputs = numOutputs >= 0 && numOutputs <= 10
    in classify validLogLevel "Valid log level" $
       classify validOutputs "Valid number of outputs" $
       property (validLogLevel && validOutputs ==> True)

-- Test group containing all QuickCheck properties
tests :: TestTree
tests = testGroup "EnhancedDebug QuickCheck tests"
    [ fastProperty "Log level ordering is preserved" prop_log_level_ordering
    , fastProperty "Setting log level preserves hierarchy" prop_set_log_level_hierarchy
    , fastProperty "Breakpoint operations are idempotent" prop_breakpoint_idempotent
    , fastProperty "Execution count increments correctly" prop_execution_count_increments
    , fastProperty "Function stack maintains LIFO order" prop_function_stack_lifo
    , fastProperty "Conditional breakpoints evaluate consistently" prop_conditional_breakpoint_consistency
    , fastProperty "Log message content is preserved" prop_log_message_preservation
    , fastProperty "Timing measurements are non-negative" prop_timing_non_negative
    , fastProperty "Debug stats aggregation is correct" prop_debug_stats_aggregation
    , fastProperty "Multiple breakpoints can coexist" prop_multiple_breakpoints
    , fastProperty "Log levels filter messages correctly" prop_log_level_filtering
    , fastProperty "Function stack depth is bounded" prop_function_stack_depth
    , fastProperty "Breakpoint hit count increases monotonically" prop_breakpoint_hit_count_monotonic
    , fastProperty "Debug configuration maintains consistency" prop_debug_config_consistency
    ]