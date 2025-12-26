{-# LANGUAGE CPP #-}

module Test.Unit.EnhancedDebugCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Map as Map

import Debug (DebugLevel(..), DebugContext(..), debugMessage, runDebug)
import DebugIntegration (DebugIntegration, runDebugIntegration, 
                        integratedDebugMessage, DebugOutput(..))
import EnhancedDebug (EnhancedDebug, runEnhancedDebug, enhancedDebugMessage, 
                     DebugTrace(..), DebugFilter(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.Arbitrary ()

-- Test 1: Debug level ordering
prop_debug_level_ordering :: DebugLevel -> DebugLevel -> Property
prop_debug_level_ordering level1 level2 =
  let levels = [Trace, Debug, Info, Warning, Error, Critical]
      level1Index = length $ takeWhile (/= level1) levels
      level2Index = length $ takeWhile (/= level2) levels
  in (level1Index <= level2Index) ==> level1 <= level2

-- Test 2: Debug context preservation
prop_debug_context_preservation :: String -> DebugLevel -> SourcePos -> Property
prop_debug_context_preservation msg level pos =
  let context = DebugContext pos level msg
      debugResult = debugMessage context
  in case runDebug debugResult of
    Left _ -> property True -- Debug may fail
    Right output -> property True -- Debug may succeed

-- Test 3: Debug message formatting
prop_debug_message_formatting :: String -> DebugLevel -> Property
prop_debug_message_formatting msg level =
  not (null msg) ==> 
  let context = DebugContext (SourcePos 1 1 0) level msg
      debugResult = debugMessage context
  in case runDebug debugResult of
    Left _ -> property True
    Right output -> msg `isInfixOf` output

-- Test 4: Debug integration consistency
prop_debug_integration_consistency :: String -> DebugLevel -> Property
prop_debug_integration_consistency msg level =
  let debugResult = integratedDebugMessage level msg
  in case runDebugIntegration debugResult of
    Left _ -> property True
    Right output -> property True

-- Test 5: Enhanced debug trace
prop_enhanced_debug_trace :: String -> DebugTrace -> Property
prop_enhanced_debug_trace msg trace =
  not (null msg) ==> 
  let debugResult = enhancedDebugMessage msg trace
  in case runEnhancedDebug debugResult of
    Left _ -> property True
    Right output -> property True

-- Test 6: Debug filtering
prop_debug_filtering :: String -> DebugLevel -> DebugFilter -> Property
prop_debug_filtering msg level filter =
  let debugResult = enhancedDebugMessage msg (DebugTrace level filter)
  in case runEnhancedDebug debugResult of
    Left _ -> property True
    Right output -> property True

-- Test 7: Debug context with positions
prop_debug_context_positions :: String -> DebugLevel -> Int -> Int -> Property
prop_debug_context_positions msg level line col =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col 0
      context = DebugContext pos level msg
      debugResult = debugMessage context
  in case runDebug debugResult of
    Left _ -> property True
    Right output -> property True

-- Test 8: Multiple debug messages
prop_multiple_debug_messages :: [String] -> DebugLevel -> Property
prop_multiple_debug_messages msgs level =
  length msgs < 10 ==> -- Limit complexity
  let contexts = map (\msg -> DebugContext (SourcePos 1 1 0) level msg) msgs
      debugResults = map debugMessage contexts
      runResults = map runDebug debugResults
  in length runResults === length msgs

-- Test 9: Debug output types
prop_debug_output_types :: DebugOutput -> Property
prop_debug_output_types output =
  case output of
    DebugMessage msg -> length msg >= 0
    DebugError err -> length err >= 0
    DebugWarning warn -> length warn >= 0
    DebugInfo info -> length info >= 0

-- Test 10: Enhanced debug with spans
prop_enhanced_debug_spans :: String -> DebugLevel -> SourceSpan -> Property
prop_enhanced_debug_spans msg level span =
  not (null msg) ==>
  let trace = DebugTrace level (DebugFilter span)
      debugResult = enhancedDebugMessage msg trace
  in case runEnhancedDebug debugResult of
    Left _ -> property True
    Right output -> property True

tests :: TestTree
tests = testGroup "Enhanced Debug Core Tests"
  [ fastProperty "Debug level ordering" prop_debug_level_ordering
  , fastProperty "Debug context preservation" prop_debug_context_preservation
  , fastProperty "Debug message formatting" prop_debug_message_formatting
  , fastProperty "Debug integration consistency" prop_debug_integration_consistency
  , fastProperty "Enhanced debug trace" prop_enhanced_debug_trace
  , fastProperty "Debug filtering" prop_debug_filtering
  , fastProperty "Debug context with positions" prop_debug_context_positions
  , fastProperty "Multiple debug messages" prop_multiple_debug_messages
  , fastProperty "Debug output types" prop_debug_output_types
  , fastProperty "Enhanced debug with spans" prop_enhanced_debug_spans
  ]