module Test.Unit.DebugIntegrationFlowSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import DebugIntegration

-- Test debug information collection
prop_debug_info_collection :: String -> Property
prop_debug_info_collection debugMessage =
  let info = collectDebugInfo debugMessage
      message = getDebugMessage info
  in property $ message === debugMessage

-- Test debug context preservation
prop_debug_context_preservation :: String -> String -> Property
prop_debug_context_preservation context message =
  let debugInfo = createDebugWithContext context message
      preservedContext = getDebugContext debugInfo
  in property $ preservedContext === context

-- Test debug stack trace
prop_debug_stack_trace_ordering :: [String] -> Property
prop_debug_stack_trace_ordering functionNames =
  let stackTrace = createStackTrace functionNames
      ordered = getStackTraceOrder stackTrace
  in property $ length ordered === length functionNames

-- Test debug level filtering
prop_debug_level_filtering :: DebugLevel -> [DebugInfo] -> Property
prop_debug_level_filtering level debugInfos =
  let filtered = filterByDebugLevel level debugInfos
  in property $ all (\info -> getDebugLevel info <= level) filtered

-- Test debug output formatting
prop_debug_output_formatting :: DebugInfo -> Property
prop_debug_output_formatting debugInfo =
  let formatted = formatDebugOutput debugInfo
  in property $ not (null formatted)

tests :: TestTree
tests = testGroup "DebugIntegration Flow Tests"
  [ testProperty "debug info collection" prop_debug_info_collection
  , testProperty "debug context preservation" prop_debug_context_preservation
  , testProperty "debug stack trace ordering" prop_debug_stack_trace_ordering
  , testProperty "debug level filtering" prop_debug_level_filtering
  , testProperty "debug output formatting" prop_debug_output_formatting
  ]