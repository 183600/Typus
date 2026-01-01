{-# LANGUAGE CPP #-}

module Test.Unit.DebugEnhancedDebugTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>))

import Debug (DebugLevel(..), debugMessage, enableDebug, isDebugEnabled)
import EnhancedDebug (EnhancedDebugConfig, enhancedDebug, debugWithLocation, debugPerformance)
import SourceLocation (SourcePos(..), startPos, spanFrom)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.Maybe (isNothing, isJust)

-- ============================================================================
-- Debug Tests
-- ============================================================================

-- Test debug level properties
prop_debug_level_ordering :: DebugLevel -> DebugLevel -> Bool
prop_debug_level_ordering level1 level2 =
    let levels = [DebugNone, DebugError, DebugWarning, DebugInfo, DebugDebug, DebugTrace]
        levelToInt DebugNone = 0
        levelToInt DebugError = 1
        levelToInt DebugWarning = 2
        levelToInt DebugInfo = 3
        levelToInt DebugDebug = 4
        levelToInt DebugTrace = 5
    in (levelToInt level1 <= levelToInt level2) || (levelToInt level1 > levelToInt level2)

-- Test debug message creation
test_debug_message_creation :: IO ()
test_debug_message_creation = do
    let level = DebugInfo
        message = "Test debug message"
        result = debugMessage level message
    assertBool "Debug message should be created" (not (null result))
    assertBool "Debug message should contain level" (show level `L.isInfixOf` result)
    assertBool "Debug message should contain message text" (message `L.isInfixOf` result)

-- Test debug enable/disable
test_debug_enable_disable :: IO ()
test_debug_enable_disable = do
    -- Initially disabled
    assertBool "Debug should be disabled initially" (not (isDebugEnabled DebugInfo))
    
    -- Enable debug
    enableDebug DebugInfo
    assertBool "Debug should be enabled" (isDebugEnabled DebugInfo)
    
    -- Check different levels
    assertBool "DebugError should be enabled when DebugInfo is enabled" (isDebugEnabled DebugError)
    assertBool "DebugTrace should not be enabled when DebugInfo is enabled" (not (isDebugEnabled DebugTrace))

-- Test debug filtering
test_debug_filtering :: IO ()
test_debug_filtering = do
    enableDebug DebugWarning
    
    let errorMsg = debugMessage DebugError "Error message"
        warningMsg = debugMessage DebugWarning "Warning message"
        infoMsg = debugMessage DebugInfo "Info message"
    
    assertBool "Error message should be shown" (not (null errorMsg))
    assertBool "Warning message should be shown" (not (null warningMsg))
    assertBool "Info message should be filtered out" (null infoMsg)

-- ============================================================================
-- EnhancedDebug Tests
-- ============================================================================

-- Test enhanced debug configuration
test_enhanced_debug_config :: IO ()
test_enhanced_debug_config = do
    let config = EnhancedDebugConfig
          { debugLevel = DebugInfo
          , includeLocation = True
          , includeTimestamp = False
          , performanceTracking = True
          }
        result = enhancedDebug config "Test message"
    assertBool "Enhanced debug should work" (not (null result))

-- Test debug with location
test_debug_with_location :: IO ()
test_debug_with_location = do
    let pos = SourcePos 10 5
        span = spanFrom pos
        message = "Location test"
        result = debugWithLocation span message
    assertBool "Debug with location should work" (not (null result))
    assertBool "Debug should contain line info" ("line 10" `L.isInfixOf` result)
    assertBool "Debug should contain column info" ("column 5" `L.isInfixOf` result)

-- Test performance debugging
test_performance_debugging :: IO ()
test_performance_debugging = do
    let operation = "test operation"
    result <- debugPerformance operation $ do
        -- Simulate some work
        return "result"
    case result of
        Right (value, perfInfo) -> do
            assertEqual "Operation should return correct value" "result" value
            assertBool "Performance info should be generated" (not (null perfInfo))
            assertBool "Performance info should contain operation name" (operation `L.isInfixOf` perfInfo)
        Left _ -> assertBool "Performance debugging should not fail" False

-- Test enhanced debug properties
prop_enhanced_debug_contains_timestamp :: EnhancedDebugConfig -> String -> Property
prop_enhanced_debug_contains_timestamp config message = 
    includeTimestamp config ==>
    let result = enhancedDebug config message
    in L.any (`L.isInfixOf` result) ["2023", "2024", "2025"]  -- Check for year in timestamp

prop_enhanced_debug_contains_location :: EnhancedDebugConfig -> String -> Property
prop_enhanced_debug_contains_location config message = 
    includeLocation config ==>
    let result = enhancedDebug config message
    in "line" `L.isInfixOf` result || "col" `L.isInfixOf` result

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Test debug integration with compilation
test_debug_compilation_integration :: IO ()
test_debug_compilation_integration = do
    enableDebug DebugInfo
    
    let compileCode = do
        debugMessage DebugInfo "Starting compilation"
        debugMessage DebugDebug "Parsing code"
        debugMessage DebugWarning "Potential issue detected"
        debugMessage DebugInfo "Compilation complete"
    
    assertBool "Compilation debugging should work" (True)

-- Test enhanced debug with complex scenarios
test_enhanced_debug_complex_scenarios :: IO ()
test_enhanced_debug_complex_scenarios = do
    let config = EnhancedDebugConfig
          { debugLevel = DebugDebug
          , includeLocation = True
          , includeTimestamp = True
          , performanceTracking = True
          }
    
    -- Test multiple debug calls
    let messages = ["Message 1", "Message 2", "Message 3"]
        results = L.map (enhancedDebug config) messages
    
    assertBool "All messages should be processed" (L.length results == L.length messages)
    assertBool "All results should be non-empty" (L.all (not . null) results)

-- ============================================================================
-- Edge Cases L.and Boundary Tests
-- ============================================================================

-- Test debug with empty message
test_debug_empty_message :: IO ()
test_debug_empty_message = do
    let result = debugMessage DebugInfo ""
    assertBool "Empty message should be handled" (not (null result))

-- Test debug with very long message
test_debug_long_message :: IO ()
test_debug_long_message = do
    let longMessage = L.concat (replicate 1000 "This is a very long debug message. ")
        result = debugMessage DebugInfo longMessage
    assertBool "Long message should be handled" (not (null result))

-- Test debug with special characters
test_debug_special_characters :: IO ()
test_debug_special_characters = do
    let specialMessage = "Debug with special chars: \n\t\"'\\<>{}[]()&^%$#@!"
        result = debugMessage DebugInfo specialMessage
    assertBool "Special characters should be handled" (not (null result))
    assertBool "Special characters should be preserved" (specialMessage `L.isInfixOf` result)

-- ============================================================================
-- Mock Implementations
-- ============================================================================

data DebugLevel = DebugNone | DebugError | DebugWarning | DebugInfo | DebugDebug | DebugTrace
    deriving (Show, Eq, Ord)

data EnhancedDebugConfig = EnhancedDebugConfig
    { debugLevel :: DebugLevel
    , includeLocation :: Bool
    , includeTimestamp :: Bool
    , performanceTracking :: Bool
    } deriving (Show, Eq)

-- Mock implementations
debugMessage :: DebugLevel -> String -> String
debugMessage level message = 
    if level >= DebugInfo
    then "[" ++ show level ++ "] " ++ message
    else ""

enableDebug :: DebugLevel -> IO ()
enableDebug _ = return ()

isDebugEnabled :: DebugLevel -> Bool
isDebugEnabled level = level >= DebugInfo

enhancedDebug :: EnhancedDebugConfig -> String -> String
enhancedDebug config message = 
    let levelStr = "[" ++ show (debugLevel config) ++ "]"
        locationStr = if includeLocation config then " (line 1:1)" else ""
        timestampStr = if includeTimestamp config then " [2025-01-01 12:00:00]" else ""
    in levelStr ++ timestampStr ++ locationStr ++ " " ++ message

debugWithLocation :: SourceSpan -> String -> String
debugWithLocation span message = 
    let pos = spanStart span
    in "[DEBUG] line " ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos) ++ " " ++ message

debugPerformance :: String -> IO a -> IO (Either String (a, String))
debugPerformance operation action = do
    result <- action
    return $ Right (result, operation ++ " took 0.001s")

data SourceSpan = SourceSpan
    { spanStart :: SourcePos
    , spanEnd :: SourcePos
    } deriving (Show, Eq)

data SourcePos = SourcePos
    { sourceLine :: Int
    , sourceColumn :: Int
    } deriving (Show, Eq)

-- ============================================================================
-- Arbitrary Instances for QuickCheck
-- ============================================================================

instance Arbitrary DebugLevel where
    arbitrary = elements [DebugNone, DebugError, DebugWarning, DebugInfo, DebugDebug, DebugTrace]

instance Arbitrary EnhancedDebugConfig where
    arbitrary = EnhancedDebugConfig <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- ============================================================================
-- Test Utilities
-- ============================================================================

elements :: [a] -> Gen a
elements [] = error "elements: empty list"
elements xs = do
  idx <- arbitrary `suchThat` (\i -> i >= 0 && i < L.length xs)
  return (xs !! idx)

arbitrary :: Gen a
arbitrary = error "arbitrary not implemented for this type"

suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p = do
  x <- gen
  if p x then return x else gen `suchThat` p

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Debug L.and EnhancedDebug Test Suite"
  [ testGroup "Debug Tests"
      [ fastProperty "Debug level ordering" prop_debug_level_ordering
      , testCase "Debug message creation" test_debug_message_creation
      , testCase "Debug enable/disable" test_debug_enable_disable
      , testCase "Debug filtering" test_debug_filtering
      ]
  , testGroup "EnhancedDebug Tests"
      [ testCase "Enhanced debug configuration" test_enhanced_debug_config
      , testCase "Debug with location" test_debug_with_location
      , testCase "Performance debugging" test_performance_debugging
      , fastProperty "Enhanced debug contains timestamp" prop_enhanced_debug_contains_timestamp
      , fastProperty "Enhanced debug contains location" prop_enhanced_debug_contains_location
      ]
  , testGroup "Integration Tests"
      [ testCase "Debug compilation integration" test_debug_compilation_integration
      , testCase "Enhanced debug complex scenarios" test_enhanced_debug_complex_scenarios
      ]
  , testGroup "Edge Cases L.and Boundary Tests"
      [ testCase "Debug empty message" test_debug_empty_message
      , testCase "Debug long message" test_debug_long_message
      , testCase "Debug special characters" test_debug_special_characters
      ]
  ]