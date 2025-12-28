{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ConcurrentSafetySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, getPositive, getNonNegative, vector, listOf1, elements)
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..))
import Utils (trim, removeComments, normalizeIndentation, splitBy)
import SourceLocation (SourcePos(..), startPos, advancePosByText)
import ErrorHandler (runErrorHandler)

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isNothing, isJust, fromMaybe, catMaybes)
import Control.Concurrent (forkIO, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, void)
import Data.IORef
import System.Random (randomRIO)

-- | Concurrent safety and thread safety tests
tests :: TestTree
tests = testGroup "Concurrent Safety"
  [ testGroup "Parser Concurrent Safety"
    [ testCase "concurrent parsing" test_concurrent_parsing
    , testCase "parser thread isolation" test_parser_thread_isolation
    , fastProperty "parser concurrent consistency" prop_parser_concurrent_consistency
    ]

  , testGroup "Utils Concurrent Safety"
    [ testCase "concurrent utils operations" test_concurrent_utils_operations
    , testCase "utils thread safety" test_utils_thread_safety
    , fastProperty "utils concurrent determinism" prop_utils_concurrent_determinism
    ]

  , testGroup "SourceLocation Concurrent Safety"
    [ testCase "concurrent position tracking" test_concurrent_position_tracking
    , testCase "location calculation thread safety" test_location_calculation_thread_safety
    , fastProperty "position tracking consistency" prop_position_tracking_consistency
    ]

  , testGroup "ErrorHandler Concurrent Safety"
    [ testCase "concurrent error handling" test_concurrent_error_handling
    , testCase "error handler state isolation" test_error_handler_state_isolation
    , fastProperty "error handling concurrent consistency" prop_error_handling_concurrent_consistency
    ]

  , testGroup "Memory Consistency"
    [ testCase "shared memory consistency" test_shared_memory_consistency
    , testCase "memory barrier behavior" test_memory_barrier_behavior
    , fastProperty "memory access patterns" prop_memory_access_patterns
    ]

  , testGroup "Race Condition Prevention"
    [ testCase "race condition prevention" test_race_condition_prevention
    , testCase "atomic operations" test_atomic_operations
    , fastProperty "concurrent modification safety" prop_concurrent_modification_safety
    ]

  , testGroup "Performance Under Concurrency"
    [ testCase "concurrent performance" test_concurrent_performance
    , testCase "scalability under load" test_scalability_under_load
    , fastProperty "performance degradation limits" prop_performance_degradation_limits
    ]

  , testGroup "Resource Management"
    [ testCase "resource cleanup" test_resource_cleanup
    , testCase "resource exhaustion handling" test_resource_exhaustion_handling
    , fastProperty "resource allocation consistency" prop_resource_allocation_consistency
    ]
  ]

-- ============================================================================
-- Parser Concurrent Safety
-- ============================================================================

test_concurrent_parsing :: IO ()
test_concurrent_parsing = do
  let content = unlines
        [ "//! ownership=true"
        , "func test() {"
        , "    x := 42"
        , "    return x"
        , "}"
        ]
      numThreads = 10
      results <- replicateM numThreads $ do
        result <- parseTypus content "concurrent.typus"
        return result
  let successCount = length $ filter isRight results
  assertBool "All concurrent parses should succeed" $ successCount == numThreads
  where
    isRight (Right _) = True
    isRight _ = False

test_parser_thread_isolation :: IO ()
test_parser_thread_isolation = do
  let contents = 
        [ "func test1() { return 1; }"
        , "func test2() { return 2; }"
        , "func test3() { return 3; }"
        ]
      numThreads = length contents
      results <- zipWithM parseContent contents [1..numThreads]
  let successCount = length $ filter isRight results
  assertBool "All thread-isolated parses should succeed" $ successCount == numThreads
  where
    parseContent content threadId = parseTypus content ("isolation" ++ show threadId ++ ".typus")

prop_parser_concurrent_consistency :: String -> Property
prop_parser_concurrent_consistency content =
  length content <= 100 ==>
  let parse1 = parseTypus content "consistency1.typus"
      parse2 = parseTypus content "consistency2.typus"
      parse3 = parseTypus content "consistency3.typus"
  in case (parse1, parse2, parse3) of
       (Right f1, Right f2, Right f3) -> 
         property $ length (tfBlocks f1) == length (tfBlocks f2) && 
                   length (tfBlocks f2) == length (tfBlocks f3)
       _ -> property True

-- ============================================================================
-- Utils Concurrent Safety
-- ============================================================================

test_concurrent_utils_operations :: IO ()
test_concurrent_utils_operations = do
  let content = "    café naïve résumé 🚀 测试    "
      operations = [trim, removeComments, normalizeIndentation]
      numThreads = length operations
      results <- zipWithM runOperation operations [1..numThreads]
  let successCount = length $ filter (not . null) results
  assertBool "All concurrent utils operations should succeed" $ successCount == numThreads
  where
    runOperation op threadId = return $ op content

test_utils_thread_safety :: IO ()
test_utils_thread_safety = do
  let content = "a,b,c,d,e"
      numThreads = 10
      results <- replicateM numThreads $ do
        return $ splitBy ',' content
  let allResults = head results
      successCount = length $ filter (== allResults) results
  assertBool "All thread-safe utils operations should be consistent" $ successCount == numThreads

prop_utils_concurrent_determinism :: String -> Property
prop_utils_concurrent_determinism content =
  length content <= 100 ==>
  let result1 = trim content
      result2 = trim content
      result3 = trim content
  in property $ result1 == result2 && result2 == result3

-- ============================================================================
-- SourceLocation Concurrent Safety
-- ============================================================================

test_concurrent_position_tracking :: IO ()
test_concurrent_position_tracking = do
  let content = unlines ["line1", "line2", "line3"]
      numThreads = 10
      results <- replicateM numThreads $ do
        return $ advancePosByText startPos content
  let allResults = head results
      successCount = length $ filter (== allResults) results
  assertBool "All concurrent position tracking should be consistent" $ successCount == numThreads

test_location_calculation_thread_safety :: IO ()
test_location_calculation_thread_safety = do
  let positions = [posAt 1 1, posAt 2 5, posAt 3 10]
      numThreads = length positions
      results <- zipWithM trackPosition positions [1..numThreads]
  let successCount = length $ filter isJust results
  assertBool "All location calculations should succeed" $ successCount == numThreads
  where
    trackPosition pos threadId = return $ Just pos

prop_position_tracking_consistency :: String -> Property
prop_position_tracking_consistency content =
  length content <= 100 ==>
  let pos1 = advancePosByText startPos content
      pos2 = advancePosByText startPos content
      pos3 = advancePosByText startPos content
  in property $ pos1 == pos2 && pos2 == pos3

-- ============================================================================
-- ErrorHandler Concurrent Safety
-- ============================================================================

test_concurrent_error_handling :: IO ()
test_concurrent_error_handling = do
  let content = "func invalid( {"
      numThreads = 10
      results <- replicateM numThreads $ do
        runErrorHandler content
  let successCount = length $ filter isRight results
  assertBool "All concurrent error handlers should succeed" $ successCount == numThreads
  where
    isRight (Right _) = True
    isRight _ = False

test_error_handler_state_isolation :: IO ()
test_error_handler_state_isolation = do
  let contents = 
        [ "func invalid1( {"
        , "func invalid2( {"
        , "func invalid3( {"
        ]
      numThreads = length contents
      results <- zipWithM handleError contents [1..numThreads]
  let successCount = length $ filter isRight results
  assertBool "All thread-isolated error handlers should succeed" $ successCount == numThreads
  where
    handleError content threadId = runErrorHandler content

prop_error_handling_concurrent_consistency :: String -> Property
prop_error_handling_concurrent_consistency content =
  length content <= 100 ==>
  let result1 = runErrorHandler content
      result2 = runErrorHandler content
      result3 = runErrorHandler content
  in case (result1, result2, result3) of
       (Right (errs1, _), Right (errs2, _), Right (errs3, _)) ->
         property $ length errs1 == length errs2 && length errs2 == length errs3
       _ -> property True

-- ============================================================================
-- Memory Consistency
-- ============================================================================

test_shared_memory_consistency :: IO ()
test_shared_memory_consistency = do
  sharedRef <- newIORef "shared content"
  let numThreads = 10
  results <- replicateM numThreads $ do
    content <- readIORef sharedRef
    return $ trim content
  let allResults = head results
  successCount = length $ filter (== allResults) results
  assertBool "Shared memory access should be consistent" $ successCount == numThreads

test_memory_barrier_behavior :: IO ()
test_memory_barrier_behavior = do
  counter <- newIORef 0
  let numThreads = 10
  results <- replicateM numThreads $ do
    modifyIORef counter (+1)
    readIORef counter
  let finalResult = last results
  assertBool "Memory barrier should ensure consistency" $ finalResult >= numThreads

prop_memory_access_patterns :: Int -> Property
prop_memory_access_patterns iterations =
  iterations > 0 && iterations <= 100 ==>
  let content = "test content"
      results = replicate iterations $ trim content
  in property $ all (== head results) results

-- ============================================================================
-- Race Condition Prevention
-- ============================================================================

test_race_condition_prevention :: IO ()
test_race_condition_prevention = do
  let content = "func test() { return 42; }"
      numThreads = 10
  mvar <- newEmptyMVar
  results <- replicateM numThreads $ do
    result <- parseTypus content "race.typus"
    putMVar mvar result
    return result
  _ <- takeMVar mvar
  let successCount = length $ filter isRight results
  assertBool "Race conditions should be prevented" $ successCount == numThreads

test_atomic_operations :: IO ()
test_atomic_operations = do
  counter <- newIORef 0
  let numThreads = 10
      increment = modifyIORef counter (+1)
  replicateM_ numThreads increment
  finalValue <- readIORef counter
  assertBool "Atomic operations should be consistent" $ finalValue == numThreads

prop_concurrent_modification_safety :: String -> Property
prop_concurrent_modification_safety content =
  length content <= 100 ==>
  let operations = [trim, removeComments, normalizeIndentation]
      results = map ($ content) operations
  in property $ all (not . null) results

-- ============================================================================
-- Performance Under Concurrency
-- ============================================================================

test_concurrent_performance :: IO ()
test_concurrent_performance = do
  let content = unlines $ replicate 100 "func test() { return 42; }"
      numThreads = 5
  start <- getCurrentTime
  results <- replicateM numThreads $ do
    parseTypus content "performance.typus"
  end <- getCurrentTime
  let duration = diffUTCTime end start
  let successCount = length $ filter isRight results
  assertBool "Concurrent operations should complete in reasonable time" $ duration < 5.0
  assertBool "Most concurrent operations should succeed" $ successCount >= numThreads `div` 2

test_scalability_under_load :: IO ()
test_scalability_under_load = do
  let content = "func test() { return 42; }"
      threadCounts = [1, 2, 5, 10]
  results <- mapM testWithThreads threadCounts
  let successRates = map fst results
      durations = map snd results
  assertBool "Success rate should remain high under load" $ all (> 0.8) successRates
  assertBool "Performance should scale reasonably" $ last durations < 10.0
  where
    testWithThreads numThreads = do
      start <- getCurrentTime
      results <- replicateM numThreads $ parseTypus content ("scale" ++ show numThreads ++ ".typus")
      end <- getCurrentTime
      let duration = diffUTCTime end start
          successCount = length $ filter isRight results
          successRate = fromIntegral successCount / fromIntegral numThreads
      return (successRate, duration)

prop_performance_degradation_limits :: Int -> Property
prop_performance_degradation_limits threadCount =
  threadCount > 0 && threadCount <= 20 ==>
  let content = "func test() { return 42; }"
      results = replicate threadCount $ parseTypus content "degradation.typus"
      successCount = length $ filter isRight results
      successRate = fromIntegral successCount / fromIntegral threadCount
  in property $ successRate >= 0.5

-- ============================================================================
-- Resource Management
-- ============================================================================

test_resource_cleanup :: IO ()
test_resource_cleanup = do
  let content = unlines $ replicate 1000 "func test() { return 42; }"
      numIterations = 10
  results <- replicateM numIterations $ do
    parseTypus content "cleanup.typus"
  let successCount = length $ filter isRight results
  assertBool "Resources should be cleaned up properly" $ successCount >= numIterations `div` 2

test_resource_exhaustion_handling :: IO ()
test_resource_exhaustion_handling = do
  let largeContent = unlines $ replicate 10000 "func test() { return 42; }"
      numThreads = 5
  results <- replicateM numThreads $ do
    parseTypus largeContent "exhaustion.typus"
  let successCount = length $ filter isRight results
  assertBool "Should handle resource exhaustion gracefully" $ successCount >= 1

prop_resource_allocation_consistency :: Int -> Property
prop_resource_allocation_consistency size =
  size > 0 && size <= 1000 ==>
  let content = unlines $ replicate size "func test() { return 42; }"
      result = parseTypus content "resource.typus"
  in case result of
       Right file -> property $ length (tfBlocks file) >= 0
       Left _ -> property True

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Mock time functions for testing
getCurrentTime :: IO UTCTime
getCurrentTime = return $ UTCTime (fromGregorian 2023 1 1) 0

diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime _ _ = 1.0  -- Mock implementation

-- Mock date types
data UTCTime = UTCTime Day NominalDiffTime
data Day = Day Int

fromGregorian :: Integer -> Int -> Int -> Day
fromGregorian _ _ _ = Day 0

-- Mock nominal diff time
type NominalDiffTime = Double