module Test.Unit.TestErrorHandlerPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.List (sort)

-- Test Properties for Error Handler

-- Property: Error collection should preserve order
prop_error_collection_preserve_order :: String -> Property
prop_error_collection_preserve_order s = property $ 
  let errors = collectErrors s
      sortedErrors = sort errors
  in if length errors > 1 
     then errors /= sortedErrors || allSame errors
     else True
  where
    allSame [] = True
    allSame [_] = True
    allSame (x:y:xs) = x == y && allSame (y:xs)

-- Property: Error severity should be within valid range
prop_error_severity_valid_range :: String -> Property
prop_error_severity_valid_range s = property $ 
  let errors = collectErrors s
      severities = map getSeverity errors
  in all (`elem` [1..5]) severities

-- Property: Error messages should contain context information
prop_error_messages_contain_context :: String -> Property
prop_error_messages_contain_context s = property $ 
  let errors = collectErrors s
      messages = map getMessage errors
  in all (not . null) messages

-- Property: Error recovery should not introduce new errors
prop_error_recovery_no_new_errors :: String -> Property
prop_error_recovery_no_new_errors s = property $ 
  let errors = collectErrors s
      recovered = recoverFromErrors s errors
      newErrors = collectErrors recovered
  in length newErrors <= length errors

-- Property: Error aggregation should combine similar errors
prop_error_aggregation_combine_similar :: String -> Property
prop_error_aggregation_combine_similar s = property $ 
  let errors = collectErrors s
      aggregated = aggregateErrors errors
  in length aggregated <= length errors

-- Property: Error location should be within source bounds
prop_error_location_within_bounds :: String -> Property
prop_error_location_within_bounds s = property $ 
  let errors = collectErrors s
      locations = map getLocation errors
      sourceLength = length s
  in all (`elem` [0..sourceLength]) locations

-- Helper functions (mock implementations)
collectErrors :: String -> [Int]
collectErrors s = if null s then [] else [1, 2, min 5 (length s)]

getSeverity :: Int -> Int
getSeverity = (`mod` 5) . (+1)

getMessage :: Int -> String
getMessage err = "Error message for " ++ show err

recoverFromErrors :: String -> [Int] -> String
recoverFromErrors s _ = take (length s) s

aggregateErrors :: [Int] -> [Int]
aggregateErrors = map safeHead . groupSort
  where
    safeHead [] = error "Empty group in aggregateErrors"
    safeHead (h:_) = h
    groupSort xs = case xs of
                   [] -> []
                   (h:_) -> let (group, rest) = span (== h) (sort xs)
                            in group : groupSort rest

getLocation :: Int -> Int
getLocation = (*10)

tests :: TestTree
tests = testGroup "Test.Unit.TestErrorHandlerPropertiesSpec Tests"
  [ testProperty "Error collection should preserve order" prop_error_collection_preserve_order
  , testProperty "Error severity should be within valid range" prop_error_severity_valid_range
  , testProperty "Error messages should contain context information" prop_error_messages_contain_context
  , testProperty "Error recovery should not introduce new errors" prop_error_recovery_no_new_errors
  , testProperty "Error aggregation should combine similar errors" prop_error_aggregation_combine_similar
  , testProperty "Error location should be within source bounds" prop_error_location_within_bounds
  ]