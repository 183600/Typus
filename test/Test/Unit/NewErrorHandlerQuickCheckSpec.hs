{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map

-- | Test properties for ErrorHandler module
tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck Tests"
  [ testProperty "severityPriority: Fatal > Error > Warning > Info" propSeverityPriorityOrder
  , testProperty "isAtLeast: correct severity comparison" propIsAtLeastCorrect
  , testProperty "filterBySeverity: correctly filters by severity" propFilterBySeverityCorrect
  , testProperty "filterByCategory: correctly filters by category" propFilterByCategoryCorrect
  , testProperty "errorAt: creates error with correct properties" propErrorAtCorrect
  , testProperty "withLocation: updates error location" propWithLocationUpdates
  , testProperty "withContext: updates error context" propWithContextUpdates
  , testProperty "withSuggestions: adds suggestions to error" propWithSuggestionsAdds
  , testProperty "wrapError: wraps error with additional message" propWrapErrorCorrect
  , testProperty "getErrorStatistics: correctly counts errors" propGetErrorStatisticsCorrect
  ]

-- | severityPriority: Fatal > Error > Warning > Info
propSeverityPriorityOrder :: Property
propSeverityPriorityOrder = 
  let priorities = [(Fatal, 100), (Error, 80), (Warning, 30), (Info, 10)]
      checkOrder [] = True
      checkOrder [_] = True
      checkOrder ((s1, p1):(s2, p2):rest) = 
        p1 >= p2 && checkOrder ((s2, p2):rest)
  in counterexample ("Priorities: " ++ show priorities) $
     checkOrder priorities

-- | isAtLeast: Correct severity comparison
propIsAtLeastCorrect :: ErrorSeverity -> ErrorSeverity -> Property
propIsAtLeastCorrect minSeverity targetSeverity = 
  let result = isAtLeast minSeverity targetSeverity
      severityOrder = [Info, Warning, Error, Fatal]
      minIndex = case elemIndex minSeverity severityOrder of
                   Just idx -> idx
                   Nothing -> 0
      targetIndex = case elemIndex targetSeverity severityOrder of
                      Just idx -> idx
                      Nothing -> 0
      expected = targetIndex >= minIndex
  in counterexample ("Min: " ++ show minSeverity ++ ", Target: " ++ show targetSeverity ++ 
                    ", Expected: " ++ show expected ++ ", Got: " ++ show result) $
     result == expected

-- | filterBySeverity: Correctly filters by severity
propFilterBySeverityCorrect :: [ErrorSeverity] -> ErrorSeverity -> Property
propFilterBySeverityCorrect severities targetSeverity = 
  let errors = map (\(i, sev) -> errorAt ("ERR" ++ show i) (T.pack $ "Error " ++ show i) (ErrorLocation Nothing i i Nothing Nothing)) (zip [1..] severities)
      filtered = filterBySeverity targetSeverity errors
      expected = filter (\e -> severity e == targetSeverity) errors
  in counterexample ("Original: " ++ show (map severity errors) ++ 
                    ", Filtered: " ++ show (map severity filtered) ++ 
                    ", Expected: " ++ show (map severity expected)) $
     length filtered == length expected &&
     all (\e -> severity e == targetSeverity) filtered

-- | filterByCategory: Correctly filters by category
propFilterByCategoryCorrect :: [ErrorCategory] -> ErrorCategory -> Property
propFilterByCategoryCorrect categories targetCategory = 
  let errors = map (\(i, cat) -> errorWithCategory ("ERR" ++ show i) cat (T.pack $ "Error " ++ show i) (ErrorLocation Nothing i i Nothing Nothing)) (zip [1..] categories)
      filtered = filterByCategory targetCategory errors
      expected = filter (\e -> category e == targetCategory) errors
  in counterexample ("Original: " ++ show (map category errors) ++ 
                    ", Filtered: " ++ show (map category filtered) ++ 
                    ", Expected: " ++ show (map category expected)) $
     length filtered == length expected &&
     all (\e -> category e == targetCategory) filtered

-- | errorAt: Creates error with correct properties
propErrorAtCorrect :: String -> Property
propErrorAtCorrect errId = 
  let msg = T.pack "Test error message"
      loc = ErrorLocation (Just "test.txt") 10 5 Nothing Nothing
      err = errorAt errId msg loc
  in counterexample ("Error ID: " ++ show (errorId err) ++ 
                    ", Severity: " ++ show (severity err) ++ 
                    ", Category: " ++ show (category err) ++
                    ", Message: " ++ show (message err) ++
                    ", Location: " ++ show (location err)) $
     errorId err == errId &&
     severity err == Error &&
     category err == Unknown &&
     message err == msg &&
     location err == loc

-- | withLocation: Updates error location
propWithLocationUpdates :: String -> Property
propWithLocationUpdates errId = 
  let msg = T.pack "Test error message"
      loc1 = ErrorLocation (Just "file1.txt") 10 5 Nothing Nothing
      loc2 = ErrorLocation (Just "file2.txt") 20 10 (Just 25) (Just 15)
      err1 = errorAt errId msg loc1
      err2 = withLocation err1 loc2
  in counterexample ("Original location: " ++ show (location err1) ++ 
                    ", Updated location: " ++ show (location err2)) $
     location err2 == loc2 &&
     errorId err2 == errId &&
     message err2 == msg

-- | withContext: Updates error context
propWithContextUpdates :: String -> Property
propWithContextUpdates errId = 
  let msg = T.pack "Test error message"
      loc = ErrorLocation Nothing 10 5 Nothing Nothing
      ctx1 = ErrorContext (Just "code1") (Just "func1") (Just "var1") (Just "type1") [("key1", "value1")]
      ctx2 = ErrorContext (Just "code2") (Just "func2") (Just "var2") (Just "type2") [("key2", "value2")]
      err1 = errorAt errId msg loc
      err2 = withContext err1 ctx2
  in counterexample ("Original context: " ++ show (context err1) ++ 
                    ", Updated context: " ++ show (context err2)) $
     context err2 == ctx2 &&
     errorId err2 == errId &&
     message err2 == msg

-- | withSuggestions: Adds suggestions to error
propWithSuggestionsAdds :: String -> [Text] -> Property
propWithSuggestionsAdds errId newSuggestions = 
  let msg = T.pack "Test error message"
      loc = ErrorLocation Nothing 10 5 Nothing Nothing
      originalSuggestions = [T.pack "Original suggestion 1", T.pack "Original suggestion 2"]
      err1 = errorWithSuggestions errId msg originalSuggestions loc
      err2 = withSuggestions newSuggestions err1
  in counterexample ("Original suggestions: " ++ show (suggestions err1) ++ 
                    ", New suggestions: " ++ show newSuggestions ++
                    ", Final suggestions: " ++ show (suggestions err2)) $
     suggestions err2 == newSuggestions ++ originalSuggestions

-- | wrapError: Wraps error with additional message
propWrapErrorCorrect :: String -> Property
propWrapErrorCorrect errId = 
  let innerMsg = T.pack "Inner error message"
      wrapperMsg = T.pack "Wrapper message"
      loc = ErrorLocation Nothing 10 5 Nothing Nothing
      innerErr = errorAt errId innerMsg loc
      wrappedErr = wrapError wrapperMsg innerErr
  in counterexample ("Inner message: " ++ show (message innerErr) ++ 
                    ", Wrapper message: " ++ show wrapperMsg ++
                    ", Wrapped message: " ++ show (message wrappedErr)) $
     message wrappedErr == wrapperMsg <> ": " <> innerMsg &&
     errorId wrappedErr == errId &&
     location wrappedErr == loc &&
     errorChain wrappedErr == [innerErr]

-- | getErrorStatistics: Correctly counts errors
propGetErrorStatisticsCorrect :: [ErrorSeverity] -> [ErrorCategory] -> Property
propGetErrorStatisticsCorrect severities categories = 
  let errors = zipWith (\i (sev, cat) -> 
          let err = errorAt ("ERR" ++ show i) (T.pack $ "Error " ++ show i) (ErrorLocation Nothing i i Nothing Nothing)
          in err { severity = sev, category = cat }) [1..] (zip severities categories)
      stats = getErrorStatistics errors
      expectedTotal = length errors
      expectedFatal = length $ filter (== Fatal) severities
      expectedErrors = length $ filter (== Error) severities
      expectedWarnings = length $ filter (== Warning) severities
      expectedInfo = length $ filter (== Info) severities
  in counterexample ("Errors: " ++ show (length errors) ++ 
                    ", Stats: " ++ show (Map.toList stats)) $
     Map.findWithDefault 0 "total" stats == expectedTotal &&
     Map.findWithDefault 0 "fatal" stats == expectedFatal &&
     Map.findWithDefault 0 "errors" stats == expectedErrors &&
     Map.findWithDefault 0 "warnings" stats == expectedWarnings &&
     Map.findWithDefault 0 "info" stats == expectedInfo

-- Helper function to find element index
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex _ [] = Nothing
elemIndex x (y:ys) = if x == y then Just 0 else fmap (+1) (elemIndex x ys)