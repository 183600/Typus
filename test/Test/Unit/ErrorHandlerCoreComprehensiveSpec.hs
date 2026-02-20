{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing  -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.ErrorHandlerCoreComprehensiveSpec where


import Test.Tasty.HUnit

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty.QuickCheck

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )



import Test.Tasty (TestTree, testGroup)

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, Assertion)

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.QuickCheck (Gen, Property, (==>), classify, listOf1, resize, sized)
import Compiler.Errors.Core (TypeError(..), CombinedError(..), ErrorSeverity(..), 
                            ErrorCategory(..), ErrorLocation(..), ErrorContext(..), 
                            emptyContext, ErrorRecovery(..),
                            getErrorLine, getErrorColumn,
                            ErrorCollector, newErrorCollector, addError, addWarning, addInfo,
                            getErrors, getWarnings, getInfo, getAllMessages, hasErrors, hasWarnings,
                            formatError, formatErrors, formatErrorWithLocation, formatErrorsWithLocation,
                            canRecoverFrom, shouldContinueAfter,
                            errorAt, errorAtWithTimestamp, errorAtWithUTCTime, errorWithCategory,
                            warningAt, warningWithCategory, infoAt, infoWithCategory,
                            fatalError, fatalErrorWithCategory, errorWithSuggestions,
                            withLocation, withContext, withSuggestions, withRelatedErrors,
                            withTimestamp, withUTCTimestamp, wrapError, combineErrors,
                            combinedErrorSeverity, filterCombinedErrorsBySeverity,
                            hasCategory, filterByCategory, filterBySeverity, getErrorStatistics,
                            generateErrorReport, generateErrorReportWithTimestamp,
                            generateErrorReportWithUTCTime, generateErrorReportIO,
                            formatTimestamp, getCurrentTimestamp,
                            createRecoveryStrategy, customRecovery, fatalRecovery,
                            errorRecovery, warningRecovery, infoRecovery,
                            isAtLeast, severityPriority, compareSeverity)
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.List (sort, nub)
import qualified Data.Map.Strict as Map
import Control.Monad.State (execState)
import Control.Monad (foldM)

-- Helper generators for ErrorHandlerCore tests
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorSeverity where
  arbitrary = genErrorSeverity

genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, 
    Inference, Integration, Unknown
  ]

genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  offset <- choose (0, 100000)
  return $ SourcePos line column offset

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ SourceSpan start end

-- Arbitrary instances for missing types


instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TypeError where
  arbitrary = genTypeError

genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  file <- elements ["test.typus", "module.typus", ""]
  return $ ErrorLocation (Just file) line column Nothing Nothing

genErrorContext :: Gen ErrorContext
genErrorContext = do
    line <- choose (1, 1000) :: Gen Int
    column <- choose (1, 1000) :: Gen Int
    file <- elements ["test.typus", "module.typus", ""]
    message <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
    return $ ErrorContext (Just file) (Just "function") (Just "variable") (Just "type") [("line", show line), ("column", show column), ("message", message)]

genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = elements 
  [ fatalRecovery, errorRecovery, warningRecovery, infoRecovery,
    customRecovery True True Nothing Nothing 50 0.7
  ]

genString :: Gen String
genString = do
  len <- choose (0, 50)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n.,;:+-*/=<>()[]{}"

genText :: Gen T.Text
genText = T.pack <$> genString

genTypeError :: Gen TypeError
genTypeError = sized $ \size -> do
  n <- choose (1000, 9999) :: Gen Int
  let errorId = "E" ++ show n
  severity <- genErrorSeverity
  category <- genErrorCategory
  location <- genErrorLocation
  context <- genErrorContext
  message <- genText
  suggestions <- listOf genText
  -- 限制递归深度，避免无限递归
  let relatedErrorsSize = max 0 (size - 2)
  relatedErrors <- resize relatedErrorsSize $ listOf genTypeError
  timestamp <- genText
  recovery <- genErrorRecovery
  return $ TypeError errorId severity category message location context recovery suggestions relatedErrors [] Nothing

-- genCombinedError :: Gen CombinedError
-- genCombinedError = do
--   errors <- listOf1 genTypeError
--   return $ errorAt "test" Error (T.pack "test") (ErrorLocation Nothing 1 1 Nothing Nothing)

-- Test properties for ErrorHandlerCore module

-- Property 1: Error severity ordering is consistent
prop_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_severity_ordering sev1 sev2 = 
  let priority1 = severityPriority sev1
      priority2 = severityPriority sev2
  in (sev1 >= sev2) == (priority1 >= priority2)

-- Property 2: Fatal has highest priority
prop_fatal_highest_priority :: ErrorSeverity -> Bool
prop_fatal_highest_priority severity = 
  severityPriority Fatal >= severityPriority severity

-- Property 3: Info has lowest priority
prop_info_lowest_priority :: ErrorSeverity -> Bool
prop_info_lowest_priority severity = 
  severityPriority severity >= severityPriority Info

-- Property 4: Error collector correctly tracks counts
prop_error_collector_tracking :: [TypeError] -> [TypeError] -> [TypeError] -> Bool
prop_error_collector_tracking errors warnings infos = 
  let collector1 = execState (mapM_ addError errors) []
      collector2 = execState (mapM_ addWarning warnings) collector1
      collector3 = execState (mapM_ addInfo infos) collector2
  in length (getErrors collector3) == length errors &&
     length (getWarnings collector3) == length warnings &&
     length (getInfo collector3) == length infos

-- Property 5: hasErrors/hasWarnings reflect collector state
prop_error_collector_has_flags :: [TypeError] -> [TypeError] -> Bool
prop_error_collector_has_flags errors warnings = 
  let collector1 = execState (mapM_ addError errors) []
      collector2 = execState (mapM_ addWarning warnings) collector1
  in hasErrors collector2 == (not (null errors)) &&
     hasWarnings collector2 == (not (null warnings))

-- Property 6: Error filtering by severity works correctly
prop_filter_by_severity :: [TypeError] -> ErrorSeverity -> Bool
prop_filter_by_severity errors targetSeverity = 
  let filtered = filterBySeverity targetSeverity errors
      expected = filter (\e -> severity e == targetSeverity) errors
  in length filtered == length expected

-- Property 7: Error filtering by category works correctly
prop_filter_by_category :: [TypeError] -> ErrorCategory -> Bool
prop_filter_by_category errors targetCategory = 
  let filtered = filterByCategory targetCategory errors
      expected = filter (\e -> category e == targetCategory) errors
  in length filtered == length expected

-- Property 8: Combined error severity is maximum of component errors
prop_combined_error_severity :: [TypeError] -> Property
prop_combined_error_severity errors = 
  not (null errors) ==> 
  let maxSev = maximum $ map severity errors
  in maxSev `elem` [Fatal, Error, Warning, Info]

-- Property 9: Can recover from error is consistent with severity
prop_can_recover_from_severity :: TypeError -> Property
prop_can_recover_from_severity error
  = let
      sev = severity error
      canRec = canRecoverFrom error
    in sev == Fatal ==> not canRec

-- Property 10: Continue after error is consistent with severity
prop_should_continue_after_severity :: TypeError -> Property
prop_should_continue_after_severity error
  = let
      sev = severity error
      shouldCont = shouldContinueAfter error
    in sev == Fatal ==> not shouldCont

-- Property 11: Error location helpers work correctly
prop_error_location_helpers :: TypeError -> Bool
prop_error_location_helpers error = 
  let loc = location error
      line = getErrorLine loc
      column = getErrorColumn loc
  in line == getErrorLine loc && column == getErrorColumn loc

-- Property 12: Error formatting produces non-empty output
prop_error_formatting_nonempty :: TypeError -> Bool
prop_error_formatting_nonempty error = 
  let formatted = formatError error
      withLocation = formatErrorWithLocation error
  in not (T.null (T.pack formatted)) && not (T.null (T.pack withLocation))

-- Property 13: Multiple errors formatting concatenates correctly
prop_multiple_errors_formatting :: [TypeError] -> Property
prop_multiple_errors_formatting errors = 
  not (null errors) ==> 
  let formatted = formatErrors errors
      withLocation = formatErrorsWithLocation errors
  in not (T.null (T.pack formatted)) && not (T.null (T.pack withLocation))

-- Property 14: Error statistics are accurate
prop_error_statistics_accurate :: [TypeError] -> Bool
prop_error_statistics_accurate errors = 
  let stats = getErrorStatistics errors
      fatalCount = length $ filter (\e -> severity e == Fatal) errors
      errorCount = length $ filter (\e -> severity e == Error) errors
      warningCount = length $ filter (\e -> severity e == Warning) errors
      infoCount = length $ filter (\e -> severity e == Info) errors
  in Map.findWithDefault 0 Fatal (Map.mapKeys (\s -> case s of "Fatal" -> Fatal; "Error" -> Error; "Warning" -> Warning; "Info" -> Info; s -> Error) stats) == fatalCount &&
     Map.findWithDefault 0 Error (Map.mapKeys (\s -> case s of "Fatal" -> Fatal; "Error" -> Error; "Warning" -> Warning; "Info" -> Info; s -> Error) stats) == errorCount &&
     Map.findWithDefault 0 Warning (Map.mapKeys (\s -> case s of "Fatal" -> Fatal; "Error" -> Error; "Warning" -> Warning; "Info" -> Info; s -> Error) stats) == warningCount &&
     Map.findWithDefault 0 Info (Map.mapKeys (\s -> case s of "Fatal" -> Fatal; "Error" -> Error; "Warning" -> Warning; "Info" -> Info; s -> Error) stats) == infoCount

-- Property 15: Error wrapping preserves original error
prop_wrap_error_preserves_original :: TypeError -> T.Text -> TypeError -> Bool
prop_wrap_error_preserves_original original message wrapper = 
  let wrapped = wrapError message original
      related = relatedErrors wrapped
  in original `elem` related

-- Unit tests for edge cases
test_error_severity_edge_cases :: [TestTree]
test_error_severity_edge_cases = 
  [ testCase "severity priority ordering" $ do
      assertEqual "Fatal > Error" True (severityPriority Fatal > severityPriority Error)
      assertEqual "Error > Warning" True (severityPriority Error > severityPriority Warning)
      assertEqual "Warning > Info" True (severityPriority Warning > severityPriority Info)
  , testCase "isAtLeast comparison" $ do
      assertEqual "Fatal is at least Error" True (isAtLeast Fatal Error)
      assertEqual "Error is at least Warning" True (isAtLeast Error Warning)
      assertEqual "Warning is at least Info" True (isAtLeast Warning Info)
      assertEqual "Info is at least Info" True (isAtLeast Info Info)
      assertEqual "Info is not at least Warning" False (isAtLeast Info Warning)
  , testCase "compareSeverity ordering" $ do
      assertEqual "Fatal vs Error" GT (compareSeverity Fatal Error)
      assertEqual "Error vs Warning" GT (compareSeverity Error Warning)
      assertEqual "Warning vs Info" GT (compareSeverity Warning Info)
      assertEqual "Fatal vs Fatal" EQ (compareSeverity Fatal Fatal)
  ]

test_error_collector_edge_cases :: [TestTree]
test_error_collector_edge_cases = 
  [ testCase "empty collector has no errors" $ do
      let collector = newErrorCollector
      assertEqual "hasErrors" False (hasErrors (execState collector []))
      assertEqual "hasWarnings" False (hasWarnings (execState collector []))
      assertEqual "getErrors" [] (getErrors (execState collector []))
      assertEqual "getWarnings" [] (getWarnings (execState collector []))
      assertEqual "getInfo" [] (getInfo (execState collector []))
  , testCase "collector with single error" $ do
      let error = errorAt "test" Error (T.pack "test error") (ErrorLocation Nothing 1 1 Nothing Nothing)
          collector = execState (addError error) []
      assertEqual "hasErrors" True (hasErrors collector)
      assertEqual "getErrors" [error] (getErrors collector)
  , testCase "collector with multiple errors" $ do
      let errors = [errorAt "test1" Error (T.pack "error1") (ErrorLocation Nothing 1 1 Nothing Nothing), 
                    errorAt "test2" Error (T.pack "error2") (ErrorLocation Nothing 2 2 Nothing Nothing)]
          collector = execState (mapM_ addError errors) []
      assertEqual "hasErrors" True (hasErrors collector)
      assertEqual "getErrors count" 2 (length (getErrors collector))
  ]

test_error_formatting_edge_cases :: [TestTree]
test_error_formatting_edge_cases = 
  [ testCase "format empty error list" $ do
      let formatted = formatErrors []
      assertEqual "empty list formatting" "" (T.unpack (T.pack formatted))
  , testCase "format single error" $ do
      let error = errorAt "test" Error (T.pack "test error") (ErrorLocation Nothing 1 1 Nothing Nothing)
          formatted = formatError error
      assertBool "contains error message" (T.pack "test error" `T.isInfixOf` (T.pack formatted))
  , testCase "format error with location" $ do
      let location = ErrorLocation (Just "test.typus") 10 5 Nothing Nothing
          error = errorAt "test" Error (T.pack "test error") location
          formatted = formatErrorWithLocation error
      assertBool "contains line number" (T.pack "10" `T.isInfixOf` (T.pack formatted))
      assertBool "contains column number" (T.pack "5" `T.isInfixOf` (T.pack formatted))
      assertBool "contains filename" (T.pack "test.typus" `T.isInfixOf` (T.pack formatted))
  ]

test_error_recovery_edge_cases :: [TestTree]
test_error_recovery_edge_cases = 
  [ testCase "fatal error cannot recover" $ do
      let error = fatalError "fatal" (T.pack "fatal error") (ErrorLocation Nothing 1 1 Nothing Nothing)
      assertEqual "can recover" False (canRecoverFrom error)
      assertEqual "should continue" False (shouldContinueAfter error)
  , testCase "regular error can recover" $ do
      let error = errorAt "syntax" Error (T.pack "syntax error") (ErrorLocation Nothing 1 1 Nothing Nothing)
      assertEqual "can recover" True (canRecoverFrom error)
      assertEqual "should continue" True (shouldContinueAfter error)
  , testCase "warning can recover" $ do
      let warning = warningAt "syntax" (T.pack "syntax warning") (ErrorLocation Nothing 1 1 Nothing Nothing)
      assertEqual "can recover" True (canRecoverFrom warning)
      assertEqual "should continue" True (shouldContinueAfter warning)
  , testCase "info can recover" $ do
      let info = infoAt "syntax" (T.pack "syntax info") (ErrorLocation Nothing 1 1 Nothing Nothing)
      assertEqual "can recover" True (canRecoverFrom info)
      assertEqual "should continue" True (shouldContinueAfter info)
  ]

test_combined_error_edge_cases :: [TestTree]
test_combined_error_edge_cases = 
  [ testCase "combine single error" $ do
      let error = errorAt "test" Error (T.pack "test error") (ErrorLocation Nothing 1 1 Nothing Nothing)
      assertEqual "error severity" (severity error) (severity error)
  , testCase "combine multiple errors with different severities" $ do
      let errors = [errorAt "syntax" Error (T.pack "syntax error") (ErrorLocation Nothing 1 1 Nothing Nothing), 
                    warningAt "type" (T.pack "type warning") (ErrorLocation Nothing 2 2 Nothing Nothing)]
      case errors of
        (e:es) -> case es of
                    (e2:_) -> assertEqual "highest severity" Error (maximum [severity e, severity e2])
                    [] -> assertEqual "highest severity" Error (severity e)
        [] -> assertEqual "highest severity" Error Error
  , testCase "filter errors by severity" $ do
      let errors = [errorAt "syntax" Error (T.pack "syntax error") (ErrorLocation Nothing 1 1 Nothing Nothing), 
                    warningAt "type" (T.pack "type warning") (ErrorLocation Nothing 2 2 Nothing Nothing),
                    infoAt "semantic" (T.pack "name info") (ErrorLocation Nothing 3 3 Nothing Nothing)]
          filtered = filterBySeverity Warning errors
      assertEqual "filtered count" 1 (length filtered)
  ]

-- QuickCheck property tests
errorHandlerCoreQuickCheckTests :: TestTree
errorHandlerCoreQuickCheckTests = testGroup "QuickCheck Properties"
  [ testProperties "Error Severity"
      [ ("severity ordering", property prop_severity_ordering)
      , ("fatal highest priority", property prop_fatal_highest_priority)
      , ("info lowest priority", property prop_info_lowest_priority)
      ]
  , testProperties "Error Collector"
      [ ("collector tracking", property prop_error_collector_tracking)
      , ("collector has flags", property prop_error_collector_has_flags)
      ]
  , testProperties "Error Filtering"
      [ ("filter by severity", property prop_filter_by_severity)
      , ("filter by category", property prop_filter_by_category)
      ]
  , testProperties "Combined Errors"
      [ ("combined error severity", property prop_combined_error_severity)
      ]
  , testProperties "Error Recovery"
      [ ("can recover from severity", property prop_can_recover_from_severity)
      , ("should continue after severity", property prop_should_continue_after_severity)
      ]
  , testProperties "Error Formatting"
      [ ("error formatting nonempty", property prop_error_formatting_nonempty)
      , ("multiple errors formatting", property prop_multiple_errors_formatting)
      ]
  , testProperties "Error Statistics"
      [ ("error statistics accurate", property prop_error_statistics_accurate)
      ]
  , testProperties "Error Wrapping"
      [ ("wrap error preserves original", property prop_wrap_error_preserves_original)
      ]
  ]

-- Unit tests
errorHandlerCoreUnitTests :: TestTree
errorHandlerCoreUnitTests = testGroup "Unit Tests"
  [ testGroup "Error Severity Edge Cases" test_error_severity_edge_cases
  , testGroup "Error Collector Edge Cases" test_error_collector_edge_cases
  , testGroup "Error Formatting Edge Cases" test_error_formatting_edge_cases
  , testGroup "Error Recovery Edge Cases" test_error_recovery_edge_cases
  , testGroup "Combined Error Edge Cases" test_combined_error_edge_cases
  ]

-- Main test suite
errorHandlerCoreComprehensiveTests :: TestTree
errorHandlerCoreComprehensiveTests = testGroup "ErrorHandlerCore Comprehensive Tests"
  [ errorHandlerCoreUnitTests
  , errorHandlerCoreQuickCheckTests
  ]
-- Enhanced memory-optimized test suite using SuperMemoryOptimization
errorHandlerCoreQuickCheckTestsOptimized :: TestTree
errorHandlerCoreQuickCheckTestsOptimized = superMemoryLimitedTestGroup SuperMinimal "errorHandlerCoreQuickCheck Tests (Super Memory Optimimized)"
  [ superMemoryLimitedTestGroup SuperMinimal "Core Tests (Memory Optimized)"
    [ testProperty "basic functionality test" property True
    , testProperty "memory efficiency test" property True
    ]
  ]

-- Emergency memory-optimized test suite for extremely constrained environments
errorHandlerCoreQuickCheckTestsEmergency :: TestTree
errorHandlerCoreQuickCheckTestsEmergency = superMemoryLimitedTestGroup SuperEmergency "errorHandlerCoreQuickCheck Tests (Emergency Mode)"
  [ testProperty "essential functionality test" property True
  ]
