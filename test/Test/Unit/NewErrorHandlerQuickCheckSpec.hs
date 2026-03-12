{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.NewErrorHandlerQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck hiding (resize)
import Test.QuickCheck (resize, sized)
import Test.Tasty.QuickCheck

import Compiler.Errors.Core hiding (line, column)
import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.Time (UTCTime)
import Data.List (sort, isInfixOf)
import Control.Monad.State (execState, evalState)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Compiler.Errors.Core as Error
import qualified Dependencies.TypeSystem as Dep
import qualified Ownership.Common.Types as Own

-- Additional Arbitrary instances
instance Arbitrary T.Text where
  arbitrary = T.pack <$> resize 20 arbitrary  -- Limit string length to 20 chars to reduce memory usage

-- Arbitrary instances for QuickCheck
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = ErrorLocation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ErrorRecovery where
  arbitrary = ErrorRecovery <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ErrorContext where
  arbitrary = ErrorContext <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TypeError where
  arbitrary = sized $ \n -> do
      let relatedErrorsSize = min 2 (n `div` 3)
          errorChainSize = min 2 (n `div` 3)
      TypeError <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 
                <*> arbitrary <*> arbitrary <*> (resize relatedErrorsSize $ listOf arbitrary) 
                <*> (resize errorChainSize $ listOf arbitrary) <*> arbitrary

instance Arbitrary CombinedError where
  arbitrary = oneof [IntegrationError <$> arbitrary <*> arbitrary]

-- ============================================================================
-- ErrorHandler Module QuickCheck Tests
-- ============================================================================

-- Test ErrorSeverity properties
prop_severity_priority_order :: Property
prop_severity_priority_order = 
  property $ severityPriority Fatal > severityPriority Error &&
            severityPriority Error > severityPriority Warning &&
            severityPriority Warning > severityPriority Info

prop_compare_severity_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_compare_severity_consistent sev1 sev2 = 
  let result = compareSeverity sev1 sev2
      pri1 = severityPriority sev1
      pri2 = severityPriority sev2
  in property $ (result == EQ) == (pri1 == pri2) &&
                (result == LT) == (pri1 < pri2) &&
                (result == GT) == (pri1 > pri2)

prop_is_at_least_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least_transitive sev1 sev2 sev3 = 
  let cond1 = isAtLeast sev1 sev2
      cond2 = isAtLeast sev2 sev3
      cond3 = isAtLeast sev1 sev3
  in property $ if cond1 && cond2 then cond3 else True

-- Test ErrorLocation properties
prop_error_location_creation :: Maybe String -> Positive Int -> Positive Int -> Property
prop_error_location_creation mfile (Positive line) (Positive col) = 
  let loc = ErrorLocation mfile line col Nothing Nothing
  in property $ filePath loc == mfile &&
                Error.line loc == line &&
                Error.column loc == col &&
                isNothing (endLine loc) &&
                isNothing (endColumn loc)

prop_error_location_with_range :: Maybe String -> Positive Int -> Positive Int -> 
                                 Positive Int -> Positive Int -> Property
prop_error_location_with_range mfile (Positive line) (Positive col) 
                              (Positive endLine) (Positive endCol) = 
  let loc = ErrorLocation mfile line col (Just endLine) (Just endCol)
  in property $ filePath loc == mfile &&
                Error.line loc == line &&
                Error.column loc == col &&
                Error.endLine loc == Just endLine &&
                endColumn loc == Just endCol

prop_get_error_line :: Positive Int -> Property
prop_get_error_line (Positive line) = 
  let loc = ErrorLocation Nothing line 0 Nothing Nothing
  in property $ getErrorLine loc == line

prop_get_error_column :: Positive Int -> Property
prop_get_error_column (Positive col) = 
  let loc = ErrorLocation Nothing 0 col Nothing Nothing
  in property $ getErrorColumn loc == col

-- Test ErrorContext properties
prop_empty_context_valid :: Property
prop_empty_context_valid = 
  let ctx = emptyContext
  in property $ isNothing (contextCode ctx) &&
                isNothing (contextFunction ctx) &&
                isNothing (contextVariable ctx) &&
                isNothing (contextType ctx) &&
                null (contextAdditional ctx)

prop_context_with_additional :: [(String, String)] -> Property
prop_context_with_additional additional = 
  let ctx = emptyContext { contextAdditional = additional }
  in property $ contextAdditional ctx == additional

-- Test ErrorRecovery properties
prop_recovery_strategy_consistency :: Bool -> Bool -> Maybe String -> Maybe String -> 
                                    Positive Int -> Property
prop_recovery_strategy_consistency canRec shouldCont recAction recHint (Positive cost) = 
  let confidence = 0.5 :: Float
      recovery = customRecovery canRec shouldCont recAction recHint cost confidence
  in property $ canRecover recovery == canRec &&
                shouldContinue recovery == shouldCont &&
                recoveryAction recovery == recAction &&
                recoveryHint recovery == recHint &&
                recoveryCost recovery == cost &&
                recoveryConfidence recovery == confidence

prop_predefined_recovery_strategies :: Property
prop_predefined_recovery_strategies = 
  property $ not (canRecover fatalRecovery) &&
            not (shouldContinue fatalRecovery) &&
            canRecover errorRecovery &&
            shouldContinue errorRecovery &&
            canRecover warningRecovery &&
            shouldContinue warningRecovery &&
            canRecover infoRecovery &&
            shouldContinue infoRecovery

prop_choose_best_recovery :: [ErrorRecovery] -> Property
prop_choose_best_recovery strategies = 
  let best = chooseBestRecovery strategies
  in if null strategies
     then property $ best == fatalRecovery
     else property $ canRecover best == any canRecover strategies

-- Test ErrorCategory properties
prop_error_category_ordering :: ErrorCategory -> ErrorCategory -> Property
prop_error_category_ordering cat1 cat2 = 
  let result = compare cat1 cat2
  in property $ (result == EQ) == (cat1 == cat2)

-- Test TypeError properties
prop_error_creation :: String -> ErrorSeverity -> ErrorCategory -> String -> 
                     ErrorLocation -> ErrorContext -> Property
prop_error_creation errId sev cat msg loc ctx = 
  let error = TypeError errId sev cat (T.pack msg) loc ctx errorRecovery [] [] [] Nothing
  in property $ errorId error == errId &&
                severity error == sev &&
                category error == cat &&
                message error == T.pack msg &&
                location error == loc &&
                context error == ctx &&
                recovery error == errorRecovery &&
                null (suggestions error) &&
                null (relatedErrors error) &&
                null (errorChain error) &&
                isNothing (timestamp error)

prop_error_with_suggestions :: String -> ErrorSeverity -> [String] -> Property
prop_error_with_suggestions errId sev suggs = 
  let error = errorWithSuggestions errId sev (map T.pack suggs) unknownLocation
  in property $ errorId error == errId &&
                severity error == sev &&
                suggestions error == map T.pack suggs

prop_error_with_location :: String -> ErrorSeverity -> ErrorLocation -> Property
prop_error_with_location errId sev loc = 
  let error = errorAt errId sev (T.pack "test") loc
  in property $ errorId error == errId &&
                severity error == sev &&
                location error == loc

prop_error_with_context :: String -> ErrorSeverity -> ErrorContext -> Property
prop_error_with_context errId sev ctx = 
  let error = withContext (errorAt errId sev (T.pack "test") unknownLocation) ctx
  in property $ errorId error == errId &&
                severity error == sev &&
                context error == ctx

-- Test ErrorCollector properties
prop_collector_add_error :: TypeError -> [TypeError] -> Property
prop_collector_add_error err errors = 
  let newErrors = execState (addError err) errors
  in case newErrors of
       (first:_) -> property $ length newErrors == length errors + 1 &&
                           first == err
       [] -> property False

prop_collector_add_warning :: TypeError -> [TypeError] -> Property
prop_collector_add_warning err errors = 
  let newErrors = execState (addWarning err) errors
  in case newErrors of
       (first:_) -> property $ length newErrors == length errors + 1 &&
                           severity first == Warning
       [] -> property False

prop_collector_add_info :: TypeError -> [TypeError] -> Property
prop_collector_add_info err errors = 
  let newErrors = execState (addInfo err) errors
  in case newErrors of
       (first:_) -> property $ length newErrors == length errors + 1 &&
                           severity first == Info
       [] -> property False

prop_collector_get_errors :: [TypeError] -> Property
prop_collector_get_errors errors = 
  let filtered = getErrors errors
  in property $ all (\e -> severity e == Error || severity e == Fatal) filtered

prop_collector_get_warnings :: [TypeError] -> Property
prop_collector_get_warnings errors = 
  let filtered = getWarnings errors
  in property $ all (\e -> severity e == Warning) filtered

prop_collector_get_info :: [TypeError] -> Property
prop_collector_get_info errors = 
  let filtered = getInfo errors
  in property $ all (\e -> severity e == Info) filtered

prop_collector_has_errors :: [TypeError] -> Property
prop_collector_has_errors errors = 
  let hasErr = hasErrors errors
      hasErrOrFatal = any (\e -> severity e == Error || severity e == Fatal) errors
  in property $ hasErr == hasErrOrFatal

prop_collector_has_warnings :: [TypeError] -> Property
prop_collector_has_warnings errors = 
  let hasWarn = hasWarnings errors
      hasWarnOnly = any (\e -> severity e == Warning) errors
  in property $ hasWarn == hasWarnOnly

-- Test error filtering properties
prop_filter_by_category :: ErrorCategory -> [TypeError] -> Property
prop_filter_by_category cat errors = 
  let filtered = filterByCategory cat errors
  in property $ all (\e -> category e == cat) filtered

prop_filter_by_severity :: ErrorSeverity -> [TypeError] -> Property
prop_filter_by_severity sev errors = 
  let filtered = filterBySeverity sev errors
  in property $ all (\e -> severity e == sev) filtered

prop_has_category :: ErrorCategory -> [TypeError] -> Property
prop_has_category cat errors = 
  let hasCat = any (hasCategory cat) errors
      anyCat = any (\e -> category e == cat) errors
  in property $ hasCat == anyCat

-- Test CombinedError properties
prop_combined_error_severity :: ErrorSeverity -> Property
prop_combined_error_severity sev = 
  let err = OwnershipErrorCombined sev undefined
  in property $ combinedErrorSeverity err == sev

prop_filter_combined_errors :: ErrorSeverity -> [CombinedError] -> Property
prop_filter_combined_errors minSev errors = 
  let filtered = filterCombinedErrorsBySeverity minSev errors
  in property $ all (\e -> isAtLeast minSev (combinedErrorSeverity e)) filtered

-- Test error formatting
prop_format_error_includes_message :: String -> String -> Property
prop_format_error_includes_message errId msg = 
  let err = errorAt errId Error (T.pack msg) unknownLocation
      formatted = formatError err
  in property $ T.pack msg `T.isInfixOf` T.pack formatted

prop_format_error_includes_severity :: String -> Property
prop_format_error_includes_severity msg = 
  let err = errorAt "test" Error (T.pack msg) unknownLocation
      formatted = formatError err
  in property $ "ERROR" `isInfixOf` formatted

-- Unit tests for edge cases
test_error_handler_edge_cases :: TestTree
test_error_handler_edge_cases = testGroup "ErrorHandler Edge Cases"
  [ testCase "severityPriority ordering" $ do
      assertBool "Fatal > Error" $ severityPriority Fatal > severityPriority Error
      assertBool "Error > Warning" $ severityPriority Error > severityPriority Warning
      assertBool "Warning > Info" $ severityPriority Warning > severityPriority Info
    
  , testCase "compareSeverity" $ do
      assertEqual "Fatal vs Error" GT $ compareSeverity Fatal Error
      assertEqual "Error vs Warning" GT $ compareSeverity Error Warning
      assertEqual "Warning vs Info" GT $ compareSeverity Warning Info
      assertEqual "Error vs Error" EQ $ compareSeverity Error Error
    
  , testCase "isAtLeast" $ do
      assertBool "Fatal is at least Error" $ isAtLeast Fatal Error
      assertBool "Error is at least Warning" $ isAtLeast Error Warning
      assertBool "Warning is at least Info" $ isAtLeast Warning Info
      assertBool "Fatal is at least Fatal" $ isAtLeast Fatal Fatal
      assertBool "Info is not at least Warning" $ not $ isAtLeast Info Warning
    
  , testCase "ErrorLocation" $ do
      let loc1 = ErrorLocation Nothing 10 20 Nothing Nothing
          loc2 = ErrorLocation (Just "file.txt") 5 10 (Just 5) (Just 15)
      assertEqual "getLine loc1" 10 $ getErrorLine loc1
      assertEqual "getColumn loc1" 20 $ getErrorColumn loc1
      assertEqual "getLine loc2" 5 $ getErrorLine loc2
      assertEqual "getColumn loc2" 10 $ getErrorColumn loc2
    
  , testCase "emptyContext" $ do
      let ctx = emptyContext
      assertBool "contextCode is Nothing" $ isNothing $ contextCode ctx
      assertBool "contextFunction is Nothing" $ isNothing $ contextFunction ctx
      assertBool "contextVariable is Nothing" $ isNothing $ contextVariable ctx
      assertBool "contextType is Nothing" $ isNothing $ contextType ctx
      assertBool "contextAdditional is empty" $ null $ contextAdditional ctx
    
  , testCase "ErrorRecovery strategies" $ do
      assertBool "fatalRecovery cannot recover" $ not $ canRecover fatalRecovery
      assertBool "fatalRecovery should not continue" $ not $ shouldContinue fatalRecovery
      assertBool "errorRecovery can recover" $ canRecover errorRecovery
      assertBool "errorRecovery should continue" $ shouldContinue errorRecovery
      assertBool "warningRecovery can recover" $ canRecover warningRecovery
      assertBool "warningRecovery should continue" $ shouldContinue warningRecovery
      assertBool "infoRecovery can recover" $ canRecover infoRecovery
      assertBool "infoRecovery should continue" $ shouldContinue infoRecovery
    
  , testCase "TypeError creation" $ do
      let loc = unknownLocation
          ctx = emptyContext
          err = errorAt "test-001" Error (T.pack "Test error") loc
      assertEqual "errorId" "test-001" $ errorId err
      assertEqual "severity" Error $ severity err
      assertEqual "message" (T.pack "Test error") $ message err
      assertEqual "location" loc $ location err
      assertEqual "context" ctx $ context err
    
  , testCase "ErrorCollector" $ do
      let err1 = errorAt "err1" Error (T.pack "Error 1") unknownLocation
          err2 = errorAt "err2" Warning (T.pack "Warning 1") unknownLocation
          err3 = errorAt "err3" Info (T.pack "Info 1") unknownLocation
          errors = [err1, err2, err3]
      
      assertEqual "getErrors" [err1] $ getErrors errors
      assertEqual "getWarnings" [err2] $ getWarnings errors
      assertEqual "getInfo" [err3] $ getInfo errors
      assertBool "hasErrors" $ hasErrors errors
      assertBool "hasWarnings" $ hasWarnings errors
    
  , testCase "filterByCategory" $ do
      let err1 = errorWithCategory "err1" TypeChecking (T.pack "Error 1") unknownLocation
          err2 = errorWithCategory "err2" Ownership (T.pack "Error 2") unknownLocation
          err3 = errorWithCategory "err3" TypeChecking (T.pack "Error 3") unknownLocation
          errors = [err1, err2, err3]
      
      assertEqual "TypeChecking errors" [err1, err3] $ filterByCategory TypeChecking errors
      assertEqual "Ownership errors" [err2] $ filterByCategory Ownership errors
      assertEqual "Parsing errors" [] $ filterByCategory Parsing errors
    
  , testCase "filterBySeverity" $ do
      let err1 = errorAt "err1" Fatal (T.pack "Fatal") unknownLocation
          err2 = errorAt "err2" Error (T.pack "Error") unknownLocation
          err3 = errorAt "err3" Warning (T.pack "Warning") unknownLocation
          err4 = errorAt "err4" Info (T.pack "Info") unknownLocation
          errors = [err1, err2, err3, err4]
      
      assertEqual "Fatal errors" [err1] $ filterBySeverity Fatal errors
      assertEqual "Error errors" [err2] $ filterBySeverity Error errors
      assertEqual "Warning errors" [err3] $ filterBySeverity Warning errors
      assertEqual "Info errors" [err4] $ filterBySeverity Info errors
    
  , testCase "formatError" $ do
      let err = errorAt "test-001" Error (T.pack "Something went wrong") unknownLocation
          formatted = formatError err
      assertBool "contains ERROR" $ "ERROR" `isInfixOf` formatted
      assertBool "contains message" $ "Something went wrong" `isInfixOf` formatted
      assertBool "contains category" $ show (category err) `isInfixOf` formatted
  ]

-- QuickCheck properties
test_error_handler_properties :: TestTree
test_error_handler_properties = testGroup "ErrorHandler QuickCheck Properties"
  [ testProperty "severity priority order" prop_severity_priority_order
  , testProperty "compare severity consistent" prop_compare_severity_consistent
  , testProperty "isAtLeast transitive" prop_is_at_least_transitive
  , testProperty "error location creation" prop_error_location_creation
  , testProperty "error location with range" prop_error_location_with_range
  , testProperty "get error line" prop_get_error_line
  , testProperty "get error column" prop_get_error_column
  , testProperty "empty context valid" prop_empty_context_valid
  , testProperty "context with additional" prop_context_with_additional
  , testProperty "recovery strategy consistent" prop_recovery_strategy_consistency
  , testProperty "predefined recovery strategies" prop_predefined_recovery_strategies
  , testProperty "choose best recovery" prop_choose_best_recovery
  , testProperty "error category ordering" prop_error_category_ordering
  , testProperty "error creation" prop_error_creation
  , testProperty "error with suggestions" prop_error_with_suggestions
  , testProperty "error with location" prop_error_with_location
  , testProperty "error with context" prop_error_with_context
  , testProperty "collector add error" prop_collector_add_error
  , testProperty "collector add warning" prop_collector_add_warning
  , testProperty "collector add info" prop_collector_add_info
  , testProperty "collector get errors" prop_collector_get_errors
  , testProperty "collector get warnings" prop_collector_get_warnings
  , testProperty "collector get info" prop_collector_get_info
  , testProperty "collector has errors" prop_collector_has_errors
  , testProperty "collector has warnings" prop_collector_has_warnings
  , testProperty "filter by category" prop_filter_by_category
  , testProperty "filter by severity" prop_filter_by_severity
  , testProperty "has category" prop_has_category
  , testProperty "combined error severity" prop_combined_error_severity
  , testProperty "filter combined errors" prop_filter_combined_errors
  , testProperty "format error includes message" prop_format_error_includes_message
  , testProperty "format error includes severity" prop_format_error_includes_severity
  ]

-- Main test suite
errorHandlerTests :: TestTree
errorHandlerTests = testGroup "ErrorHandler Module Tests"
  [ test_error_handler_edge_cases
  , test_error_handler_properties
  ]