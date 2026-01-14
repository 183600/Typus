module Test.Unit.ErrorHandlerQuickCheckPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T

-- Arbitrary instance for Text
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Info, Warning, Error, Fatal]

instance Arbitrary ErrorCategory where
  arbitrary = elements 
    [ TypeChecking
    , Ownership
    , Parsing
    , Semantic
    , Runtime
    , Constraint
    , Inference
    , Integration
    , Unknown
    ]

-- ============================================================================
-- Error Severity Properties
-- ============================================================================

-- Property: severityPriority should have consistent ordering
prop_severity_priority_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_ordering s1 s2 = 
  let p1 = severityPriority s1
      p2 = severityPriority s2
  in property $ 
    if s1 == s2 
    then p1 == p2
    else p1 /= p2

-- Property: compareSeverity should be consistent with severityPriority
prop_compare_severity_consistency :: ErrorSeverity -> ErrorSeverity -> Property
prop_compare_severity_consistency s1 s2 = 
  let priorityComparison = compare (severityPriority s1) (severityPriority s2)
      severityComparison = compareSeverity s1 s2
  in property $ priorityComparison == severityComparison

-- Property: isAtLeast should be reflexive
prop_is_at_least_reflexive :: ErrorSeverity -> Property
prop_is_at_least_reflexive s = property $ isAtLeast s s

-- Property: isAtLeast should be transitive
prop_is_at_least_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least_transitive s1 s2 s3 = 
  property $ 
    if isAtLeast s1 s2 && isAtLeast s2 s3
    then isAtLeast s1 s3
    else True  -- Property doesn't apply if premises aren't met

-- Property: Fatal should be at least as severe as any other severity
prop_fatal_is_most_severe :: ErrorSeverity -> Property
prop_fatal_is_most_severe s = property $ isAtLeast Fatal s

-- Property: Info should not be at least as severe as any other severity except itself
prop_info_is_least_severe :: ErrorSeverity -> Property
prop_info_is_least_severe s = 
  property $ 
    if s == Info
    then isAtLeast Info s
    else not (isAtLeast Info s)

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: getErrorLine should return the line field
prop_get_error_line :: Int -> Int -> Property
prop_get_error_line line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
  in property $ getErrorLine loc == line

-- Property: getErrorColumn should return the column field
prop_get_error_column :: Int -> Int -> Property
prop_get_error_column line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
  in property $ getErrorColumn loc == col

-- ============================================================================
-- Error Context Properties
-- ============================================================================

-- Property: emptyContext should have all Nothing fields
prop_empty_context_properties :: Property
prop_empty_context_properties = 
  let ErrorContext code function varType varType' additional = emptyContext
  in property $ 
    code == (Nothing :: Maybe String) && 
    function == (Nothing :: Maybe String) && 
    varType == (Nothing :: Maybe String) && 
    varType' == (Nothing :: Maybe String) && 
    null additional

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: fatalRecovery should not allow recovery or continuation
prop_fatal_recovery_properties :: Property
prop_fatal_recovery_properties = 
  let RecoveryStrategy canRec shouldCont action hint cost confidence = fatalRecovery
  in property $ 
    not canRec && 
    not shouldCont && 
    action == Nothing && 
    hint == Nothing && 
    cost == 100 && 
    confidence == 0.0

-- Property: errorRecovery should allow recovery and continuation
prop_error_recovery_properties :: Property
prop_error_recovery_properties = 
  let RecoveryStrategy canRec shouldCont action hint cost confidence = errorRecovery
  in property $ 
    canRec && 
    shouldCont && 
    action == Nothing && 
    hint == Nothing && 
    cost == 50 && 
    confidence == 0.7

-- Property: warningRecovery should allow recovery and continuation with low cost
prop_warning_recovery_properties :: Property
prop_warning_recovery_properties = 
  let RecoveryStrategy canRec shouldCont action hint cost confidence = warningRecovery
  in property $ 
    canRec && 
    shouldCont && 
    action == Nothing && 
    hint == Nothing && 
    cost == 10 && 
    confidence == 0.9

-- Property: infoRecovery should allow recovery and continuation with no cost
prop_info_recovery_properties :: Property
prop_info_recovery_properties = 
  let RecoveryStrategy canRec shouldCont action hint cost confidence = infoRecovery
  in property $ 
    canRec && 
    shouldCont && 
    action == Nothing && 
    hint == Nothing && 
    cost == 0 && 
    confidence == 1.0

-- Property: customRecovery should use provided values
prop_custom_recovery_properties :: Bool -> Bool -> String -> String -> Int -> Float -> Property
prop_custom_recovery_properties canRec shouldCont action hint cost confidence = 
  let recovery = customRecovery canRec shouldCont (Just action) (Just hint) cost confidence
      RecoveryStrategy canRec' shouldCont' action' hint' cost' confidence' = recovery
  in property $ 
    canRec' == canRec && 
    shouldCont' == shouldCont && 
    action' == Just action && 
    hint' == Just hint && 
    cost' == cost && 
    confidence' == confidence

-- ============================================================================
-- Error Construction Properties
-- ============================================================================

-- Property: errorAt should Error (T.pack should) create error with provided values
prop_error_at_properties :: String -> Text -> Int -> Int -> Property
prop_error_at_properties errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      err = errorAt errId Error msg loc
  in property $ 
    errorId err == errId && 
    message err == msg && 
    location err == loc && 
    severity err == Error && 
    category err == Unknown

-- Property: errorWithCategory should create error with provided category
prop_error_with_category_properties :: String -> ErrorCategory -> Text -> Int -> Int -> Property
prop_error_with_category_properties errId errCategory msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      err = errorWithCategory errId errCategory msg loc
  in property $ 
    errorId err == errId && 
    message err == msg && 
    location err == loc && 
    severity err == Error && 
    category err == errCategory

-- Property: warningAt should create warning with provided values
prop_warning_at_properties :: String -> Text -> Int -> Int -> Property
prop_warning_at_properties errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      err = warningAt errId msg loc
  in property $ 
    errorId err == errId && 
    message err == msg && 
    location err == loc && 
    severity err == Warning && 
    category err == Unknown

-- Property: infoAt should create info with provided values
prop_info_at_properties :: String -> Text -> Int -> Int -> Property
prop_info_at_properties errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      err = infoAt errId msg loc
  in property $ 
    errorId err == errId && 
    message err == msg && 
    location err == loc && 
    severity err == Info && 
    category err == Unknown

-- Property: withLocation should update error location
prop_with_location_properties :: String -> Text -> Int -> Int -> Int -> Int -> Property
prop_with_location_properties errId msg line1 col1 line2 col2 = 
  let loc1 = ErrorLocation Nothing line1 col1 Nothing Nothing
      loc2 = ErrorLocation Nothing line2 col2 Nothing Nothing
      err = errorAt errId Error msg loc1
      updatedErr = withLocation err loc2
  in property $ 
    errorId updatedErr == errId && 
    message updatedErr == msg && 
    location updatedErr == loc2

-- Property: withContext should update error context
prop_with_context_properties :: String -> Text -> Int -> Int -> String -> String -> String -> Property
prop_with_context_properties errId msg line col func var typ = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      ctx = ErrorContext Nothing (Just func) (Just var) (Just typ) []
      err = errorAt errId Error msg loc
      updatedErr = withContext err ctx
  in property $ 
    errorId updatedErr == errId && 
    message updatedErr == msg && 
    location updatedErr == loc && 
    context updatedErr == ctx

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: hasCategory should be true for matching category
prop_has_category_matching :: ErrorCategory -> String -> Text -> Int -> Int -> Property
prop_has_category_matching cat errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      err = errorWithCategory errId cat msg loc
  in property $ hasCategory cat err

-- Property: hasCategory should be false for non-matching category
prop_has_category_non_matching :: ErrorCategory -> ErrorCategory -> String -> Text -> Int -> Int -> Property
prop_has_category_non_matching cat1 cat2 errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      err = errorWithCategory errId cat1 msg loc
  in property $ 
    if cat1 == cat2
    then hasCategory cat2 err
    else not (hasCategory cat2 err)

-- Property: filterByCategory should only return errors with matching category
prop_filter_by_category :: ErrorCategory -> [ErrorCategory] -> String -> Text -> Int -> Int -> Property
prop_filter_by_category targetCat cats errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      errors = [errorWithCategory (errId ++ show i) cat msg loc | (i, cat) <- zip [0..] cats]
      filtered = filterByCategory targetCat errors
  in property $ all (hasCategory targetCat) filtered

-- Property: filterBySeverity should only return errors with matching severity
prop_filter_by_severity :: ErrorSeverity -> [ErrorSeverity] -> String -> Text -> Int -> Int -> Property
prop_filter_by_severity targetSev sevs errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      errors = [errorAt ("error" ++ show i) Error msg loc {line = i} | (i, sev) <- zip [0..] sevs] ++
                [warningAt ("warning" ++ show i) msg loc {line = i} | (i, sev) <- zip [0..] sevs] ++
                [infoAt ("info" ++ show i) msg loc {line = i} | (i, sev) <- zip [0..] sevs]
      adjustedErrors = [err {severity = sev} | (err, sev) <- zip errors sevs]
      filtered = filterBySeverity targetSev adjustedErrors
  in property $ all (\e -> severity e == targetSev) filtered

-- ============================================================================
-- Error Statistics Properties
-- ============================================================================

-- Property: getErrorStatistics should count total errors correctly
prop_error_statistics_total :: [ErrorSeverity] -> [ErrorCategory] -> String -> Text -> Int -> Int -> Property
prop_error_statistics_total sevs cats errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      errors = [errorAt ("error" ++ show i) Error msg loc {line = i} | (i, sev) <- zip [0..] sevs] ++
                [warningAt ("warning" ++ show i) msg loc {line = i} | (i, sev) <- zip [0..] sevs] ++
                [infoAt ("info" ++ show i) msg loc {line = i} | (i, sev) <- zip [0..] sevs]
      adjustedErrors = [err {severity = sev, category = cat} | (err, (sev, cat)) <- zip errors (zip sevs cats)]
      stats = getErrorStatistics adjustedErrors
      totalCount = Map.findWithDefault 0 "total" stats
  in property $ totalCount == length adjustedErrors

-- Property: getErrorStatistics should count errors by severity correctly
prop_error_statistics_by_severity :: [ErrorSeverity] -> String -> Text -> Int -> Int -> Property
prop_error_statistics_by_severity sevs errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      errors = [errorAt (errId ++ show i) Error msg loc {line = i} | (i, sev) <- zip [0..] sevs] ++
                [warningAt (errId ++ show i) msg loc {line = i} | (i, sev) <- zip [0..] sevs] ++
                [infoAt (errId ++ show i) msg loc {line = i} | (i, sev) <- zip [0..] sevs]
      adjustedErrors = [err {severity = sev} | (err, sev) <- zip errors sevs]
      stats = getErrorStatistics adjustedErrors
      expectedCounts = Map.fromList 
        [(show sev, length $ filter (\e -> severity e == sev) adjustedErrors) | sev <- [Fatal, Error, Warning, Info]]
  in property $ all (\sev -> Map.findWithDefault 0 (show sev) stats == 
                       Map.findWithDefault 0 (show sev) expectedCounts) [Fatal, Error, Warning, Info]

-- Property: getErrorStatistics should count errors by category correctly
prop_error_statistics_by_category :: [ErrorCategory] -> String -> Text -> Int -> Int -> Property
prop_error_statistics_by_category cats errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      errors = [errorWithCategory (errId ++ show i) cat msg loc {line = i} | (i, cat) <- zip [0..] cats]
      stats = getErrorStatistics errors
      expectedCounts = Map.fromList 
        [(show cat, length $ filter (\e -> category e == cat) errors) | cat <- cats]
  in property $ all (\cat -> Map.findWithDefault 0 (show cat) stats == 
                       Map.findWithDefault 0 (show cat) expectedCounts) cats

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: canRecoverFrom should return recovery.canRecover
prop_can_recover_from :: Bool -> Bool -> String -> Text -> Int -> Int -> Property
prop_can_recover_from canRec shouldCont errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      recovery = RecoveryStrategy canRec shouldCont Nothing Nothing 50 0.5
      err = (errorAt errId Error msg loc) {recovery = recovery}
  in property $ canRecoverFrom err == canRec

-- Property: shouldContinueAfter should return recovery.shouldContinue
prop_should_continue_after :: Bool -> Bool -> String -> Text -> Int -> Int -> Property
prop_should_continue_after canRec shouldCont errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      recovery = RecoveryStrategy canRec shouldCont Nothing Nothing 50 0.5
      err = (errorAt errId Error msg loc) {recovery = recovery}
  in property $ shouldContinueAfter err == shouldCont

tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck Properties Tests"
  [ testProperty "severityPriority ordering" prop_severity_priority_ordering
  , testProperty "compareSeverity consistency" prop_compare_severity_consistency
  , testProperty "isAtLeast reflexive" prop_is_at_least_reflexive
  , testProperty "isAtLeast transitive" prop_is_at_least_transitive
  , testProperty "fatal is most severe" prop_fatal_is_most_severe
  , testProperty "info is least severe" prop_info_is_least_severe
  , testProperty "getErrorLine" prop_get_error_line
  , testProperty "getErrorColumn" prop_get_error_column
  , testProperty "emptyContext properties" prop_empty_context_properties
  , testProperty "fatalRecovery properties" prop_fatal_recovery_properties
  , testProperty "errorRecovery properties" prop_error_recovery_properties
  , testProperty "warningRecovery properties" prop_warning_recovery_properties
  , testProperty "infoRecovery properties" prop_info_recovery_properties
  , testProperty "customRecovery properties" prop_custom_recovery_properties
  , testProperty "errorAt properties" prop_error_at_properties
  , testProperty "errorWithCategory properties" prop_error_with_category_properties
  , testProperty "warningAt properties" prop_warning_at_properties
  , testProperty "infoAt properties" prop_info_at_properties
  , testProperty "withLocation properties" prop_with_location_properties
  , testProperty "withContext properties" prop_with_context_properties
  , testProperty "hasCategory matching" prop_has_category_matching
  , testProperty "hasCategory non-matching" prop_has_category_non_matching
  , testProperty "filterByCategory" prop_filter_by_category
  , testProperty "filterBySeverity" prop_filter_by_severity
  , testProperty "errorStatistics total" prop_error_statistics_total
  , testProperty "errorStatistics by severity" prop_error_statistics_by_severity
  , testProperty "errorStatistics by category" prop_error_statistics_by_category
  , testProperty "canRecoverFrom" prop_can_recover_from
  , testProperty "shouldContinueAfter" prop_should_continue_after
  ]