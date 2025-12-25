{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ErrorHandlerCoreQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Compiler.Errors.Core
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    maybeFile <- oneof [return Nothing, Just <$> arbitrary]
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    endLine <- oneof [return Nothing, Just <$> choose (1, 1000)]
    endColumn <- oneof [return Nothing, Just <$> choose (1, 1000)]
    return $ ErrorLocation maybeFile line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    code <- oneof [return Nothing, Just <$> arbitrary]
    func <- oneof [return Nothing, Just <$> arbitrary]
    var <- oneof [return Nothing, Just <$> arbitrary]
    typ <- oneof [return Nothing, Just <$> arbitrary]
    additional <- listOf ((,) <$> arbitrary <*> arbitrary)
    return $ ErrorContext code func var typ additional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    action <- oneof [return Nothing, Just <$> arbitrary]
    hint <- oneof [return Nothing, Just <$> arbitrary]
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRec shouldCont action hint cost confidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf arbitrary
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- oneof [return Nothing, Just <$> arbitrary]
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

-- ============================================================================
-- Error Severity Properties
-- ============================================================================

prop_severityPriorityOrdering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severityPriorityOrdering sev1 sev2 =
  let priority1 = severityPriority sev1
      priority2 = severityPriority sev2
      ordering = compareSeverity sev1 sev2
  in counterexample "severityPriority should be consistent with compareSeverity" $
    (ordering == EQ) ==> (priority1 === priority2)

prop_severityPriorityMonotonic :: Property
prop_severityPriorityMonotonic =
  let priorities = map severityPriority [Info, Warning, Error, Fatal]
  in counterexample "severityPriority should be monotonic increasing" $
    priorities === [10, 30, 80, 100]

prop_isAtLeastProperties :: ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeastProperties minSeverity sev =
  let result = isAtLeast minSeverity sev
      expected = compareSeverity sev minSeverity /= LT
  in counterexample "isAtLeast should be consistent with compareSeverity" $
    result === expected

prop_severityPredicates :: ErrorSeverity -> Property
prop_severityPredicates sev =
  counterexample "severity predicates should be mutually exclusive" $
    (isFatal sev && not (isError sev || isWarning sev || isInfo sev)) ||
    (isError sev && not (isFatal sev || isWarning sev || isInfo sev)) ||
    (isWarning sev && not (isFatal sev || isError sev || isInfo sev)) ||
    (isInfo sev && not (isFatal sev || isError sev || isWarning sev))

-- ============================================================================
-- Error Location Properties
-- ============================================================================

prop_errorLocationConstruction :: Maybe String -> Positive Int -> Positive Int -> Property
prop_errorLocationConstruction maybeFile (Positive line) (Positive column) =
  let loc = _atLocation line column
  in counterexample "_atLocation should create location without file" $
    line loc === line .&.
    column loc === column .&.
    filePath loc === Nothing

prop_errorLocationWithFile :: String -> Positive Int -> Positive Int -> Property
prop_errorLocationWithFile file (Positive line) (Positive column) =
  let loc = _atFileLocation file line column
  in counterexample "_atFileLocation should create location with file" $
    line loc === line .&.
    column loc === column .&.
    filePath loc === Just file

prop_errorLocationWithRange :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_errorLocationWithRange (Positive startLine) (Positive startCol) (Positive endLine) (Positive endCol) =
  let loc = _atRange startLine startCol endLine endCol
  in counterexample "_atRange should create location with range" $
    line loc === startLine .&.
    column loc === startCol .&.
    endLine loc === Just endLine .&.
    endColumn loc === Just endCol

-- ============================================================================
-- Error Context Properties
-- ============================================================================

prop_emptyContextProperties :: Property
prop_emptyContextProperties =
  let ctx = emptyContext
  in counterexample "emptyContext should have all fields empty" $
    contextCode ctx === Nothing .&.
    contextFunction ctx === Nothing .&.
    contextVariable ctx === Nothing .&.
    contextType ctx === Nothing .&.
    contextAdditional ctx === []

prop_contextConstruction :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> [(String, String)] -> Property
prop_contextConstruction code func var typ additional =
  let ctx = ErrorContext code func var typ additional
  in counterexample "ErrorContext constructor should preserve all fields" $
    contextCode ctx === code .&.
    contextFunction ctx === func .&.
    contextVariable ctx === var .&.
    contextType ctx === typ .&.
    contextAdditional ctx === additional

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

prop_recoveryStrategyProperties :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_recoveryStrategyProperties canRec shouldCont action hint cost confidence =
  let recovery = RecoveryStrategy canRec shouldCont action hint cost confidence
  in counterexample "RecoveryStrategy should preserve all fields" $
    canRecover recovery === canRec .&.
    shouldContinue recovery === shouldCont .&.
    recoveryAction recovery === action .&.
    recoveryHint recovery === hint .&.
    recoveryCost recovery === cost .&.
    recoveryConfidence recovery === confidence

prop_predefinedRecoveryStrategies :: Property
prop_predefinedRecoveryStrategies =
  counterexample "predefined recovery strategies should have consistent properties" $
    canRecover fatalRecovery === False .&.
    shouldContinue fatalRecovery === False .&.
    canRecover errorRecovery === True .&.
    shouldContinue errorRecovery === True .&.
    canRecover warningRecovery === True .&.
    shouldContinue warningRecovery === True .&.
    canRecover infoRecovery === True .&.
    shouldContinue infoRecovery === True

prop_customRecoveryProperties :: Property
prop_customRecoveryProperties =
  let custom = customRecovery True False (Just "action") (Just "hint") 25 0.75
  in counterexample "customRecovery should create strategy with given properties" $
    canRecover custom === True .&.
    shouldContinue custom === False .&.
    recoveryAction custom === Just "action" .&.
    recoveryHint custom === Just "hint" .&.
    recoveryCost custom === 25 .&.
    recoveryConfidence custom === 0.75

-- ============================================================================
-- Error Construction Properties
-- ============================================================================

prop_errorAtProperties :: String -> Text -> ErrorLocation -> Property
prop_errorAtProperties errId msg loc =
  let err = errorAt errId msg loc
  in counterexample "errorAt should create error with given properties" $
    errorId err === errId .&.
    message err === msg .&.
    location err === loc .&.
    severity err === Error .&.
    category err === Unknown

prop_errorWithCategoryProperties :: String -> ErrorCategory -> Text -> ErrorLocation -> Property
prop_errorWithCategoryProperties errId cat msg loc =
  let err = errorWithCategory errId cat msg loc
  in counterexample "errorWithCategory should create error with given category" $
    errorId err === errId .&.
    message err === msg .&.
    location err === loc .&.
    severity err === Error .&.
    category err === cat

prop_warningAtProperties :: String -> Text -> ErrorLocation -> Property
prop_warningAtProperties errId msg loc =
  let warn = warningAt errId msg loc
  in counterexample "warningAt should create warning with given properties" $
    errorId warn === errId .&.
    message warn === msg .&.
    location warn === loc .&.
    severity warn === Warning .&.
    category warn === Unknown

prop_infoAtProperties :: String -> Text -> ErrorLocation -> Property
prop_infoAtProperties errId msg loc =
  let info = infoAt errId msg loc
  in counterexample "infoAt should create info with given properties" $
    errorId info === errId .&.
    message info === msg .&.
    location info === loc .&.
    severity info === Info .&.
    category info === Unknown

prop_fatalErrorProperties :: String -> Text -> ErrorLocation -> Property
prop_fatalErrorProperties errId msg loc =
  let fatal = fatalError errId msg loc
  in counterexample "fatalError should create fatal error with given properties" $
    errorId fatal === errId .&.
    message fatal === msg .&.
    location fatal === loc .&.
    severity fatal === Fatal .&.
    recovery fatal === fatalRecovery

-- ============================================================================
-- Error Modification Properties
-- ============================================================================

prop_withLocationProperties :: TypeError -> ErrorLocation -> Property
prop_withLocationProperties err loc =
  let modified = withLocation err loc
  in counterexample "withLocation should change location but preserve other fields" $
    location modified === loc .&.
    errorId modified === errorId err .&.
    message modified === message err .&.
    severity modified === severity err

prop_withContextProperties :: TypeError -> ErrorContext -> Property
prop_withContextProperties err ctx =
  let modified = withContext err ctx
  in counterexample "withContext should change context but preserve other fields" $
    context modified === ctx .&.
    errorId modified === errorId err .&.
    message modified === message err .&.
    location modified === location err

prop_withSuggestionsProperties :: TypeError -> [Text] -> Property
prop_withSuggestionsProperties err suggestions =
  let modified = withSuggestions suggestions err
  in counterexample "withSuggestions should prepend suggestions" $
    suggestions modified === suggestions ++ suggestions err .&.
    errorId modified === errorId err

prop_withTimestampProperties :: TypeError -> String -> Property
prop_withTimestampProperties err timestamp =
  let modified = withTimestamp timestamp err
  in counterexample "withTimestamp should set timestamp" $
    timestamp modified === Just timestamp .&.
    errorId modified === errorId err

prop_wrapErrorProperties :: TypeError -> Text -> Property
prop_wrapErrorProperties err wrapperMsg =
  let wrapped = wrapError wrapperMsg err
  in counterexample "wrapError should prepend message and add to chain" $
    message wrapped === wrapperMsg <> ": " <> message err .&.
    errorChain wrapped === err : errorChain err .&.
    errorId wrapped === errorId err

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

prop_filterBySeverityProperties :: [TypeError] -> ErrorSeverity -> Property
prop_filterBySeverityProperties errors targetSeverity =
  let filtered = filterBySeverity targetSeverity errors
  in counterexample "filterBySeverity should only return errors with target severity" $
    all (\e -> severity e === targetSeverity) filtered

prop_filterByCategoryProperties :: [TypeError] -> ErrorCategory -> Property
prop_filterByCategoryProperties errors targetCategory =
  let filtered = filterByCategory targetCategory errors
  in counterexample "filterByCategory should only return errors with target category" $
    all (\e -> category e === targetCategory) filtered

prop_hasCategoryProperties :: TypeError -> ErrorCategory -> Property
prop_hasCategoryProperties err cat =
  let result = hasCategory cat err
      expected = category err === cat
  in counterexample "hasCategory should check category equality" $
    result === expected

-- ============================================================================
-- Error Statistics Properties
-- ============================================================================

prop_errorStatisticsProperties :: [TypeError] -> Property
prop_errorStatisticsProperties errors =
  let stats = getErrorStatistics errors
      total = Map.findWithDefault 0 "total" stats
      fatalCount = Map.findWithDefault 0 "fatal" stats
      errorCount = Map.findWithDefault 0 "errors" stats
      warningCount = Map.findWithDefault 0 "warnings" stats
      infoCount = Map.findWithDefault 0 "info" stats
  in counterexample "error statistics should sum correctly" $
    total === length errors .&.
    fatalCount === length (filterBySeverity Fatal errors) .&.
    errorCount === length (filterBySeverity Error errors) .&.
    warningCount === length (filterBySeverity Warning errors) .&.
    infoCount === length (filterBySeverity Info errors)

prop_errorStatisticsCompleteness :: [TypeError] -> Property
prop_errorStatisticsCompleteness errors =
  let stats = getErrorStatistics errors
      expectedKeys = ["total", "fatal", "errors", "warnings", "info", 
                      "typeChecking", "ownership", "parsing", "semantic", 
                      "runtime", "constraint", "inference", "integration", "unknown"]
  in counterexample "error statistics should contain all expected keys" $
    all (`Map.member` stats) expectedKeys

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

prop_canRecoverFromProperties :: TypeError -> Property
prop_canRecoverFromProperties err =
  let result = canRecoverFrom err
      expected = canRecover (recovery err)
  in counterexample "canRecoverFrom should check recovery.canRecover" $
    result === expected

prop_shouldContinueAfterProperties :: TypeError -> Property
prop_shouldContinueAfterProperties err =
  let result = shouldContinueAfter err
      expected = shouldContinue (recovery err)
  in counterexample "shouldContinueAfter should check recovery.shouldContinue" $
    result === expected

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

prop_formatErrorContainsSeverity :: TypeError -> Property
prop_formatErrorContainsSeverity err =
  let formatted = formatError err
      severityStr = case severity err of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
  in counterexample "formatError should contain severity string" $
    severityStr `isInfixOf` formatted

prop_formatErrorContainsMessage :: TypeError -> Property
prop_formatErrorContainsMessage err =
  let formatted = formatError err
      msgStr = T.unpack (message err)
  in counterexample "formatError should contain message" $
    msgStr `isInfixOf` formatted

prop_formatErrorWithLocationContainsLocation :: TypeError -> Property
prop_formatErrorWithLocationContainsLocation err =
  let formatted = formatErrorWithLocation err
      lineStr = show (line (location err))
      colStr = show (column (location err))
  in counterexample "formatErrorWithLocation should contain line and column" $
    lineStr `isInfixOf` formatted && colStr `isInfixOf` formatted

-- Helper function
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler Core QuickCheck Tests"
  [ testGroup "Error Severity Tests"
      [ testProperty "severityPriority consistent with compareSeverity" prop_severityPriorityOrdering
      , testProperty "severityPriority is monotonic" prop_severityPriorityMonotonic
      , testProperty "isAtLeast consistent with compareSeverity" prop_isAtLeastProperties
      , testProperty "severity predicates are mutually exclusive" prop_severityPredicates
      ]
  , testGroup "Error Location Tests"
      [ testProperty "_atLocation creates location without file" prop_errorLocationConstruction
      , testProperty "_atFileLocation creates location with file" prop_errorLocationWithFile
      , testProperty "_atRange creates location with range" prop_errorLocationWithRange
      ]
  , testGroup "Error Context Tests"
      [ testProperty "emptyContext has all fields empty" prop_emptyContextProperties
      , testProperty "ErrorContext constructor preserves all fields" prop_contextConstruction
      ]
  , testGroup "Error Recovery Tests"
      [ testProperty "RecoveryStrategy preserves all fields" prop_recoveryStrategyProperties
      , testProperty "predefined recovery strategies have consistent properties" prop_predefinedRecoveryStrategies
      , testProperty "customRecovery creates strategy with given properties" prop_customRecoveryProperties
      ]
  , testGroup "Error Construction Tests"
      [ testProperty "errorAt creates error with given properties" prop_errorAtProperties
      , testProperty "errorWithCategory creates error with given category" prop_errorWithCategoryProperties
      , testProperty "warningAt creates warning with given properties" prop_warningAtProperties
      , testProperty "infoAt creates info with given properties" prop_infoAtProperties
      , testProperty "fatalError creates fatal error with given properties" prop_fatalErrorProperties
      ]
  , testGroup "Error Modification Tests"
      [ testProperty "withLocation changes location but preserves other fields" prop_withLocationProperties
      , testProperty "withContext changes context but preserves other fields" prop_withContextProperties
      , testProperty "withSuggestions prepends suggestions" prop_withSuggestionsProperties
      , testProperty "withTimestamp sets timestamp" prop_withTimestampProperties
      , testProperty "wrapError prepends message and adds to chain" prop_wrapErrorProperties
      ]
  , testGroup "Error Filtering Tests"
      [ testProperty "filterBySeverity only returns errors with target severity" prop_filterBySeverityProperties
      , testProperty "filterByCategory only returns errors with target category" prop_filterByCategoryProperties
      , testProperty "hasCategory checks category equality" prop_hasCategoryProperties
      ]
  , testGroup "Error Statistics Tests"
      [ testProperty "error statistics sum correctly" prop_errorStatisticsProperties
      , testProperty "error statistics contain all expected keys" prop_errorStatisticsCompleteness
      ]
  , testGroup "Error Recovery Function Tests"
      [ testProperty "canRecoverFrom checks recovery.canRecover" prop_canRecoverFromProperties
      , testProperty "shouldContinueAfter checks recovery.shouldContinue" prop_shouldContinueAfterProperties
      ]
  , testGroup "Error Formatting Tests"
      [ testProperty "formatError contains severity string" prop_formatErrorContainsSeverity
      , testProperty "formatError contains message" prop_formatErrorContainsMessage
      , testProperty "formatErrorWithLocation contains line and column" prop_formatErrorWithLocationContainsLocation
      ]
  ]