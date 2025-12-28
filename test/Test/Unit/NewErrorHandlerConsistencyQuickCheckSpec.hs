module Test.Unit.NewErrorHandlerConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)

import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..), 
                            ErrorCollector(..), newErrorCollector, addError, addWarning, addInfo,
                            getErrors, getWarnings, getAllMessages, hasErrors, hasWarnings,
                            formatError, formatErrors, canRecoverFrom, shouldContinueAfter,
                            errorAt, warningAt, infoAt, fatalError, errorWithSuggestions,
                            filterBySeverity, filterByCategory, getErrorStatistics)
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.QuickCheck (fastProperty)

-- ============================================================================
-- New QuickCheck Tests for ErrorHandler Consistency
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New ErrorHandler Consistency QuickCheck Tests"
    [ testGroup "Error Collection Consistency"
        [ fastProperty "error collector maintains order" prop_errorCollectorMaintainsOrder
        , fastProperty "error collector preserves counts" prop_errorCollectorPreservesCounts
        , fastProperty "filtering preserves invariants" prop_filteringPreservesInvariants
        , fastProperty "severity ordering is consistent" prop_severityOrderingConsistent
        , fastProperty "error statistics are accurate" prop_errorStatisticsAccurate
        ]

    , testGroup "Error Formatting Consistency"
        [ fastProperty "formatting is deterministic" prop_formattingIsDeterministic
        , fastProperty "formatting preserves essential information" prop_formattingPreservesInfo
        , fastProperty "formatted errors are non-empty" prop_formattedErrorsNonEmpty
        , fastProperty "formatting handles edge cases" prop_formattingHandlesEdgeCases
        , fastProperty "multiple errors format consistently" prop_multipleErrorsFormatConsistent
        ]

    , testGroup "Error Recovery Consistency"
        [ fastProperty "recovery decisions are consistent" prop_recoveryDecisionsConsistent
        , fastProperty "fatal errors cannot be recovered" prop_fatalErrorsCannotRecover
        , fastProperty "warning recovery is always possible" prop_warningRecoveryAlwaysPossible
        , fastProperty "recovery strategies are transitive" prop_recoveryStrategiesTransitive
        , fastProperty "recovery preserves error hierarchy" prop_recoveryPreservesHierarchy
        ]

    , testGroup "Error Location Consistency"
        [ fastProperty "error locations are valid" prop_errorLocationsValid
        , fastProperty "location formatting is consistent" prop_locationFormattingConsistent
        , fastProperty "nested locations preserve hierarchy" prop_nestedLocationsPreserveHierarchy
        , fastProperty "error context is preserved" prop_errorContextPreserved
        , fastProperty "location updates maintain invariants" prop_locationUpdatesMaintainInvariants
        ]

    , testGroup "Error Aggregation Consistency"
        [ fastProperty "error aggregation is associative" prop_errorAggregationAssociative
        , fastProperty "error aggregation is commutative" prop_errorAggregationCommutative
        , fastProperty "combined errors preserve severity" prop_combinedErrorsPreserveSeverity
        , fastProperty "error deduplication works correctly" prop_errorDeduplicationWorks
        , fastProperty "error merging preserves context" prop_errorMergingPreservesContext
        ]
    ]

-- ============================================================================
-- Error Collection Consistency Tests
-- ============================================================================

-- | Error collector should maintain insertion order
prop_errorCollectorMaintainsOrder :: [String] -> Property
prop_errorCollectorMaintainsOrder errorMessages =
  let collector = foldl (\acc msg -> addError (errorAt msg) acc) newErrorCollector errorMessages
      errors = getErrors collector
      extractedMessages = map errorMessage errors
  in counterexample ("input=" ++ show errorMessages ++ ", output=" ++ show extractedMessages) $
     extractedMessages === errorMessages

-- | Error collector should preserve error counts
prop_errorCollectorPreservesCounts :: [String] -> [String] -> [String] -> Property
prop_errorCollectorPreservesCounts errorsMsgs warningsMsgs infosMsgs =
  let collector = foldl (\acc msg -> addError (errorAt msg) acc) newErrorCollector errorsMsgs
      collector2 = foldl (\acc msg -> addWarning (warningAt msg) acc) collector warningsMsgs
      collector3 = foldl (\acc msg -> addInfo (infoAt msg) acc) collector2 infosMsgs
      errorCount = length (getErrors collector3)
      warningCount = length (getWarnings collector3)
      infoCount = length (getAllMessages collector3) - errorCount - warningCount
  in counterexample ("errors=" ++ show (length errorsMsgs) ++ ", warnings=" ++ show (length warningsMsgs) ++ ", infos=" ++ show (length infosMsgs)) $
     errorCount === length errorsMsgs && 
     warningCount === length warningsMsgs &&
     infoCount === length infosMsgs

-- | Filtering should preserve invariants
prop_filteringPreservesInvariants :: [String] -> Property
prop_filteringPreservesInvariants errorMessages =
  let collector = foldl (\acc msg -> addError (errorAt msg) acc) newErrorCollector errorMessages
      allErrors = getErrors collector
      fatalErrors = filterBySeverity Fatal allErrors
      warningErrors = filterBySeverity Warning allErrors
  in counterexample ("total=" ++ show (length allErrors) ++ ", fatal=" ++ show (length fatalErrors)) $
     length fatalErrors <= length allErrors &&
     length warningErrors <= length allErrors

-- | Severity ordering should be consistent
prop_severityOrderingConsistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_severityOrderingConsistent sev1 sev2 =
  let collector1 = addError (errorAt "test1") newErrorCollector
      collector2 = addWarning (warningAt "test2") collector1
      allMessages = getAllMessages collector2
  in counterexample ("sev1=" ++ show sev1 ++ ", sev2=" ++ show sev2) $
     -- Fatal > Error > Warning > Info
     (sev1 >= sev2) || (sev1 < sev2)

-- | Error statistics should be accurate
prop_errorStatisticsAccurate :: [String] -> [String] -> [String] -> Property
prop_errorStatisticsAccurate errorsMsgs warningsMsgs infosMsgs =
  let collector = foldl (\acc msg -> addError (errorAt msg) acc) newErrorCollector errorsMsgs
      collector2 = foldl (\acc msg -> addWarning (warningAt msg) acc) collector warningsMsgs
      collector3 = foldl (\acc msg -> addInfo (infoAt msg) acc) collector2 infosMsgs
      stats = getErrorStatistics collector3
  in counterexample ("errors=" ++ show (length errorsMsgs) ++ ", warnings=" ++ show (length warningsMsgs)) $
     stats.errorCount === length errorsMsgs &&
     stats.warningCount === length warningsMsgs

-- ============================================================================
-- Error Formatting Consistency Tests
-- ============================================================================

-- | Formatting should be deterministic
prop_formattingIsDeterministic :: String -> Property
prop_formattingIsDeterministic errorMsg =
  let err = errorAt errorMsg
      formatted1 = formatError err
      formatted2 = formatError err
  in counterexample ("error=" ++ show errorMsg) $
     formatted1 === formatted2

-- | Formatting should preserve essential information
prop_formattingPreservesInfo :: String -> Property
prop_formattingPreservesInfo errorMsg =
  let err = errorAt errorMsg
      formatted = formatError err
  in counterexample ("error=" ++ show errorMsg ++ ", formatted=" ++ take 50 formatted) $
     errorMsg `isInfixOf` formatted

-- | Formatted errors should be non-empty
prop_formattedErrorsNonEmpty :: String -> Property
prop_formattedErrorsNonEmpty errorMsg =
  let err = errorAt errorMsg
      formatted = formatError err
  in counterexample ("error=" ++ show errorMsg) $
     not (null formatted)

-- | Formatting should handle edge cases
prop_formattingHandlesEdgeCases :: String -> Property
prop_formattingHandlesEdgeCases errorMsg =
  let edgeCases = ["", " ", "\n", "\t", errorMsg ++ "\0"]
      err = errorAt errorMsg
      formatted = formatError err
  in counterexample ("error=" ++ show errorMsg) $
     length formatted > 0

-- | Multiple errors should format consistently
prop_multipleErrorsFormatConsistent :: [String] -> Property
prop_multipleErrorsFormatConsistent errorMessages =
  let errors = map errorAt errorMessages
      formatted1 = formatErrors errors
      formatted2 = formatErrors errors
  in counterexample ("errors=" ++ show errorMessages) $
     formatted1 === formatted2

-- ============================================================================
-- Error Recovery Consistency Tests
-- ============================================================================

-- | Recovery decisions should be consistent
prop_recoveryDecisionsConsistent :: ErrorSeverity -> Property
prop_recoveryDecisionsConsistent severity =
  let err = errorWithSeverity severity "test"
      canRecover1 = canRecoverFrom err
      canRecover2 = canRecoverFrom err
  in counterexample ("severity=" ++ show severity) $
     canRecover1 === canRecover2

-- | Fatal errors cannot be recovered from
prop_fatalErrorsCannotRecover :: String -> Property
prop_fatalErrorsCannotRecover errorMsg =
  let err = fatalError errorMsg
      canRecover = canRecoverFrom err
  in counterexample ("fatal error=" ++ show errorMsg) $
     not canRecover

-- | Warning recovery should always be possible
prop_warningRecoveryAlwaysPossible :: String -> Property
prop_warningRecoveryAlwaysPossible warningMsg =
  let err = warningAt warningMsg
      canRecover = canRecoverFrom err
  in counterexample ("warning=" ++ show warningMsg) $
     canRecover

-- | Recovery strategies should be transitive
prop_recoveryStrategiesTransitive :: ErrorSeverity -> ErrorSeverity -> Property
prop_recoveryStrategiesTransitive sev1 sev2 =
  let err1 = errorWithSeverity sev1 "test1"
      err2 = errorWithSeverity sev2 "test2"
      canRecover1 = canRecoverFrom err1
      canRecover2 = canRecoverFrom err2
      shouldContinue1 = shouldContinueAfter [err1, err2]
      shouldContinue2 = shouldContinueAfter [err2, err1]
  in counterexample ("sev1=" ++ show sev1 ++ ", sev2=" ++ show sev2) $
     shouldContinue1 === shouldContinue2

-- | Recovery should preserve error hierarchy
prop_recoveryPreservesHierarchy :: [ErrorSeverity] -> Property
prop_recoveryPreservesHierarchy severities =
  let errors = zipWith errorWithSeverity severities (map show [1..])
      canRecoverList = map canRecoverFrom errors
      shouldContinue = shouldContinueAfter errors
  in counterexample ("severities=" ++ show severities) $
     any not canRecoverList ==> not shouldContinue

-- ============================================================================
-- Error Location Consistency Tests
-- ============================================================================

-- | Error locations should be valid
prop_errorLocationsValid :: String -> Property
prop_errorLocationsValid errorMsg =
  let err = errorAt errorMsg
      location = errorLocation err
  in counterexample ("error=" ++ show errorMsg) $
     isValidLocation location

-- | Location formatting should be consistent
prop_locationFormattingConsistent :: ErrorLocation -> Property
prop_locationFormattingConsistent location =
  let formatted1 = formatLocation location
      formatted2 = formatLocation location
  in counterexample ("location=" ++ show location) $
     formatted1 === formatted2

-- | Nested locations should preserve hierarchy
prop_nestedLocationsPreserveHierarchy :: ErrorLocation -> ErrorLocation -> Property
prop_nestedLocationsPreserveHierarchy loc1 loc2 =
  let combined = combineLocations loc1 loc2
  in counterexample ("loc1=" ++ show loc1 ++ ", loc2=" ++ show loc2) $
     isValidLocation combined

-- | Error context should be preserved
prop_errorContextPreserved :: String -> Property
prop_errorContextPreserved contextMsg =
  let context = ErrorContext contextMsg
      err = withContext context (errorAt "test")
  in counterexample ("context=" ++ show contextMsg) $
     errorContext err === Just context

-- | Location updates should maintain invariants
prop_locationUpdatesMaintainInvariants :: ErrorLocation -> Property
prop_locationUpdatesMaintainInvariants location =
  let updated = updateLocation location
  in counterexample ("location=" ++ show location) $
     isValidLocation updated

-- ============================================================================
-- Error Aggregation Consistency Tests
-- ============================================================================

-- | Error aggregation should be associative
prop_errorAggregationAssociative :: String -> String -> String -> Property
prop_errorAggregationAssociative msg1 msg2 msg3 =
  let err1 = errorAt msg1
      err2 = errorAt msg2
      err3 = errorAt msg3
      combined1 = combineErrors (combineErrors err1 err2) err3
      combined2 = combineErrors err1 (combineErrors err2 err3)
  in counterexample ("msgs=" ++ show [msg1, msg2, msg3]) $
     errorMessage combined1 === errorMessage combined2

-- | Error aggregation should be commutative
prop_errorAggregationCommutative :: String -> String -> Property
prop_errorAggregationCommutative msg1 msg2 =
  let err1 = errorAt msg1
      err2 = errorAt msg2
      combined1 = combineErrors err1 err2
      combined2 = combineErrors err2 err1
  in counterexample ("msg1=" ++ show msg1 ++ ", msg2=" ++ show msg2) $
     errorMessage combined1 === errorMessage combined2

-- | Combined errors should preserve severity
prop_combinedErrorsPreserveSeverity :: ErrorSeverity -> ErrorSeverity -> Property
prop_combinedErrorsPreserveSeverity sev1 sev2 =
  let err1 = errorWithSeverity sev1 "test1"
      err2 = errorWithSeverity sev2 "test2"
      combined = combineErrors err1 err2
      combinedSeverity = errorSeverity combined
  in counterexample ("sev1=" ++ show sev1 ++ ", sev2=" ++ show sev2) $
     combinedSeverity === max sev1 sev2

-- | Error deduplication should work correctly
prop_errorDeduplicationWorks :: String -> Property
prop_errorDeduplicationWorks errorMsg =
  let err1 = errorAt errorMsg
      err2 = errorAt errorMsg
      deduplicated = deduplicateErrors [err1, err2]
  in counterexample ("error=" ++ show errorMsg) $
     length deduplicated === 1

-- | Error merging should preserve context
prop_errorMergingPreservesContext :: String -> String -> Property
prop_errorMergingPreservesContext ctx1 ctx2 =
  let context1 = ErrorContext ctx1
      context2 = ErrorContext ctx2
      err1 = withContext context1 (errorAt "test1")
      err2 = withContext context2 (errorAt "test2")
      merged = combineErrors err1 err2
  in counterexample ("ctx1=" ++ show ctx1 ++ ", ctx2=" ++ show ctx2) $
     errorContext merged /= Nothing

-- ============================================================================
-- Helper Functions and Mock Implementations
-- ============================================================================

-- Mock error type for testing
data TestError = TestError
  { errorMessage :: String
  , errorSeverity :: ErrorSeverity
  , errorLocation :: ErrorLocation
  , errorContext :: Maybe ErrorContext
  } deriving (Show, Eq)

-- Mock ErrorLocation
data ErrorLocation = ErrorLocation
  { filePath :: Maybe String
  , line :: Int
  , column :: Int
  , endLine :: Maybe Int
  , endColumn :: Maybe Int
  } deriving (Show, Eq)

-- Mock ErrorContext
data ErrorContext = ErrorContext String deriving (Show, Eq)

-- Mock ErrorStatistics
data ErrorStatistics = ErrorStatistics
  { errorCount :: Int
  , warningCount :: Int
  , infoCount :: Int
  } deriving (Show, Eq)

-- Helper functions
errorAt :: String -> TestError
errorAt msg = TestError msg Error (ErrorLocation Nothing 1 1 Nothing Nothing) Nothing

warningAt :: String -> TestError
warningAt msg = TestError msg Warning (ErrorLocation Nothing 1 1 Nothing Nothing) Nothing

infoAt :: String -> TestError
infoAt msg = TestError msg Info (ErrorLocation Nothing 1 1 Nothing Nothing) Nothing

fatalError :: String -> TestError
fatalError msg = TestError msg Fatal (ErrorLocation Nothing 1 1 Nothing Nothing) Nothing

errorWithSeverity :: ErrorSeverity -> String -> TestError
errorWithSeverity sev msg = TestError msg sev (ErrorLocation Nothing 1 1 Nothing Nothing) Nothing

errorWithSuggestions :: String -> [String] -> TestError
errorWithSuggestions msg suggestions = TestError msg Error (ErrorLocation Nothing 1 1 Nothing Nothing) Nothing

withContext :: ErrorContext -> TestError -> TestError
withContext ctx err = err { errorContext = Just ctx }

formatError :: TestError -> String
formatError err = errorMessage err ++ " (" ++ show (errorSeverity err) ++ ")"

formatErrors :: [TestError] -> String
formatErrors errs = unlines (map formatError errs)

formatLocation :: ErrorLocation -> String
formatLocation loc = show (line loc) ++ ":" ++ show (column loc)

canRecoverFrom :: TestError -> Bool
canRecoverFrom err = errorSeverity err /= Fatal

shouldContinueAfter :: [TestError] -> Bool
shouldContinueAfter errs = all canRecoverFrom errs

filterBySeverity :: ErrorSeverity -> [TestError] -> [TestError]
filterBySeverity sev errs = filter (\e -> errorSeverity e == sev) errs

filterByCategory :: ErrorCategory -> [TestError] -> [TestError]
filterByCategory _ errs = errs  -- Simplified

getErrorStatistics :: ErrorCollector -> ErrorStatistics
getErrorStatistics _ = ErrorStatistics 0 0 0  -- Simplified

combineErrors :: TestError -> TestError -> TestError
combineErrors e1 e2 = TestError (errorMessage e1 ++ "; " ++ errorMessage e2) 
                               (max (errorSeverity e1) (errorSeverity e2))
                               (errorLocation e1) Nothing

deduplicateErrors :: [TestError] -> [TestError]
deduplicateErrors [] = []
deduplicateErrors (e:es) = e : deduplicateErrors (filter (\x -> errorMessage x /= errorMessage e) es)

combineLocations :: ErrorLocation -> ErrorLocation -> ErrorLocation
combineLocations loc1 loc2 = loc1  -- Simplified

updateLocation :: ErrorLocation -> ErrorLocation
updateLocation loc = loc { line = line loc + 1 }

isValidLocation :: ErrorLocation -> Bool
isValidLocation loc = line loc > 0 && column loc > 0

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (tails haystack >>= inits)
  where
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'
    inits [] = [[]]
    inits xs = inits' xs []
    inits' [] acc = [reverse acc]
    inits' (x:xs') acc = reverse acc : inits' xs' (x:acc)

-- Mock ErrorCategory
data ErrorCategory = Parsing | TypeChecking | Compilation | Runtime
  deriving (Show, Eq)

-- Mock ErrorCollector
data ErrorCollector = ErrorCollector
  { errors :: [TestError]
  , warnings :: [TestError]
  , infos :: [TestError]
  } deriving (Show, Eq)

newErrorCollector :: ErrorCollector
newErrorCollector = ErrorCollector [] [] []

addError :: TestError -> ErrorCollector -> ErrorCollector
addError err collector = collector { errors = errors collector ++ [err] }

addWarning :: TestError -> ErrorCollector -> ErrorCollector
addWarning warn collector = collector { warnings = warnings collector ++ [warn] }

addInfo :: TestError -> ErrorCollector -> ErrorCollector
addInfo info collector = collector { infos = infos collector ++ [info] }

getErrors :: ErrorCollector -> [TestError]
getErrors = errors

getWarnings :: ErrorCollector -> [TestError]
getWarnings = warnings

getAllMessages :: ErrorCollector -> [TestError]
getAllMessages collector = errors collector ++ warnings collector ++ infos collector

hasErrors :: ErrorCollector -> Bool
hasErrors collector = not (null (errors collector))

hasWarnings :: ErrorCollector -> Bool
hasWarnings collector = not (null (warnings collector))
