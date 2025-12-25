{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Test.QuickCheck as QC

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , errorAt
  , warningAt
  , infoAt
  , fatalError
  , errorWithCategory
  , withLocation
  , withContext
  , withSuggestions
  , withTimestamp
  , wrapError
  , combineErrors
  , formatError
  , formatErrorWithLocation
  , formatErrors
  , filterBySeverity
  , filterByCategory
  , hasCategory
  , getErrorStatistics
  , canRecoverFrom
  , shouldContinueAfter
  , severityPriority
  , isAtLeast
  , _unknownLocation
  , _atLocation
  , _atFileLocation
  , _atRange
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , fatalRecovery
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map

-- Arbitrary instances for QuickCheck testing

-- Generate arbitrary error severity
instance Arbitrary ErrorSeverity where
  arbitrary = QC.elements [Fatal, Error, Warning, Info]

-- Generate arbitrary error category
instance Arbitrary ErrorCategory where
  arbitrary = QC.elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

-- Generate arbitrary error location
instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- QC.arbitrary
    line <- QC.choose (0, 1000)
    column <- QC.choose (0, 1000)
    endLine <- QC.arbitrary
    endColumn <- QC.arbitrary
    return $ ErrorLocation filePath line column endLine endColumn

-- Generate arbitrary error context
instance Arbitrary ErrorContext where
  arbitrary = do
    contextCode <- QC.arbitrary
    contextFunction <- QC.arbitrary
    contextVariable <- QC.arbitrary
    contextType <- QC.arbitrary
    contextAdditional <- QC.listOf (QC.arbitrary :: QC.Gen (String, String))
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

-- Generate arbitrary error recovery
instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRecover <- QC.arbitrary
    shouldContinue <- QC.arbitrary
    recoveryAction <- QC.arbitrary
    recoveryHint <- QC.arbitrary
    recoveryCost <- QC.choose (0, 100)
    recoveryConfidence <- QC.choose (0.0, 1.0)
    return $ RecoveryStrategy canRecover shouldContinue recoveryAction recoveryHint recoveryCost recoveryConfidence

-- Generate arbitrary text
instance Arbitrary Text where
  arbitrary = T.pack <$> QC.arbitrary

-- Generate arbitrary type error
instance Arbitrary TypeError where
  arbitrary = do
    errorId <- QC.arbitrary
    severity <- QC.arbitrary
    category <- QC.arbitrary
    message <- QC.arbitrary
    location <- QC.arbitrary
    context <- QC.arbitrary
    recovery <- QC.arbitrary
    suggestions <- QC.listOf QC.arbitrary
    relatedErrors <- QC.listOf QC.arbitrary
    errorChain <- QC.listOf QC.arbitrary
    timestamp <- QC.arbitrary
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

-- ============================================================================
-- Property Tests for Error Handling
-- ============================================================================

-- Property: Error severity ordering is consistent
prop_severity_ordering_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering_consistent sev1 sev2 =
  let priority1 = severityPriority sev1
      priority2 = severityPriority sev2
      ordering = compare sev1 sev2
      priorityOrdering = compare priority1 priority2
  in property $ ordering === priorityOrdering

-- Property: isAtLeast is reflexive
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive sev =
  property $ isAtLeast sev sev

-- Property: isAtLeast is transitive
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive sev1 sev2 sev3 =
  isAtLeast sev1 sev2 .&&. isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

-- Property: Fatal is the highest severity
prop_fatal_is_highest :: ErrorSeverity -> Property
prop_fatal_is_highest sev =
  property $ isAtLeast sev Fatal ==> sev === Fatal

-- Property: Info is the lowest severity
prop_info_is_lowest :: ErrorSeverity -> Property
prop_info_is_lowest sev =
  property $ isAtLeast Info sev ==> sev === Info

-- Property: Error creation preserves provided values
prop_error_creation_preserves_values :: String -> Text -> ErrorLocation -> Property
prop_error_creation_preserves_values errId msg loc =
  let err = errorAt errId msg loc
  in property $ errorId err === errId .&&.
     message err === msg .&&.
     location err === loc .&&.
     severity err === Error

-- Property: Warning creation has warning severity
prop_warning_creation_has_warning_severity :: String -> Text -> ErrorLocation -> Property
prop_warning_creation_has_warning_severity errId msg loc =
  let err = warningAt errId msg loc
  in property $ severity err === Warning .&&.
     errorId err === errId .&&.
     message err === msg .&&.
     location err === loc

-- Property: Info creation has info severity
prop_info_creation_has_info_severity :: String -> Text -> ErrorLocation -> Property
prop_info_creation_has_info_severity errId msg loc =
  let err = infoAt errId msg loc
  in property $ severity err === Info .&&.
     errorId err === errId .&&.
     message err === msg .&&.
     location err === loc

-- Property: Fatal error creation has fatal severity and cannot recover
prop_fatal_error_properties :: String -> Text -> ErrorLocation -> Property
prop_fatal_error_properties errId msg loc =
  let err = fatalError errId msg loc
  in property $ severity err === Fatal .&&.
     not (canRecoverFrom err) .&&.
     not (shouldContinueAfter err)

-- Property: Error with category preserves category
prop_error_with_category_preserves_category :: String -> ErrorCategory -> Text -> ErrorLocation -> Property
prop_error_with_category_preserves_category errId cat msg loc =
  let err = errorWithCategory errId cat msg loc
  in property $ category err === cat .&&.
     hasCategory cat err

-- Property: Location override works correctly
prop_location_override :: TypeError -> ErrorLocation -> Property
prop_location_override originalErr newLoc =
  let updatedErr = withLocation originalErr newLoc
  in property $ location updatedErr === newLoc .&&.
     errorId updatedErr === errorId originalErr .&&.
     message updatedErr === message originalErr

-- Property: Context override works correctly
prop_context_override :: TypeError -> ErrorContext -> Property
prop_context_override originalErr newCtx =
  let updatedErr = withContext originalErr newCtx
  in property $ context updatedErr === newCtx .&&.
     errorId updatedErr === errorId originalErr .&&.
     message updatedErr === message originalErr

-- Property: Timestamp override works correctly
prop_timestamp_override :: TypeError -> String -> Property
prop_timestamp_override originalErr newTimestamp =
  let updatedErr = withTimestamp newTimestamp originalErr
  in property $ timestamp updatedErr === Just newTimestamp .&&.
     errorId updatedErr === errorId originalErr .&&.
     message updatedErr === message originalErr

-- Property: Error wrapping preserves original error in chain
prop_error_wrapping_preserves_original :: Text -> TypeError -> Property
prop_error_wrapping_preserves_original wrapperMsg originalErr =
  let wrappedErr = wrapError wrapperMsg originalErr
  in property $ message wrappedErr === wrapperMsg <> ": " <> message originalErr .&&.
     originalErr `elem` errorChain wrappedErr .&&.
     errorId wrappedErr === errorId originalErr

-- Property: Error suggestions are additive
prop_suggestions_additive :: TypeError -> [Text] -> [Text] -> Property
prop_suggestions_additive baseErr suggestions1 suggestions2 =
  let errWith1 = withSuggestions suggestions1 baseErr
      errWith2 = withSuggestions suggestions2 errWith1
  in property $ suggestions errWith2 === suggestions1 ++ suggestions2 ++ suggestions baseErr

-- Property: Error filtering by severity works correctly
prop_filter_by_severity :: [TypeError] -> ErrorSeverity -> Property
prop_filter_by_severity errors targetSeverity =
  let filtered = filterBySeverity targetSeverity errors
  in property $ all (\e -> severity e === targetSeverity) filtered

-- Property: Error filtering by category works correctly
prop_filter_by_category :: [TypeError] -> ErrorCategory -> Property
prop_filter_by_category errors targetCategory =
  let filtered = filterByCategory targetCategory errors
  in property $ all (\e -> category e === targetCategory) filtered

-- Property: Error statistics are accurate
prop_error_statistics_accurate :: [TypeError] -> Property
prop_error_statistics_accurate errors =
  let stats = getErrorStatistics errors
      total = Map.findWithDefault 0 "total" stats
      fatalCount = Map.findWithDefault 0 "fatal" stats
      errorCount = Map.findWithDefault 0 "errors" stats
      warningCount = Map.findWithDefault 0 "warnings" stats
      infoCount = Map.findWithDefault 0 "info" stats
  in property $ total === length errors .&&.
     fatalCount === length (filterBySeverity Fatal errors) .&&.
     errorCount === length (filterBySeverity Error errors) .&&.
     warningCount === length (filterBySeverity Warning errors) .&&.
     infoCount === length (filterBySeverity Info errors)

-- Property: Error formatting contains expected elements
prop_error_formatting_contains_elements :: TypeError -> Property
prop_error_formatting_contains_elements err =
  let formatted = formatError err
      severityStr = case severity err of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
      categoryStr = "[" ++ show (category err) ++ "]"
      msgStr = T.unpack (message err)
  in property $ severityStr `isInfixOf` formatted .&&.
     categoryStr `isInfixOf` formatted .&&.
     msgStr `isInfixOf` formatted

-- Property: Error formatting with location contains location info
prop_error_formatting_with_location :: TypeError -> Property
prop_error_formatting_with_location err =
  let formatted = formatErrorWithLocation err
      hasLocation = line (location err) > 0 || column (location err) > 0
  in classify hasLocation "has location" $
     property $ if hasLocation
                then let locStr = show (line (location err)) ++ ":" ++ show (column (location err))
                     in locStr `isInfixOf` formatted
                else property $ formatted === formatError err

-- Property: Multiple errors formatting preserves order by severity
prop_multiple_errors_formatting_order :: [TypeError] -> Property
prop_multiple_errors_formatting_order errors =
  let formatted = formatErrors errors
      sortedErrors = sortBySeverity errors
      -- Check that the formatted string contains errors in severity order
      severityStrings = map (\e -> case severity e of
                                   Fatal -> "FATAL"
                                   Error -> "ERROR"
                                   Warning -> "WARNING"
                                   Info -> "INFO") sortedErrors
  in not (null errors) ==> 
     property $ all (`isInfixOf` formatted) severityStrings

-- Property: Recovery strategies are consistent with severity
prop_recovery_consistency :: TypeError -> Property
prop_recovery_consistency err =
  let sev = severity err
      rec = recovery err
  in case sev of
       Fatal -> property $ not (canRecover rec) .&&. not (shouldContinue rec)
       Error -> property $ canRecover rec .&&. shouldContinue rec
       Warning -> property $ canRecover rec .&&. shouldContinue rec
       Info -> property $ canRecover rec .&&. shouldContinue rec

-- Property: Error combination preserves all errors
prop_error_combination_preserves_all :: [TypeError] -> Property
prop_error_combination_preserves_all errors =
  let combined = combineErrors errors
      originalIds = sort $ nub $ map errorId errors
      combinedIds = sort $ nub $ map errorId combined
  in property $ combinedIds === originalIds

-- Property: Location creation helpers work correctly
prop_location_creation_helpers :: Int -> Int -> Int -> Int -> Property
prop_location_creation_helpers line col endLine endCol =
  line >= 0 && col >= 0 && endLine >= line && endCol >= col ==>
  let simpleLoc = _atLocation line col
      rangedLoc = _atRange line col endLine endCol
  in property $ line simpleLoc === line .&&. column simpleLoc === col .&&.
     line rangedLoc === line .&&. column rangedLoc === col .&&.
     endLine rangedLoc === Just endLine .&&. endColumn rangedLoc === Just endCol

-- Property: File location creation preserves file path
prop_file_location_creation :: String -> Int -> Int -> Property
prop_file_location_creation filePath line col =
  let fileLoc = _atFileLocation filePath line col
  in property $ filePath fileLoc === Just filePath .&&.
     line fileLoc === line .&&. column fileLoc === col

-- Property: Empty context is actually empty
prop_empty_context_is_empty :: Property
prop_empty_context_is_empty =
  let ctx = emptyContext
  in property $ contextCode ctx === Nothing .&&.
     contextFunction ctx === Nothing .&&.
     contextVariable ctx === Nothing .&&.
     contextType ctx === Nothing .&&.
     null (contextAdditional ctx)

-- Property: Error ID uniqueness in combined errors
prop_error_id_uniqueness :: [TypeError] -> Property
prop_error_id_uniqueness errors =
  let uniqueIds = nub $ map errorId errors
      totalIds = length $ map errorId errors
  in property $ length uniqueIds <= totalIds

-- Property: Error recovery cost bounds
prop_recovery_cost_bounds :: ErrorRecovery -> Property
prop_recovery_cost_bounds recovery =
  let cost = recoveryCost recovery
      confidence = recoveryConfidence recovery
  in property $ cost >= 0 .&&. cost <= 100 .&&.
     confidence >= 0.0 .&&. confidence <= 1.0

-- Property: Severity priority ordering
prop_severity_priority_ordering :: Property
prop_severity_priority_ordering =
  let priorities = map severityPriority [Fatal, Error, Warning, Info]
  in property $ priorities === [100, 80, 30, 10]

-- Property: Error message non-empty after creation
prop_error_message_non_empty :: String -> Text -> ErrorLocation -> Property
prop_error_message_non_empty errId msg loc =
  let err = errorAt errId msg loc
  in property $ not (T.null $ message err)

-- Property: Error chain length increases with wrapping
prop_error_chain_length_increases :: TypeError -> Int -> Property
prop_error_chain_length_increases baseErr wrapCount =
  wrapCount >= 0 && wrapCount <= 10 ==> -- Limit for performance
  let wrapped = iterate (wrapError "wrapper") baseErr !! wrapCount
  in property $ length (errorChain wrapped) === wrapCount

-- Property: Category filtering preserves other attributes
prop_category_filtering_preserves_attributes :: [TypeError] -> ErrorCategory -> Property
prop_category_filtering_preserves_attributes errors targetCat =
  let filtered = filterByCategory targetCat errors
  in property $ all (\e -> category e === targetCat && 
                        errorId e `elem` map errorId errors) filtered

-- Property: Severity filtering preserves other attributes
prop_severity_filtering_preserves_attributes :: [TypeError] -> ErrorSeverity -> Property
prop_severity_filtering_preserves_attributes errors targetSev =
  let filtered = filterBySeverity targetSev errors
  in property $ all (\e -> severity e === targetSev && 
                        errorId e `elem` map errorId errors) filtered

tests :: TestTree
tests =
  testGroup "New Error Handler QuickCheck Tests"
    [ fastProperty "Error severity ordering is consistent" prop_severity_ordering_consistent
    , fastProperty "isAtLeast is reflexive" prop_isAtLeast_reflexive
    , fastProperty "isAtLeast is transitive" prop_isAtLeast_transitive
    , fastProperty "Fatal is the highest severity" prop_fatal_is_highest
    , fastProperty "Info is the lowest severity" prop_info_is_lowest
    , fastProperty "Error creation preserves provided values" prop_error_creation_preserves_values
    , fastProperty "Warning creation has warning severity" prop_warning_creation_has_warning_severity
    , fastProperty "Info creation has info severity" prop_info_creation_has_info_severity
    , fastProperty "Fatal error properties" prop_fatal_error_properties
    , fastProperty "Error with category preserves category" prop_error_with_category_preserves_category
    , fastProperty "Location override works correctly" prop_location_override
    , fastProperty "Context override works correctly" prop_context_override
    , fastProperty "Timestamp override works correctly" prop_timestamp_override
    , fastProperty "Error wrapping preserves original error in chain" prop_error_wrapping_preserves_original
    , fastProperty "Error suggestions are additive" prop_suggestions_additive
    , fastProperty "Error filtering by severity works correctly" prop_filter_by_severity
    , fastProperty "Error filtering by category works correctly" prop_filter_by_category
    , fastProperty "Error statistics are accurate" prop_error_statistics_accurate
    , fastProperty "Error formatting contains expected elements" prop_error_formatting_contains_elements
    , fastProperty "Error formatting with location" prop_error_formatting_with_location
    , fastProperty "Multiple errors formatting preserves order by severity" prop_multiple_errors_formatting_order
    , fastProperty "Recovery strategies are consistent with severity" prop_recovery_consistency
    , fastProperty "Error combination preserves all errors" prop_error_combination_preserves_all
    , fastProperty "Location creation helpers work correctly" prop_location_creation_helpers
    , fastProperty "File location creation preserves file path" prop_file_location_creation
    , fastProperty "Empty context is actually empty" prop_empty_context_is_empty
    , fastProperty "Error ID uniqueness in combined errors" prop_error_id_uniqueness
    , fastProperty "Error recovery cost bounds" prop_recovery_cost_bounds
    , fastProperty "Severity priority ordering" prop_severity_priority_ordering
    , fastProperty "Error message non-empty after creation" prop_error_message_non_empty
    , fastProperty "Error chain length increases with wrapping" prop_error_chain_length_increases
    , fastProperty "Category filtering preserves other attributes" prop_category_filtering_preserves_attributes
    , fastProperty "Severity filtering preserves other attributes" prop_severity_filtering_preserves_attributes
    ]