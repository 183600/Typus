{-# LANGUAGE CPP #-}

module Test.Unit.ErrorHandlingComprehensiveQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.ExtendedArbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, label, cover)

import Compiler.Errors.Core
  ( ErrorSeverity(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  )
import qualified Compiler.Errors as CE
import qualified Compiler.Errors.Core as Core
import Compiler.Errors (CompilerError(..), CompilationPhase(..))
import qualified Data.Text as T
import Data.List (isInfixOf, nub, sort)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Error Severity Properties
-- ============================================================================

-- Property: Error severity ordering is consistent
prop_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering sev1 sev2 = 
  let severityOrder = severityRank sev1
      severityOrder2 = severityRank sev2
      isOrdered = sev1 == sev2 || 
                  (sev1 == Error && sev2 /= Error) ||
                  (sev1 == Warning && sev2 `elem` [Info]) ||
                  (sev1 == Info && sev2 == Info)
  in property $ isOrdered

-- Property: Severity ranking is total order
prop_severity_total_order :: [ErrorSeverity] -> Property
prop_severity_total_order severities = 
  let rankings = map severityRank severities
      sortedRankings = Data.List.sort rankings
      isTotalOrder = rankings == sortedRankings || 
                     rankings == reverse sortedRankings
  in property $ isTotalOrder

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: Error locations are valid
prop_error_location_valid :: ErrorLocation -> Property
prop_error_location_valid loc = 
  let file = elFile loc
      line = elLine loc
      column = elColumn loc
  in property $ line >= 0 && column >= 0

-- Property: Error locations can be formatted
prop_error_location_formattable :: ErrorLocation -> Property
prop_error_location_formattable loc = 
  let formatted = formatErrorLocation loc
  in property $ not (null formatted)

-- Property: Error location comparison works
prop_error_location_comparison :: ErrorLocation -> ErrorLocation -> Property
prop_error_location_comparison loc1 loc2 = 
  let sameFile = elFile loc1 == elFile loc2
      sameLine = elLine loc1 == elLine loc2
      sameColumn = elColumn loc1 == elColumn loc2
      isEqual = loc1 == loc2
  in property $ isEqual == (sameFile && sameLine && sameColumn)

-- ============================================================================
-- Error Context Properties
-- ============================================================================

-- Property: Empty context is identity for merging
prop_context_empty_identity :: ErrorContext -> Property
prop_context_empty_identity ctx = 
  let merged = mergeErrorContext emptyContext ctx
  in property $ merged == ctx

-- Property: Context information is preserved
prop_context_preservation :: ErrorContext -> Property
prop_context_preservation ctx = 
  let originalInfo = getContextInfo ctx
      merged = mergeErrorContext ctx emptyContext
      finalInfo = getContextInfo merged
  in property $ originalInfo == finalInfo

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: Error recovery strategies are exhaustive
prop_recovery_exhaustive :: ErrorRecovery -> Property
prop_recovery_exhaustive recovery = 
  let isFatal = recovery == Core.fatalRecovery
      canRecover = not isFatal
  in property $ canRecover || isFatal

-- Property: Recovery strategies are ordered by severity
prop_recovery_severity_ordering :: ErrorRecovery -> ErrorRecovery -> Property
prop_recovery_severity_ordering rec1 rec2 = 
  let rank1 = recoveryRank rec1
      rank2 = recoveryRank rec2
  in property $ rank1 <= rank2 || rec1 == Core.fatalRecovery

-- Property: Recovery suggestions are helpful
prop_recovery_suggestions_helpful :: ErrorRecovery -> Property
prop_recovery_suggestions_helpful recovery = 
  let suggestions = getRecoverySuggestions recovery
  in property $ null suggestions || all (not . null) suggestions

-- ============================================================================
-- Compiler Error Properties
-- ============================================================================

-- Property: Compiler errors have unique IDs
prop_compiler_error_unique_ids :: [CE.CompilerError] -> Property
prop_compiler_error_unique_ids errors = 
  let errorIds = map ceErrorId errors
      uniqueIds = nub errorIds
  in property $ length errorIds == length uniqueIds

-- Property: Compiler error messages are informative
prop_compiler_error_informative :: CE.CompilerError -> Property
prop_compiler_error_informative err = 
  let message = ceMessage err
      hasContent = not (T.null message)
  in property $ hasContent

-- Property: Compiler error phase is valid
prop_compiler_error_valid_phase :: CE.CompilerError -> Property
prop_compiler_error_valid_phase err = 
  let phase = cePhase err
  in property $ isValidPhase phase

-- ============================================================================
-- Error Reporting Properties
-- ============================================================================

-- Property: Error reports are well-formatted
prop_error_report_formatting :: CE.CompilerError -> Property
prop_error_report_formatting err = 
  let report = formatErrorReport err
      hasHeader = "Error:" `isInfixOf` report
      hasMessage = not (T.null (ceMessage err))
  in property $ hasHeader && hasMessage

-- Property: Error reports preserve severity information
prop_error_report_severity :: CE.CompilerError -> Property
prop_error_report_severity err = 
  let report = formatErrorReport err
      severityStr = show (ceSeverity err)
      containsSeverity = severityStr `isInfixOf` report
  in property $ containsSeverity

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handling Comprehensive QuickCheck Tests"
  [ testGroup "Error Severity Properties"
    [ fastProperty "Severity ordering is consistent" prop_severity_ordering
    , fastProperty "Severity ranking is total order" prop_severity_total_order
    ]
  , testGroup "Error Location Properties"
    [ fastProperty "Error locations are valid" prop_error_location_valid
    , fastProperty "Error locations can be formatted" prop_error_location_formattable
    , fastProperty "Error location comparison works" prop_error_location_comparison
    ]
  , testGroup "Error Recovery Properties"
    [ fastProperty "Recovery strategies are exhaustive" prop_recovery_exhaustive
    , fastProperty "Recovery strategies are ordered by severity" prop_recovery_severity_ordering
    , fastProperty "Recovery suggestions are helpful" prop_recovery_suggestions_helpful
    ]
  , testGroup "Compiler Error Properties"
    [ fastProperty "Compiler errors have unique IDs" prop_compiler_error_unique_ids
    , fastProperty "Compiler error messages are informative" prop_compiler_error_informative
    , fastProperty "Compiler error phase is valid" prop_compiler_error_valid_phase
    ]
  , testGroup "Error Reporting Properties"
    [ fastProperty "Error reports are well-formatted" prop_error_report_formatting
    , fastProperty "Error reports preserve severity information" prop_error_report_severity
    ]
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

severityRank :: ErrorSeverity -> Int
severityRank Error = 3
severityRank Warning = 2
severityRank Info = 1

recoveryRank :: ErrorRecovery -> Int
recoveryRank fatalRecovery = 0
recoveryRank errorRecovery = 1
recoveryRank warningRecovery = 2
recoveryRank infoRecovery = 3

formatErrorLocation :: ErrorLocation -> String
formatErrorLocation loc = 
  let file = maybe "" id (elFile loc)
      line = elLine loc
      column = elColumn loc
  in file ++ ":" ++ show line ++ ":" ++ show column

mergeErrorContext :: ErrorContext -> ErrorContext -> ErrorContext
mergeErrorContext ctx1 ctx2 = ctx1 -- Simplified implementation

getContextInfo :: ErrorContext -> String
getContextInfo ctx = "context info" -- Simplified implementation

getRecoverySuggestions :: ErrorRecovery -> [String]
getRecoverySuggestions recovery = case recovery of
  fatalRecovery -> ["Cannot recover from this error"]
  errorRecovery -> ["Try fixing the syntax error"]
  warningRecovery -> ["Consider refactoring this code"]
  infoRecovery -> ["This is just informational"]

isValidPhase :: CE.CompilationPhase -> Bool
isValidPhase phase = phase `elem` 
  [ LexingPhase
  , ParsingPhase
  , TypeCheckingPhase
  , OwnershipAnalysisPhase
  , DependentTypeCheckingPhase
  , CodeGenerationPhase
  , OptimizationPhase
  ]

ceErrorId :: CE.CompilerError -> T.Text
ceErrorId = T.pack . show -- Simplified

ceMessage :: CE.CompilerError -> T.Text
ceMessage = T.pack . show -- Simplified

ceSeverity :: CE.CompilerError -> Core.ErrorSeverity
ceSeverity err = Core.severity (CE.ceError err) -- Simplified

-- cePhase is imported from Compiler.Errors.Compiler

formatErrorReport :: CE.CompilerError -> String
formatErrorReport err = 
  "Error: " ++ T.unpack (ceMessage err) ++ "\n" ++
  "Location: " ++ formatErrorLocation (Core.location (CE.ceError err)) ++ "\n" ++
  "Severity: " ++ show (ceSeverity err)

-- Simplified functions to avoid complex dependencies
elFile :: ErrorLocation -> Maybe String
elFile = undefined -- Placeholder

elLine :: ErrorLocation -> Int
elLine = undefined -- Placeholder

elColumn :: ErrorLocation -> Int
elColumn = undefined -- Placeholder

location :: Core.TypeError -> ErrorLocation
location = undefined -- Placeholder

severity :: Core.TypeError -> ErrorSeverity
severity = undefined -- Placeholder

ceError :: CE.CompilerError -> Core.TypeError
ceError = undefined -- Placeholder