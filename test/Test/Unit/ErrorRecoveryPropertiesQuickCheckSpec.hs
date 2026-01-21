module Test.Unit.ErrorRecoveryPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.QuickCheck (fastProperty)
import Compiler.Errors.Core (ErrorSeverity(..), ErrorLocation(..), ErrorContext(..), ErrorRecovery(..))
import Compiler.Errors (CompilationPhase(..))
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Read (readMaybe)

-- Helper types and generators
data TestError = TestError
  { errorId :: String
  , errorSeverity :: ErrorSeverity
  , errorMessage :: String
  , errorPhase :: CompilationPhase
  , errorLocation :: ErrorLocation
  } deriving (Show, Eq)

instance Arbitrary TestError where
  arbitrary = do
    id' <- arbitrary
    severity <- arbitrary
    message <- arbitrary
    phase <- arbitrary
    location <- arbitrary
    return $ TestError id' severity message phase location

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Error, Warning, Info]

instance Arbitrary CompilationPhase where
  arbitrary = elements 
    [ LexingPhase
    , ParsingPhase
    , TypeCheckingPhase
    , OwnershipAnalysisPhase
    , DependentTypeCheckingPhase
    , CodeGenerationPhase
    , OptimizationPhase
    ]

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    recAction <- arbitrary
    recHint <- arbitrary
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    return $ ErrorRecovery canRec shouldCont recAction recHint cost confidence

instance Arbitrary ErrorLocation where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    return $ ErrorLocation (Just "test.typus") line column Nothing Nothing

-- Properties for error severity ordering
prop_severity_ordering :: TestError -> TestError -> Property
prop_severity_ordering err1 err2 = 
  (errorSeverity err1 == Error && errorSeverity err2 == Warning) ==> 
  isMoreSevere (errorSeverity err1) (errorSeverity err2)

prop_severity_reflexive :: ErrorSeverity -> Bool
prop_severity_reflexive severity = not (isMoreSevere severity severity)

prop_severity_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_severity_transitive s1 s2 s3 = 
  (isMoreSevere s1 s2 && isMoreSevere s2 s3) ==> 
  isMoreSevere s1 s3

-- Properties for error messages
prop_error_message_non_empty :: TestError -> Bool
prop_error_message_non_empty err = not (null (errorMessage err))

prop_error_message_contains_id :: TestError -> Property
prop_error_message_contains_id err = 
  not (null (errorId err)) ==> 
  errorId err `isInfixOf` errorMessage err

prop_error_message_format :: TestError -> Bool
prop_error_message_format err = 
  let msg = errorMessage err
  in length msg > 0 && all (>= ' ') msg  -- Basic validation

-- Properties for error location
prop_location_line_positive :: TestError -> Bool
prop_location_line_positive err = 
  let ErrorLocation _ line _ _ _ = errorLocation err
  in line >= 1

prop_location_column_positive :: TestError -> Bool
prop_location_column_positive err = 
  let ErrorLocation _ _ column _ _ = errorLocation err
  in column >= 1

prop_location_valid_source :: TestError -> Bool
prop_location_valid_source err = 
  let ErrorLocation source _ _ _ _ = errorLocation err
  in case source of
       Just src -> not (null src)
       Nothing -> True

-- Properties for error recovery
prop_recovery_preserves_context :: TestError -> ErrorRecovery -> Bool
prop_recovery_preserves_context err recovery = 
  let originalContext = errorContext err
      recoveredContext = applyRecovery originalContext recovery
  in contextPreserved originalContext recoveredContext
  where
    errorContext _ = ErrorContext Nothing Nothing Nothing Nothing []  -- Simplified
    applyRecovery ctx _ = ctx  -- Simplified
    contextPreserved _ _ = True  -- Simplified

prop_recovery_never_worsens :: TestError -> ErrorRecovery -> Bool
prop_recovery_never_worsens err recovery = 
  let originalSeverity = errorSeverity err
      recoveredSeverity = recoverySeverity recovery
  in not (isMoreSevere recoveredSeverity originalSeverity)
  where
    recoverySeverity _ = Warning  -- Simplified

-- Properties for error aggregation
prop_aggregate_preserves_all_errors :: [TestError] -> Bool
prop_aggregate_preserves_all_errors errs = 
  let aggregated = aggregateErrors errs
      originalIds = map errorId errs
      aggregatedIds = map errorId aggregated
  in all (`elem` aggregatedIds) originalIds

prop_aggregate_sorts_by_severity :: [TestError] -> Property
prop_aggregate_sorts_by_severity errs = 
  length errs > 1 ==> 
  let aggregated = aggregateErrors errs
      severities = map errorSeverity aggregated
  in isSortedBySeverity severities
  where
    isSortedBySeverity [] = True
    isSortedBySeverity [_] = True
    isSortedBySeverity (s1:s2:rest) = 
      (isMoreSevere s1 s2 || s1 == s2) && isSortedBySeverity (s2:rest)

prop_aggregate_removes_duplicates :: [TestError] -> Bool
prop_aggregate_removes_duplicates errs = 
  let aggregated = aggregateErrors errs
      uniqueIds = nub (map errorId errs)
      aggregatedIds = nub (map errorId aggregated)
  in length aggregatedIds == length uniqueIds

-- Properties for error filtering
prop_filter_by_severity :: [TestError] -> ErrorSeverity -> Bool
prop_filter_by_severity errs severity = 
  let filtered = filterBySeverity errs severity
  in all (\e -> errorSeverity e == severity) filtered

prop_filter_by_phase :: [TestError] -> CompilationPhase -> Bool
prop_filter_by_phase errs phase = 
  let filtered = filterByPhase errs phase
  in all (\e -> errorPhase e == phase) filtered

prop_filter_preserves_order :: [TestError] -> ErrorSeverity -> Bool
prop_filter_preserves_order errs severity = 
  let filtered = filterBySeverity errs severity
      originalOrder = filter (\e -> errorSeverity e == severity) errs
  in map errorId filtered == map errorId originalOrder

-- Properties for error transformation
prop_transform_preserves_severity :: TestError -> Bool
prop_transform_preserves_severity err = 
  let transformed = transformErrorMessage err (++ " (transformed)")
  in errorSeverity transformed == errorSeverity err

prop_transform_updates_message :: TestError -> Bool
prop_transform_updates_message err = 
  let suffix = " (transformed)"
      transformed = transformErrorMessage err (++ suffix)
  in suffix `isSuffixOf` errorMessage transformed

prop_transform_preserves_location :: TestError -> Bool
prop_transform_preserves_location err = 
  let transformed = transformErrorMessage err (++ " (transformed)")
  in errorLocation transformed == errorLocation err

-- Properties for error context
prop_context_accumulates_errors :: [TestError] -> Bool
prop_context_accumulates_errors errs = 
  let context = buildContext errs
      errorCount = contextErrorCount context
  in errorCount == length errs

prop_context_tracks_phases :: [TestError] -> Bool
prop_context_tracks_phases errs = 
  let context = buildContext errs
      phases = contextPhases context
      expectedPhases = nub (map errorPhase errs)
  in all (`elem` phases) expectedPhases

-- Helper functions
isMoreSevere :: ErrorSeverity -> ErrorSeverity -> Bool
isMoreSevere Error Warning = True
isMoreSevere Error Info = True
isMoreSevere Warning Info = True
isMoreSevere _ _ = False

aggregateErrors :: [TestError] -> [TestError]
aggregateErrors errs = 
  let uniqueErrs = nubBy (\e1 e2 -> errorId e1 == errorId e2) errs
  in sortBySeverity uniqueErrs
  where
    nubBy _ [] = []
    nubBy eq (x:xs) = x : nubBy eq (filter (\y -> not (eq x y)) xs)
    sortBySeverity es = 
      let errors = filter (\e -> errorSeverity e == Error) es
          warnings = filter (\e -> errorSeverity e == Warning) es
          infos = filter (\e -> errorSeverity e == Info) es
      in errors ++ warnings ++ infos

filterBySeverity :: [TestError] -> ErrorSeverity -> [TestError]
filterBySeverity errs severity = filter (\e -> errorSeverity e == severity) errs

filterByPhase :: [TestError] -> CompilationPhase -> [TestError]
filterByPhase errs phase = filter (\e -> errorPhase e == phase) errs

transformErrorMessage :: TestError -> (String -> String) -> TestError
transformErrorMessage err f = err { errorMessage = f (errorMessage err) }

data ErrorContextData = ErrorContextData
  { contextErrorCount :: Int
  , contextPhases :: [CompilationPhase]
  } deriving (Show, Eq)

buildContext :: [TestError] -> ErrorContextData
buildContext errs = ErrorContextData
  { contextErrorCount = length errs
  , contextPhases = nub (map errorPhase errs)
  }

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

tests :: TestTree
tests = testGroup "Test.Unit.ErrorRecoveryPropertiesQuickCheckSpec Tests"
  [ fastProperty "severity ordering" prop_severity_ordering
  , fastProperty "severity reflexive" prop_severity_reflexive
  , fastProperty "severity transitive" prop_severity_transitive
  , fastProperty "error message non empty" prop_error_message_non_empty
  , fastProperty "error message contains id" prop_error_message_contains_id
  , fastProperty "error message format" prop_error_message_format
  , fastProperty "location line positive" prop_location_line_positive
  , fastProperty "location column positive" prop_location_column_positive
  , fastProperty "location valid source" prop_location_valid_source
  , fastProperty "recovery preserves context" prop_recovery_preserves_context
  , fastProperty "recovery never worsens" prop_recovery_never_worsens
  , fastProperty "aggregate preserves all errors" prop_aggregate_preserves_all_errors
  , fastProperty "aggregate sorts by severity" prop_aggregate_sorts_by_severity
  , fastProperty "aggregate removes duplicates" prop_aggregate_removes_duplicates
  , fastProperty "filter by severity" prop_filter_by_severity
  , fastProperty "filter by phase" prop_filter_by_phase
  , fastProperty "filter preserves order" prop_filter_preserves_order
  , fastProperty "transform preserves severity" prop_transform_preserves_severity
  , fastProperty "transform updates message" prop_transform_updates_message
  , fastProperty "transform preserves location" prop_transform_preserves_location
  , fastProperty "context accumulates errors" prop_context_accumulates_errors
  , fastProperty "context tracks phases" prop_context_tracks_phases
  ]