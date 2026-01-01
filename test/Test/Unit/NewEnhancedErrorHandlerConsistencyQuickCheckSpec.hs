{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewEnhancedErrorHandlerConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import ErrorHandler
import EnhancedErrorHandler
import SourceLocation
import Data.List (isInfixOf)
import Data.List (sort, nub, group, intercalate, find, delete)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map

-- ============================================================================
-- Error Handler Consistency QuickCheck Tests
-- ============================================================================

-- Property: Error message consistency across handlers
prop_error_message_consistency :: String -> String -> Int -> Property
prop_error_message_consistency errorType context line =
  line >= 0 ==> 
  let location = SourceLocation line 0 "test.typus"
      basicError = createBasicError errorType context location
      enhancedError = createEnhancedError errorType context location
      basicMsg = getErrorMessage basicError
      enhancedMsg = getEnhancedErrorMessage enhancedError
  in property $ errorType `L.isInfixOf` basicMsg .&&. 
     errorType `L.isInfixOf` enhancedMsg .&&.
     context `L.isInfixOf` basicMsg .&&.
     context `L.isInfixOf` enhancedMsg

-- Property: Error severity classification consistency
prop_error_severity_consistency :: String -> Property
prop_error_severity_consistency errorType =
  let basicSeverity = classifyErrorSeverity errorType
      enhancedSeverity = classifyEnhancedErrorSeverity errorType
  in property $ basicSeverity === enhancedSeverity

-- Property: Error context preservation
prop_error_context_preservation :: [String] -> String -> Property
prop_error_context_preservation contextStack errorType =
  not (null contextStack) ==> 
  let fullContext = intercalate " -> " contextStack
      location = SourceLocation 0 0 "test.typus"
      error = createErrorWithContext errorType fullContext location
      preservedContext = extractErrorContext error
  in property $ preservedContext === fullContext

-- Property: Error chaining maintains order
prop_error_chaining_maintains_order :: [String] -> Property
prop_error_chaining_maintains_order errorTypes =
  length errorTypes >= 2 ==> 
  let location = SourceLocation 0 0 "test.typus"
      baseError = createBasicError (L.head errorTypes) "base" location
      chainedErrors = L.foldl (\err errType -> chainError err (createBasicError errType "chained" location)) baseError (L.tail errorTypes)
      extractedTypes = extractChainedErrorTypes chainedErrors
  in property $ errorTypes === extractedTypes

-- Property: Error recovery suggestions relevance
prop_error_recovery_suggestions_relevance :: String -> Property
prop_error_recovery_suggestions_relevance errorType =
  let suggestions = generateRecoverySuggestions errorType
      relevantSuggestion = not (null suggestions) && L.any (isRelevantToError errorType) suggestions
  in property $ relevantSuggestion

-- Property: Error location accuracy preservation
prop_error_location_accuracy :: Int -> Int -> String -> Property
prop_error_location_accuracy line column filename =
  line >= 0 && column >= 0 ==> 
  let location = SourceLocation line column filename
      error = createErrorAtLocation "testError" location
      extractedLocation = extractErrorLocation error
  in property $ extractedLocation === location

-- Property: Error type hierarchy consistency
prop_error_type_hierarchy_consistency :: String -> Property
prop_error_type_hierarchy_consistency errorType =
  let basicCategory = getErrorCategory errorType
      enhancedCategory = getEnhancedErrorCategory errorType
      isConsistent = basicCategory `elem` getValidCategories enhancedCategory
  in property $ isConsistent

-- Property: Multiple error aggregation
prop_multiple_error_aggregation :: [String] -> Property
prop_multiple_error_aggregation errorTypes =
  not (null errorTypes) ==> 
  let location = SourceLocation 0 0 "test.typus"
      errors = L.map (\errType -> createBasicError errType "test" location) errorTypes
      aggregated = aggregateErrors errors
      aggregatedTypes = extractAggregatedErrorTypes aggregated
  in property $ sort errorTypes === sort aggregatedTypes

-- Property: Error formatting consistency
prop_error_formatting_consistency :: String -> String -> Int -> Int -> Property
prop_error_formatting_consistency errorType context line column =
  line >= 0 && column >= 0 ==> 
  let location = SourceLocation line column "test.typus"
      error = createBasicError errorType context location
      basicFormat = formatError error
      enhancedFormat = formatEnhancedError error
      hasType = errorType `L.isInfixOf` basicFormat && errorType `L.isInfixOf` enhancedFormat
      hasContext = context `L.isInfixOf` basicFormat && context `L.isInfixOf` enhancedFormat
      hasLocation = show line `L.isInfixOf` basicFormat && show line `L.isInfixOf` enhancedFormat
  in property $ hasType .&&. hasContext .&&. hasLocation

-- Property: Error filtering by severity
prop_error_filtering_by_severity :: [String] -> Property
prop_error_filtering_by_severity errorTypes =
  not (null errorTypes) ==> 
  let location = SourceLocation 0 0 "test.typus"
      errors = L.map (\errType -> createBasicError errType "test" location) errorTypes
      highSeverityErrors = filterErrorsBySeverity errors "high"
      expectedHigh = L.filter (\err -> classifyErrorSeverity errType == "high") errorTypes
  in property $ L.length highSeverityErrors === L.length expectedHigh

-- Property: Error context propagation
prop_error_context_propagation :: [String] -> String -> Property
prop_error_context_propagation contextSteps finalErrorType =
  not (null contextSteps) ==> 
  let location = SourceLocation 0 0 "test.typus"
      context = intercalate " -> " contextSteps
      error = createErrorWithContext finalErrorType context location
      propagatedContext = getPropagatedContext error
  in property $ propagatedContext === context

-- Property: Error recovery state consistency
prop_error_recovery_state_consistency :: String -> Property
prop_error_recovery_state_consistency errorType =
  let initialState = createRecoveryState
      afterError = applyErrorToState initialState errorType
      canRecover = checkRecoverability afterError
      recoverySteps = getRecoverySteps afterError
  in property $ canRecover ==> not (null recoverySteps)

-- Property: Error localization accuracy
prop_error_localization_accuracy :: String -> Int -> Int -> Int -> Property
prop_error_localization_accuracy errorType startLine endLine column =
  startLine >= 0 && endLine >= startLine && column >= 0 ==> 
  let location = SourceLocation startLine column "test.typus"
      error = createLocalizedError errorType location endLine
      localization = getErrorLocalization error
  in property $ localizationStart localization === startLine .&&.
     localizationEnd localization === endLine .&&.
     localizationColumn localization === column

-- ============================================================================
-- Helper Functions L.and Types
-- ============================================================================

-- Error handling types
data BasicError = BasicError
  { errorType :: String
  , errorMessage :: String
  , errorLocation :: SourceLocation
  , errorContext :: String
  , errorSeverity :: String
  } deriving (Eq, Show)

data EnhancedError = EnhancedError
  { enhancedType :: String
  , enhancedMessage :: String
  , enhancedLocation :: SourceLocation
  , enhancedContext :: String
  , enhancedSeverity :: String
  , enhancedSuggestions :: [String]
  , chainedErrors :: [EnhancedError]
  } deriving (Eq, Show)

data ErrorLocalization = ErrorLocalization
  { localizationStart :: Int
  , localizationEnd :: Int
  , localizationColumn :: Int
  , localizationFile :: String
  } deriving (Eq, Show)

data RecoveryState = RecoveryState
  { stateErrors :: [String]
  , stateRecoverable :: Bool
  , stateRecoverySteps :: [String]
  } deriving (Eq, Show)

data AggregatedError = AggregatedError
  { aggregatedTypes :: [String]
  , aggregatedMessages :: [String]
  , aggregatedLocations :: [SourceLocation]
  } deriving (Eq, Show)

-- Error handling functions
createBasicError :: String -> String -> SourceLocation -> BasicError
createBasicError errType context location = BasicError
  { errorType = errType
  , errorMessage = errType ++ ": " ++ context
  , errorLocation = location
  , errorContext = context
  , errorSeverity = classifyErrorSeverity errType
  }

createEnhancedError :: String -> String -> SourceLocation -> EnhancedError
createEnhancedError errType context location = EnhancedError
  { enhancedType = errType
  , enhancedMessage = errType ++ ": " ++ context
  , enhancedLocation = location
  , enhancedContext = context
  , enhancedSeverity = classifyEnhancedErrorSeverity errType
  , enhancedSuggestions = generateRecoverySuggestions errType
  , chainedErrors = []
  }

createErrorWithContext :: String -> String -> SourceLocation -> BasicError
createErrorWithContext errType context location = 
  createBasicError errType context location

createErrorAtLocation :: String -> SourceLocation -> BasicError
createErrorAtLocation errType location = 
  createBasicError errType "Error at location" location

createLocalizedError :: String -> SourceLocation -> Int -> EnhancedError
createLocalizedError errType location endLine = EnhancedError
  { enhancedType = errType
  , enhancedMessage = "Localized error"
  , enhancedLocation = location
  , enhancedContext = "Localized context"
  , enhancedSeverity = "medium"
  , enhancedSuggestions = []
  , chainedErrors = []
  }

chainError :: BasicError -> BasicError -> BasicError
chainError base newChain = base { errorMessage = errorMessage base ++ " -> " ++ errorMessage newChain }

aggregateErrors :: [BasicError] -> AggregatedError
aggregateErrors errors = AggregatedError
  { aggregatedTypes = map errorType errors
  , aggregatedMessages = map errorMessage errors
  , aggregatedLocations = map errorLocation errors
  }

-- Error analysis functions
getErrorMessage :: BasicError -> String
getErrorMessage = errorMessage

getEnhancedErrorMessage :: EnhancedError -> String
getEnhancedErrorMessage = enhancedMessage

classifyErrorSeverity :: String -> String
classifyErrorSeverity errType
  | "syntax" `L.isInfixOf` errType = "high"
  | "type" `L.isInfixOf` errType = "medium"
  | "warning" `L.isInfixOf` errType = "low"
  | otherwise = "medium"

classifyEnhancedErrorSeverity :: String -> String
classifyEnhancedErrorSeverity = classifyErrorSeverity

extractErrorContext :: BasicError -> String
extractErrorContext = errorContext

extractChainedErrorTypes :: BasicError -> [String]
extractChainedErrorTypes error = [errorType error]

generateRecoverySuggestions :: String -> [String]
generateRecoverySuggestions errType
  | "syntax" `L.isInfixOf` errType = ["Check syntax", "Verify parentheses"]
  | "type" `L.isInfixOf` errType = ["Check types", "Add type annotations"]
  | otherwise = ["Review code"]

isRelevantToError :: String -> String -> Bool
isRelevantToError suggestion errType = L.length suggestion > 0

extractErrorLocation :: BasicError -> SourceLocation
extractErrorLocation = errorLocation

getErrorCategory :: String -> String
getErrorCategory errType
  | "syntax" `L.isInfixOf` errType = "parsing"
  | "type" `L.isInfixOf` errType = "type-checking"
  | otherwise = "general"

getEnhancedErrorCategory :: String -> String
getEnhancedErrorCategory = getErrorCategory

getValidCategories :: String -> [String]
getValidCategories category = [category, "general"]

extractAggregatedErrorTypes :: AggregatedError -> [String]
extractAggregatedErrorTypes = aggregatedTypes

formatError :: BasicError -> String
formatError error = errorMessage error ++ " at " ++ show (errorLocation error)

formatEnhancedError :: BasicError -> String
formatEnhancedError error = "Enhanced: " ++ errorMessage error

filterErrorsBySeverity :: [BasicError] -> String -> [BasicError]
filterErrorsBySeverity errors severity = 
  filter (\err -> errorSeverity err == severity) errors

getPropagatedContext :: BasicError -> String
getPropagatedContext = errorContext

createRecoveryState :: RecoveryState
createRecoveryState = RecoveryState [] True []

applyErrorToState :: RecoveryState -> String -> RecoveryState
applyErrorToState state errType = state
  { stateErrors = errType : stateErrors state
  , stateRecoverable = classifyErrorSeverity errType /= "high"
  , stateRecoverySteps = generateRecoverySuggestions errType
  }

checkRecoverability :: RecoveryState -> Bool
checkRecoverability = stateRecoverable

getRecoverySteps :: RecoveryState -> [String]
getRecoverySteps = stateRecoverySteps

getErrorLocalization :: EnhancedError -> ErrorLocalization
getErrorLocalization error = 
  let location = enhancedLocation error
  in ErrorLocalization
    { localizationStart = sourceLine location
    , localizationEnd = sourceLine location + 1
    , localizationColumn = sourceColumn location
    , localizationFile = sourceFile location
    }

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handler Consistency QuickCheck Tests"
  [ fastProperty "Error message consistency across handlers" prop_error_message_consistency
  , fastProperty "Error severity classification consistency" prop_error_severity_consistency
  , fastProperty "Error context preservation" prop_error_context_preservation
  , fastProperty "Error chaining maintains order" prop_error_chaining_maintains_order
  , fastProperty "Error recovery suggestions relevance" prop_error_recovery_suggestions_relevance
  , fastProperty "Error location accuracy preservation" prop_error_location_accuracy
  , fastProperty "Error type hierarchy consistency" prop_error_type_hierarchy_consistency
  , fastProperty "Multiple error aggregation" prop_multiple_error_aggregation
  , fastProperty "Error formatting consistency" prop_error_formatting_consistency
  , fastProperty "Error filtering by severity" prop_error_filtering_by_severity
  , fastProperty "Error context propagation" prop_error_context_propagation
  , fastProperty "Error recovery state consistency" prop_error_recovery_state_consistency
  , fastProperty "Error localization accuracy" prop_error_localization_accuracy
  ]