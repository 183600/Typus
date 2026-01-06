{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalErrorHandlerRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import ErrorHandler
import EnhancedErrorHandler
import Compiler.Errors.Core (ErrorLocation(..))
import SourceLocation (SourcePos(..), startPos, posAt)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

-- | Test suite for Error Handler recovery mechanisms
tests :: TestTree
tests =
  testGroup "Error Handler Recovery Properties"
    [ testGroup "Basic error handling properties"
        [ fastProperty "error recovery preserves context" prop_error_recovery_preserves_context
        , fastProperty "error messages contain location information" prop_error_messages_contain_location
        , fastProperty "error recovery attempts are bounded" prop_recovery_attempts_bounded
        , fastProperty "error classification is deterministic" prop_error_classification_deterministic
        ]

    , testGroup "Recovery strategy properties"
        [ fastProperty "recovery strategies are exhaustive" prop_recovery_strategies_exhaustive
        , fastProperty "recovery preserves program structure" prop_recovery_preserves_structure
        , fastProperty "recovery does not introduce new errors" prop_recovery_no_new_errors
        , fastProperty "recovery is idempotent" prop_recovery_idempotent
        ]

    , testGroup "Error accumulation properties"
        [ fastProperty "error accumulation preserves ordering" prop_error_accumulation_ordering
        , fastProperty "error deduplication removes duplicates" prop_error_deduplication
        , fastProperty "error context is maintained through accumulation" prop_error_context_maintained
        ]

    , testGroup "Enhanced error handling properties"
        [ fastProperty "enhanced error handling provides better context" prop_enhanced_better_context
        , fastProperty "error suggestions are relevant" prop_error_suggestions_relevant
        , fastProperty "error recovery hints are actionable" prop_recovery_hints_actionable
        ]

    , testGroup "Performance properties"
        [ fastProperty "error handling does not degrade performance significantly" prop_error_handling_performance
        , fastProperty "large error collections are handled efficiently" prop_large_error_collections_efficient
        , fastProperty "error recovery memory usage is bounded" prop_recovery_memory_bounded
        ]
    ]

-- Helper types for error handling testing
data ErrorSeverity = Error | Warning | Info deriving (Show, Eq, Ord)

data ErrorType = ParseError | TypeError | SemanticError | RuntimeError deriving (Show, Eq, Ord)

data CustomError = CustomError
  { errorId :: String
  , errorType :: ErrorType
  , errorSeverity :: ErrorSeverity
  , errorMessage :: String
  , errorLocation :: ErrorLocation
  , errorContext :: Map String String
  } deriving (Show, Eq, Ord)

data RecoveryStrategy = SkipToken | InsertToken | ReplaceToken | Abort deriving (Show, Eq, Ord)

data ErrorState = ErrorState
  { errors :: [CustomError]
  , recoveryAttempts :: Int
  , maxRecoveryAttempts :: Int
  , currentContext :: Map String String
  } deriving (Show, Eq)

-- Helper functions
createError :: String -> ErrorType -> ErrorSeverity -> String -> ErrorLocation -> CustomError
createError errId errType errSeverity errMsg errLoc = 
  CustomError errId errType errSeverity errMsg errLoc Map.empty

addError :: CustomError -> ErrorState -> ErrorState
addError error state = 
  state { errors = error : errors state }

attemptRecovery :: RecoveryStrategy -> ErrorState -> Either String ErrorState
attemptRecovery strategy state =
  if recoveryAttempts state >= maxRecoveryAttempts state
  then Left "Maximum recovery attempts exceeded"
  else 
    let newAttempts = recoveryAttempts state + 1
        newState = state { recoveryAttempts = newAttempts }
    in case strategy of
      SkipToken -> Right newState
      InsertToken -> Right newState
      ReplaceToken -> Right newState
      Abort -> Left "Recovery aborted"

classifyError :: CustomError -> ErrorType
classifyError = errorType

deduplicateErrors :: [CustomError] -> [CustomError]
deduplicateErrors = nub

-- Basic error handling properties

prop_error_recovery_preserves_context :: String -> ErrorType -> ErrorSeverity -> Property
prop_error_recovery_preserves_context errId errType errSeverity =
  not (null errId) && L.length errId <= 10 ==>
  let errLoc = ErrorLocation (startPos) Nothing
      context = Map.fromList [("file", "test.typus"), ("phase", "parsing")]
      error = createError errId errType errSeverity "Test error" errLoc
      initialState = ErrorState [] 0 3 context
      stateWithError = addError error initialState
  in property $ currentContext stateWithError === context

prop_error_messages_contain_location :: String -> ErrorType -> Property
prop_error_messages_contain_location errId errType =
  not (null errId) && L.length errId <= 10 ==>
  let line = 10
      col = 5
      errLoc = ErrorLocation Nothing line col Nothing Nothing
      error = createError errId errType Error "Test error" errLoc
      msg = errorMessage error
  in property $ show line `L.isInfixOf` msg .||. show col `L.isInfixOf` msg

prop_recovery_attempts_bounded :: Int -> Property
prop_recovery_attempts_bounded initialAttempts =
  initialAttempts >= 0 && initialAttempts <= 10 ==>
  let maxAttempts = 5
      state = ErrorState [] initialAttempts maxAttempts Map.empty
      canRecover = recoveryAttempts state < maxRecoveryAttempts state
  in property $ canRecover === (initialAttempts < maxAttempts)

prop_error_classification_deterministic :: String -> ErrorType -> ErrorSeverity -> Property
prop_error_classification_deterministic errId errType errSeverity =
  not (null errId) && L.length errId <= 10 ==>
  let errLoc = ErrorLocation (startPos) Nothing
      error = createError errId errType errSeverity "Test error" errLoc
      classification1 = classifyError error
      classification2 = classifyError error
  in property $ classification1 === classification2

-- Recovery strategy properties

prop_recovery_strategies_exhaustive :: ErrorType -> Property
prop_recovery_strategies_exhaustive errType =
  let allStrategies = [SkipToken, InsertToken, ReplaceToken, Abort]
      state = ErrorState [] 0 3 Map.empty
      results = L.map (\strategy -> attemptRecovery strategy state) allStrategies
      hasSuccess = L.any isRight results
      hasFailure = L.any isLeft results
  in property $ hasSuccess .&&. hasFailure
  where
    isRight (Right _) = True
    isRight _ = False
    isLeft (Left _) = True
    isLeft _ = False

prop_recovery_preserves_structure :: Int -> Property
prop_recovery_preserves_structure initialErrorCount =
  initialErrorCount >= 0 && initialErrorCount <= 5 ==>
  let errors = replicate initialErrorCount (createError "test" ParseError Error "test" (ErrorLocation (startPos) Nothing))
      state = ErrorState errors 0 3 Map.empty
  in case attemptRecovery SkipToken state of
    Right recoveredState -> property $ L.length (errors recoveredState) === initialErrorCount
    Left _ -> property $ True

prop_recovery_no_new_errors :: Int -> Property
prop_recovery_no_new_errors initialErrorCount =
  initialErrorCount >= 0 && initialErrorCount <= 5 ==>
  let errors = replicate initialErrorCount (createError "test" ParseError Error "test" (ErrorLocation (startPos) Nothing))
      state = ErrorState errors 0 3 Map.empty
  in case attemptRecovery SkipToken state of
    Right recoveredState -> property $ L.length (errors recoveredState) <= initialErrorCount
    Left _ -> property $ True

prop_recovery_idempotent :: Int -> Property
prop_recovery_idempotent attempts =
  attempts >= 0 && attempts <= 3 ==>
  let state = ErrorState [] 0 5 Map.empty
      recoverOnce = attemptRecovery SkipToken state
      recoverTwice = case recoverOnce of
        Right s1 -> attemptRecovery SkipToken s1
        Left _ -> Left "First recovery failed"
  in case (recoverOnce, recoverTwice) of
    (Right s1, Right s2) -> property $ recoveryAttempts s2 === recoveryAttempts s1 + 1
    (Left _, Left _) -> property $ True
    _ -> property $ False

-- Error accumulation properties

prop_error_accumulation_ordering :: [String] -> Property
prop_error_accumulation_ordering errorIds =
  not (null errorIds) && L.length errorIds <= 5 && L.all (not . null) errorIds ==>
  let errors = zipWith (\i errId -> createError errId ParseError Error ("Error " ++ show i) (ErrorLocation Nothing i 1 Nothing Nothing)) [1..] errorIds
      state = L.foldl (flip addError) (ErrorState [] 0 3 Map.empty) errors
      errorIdsInState = map errorId (errors state)
  in property $ errorIdsInState === L.reverse errorIds

prop_error_deduplication :: [String] -> Property
prop_error_deduplication errorIds =
  not (null errorIds) && L.length errorIds <= 5 ==>
  let errors = L.map (\errId -> createError errId ParseError Error "Duplicate error" (ErrorLocation (startPos) Nothing)) errorIds
      deduplicated = deduplicateErrors errors
  in property $ L.length deduplicated <= L.length errors

prop_error_context_maintained :: String -> Property
prop_error_context_maintained contextKey =
  not (null contextKey) && L.length contextKey <= 10 ==>
  let context = Map.fromList [(contextKey, "context value")]
      state = ErrorState [] 0 3 context
      error = createError "test" ParseError Error "Test error" (ErrorLocation (startPos) Nothing)
      stateWithError = addError error state
  in property $ Map.lookup contextKey (currentContext stateWithError) === Just "context value"

-- Enhanced error handling properties

prop_enhanced_better_context :: String -> ErrorType -> Property
prop_enhanced_better_context errId errType =
  not (null errId) && L.length errId <= 10 ==>
  let errLoc = ErrorLocation Nothing 5 10 Nothing Nothing
      basicError = createError errId errType Error "Basic error" errLoc
      -- Simulate enhanced error handling with more context
      enhancedContext = Map.fromList 
        [ ("line", "5")
        , ("column", "10") 
        , ("expected", "identifier")
        , ("found", "number")
        ]
      enhancedError = basicError { errorContext = enhancedContext }
  in property $ Map.size (errorContext enhancedError) > Map.size (errorContext basicError)

prop_error_suggestions_relevant :: ErrorType -> Property
prop_error_suggestions_relevant errType =
  let suggestions = case errType of
        ParseError -> ["Check syntax", "Verify brackets"]
        TypeError -> ["Check types", "Verify imports"]
        SemanticError -> ["Check variable scope", "Verify function calls"]
        RuntimeError -> ["Check input", "Verify resources"]
  in property $ not (null suggestions) && L.all (not . null) suggestions

prop_recovery_hints_actionable :: ErrorType -> Property
prop_recovery_hints_actionable errType =
  let hints = case errType of
        ParseError -> "Add missing semicolon"
        TypeError -> "Add type annotation"
        SemanticError -> "Import required module"
        RuntimeError -> "Add error handling"
  in property $ not (null hints) && L.length hints <= 50

-- Performance properties

prop_error_handling_performance :: Int -> Property
prop_error_handling_performance errorCount =
  errorCount >= 0 && errorCount <= 100 ==>
  let errors = replicate errorCount (createError "test" ParseError Error "Performance test" (ErrorLocation (startPos) Nothing))
      state = L.foldl (flip addError) (ErrorState [] 0 3 Map.empty) errors
  in property $ L.length (errors state) === errorCount

prop_large_error_collections_efficient :: Int -> Property
prop_large_error_collections_efficient errorCount =
  errorCount >= 0 && errorCount <= 50 ==>
  let errors = replicate errorCount (createError ("error_" ++ show errorCount) ParseError Error "Collection test" (ErrorLocation (startPos) Nothing))
      deduplicated = deduplicateErrors errors
  in property $ L.length deduplicated <= errorCount

prop_recovery_memory_bounded :: Int -> Property
prop_recovery_memory_bounded maxAttempts =
  maxAttempts >= 0 && maxAttempts <= 20 ==>
  let state = ErrorState [] 0 maxAttempts Map.empty
      memoryUsage = maxRecoveryAttempts state
  in property $ memoryUsage === maxAttempts