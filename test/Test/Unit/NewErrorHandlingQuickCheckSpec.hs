{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.NewErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , CombinedError(..)
  , emptyContext
  , errorAt
  , errorAtWithTimestamp
  , warningAt
  , infoAt
  , errorWithCategory
  , warningWithCategory
  , infoWithCategory
  , fatalError
  , fatalErrorWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , withTimestamp
  , wrapError
  , combineErrors
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , formatError
  , formatErrorWithLocation
  , formatErrors
  , canRecoverFrom
  , shouldContinueAfter
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , createRecoveryStrategy
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  , getErrorLine
  , getErrorColumn
  , _atLocation
  , _atFileLocation
  , _atRange
  )

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (sort, isInfixOf)
import Data.Time (UTCTime, getCurrentTime)
import Data.Char (isSpace)

-- | 新的错误处理QuickCheck测试套件
tests :: TestTree
tests =
  testGroup "New Error Handling QuickCheck Tests"
    [ fastProperty "errorAt creates error with correct properties" prop_errorAt_properties
    , fastProperty "filterBySeverity correctly filters errors" prop_filterBySeverity_correct
    , fastProperty "filterByCategory correctly filters errors" prop_filterByCategory_correct
    , fastProperty "hasCategory correctly identifies category" prop_hasCategory_correct
    , fastProperty "wrapError preserves inner error in chain" prop_wrapError_preserves_chain
    , fastProperty "combineErrors expands related errors" prop_combineErrors_expands
    , fastProperty "withLocation updates error location" prop_withLocation_updates
    , fastProperty "withContext updates error context" prop_withContext_updates
    , fastProperty "formatError includes severity and message" prop_formatError_includes_severity
    , fastProperty "getErrorStatistics counts errors correctly" prop_getErrorStatistics_counts
    ]

-- Property: errorAt creates error with correct properties
prop_errorAt_properties :: String -> String -> Int -> Int -> Property
prop_errorAt_properties errId msg line col =
  not (null errId) && not (null msg) &&
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let location = _atLocation line col
      error = errorAt errId (T.pack msg) location
  in property $ errorId error === errId .&&.
     message error === T.pack msg .&&.
     location error === location .&&.
     severity error === Error .&&.
     category error === Unknown .&&.
     context error === emptyContext

-- Property: filterBySeverity correctly filters errors
prop_filterBySeverity_correct :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filterBySeverity_correct severities targetSeverity =
  let errors = zipWith (\i sev -> errorAt ("err" ++ show i) (T.pack $ "message " ++ show i) (_atLocation i 1)) 
                       [1..length severities] severities
      filtered = filterBySeverity targetSeverity errors
      expected = filter (\e -> severity e == targetSeverity) errors
  in property $ length filtered === length expected .&&.
     all (\e -> severity e == targetSeverity) filtered

-- Property: filterByCategory correctly filters errors
prop_filterByCategory_correct :: [ErrorCategory] -> ErrorCategory -> Property
prop_filterByCategory_correct categories targetCategory =
  let errors = zipWith (\i cat -> errorWithCategory ("err" ++ show i) cat (T.pack $ "message " ++ show i) (_atLocation i 1))
                       [1..length categories] categories
      filtered = filterByCategory targetCategory errors
      expected = filter (\e -> category e == targetCategory) errors
  in property $ length filtered === length expected .&&.
     all (\e -> category e == targetCategory) filtered

-- Property: hasCategory correctly identifies category
prop_hasCategory_correct :: ErrorCategory -> ErrorCategory -> Property
prop_hasCategory_correct testCategory targetCategory =
  let error = errorWithCategory "test" testCategory (T.pack "test message") (_atLocation 1 1)
      result = hasCategory targetCategory error
  in property $ result === (testCategory == targetCategory)

-- Property: wrapError preserves inner error in chain
prop_wrapError_preserves_chain :: String -> String -> Property
prop_wrapError_preserves_chain wrapperMsg innerMsg =
  not (null wrapperMsg) && not (null innerMsg) ==>
  let innerError = errorAt "inner" (T.pack innerMsg) (_atLocation 1 1)
      wrappedError = wrapError (T.pack wrapperMsg) innerError
  in property $ message wrappedError === T.pack wrapperMsg <> ": " <> T.pack innerMsg .&&.
     errorChain wrappedError === [innerError]

-- Property: combineErrors expands related errors
prop_combineErrors_expands :: [String] -> Property
prop_combineErrors_expands messages =
  not (null messages) && length messages <= 10 ==>
  let errors = zipWith (\i msg -> errorAt ("err" ++ show i) (T.pack msg) (_atLocation i 1))
                       [1..length messages] messages
      -- Add some related errors to the first error
      errorsWithRelated = case errors of
        (first:rest) -> [first { relatedErrors = take 2 rest }] ++ drop 2 rest
        [] -> []
      combined = combineErrors errorsWithRelated
  in property $ length combined >= length errorsWithRelated .&&.
     all (\e -> any (\r -> errorId e == errorId r) (relatedErrors e)) combined

-- Property: withLocation updates error location
prop_withLocation_updates :: String -> Int -> Int -> Int -> Int -> Property
prop_withLocation_updates errId line1 col1 line2 col2 =
  not (null errId) &&
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 &&
  line1 <= 1000 && col1 <= 1000 && line2 <= 1000 && col2 <= 1000 ==>
  let originalLoc = _atLocation line1 col1
      newLoc = _atLocation line2 col2
      error = errorAt errId (T.pack "test") originalLoc
      updatedError = withLocation error newLoc
  in property $ location updatedError === newLoc .&&.
     errorId updatedError === errId .&&.
     message updatedError === T.pack "test"

-- Property: withContext updates error context
prop_withContext_updates :: String -> String -> String -> String -> Property
prop_withContext_updates errId funcName varName typeName =
  not (null errId) && not (null funcName) && not (null varName) && not (null typeName) ==>
  let error = errorAt errId (T.pack "test") (_atLocation 1 1)
      newContext = emptyContext 
        { contextFunction = Just funcName
        , contextVariable = Just varName
        , contextType = Just typeName
        }
      updatedError = withContext error newContext
  in property $ context updatedError === newContext .&&.
     contextFunction (context updatedError) === Just funcName .&&.
     contextVariable (context updatedError) === Just varName .&&.
     contextType (context updatedError) === Just typeName

-- Property: formatError includes severity and message
prop_formatError_includes_severity :: String -> String -> ErrorSeverity -> Property
prop_formatError_includes_severity errId msg sev =
  not (null errId) && not (null msg) ==>
  let error = (errorAt errId (T.pack msg) (_atLocation 1 1)) { severity = sev }
      formatted = formatError error
      severityStr = case sev of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
  in property $ severityStr `isInfixOf` formatted .&&.
     msg `isInfixOf` formatted

-- Property: getErrorStatistics counts errors correctly
prop_getErrorStatistics_counts :: [ErrorSeverity] -> [ErrorCategory] -> Property
prop_getErrorStatistics_counts severities categories =
  length severities == length categories && length severities <= 20 ==>
  let errors = zipWith3 (\i sev cat -> (errorWithCategory ("err" ++ show i) cat (T.pack $ "message " ++ show i) (_atLocation i 1)) { severity = sev })
                        [1..length severities] severities categories
      stats = getErrorStatistics errors
      expectedTotal = length errors
      expectedFatal = length $ filter (\e -> severity e == Fatal) errors
      expectedErrors = length $ filter (\e -> severity e == Error) errors
      expectedWarnings = length $ filter (\e -> severity e == Warning) errors
      expectedInfo = length $ filter (\e -> severity e == Info) errors
  in property $ Map.lookup "total" stats === Just expectedTotal .&&.
     Map.lookup "fatal" stats === Just expectedFatal .&&.
     Map.lookup "errors" stats === Just expectedErrors .&&.
     Map.lookup "warnings" stats === Just expectedWarnings .&&.
     Map.lookup "info" stats === Just expectedInfo

-- Additional properties for error handling

-- Property: withSuggestions adds suggestions to error
prop_withSuggestions_adds :: String -> [String] -> Property
prop_withSuggestions_adds errId suggestions =
  not (null errId) && length suggestions <= 5 ==>
  let error = errorAt errId (T.pack "test") (_atLocation 1 1)
      suggestionsText = map T.pack suggestions
      updatedError = withSuggestions suggestionsText error
  in property $ take (length suggestionsText) (suggestions updatedError) === suggestionsText

-- Property: withTimestamp adds timestamp to error
prop_withTimestamp_adds :: String -> String -> Property
prop_withTimestamp_adds errId timestamp =
  not (null errId) && not (null timestamp) ==>
  let error = errorAt errId (T.pack "test") (_atLocation 1 1)
      updatedError = withTimestamp timestamp error
  in property $ timestamp updatedError === Just timestamp

-- Property: canRecoverFrom and shouldContinueAfter work correctly
prop_canRecover_shouldContinue :: ErrorSeverity -> Property
prop_canRecover_shouldContinue sev =
  let error = (errorAt "test" (T.pack "test") (_atLocation 1 1)) { severity = sev }
      expectedRecovery = case sev of
        Fatal -> fatalRecovery
        Error -> errorRecovery
        Warning -> warningRecovery
        Info -> infoRecovery
  in property $ canRecoverFrom error === canRecover expectedRecovery .&&.
     shouldContinueAfter error === shouldContinue expectedRecovery

-- Property: combinedErrorSeverity extracts severity correctly
prop_combinedErrorSeverity :: ErrorSeverity -> Int -> Property
prop_combinedErrorSeverity sev errorCode =
  let combinedError = case errorCode `mod` 3 of
        0 -> OwnershipErrorCombined sev undefined
        1 -> DependentTypeErrorCombined sev undefined
        _ -> IntegrationError "test" sev
  in property $ combinedErrorSeverity combinedError === sev

-- Property: filterCombinedErrorsBySeverity works correctly
prop_filterCombinedErrorsBySeverity :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filterCombinedErrorsBySeverity severities minSeverity =
  let combinedErrors = zipWith (\sev i -> case i `mod` 3 of
                                   0 -> OwnershipErrorCombined sev undefined
                                   1 -> DependentTypeErrorCombined sev undefined
                                   _ -> IntegrationError ("test" ++ show i) sev)
                                severities [0..]
      filtered = filterCombinedErrorsBySeverity minSeverity combinedErrors
      expected = filter (\err -> isAtLeast minSeverity (combinedErrorSeverity err)) combinedErrors
  in property $ length filtered === length expected

-- Property: createRecoveryStrategy creates strategy with correct properties
prop_createRecoveryStrategy :: Bool -> Bool -> Maybe String -> Maybe String -> Property
prop_createRecoveryStrategy canRec shouldCont action hint =
  let strategy = createRecoveryStrategy canRec shouldCont action hint
  in property $ canRecover strategy === canRec .&&.
     shouldContinue strategy === shouldCont .&&.
     recoveryAction strategy === action .&&.
     recoveryHint strategy === hint

-- Property: customRecovery creates strategy with correct properties
prop_customRecovery :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_customRecovery canRec shouldCont action hint cost confidence =
  let strategy = customRecovery canRec shouldCont action hint cost confidence
  in property $ canRecover strategy === canRec .&&.
     shouldContinue strategy === shouldCont .&&.
     recoveryAction strategy === action .&&.
     recoveryHint strategy === hint .&&.
     recoveryCost strategy === cost .&&.
     recoveryConfidence strategy === confidence

-- Property: _atLocation creates location with correct properties
prop_atLocation :: Int -> Int -> Property
prop_atLocation line col =
  line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let location = _atLocation line col
  in property $ getErrorLine location === line .&&.
     getErrorColumn location === col .&&.
     filePath location === Nothing .&&.
     endLine location === Nothing .&&.
     endColumn location === Nothing

-- Property: _atFileLocation creates location with file path
prop_atFileLocation :: String -> Int -> Int -> Property
prop_atFileLocation file line col =
  not (null file) && line > 0 && col > 0 && line <= 1000 && col <= 1000 ==>
  let location = _atFileLocation file line col
  in property $ getErrorLine location === line .&&.
     getErrorColumn location === col .&&.
     filePath location === Just file

-- Property: _atRange creates location with range
prop_atRange :: Int -> Int -> Int -> Int -> Property
prop_atRange startLine startCol endLine endCol =
  startLine > 0 && startCol > 0 && endLine > 0 && endCol > 0 &&
  startLine <= 1000 && startCol <= 1000 && endLine <= 1000 && endCol <= 1000 ==>
  let location = _atRange startLine startCol endLine endCol
  in property $ getErrorLine location === startLine .&&.
     getErrorColumn location === startCol .&&.
     endLine location === Just endLine .&&.
     endColumn location === Just endCol

-- Helper function to check if severity is at least a given level
isAtLeast :: ErrorSeverity -> ErrorSeverity -> Bool
isAtLeast minSeverity sev = 
  case (minSeverity, sev) of
    (Info, _) -> True
    (Warning, Info) -> False
    (Warning, _) -> True
    (Error, Info) -> False
    (Error, Warning) -> False
    (Error, _) -> True
    (Fatal, Fatal) -> True
    (Fatal, _) -> False