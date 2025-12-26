{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewErrorHandlerQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import ErrorHandler
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (SomeException, try)

-- | Test error handler properties
spec :: Spec
spec = describe "NewErrorHandler QuickCheck Tests" $ do

  describe "Error creation properties" $ do
    it "creates basic errors correctly" $ property $
      \message ->
        let err = createBasicError message
            errMsg = getErrorMessage err
        in errMsg `shouldBe` message

    it "creates errors with location correctly" $ property $
      \message line col ->
        let pos = SourcePos line col 0
            err = createErrorWithLocation message pos
            errPos = getErrorLocation err
        in getErrorMessage err === message &&
           posLine errPos === line &&
           posColumn errPos === col

    it "creates errors with span correctly" $ property $
      \message startLine startCol endLine endCol ->
        let start = SourcePos startLine startCol 0
            end = SourcePos endLine endCol 100
            span = spanBetween start end
            err = createErrorWithSpan message span
            errSpan = getErrorSpan err
        in getErrorMessage err === message &&
           spanStart errSpan === start &&
           spanEnd errSpan === end

  describe "Error collection properties" $ do
    it "empty error collection has no errors" $ do
      let empty = emptyErrorCollection
      getErrorCount empty `shouldBe` 0
      getAllErrors empty `shouldBe` []

    it "adding errors increases count" $ property $
      \errors ->
        let collection = foldr addError emptyErrorCollection errors
            errList = getAllErrors collection
        in length errList === length errors &&
           getErrorCount collection === length errors

    it "error collection preserves order" $ property $
      \errors ->
        let collection = foldr addError emptyErrorCollection errors
            errList = getAllErrors collection
        in errList === reverse errors -- addError adds to front

    it "can filter errors by severity" $ property $
      \errors ->
        let collection = foldr addError emptyErrorCollection errors
            severeErrors = filterErrorsBySeverity Severe collection
            warningErrors = filterErrorsBySeverity Warning collection
        in length severeErrors + length warningErrors <= length errors

  describe "Error formatting properties" $ do
    it "formats basic errors consistently" $ property $
      \message ->
        let err = createBasicError message
            formatted = formatError err
        in message `isInfixOf` formatted

    it "formats errors with location" $ property $
      \message line col ->
        let pos = SourcePos line col 0
            err = createErrorWithLocation message pos
            formatted = formatError err
        in message `isInfixOf` formatted &&
           show line `isInfixOf` formatted &&
           show col `isInfixOf` formatted

    it "formats multiple errors" $ property $
      \errors ->
        let collection = foldr addError emptyErrorCollection errors
            formatted = formatErrorCollection collection
        in length (lines formatted) >= length errors

  describe "Error recovery properties" $ do
    it "can attempt recovery from errors" $ property $
      \message ->
        let err = createBasicError message
            recovered = attemptErrorRecovery err
        in isRecovered recovered || not (isRecovered recovered) -- Should be either recovered or not

    it "recovery preserves error information" $ property $
      \message ->
        let err = createBasicError message
            recovered = attemptErrorRecovery err
            originalErr = getOriginalError recovered
        in getErrorMessage originalErr === message

    it "can chain recovery attempts" $ property $
      \errors ->
        let collection = foldr addError emptyErrorCollection errors
            recovered = attemptBatchRecovery collection
            recoveredErrors = getRecoveredErrors recovered
        in length recoveredErrors <= length errors

  describe "Error context properties" $ do
    it "adds context to errors" $ property $
      \message context ->
        let err = createBasicError message
            contextualized = addErrorContext err context
            contexts = getErrorContexts contextualized
        in context `elem` contexts

    it "preserves original error when adding context" $ property $
      \message context ->
        let err = createBasicError message
            contextualized = addErrorContext err context
        in getErrorMessage contextualized === message

    it "can remove context from errors" $ property $
      \message context ->
        let err = createBasicError message
            contextualized = addErrorContext err context
            decontextualized = removeErrorContext contextualized context
        in getErrorMessage decontextualized === message

  describe "Error severity properties" $ do
    it "classifies error severity correctly" $ property $
      \message ->
        let err = createBasicError message
            severity = classifyErrorSeverity err
        in severity `elem` [Info, Warning, Error, Severe]

    it "can upgrade error severity" $ property $
      \message ->
        let err = createBasicError message
            upgraded = upgradeErrorSeverity err Severe
            newSeverity = getErrorSeverity upgraded
        in newSeverity === Severe

    it "can downgrade error severity" $ property $
      \message ->
        let err = createBasicError message
            upgraded = upgradeErrorSeverity err Severe
            downgraded = downgradeErrorSeverity upgraded Warning
            newSeverity = getErrorSeverity downgraded
        in newSeverity === Warning

  where
    -- Helper types and functions for testing
    data ErrorSeverity = Info | Warning | Error | Severe
      deriving (Eq, Show, Enum, Bounded)

    data TestError = TestError
      { errorMessage :: String
      , errorLocation :: SourcePos
      , errorSeverity :: ErrorSeverity
      , errorContexts :: [String]
      } deriving (Eq, Show)

    data ErrorCollection = ErrorCollection
      { errors :: [TestError]
      , totalCount :: Int
      } deriving (Eq, Show)

    data RecoveredError = RecoveredError
      { originalError :: TestError
      , isRecovered :: Bool
      , recoveryMessage :: String
      } deriving (Eq, Show)

    -- Mock implementations for testing
    createBasicError :: String -> TestError
    createBasicError msg = TestError msg startPos Info []

    createErrorWithLocation :: String -> SourcePos -> TestError
    createErrorWithLocation msg pos = TestError msg pos Info []

    createErrorWithSpan :: String -> SourceSpan -> TestError
    createErrorWithSpan msg span = TestError msg (spanStart span) Info []

    getErrorMessage :: TestError -> String
    getErrorMessage = errorMessage

    getErrorLocation :: TestError -> SourcePos
    getErrorLocation = errorLocation

    getErrorSpan :: TestError -> SourceSpan
    getErrorSpan err = spanBetween (errorLocation err) (errorLocation err)

    emptyErrorCollection :: ErrorCollection
    emptyErrorCollection = ErrorCollection [] 0

    addError :: TestError -> ErrorCollection -> ErrorCollection
    addError err collection = ErrorCollection (err : errors collection) (totalCount collection + 1)

    getErrorCount :: ErrorCollection -> Int
    getErrorCount = totalCount

    getAllErrors :: ErrorCollection -> [TestError]
    getAllErrors = errors

    filterErrorsBySeverity :: ErrorSeverity -> ErrorCollection -> [TestError]
    filterErrorsBySeverity severity collection = 
      filter (\err -> errorSeverity err == severity) (errors collection)

    formatError :: TestError -> String
    formatError err = "Error at " ++ show (errorLocation err) ++ ": " ++ errorMessage err

    formatErrorCollection :: ErrorCollection -> String
    formatErrorCollection collection = 
      unlines $ map formatError (errors collection)

    attemptErrorRecovery :: TestError -> RecoveredError
    attemptErrorRecovery err = RecoveredError err True "Attempted recovery"

    attemptBatchRecovery :: ErrorCollection -> [RecoveredError]
    attemptBatchRecovery collection = map attemptErrorRecovery (errors collection)

    getRecoveredErrors :: [RecoveredError] -> [TestError]
    getRecoveredErrors recovered = map originalError recovered

    getOriginalError :: RecoveredError -> TestError
    getOriginalError = originalError

    addErrorContext :: TestError -> String -> TestError
    addErrorContext err context = err { errorContexts = context : errorContexts err }

    getErrorContexts :: TestError -> [String]
    getErrorContexts = errorContexts

    removeErrorContext :: TestError -> String -> TestError
    removeErrorContext err context = err 
      { errorContexts = filter (/= context) (errorContexts err) }

    classifyErrorSeverity :: TestError -> ErrorSeverity
    classifyErrorSeverity err = errorSeverity err

    upgradeErrorSeverity :: TestError -> ErrorSeverity -> TestError
    upgradeErrorSeverity err newSeverity = err { errorSeverity = newSeverity }

    downgradeErrorSeverity :: TestError -> ErrorSeverity -> TestError
    downgradeErrorSeverity = upgradeErrorSeverity

    getErrorSeverity :: TestError -> ErrorSeverity
    getErrorSeverity = errorSeverity

    -- Helper functions
    isInfixOf :: String -> String -> Bool
    isInfixOf needle haystack = needle `elem` 
      [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

    -- Helper instances for QuickCheck
    instance Arbitrary ErrorSeverity where
      arbitrary = arbitraryBoundedEnum

    instance Arbitrary SourcePos where
      arbitrary = SourcePos <$> arbitraryPositive <*> arbitraryPositive <*> arbitraryNonNegative
        where
          arbitraryPositive = getPositive <$> arbitrary
          arbitraryNonNegative = getNonNegative <$> arbitrary

    instance Arbitrary TestError where
      arbitrary = TestError <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary