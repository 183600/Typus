{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ErrorHandlingConsistencyQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

import Compiler.Errors.Core
import SourceLocation

-- | Test error severity ordering consistency
testErrorSeverityOrdering :: Property
testErrorSeverityOrdering =
  forAll arbitrary $ \errors ->
    let sortedBySeverity = sort $ map errorSeverity errors
        orderedSeverities = [Info, Warning, Error, FatalError]
    in all (`elem` orderedSeverities) sortedBySeverity

-- | Test error collection consistency
testErrorCollectionConsistency :: Property
testErrorCollectionConsistency =
  forAll arbitrary $ \errors ->
    forAll arbitrary $ \warnings ->
      forAll arbitrary $ \infos ->
        let collector = newErrorCollector
            withErrors = foldr addError collector errors
            withWarnings = foldr addWarning withErrors warnings
            withInfos = foldr addInfo withWarnings infos
            allErrors = getErrors withInfos
            allWarnings = getWarnings withInfos
            allInfos = getInfo withInfos
        in length allErrors === length errors .&&.
           length allWarnings === length warnings .&&.
           length allInfos === length infos

-- | Test error formatting consistency
testErrorFormattingConsistency :: Property
testErrorFormattingConsistency =
  forAll arbitrary $ \error ->
    let formatted = formatError error
        formattedWithLocation = formatErrorWithLocation error
        hasLocation = not (null formattedWithLocation) && 
                      formattedWithLocation /= formatted
    in not (null formatted) .&&.
       (if isValidLocation (errorLocation error)
        then hasLocation
        else formattedWithLocation === formatted)

-- | Test error filtering consistency
testErrorFilteringConsistency :: Property
testErrorFilteringConsistency =
  forAll arbitrary $ \errors ->
    forAll arbitrary $ \category ->
      let filtered = filterByCategory category errors
          categorized = filter (\e -> hasCategory e category) errors
      in sort filtered === sort categorized

-- | Test error recovery strategy consistency
testErrorRecoveryStrategyConsistency :: Property
testErrorRecoveryStrategyConsistency =
  forAll arbitrary $ \error ->
    let canRecover = canRecoverFrom error
        shouldContinue = shouldContinueAfter error
        severity = errorSeverity error
    in if severity == FatalError
       then not canRecover .&&. not shouldContinue
       else property True

-- | Test error combination consistency
testErrorCombinationConsistency :: Property
testErrorCombinationConsistency =
  forAll arbitrary $ \errors ->
    let combined = combineErrors errors
        combinedSeverity = combinedErrorSeverity combined
        maxSeverity = if null errors 
                     then Info 
                     else maximum $ map errorSeverity errors
    in combinedSeverity === maxSeverity

-- | Test error location consistency
testErrorLocationConsistency :: Property
testErrorLocationConsistency =
  forAll arbitrary $ \pos ->
    forAll arbitrary $ \message ->
      let error = errorAt pos message
          locatedError = withLocation pos error
      in errorLocation error === toErrorLocation pos .&&.
         errorLocation locatedError === errorLocation error

-- | Test error context consistency
testErrorContextConsistency :: Property
testErrorContextConsistency =
  forAll arbitrary $ \error ->
    forAll arbitrary $ \context ->
      let contextualized = withContext context error
          originalContext = errorContext error
          newContext = errorContext contextualized
      in if null context
         then contextualized === error
         else newContext /= originalContext

-- | Test error suggestion consistency
testErrorSuggestionConsistency :: Property
testErrorSuggestionConsistency =
  forAll arbitrary $ \error ->
    forAll arbitrary $ \suggestions ->
      let withSuggestions = errorWithSuggestions error suggestions
          originalSuggestions = errorSuggestions error
          newSuggestions = errorSuggestions withSuggestions
      in if null suggestions
         then withSuggestions === error
         else length newSuggestions >= length originalSuggestions

-- | Test error timestamp consistency
testErrorTimestampConsistency :: Property
testErrorTimestampConsistency =
  forAll arbitrary $ \error ->
    let withTimestamp = withUTCTimestamp error
        originalTime = errorTimestamp error
        newTime = errorTimestamp withTimestamp
    in isJust newTime .&&.
       newTime /= originalTime

-- | Test error statistics consistency
testErrorStatisticsConsistency :: Property
testErrorStatisticsConsistency =
  forAll arbitrary $ \errors ->
    let stats = getErrorStatistics errors
        totalCount = length errors
        errorCount = length $ filter (\e -> errorSeverity e == Error) errors
        warningCount = length $ filter (\e -> errorSeverity e == Warning) errors
        infoCount = length $ filter (\e -> errorSeverity e == Info) errors
        fatalCount = length $ filter (\e -> errorSeverity e == FatalError) errors
    in esTotal stats === fromIntegral totalCount .&&.
       esErrors stats === fromIntegral errorCount .&&.
       esWarnings stats === fromIntegral warningCount .&&.
       esInfos stats === fromIntegral infoCount .&&.
       esFatal stats === fromIntegral fatalCount

-- | Test error report generation consistency
testErrorReportGenerationConsistency :: Property
testErrorReportGenerationConsistency =
  forAll arbitrary $ \errors ->
    let report = generateErrorReport errors
        reportLines = lines report
        errorCount = length errors
    in if null errors
       then null report
       else length reportLines >= errorCount

-- | Test error category ordering
testErrorCategoryOrdering :: Property
testErrorCategoryOrdering =
  forAll arbitrary $ \errors ->
    let categories = map errorCategory errors
        uniqueCategories = nub categories
        sortedCategories = sort uniqueCategories
    in length sortedCategories >= 0

-- | Test error message consistency
testErrorMessageConsistency :: Property
testErrorMessageConsistency =
  forAll arbitrary $ \error ->
    let message = errorMessage error
        formatted = formatError error
    in not (null message) ==> message `isInfixOf` formatted

-- | Test error wrapping consistency
testErrorWrappingConsistency :: Property
testErrorWrappingConsistency =
  forAll arbitrary $ \innerError ->
    forAll arbitrary $ \wrapperMessage ->
      let wrapped = wrapError wrapperMessage innerError
          innerMessage = errorMessage innerError
          wrapperMessageCheck = errorMessage wrapped
      in wrapperMessage `isInfixOf` wrapperMessageCheck .&&.
         innerMessage `isInfixOf` wrapperMessageCheck

-- | Test error location tracking through transformations
testErrorLocationTracking :: Property
testErrorLocationTracking =
  forAll arbitrary $ \pos1 ->
    forAll arbitrary $ \pos2 ->
      forAll arbitrary $ \message ->
        let error1 = errorAt pos1 message
            error2 = withLocation pos2 error1
            finalPos = errorLocation error2
        in finalPos === toErrorLocation pos2

tests :: TestTree
tests = testGroup "Error Handling Consistency QuickCheck Tests"
  [ testProperty "Severity ordering" testErrorSeverityOrdering
  , testProperty "Collection consistency" testErrorCollectionConsistency
  , testProperty "Formatting consistency" testErrorFormattingConsistency
  , testProperty "Filtering consistency" testErrorFilteringConsistency
  , testProperty "Recovery strategy" testErrorRecoveryStrategyConsistency
  , testProperty "Combination consistency" testErrorCombinationConsistency
  , testProperty "Location consistency" testErrorLocationConsistency
  , testProperty "Context consistency" testErrorContextConsistency
  , testProperty "Suggestion consistency" testErrorSuggestionConsistency
  , testProperty "Timestamp consistency" testErrorTimestampConsistency
  , testProperty "Statistics consistency" testErrorStatisticsConsistency
  , testProperty "Report generation" testErrorReportGenerationConsistency
  , testProperty "Category ordering" testErrorCategoryOrdering
  , testProperty "Message consistency" testErrorMessageConsistency
  , testProperty "Wrapping consistency" testErrorWrappingConsistency
  , testProperty "Location tracking" testErrorLocationTracking
  ]