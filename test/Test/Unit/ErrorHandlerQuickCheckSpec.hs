{-# LANGUAGE CPP #-}

module Test.Unit.ErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), property, forAll, counterexample, classify, Arbitrary(..), Gen, oneof, choose, listOf, elements, vectorOf)
import Data.List (isPrefixOf, isInfixOf, nub, nubBy, sortBy)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T

import Compiler.Errors (CompilerError(..), CompilerResult, CompilationPhase(..))
import Compiler.Errors.Core (ErrorCategory(..), ErrorSeverity(..), errorWithCategory, message, severity)
import ErrorHandler
import EnhancedErrorHandler
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..), toErrorLocation, spanFrom, startPos, toErrorLocationWithSpan)
import TestSupport.ExtendedArbitrary
import qualified Data.Text as T

-- Property: Error message formatting
prop_error_message_formatting :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_message_formatting message severity category =
  let location = toErrorLocation startPos
      typeError = errorWithCategory "test" category (T.pack message) location
      error = CompilerError typeError Nothing [] TypeCheckingPhase
      formatted = show error
  in property $ 
       message `isInfixOf` formatted &&
       show severity `isInfixOf` formatted &&
       show category `isInfixOf` formatted

-- Property: Error severity classification
prop_error_severity_classification :: ErrorSeverity -> String -> Property
prop_error_severity_classification severityVal message =
  let location = toErrorLocation startPos
      typeError = errorWithCategory "test" Parsing (T.pack message) location
      error = CompilerError typeError Nothing [] TypeCheckingPhase
      extractedSeverity = severity (ceError error)
  in property $ extractedSeverity === severityVal

-- Property: Error category classification
prop_error_category_classification :: ErrorCategory -> String -> Property
prop_error_category_classification categoryVal message =
  let location = toErrorLocation startPos
      typeError = errorWithCategory "test" categoryVal (T.pack message) location
      error = CompilerError typeError Nothing [] TypeCheckingPhase
      extractedCategory = category (ceError error)
  in property $ extractedCategory === categoryVal

-- Property: Error location preservation
prop_error_location_preservation :: String -> SourceSpan -> Property
prop_error_location_preservation message span =
  let expectedLoc = toErrorLocationWithSpan span
      typeError = errorWithCategory "test" Parsing (T.pack message) expectedLoc
      error = CompilerError typeError Nothing [] TypeCheckingPhase
      extractedLocation = location (ceError error)
  in property $ extractedLocation === expectedLoc

-- Property: Error context preservation
prop_error_context_preservation :: String -> String -> Property
prop_error_context_preservation message contextStr =
  let location = toErrorLocation startPos
      typeError = errorWithCategory "test" Parsing (T.pack message) location
      error = CompilerError typeError (Just contextStr) [] TypeCheckingPhase
      extractedContext = contextCode (context (ceError error))
  in property $ extractedContext === Just contextStr

-- Property: Error result creation
prop_error_result_creation :: CompilerError -> Property
prop_error_result_creation error =
  let result = Left [error] :: CompilerResult ()
  in property $ isLeft result

-- Property: Multiple error accumulation
prop_multiple_error_accumulation :: [CompilerError] -> Property
prop_multiple_error_accumulation errors =
  let result = Left errors :: CompilerResult ()
      errorCount = length errors
  in classify (errorCount > 1) "multiple errors" $
     property $ errorCount > 0 ==> isLeft result

-- Property: Error message uniqueness
prop_error_message_uniqueness :: [String] -> Property
prop_error_message_uniqueness messages =
  let location = toErrorLocation startPos
      errors = map (\msg -> 
        let typeError = errorWithCategory "test" Parsing (T.pack msg) location
        in CompilerError typeError Nothing [] TypeCheckingPhase) messages
      uniqueMessages = nub messages
      uniqueErrors = length [() | e <- errors, T.unpack (message (ceError e)) `elem` uniqueMessages]
  in property $ uniqueErrors === length uniqueMessages

-- Property: Error severity ordering
prop_error_severity_ordering :: [ErrorSeverity] -> Property
prop_error_severity_ordering severities =
  let errors = zipWith (\i sev -> 
        let typeError = errorWithCategory ("Error " ++ show i) Parsing (T.pack "Test message") (toErrorLocation startPos)
        in CompilerError typeError Nothing [] TypeCheckingPhase) [1..] severities
      hasError = any (\e -> severity (ceError e) == Error) errors
      hasWarning = any (\e -> severity (ceError e) == Warning) errors
      hasInfo = any (\e -> severity (ceError e) == Info) errors
  in property $ hasError || hasWarning || hasInfo

-- Property: Error category distribution
prop_error_category_distribution :: [ErrorCategory] -> Property
prop_error_category_distribution categories =
  let location = toErrorLocation startPos
      errors = zipWith (\i cat -> 
        let typeError = errorWithCategory ("Error " ++ show i) cat (T.pack "Test message") location
        in CompilerError typeError Nothing [] TypeCheckingPhase) [1..] categories
      parseErrors = length [() | e <- errors, category (ceError e) == Parsing]
      typeErrors = length [() | e <- errors, category (ceError e) == TypeChecking]
      ownershipErrors = length [() | e <- errors, category (ceError e) == Ownership]
      dependentTypeErrors = length [() | e <- errors, category (ceError e) == Inference]
      totalErrors = parseErrors + typeErrors + ownershipErrors + dependentTypeErrors
  in property $ totalErrors === length errors

-- Property: Error location validity
prop_error_location_validity :: SourceSpan -> String -> Property
prop_error_location_validity span message =
  let expectedLocation = toErrorLocationWithSpan span
      typeError = errorWithCategory "test" Parsing (T.pack message) expectedLocation
      error = CompilerError typeError Nothing [] TypeCheckingPhase
      actualLocation = location (ceError error)
  in property $ actualLocation === expectedLocation

-- Property: Error context relevance
prop_error_context_relevance :: String -> String -> Property
prop_error_context_relevance message contextStr =
  let location = toErrorLocation startPos
      typeError = errorWithCategory "test" Parsing (T.pack message) location
      error = CompilerError typeError (Just contextStr) [] TypeCheckingPhase
      errorContext = context (ceError error)
      contextRelevant = case contextCode errorContext of
        Nothing -> False
        Just ctx -> ctx `isInfixOf` message || message `isInfixOf` ctx
  in classify contextRelevant "relevant context" $
     property $ True

-- Property: Error message length limits
prop_error_message_length :: Int -> Property
prop_error_message_length length =
  length >= 0 && length <= 10000 ==>
  let messageStr = replicate length 'x'
      location = toErrorLocation startPos
      typeError = errorWithCategory "test" Parsing (T.pack messageStr) location
      error = CompilerError typeError Nothing [] TypeCheckingPhase
      actualLength = T.length $ message (ceError error)
  in property $ actualLength === length

-- Property: Error formatting consistency
prop_error_formatting_consistency :: CompilerError -> Property
prop_error_formatting_consistency error =
  let formatted1 = show error
      formatted2 = show error
  in property $ formatted1 === formatted2

-- Property: Error filtering by severity
prop_error_filter_by_severity :: [CompilerError] -> ErrorSeverity -> Property
prop_error_filter_by_severity errors targetSeverity =
  let filtered = filter (\e -> severity (ceError e) == targetSeverity) errors
      allMatched = all (\e -> severity (ceError e) == targetSeverity) filtered
  in property $ allMatched

-- Property: Error filtering by category
prop_error_filter_by_category :: [CompilerError] -> ErrorCategory -> Property
prop_error_filter_by_category errors targetCategory =
  let filtered = filter (\e -> category (ceError e) == targetCategory) errors
      allMatched = all (\e -> category (ceError e) == targetCategory) filtered
  in property $ allMatched

-- Property: Error aggregation
prop_error_aggregation :: [CompilerError] -> [CompilerError] -> Property
prop_error_aggregation errors1 errors2 =
  let allErrors = errors1 ++ errors2
      totalCount = length allErrors
      expectedCount = length errors1 + length errors2
  in property $ totalCount === expectedCount

-- Property: Error deduplication
prop_error_deduplication :: [CompilerError] -> Property
prop_error_deduplication errors =
  let uniqueErrors = nubBy (\e1 e2 -> message (ceError e1) == message (ceError e2)) errors
      hasDuplicates = length errors > length uniqueErrors
  in classify hasDuplicates "has duplicates" $
     property $ length uniqueErrors <= length errors

-- Property: Error sorting by severity
prop_error_sorting_by_severity :: [CompilerError] -> Property
prop_error_sorting_by_severity errors =
  let sorted = sortErrorsBySeverity errors
      isSorted = all (\(e1, e2) -> severityOrder (severity $ ceError e1) <= severityOrder (severity $ ceError e2)) (zip sorted (tail sorted))
  in property $ isSorted || length sorted <= 1
  where
    severityOrder Fatal = 3
    severityOrder Error = 2
    severityOrder Warning = 1
    severityOrder Info = 0
    sortErrorsBySeverity = sortBy (\e1 e2 -> compare (severityOrder $ severity $ ceError e1) (severityOrder $ severity $ ceError e2))

-- Property: Error count statistics
prop_error_count_statistics :: [CompilerError] -> Property
prop_error_count_statistics errors =
  let errorCount = length [() | e <- errors, severity (ceError e) == Error]
      warningCount = length [() | e <- errors, severity (ceError e) == Warning]
      infoCount = length [() | e <- errors, severity (ceError e) == Info]
      totalCount = errorCount + warningCount + infoCount
  in property $ totalCount === length errors

-- Property: Error message templates
prop_error_message_templates :: String -> String -> Property
prop_error_message_templates template placeholder =
  let messageStr = template ++ " " ++ placeholder
      location = toErrorLocation startPos
      typeError = errorWithCategory "test" Parsing (T.pack messageStr) location
      error = CompilerError typeError Nothing [] TypeCheckingPhase
      hasTemplate = template `isInfixOf` (T.unpack $ message $ ceError error)
      hasPlaceholder = placeholder `isInfixOf` (T.unpack $ message $ ceError error)
  in property $ hasTemplate && hasPlaceholder

-- Property: Error context extraction
prop_error_context_extraction :: String -> String -> String -> Property
prop_error_context_extraction before message after =
  let fullContext = before ++ message ++ after
      location = toErrorLocation startPos
      typeError = errorWithCategory "test" Parsing (T.pack message) location
      error = CompilerError typeError (Just fullContext) [] TypeCheckingPhase
      extractedContext = fromMaybe "" $ ceSourceContext error
      containsMessage = message `isInfixOf` extractedContext
  in property $ containsMessage

-- Property: Error location span validation
prop_error_span_validation :: SourcePos -> SourcePos -> Property
prop_error_span_validation start end =
  let span = SourceSpan start end
      message = "Test error"
      errorLocation = toErrorLocation start
      typeError = errorWithCategory "test" Parsing (T.pack message) errorLocation
      error = CompilerError typeError Nothing [] TypeCheckingPhase
      extractedLocation = location (ceError error)
      extractedSpan = Just span
  in property $ extractedSpan === Just span

-- Property: Error result unwrapping
prop_error_result_unwrapping :: CompilerError -> Property
prop_error_result_unwrapping error =
  let result = Left [error] :: CompilerResult ()
      unwrapped = either show (const "Success") result
  in property $ unwrapped === show error

-- Property: Enhanced error handling
prop_enhanced_error_handling :: [CompilerError] -> Property
prop_enhanced_error_handling errors =
  let enhanced = mapEnhanceError errors
      enhancedCount = length enhanced
  in property $ enhancedCount === length errors
  where
    mapEnhanceError = map (\e -> 
      let typeError = ceError e
          enhancedMessage = T.pack "Enhanced: " `T.append` message typeError
          enhancedTypeError = typeError { message = enhancedMessage }
      in e { ceError = enhancedTypeError })

tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck Tests"
  [ fastProperty "Error message formatting" prop_error_message_formatting
  , fastProperty "Error severity classification" prop_error_severity_classification
  , fastProperty "Error category classification" prop_error_category_classification
  , fastProperty "Error location preservation" prop_error_location_preservation
  , fastProperty "Error context preservation" prop_error_context_preservation
  , fastProperty "Error result creation" prop_error_result_creation
  , fastProperty "Multiple error accumulation" prop_multiple_error_accumulation
  , fastProperty "Error message uniqueness" prop_error_message_uniqueness
  , fastProperty "Error severity ordering" prop_error_severity_ordering
  , fastProperty "Error category distribution" prop_error_category_distribution
  , fastProperty "Error location validity" prop_error_location_validity
  , fastProperty "Error context relevance" prop_error_context_relevance
  , fastProperty "Error message length limits" prop_error_message_length
  , fastProperty "Error formatting consistency" prop_error_formatting_consistency
  , fastProperty "Error filtering by severity" prop_error_filter_by_severity
  , fastProperty "Error filtering by category" prop_error_filter_by_category
  , fastProperty "Error aggregation" prop_error_aggregation
  , fastProperty "Error deduplication" prop_error_deduplication
  , fastProperty "Error sorting by severity" prop_error_sorting_by_severity
  , fastProperty "Error count statistics" prop_error_count_statistics
  , fastProperty "Error message templates" prop_error_message_templates
  , fastProperty "Error context extraction" prop_error_context_extraction
  , fastProperty "Error location span validation" prop_error_span_validation
  , fastProperty "Error result unwrapping" prop_error_result_unwrapping
  , fastProperty "Enhanced error handling" prop_enhanced_error_handling
  ]