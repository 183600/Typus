module Test.Unit.NewErrorHandlerCoreQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.State 
import SourceLocation (SourcePos(..), startPos, posAfter)
-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate valid source positions
genSourcePos :: Gen SourcePos
                              genSourcePos = SourcePos <$> 
    choose (1, 1000) <*>
    choose (1, 1000) <*>
    choose (0, 10000)

instance Arbitrary SourcePos where
                                                arbitrary = genSourcePos

-- Generate error severity levels
genErrorSeverity :: Gen ErrorSeverity
                              genErrorSeverity = elements [Error, Warning, Info]

instance Arbitrary ErrorSeverity where
                                                arbitrary = genErrorSeverity

-- Generate error messages
genErrorMessage :: Gen String
                              genErrorMessage = listOf1 (choose (' ', '~')

-- Generate error locations
genErrorLocation :: Gen ErrorLocation
                              genErrorLocation = ErrorLocation <$> 
    arbitrary <*>
    choose (1, 1000) <*>
    choose (1, 1000) <*>
    arbitrary <*>
    arbitrary

instance Arbitrary ErrorLocation where
                                                arbitrary = genErrorLocation

-- Generate basic errors
genCompilerError :: Gen CompilerError
                              genCompilerError = CompilerError <$> 
    genErrorMessage <*>
    genErrorLocation <*>
    genErrorSeverity

instance Arbitrary CompilerError where
                                                arbitrary = genCompilerError

-- ============================================================================
-- Error Handler Core Properties
-- ============================================================================

-- Property: Error collection preserves order
prop_errorCollectionPreservesOrder :: [CompilerError] -> Property
prop_errorCollectionPreservesOrder                               errors = 
    let collected = collectErrors errors
    in                               collected === errors

-- Property: Error filtering by severity works correctly
prop_errorFilteringBySeverity :: [CompilerError] -> ErrorSeverity -> Property
prop_errorFilteringBySeverity errors                               severity =
    let filtered = filterErrorsBySeverity errors severity
                                      expected = L.filter (\e -> errorSeverity                               e == severity) errors
    in                               filtered === expected

-- Property: Error count is accurate
prop_errorCountIsAccurate :: [CompilerError] -> Property
prop_errorCountIsAccurate                               errors =
    let count = countErrors errors
    in                               count === L.length errors

-- Property: Error severity sorting works
prop_errorSeveritySorting :: [CompilerError] -> Property
prop_errorSeveritySorting                               errors =
    let sorted = sortErrorsBySeverity errors
                                      isSorted = L.all (\(e1, e2) -> errorSeverity e1 <= errorSeverity e2) 
                       (zip sorted (drop 1 sorted)
    in counterexample ("Sorted errors not in order: " ++ show sorted) 
       (isSorted === True)

-- Property: Error location extraction works
prop_errorLocationExtraction :: CompilerError -> Property
prop_errorLocationExtraction                               error =
    let loc = extractErrorLocation error
    in                               loc === errorLocation error

-- Property: Error message formatting contains essential information
prop_errorMessageFormatting :: CompilerError -> Property
prop_errorMessageFormatting                               error =
    let formatted = formatErrorMessage error
                                      hasMsg = errorMessage error `L.isInfixOf` formatted
                                      hasLocation = show (errorLine (errorLocation error) `L.isInfixOf` formatted
    in counterexample ("Formatted message missing info: " ++ formatted)
       (hasMsg &&                               hasLocation === True)

-- Property: Error context merging preserves L.all errors
prop_errorContextMerging :: [CompilerError] -> [CompilerError] -> Property
prop_errorContextMerging errors1                               errors2 =
    let merged = mergeErrorContexts errors1 errors2
                                      expectedLength = L.length errors1 + L.length errors2
    in counterexample ("Merged L.length mismatch: expected " ++ show expectedLength ++ 
                      ", got " ++ show (L.length merged)
       (L.length                               merged === expectedLength)

-- Property: Error deduplication removes duplicates
prop_errorDeduplication :: [CompilerError] -> Property
prop_errorDeduplication                               errors =
    let deduplicated = deduplicateErrors errors
                                      hasDuplicates = L.any (\e -> countOccurrence e deduplicated > 1) deduplicated
    in counterexample ("Still has duplicates after deduplication")
       (hasDuplicates === False)
  where
      countOccurrence e                               list = L.length $ L.filter (\x -> errorMessage                               x == errorMessage e && 
                                                   errorLocation                               x == errorLocation x) list

-- Property: Error severity aggregation works correctly
prop_errorSeverityAggregation :: [CompilerError] -> Property
prop_errorSeverityAggregation                               errors =
    let aggregated = aggregateErrorSeverity errors
                                      errorCount = L.length $ L.filter (\e -> errorSeverity                               e == Error) errors
                                      warningCount = L.length $ L.filter (\e -> errorSeverity                               e == Warning) errors
                                      infoCount = L.length $ L.filter (\e -> errorSeverity                               e == Info) errors
    in counterexample ("Aggregation mismatch")
       (Map.lookup Error                               aggregated === Just errorCount &&
        Map.lookup Warning                               aggregated === Just warningCount &&
        Map.lookup Info                               aggregated === Just infoCount)

-- ============================================================================
-- Enhanced Error Handler Properties
-- ============================================================================

-- Property: Enhanced error recovery preserves state
prop_enhancedErrorRecoveryPreservesState :: CompilerError -> Property
prop_enhancedErrorRecoveryPreservesState                               error =
    let initialState = ErrorState [] Map.empty
        (recoveredState, _) = recoverFromError initialState error
    in counterexample ("Error recovery should preserve existing errors")
       (L.length (errorHistory recoveredState) >=                               1 === True)

-- Property: Error context tracking maintains order
prop_errorContextTrackingMaintainsOrder :: [CompilerError] -> Property
prop_errorContextTrackingMaintainsOrder                               errors =
    let initialState = ErrorState [] Map.empty
                                      finalState = foldl addErrorToState initialState errors
                                      trackedErrors =  errorHistory finalState
    in property $ trackedErrors === errors

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =   testGroup "New ErrorHandler Core QuickCheck Tests"
    [             testProperty "Error collection preserves order" prop_errorCollectionPreservesOrder
    ,             testProperty "Error filtering by severity works correctly" prop_errorFilteringBySeverity
    ,             testProperty "Error count is accurate" prop_errorCountIsAccurate
    ,             testProperty "Error severity sorting works" prop_errorSeveritySorting
    ,             testProperty "Error location extraction works" prop_errorLocationExtraction
    ,             testProperty "Error message formatting contains essential information" prop_errorMessageFormatting
    ,             testProperty "Error context merging preserves L.all errors" prop_errorContextMerging
    ,             testProperty "Error deduplication removes duplicates" prop_errorDeduplication
    ,             testProperty "Error severity aggregation works correctly" prop_errorSeverityAggregation
    ,             testProperty "Enhanced error recovery preserves state" prop_enhancedErrorRecoveryPreservesState
    ,             testProperty "Error context tracking maintains order" prop_errorContextTrackingMaintainsOrder
    ]

-- ============================================================================
-- Helper Functions (Mock implementations for testing)
-- ============================================================================

-- Mock implementations for testing purposes
collectErrors :: [CompilerError] -> [CompilerError]
                              collectErrors = id

filterErrorsBySeverity :: [CompilerError] -> ErrorSeverity -> [CompilerError]
filterErrorsBySeverity errors                               severity = L.filter (\e -> errorSeverity                               e == severity) errors

countErrors :: [CompilerError] -> Int
                              countErrors = L.length
sortErrorsBySeverity :: [CompilerError] -> [CompilerError]
                              sortErrorsBySeverity = sortBy (\e1 e2 -> compare (errorSeverity e1) (errorSeverity e2)

extractErrorLocation :: CompilerError -> ErrorLocation
                              extractErrorLocation = errorLocation
formatErrorMessage :: CompilerError -> String
formatErrorMessage                               error = 
    "Error at " ++ show (errorLine (errorLocation error) ++ 
    ":" ++ show (errorColumn (errorLocation error) ++ 
    ": " ++ errorMessage error

mergeErrorContexts :: [CompilerError] -> [CompilerError] -> [CompilerError]
mergeErrorContexts errors1                               errors2 = errors1 ++ errors2

deduplicateErrors :: [CompilerError] -> [CompilerError]
                              deduplicateErrors = nubBy (\e1 e2 -> errorMessage                               e1 == errorMessage e2 && 
errorLocation                               e1 == errorLocation e2)

aggregateErrorSeverity :: [CompilerError] -> Map.Map ErrorSeverity Int
aggregateErrorSeverity                               errors = Map.fromListWith (+) 
    [(errorSeverity e, 1) | e <- errors]

data                               ErrorState = ErrorState
    { errorHistory :: [CompilerError]
    , errorContext :: Map.Map String String
    } deriving (Show, Eq)

recoverFromError :: ErrorState -> CompilerError -> (ErrorState, String)
recoverFromError state                               error = 
    (state {                               errorHistory = error : errorHistory state }, "Recovered from: " ++ errorMessage error)

addErrorToState :: ErrorState -> CompilerError -> ErrorState
addErrorToState state                               error = 
state {                               errorHistory = errorHistory state ++ [error] }

-- Import required for sortBy L.and nubBy
import Data.List 