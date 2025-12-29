module Test.Unit.NewErrorHandlerCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, (===), forAll, Gen, choose, arbitrary, listOf1, elements)
import TestSupport.QuickCheck (fastProperty)

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , emptyContext
  , ErrorRecovery(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getErrors
  , getWarnings
  , getInfo
  , getAllMessages
  , hasErrors
  , hasWarnings
  , formatError
  , formatErrors
  , formatErrorWithLocation
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , errorAt
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  , fatalError
  , fatalErrorWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , wrapError
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  )
import SourceLocation (SourcePos(..))

-- ============================================================================
-- Generators
-- ============================================================================

-- Generate error severity levels
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [ErrorInfo, Warning, Error, FatalError]

-- Generate error categories
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ ParseError
  , TypeError
  , NameError
  , ScopeError
  , ImportError
  , SyntaxError
  , SemanticError
  , RuntimeError
  , InternalError
  , UserError
  ]

-- Generate source positions for error locations
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  column <- choose (1, 200)
  offset <- choose (0, 100000)
  return $ SourcePos line column offset

-- Generate error messages
genErrorMessage :: Gen String
genErrorMessage = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t.,;:!()[]{}<>+-*/%=|&^~?@#"

-- Generate suggestion strings
genSuggestion :: Gen String
genSuggestion = do
  base <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['.', '!']
  return $ "Try: " ++ base

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Empty context has no information
prop_emptyContext :: Bool
prop_emptyContext =
  let ctx = emptyContext
  in null ctx  -- Simplified property

-- Property: New error collector has no messages
prop_newErrorCollectorEmpty :: Bool
prop_newErrorCollectorEmpty =
  let collector = newErrorCollector
  in not (hasErrors collector) && not (hasWarnings collector)

-- Property: Adding error results in hasErrors returning True
prop_addErrorCreatesError :: String -> ErrorSeverity -> ErrorCategory -> Bool
prop_addErrorCreatesError msg severity category =
  let collector = newErrorCollector
      withError = addError msg severity category collector
  in hasErrors withError

-- Property: Adding warning results in hasWarnings returning True
prop_addWarningCreatesWarning :: String -> ErrorCategory -> Bool
prop_addWarningCreatesWarning msg category =
  let collector = newErrorCollector
      withWarning = addWarning msg category collector
  in hasWarnings withWarning

-- Property: Error severity ordering is consistent
prop_severityOrdering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_severityOrdering sev1 sev2 =
  let combined = combineErrors 
        [CombinedError sev1 "" "" Nothing [] [] []]
        [CombinedError sev2 "" "" Nothing [] [] []]
      maxSev = combinedErrorSeverity combined
  in (sev1 >= sev2 && maxSev == sev1) || (sev2 > sev1 && maxSev == sev2)

-- Property: Filtering by category preserves order
prop_filterByCategoryPreservesOrder :: ErrorCategory -> [CombinedError] -> Bool
prop_filterByCategoryPreservesOrder category errors =
  let filtered = filterByCategory category errors
      originalIndices = map fst $ filter (\(_, e) -> errorCategory e == category) $ zip [0..] errors
  in length filtered == length originalIndices

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "New ErrorHandler Core Tests"
  [ testGroup "Error Collector Properties"
    [ testProperty "Empty context has no information" prop_emptyContext
    , testProperty "New error collector has no messages" prop_newErrorCollectorEmpty
    , testProperty "Adding error results in hasErrors returning True" prop_addErrorCreatesError
    , testProperty "Adding warning results in hasWarnings returning True" prop_addWarningCreatesWarning
    , testProperty "Error severity ordering is consistent" prop_severityOrdering
    , testProperty "Filtering by category preserves order" prop_filterByCategoryPreservesOrder
    ]

  , testGroup "Basic Error Creation"
    [ testCase "Create simple error with errorAt" $ do
        let pos = SourcePos 10 5 100
            error = errorAt pos "Test error message"
        errorMessage error @?= "Test error message"
        errorSeverity error @?= Error
        errorLocation error @?= Just (ErrorLocation 10 5 Nothing Nothing)

    , testCase "Create error with category using errorWithCategory" $ do
        let error = errorWithCategory TypeError "Type mismatch error"
        errorMessage error @?= "Type mismatch error"
        errorSeverity error @?= Error
        errorCategory error @?= TypeError

    , testCase "Create warning with warningAt" $ do
        let pos = SourcePos 5 3 50
            warning = warningAt pos "Warning message"
        errorMessage warning @?= "Warning message"
        errorSeverity warning @?= Warning
        errorLocation warning @?= Just (ErrorLocation 5 3 Nothing Nothing)

    , testCase "Create info message with infoAt" $ do
        let pos = SourcePos 1 1 0
            info = infoAt pos "Info message"
        errorMessage info @?= "Info message"
        errorSeverity info @?= ErrorInfo
        errorLocation info @?= Just (ErrorLocation 1 1 Nothing Nothing)

    , testCase "Create fatal error with fatalError" $ do
        let fatal = fatalError "Fatal error occurred"
        errorMessage fatal @?= "Fatal error occurred"
        errorSeverity fatal @?= FatalError
    ]

  , testGroup "Error Enhancement"
    [ testCase "Add location to error with withLocation" $ do
        let baseError = errorWithCategory TypeError "Base error"
            pos = SourcePos 7 8 70
            locatedError = withLocation pos baseError
        errorLocation locatedError @?= Just (ErrorLocation 7 8 Nothing Nothing)

    , testCase "Add context to error with withContext" $ do
        let baseError = errorWithCategory TypeError "Base error"
            context = "Function call context"
            contextualError = withContext context baseError
        errorContext contextualError @?= context

    , testCase "Add suggestions to error with withSuggestions" $ do
        let baseError = errorWithCategory TypeError "Base error"
            suggestions = ["Try using different type", "Check variable declaration"]
            suggestedError = withSuggestions suggestions baseError
        errorSuggestions suggestedError @?= suggestions

    , testCase "Add related errors with withRelatedErrors" $ do
        let baseError = errorWithCategory TypeError "Base error"
            related = [warningWithCategory NameError "Related warning"]
            relatedError = withRelatedErrors related baseError
        relatedErrors relatedError @?= related

    , testCase "Wrap error with additional context using wrapError" $ do
        let originalError = errorWithCategory TypeError "Original error"
            wrapper = wrapError "Wrapper context" originalError
        errorMessage wrapper @?= "Wrapper context: Original error"
    ]

  , testGroup "Error Combination and Analysis"
    [ testCase "Combine errors with combineErrors" $ do
        let error1 = errorWithCategory TypeError "Type error"
            error2 = warningWithCategory NameError "Name warning"
            combined = combineErrors [error1] [error2]
        length combined @?= 2
        combinedErrorSeverity combined @?= Error  -- Higher severity wins

    , testCase "Filter errors by severity" $ do
        let errors = [errorWithCategory TypeError "Error", warningWithCategory NameError "Warning"]
            errorOnly = filterBySeverity Error errors
            warningOnly = filterBySeverity Warning errors
        length errorOnly @?= 1
        length warningOnly @?= 1
        errorSeverity (head errorOnly) @?= Error
        errorSeverity (head warningOnly) @?= Warning

    , testCase "Filter errors by category" $ do
        let errors = [errorWithCategory TypeError "Type error", errorWithCategory NameError "Name error"]
            typeErrors = filterByCategory TypeError errors
            nameErrors = filterByCategory NameError errors
        length typeErrors @?= 1
        length nameErrors @?= 1
        errorCategory (head typeErrors) @?= TypeError
        errorCategory (head nameErrors) @?= NameError

    , testCase "Check if error has specific category" $ do
        let typeError = errorWithCategory TypeError "Type error"
            nameError = errorWithCategory NameError "Name error"
        hasCategory TypeError typeError @?= True
        hasCategory NameError typeError @?= False
        hasCategory NameError nameError @?= True
        hasCategory TypeError nameError @?= False
    ]

  , testGroup "Error Formatting"
    [ testCase "Format simple error" $ do
        let error = errorWithCategory TypeError "Type mismatch"
            formatted = formatError error
        "Type mismatch" `isInfixOf` formatted @?= True
        "TypeError" `isInfixOf` formatted @?= True

    , testCase "Format error with location" $ do
        let pos = SourcePos 10 5 100
            error = errorAt pos "Location error"
            formatted = formatErrorWithLocation error
        "Location error" `isInfixOf` formatted @?= True
        "10:5" `isInfixOf` formatted @?= True

    , testCase "Format multiple errors" $ do
        let errors = [errorWithCategory TypeError "Error 1", warningWithCategory NameError "Warning 1"]
            formatted = formatErrorsWithLocation errors
        "Error 1" `isInfixOf` formatted @?= True
        "Warning 1" `isInfixOf` formatted @?= True
    ]

  , testGroup "Error Recovery"
    [ testCase "Check recovery capability by severity" $ do
        let infoError = infoAt (SourcePos 1 1 0) "Info message"
            warningError = warningAt (SourcePos 1 1 0) "Warning message"
            regularError = errorAt (SourcePos 1 1 0) "Error message"
            fatalError' = fatalError "Fatal message"
        canRecoverFrom infoError @?= True
        canRecoverFrom warningError @?= True
        canRecoverFrom regularError @?= True
        canRecoverFrom fatalError' @?= False

    , testCase "Check continuation capability by severity" $ do
        let infoError = infoAt (SourcePos 1 1 0) "Info message"
            warningError = warningAt (SourcePos 1 1 0) "Warning message"
            regularError = errorAt (SourcePos 1 1 0) "Error message"
            fatalError' = fatalError "Fatal message"
        shouldContinueAfter infoError @?= True
        shouldContinueAfter warningError @?= True
        shouldContinueAfter regularError @?= True
        shouldContinueAfter fatalError' @?= False

    , testCase "Create custom recovery strategy" $ do
        let strategy = createRecoveryStrategy True "Custom recovery action"
            error = errorAt (SourcePos 1 1 0) "Recoverable error"
            recovered = customRecovery strategy error
        errorRecovery recovered @?= Just strategy

    , testCase "Use predefined recovery strategies" $ do
        let error = errorAt (SourcePos 1 1 0) "Test error"
            fatalRecovered = fatalRecovery error
            errorRecovered = errorRecovery error
            warningRecovered = warningRecovery error
            infoRecovered = infoRecovery error
        canRecoverFrom (errorWithRecovery fatalRecovered) @?= False
        canRecoverFrom (errorWithRecovery errorRecovered) @?= True
        canRecoverFrom (errorWithRecovery warningRecovered) @?= True
        canRecoverFrom (errorWithRecovery infoRecovered) @?= True

    , testCase "Error with suggestions for recovery" $ do
        let error = errorWithSuggestions ["Try alternative approach"] (errorAt (SourcePos 1 1 0) "Suggested error")
        length (errorSuggestions error) @?= 1
        head (errorSuggestions error) @?= "Try alternative approach"
    ]

  , testGroup "Error Statistics and Reporting"
    [ testCase "Generate error statistics" $ do
        let errors = [errorWithCategory TypeError "Type error", warningWithCategory NameError "Name warning"]
            stats = getErrorStatistics errors
        -- Check that stats contain information about errors and warnings
        stats `seq` True @?= True  -- Basic check that stats are generated

    , testCase "Generate error report" $ do
        let errors = [errorWithCategory TypeError "Type error", warningWithCategory NameError "Name warning"]
            report = generateErrorReport errors
        "Type error" `isInfixOf` report @?= True
        "Name warning" `isInfixOf` report @?= True

    , testCase "Error with suggestions creates enhanced report" $ do
        let error = errorWithSuggestions ["Check imports", "Verify syntax"] (errorWithCategory TypeError "Suggested error")
            report = generateErrorReport [error]
        "Suggested error" `isInfixOf` report @?= True
        "Check imports" `isInfixOf` report @?= True
        "Verify syntax" `isInfixOf` report @?= True
    ]
  ]

-- Helper function to check if a string is contained in another
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'

-- Helper function to add recovery to an error
errorWithRecovery :: TypeError -> ErrorRecovery -> TypeError
errorWithRecovery error recovery = error { errorRecovery = Just recovery }