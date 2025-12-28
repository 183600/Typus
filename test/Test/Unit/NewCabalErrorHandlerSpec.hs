module Test.Unit.NewCabalErrorHandlerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck ((===), Property, counterexample)

import TestSupport.QuickCheck (fastProperty)
import ErrorHandler
  ( ErrorHandler(..)
  , ErrorContext(..)
  , ErrorSeverity(..)
  , ErrorMessage(..)
  , defaultErrorHandler
  , handleError
  , formatError
  , collectErrors
  )
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Compiler.Errors.Core (ErrorLocation(..))
import Data.List (isPrefixOf, isInfixOf)

-- | Additional comprehensive tests for ErrorHandler module
tests :: TestTree
tests =
  testGroup "NewCabal ErrorHandler Tests"
    [ testGroup "Error creation and formatting"
        [ testCase "formats simple error message correctly" $ do
            let pos = SourcePos 5 10 45
                span = SourceSpan pos pos
                context = ErrorContext "parsing" span Nothing
                message = ErrorMessage "syntax error" ErrorError context
                formatted = formatError message
            assertBool "should contain line number" $ "line 5" `isInfixOf` formatted
            assertBool "should contain column number" $ "column 10" `isInfixOf` formatted
            assertBool "should contain error message" $ "syntax error" `isInfixOf` formatted

        , testCase "formats warning message with correct severity" $ do
            let pos = SourcePos 2 3 8
                span = SourceSpan pos pos
                context = ErrorContext "type checking" span Nothing
                message = ErrorMessage "unused variable" ErrorWarning context
                formatted = formatError message
            assertBool "should indicate warning" $ "warning" `isInfixOf` formatted
            assertBool "should contain message" $ "unused variable" `isInfixOf` formatted

        , testCase "formats multi-line span correctly" $ do
            let start = SourcePos 1 5 4
                end = SourcePos 3 2 20
                span = SourceSpan start end
                context = ErrorContext "compilation" span Nothing
                message = ErrorMessage "multi-line error" ErrorError context
                formatted = formatError message
            assertBool "should contain start line" $ "line 1" `isInfixOf` formatted
            assertBool "should contain end line" $ "line 3" `isInfixOf` formatted
        ]

    , testGroup "Error handling workflow"
        [ testCase "handles single error correctly" $ do
            let handler = defaultErrorHandler
                pos = SourcePos 1 1 0
                span = SourceSpan pos pos
                context = ErrorContext "test" span Nothing
                message = ErrorMessage "test error" ErrorError context
                result = handleError handler message
            assertBool "should handle error" $ result
            let errors = collectErrors handler
            assertBool "should have one error" $ length errors == 1
            assertBool "error should be preserved" $ 
              case errors of
                (err:_) -> emMessage err == "test error"
                [] -> False

        , testCase "handles multiple errors in sequence" $ do
            let handler = defaultErrorHandler
                createError msg line = ErrorMessage msg ErrorError (ErrorContext "test" (SourceSpan (SourcePos line 1 0) (SourcePos line 1 5)) Nothing)
                errors = [createError "error1" 1, createError "error2" 2, createError "error3" 3]
                results = map (handleError handler) errors
            assertBool "all errors should be handled" $ and results
            let collected = collectErrors handler
            assertBool "should have three errors" $ length collected == 3
            assertBool "errors should be in order" $ 
              map emMessage collected == ["error1", "error2", "error3"]
        ]

    , testGroup "Error context management"
        [ testCase "preserves error context information" $ do
            let pos = SourcePos 10 15 100
                span = SourceSpan pos pos
                additionalInfo = Just "additional context"
                context = ErrorContext "validation" span additionalInfo
                message = ErrorMessage "validation failed" ErrorError context
                formatted = formatError message
            assertBool "should contain context type" $ "validation" `isInfixOf` formatted
            assertBool "should contain additional info" $ 
              case additionalInfo of
                Just info -> info `isInfixOf` formatted
                Nothing -> True
        ]

    , testGroup "Error severity handling"
        [ testCase "distinguishes between error and warning severity" $ do
            let errorMsg = ErrorMessage "error" ErrorError (ErrorContext "test" (SourceSpan startPos startPos) Nothing)
                warningMsg = ErrorMessage "warning" ErrorWarning (ErrorContext "test" (SourceSpan startPos startPos) Nothing)
                errorFormatted = formatError errorMsg
                warningFormatted = formatError warningMsg
            assertBool "error should be marked as error" $ 
              "error" `isInfixOf` errorFormatted && not ("warning" `isInfixOf` errorFormatted)
            assertBool "warning should be marked as warning" $ 
              "warning" `isInfixOf` warningFormatted
        ]

    , testGroup "QuickCheck property tests"
        [ fastProperty "error formatting contains location information" prop_errorFormatContainsLocation
        , fastProperty "collected errors preserve order" prop_collectedErrorsPreserveOrder
        , fastProperty "error context is preserved in formatting" prop_errorContextPreserved
        , fastProperty "error severity affects formatting" prop_errorSeverityAffectsFormatting
        ]
    ]

-- Property: error formatting should always contain location information
prop_errorFormatContainsLocation :: String -> ErrorSeverity -> Int -> Int -> Property
prop_errorFormatContainsLocation msg severity line col =
  let pos = SourcePos (abs line `mod` 100 + 1) (abs col `mod` 100 + 1) (abs line + abs col)
      span = SourceSpan pos pos
      context = ErrorContext "test" span Nothing
      message = ErrorMessage msg severity context
      formatted = formatError message
  in counterexample ("formatted: " ++ formatted) $
     show line `isInfixOf` formatted && show col `isInfixOf` formatted

-- Property: collected errors should preserve the order they were added
prop_collectedErrorsPreserveOrder :: [String] -> Property
prop_collectedErrorsPreserveOrder msgs =
  let handler = defaultErrorHandler
        createError msg line = ErrorMessage msg ErrorError (ErrorContext "test" (SourceSpan line 1 0) (SourcePos line 5 4)) Nothing
        errors = zipWith createError (take 10 msgs) [1..]
        results = map (handleError handler) errors
  in counterexample ("results: " ++ show results) $
     and results ==> 
     let collected = collectErrors handler
         actualMessages = map emMessage (take (length errors) collected)
     in actualMessages === take (length errors) msgs

-- Property: error context should be preserved in formatted output
prop_errorContextPreserved :: String -> String -> Property
prop_errorContextPreserved contextType additionalInfo =
  let pos = SourcePos 1 1 0
      span = SourceSpan pos pos
      context = ErrorContext contextType span (if null additionalInfo then Nothing else Just additionalInfo)
      message = ErrorMessage "test message" ErrorError context
      formatted = formatError message
  in counterexample ("formatted: " ++ formatted) $
     contextType `isInfixOf` formatted &&
     (if null additionalInfo then True else additionalInfo `isInfixOf` formatted)

-- Property: error severity should affect formatting output
prop_errorSeverityAffectsFormatting :: String -> ErrorSeverity -> Property
prop_errorSeverityAffectsFormatting msg severity =
  let pos = SourcePos 1 1 0
      span = SourceSpan pos pos
      context = ErrorContext "test" span Nothing
      message = ErrorMessage msg severity context
      formatted = formatError message
  in counterexample ("formatted: " ++ formatted) $
     case severity of
       ErrorError -> "error" `isInfixOf` formatted
       ErrorWarning -> "warning" `isInfixOf` formatted
       ErrorInfo -> "info" `isInfixOf` formatted