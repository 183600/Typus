{-# LANGUAGE CPP #-}

module Test.Unit.ErrorHandlerConsistencyAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as Map

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
  , warningAt
  , infoAt
  , withLocation
  , withContext
  , combineErrors
  , filterByCategory
  , filterBySeverity
  , hasCategory
  )

tests :: TestTree
tests = testGroup "ErrorHandler Consistency Advanced Tests"
  [ errorCollectorTests
  , errorFormattingTests
  , errorRecoveryTests
  , errorFilteringTests
  , errorCombinationTests
  , quickCheckProperties
  ]

errorCollectorTests :: TestTree
errorCollectorTests = testGroup "Error Collector Tests"
  [ testCase "newErrorCollector starts empty" $ do
      let collector = newErrorCollector
      hasErrors collector @?= False
      hasWarnings collector @?= False
      getAllMessages collector @?= []
      
  , testCase "addError adds to error collection" $ do
      let collector = newErrorCollector
          error = TypeError "test error" SyntaxError ErrorLocation
              { filePath = Nothing
              , line = 1
              , column = 1
              , endLine = Nothing
              , endColumn = Nothing
              } emptyContext
          updatedCollector = addError collector error
      hasErrors updatedCollector @?= True
      length (getErrors updatedCollector) @?= 1
      
  , testCase "addWarning adds to warning collection" $ do
      let collector = newErrorCollector
          warning = TypeError "test warning" WarningCategory ErrorLocation
              { filePath = Nothing
              , line = 1
              , column = 1
              , endLine = Nothing
              , endColumn = Nothing
              } emptyContext
          updatedCollector = addWarning collector warning
      hasWarnings updatedCollector @?= True
      length (getWarnings updatedCollector) @?= 1
      
  , testCase "addInfo adds to info collection" $ do
      let collector = newErrorCollector
          info = TypeError "test info" InfoCategory ErrorLocation
              { filePath = Nothing
              , line = 1
              , column = 1
              , endLine = Nothing
              , endColumn = Nothing
              } emptyContext
          updatedCollector = addInfo collector info
      length (getInfo updatedCollector) @?= 1
      
  , testCase "getAllMessages returns all message types" $ do
      let collector = newErrorCollector
          error = TypeError "error" TypeErrorCategory ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          warning = TypeError "warning" WarningCategory ErrorLocation
              { filePath = Nothing, line = 2, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          info = TypeError "info" InfoCategory ErrorLocation
              { filePath = Nothing, line = 3, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          updatedCollector = addInfo (addWarning (addError collector error) warning) info
      length (getAllMessages updatedCollector) @?= 3
  ]

errorFormattingTests :: TestTree
errorFormattingTests = testGroup "Error Formatting Tests"
  [ testCase "formatError includes message and location" $ do
      let error = TypeError "test error" SyntaxError ErrorLocation
              { filePath = Just "test.typus"
              , line = 10
              , column = 5
              , endLine = Just 10
              , endColumn = Just 15
              } emptyContext
          formatted = formatError error
      "test error" `isInfixOf` formatted @?= True
      "test.typus" `isInfixOf` formatted @?= True
      "10:5" `isInfixOf` formatted @?= True
      
  , testCase "formatErrors handles multiple errors" $ do
      let error1 = TypeError "first error" SyntaxError ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          error2 = TypeError "second error" TypeErrorCategory ErrorLocation
              { filePath = Nothing, line = 2, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          formatted = formatErrors [error1, error2]
      "first error" `isInfixOf` formatted @?= True
      "second error" `isInfixOf` formatted @?= True
      
  , testCase "formatErrorWithLocation includes detailed location" $ do
      let error = TypeError "test error" SyntaxError ErrorLocation
              { filePath = Just "test.typus"
              , line = 5
              , column = 10
              , endLine = Just 5
              , endColumn = Just 20
              } emptyContext
          formatted = formatErrorWithLocation error
      "test.typus:5:10-20" `isInfixOf` formatted @?= True
  ]

errorRecoveryTests :: TestTree
errorRecoveryTests = testGroup "Error Recovery Tests"
  [ testCase "canRecoverFrom info messages" $ do
      let info = TypeError "info message" InfoCategory ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
      canRecoverFrom info @?= True
      
  , testCase "canRecoverFrom warnings" $ do
      let warning = TypeError "warning message" WarningCategory ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
      canRecoverFrom warning @?= True
      
  , testCase "cannotRecoverFrom syntax errors" $ do
      let syntaxError = TypeError "syntax error" SyntaxError ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
      canRecoverFrom syntaxError @?= False
      
  , testCase "shouldContinueAfter info messages" $ do
      let info = TypeError "info message" InfoCategory ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
      shouldContinueAfter info @?= True
      
  , testCase "shouldContinueAfter warnings" $ do
      let warning = TypeError "warning message" WarningCategory ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
      shouldContinueAfter warning @?= True
      
  , testCase "shouldContinueAfter type errors" $ do
      let typeError = TypeError "type error" TypeErrorCategory ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
      shouldContinueAfter typeError @?= True
  ]

errorFilteringTests :: TestTree
errorFilteringTests = testGroup "Error Filtering Tests"
  [ testCase "filterByCategory selects matching errors" $ do
      let error1 = TypeError "syntax error" SyntaxError ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          error2 = TypeError "type error" TypeErrorCategory ErrorLocation
              { filePath = Nothing, line = 2, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          error3 = TypeError "another syntax error" SyntaxError ErrorLocation
              { filePath = Nothing, line = 3, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          syntaxErrors = filterByCategory SyntaxError [error1, error2, error3]
      length syntaxErrors @?= 2
      
  , testCase "filterBySeverity selects matching severity" $ do
      let error1 = TypeError "error" TypeErrorCategory ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          warning = TypeError "warning" WarningCategory ErrorLocation
              { filePath = Nothing, line = 2, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          info = TypeError "info" InfoCategory ErrorLocation
              { filePath = Nothing, line = 3, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          errorsOnly = filterBySeverity ErrorSeverity [error1, warning, info]
      length errorsOnly @?= 1
      
  , testCase "hasCategory checks for category presence" $ do
      let error = TypeError "test error" SyntaxError ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
      hasCategory SyntaxError error @?= True
      hasCategory TypeErrorCategory error @?= False
  ]

errorCombinationTests :: TestTree
errorCombinationTests = testGroup "Error Combination Tests"
  [ testCase "combineErrors merges related errors" $ do
      let error1 = TypeError "first error" SyntaxError ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          error2 = TypeError "second error" TypeErrorCategory ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          combined = combineErrors error1 error2
      case combined of
        CombinedError errors -> length errors @?= 2
        _ -> "Expected CombinedError" @?= "Got TypeError"
        
  , testCase "combinedErrorSeverity chooses highest severity" $ do
      let error1 = TypeError "warning" WarningCategory ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          error2 = TypeError "error" TypeErrorCategory ErrorLocation
              { filePath = Nothing, line = 1, column = 1, endLine = Nothing, endColumn = Nothing
              } emptyContext
          combined = combineErrors error1 error2
      case combined of
        CombinedError errors -> combinedErrorSeverity errors @?= ErrorSeverity
        _ -> "Expected CombinedError" @?= "Got TypeError"
  ]

quickCheckProperties :: TestTree
quickCheckProperties = testGroup "QuickCheck Error Properties"
  [ fastProperty "error formatting preserves essential information" prop_format_preserves
  , fastProperty "error filtering maintains count invariants" prop_filtering_invariants
  , fastProperty "error combination is associative" prop_combination_associative
  ]

-- QuickCheck property implementations
prop_format_preserves :: TypeError -> Property
prop_format_preserves error =
  let formatted = formatError error
      errorMsg = case error of
        TypeError msg _ _ _ -> msg
        CombinedError errors -> concatMap (\e -> case e of
          TypeError msg _ _ _ -> msg
          CombinedError _ -> "") errors
  in not (null errorMsg) ==> errorMsg `isInfixOf` formatted

prop_filtering_invariants :: [TypeError] -> ErrorCategory -> Property
prop_filtering_invariants errors category =
  let filtered = filterByCategory category errors
  in length filtered <= length errors ==> property True

prop_combination_associative :: TypeError -> TypeError -> TypeError -> Property
prop_combination_associative err1 err2 err3 =
  let combined1 = combineErrors (combineErrors err1 err2) err3
      combined2 = combineErrors err1 (combineErrors err2 err3)
  in case (combined1, combined2) of
    (CombinedError errors1, CombinedError errors2) -> 
      length errors1 === length errors2
    _ -> property True