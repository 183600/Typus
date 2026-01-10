{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestErrorHandlerConsistencySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
import SourceLocation (SourcePos(..))
import qualified Data.Text as T
import TestSupport.Arbitrary ()
import Data.Time (UTCTime(..))
import Data.Time.Clock (secondsToDiffTime)

-- | Test suite for ErrorHandler consistency
testErrorHandlerConsistency :: TestTree
testErrorHandlerConsistency = testGroup "ErrorHandler Consistency Tests"
  [ testProperty "newErrorCollector: starts with no errors" $
      \() -> null (getErrors (newErrorCollector ()))
      
  , testProperty "newErrorCollector: starts with no warnings" $
      \() -> null (getWarnings (newErrorCollector ()))
      
  , testProperty "newErrorCollector: starts with no info messages" $
      \() -> null (getInfo (newErrorCollector ()))
      
  , testProperty "addError: increases error count" $
      \() err collector -> 
        let newCollector = addError err collector
        in length (getErrors newCollector) > length (getErrors collector)
        
  , testProperty "addWarning: increases warning count" $
      \() warning collector -> 
        let newCollector = addWarning warning collector
        in length (getWarnings newCollector) > length (getWarnings collector)
        
  , testProperty "addInfo: increases info count" $
      \() info collector -> 
        let newCollector = addInfo info collector
        in length (getInfo newCollector) > length (getInfo collector)
        
  , testProperty "hasErrors: true after adding error" $
      \() err collector -> 
        hasErrors (addError err collector)
        
  , testProperty "hasWarnings: true after adding warning" $
      \() warning collector -> 
        hasWarnings (addWarning warning collector)
        
  , testProperty "errorAt: creates error with correct location" $
      \pos message -> 
        let err = errorAt pos message
        in line (errorLocation err) == posLine pos && 
           column (errorLocation err) == posColumn pos
           
  , testProperty "warningAt: creates warning with correct location" $
      \pos message -> 
        let warning = warningAt pos message
        in line (errorLocation warning) == posLine pos && 
           column (errorLocation warning) == posColumn pos
           
  , testProperty "errorWithCategory: preserves category" $
      \category message -> 
        errorCategory (errorWithCategory category message) == category
        
  , testProperty "warningWithCategory: preserves category" $
      \category message -> 
        errorCategory (warningWithCategory category message) == category
        
  , testProperty "combineErrors: preserves all errors" $
      \err1 err2 -> 
        let combined = combineErrors err1 err2
        in combinedErrors combined `shouldContain` [err1, err2]
        
  , testProperty "combinedErrorSeverity: returns maximum severity" $
      \err1 err2 -> 
        let combined = combineErrors err1 err2
            severity = combinedErrorSeverity combined
        in severity == max (errorSeverity err1) (errorSeverity err2)
        
  , testProperty "filterBySeverity: only returns errors with matching severity" $
      \severity errors -> 
        let filtered = filterBySeverity severity errors
        in all (\e -> errorSeverity e == severity) filtered
        
  , testProperty "filterByCategory: only returns errors with matching category" $
      \category errors -> 
        let filtered = filterByCategory category errors
        in all (\e -> errorCategory e == category) filtered
        
  , testProperty "isAtLeast: true for same or higher severity" $
      \severity1 severity2 -> 
        isAtLeast severity1 severity2 == (severity1 >= severity2)
        
  , testProperty "severityPriority: maintains consistent ordering" $
      \severity1 severity2 -> 
        compareSeverity severity1 severity2 == compare (severityPriority severity1) (severityPriority severity2)
        
  , testProperty "canRecoverFrom: Error severity cannot recover" $
      \() -> not (canRecoverFrom Error)
      
  , testProperty "canRecoverFrom: Info severity can recover" $
      \() -> canRecoverFrom Info
      
  , testProperty "shouldContinueAfter: Error severity should not continue" $
      \() -> not (shouldContinueAfter Error)
      
  , testProperty "shouldContinueAfter: Warning severity should continue" $
      \() -> shouldContinueAfter Warning
      
  , testProperty "withLocation: updates error location" $
      \pos err -> 
        let newErr = withLocation pos err
        in errorLocation newErr == toErrorLocation pos
        
  , testProperty "withContext: adds context to error" $
      \context err -> 
        let newErr = withContext context err
        in errorContext newErr == context
        
  , testProperty "withSuggestions: adds suggestions to error" $
      \suggestions err -> 
        let newErr = withSuggestions suggestions err
        in errorSuggestions newErr == suggestions
        
  , testCase "formatError: produces non-empty string" $
      let err = errorAt (SourcePos 1 1 0) "Test error message"
          formatted = formatError err
      in length formatted > 0
      
  , testCase "formatErrors: formats multiple errors" $
      let err1 = errorAt (SourcePos 1 1 0) "First error"
          err2 = errorAt (SourcePos 2 1 0) "Second error"
          formatted = formatErrors [err1, err2]
      in length formatted > length (formatError err1)
      
  , testCase "generateErrorReport: includes statistics" $
      let collector = newErrorCollector ()
          collector' = addError (errorAt (SourcePos 1 1 0) "Error") collector
          collector'' = addWarning (warningAt (SourcePos 2 1 0) "Warning") collector'
          report = generateErrorReport collector''
      in "errors" `isInfixOf` report && "warnings" `isInfixOf` report
      
  , testCase "wrapError: preserves original error" $
      let originalErr = errorAt (SourcePos 1 1 0) "Original error"
          wrappedErr = wrapError "Context" originalErr
      in "Original error" `isInfixOf` errorMessage wrappedErr
  ]

-- Helper functions
shouldContain :: (Show a, Eq a) => [a] -> [a] -> Bool
shouldContain list elements = all (`elem` list) elements

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]