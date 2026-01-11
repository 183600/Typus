{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestErrorHandlerConsistencySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Compiler.Errors.Core as Error
import SourceLocation (SourcePos(..))
import qualified Data.Text as T
import TestSupport.Arbitrary ()
import Data.Time (UTCTime(..))
import Data.Time.Clock (secondsToDiffTime)
import Control.Monad.State

-- | Test suite for ErrorHandler consistency
testErrorHandlerConsistency :: TestTree
testErrorHandlerConsistency = testGroup "ErrorHandler Consistency Tests"
  [ testProperty "newErrorCollector: starts with no errors" $
      \() -> null (Error.getErrors (Error.newErrorCollector ()))
      
  , testProperty "newErrorCollector: starts with no warnings" $
      \() -> null (Error.getWarnings (Error.newErrorCollector ()))
      
  , testProperty "newErrorCollector: starts with no info messages" $
      \() -> null (Error.getInfo (Error.newErrorCollector ()))
      
  , testProperty "addError: increases error count" $
      \() err collector -> 
        let newCollector = execState (Error.addError err) collector
        in length (Error.getErrors newCollector) > length (Error.getErrors collector)
        
  , testProperty "addWarning: increases warning count" $
      \() warning collector -> 
        let newCollector = execState (Error.addWarning warning) collector
        in length (Error.getWarnings newCollector) > length (Error.getWarnings collector)
        
  , testProperty "addInfo: increases info count" $
      \() info collector -> 
        let newCollector = execState (Error.addInfo info) collector
        in length (Error.getInfo newCollector) > length (Error.getInfo collector)
        
  , testProperty "hasErrors: true after adding error" $
      \() err collector -> 
        Error.hasErrors (execState (Error.addError err) collector)
        
  , testProperty "hasWarnings: true after adding warning" $
      \() warning collector -> 
        Error.hasWarnings (execState (Error.addWarning warning) collector)
        
  , testProperty "errorAt: creates error with correct location" $
      \(pos :: SourcePos) message -> 
        let err = Error.errorAt "test" (T.pack message) (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
        in Error.line (Error.location err) == 1 && 
           Error.column (Error.location err) == 1
           
  , testProperty "warningAt: creates warning with correct location" $
      \(pos :: SourcePos) message -> 
        let warning = Error.warningAt "test" (T.pack message) (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
        in Error.line (Error.location warning) == 1 && 
           Error.column (Error.location warning) == 1
           
  , testProperty "errorAt with suggestions includes suggestions in formatted output" $
      \pos message suggestions -> 
        let err = Error.errorAt "test" (T.pack message) (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
            errWithSuggestions = Error.withSuggestions suggestions err
            formatted = Error.formatError errWithSuggestions
        in all (`elem` formatted) suggestions
        
  , testCase "Single error is formatted correctly" $
      let err = Error.errorAt "1:1" "Test error message" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          formatted = Error.formatError err
      in "1:1: Test error message" `isInfixOf` formatted
      
  , testCase "Multiple errors are formatted correctly" $ do
      let err1 = Error.errorAt "1:1" "First error" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          err2 = Error.errorAt "2:1" "Second error" (Error.ErrorLocation Nothing 2 1 Nothing Nothing)
          formatted = Error.formatErrors [err1, err2]
      in do
        "1:1: First error" `isInfixOf` formatted
        "2:1: Second error" `isInfixOf` formatted
      
  , testCase "Error with suggestions includes suggestions in formatted output" $ do
      let err = Error.errorAt "1:1" "Test error" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          errWithSuggestions = Error.withSuggestions ["Try adding a type annotation"] err
          formatted = Error.formatError errWithSuggestions
      in "Suggestions: Try adding a type annotation" `isInfixOf` formatted
      
  , testCase "Warning is formatted correctly" $
      let warning = Error.warningAt "1:1" "Test warning message" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          formatted = Error.formatError warning
      in "Warning: Test warning message" `isInfixOf` formatted
      
  , testCase "Error severity affects recovery behavior" $ do
      let collector = Error.newErrorCollector ()
          fatalError = Error.errorAt "1:1" "Fatal error" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          collector' = execState (Error.addError fatalError) collector
      in Error.canRecoverFrom Error.Error @?= False && 
         Error.shouldContinueAfter Error.Error @?= False
         
  , testCase "Warning severity affects recovery behavior" $ do
      let collector = Error.newErrorCollector ()
          warning = Error.warningAt "1:1" "Warning message" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          collector' = execState (Error.addWarning warning) collector
      in Error.canRecoverFrom Error.Warning @?= True && 
         Error.shouldContinueAfter Error.Warning @?= True
         
  , testCase "Info severity affects recovery behavior" $ do
      let collector = Error.newErrorCollector ()
          info = Error.infoAt "1:1" "Info message" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          collector' = execState (Error.addInfo info) collector
      in Error.canRecoverFrom Error.Info @?= True && 
         Error.shouldContinueAfter Error.Info @?= True
         
  , testCase "Error report includes all errors" $ do
      let collector = Error.newErrorCollector ()
          err1 = Error.errorAt "1:1" "First error" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          err2 = Error.errorAt "2:1" "Second error" (Error.ErrorLocation Nothing 2 1 Nothing Nothing)
          err3 = Error.errorAt "3:1" "Third error" (Error.ErrorLocation Nothing 3 1 Nothing Nothing)
          collector' = execState (Error.addError err1) (execState (Error.addError err2) (execState (Error.addError err3) collector))
          report = Error.generateErrorReport collector'
      in "First error" `isInfixOf` report && 
         "Second error" `isInfixOf` report && 
         "Third error" `isInfixOf` report
         
  , testCase "Error report includes warnings when present" $ do
      let collector = Error.newErrorCollector ()
          err = Error.errorAt "1:1" "Error" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          warning = Error.warningAt "2:1" "Warning" (Error.ErrorLocation Nothing 2 1 Nothing Nothing)
          collector' = execState (Error.addError err) (execState (Error.addWarning warning) collector)
          report = Error.generateErrorReport collector'
      in "Error" `isInfixOf` report && "Warning" `isInfixOf` report
         
  , testCase "Error report includes info when present" $ do
      let collector = Error.newErrorCollector ()
          err = Error.errorAt "1:1" "Error" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          info = Error.infoAt "2:1" "Info" (Error.ErrorLocation Nothing 2 1 Nothing Nothing)
          collector' = execState (Error.addError err) (execState (Error.addInfo info) collector)
          report = Error.generateErrorReport collector'
      in "Error" `isInfixOf` report && "Info" `isInfixOf` report
  ]

-- Helper function
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]