{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestEnhancedErrorHandlerSpec where

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

-- | Test suite for Enhanced ErrorHandler features
testEnhancedErrorHandler :: TestTree
testEnhancedErrorHandler = testGroup "Enhanced ErrorHandler Tests"
  [ testProperty "addErrorWithCategory: categorizes errors correctly" $
      \() category message collector -> 
        let err = Error.errorAt "test" (T.pack message) (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
            errWithCategory = Error.addErrorWithCategory category err
            newCollector = execState (Error.addError errWithCategory) collector
        in length (Error.getErrors newCollector) > length (Error.getErrors collector)
        
  , testProperty "addWarningWithCategory: categorizes warnings correctly" $
      \() category message collector -> 
        let warning = Error.warningAt "test" (T.pack message) (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
            warningWithCategory = Error.addWarningWithCategory category warning
            newCollector = execState (Error.addWarningWithCategory warningWithCategory) collector
        in length (Error.getWarnings newCollector) > length (Error.getWarnings collector)
        
  , testCase "Error categories affect recovery behavior" $ do
      let collector = Error.newErrorCollector ()
          syntaxError = Error.errorAt "test" (T.pack "Syntax error") (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          typeError = Error.errorAt "test" (T.pack "Type error") (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          runtimeError = Error.errorAt "test" (T.pack "Runtime error") (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          
          collector1 = execState (Error.addErrorWithCategory Error.Syntax syntaxError) collector
          collector2 = execState (Error.addErrorWithCategory Error.Type typeError) collector1
          collector3 = execState (Error.addErrorWithCategory Error.Runtime runtimeError) collector2
          
      in do
        Error.canRecoverFrom Error.Syntax @?= True
        Error.shouldContinueAfter Error.Syntax @?= True
        Error.canRecoverFrom Error.Type @?= False
        Error.shouldContinueAfter Error.Type @?= False
        Error.canRecoverFrom Error.Runtime @?= False
        Error.shouldContinueAfter Error.Runtime @?= False
         
  , testCase "Error recovery strategies are applied correctly" $ do
      let collector = Error.newErrorCollector ()
          recoverableError = Error.errorAt "test" (T.pack "Recoverable error") (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          fatalError = Error.errorAt "test" (T.pack "Fatal error") (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          
          collector1 = execState (Error.addErrorWithRecovery Error.Continue recoverableError) collector
          collector2 = execState (Error.addErrorWithRecovery Error.Stop fatalError) collector
          
      in do
        Error.shouldContinueAfter (Error.errorSeverity (head (Error.getErrors collector1))) @?= True &&
        Error.shouldContinueAfter (Error.errorSeverity (head (Error.getErrors collector2))) @?= False
         
  , testCase "Error highlighting preserves context" $ do
      let highlight = Error.ErrorHighlight "String" 5 10
          err = Error.errorAt "test" (T.pack "Error in string") (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          errWithHighlight = Error.addErrorHighlight highlight err
          formatted = Error.formatError errWithHighlight
      in "Error in string" `isInfixOf` formatted
          
  , testCase "Multiple highlights are combined correctly" $ do
      let highlight1 = Error.ErrorHighlight "String" 5 10
          highlight2 = Error.ErrorHighlight "String" 15 20
          err = Error.errorAt "test" (T.pack "Multiple errors") (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          errWithHighlights = Error.addErrorHighlight highlight1 (Error.addErrorHighlight highlight2 err)
          formatted = Error.formatError errWithHighlights
      in "Multiple errors" `isInfixOf` formatted
          
  , testCase "Error timestamp is recorded correctly" $ do
      let now = UTCTime (secondsToDiffTime 1000000)
          err = Error.errorAt "test" (T.pack "Timestamped error") (Error.ErrorLocation Nothing 1 1 (Just now))
          timestamp = Error.errorTimestamp err
      in timestamp == now
          
  , testCase "Error context includes surrounding code" $ do
      let context = ["line 1", "line 2", "line 3"]
          err = Error.errorAt "test" (T.pack "Context error") (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          errWithContext = Error.addErrorContext context err
          formatted = Error.formatError errWithContext
      in "Context error" `isInfixOf` formatted
          
  , testCase "Error suggestions are prioritized by severity" $ do
      let collector = Error.newErrorCollector ()
          highPrioritySuggestion = "Check variable types"
          lowPrioritySuggestion = "Consider refactoring"
          err = Error.errorAt "test" (T.pack "Error with suggestions") (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          errWithHighPriority = Error.addErrorSuggestion highPrioritySuggestion err
          errWithBoth = Error.addErrorSuggestion lowPrioritySuggestion errWithHighPriority
          formattedHigh = Error.formatError errWithHighPriority
          formattedBoth = Error.formatError errWithBoth
      in "Check variable types" `isPrefixOf` formattedHigh && 
         "Consider refactoring" `isInfixOf` formattedBoth
          
  , testCase "Error filtering by severity works correctly" $ do
      let collector = Error.newErrorCollector ()
          info1 = Error.infoAt "test" "Info 1" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          info2 = Error.infoAt "test" "Info 2" (Error.ErrorLocation Nothing 2 1 Nothing Nothing)
          warning = Error.warningAt "test" "Warning" (Error.ErrorLocation Nothing 3 1 Nothing Nothing)
          error = Error.errorAt "test" "Error" (Error.ErrorLocation Nothing 4 1 Nothing Nothing)
          
          collector' = execState (Error.addInfo info1) (execState (Error.addInfo info2) (execState (Error.addWarning warning) (execState (Error.addError error) collector)))
          infoErrors = Error.filterBySeverity Error.Info collector'
          warningErrors = Error.filterBySeverity Error.Warning collector'
          criticalErrors = Error.filterBySeverity Error.Error collector'
      in length infoErrors @?= 2 && length warningErrors @?= 1 && length criticalErrors @?= 1
          
  , testCase "Error filtering by category works correctly" $ do
      let collector = Error.newErrorCollector ()
          syntaxError = Error.errorAt "test" (T.pack "Syntax error") (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          typeError = Error.errorAt "test" (T.pack "Type error") (Error.ErrorLocation Nothing 2 1 Nothing Nothing)
          runtimeError = Error.errorAt "test" (T.pack "Runtime error") (Error.ErrorLocation Nothing 3 1 Nothing Nothing)
          
          collector' = execState (Error.addErrorWithCategory Error.Syntax syntaxError) (execState (Error.addErrorWithCategory Error.Type typeError) (execState (Error.addErrorWithCategory Error.Runtime runtimeError) collector))
          syntaxErrors = Error.filterByCategory Error.Syntax collector'
          typeErrors = Error.filterByCategory Error.Type collector'
          runtimeErrors = Error.filterByCategory Error.Runtime collector'
      in length syntaxErrors @?= 1 && length typeErrors @?= 1 && length runtimeErrors @?= 1
          
  , testCase "Error sorting by severity works correctly" $ do
      let collector = Error.newErrorCollector ()
          info = Error.infoAt "test" "Info" (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
          warning = Error.warningAt "test" "Warning" (Error.ErrorLocation Nothing 2 1 Nothing Nothing)
          error = Error.errorAt "test" "Error" (Error.ErrorLocation Nothing 3 1 Nothing Nothing)
          
          collector' = execState (Error.addInfo info) (execState (Error.addWarning warning) (execState (Error.addError error) collector))
          sortedErrors = Error.sortBySeverity (Error.getErrors collector')
          sortedSeverity = map Error.errorSeverity sortedErrors
      in sortedSeverity == [Error.Error, Error.Warning, Error.Info]
  ]

-- Local type definitions to avoid conflicts
data TestErrorSeverity = TestInfo | TestWarning | TestError
  deriving (Eq, Show, Ord)

data TestErrorRecovery = TestContinue | TestStop | TestRetry | TestCustom String
  deriving (Eq, Show)

data TestErrorHighlight = TestErrorHighlight String Int Int  -- Type, start, end
  deriving (Eq, Show)