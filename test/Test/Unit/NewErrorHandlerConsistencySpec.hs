{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorHandlerConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
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
  , canRecoverFrom
  , shouldContinueAfter
  , errorAt
  , errorAtWithTimestamp
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
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

import qualified Data.Map.Strict as Map
import Data.List (sort, nub, find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.State (runState, get, put)

-- | Error handler consistency tests
tests :: TestTree
tests =
  testGroup "New Error Handler Consistency Tests"
    [ testGroup "Error severity consistency"
        [ testCase "severity ordering is consistent" $ do
            let errors = 
                  [ errorAt Info 1 1 "info message"
                  , errorAt Warning 1 1 "warning message"
                  , errorAt Error 1 1 "error message"
                  , fatalError 1 1 "fatal message"
                  ]
                sortedBySeverity = sort errors
            -- Verify that sorting respects severity ordering
            map severity sortedBySeverity @?= [Info, Warning, Error, Fatal]
            
        , testCase "recovery strategy matches severity" $ do
            let infoError = errorAt Info 1 1 "info"
                warningError = errorAt Warning 1 1 "warning"
                regularError = errorAt Error 1 1 "error"
                fatalError' = fatalError 1 1 "fatal"
                
                canRecoverInfo = canRecoverFrom infoError
                canRecoverWarning = canRecoverFrom warningError
                canRecoverError = canRecoverFrom regularError
                canRecoverFatal = canRecoverFrom fatalError'
                
                shouldContinueInfo = shouldContinueAfter infoError
                shouldContinueWarning = shouldContinueAfter warningError
                shouldContinueError = shouldContinueAfter regularError
                shouldContinueFatal = shouldContinueAfter fatalError'
                
            -- Fatal errors should not be recoverable
            canRecoverFatal @?= False
            shouldContinueFatal @?= False
            
            -- Other errors should be recoverable
            canRecoverInfo @?= True
            canRecoverWarning @?= True
            canRecoverError @?= True
            
            shouldContinueInfo @?= True
            shouldContinueWarning @?= True
            shouldContinueError @?= True
        ]
        
    , testGroup "Error location consistency"
        [ testCase "location information is preserved in formatting" $ do
            let location = ErrorLocation (Just "test.typus") 10 5 (Just 10) (Just 15)
                error = errorAt Error 10 5 "test error"
                    { location = location }
                formatted = formatError error
            -- Formatted error should contain file path and line numbers
            "test.typus" `T.isInfixOf` formatted @?= True
            "10:5" `T.isInfixOf` formatted @?= True
            
        , testCase "location ranges are handled correctly" $ do
            let startLocation = ErrorLocation (Just "test.typus") 5 1 (Just 5) (Just 10)
                endLocation = ErrorLocation (Just "test.typus") 8 20 (Just 8) (Just 25)
                error1 = errorAt Error 5 1 "error at start"
                    { location = startLocation }
                error2 = errorAt Error 8 20 "error at end"
                    { location = endLocation }
                    
            -- Both errors should have valid locations
            line (location error1) @?= 5
            column (location error1) @?= 1
            line (location error2) @?= 8
            column (location error2) @?= 20
        ]
        
    , testGroup "Error context consistency"
        [ testCase "context information is preserved through operations" $ do
            let context = emptyContext 
                    { contextFunction = Just "testFunction"
                    , contextVariable = Just "testVar"
                    , contextType = Just "String"
                    }
                baseError = errorAt Error 1 1 "base error"
                contextError = withContext context baseError
                
                finalContext = context contextError
                finalFunction = contextFunction finalContext
                finalVariable = contextVariable finalContext
                finalType = contextType finalContext
                
            finalFunction @?= Just "testFunction"
            finalVariable @?= Just "testVar"
            finalType @?= Just "String"
            
        , testCase "context merging works correctly" $ do
            let context1 = emptyContext { contextFunction = Just "func1" }
                context2 = emptyContext { contextVariable = Just "var1" }
                error1 = errorAt Error 1 1 "error1"
                    { context = context1 }
                error2 = withContext context2 error1
                
                mergedContext = context error2
                contextFunction mergedContext @?= Just "func1"
                contextVariable mergedContext @?= Just "var1"
        ]
        
    , testGroup "Error collection consistency"
        [ testCase "error collector maintains separation by severity" $ do
            let collector = newErrorCollector
                (collector1, _) = runState (addError $ errorAt Error 1 1 "error") collector
                (collector2, _) = runState (addWarning $ warningAt Warning 1 1 "warning") collector1
                (collector3, _) = runState (addInfo $ infoAt Info 1 1 "info") collector2
                
                errors = getErrors collector3
                warnings = getWarnings collector3
                infos = getInfo collector3
                
            length errors @?= 1
            length warnings @?= 1
            length infos @?= 1
            
            severity (head errors) @?= Error
            severity (head warnings) @?= Warning
            severity (head infos) @?= Info
            
        , testCase "error statistics are consistent" $ do
            let collector = newErrorCollector
                actions = [ addError $ errorAt Error 1 1 "error1"
                          , addError $ errorAt Error 2 1 "error2"
                          , addWarning $ warningAt Warning 1 1 "warning1"
                          , addWarning $ warningAt Warning 2 1 "warning2"
                          , addWarning $ warningAt Warning 3 1 "warning3"
                          , addInfo $ infoAt Info 1 1 "info1"
                          ]
                (finalCollector, _) = runState (sequence_ actions) collector
                
                stats = getErrorStatistics finalCollector
                errorCount = Map.findWithDefault 0 Error stats
                warningCount = Map.findWithDefault 0 Warning stats
                infoCount = Map.findWithDefault 0 Info stats
                
            errorCount @?= 2
            warningCount @?= 3
            infoCount @?= 1
        ]
        
    , testGroup "Error filtering consistency"
        [ testCase "filtering by category preserves other attributes" $ do
            let errors = 
                  [ errorWithCategory TypeChecking Error 1 1 "type error"
                  , errorWithCategory Ownership Error 2 1 "ownership error"
                  , errorWithCategory Parsing Warning 3 1 "parse warning"
                  , errorWithCategory Semantic Info 4 1 "semantic info"
                  ]
                typeErrors = filterByCategory TypeChecking errors
                ownershipErrors = filterByCategory Ownership errors
                
            length typeErrors @?= 1
            length ownershipErrors @?= 1
            
            category (head typeErrors) @?= TypeChecking
            category (head ownershipErrors) @?= Ownership
            
            -- Other attributes should be preserved
            severity (head typeErrors) @?= Error
            severity (head ownershipErrors) @?= Error
            
        , testCase "filtering by severity maintains ordering" $ do
            let errors = 
                  [ errorAt Info 1 1 "info1"
                  , errorAt Error 2 1 "error1"
                  , errorAt Warning 3 1 "warning1"
                  , errorAt Info 4 1 "info2"
                  , errorAt Error 5 1 "error2"
                  ]
                errorOnly = filterBySeverity Error errors
                warningOnly = filterBySeverity Warning errors
                
            length errorOnly @?= 2
            length warningOnly @?= 1
            
            -- Should maintain original order
            map message errorOnly @?= ["error1", "error2"]
            map message warningOnly @?= ["warning1"]
        ]
        
    , testGroup "Error formatting consistency"
        [ testCase "formatting includes all relevant information" $ do
            let location = ErrorLocation (Just "test.typus") 10 5 Nothing Nothing
                context = emptyContext 
                    { contextFunction = Just "testFunc"
                    , contextVariable = Just "testVar"
                    }
                suggestions = ["Check variable types", "Verify imports"]
                error = errorAt Error 10 5 "test error"
                    { location = location
                    , context = context
                    , suggestions = suggestions
                    }
                formatted = formatError error
                
            -- Formatted error should contain key information
            "test.typus" `T.isInfixOf` formatted @?= True
            "10:5" `T.isInfixOf` formatted @?= True
            "testFunc" `T.isInfixOf` formatted @?= True
            "testVar" `T.isInfixOf` formatted @?= True
            "test error" `T.isInfixOf` formatted @?= True
            
        , testCase "multiple errors are formatted consistently" $ do
            let errors = 
                  [ errorAt Error 1 1 "first error"
                  , errorAt Warning 2 1 "warning message"
                  , errorAt Info 3 1 "info message"
                  ]
                formatted = formatErrors errors
                lines = T.lines formatted
                
            -- Should have one line per error
            length lines @?= 3
            
            -- All error messages should be present
            "first error" `T.isInfixOf` formatted @?= True
            "warning message" `T.isInfixOf` formatted @?= True
            "info message" `T.isInfixOf` formatted @?= True
        ]
        
    , testGroup "Error wrapping and chaining consistency"
        [ testCase "wrapped errors preserve original information" $ do
            let originalError = errorAt Error 1 1 "original error"
                wrappedError = wrapError "wrapper context" originalError
                
                chain = errorChain wrappedError
                
            length chain @?= 1
            message (head chain) @?= "original error"
            message wrappedError @?= "wrapper context"
            
        , testCase "error chaining maintains order" $ do
            let error1 = errorAt Error 1 1 "error1"
                error2 = wrapError "context2" error1
                error3 = wrapError "context3" error2
                
                chain = errorChain error3
                messages = map message chain
                
            length chain @?= 2
            messages @?= ["error1", "context2"]
            message error3 @?= "context3"
        ]
        
    , testGroup "Combined error consistency"
        [ testCase "combined errors have consistent severity" $ do
            let ownershipError = OwnershipErrorCombined Error undefined
                typeError = DependentTypeErrorCombined Error undefined
                integrationError = IntegrationError "test" Error
                crossError = CrossAnalyzerError "test" Error [ownershipError, typeError]
                
                sev1 = combinedErrorSeverity ownershipError
                sev2 = combinedErrorSeverity typeError
                sev3 = combinedErrorSeverity integrationError
                sev4 = combinedErrorSeverity crossError
                
            sev1 @?= Error
            sev2 @?= Error
            sev3 @?= Error
            sev4 @?= Error
            
        , testCase "combined error filtering works correctly" $ do
            let errors = 
                  [ OwnershipErrorCombined Error undefined
                  , OwnershipErrorCombined Warning undefined
                  , DependentTypeErrorCombined Error undefined
                  , DependentTypeErrorCombined Info undefined
                  ]
                errorOnly = filterCombinedErrorsBySeverity Error errors
                warningOnly = filterCombinedErrorsBySeverity Warning errors
                
            length errorOnly @?= 2
            length warningOnly @?= 1
        ]
        
    , testGroup "Recovery strategy consistency"
        [ testCase "recovery strategies are appropriate for severity" $ do
            let fatal = fatalError 1 1 "fatal"
                regular = errorAt Error 1 1 "error"
                warning = warningAt Warning 1 1 "warning"
                info = infoAt Info 1 1 "info"
                
                canRecoverFatal = canRecoverFrom fatal
                canRecoverRegular = canRecoverFrom regular
                canRecoverWarning = canRecoverFrom warning
                canRecoverInfo = canRecoverFrom info
                
            canRecoverFatal @?= False
            canRecoverRegular @?= True
            canRecoverWarning @?= True
            canRecoverInfo @?= True
            
        , testCase "custom recovery strategies are applied correctly" $ do
            let customStrat = customRecovery True True 
                                    (Just "retry") 
                                    (Just "check network") 
                                    30 0.8
                error = errorAt Error 1 1 "network error"
                    { recovery = customStrat }
                    
                recoveryStrat = recovery error
                
            canRecover recoveryStrat @?= True
            shouldContinue recoveryStrat @?= True
            recoveryAction recoveryStrat @?= Just "retry"
            recoveryHint recoveryStrat @?= Just "check network"
            recoveryCost recoveryStrat @?= 30
            recoveryConfidence recoveryStrat @?= 0.8
        ]
    ]

-- Helper function for sequence_
sequence_ :: [State s a] -> State s ()
sequence_ = mapM_ (>> return ())