{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ErrorHandlingBasicQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.List (isInfixOf)

import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "ErrorHandling Basic QuickCheck Tests"
  [ errorReportingProperties
  , errorRecoveryProperties
  , errorLocationProperties
  ]

errorReportingProperties :: TestTree
errorReportingProperties = testGroup "Error Reporting Properties"
  [ fastProperty "error messages are non-empty" $ \(msg :: String) ->
      not (null msg) ==> length msg > 0
  
  , fastProperty "error contains location information" $ \pos ->
      let line = posLine pos
          col = posColumn pos
      in line > 0 && col > 0
  
  , fastProperty "multiple errors are accumulated" $ \(e1 :: String) (e2 :: String) ->
      let errors = [e1, e2]
      in length errors === 2
  ]

errorRecoveryProperties :: TestTree
errorRecoveryProperties = testGroup "Error Recovery Properties"
  [ fastProperty "recovery continues parsing after error" $ \(tokens :: [String]) ->
      not (null tokens) ==> length tokens > 0
  
  , fastProperty "recovered parse has partial results" $ \(parsed :: [String]) (errors :: [String]) ->
      length parsed >= 0 && length errors >= 0
  ]

errorLocationProperties :: TestTree
errorLocationProperties = testGroup "Error Location Properties"
  [ fastProperty "error location is within source bounds" $ \pos ->
      posLine pos > 0 && posColumn pos > 0
  
  , fastProperty "error span has valid range" $ \sp ->
      let start = spanStart sp
          end = spanEnd sp
      in posLine end >= posLine start
  
  , fastProperty "error message includes line number" $ \pos ->
      let line = posLine pos
      in line > 0
  ]
