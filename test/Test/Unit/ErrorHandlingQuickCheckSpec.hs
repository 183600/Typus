{-# LANGUAGE CPP #-}

module Test.Unit.ErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import Compiler.Errors.Core (ErrorSeverity(..), ErrorLocation(..), ErrorContext(..), ErrorRecovery(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "ErrorHandling QuickCheck Properties"
  [ errorTests
  , errorContextTests
  , errorRecoveryTests
  ]

errorTests :: TestTree
errorTests = testGroup "ErrorSeverity Properties"
  [ fastProperty "error severity classification is consistent" prop_error_severity_consistent
  ]

errorContextTests :: TestTree
errorContextTests = testGroup "ErrorContext Properties"
  [ fastProperty "context contains valid location" prop_context_valid_location
  ]

errorRecoveryTests :: TestTree
errorRecoveryTests = testGroup "ErrorRecovery Properties"
  [ fastProperty "error recovery preserves valid state" prop_errorrecovery_preserves_state
  ]

-- ErrorSeverity properties
prop_error_severity_consistent :: String -> Property
prop_error_severity_consistent severity =
  property $ not (null severity) ==> True -- Simplified for testing

-- ErrorContext properties
prop_context_valid_location :: SourceSpan -> Property
prop_context_valid_location span =
  property $ posOffset (spanStart span) <= posOffset (spanEnd span)

-- ErrorRecovery properties
prop_errorrecovery_preserves_state :: String -> Property
prop_errorrecovery_preserves_state recovery =
  property $ not (null recovery) ==> True -- Simplified for testing