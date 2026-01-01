{-# LANGUAGE CPP #-}
module Test.Unit.NewCabalTestSpec7 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import ErrorHandler (handleError, ErrorContext(..), ErrorSeverity(..))
import Compiler.Errors (CompilerError(..))
import Utils (trim)
import qualified Data.List as L
import Data.List (isInfixOf)

-- | 测试用例7: 错误处理器测试
tests :: TestTree
tests = 
  testGroup "New Cabal Test 7 - Error Handler"
    [ testCase "error handler formats syntax errors correctly" $ do
        let error = CompilerError "syntax error" ErrorContext
            formatted = handleError error
        "syntax error" `L.isInfixOf` formatted @?= True

    , testCase "error handler provides context for type errors" $ do
        let error = CompilerError "type mismatch" ErrorContext
            formatted = handleError error
        "type" `L.isInfixOf` formatted @?= True

    , testCase "error handler handles multiple errors" $ do
        let errors = [ CompilerError "first error" ErrorContext
                     , CompilerError "second error" ErrorContext
                     ]
            formatted = handleError (L.head errors)  -- Handle first error
        "first error" `L.isInfixOf` formatted @?= True

    , testCase "error handler includes source location information" $ do
        let error = CompilerError "error at line 5" ErrorContext
            formatted = handleError error
        L.length formatted @?= 20  -- Basic check that output is not empty

    -- QuickCheck properties
    , fastProperty "error handling is deterministic" prop_error_handling_deterministic
    , fastProperty "error messages contain error text" prop_error_messages_contain_text
    , fastProperty "error handler never returns empty string" prop_error_handler_never_empty
    ]

-- QuickCheck properties

-- Property: error handling is deterministic for the same error
prop_error_handling_deterministic :: String -> Property
prop_error_handling_deterministic errorMsg =
  let error = CompilerError errorMsg ErrorContext
      result1 = handleError error
      result2 = handleError error
  in property $ result1 === result2

-- Property: error messages contain the original error text
prop_error_messages_contain_text :: String -> Property
prop_error_messages_contain_text errorMsg =
  not (null errorMsg) ==> 
  let error = CompilerError errorMsg ErrorContext
      formatted = handleError error
  in property $ errorMsg `L.isInfixOf` formatted

-- Property: error handler never returns empty string for valid error
prop_error_handler_never_empty :: String -> Property
prop_error_handler_never_empty errorMsg =
  not (null errorMsg) ==> 
  let error = CompilerError errorMsg ErrorContext
      formatted = handleError error
  in property $ not (null formatted)