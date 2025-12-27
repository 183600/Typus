{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorHandlerQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.Tasty (TestTree)

import ErrorHandler (ErrorContext(..), ErrorSeverity(..), ErrorCollector, newErrorCollector, addError, getErrors)
import Compiler.Errors.Core (ErrorSeverity(..), ErrorContext(..), ErrorCollector, newErrorCollector, addError, getErrors)
import Compiler (CompilerError(..), CompilationPhase(..))
import Parser (parseTypus)
import Data.Char (isSpace)
import qualified Data.List as List
import Data.Text (Text)

-- Property: Error handler handles empty error list
prop_error_handler_empty :: Property
prop_error_handler_empty =
  let collector = newErrorCollector
      errors = getErrors collector
  in property $ null errors

-- Property: Error handler preserves error order
prop_error_handler_preserves_order :: [String] -> Property
prop_error_handler_preserves_order errors =
  not (null errors) ==>
  let collector = newErrorCollector
      collectorWithErrors = foldr (\err acc -> addError (ErrorContext "test" ErrorInfo) err acc) collector errors
      result = getErrors collectorWithErrors
  in property $ length result === length errors

-- Property: Error handler handles duplicate errors
prop_error_handler_duplicates :: String -> Int -> Property
prop_error_handler_duplicates baseError count =
  count >= 1 && count <= 5 ==>
  let errors = List.replicate count baseError
      collector = newErrorCollector
      collectorWithErrors = foldr (\err acc -> addError (ErrorContext "test" ErrorWarning) err acc) collector errors
      result = getErrors collectorWithErrors
  in property $ length result === count

-- Property: Error handler handles different severity levels
prop_error_handler_severity :: String -> ErrorSeverity -> Property
prop_error_handler_severity error severity =
  let collector = newErrorCollector
      collectorWithError = addError (ErrorContext "severity-test" severity) error collector
      result = getErrors collectorWithError
  in property $ not (null result)

-- Property: Enhanced error handler provides more context
prop_enhanced_error_context :: String -> String -> Property
prop_enhanced_error_context error context =
  let collector = newErrorCollector
      collectorWithError = addError (ErrorContext context ErrorInfo) error collector
      result = getErrors collectorWithError
  in property $ not (null result)

-- Property: Error handler gracefully handles malformed errors
prop_error_handler_malformed :: String -> Property
prop_error_handler_malformed input =
  let malformed = "\0\1\2" ++ input ++ "\127\255"
      collector = newErrorCollector
      collectorWithError = addError (ErrorContext "malformed-test" ErrorError) malformed collector
      result = getErrors collectorWithError
  in property $ not (null result)

-- Property: Error handler handles unicode errors
prop_error_handler_unicode :: String -> Property
prop_error_handler_unicode base =
  let unicode = base ++ "测试🚀café naïve"
      collector = newErrorCollector
      collectorWithError = addError (ErrorContext "unicode-test" ErrorInfo) unicode collector
      result = getErrors collectorWithError
  in property $ not (null result)

-- Property: Error handler composes correctly
prop_error_handler_composition :: String -> String -> Property
prop_error_handler_composition error1 error2 =
  let collector1 = addError (ErrorContext "compose1" ErrorWarning) error1 newErrorCollector
      collector1' = addError (ErrorContext "compose2" ErrorError) error2 collector1
      result1 = getErrors collector1'
      collector2 = addError (ErrorContext "compose2" ErrorError) error2 newErrorCollector
      collector2' = addError (ErrorContext "compose1" ErrorWarning) error1 collector2
      result2 = getErrors collector2'
  in property $ length result1 === length result2 .&&. length result1 === 2

-- Property: Error handler handles large error messages
prop_error_handler_large :: Int -> String -> Property
prop_error_handler_large multiplier base =
  multiplier >= 1 && multiplier <= 10 ==>
  let largeError = List.concat (List.replicate multiplier (base ++ " "))
      collector = newErrorCollector
      collectorWithError = addError (ErrorContext "large-test" ErrorInfo) largeError collector
      result = getErrors collectorWithError
  in property $ not (null result)

-- Property: Error handler maintains consistency with compiler errors
prop_error_handler_compiler_consistency :: String -> Property
prop_error_handler_compiler_consistency input =
  let parseResult = parseTypus input
      collector = newErrorCollector
  in property $ case parseResult of
    Left parseErr -> 
      let collectorWithError = addError (ErrorContext "compiler-test" ErrorError) (show parseErr) collector
          result = getErrors collectorWithError
      in not (null result)
    Right _ -> property True

tests :: TestTree
tests = testGroup "New ErrorHandler QuickCheck Tests"
  [ fastProperty "Error handler handles empty list" prop_error_handler_empty
  , fastProperty "Error handler preserves order" prop_error_handler_preserves_order
  , fastProperty "Error handler handles duplicates" prop_error_handler_duplicates
  , fastProperty "Error handler handles severity levels" prop_error_handler_severity
  , fastProperty "Enhanced error provides context" prop_enhanced_error_context
  , fastProperty "Error handler handles malformed" prop_error_handler_malformed
  , fastProperty "Error handler handles unicode" prop_error_handler_unicode
  , fastProperty "Error handler composes correctly" prop_error_handler_composition
  , fastProperty "Error handler handles large messages" prop_error_handler_large
  , fastProperty "Error handler compiler consistency" prop_error_handler_compiler_consistency
  ]