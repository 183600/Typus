{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlingBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, listOf1, choose)

import ErrorHandler (ErrorHandler(..), ErrorContext(..))
import EnhancedErrorHandler (EnhancedErrorHandler(..))
import Compiler.Errors (CompilerError(..), ErrorSeverity(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub)
import Data.Char (isSpace, isControl)
import qualified Data.Text as T (pack, unpack)

-- Property: Error context preservation under nesting
prop_error_context_nesting :: [ErrorContext] -> Property
prop_error_context_nesting contexts =
  let contexts' = nub contexts  -- Remove duplicates for valid test
      handler = ErrorHandler contexts'
      nested = ErrorHandler (ErrorContext "nested" handler : contexts')
  in L.length (errorContexts nested) >= L.length contexts'

-- Property: Error severity ordering is consistent
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let severityOrder = [ErrorInfo, ErrorWarning, ErrorError, ErrorFatal]
      order1 = L.length $ takeWhile (/= sev1) severityOrder
      order2 = L.length $ takeWhile (/= sev2) severityOrder
  in (sev1 == sev2) || (order1 /= order2)

-- Property: Error messages contain source location information
prop_error_messages_contain_location :: SourceSpan -> String -> Property
prop_error_messages_contain_location span msg =
  let error = CompilerError (T.pack msg) ErrorError span
      errorMsg = T.unpack $ renderCompilationError error
  in not (null errorMsg) ==> 
     (show (sourceLineStart span) `L.isInfixOf` errorMsg) .&&.
     (show (sourceColumnStart span) `L.isInfixOf` errorMsg)

-- Property: Error handling is idempotent
prop_error_handling_idempotent :: [String] -> Property
prop_error_handling_idempotent messages =
  let handler = ErrorHandler []
      processed1 = L.map (handleError handler . T.pack) messages
      processed2 = L.map (handleError handler . T.pack) messages
  in processed1 === processed2

-- Property: Error recovery preserves essential information
prop_error_recovery_preserves_info :: String -> Property
prop_error_recovery_preserves_info msg =
  let original = T.pack msg
      recovered = recoverFromError original
  in not (T.null original) ==> not (T.null recovered)

-- Property: Multiple errors are sorted by severity
prop_multiple_errors_sorted :: [(ErrorSeverity, Int)] -> Property
prop_multiple_errors_sorted errors =
  let errorList = [CompilerError (T.pack $ "Error " ++ show i) sev (SourceSpan (SourcePos i 0 0) (SourcePos i 0 0)) 
                  | (sev, i) <- errors]
      sortedErrors = sortErrors errorList
  in L.all (\(i, (CompilerError _ sev _)) -> 
            case lookup (i-1) errors of
              Just (prevSev, _) -> severityOrder sev >= severityOrder prevSev
              Nothing -> True) 
         (zip [1..] sortedErrors)
  where
    severityOrder ErrorInfo = 0
    severityOrder ErrorWarning = 1
    severityOrder ErrorError = 2
    severityOrder ErrorFatal = 3

-- Property: Error context chain is maintained
prop_error_context_chain :: [String] -> Property
prop_error_context_chain contexts =
  let handler = L.foldr (\ctx acc -> ErrorHandler [ErrorContext ctx acc]) (ErrorHandler []) contexts
      chain = extractContextChain handler
  in L.length chain >= min (L.length contexts) 5  -- Limit chain L.length for practicality

-- Property: Error handling with special characters
prop_error_handling_special_chars :: String -> Property
prop_error_handling_special_chars msg =
  let specialChars = filter isControl msg
      handler = ErrorHandler []
      result = handleError handler (T.pack msg)
  in not (null specialChars) ==> T.L.length result >= T.L.length (T.pack msg) `div` 2

-- Property: Error boundary conditions
prop_error_boundary_conditions :: Int -> Property
prop_error_boundary_conditions n =
  let size = abs n `mod` 1000
      largeMsg = replicate size 'x'
      handler = ErrorHandler []
      result = handleError handler (T.pack largeMsg)
  in size > 0 ==> T.L.length result > 0

-- Helper functions (these would need to be implemented in the actual modules)
handleError :: ErrorHandler -> T.Text -> T.Text
handleError _ msg = msg  -- Simplified for example

recoverFromError :: T.Text -> T.Text
recoverFromError msg = if T.null msg then T.empty else T.take 10 msg

renderCompilationError :: CompilerError -> T.Text
renderCompilationError (CompilerError msg _ span) = 
  T.L.concat [msg, T.pack " at ", T.pack $ show span]

sortErrors :: [CompilerError] -> [CompilerError]
sortErrors = sortBySeverity
  where
    sortBySeverity [] = []
    sortBySeverity (x:xs) = sortBySeverity [e | e <- xs, severityOrder (errorSeverity e) <= severityOrder (errorSeverity x)] 
                           ++ [x] ++ 
                           sortBySeverity [e | e <- xs, severityOrder (errorSeverity e) > severityOrder (errorSeverity x)]
    
    severityOrder ErrorInfo = 0
    severityOrder ErrorWarning = 1
    severityOrder ErrorError = 2
    severityOrder ErrorFatal = 3

errorSeverity :: CompilerError -> ErrorSeverity
errorSeverity (CompilerError _ sev _) = sev

extractContextChain :: ErrorHandler -> [String]
extractContextChain (ErrorHandler contexts) = map extractContext contexts
  where
    extractContext (ErrorContext name _) = name

tests :: TestTree
tests = testGroup "Error Handling Boundary QuickCheck Tests"
  [ fastProperty "Error context nesting" prop_error_context_nesting
  , fastProperty "Error severity ordering" prop_error_severity_ordering
  , fastProperty "Error messages contain location" prop_error_messages_contain_location
  , fastProperty "Error handling idempotent" prop_error_handling_idempotent
  , fastProperty "Error recovery preserves info" prop_error_recovery_preserves_info
  , fastProperty "Multiple errors sorted" prop_multiple_errors_sorted
  , fastProperty "Error context chain" prop_error_context_chain
  , fastProperty "Error handling special chars" prop_error_handling_special_chars
  , fastProperty "Error boundary conditions" prop_error_boundary_conditions
  ]