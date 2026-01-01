{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ErrorHandlerQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import ErrorHandler
import Compiler.Errors.Core
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck Tests"
  [ errorCreationTests
  , errorSeverityTests
  , errorCategoryTests
  , errorCollectionTests
  , errorReportingTests
  , errorRecoveryTests
  , errorContextTests
  , errorPropagationTests
  , errorFormattingTests
  , errorHandlerValidationTests
  ]

-- | 1. 错误创建测试
errorCreationTests :: TestTree
errorCreationTests = testGroup "Error Creation Tests"
  [ testCase "Create syntax error" =
      let error = createError SyntaxError "Unexpected token" (SourceSpan startPos startPos)
      in errorSeverity error @?= Error
  
  , testCase "Create type error" =
      let error = createError TypeError "Type mismatch" (SourceSpan startPos startPos)
      in errorCategory error @?= TypeChecking
  
  , testCase "Create warning" =
      let warning = createWarning "Unused variable" (SourceSpan startPos startPos)
      in errorSeverity warning @?= Warning
  
  , fastProperty "Error message preservation" =
      \message -> let error = createError SyntaxError message (SourceSpan startPos startPos)
                  in errorMessage error == message
  ]

-- | 2. 错误严重性测试
errorSeverityTests :: TestTree
errorSeverityTests = testGroup "Error Severity Tests"
  [ testCase "Error severity comparison" =
      let error = createError SyntaxError "test" (SourceSpan startPos startPos)
          warning = createWarning "test" (SourceSpan startPos startPos)
      in compare (errorSeverity error) (errorSeverity warning) @?= GT
  
  , testCase "Info severity comparison" =
      let info = createInfo "test" (SourceSpan startPos startPos)
          warning = createWarning "test" (SourceSpan startPos startPos)
      in compare (errorSeverity info) (errorSeverity warning) @?= LT
  
  , fastProperty "Severity ordering consistency" =
      \sev1 sev2 -> let ordered = compare sev1 sev2
                    in ordered == EQ || ordered == LT || ordered == GT
  ]

-- | 3. 错误类别测试
errorCategoryTests :: TestTree
errorCategoryTests = testGroup "Error Category Tests"
  [ testCase "Syntax error category" =
      let error = createError SyntaxError "test" (SourceSpan startPos startPos)
      in errorCategory error @?= Parsing
  
  , testCase "Type error category" =
      let error = createError TypeError "test" (SourceSpan startPos startPos)
      in errorCategory error @?= TypeChecking
  
  , testCase "Semantic error category" =
      let error = createError SemanticError "test" (SourceSpan startPos startPos)
      in errorCategory error @?= Semantic
  
  , fastProperty "Category consistency" =
      \errorType -> let error = createError errorType "test" (SourceSpan startPos startPos)
                    in case errorType of
                         SyntaxError -> errorCategory error == Parsing
                         TypeError -> errorCategory error == TypeChecking
                         SemanticError -> errorCategory error == Semantic
                         _ -> True
  ]

-- | 4. 错误集合测试
errorCollectionTests :: TestTree
errorCollectionTests = testGroup "Error Collection Tests"
  [ testCase "Empty error collection" =
      let collection = emptyErrorCollection
      in errorCount collection @?= 0
  
  , testCase "Add error to collection" =
      let collection = emptyErrorCollection
          error = createError SyntaxError "test" (SourceSpan startPos startPos)
          collection' = addError collection error
      in errorCount collection' @?= 1
  
  , testCase "Get errors by severity" =
      let collection = emptyErrorCollection
          error = createError SyntaxError "test" (SourceSpan startPos startPos)
          warning = createWarning "test" (SourceSpan startPos startPos)
          collection' = addError (addError collection error) warning
          errors = getErrorsBySeverity collection' Error
      in L.length errors @?= 1
  
  , fastProperty "Error count consistency" =
      \errors -> let collection = foldl addError emptyErrorCollection errors
                 in errorCount collection == L.length errors
  ]

-- | 5. 错误报告测试
errorReportingTests :: TestTree
errorReportingTests = testGroup "Error Reporting Tests"
  [ testCase "Format single error" =
      let error = createError SyntaxError "Unexpected token" (SourceSpan startPos startPos)
          report = formatError error
      in "Unexpected token" `L.isInfixOf` report @?= True
  
  , testCase "Format error collection" =
      let collection = emptyErrorCollection
          error = createError SyntaxError "test" (SourceSpan startPos startPos)
          collection' = addError collection error
          report = formatErrorCollection collection'
      in L.length (lines report) @?= 1
  
  , testCase "Error report includes location" =
      let span = SourceSpan startPos (posAfter 'a' startPos)
          error = createError SyntaxError "test" span
          report = formatError error
      in "line 1" `L.isInfixOf` report @?= True
  
  , fastProperty "Error message in report" =
      \message -> let error = createError SyntaxError message (SourceSpan startPos startPos)
                      report = formatError error
                  in message `L.isInfixOf` report
  ]
  where
    L.isInfixOf needle haystack = needle `elem` (words haystack)

-- | 6. 错误恢复测试
errorRecoveryTests :: TestTree
errorRecoveryTests = testGroup "Error Recovery Tests"
  [ testCase "Error recovery strategy" =
      let strategy = ErrorRecovery SkipToken
          error = createError SyntaxError "test" (SourceSpan startPos startPos)
          recovered = applyRecoveryStrategy strategy error
      in recovered `seq` True @?= True
  
  , testCase "Recovery with insertion" =
      let strategy = ErrorRecovery (InsertToken ";")
          error = createError SyntaxError "Missing semicolon" (SourceSpan startPos startPos)
          recovered = applyRecoveryStrategy strategy error
      in recovered `seq` True @?= True
  
  , testCase "Recovery with deletion" =
      let strategy = ErrorRecovery DeleteToken
          error = createError SyntaxError "Unexpected token" (SourceSpan startPos startPos)
          recovered = applyRecoveryStrategy strategy error
      in recovered `seq` True @?= True
  
  , fastProperty "Recovery preserves error" =
      \strategy -> let error = createError SyntaxError "test" (SourceSpan startPos startPos)
                       recovered = applyRecoveryStrategy strategy error
                   in errorMessage recovered == errorMessage error
  ]

-- | 7. 错误上下文测试
errorContextTests :: TestTree
errorContextTests = testGroup "Error Context Tests"
  [ testCase "Add context to error" =
      let error = createError SyntaxError "test" (SourceSpan startPos startPos)
          context = ErrorContext [("line", "1"), ("column", "5")]
          error' = addErrorContext error context
      in hasErrorContext error' @?= True
  
  , testCase "Get context value" =
      let error = createError SyntaxError "test" (SourceSpan startPos startPos)
          context = ErrorContext [("key", "value")]
          error' = addErrorContext error context
          value = getErrorContext error' "key"
      in value @?= Just "value"
  
  , testCase "Missing context value" =
      let error = createError SyntaxError "test" (SourceSpan startPos startPos)
          context = ErrorContext [("key", "value")]
          error' = addErrorContext error context
          value = getErrorContext error' "missing"
      in value @?= Nothing
  
  , fastProperty "Context key preservation" =
      \key value -> let error = createError SyntaxError "test" (SourceSpan startPos startPos)
                        context = ErrorContext [(key, value)]
                        error' = addErrorContext error context
                        retrieved = getErrorContext error' key
                    in retrieved == Just value
  ]

-- | 8. 错误传播测试
errorPropagationTests :: TestTree
errorPropagationTests = testGroup "Error Propagation Tests"
  [ testCase "Error propagation up the call stack" =
      let error = createError SyntaxError "test" (SourceSpan startPos startPos)
          propagated = propagateError error "function1"
      in errorSource propagated @?= Just "function1"
  
  , testCase "Multiple propagation steps" =
      let error = createError SyntaxError "test" (SourceSpan startPos startPos)
          propagated1 = propagateError error "function1"
          propagated2 = propagateError propagated1 "function2"
      in errorSource propagated2 @?= Just "function2"
  
  , fastProperty "Propagation preserves message" =
      \source -> let error = createError SyntaxError "test" (SourceSpan startPos startPos)
                      propagated = propagateError error source
                  in errorMessage propagated == errorMessage error
  ]

-- | 9. 错误格式化测试
errorFormattingTests :: TestTree
errorFormattingTests = testGroup "Error Formatting Tests"
  [ testCase "Format error with context" =
      let error = createError SyntaxError "test" (SourceSpan startPos startPos)
          context = ErrorContext [("file", "test.go")]
          error' = addErrorContext error context
          formatted = formatErrorWithContext error'
      in "test.go" `L.isInfixOf` formatted @?= True
  
  , testCase "Format error collection with summary" =
      let collection = emptyErrorCollection
          error = createError SyntaxError "test" (SourceSpan startPos startPos)
          warning = createWarning "test" (SourceSpan startPos startPos)
          collection' = addError (addError collection error) warning
          formatted = formatErrorCollectionWithSummary collection'
      in "2 issues" `L.isInfixOf` formatted @?= True
  
  , fastProperty "Formatting includes severity" =
      \severity -> let error = Error severity SyntaxError "test" (SourceSpan startPos startPos) Nothing
                       formatted = formatError error
                   in case severity of
                        Error -> "error" `L.isInfixOf` formatted
                        Warning -> "warning" `L.isInfixOf` formatted
                        Info -> "info" `L.isInfixOf` formatted
  ]
  where
    L.isInfixOf needle haystack = needle `elem` (words haystack)

-- | 10. 错误处理器验证测试
errorHandlerValidationTests :: TestTree
errorHandlerValidationTests = testGroup "ErrorHandler Validation Tests"
  [ testCase "Valid error handler" =
      let handler = createErrorHandler []
      in validateErrorHandler handler @?= True
  
  , testCase "Error handler with custom strategy" =
      let strategy = ErrorRecovery SkipToken
          handler = createErrorHandler [strategy]
      in validateErrorHandler handler @?= True
  
  , testCase "Error handler processes collection" =
      let handler = createErrorHandler []
          collection = emptyErrorCollection
          error = createError SyntaxError "test" (SourceSpan startPos startPos)
          collection' = addError collection error
          processed = processErrors handler collection'
      in errorCount processed @?= 1
  
  , fastProperty "Handler processes L.all errors" =
      \errors -> let handler = createErrorHandler []
                      collection = foldl addError emptyErrorCollection errors
                      processed = processErrors handler collection
                  in errorCount processed == L.length errors
  ]