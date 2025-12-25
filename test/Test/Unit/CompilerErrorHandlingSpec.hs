{-# LANGUAGE CPP #-}

module Test.Unit.CompilerErrorHandlingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as Map

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , errorAt
  , warningAt
  , infoAt
  , fatalError
  , errorWithCategory
  , warningWithCategory
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , wrapError
  , formatError
  , formatErrorWithLocation
  , formatErrors
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , _atLocation
  , _atFileLocation
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  )

tests :: TestTree
tests = testGroup "Compiler Error Handling"
  [ errorCreationTests
  , errorFormattingTests
  , errorFilteringTests
  , errorRecoveryTests
  , errorContextTests
  , errorStatisticsTests
  , errorChainingTests
  , errorReportingTests
  ]

errorCreationTests :: TestTree
errorCreationTests = testGroup "Error Creation"
  [ testCase "creates basic error with required fields" $ do
      let loc = _atLocation 10 5
          err = errorAt "ERR001" "Test error message" loc
      errorId err @?= "ERR001"
      message err @?= "Test error message"
      location err @?= loc
      severity err @?= Error
      category err @?= Unknown
      context err @?= emptyContext
      suggestions err @?= []
      relatedErrors err @?= []

  , testCase "creates error with specific category" $ do
      let loc = _atLocation 5 3
          err = errorWithCategory "ERR002" TypeChecking "Type mismatch error" loc
      errorId err @?= "ERR002"
      severity err @?= Error
      category err @?= TypeChecking
      message err @?= "Type mismatch error"

  , testCase "creates warning with correct severity" $ do
      let loc = _atLocation 15 8
          warning = warningAt "WARN001" "This is a warning" loc
      severity warning @?= Warning
      message warning @?= "This is a warning"

  , testCase "creates info message with correct severity" $ do
      let loc = _atLocation 20 1
          info = infoAt "INFO001" "This is info" loc
      severity info @?= Info
      message info @?= "This is info"

  , testCase "creates fatal error with appropriate recovery" $ do
      let loc = _atLocation 1 1
          fatal = fatalError "FATAL001" "Critical system failure" loc
      severity fatal @?= Fatal
      recovery fatal @?= fatalRecovery
      assertBool "fatal errors should not be recoverable" $ not $ canRecoverFrom fatal
      assertBool "should not continue after fatal error" $ not $ shouldContinueAfter fatal

  , testCase "creates error with file location" $ do
      let loc = _atFileLocation "test.typus" 10 5
          err = errorAt "ERR003" "Error in file" loc
      filePath (location err) @?= Just "test.typus"
      line (location err) @?= 10
      column (location err) @?= 5
  ]

errorFormattingTests :: TestTree
errorFormattingTests = testGroup "Error Formatting"
  [ testCase "formats basic error correctly" $ do
      let loc = _atLocation 10 5
          err = errorAt "ERR001" "Test error" loc
          formatted = formatError err
      assertBool "format includes severity" $ "[ERROR]" `isInfixOf` formatted
      assertBool "format includes category" $ "[Unknown]" `isInfixOf` formatted
      assertBool "format includes message" $ "Test error" `isInfixOf` formatted

  , testCase "formats error with location" $ do
      let loc = _atFileLocation "test.typus" 10 5
          err = errorAt "ERR001" "Test error" loc
          formatted = formatErrorWithLocation err
      assertBool "format includes file location" $ "test.typus:10:5" `isInfixOf` formatted
      assertBool "format includes error message" $ "Test error" `isInfixOf` formatted

  , testCase "formats error with range" $ do
      let loc = ErrorLocation (Just "test.typus") 10 5 (Just 10) (Just 15)
          err = errorAt "ERR001" "Range error" loc
          formatted = formatErrorWithLocation err
      assertBool "format includes range" $ "test.typus:10:5-10:15" `isInfixOf` formatted

  , testCase "formats multiple errors sorted by severity" $ do
      let loc1 = _atLocation 1 1
          loc2 = _atLocation 2 2
          loc3 = _atLocation 3 3
          fatal = fatalError "FATAL001" "Fatal error" loc1
          error = errorAt "ERR001" "Regular error" loc2
          warning = warningAt "WARN001" "Warning" loc3
          errors = [warning, error, fatal]
          formatted = formatErrors errors
      assertBool "fatal error comes first" $ "FATAL" `isPrefixOf` formatted
      assertBool "error comes before warning" $ 
        let errorPos = length $ takeWhile (not . isInfixOf "[ERROR]") (lines formatted)
            warningPos = length $ takeWhile (not . isInfixOf "[WARNING]") (lines formatted)
        in errorPos < warningPos

  , testCase "formats error with suggestions" $ do
      let loc = _atLocation 10 5
          err = withSuggestions ["Suggestion 1", "Suggestion 2"] $ 
                errorAt "ERR001" "Error with suggestions" loc
          formatted = formatError err
      assertBool "format includes suggestions" $ "Suggestions:" `isInfixOf` formatted
      assertBool "format includes first suggestion" $ "  - Suggestion 1" `isInfixOf` formatted
      assertBool "format includes second suggestion" $ "  - Suggestion 2" `isInfixOf` formatted

  , testCase "formats error with context" $ do
      let loc = _atLocation 10 5
          ctx = emptyContext 
                  { contextFunction = Just "testFunction"
                  , contextVariable = Just "testVar"
                  , contextType = Just "String"
                  }
          err = withContext ctx $ errorAt "ERR001" "Error with context" loc
          formatted = formatErrorWithLocation err
      assertBool "format includes context" $ "Context:" `isInfixOf` formatted
      assertBool "format includes function" $ "function: testFunction" `isInfixOf` formatted
      assertBool "format includes variable" $ "variable: testVar" `isInfixOf` formatted
      assertBool "format includes type" $ "type: String" `isInfixOf` formatted
  ]

errorFilteringTests :: TestTree
errorFilteringTests = testGroup "Error Filtering"
  [ testCase "filters errors by severity correctly" $ do
      let loc = _atLocation 1 1
          fatal = fatalError "FATAL001" "Fatal" loc
          error = errorAt "ERR001" "Error" loc
          warning = warningAt "WARN001" "Warning" loc
          info = infoAt "INFO001" "Info" loc
          allErrors = [fatal, error, warning, info]
          onlyErrors = filterBySeverity Error allErrors
          onlyWarnings = filterBySeverity Warning allErrors
          onlyFatal = filterBySeverity Fatal allErrors
          onlyInfo = filterBySeverity Info allErrors
      length onlyErrors @?= 1
      length onlyWarnings @?= 1
      length onlyFatal @?= 1
      length onlyInfo @?= 1
      head onlyErrors @?= error
      head onlyWarnings @?= warning
      head onlyFatal @?= fatal
      head onlyInfo @?= info

  , testCase "filters errors by category correctly" $ do
      let loc = _atLocation 1 1
          typeError = errorWithCategory "ERR001" TypeChecking "Type error" loc
          ownershipError = errorWithCategory "ERR002" Ownership "Ownership error" loc
          parseError = errorWithCategory "ERR003" Parsing "Parse error" loc
          allErrors = [typeError, ownershipError, parseError]
          typeErrors = filterByCategory TypeChecking allErrors
          ownershipErrors = filterByCategory Ownership allErrors
          parseErrors = filterByCategory Parsing allErrors
      length typeErrors @?= 1
      length ownershipErrors @?= 1
      length parseErrors @?= 1
      head typeErrors @?= typeError
      head ownershipErrors @?= ownershipError
      head parseErrors @?= parseError

  , testCase "checks error category correctly" $ do
      let loc = _atLocation 1 1
          typeError = errorWithCategory "ERR001" TypeChecking "Type error" loc
          ownershipError = errorWithCategory "ERR002" Ownership "Ownership error" loc
      assertBool "typeError has TypeChecking category" $ hasCategory TypeChecking typeError
      assertBool "typeError does not have Ownership category" $ not $ hasCategory Ownership typeError
      assertBool "ownershipError has Ownership category" $ hasCategory Ownership ownershipError
      assertBool "ownershipError does not have TypeChecking category" $ not $ hasCategory TypeChecking ownershipError
  ]

errorRecoveryTests :: TestTree
errorRecoveryTests = testGroup "Error Recovery"
  [ testCase "uses correct recovery strategies by default" $ do
      let loc = _atLocation 1 1
          error = errorAt "ERR001" "Regular error" loc
          warning = warningAt "WARN001" "Warning" loc
          info = infoAt "INFO001" "Info" loc
      assertBool "errors should be recoverable" $ canRecoverFrom error
      assertBool "should continue after errors" $ shouldContinueAfter error
      assertBool "warnings should be recoverable" $ canRecoverFrom warning
      assertBool "should continue after warnings" $ shouldContinueAfter warning
      assertBool "info should be recoverable" $ canRecoverFrom info
      assertBool "should continue after info" $ shouldContinueAfter info

  , testCase "creates custom recovery strategy" $ do
      let custom = customRecovery True True 
                                        (Just "Custom action") 
                                        (Just "Custom hint") 
                                        25 
                                        0.8
      canRecover custom @?= True
      shouldContinue custom @?= True
      recoveryAction custom @?= Just "Custom action"
      recoveryHint custom @?= Just "Custom hint"
      recoveryCost custom @?= 25
      recoveryConfidence custom @?= 0.8

  , testCase "compares recovery costs and confidence" $ do
      let lowCost = customRecovery True True Nothing Nothing 10 0.5
          highCost = customRecovery True True Nothing Nothing 80 0.9
          lowConfidence = customRecovery True True Nothing Nothing 50 0.3
          highConfidence = customRecovery True True Nothing Nothing 50 0.9
      recoveryCost lowCost @?= 10
      recoveryCost highCost @?= 80
      recoveryConfidence lowConfidence @?= 0.3
      recoveryConfidence highConfidence @?= 0.9
  ]

errorContextTests :: TestTree
errorContextTests = testGroup "Error Context"
  [ testCase "creates empty context correctly" $ do
      let ctx = emptyContext
      contextCode ctx @?= Nothing
      contextFunction ctx @?= Nothing
      contextVariable ctx @?= Nothing
      contextType ctx @?= Nothing
      contextAdditional ctx @?= []

  , testCase "adds context to error correctly" $ do
      let loc = _atLocation 10 5
          ctx = emptyContext
                  { contextFunction = Just "testFunc"
                  , contextVariable = Just "testVar"
                  , contextCode = Just "x := 42"
                  , contextAdditional = [("hint", "check variable type")]
                  }
          err = withContext ctx $ errorAt "ERR001" "Test error" loc
      contextFunction (context err) @?= Just "testFunc"
      contextVariable (context err) @?= Just "testVar"
      contextCode (context err) @?= Just "x := 42"
      contextAdditional (context err) @?= [("hint", "check variable type")]

  , testCase "updates error location correctly" $ do
      let loc1 = _atLocation 1 1
          loc2 = _atLocation 5 10
          err = errorAt "ERR001" "Test error" loc1
          updatedErr = withLocation err loc2
      location updatedErr @?= loc2
      location err @?= loc1  -- Original should be unchanged
  ]

errorStatisticsTests :: TestTree
errorStatisticsTests = testGroup "Error Statistics"
  [ testCase "generates correct statistics for mixed errors" $ do
      let loc = _atLocation 1 1
          fatal = fatalError "FATAL001" "Fatal" loc
          error = errorAt "ERR001" "Error" loc
          warning = warningAt "WARN001" "Warning" loc
          info = infoAt "INFO001" "Info" loc
          typeError = errorWithCategory "ERR002" TypeChecking "Type error" loc
          ownershipError = errorWithCategory "ERR003" Ownership "Ownership error" loc
          allErrors = [fatal, error, warning, info, typeError, ownershipError]
          stats = getErrorStatistics allErrors
      Map.lookup "total" stats @?= Just 6
      Map.lookup "fatal" stats @?= Just 1
      Map.lookup "errors" stats @?= Just 2  -- error + typeError
      Map.lookup "warnings" stats @?= Just 1
      Map.lookup "info" stats @?= Just 1
      Map.lookup "typeChecking" stats @?= Just 1
      Map.lookup "ownership" stats @?= Just 1

  , testCase "handles empty error list in statistics" $ do
      let stats = getErrorStatistics []
      Map.lookup "total" stats @?= Just 0
      Map.lookup "fatal" stats @?= Just 0
      Map.lookup "errors" stats @?= Just 0
      Map.lookup "warnings" stats @?= Just 0
      Map.lookup "info" stats @?= Just 0
  ]

errorChainingTests :: TestTree
errorChainingTests = testGroup "Error Chaining"
  [ testCase "wraps errors correctly" $ do
      let loc = _atLocation 1 1
          original = errorAt "ERR001" "Original error" loc
          wrapped = wrapError "Wrapper message" original
      message wrapped @?= "Wrapper message: Original error"
      errorChain wrapped @?= [original]
      errorId wrapped @?= "ERR001"
      location wrapped @?= loc

  , testCase "adds related errors correctly" $ do
      let loc = _atLocation 1 1
          main = errorAt "ERR001" "Main error" loc
          related1 = warningAt "WARN001" "Related warning" loc
          related2 = errorAt "ERR002" "Related error" loc
          withRelated = withRelatedErrors [related1, related2] main
      length (relatedErrors withRelated) @?= 2
      head (relatedErrors withRelated) @?= related1
      last (relatedErrors withRelated) @?= related2

  , testCase "adds suggestions correctly" $ do
      let loc = _atLocation 1 1
          err = errorAt "ERR001" "Error" loc
          withSuggs = withSuggestions ["Check syntax", "Verify types"] err
      suggestions withSuggs @?= ["Check syntax", "Verify types"]
      suggestions err @?= []  -- Original should be unchanged
  ]

errorReportingTests :: TestTree
errorReportingTests = testGroup "Error Reporting"
  [ testCase "generates basic error report" $ do
      let loc = _atLocation 1 1
          error = errorAt "ERR001" "Test error" loc
          report = generateErrorReport [error]
      assertBool "report includes header" $ "Error Report" `isInfixOf` report
      assertBool "report includes statistics" $ "Statistics:" `isInfixOf` report
      assertBool "report includes total count" $ "total: 1" `isInfixOf` report
      assertBool "report includes error count" $ "errors: 1" `isInfixOf` report
      assertBool "report includes detailed errors" $ "Detailed Errors:" `isInfixOf` report

  , testCase "generates comprehensive report for mixed errors" $ do
      let loc = _atLocation 1 1
          fatal = fatalError "FATAL001" "Fatal error" loc
          error = errorWithCategory "ERR001" TypeChecking "Type error" loc
          warning = warningAt "WARN001" "Warning" loc
          allErrors = [fatal, error, warning]
          report = generateErrorReport allErrors
      assertBool "report includes fatal count" $ "fatal: 1" `isInfixOf` report
      assertBool "report includes error count" $ "errors: 1" `isInfixOf` report
      assertBool "report includes warning count" $ "warnings: 1" `isInfixOf` report
      assertBool "report includes typeChecking count" $ "typeChecking: 1" `isInfixOf` report
      assertBool "report includes total count" $ "total: 3" `isInfixOf` report

  , testCase "formats multiple errors with locations" $ do
      let loc1 = _atFileLocation "file1.typus" 10 5
          loc2 = _atFileLocation "file2.typus" 20 10
          err1 = errorAt "ERR001" "First error" loc1
          err2 = errorAt "ERR002" "Second error" loc2
          formatted = formatErrorsWithLocation [err1, err2]
      assertBool "includes first file location" $ "file1.typus:10:5" `isInfixOf` formatted
      assertBool "includes second file location" $ "file2.typus:20:10" `isInfixOf` formatted
      assertBool "includes both error messages" $ "First error" `isInfixOf` formatted
      assertBool "includes both error messages" $ "Second error" `isInfixOf` formatted
  ]