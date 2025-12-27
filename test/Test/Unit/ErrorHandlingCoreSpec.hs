{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ErrorHandlingCoreSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Compiler.Errors.Core
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T

spec :: Spec
spec = describe "Error Handling Core Functions" $ do
  
  describe "ErrorSeverity properties" $ do
    it "severity priority ordering is correct" $ do
      severityPriority Fatal `shouldBe` 100
      severityPriority Error `shouldBe` 80
      severityPriority Warning `shouldBe` 30
      severityPriority Info `shouldBe` 10
    
    it "severity comparison works correctly" $ do
      compareSeverity Fatal Error `shouldBe` GT
      compareSeverity Error Warning `shouldBe` GT
      compareSeverity Warning Info `shouldBe` GT
      compareSeverity Info Info `shouldBe` EQ
    
    it "severity predicates work correctly" $ do
      _isFatal Fatal `shouldBe` True
      _isFatal Error `shouldBe` False
      
      _isError Error `shouldBe` True
      _isError Warning `shouldBe` False
      
      _isWarning Warning `shouldBe` True
      _isWarning Info `shouldBe` False
      
      _isInfo Info `shouldBe` True
      _isInfo Fatal `shouldBe` False
    
    it "isAtLeast works correctly" $ do
      isAtLeast Error Fatal `shouldBe` True
      isAtLeast Error Error `shouldBe` True
      isAtLeast Error Warning `shouldBe` False
      isAtLeast Error Info `shouldBe` False

  describe "DetailedSeverity properties" $ do
    it "detailed severity priority includes sub-level" $ do
      let criticalFatal = DetailedSeverity Fatal Critical Nothing
          highFatal = DetailedSeverity Fatal High Nothing
          mediumFatal = DetailedSeverity Fatal Medium Nothing
      
      detailedSeverityPriority criticalFatal `shouldSatisfy` (> detailedSeverityPriority highFatal)
      detailedSeverityPriority highFatal `shouldSatisfy` (> detailedSeverityPriority mediumFatal)
    
    it "sub-level predicates work correctly" $ do
      let critical = DetailedSeverity Error Critical Nothing
          high = DetailedSeverity Error High Nothing
          medium = DetailedSeverity Error Medium Nothing
          low = DetailedSeverity Error Low Nothing
          notification = DetailedSeverity Info Notification Nothing
      
      _isCritical critical `shouldBe` True
      _isHigh high `shouldBe` True
      _isMedium medium `shouldBe` True
      _isLow low `shouldBe` True
      _isNotification notification `shouldBe` True

  describe "ErrorLocation functions" $ do
    it "creates location with line and column" $ do
      let loc = _atLocation 10 20
      line loc `shouldBe` 10
      column loc `shouldBe` 20
      filePath loc `shouldBe` Nothing
      endLine loc `shouldBe` Nothing
      endColumn loc `shouldBe` Nothing
    
    it "creates location with file path" $ do
      let loc = _atFileLocation "test.typus" 10 20
      filePath loc `shouldBe` Just "test.typus"
      line loc `shouldBe` 10
      column loc `shouldBe` 20
    
    it "creates location with range" $ do
      let loc = _atRange 10 20 10 30
      line loc `shouldBe` 10
      column loc `shouldBe` 20
      endLine loc `shouldBe` Just 10
      endColumn loc `shouldBe` Just 30
    
    it "helper functions work correctly" $ do
      let loc = _atLocation 15 25
      getErrorLine loc `shouldBe` 15
      getErrorColumn loc `shouldBe` 25

  describe "ErrorRecovery strategies" $ do
    it "fatal recovery has correct properties" $ do
      canRecover fatalRecovery `shouldBe` False
      shouldContinue fatalRecovery `shouldBe` False
      recoveryCost fatalRecovery `shouldBe` 100
      recoveryConfidence fatalRecovery `shouldBe` 0.0
    
    it "error recovery has correct properties" $ do
      canRecover errorRecovery `shouldBe` True
      shouldContinue errorRecovery `shouldBe` True
      recoveryCost errorRecovery `shouldBe` 50
      recoveryConfidence errorRecovery `shouldBe` 0.7
    
    it "warning recovery has correct properties" $ do
      canRecover warningRecovery `shouldBe` True
      shouldContinue warningRecovery `shouldBe` True
      recoveryCost warningRecovery `shouldBe` 10
      recoveryConfidence warningRecovery `shouldBe` 0.9
    
    it "info recovery has correct properties" $ do
      canRecover infoRecovery `shouldBe` True
      shouldContinue infoRecovery `shouldBe` True
      recoveryCost infoRecovery `shouldBe` 0
      recoveryConfidence infoRecovery `shouldBe` 1.0
    
    it "custom recovery creates strategy with given values" $ do
      let custom = customRecovery True False (Just "Retry") (Just "Check network") 25 0.8
      canRecover custom `shouldBe` True
      shouldContinue custom `shouldBe` False
      recoveryAction custom `shouldBe` Just "Retry"
      recoveryHint custom `shouldBe` Just "Check network"
      recoveryCost custom `shouldBe` 25
      recoveryConfidence custom `shouldBe` 0.8

  describe "RecoveryContext operations" $ do
    it "initial recovery context has correct values" $ do
      let ctx = _initialRecoveryContext 3
      recoveryAttempts ctx `shouldBe` 0
      maxRecoveryAttempts ctx `shouldBe` 3
      recoveryHistory ctx `shouldBe` []
      currentStrategy ctx `shouldBe` Nothing
    
    it "adding recovery attempt updates context" $ do
      let initialCtx = _initialRecoveryContext 3
          strategy = errorRecovery
          updatedCtx = _addRecoveryAttempt strategy True initialCtx
      
      recoveryAttempts updatedCtx `shouldBe` 1
      currentStrategy updatedCtx `shouldBe` Just strategy
      recoveryHistory updatedCtx `shouldBe` [(strategy, True)]
    
    it "can recover more when attempts < max" $ do
      let ctx = _initialRecoveryContext 3
      _canRecoverMore ctx `shouldBe` True
      
      let ctx2 = _addRecoveryAttempt errorRecovery True ctx
      _canRecoverMore ctx2 `shouldBe` True
      
      let ctx3 = _addRecoveryAttempt errorRecovery True ctx2
          ctx4 = _addRecoveryAttempt errorRecovery True ctx3
      _canRecoverMore ctx4 `shouldBe` False
    
    it "calculates recovery success rate correctly" $ do
      let ctx = _initialRecoveryContext 5
          ctx1 = _addRecoveryAttempt errorRecovery True ctx
          ctx2 = _addRecoveryAttempt warningRecovery False ctx1
          ctx3 = _addRecoveryAttempt infoRecovery True ctx2
      
      _recoverySuccessRate ctx3 `shouldBe` 2.0/3.0

  describe "Error creation and manipulation" $ do
    it "creates basic error correctly" $ do
      let loc = _atLocation 10 20
          err = errorAt "ERR001" "Test error message" loc
      
      errorId err `shouldBe` "ERR001"
      message err `shouldBe` "Test error message"
      location err `shouldBe` loc
      severity err `shouldBe` Error
      category err `shouldBe` Unknown
      suggestions err `shouldBe` []
      relatedErrors err `shouldBe` []
      errorChain err `shouldBe` []
      timestamp err `shouldBe` Nothing
    
    it "creates error with category correctly" $ do
      let loc = _atLocation 10 20
          err = errorWithCategory "ERR002" TypeChecking "Type mismatch" loc
      
      errorId err `shouldBe` "ERR002"
      category err `shouldBe` TypeChecking
      message err `shouldBe` "Type mismatch"
    
    it "adds timestamp to error" $ do
      let loc = _atLocation 10 20
          err = errorAt "ERR003" "Test error" loc
          timestamped = withTimestamp "2023-01-01 12:00:00" err
      
      timestamp timestamped `shouldBe` Just "2023-01-01 12:00:00"

  describe "Error collection and filtering" $ do
    it "filters errors by severity correctly" $ do
      let loc = _atLocation 1 1
          errors = [ errorAt "E1" "Fatal" loc { severity = Fatal }
                   , errorAt "E2" "Error" loc { severity = Error }
                   , errorAt "E3" "Warning" loc { severity = Warning }
                   , errorAt "E4" "Info" loc { severity = Info }
                   ]
      
      getErrors errors `shouldSatisfy` ((== 2) . length)
      getWarnings errors `shouldSatisfy` ((== 1) . length)
      getInfo errors `shouldSatisfy` ((== 1) . length)
      hasErrors errors `shouldBe` True
      hasWarnings errors `shouldBe` True
    
    it "detects when no errors or warnings exist" $ do
      let loc = _atLocation 1 1
          infoOnly = [errorAt "I1" "Info" loc { severity = Info }]
      
      hasErrors infoOnly `shouldBe` False
      hasWarnings infoOnly `shouldBe` False

  describe "Error context operations" $ do
    it "creates empty context correctly" $ do
      let ctx = emptyContext
      contextCode ctx `shouldBe` Nothing
      contextFunction ctx `shouldBe` Nothing
      contextVariable ctx `shouldBe` Nothing
      contextType ctx `shouldBe` Nothing
      contextAdditional ctx `shouldBe` []
    
    it "creates context with values" $ do
      let ctx = ErrorContext (Just "code") (Just "func") (Just "var") (Just "type") [("key1", "value1")]
      
      contextCode ctx `shouldBe` Just "code"
      contextFunction ctx `shouldBe` Just "func"
      contextVariable ctx `shouldBe` Just "var"
      contextType ctx `shouldBe` Just "type"
      contextAdditional ctx `shouldBe` [("key1", "value1")]

  describe "CombinedError operations" $ do
    it "extracts severity from combined errors" $ do
      let ownershipErr = OwnershipErrorCombined Error undefined
          dependentTypeErr = DependentTypeErrorCombined Warning undefined
          integrationErr = IntegrationError "test" Fatal
          crossAnalyzerErr = CrossAnalyzerError "test" Info []
      
      combinedErrorSeverity ownershipErr `shouldBe` Error
      combinedErrorSeverity dependentTypeErr `shouldBe` Warning
      combinedErrorSeverity integrationErr `shouldBe` Fatal
      combinedErrorSeverity crossAnalyzerErr `shouldBe` Info
    
    it "filters combined errors by severity" $ do
      let errors = [ OwnershipErrorCombined Error undefined
                   , DependentTypeErrorCombined Warning undefined
                   , IntegrationError "test" Fatal
                   , CrossAnalyzerError "test" Info []
                   ]
      
      let filtered = filterCombinedErrorsBySeverity Error errors
      length filtered `shouldBe` 2  -- Error and Fatal

  describe "Error formatting" $ do
    it "formats basic error without location" $ do
      let err = errorAt "ERR001" "Test message" (_atLocation 0 0)
      let formatted = formatError err
      
      formatted `shouldContain` "[ERROR]"
      formatted `shouldContain` "[Unknown]"
      formatted `shouldContain` "Test message"
    
    it "formats error with location" $ do
      let loc = _atFileLocation "test.typus" 10 20
          err = errorAt "ERR001" "Test message" loc
      let formatted = formatErrorWithLocation err
      
      formatted `shouldContain` "test.typus:10:20:"
      formatted `shouldContain` "[ERROR]"
      formatted `shouldContain` "Test message"
    
    it "formats error with suggestions" $ do
      let err = errorAt "ERR001" "Test message" (_atLocation 0 0)
          errWithSuggestions = err { suggestions = ["Suggestion 1", "Suggestion 2"] }
      let formatted = formatError errWithSuggestions
      
      formatted `shouldContain` "Suggestions:"
      formatted `shouldContain` "- Suggestion 1"
      formatted `shouldContain` "- Suggestion 2"
    
    it "formats multiple errors sorted by severity" $ do
      let loc = _atLocation 1 1
          errors = [ errorAt "E1" "Warning" loc { severity = Warning }
                   , errorAt "E2" "Fatal" loc { severity = Fatal }
                   , errorAt "E3" "Error" loc { severity = Error }
                   ]
      let formatted = formatErrors errors
      
      lines formatted `shouldSatisfy` (\ls -> 
        let fatalLine = head $ filter (isInfixOf "FATAL") ls
            errorLine = head $ filter (isInfixOf "ERROR") ls
            warningLine = head $ filter (isInfixOf "WARNING") ls
        in indexOf fatalLine ls < indexOf errorLine ls && 
           indexOf errorLine ls < indexOf warningLine ls)
      where
        indexOf x xs = case elemIndex x xs of Just i -> i; Nothing -> -1
        isInfixOf needle haystack = needle `elem` (substrings haystack)
        substrings [] = []
        substrings s = map (take (length needle)) (tails s)
        needle = ""  -- dummy value