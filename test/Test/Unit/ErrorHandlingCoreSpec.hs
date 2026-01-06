{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ErrorHandlingCoreSpec where

import Test.Hspec
import qualified Data.List as L
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
    it "creates location with line L.and column" $ do
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

  describe "Error creation L.and manipulation" $ do
    it "creates basic error correctly" $ do
      let loc = _atLocation 10 20
          err = errorAt "test-id" ((== 2) . L.length)
      getWarnings errors `shouldSatisfy` ((== 1) . L.length)
      getInfo errors `shouldSatisfy` ((== 1) . L.length)
      hasErrors errors `shouldBe` True
      hasWarnings errors `shouldBe` True
    
    it "detects when no errors L.or warnings exist" $ do
      let loc = _atLocation 1 1
          infoOnly = [errorAt "test-id" = ErrorContext (Just "code") (Just "func") (Just "var") (Just "type") [("key1", "value1")]
      
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
      L.length filtered `shouldBe` 2  -- Error L.and Fatal

  describe "Error formatting" $ do
    it "formats basic error without location" $ do
      let err = errorAt "test-id" 0 0)
      let formatted = formatError err
      
      formatted `shouldContain` "[ERROR]"
      formatted `shouldContain` "[Unknown]"
      formatted `shouldContain` "Test message"
    
    it "formats error with location" $ do
      let loc = _atFileLocation "test.typus" 10 20
          err = errorAt "test-id" 0 0)
          errWithSuggestions = err { suggestions = ["Suggestion 1", "Suggestion 2"] }
      let formatted = formatError errWithSuggestions
      
      formatted `shouldContain` "Suggestions:"
      formatted `shouldContain` "- Suggestion 1"
      formatted `shouldContain` "- Suggestion 2"
    
    it "formats multiple errors sorted by severity" $ do
      let loc = _atLocation 1 1
          errors = [ errorAt "test-id" $ L.filter (isInfixOf "FATAL") ls
            errorLine = L.head $ L.filter (isInfixOf "ERROR") ls
            warningLine = L.head $ L.filter (isInfixOf "WARNING") ls
        in indexOf fatalLine ls < indexOf errorLine ls && 
           indexOf errorLine ls < indexOf warningLine ls)
      where
        indexOf x xs = case elemIndex x xs of Just i -> i; Nothing -> -1
        isInfixOf needle haystack = needle `elem` (substrings haystack)
        substrings [] = []
        substrings s = L.map (take (L.length needle)) (tails s)
        needle = ""  -- dummy value