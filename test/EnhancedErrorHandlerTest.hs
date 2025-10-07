{-# LANGUAGE OverloadedStrings #-}

module EnhancedErrorHandlerTest where

import Test.Hspec
import EnhancedErrorHandler
import SourceLocation
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- Test Suite for Enhanced Error Handler
-- ============================================================================

main :: IO ()
main = hspec $ do
    describe "Error Construction" $ do
        testErrorConstruction
    
    describe "Error Formatting" $ do
        testErrorFormatting
    
    describe "Error Recovery" $ do
        testErrorRecovery
    
    describe "Location Tracking" $ do
        testLocationTracking
    
    describe "Error Statistics" $ do
        testErrorStatistics
    
    describe "User-Friendly Messages" $ do
        testUserFriendlyMessages

-- ============================================================================
-- Error Construction Tests
-- ============================================================================

testErrorConstruction :: Spec
testErrorConstruction = do
    it "creates syntax error with location" $ do
        let err = syntaxError "E001" "Missing semicolon" (posAt 10 5)
        severity (ceError err) `shouldBe` Error
        category (ceError err) `shouldBe` Parsing
        cePhase err `shouldBe` ParsingPhase
    
    it "creates type error with suggestions" $ do
        let span = spanBetween (posAt 15 10) (posAt 15 25)
            err = typeError "E002" "Type mismatch" span Nothing 
                    ["Use type conversion", "Check variable type"]
        length (suggestions (ceError err)) `shouldBe` 2
        category (ceError err) `shouldBe` TypeChecking
    
    it "creates ownership error with context" $ do
        let span = spanBetween (posAt 20 1) (posAt 20 30)
            code = "x := []int{1,2,3}\ny := x"
            err = ownershipError "E003" "Value moved" span code []
        ceSourceContext err `shouldBe` Just code
        category (ceError err) `shouldBe` Ownership
    
    it "creates fatal error that stops compilation" $ do
        let err = fatalError "E999" "Critical failure" (toErrorLocation (posAt 1 1))
        severity err `shouldBe` Fatal
        canRecover (recovery err) `shouldBe` False

-- ============================================================================
-- Error Formatting Tests
-- ============================================================================

testErrorFormatting :: Spec
testErrorFormatting = do
    it "formats simple error" $ do
        let err = syntaxError "E001" "Test error" (posAt 10 5)
            formatted = formatCompilerError err
        formatted `shouldContain` "10:5"
        formatted `shouldContain` "Test error"
        formatted `shouldContain` "ParsingPhase"
    
    it "formats error with source context" $ do
        let span = spanBetween (posAt 5 1) (posAt 5 20)
            code = "var x int = \"hello\""
            err = typeError "E002" "Type mismatch" span (Just code) []
            formatted = formatCompilerError err
        formatted `shouldContain` code
        formatted `shouldContain` "Source Context"
    
    it "formats multiple errors grouped by phase" $ do
        let err1 = syntaxError "E001" "Parse error" (posAt 1 1)
            err2 = typeError "E002" "Type error" (spanFrom (posAt 2 1)) Nothing []
            err3 = syntaxError "E003" "Another parse error" (posAt 3 1)
            formatted = formatCompilerErrors [err1, err2, err3]
        formatted `shouldContain` "ParsingPhase"
        formatted `shouldContain` "TypeCheckingPhase"
    
    it "includes recovery information" $ do
        let err = syntaxError "E001" "Recoverable error" (posAt 1 1)
            formatted = formatCompilerError err
        formatted `shouldContain` "Recoverable"
        formatted `shouldContain` "compilation can continue"

-- ============================================================================
-- Error Recovery Tests
-- ============================================================================

testErrorRecovery :: Spec
testErrorRecovery = do
    it "allows recovery from non-fatal errors" $ do
        let err = syntaxError "E001" "Syntax error" (posAt 1 1)
        canRecoverFrom err `shouldBe` True
        shouldContinueAfter err `shouldBe` True
    
    it "prevents recovery from fatal errors" $ do
        let err = fatalError "E999" "Fatal error" (toErrorLocation (posAt 1 1))
        canRecoverFrom (CompilerError err Nothing [] ParsingPhase) `shouldBe` False
    
    it "provides recovery actions" $ do
        let rec = retryRecovery 3
        recoveryAction rec `shouldSatisfy` (\x -> case x of
            Just action -> "Retry" `T.isInfixOf` T.pack action
            Nothing -> False)
    
    it "calculates recovery confidence" $ do
        let rec1 = skipRecovery
            rec2 = manualRecovery "Fix manually"
        recoveryConfidence rec1 `shouldSatisfy` (> 0.9)
        recoveryConfidence rec2 `shouldSatisfy` (< 0.6)

-- ============================================================================
-- Location Tracking Tests
-- ============================================================================

testLocationTracking :: Spec
testLocationTracking = do
    it "tracks line and column" $ do
        let pos = posAt 10 5
        posLine pos `shouldBe` 10
        posColumn pos `shouldBe` 5
    
    it "creates spans between positions" $ do
        let start = posAt 5 1
            end = posAt 5 20
            span = spanBetween start end
        posLine (spanStart span) `shouldBe` 5
        posColumn (spanStart span) `shouldBe` 1
        posColumn (spanEnd span) `shouldBe` 20
    
    it "converts to error location" $ do
        let pos = posAt 15 10
            errLoc = toErrorLocation pos
        line errLoc `shouldBe` 15
        column errLoc `shouldBe` 10
    
    it "converts span to error location with range" $ do
        let span = spanBetween (posAt 5 1) (posAt 7 30)
            errLoc = toErrorLocationWithSpan span
        line errLoc `shouldBe` 5
        column errLoc `shouldBe` 1
        endLine errLoc `shouldBe` Just 7
        endColumn errLoc `shouldBe` Just 30
    
    it "merges overlapping spans" $ do
        let span1 = spanBetween (posAt 5 1) (posAt 7 10)
            span2 = spanBetween (posAt 6 5) (posAt 8 20)
            merged = mergeSpans span1 span2
        posLine (spanStart merged) `shouldBe` 5
        posLine (spanEnd merged) `shouldBe` 8

-- ============================================================================
-- Error Statistics Tests
-- ============================================================================

testErrorStatistics :: Spec
testErrorStatistics = do
    it "counts errors by severity" $ do
        let err1 = syntaxError "E001" "Error 1" (posAt 1 1)
            err2 = syntaxError "E002" "Error 2" (posAt 2 1)
            warn1 = CompilerError
                { ceError = warningAt "W001" "Warning 1" (toErrorLocation (posAt 3 1))
                , ceSourceContext = Nothing
                , ceStackTrace = []
                , cePhase = ParsingPhase
                }
            stats = analyzeErrors [err1, err2, warn1]
        esTotal stats `shouldBe` 3
        esErrors stats `shouldBe` 2
        esWarnings stats `shouldBe` 1
    
    it "groups errors by phase" $ do
        let err1 = syntaxError "E001" "Parse error" (posAt 1 1)
            err2 = typeError "E002" "Type error" (spanFrom (posAt 2 1)) Nothing []
            err3 = syntaxError "E003" "Another parse error" (posAt 3 1)
            stats = analyzeErrors [err1, err2, err3]
        lookup ParsingPhase (esByPhase stats) `shouldBe` Just 2
        lookup TypeCheckingPhase (esByPhase stats) `shouldBe` Just 1
    
    it "groups errors by category" $ do
        let err1 = syntaxError "E001" "Parse error" (posAt 1 1)
            err2 = typeError "E002" "Type error" (spanFrom (posAt 2 1)) Nothing []
            stats = analyzeErrors [err1, err2]
        lookup Parsing (esByCategory stats) `shouldBe` Just 1
        lookup TypeChecking (esByCategory stats) `shouldBe` Just 1
    
    it "counts recoverable errors" $ do
        let err1 = syntaxError "E001" "Recoverable" (posAt 1 1)
            err2 = CompilerError
                { ceError = fatalError "E999" "Fatal" (toErrorLocation (posAt 2 1))
                , ceSourceContext = Nothing
                , ceStackTrace = []
                , cePhase = ParsingPhase
                }
            stats = analyzeErrors [err1, err2]
        esRecoverable stats `shouldBe` 1

-- ============================================================================
-- User-Friendly Messages Tests
-- ============================================================================

testUserFriendlyMessages :: Spec
testUserFriendlyMessages = do
    it "simplifies technical messages" $ do
        let msg = "Malformed syntax in expression"
            simplified = simplifyMessage (T.pack msg)
        simplified `shouldSatisfy` T.isPrefixOf "Syntax error:"
    
    it "provides helpful suggestions" $ do
        let err = syntaxError "E001" "Syntax error" (posAt 1 1)
            suggestions = suggestFix err
        suggestions `shouldSatisfy` (not . null)
        head suggestions `shouldSatisfy` T.isPrefixOf "Check"
    
    it "adds emoji to suggestions" $ do
        let original = ["Check syntax", "Add semicolon"]
            improved = improveSuggestions (map T.pack original)
        all (T.isPrefixOf "💡 ") improved `shouldBe` True
    
    it "generates category-specific suggestions" $ do
        let parseErr = syntaxError "E001" "Parse error" (posAt 1 1)
            typeErr = typeError "E002" "Type error" (spanFrom (posAt 2 1)) Nothing []
            parseSuggestions = suggestFix parseErr
            typeSuggestions = suggestFix typeErr
        parseSuggestions `shouldSatisfy` any (T.isInfixOf "brackets")
        typeSuggestions `shouldSatisfy` any (T.isInfixOf "type")

-- ============================================================================
-- Integration Tests
-- ============================================================================

testIntegration :: Spec
testIntegration = do
    it "generates comprehensive error report" $ do
        let err1 = syntaxError "E001" "Parse error" (posAt 1 1)
            err2 = typeError "E002" "Type error" (spanFrom (posAt 2 1)) Nothing []
            report = generateDetailedReport [err1, err2]
        report `shouldContain` "Error Summary"
        report `shouldContain` "Total Errors: 2"
        report `shouldContain` "Recommendations"
    
    it "tracks compilation phases" $ do
        let err1 = syntaxError "E001" "Parse error" (posAt 1 1)
            err2 = typeError "E002" "Type error" (spanFrom (posAt 2 1)) Nothing []
            err3 = ownershipError "E003" "Ownership error" 
                    (spanFrom (posAt 3 1)) "code" []
        cePhase err1 `shouldBe` ParsingPhase
        cePhase err2 `shouldBe` TypeCheckingPhase
        cePhase err3 `shouldBe` OwnershipAnalysisPhase
    
    it "maintains error chain" $ do
        let innerErr = syntaxError "E001" "Inner error" (posAt 1 1)
            wrappedErr = wrapError "Outer context" (ceError innerErr)
        length (errorChain wrappedErr) `shouldBe` 1

-- ============================================================================
-- Example Error Scenarios
-- ============================================================================

testExampleScenarios :: Spec
testExampleScenarios = do
    it "handles missing semicolon" $ do
        let err = syntaxError "E001" "Missing semicolon" (posAt 10 25)
            formatted = formatCompilerError err
        formatted `shouldContain` "10:25"
        formatted `shouldContain` "Missing semicolon"
    
    it "handles type mismatch with context" $ do
        let code = "var x int = \"hello\""
            span = spanBetween (posAt 5 13) (posAt 5 20)
            err = typeError "E002" "Cannot assign string to int" 
                    span (Just code) 
                    ["Use strconv.Atoi to convert", "Change variable type to string"]
            formatted = formatCompilerError err
        formatted `shouldContain` code
        formatted `shouldContain` "strconv.Atoi"
    
    it "handles ownership violation" $ do
        let code = "x := []int{1,2,3}\ny := &x\nz := x"
            span = spanBetween (posAt 20 5) (posAt 20 10)
            err = ownershipError "E003" "Value moved while borrowed"
                    span code ["Use reference instead", "Clone the slice"]
            formatted = formatCompilerError err
        formatted `shouldContain` "moved while borrowed"
        formatted `shouldContain` "Source Context"
    
    it "handles dependent type constraint violation" $ do
        let span = spanBetween (posAt 15 10) (posAt 15 25)
            err = dependentTypeError "E004" "Array index out of bounds"
                    span ["Add bounds check", "Use safe indexing"]
            formatted = formatCompilerError err
        formatted `shouldContain` "bounds"
        length (suggestions (ceError err)) `shouldBe` 2

-- ============================================================================
-- Helper Functions for Testing
-- ============================================================================

shouldContain :: String -> String -> Expectation
shouldContain haystack needle = 
    haystack `shouldSatisfy` \h -> needle `elem` words h || 
                                   needle `T.isInfixOf` T.pack h

-- Run all tests
runAllTests :: IO ()
runAllTests = hspec $ do
    testErrorConstruction
    testErrorFormatting
    testErrorRecovery
    testLocationTracking
    testErrorStatistics
    testUserFriendlyMessages
    testIntegration
    testExampleScenarios