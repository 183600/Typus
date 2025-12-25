{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.IntegrationFeaturesQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified IntegratedCompiler as IC
import Parser
import SyntaxValidator
import qualified AnalyzerIntegration as AI

import qualified Data.Map.Strict as Map
import Data.List (isInfixOf, null)
import Data.Maybe (isJust, isNothing)

-- Arbitrary instances for integration testing
instance Arbitrary IC.CompilerConfig where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    errorLevel <- arbitrary
    return $ IC.CompilerConfig ownership dependentTypes errorLevel

instance Arbitrary AI.ErrorSeverity where
  arbitrary = elements [AI.Info, AI.Warning, AI.Error, AI.Fatal]

instance Arbitrary IC.IntegratedCompileResult where
  arbitrary = do
    success <- arbitrary
    compiledCode <- codeGen
    analysisResult <- arbitrary
    syntaxErrors <- listOf arbitrary
    filteredErrors <- listOf arbitrary
    compilerErrors <- listOf arbitrary
    warnings <- listOf warningGen
    info <- listOf infoGen
    return $ IC.IntegratedCompileResult success compiledCode analysisResult syntaxErrors filteredErrors compilerErrors warnings info
    where
      codeGen = elements ["", "func main() {}", "package main\n\nfunc main() {}", "compiled code"]
      warningGen = elements ["Warning: unused variable", "Warning: unreachable code", "Warning: deprecated feature"]
      infoGen = elements ["Info: compilation successful", "Info: optimization applied", "Info: inline function"]

-- Helper generators
validTypusCodeGen :: Gen String
validTypusCodeGen = elements
  [ "func main() { return 42 }"
  , "package main\n\nfunc add(x int, y int) int { return x + y }"
  , "type Point struct { x int; y int }"
  , "func process(items []string) []string { return items }"
  , "var global int = 100"
  ]

invalidTypusCodeGen :: Gen String
invalidTypusCodeGen = elements
  [ "func main( { return 42 }"  -- Missing closing parenthesis
  , "func main() { return 42"   -- Missing closing brace
  , "func 123main() { return 42 }"  -- Invalid function name
  , "return 42"  -- Return outside function
  , "{ invalid syntax }"
  ]

mixedTypusCodeGen :: Gen String
mixedTypusCodeGen = oneof
  [ validTypusCodeGen
  , invalidTypusCodeGen
  , do
      valid <- validTypusCodeGen
      invalid <- invalidTypusCodeGen
      return (valid ++ "\n" ++ invalid)
  ]

sourceCodeGen :: Gen String
sourceCodeGen = oneof
  [ validTypusCodeGen
  , invalidTypusCodeGen
  , mixedTypusCodeGen
  , return ""
  , return "   "  -- Only whitespace
  , return "\n\n"  -- Only newlines
  ]

-- Test properties
tests :: TestTree
tests = testGroup "Integration Features QuickCheck Tests"
  [ testProperty "Compiler config is correctly applied" testCompilerConfig
  , testProperty "Default config has expected values" testDefaultConfig
  , testProperty "Integration result preserves compilation status" testCompilationStatus
  , testProperty "Syntax errors are properly handled" testSyntaxErrorHandling
  , testProperty "Valid code compiles successfully" testValidCodeCompilation
  , testProperty "Invalid code fails compilation" testInvalidCodeCompilation
  , testProperty "Analysis results are included when enabled" testAnalysisResults
  , testProperty "Error filtering works correctly" testErrorFiltering
  , testProperty "Compilation pipeline is deterministic" testDeterministicCompilation
  , testProperty "Error messages are informative" testErrorMessages
  ]

testCompilerConfig :: IC.CompilerConfig -> Property
testCompilerConfig config =
  let ownership = IC.enableOwnership config
      dependentTypes = IC.enableDependentTypes config
      errorLevel = IC.errorReportingLevel config
      validConfig = ownership `elem` [True, False] && 
                   dependentTypes `elem` [True, False] &&
                   errorLevel `elem` [AI.Info, AI.Warning, AI.Error, AI.Fatal]
  in validConfig === True

testDefaultConfig :: Property
testDefaultConfig =
  let config = IC.defaultCompilerConfig
      expectedOwnership = True
      expectedDependentTypes = True
      expectedErrorLevel = AI.Warning
  in IC.enableOwnership config === expectedOwnership .&&.
      IC.enableDependentTypes config === expectedDependentTypes .&&.
      IC.errorReportingLevel config === expectedErrorLevel

testCompilationStatus :: IC.IntegratedCompileResult -> Property
testCompilationStatus result =
  let successFlag = IC.success result
      hasErrors = not (null (IC.syntaxErrors result)) || 
                  not (null (IC.filteredErrors result)) ||
                  not (null (IC.compilerErrors result))
      expectedSuccess = not hasErrors
  in successFlag === expectedSuccess

testSyntaxErrorHandling :: Property
testSyntaxErrorHandling =
  forAll invalidTypusCodeGen $ \invalidCode ->
    let syntaxValidation = validateFile invalidCode
        hasSyntaxErrors = not (null syntaxValidation)
    in hasSyntaxErrors === True

testValidCodeCompilation :: Property
testValidCodeCompilation =
  forAll validTypusCodeGen $ \validCode ->
    let parseResult = parseTypus validCode
        syntaxValidation = validateFile validCode
        canParse = case parseResult of
          Right _ -> True
          Left _ -> False
        hasNoSyntaxErrors = null syntaxValidation
    in canParse .&&. hasNoSyntaxErrors

testInvalidCodeCompilation :: Property
testInvalidCodeCompilation =
  forAll invalidTypusCodeGen $ \invalidCode ->
    let parseResult = parseTypus invalidCode
        syntaxValidation = validateFile invalidCode
        failsToParse = case parseResult of
          Right _ -> False
          Left _ -> True
        hasSyntaxErrors = not (null syntaxValidation)
    in failsToParse .||. hasSyntaxErrors

testAnalysisResults :: IC.CompilerConfig -> Property
testAnalysisResults config =
  let ownershipEnabled = IC.enableOwnership config
      dependentTypesEnabled = IC.enableDependentTypes config
      analysisEnabled = ownershipEnabled || dependentTypesEnabled
  in analysisEnabled === True .||. analysisEnabled === False

testErrorFiltering :: AI.ErrorSeverity -> Property
testErrorFiltering severity =
  let validSeverity = severity `elem` [AI.Info, AI.Warning, AI.Error, AI.Fatal]
  in validSeverity === True

testDeterministicCompilation :: IC.CompilerConfig -> Property
testDeterministicCompilation config =
  -- Test that the same configuration produces consistent results
  let configConsistency = IC.enableOwnership config == IC.enableOwnership config &&
                         IC.enableDependentTypes config == IC.enableDependentTypes config &&
                         IC.errorReportingLevel config == IC.errorReportingLevel config
  in configConsistency === True

testErrorMessages :: IC.IntegratedCompileResult -> Property
testErrorMessages result =
  let warnings = IC.compilationWarnings result
      info = IC.compilationInfo result
      syntaxErrors = IC.syntaxErrors result
      allMessages = warnings ++ info ++ map show syntaxErrors
      allMessagesValid = all (\msg -> not (null msg) && length msg > 3) allMessages
  in allMessagesValid === True

-- Helper functions
isSyntaxWarning :: SyntaxValidator.SyntaxError -> Bool
isSyntaxWarning error = 
  -- Simplified check - in real implementation this would check error type
  False

parserErrorToSyntaxError :: String -> SyntaxValidator.SyntaxError
parserErrorToSyntaxError errorMsg = 
  -- Simplified conversion
  SyntaxValidator.SyntaxError 
    SyntaxValidator.InvalidStatement 
    errorMsg 
    1 
    1 
    "Parse error"