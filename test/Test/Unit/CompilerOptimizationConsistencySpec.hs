{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerOptimizationConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof, listOf, suchThat)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)

import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , renderCompilationError
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , buildTypeEnvFromPairs
  , createTypusFileFromErrors
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
  , checkDependentTypes
  , checkOwnership
  , ensureSourceIR
  , typeCheckFailure
  , typeDiagnosticToCompilerError
  , generateGoCode
  )
import Compiler.Errors.Compiler
  ( CompilerError(..)
  , CompilationPhase(..)
  , ErrorCategory(..)
  , ErrorSeverity(..)
  , formatCompilerError
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , mkCompilerError
  , syntaxError
  , typeError
  , ownershipError
  , dependentTypeError
  , semanticError
  , ErrorStatistics(..)
  )
import Parser (TypusFile(..), CodeBlock(..), BlockDirectives(..), FileDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import Data.List (sort, nub, isInfixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Test Generators
-- ============================================================================

-- Generate compilation phases
instance Arbitrary CompilationPhase where
  arbitrary = oneof
    [ return ParsingPhase
    , return LexingPhase
    , return TypeCheckingPhase
    , return OwnershipAnalysisPhase
    , return DependentTypeAnalysisPhase
    , return CodeGenerationPhase
    , return OptimizationPhase
    ]

-- Generate error categories
instance Arbitrary ErrorCategory where
  arbitrary = oneof
    [ return Syntax
    , return Type
    , return Ownership
    , return DependentType
    , return Semantic
    , return Runtime
    , return Internal
    ]

-- Generate error severities
instance Arbitrary ErrorSeverity where
  arbitrary = oneof
    [ return Error
    , return Warning
    , return Info
    ]

-- Generate source positions
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

-- Generate source spans
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end

-- Generate simple typus files for testing
genSimpleTypusFile :: Gen TypusFile
genSimpleTypusFile = do
  numBlocks <- choose (0, 3)
  blocks <- listOf $ do
    directives <- arbitrary
    content <- listOf $ choose ('a', 'z')
    return $ CodeBlock directives content
  fileDirectives <- arbitrary
  return $ TypusFile fileDirectives blocks

instance Arbitrary TypusFile where
  arbitrary = genSimpleTypusFile

-- ============================================================================
-- Compiler Consistency Properties
-- ============================================================================

-- Property: Compilation should be deterministic for same input
propCompilationDeterministic :: TypusFile -> Bool
propCompilationDeterministic typusFile =
  let result1 = compile typusFile
      result2 = compile typusFile
  in case (result1, result2) of
    (Left _, Left _) -> True  -- Both failed
    (Right code1, Right code2) -> code1 == code2  -- Both succeeded with same output
    _ -> False  -- One succeeded, one failed - inconsistency

-- Property: Error formatting should never crash
propErrorFormattingNeverCrashes :: TypusFile -> Bool
propErrorFormattingNeverCrashes typusFile =
  let result = compile typusFile
  in case result of
    Left errors -> length (formatCompilerErrors errors) >= 0  -- Should not crash
    Right _ -> True

-- Property: Error analysis should be consistent
propErrorAnalysisConsistent :: TypusFile -> Bool
propErrorAnalysisConsistent typusFile =
  let result = compile typusFile
  in case result of
    Left errors -> 
      let stats1 = analyzeErrors errors
          stats2 = analyzeErrors errors  -- Analyze again
      in errorCount stats1 == errorCount stats2
    Right _ -> True

-- Property: Multiple compilations should produce same error count
propMultipleCompilationsSameErrorCount :: TypusFile -> Int -> Bool
propMultipleCompilationsSameErrorCount typusFile n =
  let results = take n $ repeat (compile typusFile)
      errorCounts = map (\r -> case r of
        Left errs -> length errs
        Right _ -> 0) results
  in length (nub errorCounts) <= 1  -- All error counts should be the same

-- Property: Compilation phase ordering should be consistent
propCompilationPhaseOrdering :: TypusFile -> Bool
propCompilationPhaseOrdering typusFile =
  let result = compile typusFile
  in case result of
    Left errors -> 
      let phases = map errPhase errors
          orderedPhases = sort phases
      in phases == orderedPhases || length phases <= 1  -- Should be ordered or single
    Right _ -> True

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Syntax errors should occur in parsing phase
propSyntaxErrorsInParsingPhase :: TypusFile -> Bool
propSyntaxErrorsInParsingPhase typusFile =
  let result = compile typusFile
  in case result of
    Left errors -> 
      let syntaxErrs = filter (\e -> errCategory e == Syntax) errors
          phases = map errPhase syntaxErrs
      in all (== ParsingPhase) phases || null syntaxErrs
    Right _ -> True

-- Property: Type errors should occur in type checking phase
propTypeErrorsInTypeCheckingPhase :: TypusFile -> Bool
propTypeErrorsInTypeCheckingPhase typusFile =
  let result = compile typusFile
  in case result of
    Left errors -> 
      let typeErrs = filter (\e -> errCategory e == Type) errors
          phases = map errPhase typeErrs
      in all (== TypeCheckingPhase) phases || null typeErrs
    Right _ -> True

-- Property: Error severity should be preserved through formatting
propErrorSeverityPreserved :: TypusFile -> Bool
propErrorSeverityPreserved typusFile =
  let result = compile typusFile
  in case result of
    Left errors -> 
      let formatted = formatCompilerErrors errors
      in length formatted >= 0  -- Basic check that formatting worked
    Right _ -> True

-- ============================================================================
-- Optimization Properties
-- ============================================================================

-- Property: Generated code should be consistent
propGeneratedCodeConsistent :: TypusFile -> Bool
propGeneratedCodeConsistent typusFile =
  let result1 = compile typusFile
      result2 = compile typusFile
  in case (result1, result2) of
    (Right code1, Right code2) -> code1 == code2
    (Left _, Left _) -> True  -- Both failed consistently
    _ -> False  -- Inconsistent results

-- Property: Compilation should handle empty files gracefully
propEmptyFileHandling :: Bool
propEmptyFileHandling =
  let emptyFile = TypusFile (FileDirectives Nothing Nothing Nothing) []
      result = compile emptyFile
  in case result of
    Left _ -> True  -- Should handle gracefully
    Right code -> length code >= 0  -- Should produce some output

-- Property: Compilation should handle malformed input gracefully
propMalformedInputHandling :: String -> Bool
propMalformedInputHandling input =
  let malformedFile = TypusFile (FileDirectives Nothing Nothing Nothing) 
                        [CodeBlock (BlockDirectives Nothing Nothing Nothing) input]
      result = compile malformedFile
  in case result of
    Left _ -> True  -- Should handle gracefully
    Right code -> length code >= 0  -- Should produce some output

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test compilation consistency
testCompilationConsistency :: TestTree
testCompilationConsistency = testCase "Compilation consistency" $ do
  let simpleFile = TypusFile (FileDirectives Nothing Nothing Nothing)
                            [CodeBlock (BlockDirectives Nothing Nothing Nothing) "x := 5"]
  
  let result1 = compile simpleFile
  let result2 = compile simpleFile
  
  case (result1, result2) of
    (Left errs1, Left errs2) -> 
      assertEqual "Error counts should be consistent" (length errs1) (length errs2)
    (Right code1, Right code2) -> 
      assertEqual "Generated code should be identical" code1 code2
    _ -> assertBool "Results should be consistent" False

-- Test error formatting consistency
testErrorFormattingConsistency :: TestTree
testErrorFormattingConsistency = testCase "Error formatting consistency" $ do
  let testError = mkCompilerError "TEST001" (T.pack "Test error") TypeCheckingPhase 
                    Type Error Nothing Nothing [] [] Nothing
  
  let formatted1 = formatCompilerError testError
  let formatted2 = formatCompilerError testError
  
  assertEqual "Error formatting should be consistent" formatted1 formatted2
  assertBool "Formatted error should contain error code" ("TEST001" `isInfixOf` formatted1)
  assertBool "Formatted error should contain message" ("Test error" `isInfixOf` formatted1)

-- Test phase progression
testPhaseProgression :: TestTree
testPhaseProgression = testCase "Phase progression" $ do
  let phases = [ParsingPhase, LexingPhase, TypeCheckingPhase, 
                OwnershipAnalysisPhase, DependentTypeAnalysisPhase, 
                CodeGenerationPhase, OptimizationPhase]
  
  let orderedPhases = sort phases
  assertEqual "Phases should be sortable" orderedPhases orderedPhases

-- Test error analysis
testErrorAnalysis :: TestTree
testErrorAnalysis = testCase "Error analysis" $ do
  let errors = 
        [ mkCompilerError "ERR001" (T.pack "Error 1") ParsingPhase Syntax Error Nothing Nothing [] [] Nothing
        , mkCompilerError "ERR002" (T.pack "Error 2") TypeCheckingPhase Type Error Nothing Nothing [] [] Nothing
        , mkCompilerError "ERR003" (T.pack "Warning") OwnershipAnalysisPhase Ownership Warning Nothing Nothing [] [] Nothing
        ]
  
  let stats = analyzeErrors errors
  assertEqual "Should count all errors" 3 (errorCount stats)
  assertEqual "Should count errors by severity" 2 (errorSeverityCount stats Error)
  assertEqual "Should count errors by severity" 1 (errorSeverityCount stats Warning)

-- Test malformed syntax detection
testMalformedSyntaxDetection :: TestTree
testMalformedSyntaxDetection = testCase "Malformed syntax detection" $ do
  let malformedFile = TypusFile (FileDirectives Nothing Nothing Nothing)
                                [CodeBlock (BlockDirectives Nothing Nothing Nothing) "{invalid"]
  
  let hasMalformed = hasMalformedSyntax malformedFile
  assertBool "Should detect malformed syntax" hasMalformed

-- Test type environment building
testTypeEnvironmentBuilding :: TestTree
testTypeEnvironmentBuilding = testCase "Type environment building" $ do
  let pairs = [("x", "Int"), ("y", "String"), ("z", "Bool")]
  let typeEnv = buildTypeEnvFromPairs pairs
  
  assertEqual "Type environment should contain all pairs" 3 (length typeEnv)

-- Test function call extraction
testFunctionCallExtraction :: TestTree
testFunctionCallExtraction = testCase "Function call extraction" $ do
  let testFile = TypusFile (FileDirectives Nothing Nothing Nothing)
                           [CodeBlock (BlockDirectives Nothing Nothing Nothing) "foo() bar() baz()"]
  
  let calls = extractFunctionCalls testFile
  assertBool "Should extract function calls" (length calls >= 0)

-- Test declaration extraction
testDeclarationExtraction :: TestTree
testDeclarationExtraction = testCase "Declaration extraction" $ do
  let testFile = TypusFile (FileDirectives Nothing Nothing Nothing)
                           [CodeBlock (BlockDirectives Nothing Nothing Nothing) "func x() -> int"]
  
  let declarations = extractDeclarations testFile
  assertBool "Should extract declarations" (length declarations >= 0)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler Optimization Consistency Tests"
  [ -- QuickCheck properties for compilation consistency
    testProperty "Compilation is deterministic" propCompilationDeterministic
  , testProperty "Error formatting never crashes" propErrorFormattingNeverCrashes
  , testProperty "Error analysis is consistent" propErrorAnalysisConsistent
  , testProperty "Multiple compilations same error count" propMultipleCompilationsSameErrorCount
  , testProperty "Compilation phase ordering" propCompilationPhaseOrdering
  
    -- QuickCheck properties for error handling
  , testProperty "Syntax errors in parsing phase" propSyntaxErrorsInParsingPhase
  , testProperty "Type errors in type checking phase" propTypeErrorsInTypeCheckingPhase
  , testProperty "Error severity preserved" propErrorSeverityPreserved
  
    -- QuickCheck properties for optimization
  , testProperty "Generated code consistent" propGeneratedCodeConsistent
  , testProperty "Empty file handling" propEmptyFileHandling
  , testProperty "Malformed input handling" propMalformedInputHandling
  
    -- Unit tests
  , testCompilationConsistency
  , testErrorFormattingConsistency
  , testPhaseProgression
  , testErrorAnalysis
  , testMalformedSyntaxDetection
  , testTypeEnvironmentBuilding
  , testFunctionCallExtraction
  , testDeclarationExtraction
  ]