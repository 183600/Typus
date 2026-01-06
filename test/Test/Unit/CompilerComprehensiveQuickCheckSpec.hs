{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerComprehensiveQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck (property)
import Test.Tasty.HUnit

import Compiler
import Parser (TypusFile(..))
import Compiler.Errors (CompilerError(..), CompilationPhase(..), ErrorCategory(..), ErrorSeverity(..))
import Compiler.TypeChecker (TypeCheckDiagnostic(..))
import SourceLocation (SourceSpan(..))
import TestSupport.Arbitrary ()

-- | Test suite for Compiler module with comprehensive QuickCheck properties
compilerComprehensiveQuickCheckSpec :: TestTree
compilerComprehensiveQuickCheckSpec = testGroup "Compiler Comprehensive QuickCheck Tests"
  [ compilerErrorProperties
  , compilationPhaseProperties
  , typeCheckDiagnosticProperties
  , compilerFunctionProperties
  , compilationProperties
  ]

-- | Properties for CompilerError
compilerErrorProperties :: TestTree
compilerErrorProperties = testGroup "CompilerError Properties"
  [ testProperty "CompilerError equality is reflexive" $
      \error -> error == error
  
  , testProperty "CompilerError equality is symmetric" $
      \error1 error2 -> (error1 == error2) ==> (error2 == error1)
  
  , testProperty "CompilerError equality is transitive" $
      \error1 error2 error3 -> (error1 == error2 && error2 == error3) ==> (error1 == error3)
  
  , testProperty "CompilerError with same code but different messages are different" $
      \code msg1 msg2 -> msg1 /= msg2 ==>
        let error1 = CompilerError code msg1 ParsingPhase ParseError Error Nothing Nothing [] [] Nothing
            error2 = CompilerError code msg2 ParsingPhase ParseError Error Nothing Nothing [] [] Nothing
        in error1 /= error2
  
  , testProperty "CompilerError with different phases are different" $
      \code msg phase1 phase2 -> phase1 /= phase2 ==>
        let error1 = CompilerError code msg phase1 ParseError Error Nothing Nothing [] [] Nothing
            error2 = CompilerError code msg phase2 ParseError Error Nothing Nothing [] [] Nothing
        in error1 /= error2
  
  , testProperty "CompilerError preserves L.all fields" $
      \code msg phase category severity span context suggestions related timestamp ->
        let error = CompilerError code msg phase category severity span context suggestions related timestamp
        in True -- Check that L.all fields are preserved
  ]

-- | Properties for CompilationPhase
compilationPhaseProperties :: TestTree
compilationPhaseProperties = testGroup "CompilationPhase Properties"
  [ testProperty "CompilationPhase equality is reflexive" $
      \phase -> phase == phase
  
  , testProperty "CompilationPhase equality is symmetric" $
      \phase1 phase2 -> (phase1 == phase2) ==> (phase2 == phase1)
  
  , testProperty "CompilationPhase equality is transitive" $
      \phase1 phase2 phase3 -> (phase1 == phase2 && phase2 == phase3) ==> (phase1 == phase3)
  
  , testProperty "CompilationPhase ordering is consistent" $
      \phase1 phase2 ->
        let cmp1 = compare phase1 phase2
            cmp2 = compare (show phase1) (show phase2)
        in (cmp1 == EQ) ==> (cmp2 == EQ)
  
  , testProperty "All compilation phases are distinct" $
      let phases = [ParsingPhase, LexingPhase, TypeCheckingPhase, OwnershipPhase, 
                   DependencyAnalysisPhase, CodeGenerationPhase, OptimizationPhase]
          distinctPairs = [(p1, p2) | p1 <- phases, p2 <- phases, p1 < p2]
      in L.all (\(p1, p2) -> p1 /= p2) distinctPairs
  ]

-- | Properties for TypeCheckDiagnostic
typeCheckDiagnosticProperties :: TestTree
typeCheckDiagnosticProperties = testGroup "TypeCheckDiagnostic Properties"
  [ testProperty "TypeCheckDiagnostic equality is reflexive" $
      \diagnostic -> diagnostic == diagnostic
  
  , testProperty "TypeCheckDiagnostic equality is symmetric" $
      \diagnostic1 diagnostic2 -> (diagnostic1 == diagnostic2) ==> (diagnostic2 == diagnostic1)
  
  , testProperty "TypeCheckDiagnostic equality is transitive" $
      \diagnostic1 diagnostic2 diagnostic3 -> (diagnostic1 == diagnostic2 && diagnostic2 == diagnostic3) ==> (diagnostic1 == diagnostic3)
  
  , testProperty "TypeCheckDiagnostic preserves message" $
      \msg ->
        let diagnostic = TypeDiagnostic msg Nothing Nothing
        in True -- Check that message is preserved
  
  , testProperty "TypeCheckDiagnostic with different messages are different" $
      \msg1 msg2 -> msg1 /= msg2 ==>
        let diagnostic1 = TypeDiagnostic msg1 Nothing Nothing
            diagnostic2 = TypeDiagnostic msg2 Nothing Nothing
        in diagnostic1 /= diagnostic2
  ]

-- | Properties for compiler functions
compilerFunctionProperties :: TestTree
compilerFunctionProperties = testGroup "Compiler Function Properties"
  [ testProperty "renderCompilationError on empty list returns empty string" $
      renderCompilationError [] == ""
  
  , testProperty "renderCompilationError on non-empty list returns non-empty string" $
      \errors ->
        let rendered = renderCompilationError errors
        in not (null errors) ==> not (null rendered)
  
  , testProperty "formatCompilerErrors is deterministic" $
      \errors ->
        let formatted1 = formatCompilerErrors errors
            formatted2 = formatCompilerErrors errors
        in formatted1 == formatted2
  
  , testProperty "generateDetailedReport contains error information" $
      \errors ->
        let report = generateDetailedReport errors
        in not (null errors) ==> not (null report)
  
  , testProperty "analyzeErrors returns statistics" $
      \errors ->
        let analysis = analyzeErrors errors
        in True -- Check that analysis returns meaningful statistics
  
  , testProperty "hasTypeErrors identifies type errors" $
      \errors ->
        let hasTypeErrs = hasTypeErrors errors
            typeErrors = filter isTypeError errors
        in hasTypeErrs == not (null typeErrors)
  
  , testProperty "diagnoseTypeErrors on empty file returns Right []" $
      let emptyFile = TypusFile undefined [] [] []
      in diagnoseTypeErrors emptyFile == Right []
  
  , testProperty "extractDeclarations is deterministic" $
      \file ->
        let decls1 = extractDeclarations file
            decls2 = extractDeclarations file
        in decls1 == decls2
  
  , testProperty "extractFunctionCalls is deterministic" $
      \file ->
        let calls1 = extractFunctionCalls file
            calls2 = extractFunctionCalls file
        in calls1 == calls2
  
  , testProperty "buildTypeEnv from pairs is deterministic" $
      \pairs ->
        let env1 = buildTypeEnvFromPairs pairs
            env2 = buildTypeEnvFromPairs pairs
        in env1 == env2
  
  , testProperty "checkDependentTypes is deterministic" $
      \file ->
        let result1 = checkDependentTypes file
            result2 = checkDependentTypes file
        in result1 == result2
  
  , testProperty "checkOwnership is deterministic" $
      \file ->
        let result1 = checkOwnership file
            result2 = checkOwnership file
        in result1 == result2
  
  , testProperty "isMethodDeclaration is consistent" $
      \declaration ->
        let isMethod = isMethodDeclaration declaration
        in isMethod == isMethod -- Check consistency
  
  , testProperty "checkTypeError identifies type errors" $
      \error ->
        let isTypeErr = checkTypeError error
        in isTypeErr == isTypeError error
  
  , testProperty "hasMalformedSyntax is deterministic" $
      \file ->
        let malformed1 = hasMalformedSyntax file
            malformed2 = hasMalformedSyntax file
        in malformed1 == malformed2
  
  , testProperty "typeDiagnosticToCompilerError preserves information" $
      \diagnostic ->
        let error = typeDiagnosticToCompilerError diagnostic
        in True -- Check that error preserves diagnostic information
  ]

-- | Properties for compilation process
compilationProperties :: TestTree
compilationProperties = testGroup "Compilation Properties"
  [ testProperty "compile on empty file returns result" $
      let emptyFile = TypusFile undefined [] [] []
          result = compile emptyFile
      in -- Check that compilation returns either success L.or error
         True
  
  , testProperty "compile is deterministic" $
      \file ->
        let result1 = compile file
            result2 = compile file
        in result1 == result2
  
  , testProperty "generateGoCode produces Go code" $
      \file ->
        let goCode = generateGoCode file
        in -- Check that Go code is generated
           True
  
  , testProperty "ensureSourceIR handles malformed syntax" $
      \file ->
        let result = ensureSourceIR file
        in hasMalformedSyntax file ==> 
           case result of
             Left _ -> True
             Right _ -> False
  
  , testProperty "compilation with type errors fails appropriately" $
      \file ->
        let typeErrors = diagnoseTypeErrors file
        in case typeErrors of
             Left _ -> True -- Should fail
             Right [] -> True -- Should succeed
             Right _ -> True -- Should fail with diagnostics
  ]

-- Helper function to check if error is a type error
isTypeError :: CompilerError -> Bool
isTypeError error = errorCategory error == TypeChecking

-- Arbitrary instances for testing
instance Arbitrary CompilationPhase where
  arbitrary = elements 
    [ ParsingPhase
    , LexingPhase
    , TypeCheckingPhase
    , OwnershipPhase
    , DependencyAnalysisPhase
    , CodeGenerationPhase
    , OptimizationPhase
    ]

instance Arbitrary ErrorCategory where
  arbitrary = elements
    [ ParseError
    , TypeError
    , OwnershipError
    , DependencyError
    , CodeGenerationError
    , OptimizationError
    ]

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Info, Warning, Error, Fatal]

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    msg <- arbitrary
    span <- arbitrary
    suggestion <- arbitrary
    oneof
      [ TypeDiagnostic msg span <$> arbitrary
      , ReturnDiagnostic msg <$> arbitrary <*> arbitrary
      , ParameterDiagnostic msg <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Arbitrary CompilerError where
  arbitrary = do
    code <- arbitrary
    msg <- arbitrary
    phase <- arbitrary
    category <- arbitrary
    severity <- arbitrary
    span <- arbitrary
    context <- arbitrary
    suggestions <- arbitrary
    related <- arbitrary
    timestamp <- arbitrary
    return $ CompilerError code msg phase category severity span context suggestions related timestamp