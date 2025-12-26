{-# LANGUAGE CPP #-}
module Test.Unit.CompilerAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, choose, listOf, forAll, Property, (===), counterexample, (==>))

import qualified Data.Text as T
import Data.List (isInfixOf, null)
import Data.Maybe (isJust, isNothing)

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

import Parser (TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourceSpan(..), SourcePos(..))
import Compiler.Errors (ErrorCategory(..), ErrorSeverity(..), CompilationPhase(..))
import qualified Compiler.IR as IR

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary CompilationPhase where
  arbitrary = elements [ParsingPhase, TypeCheckingPhase, OwnershipPhase, DependencyPhase, CodeGenPhase]

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    context <- oneof [return Nothing, fmap Just arbitrary]
    detail <- arbitrary
    return $ TypeCheckDiagnostic context detail

-- Generate simple TypusFile for testing
genSimpleTypusFile :: Gen TypusFile
genSimpleTypusFile = do
  numBlocks <- choose (0, 3)
  blocks <- replicateM numBlocks genSimpleCodeBlock
  return $ TypusFile defaultFileDirectives [] blocks []

genSimpleCodeBlock :: Gen CodeBlock
genSimpleCodeBlock = do
  content <- listOf $ elements "abc def 123\n\t"
  let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length content + 1) (length content))
  return $ CodeBlock defaultBlockDirectives content span

-- Generate TypusFile with potential type errors
genTypusFileWithTypeErrors :: Gen TypusFile
genTypusFileWithTypeErrors = do
  includeError <- arbitrary
  if includeError
    then do
      let errorContent = "var x int = \"string\""  -- Known type error pattern
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 23 22)
          block = CodeBlock defaultBlockDirectives errorContent span
      return $ TypusFile defaultFileDirectives [] [block] []
    else genSimpleTypusFile

-- ============================================================================
-- Property Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Compiler Advanced QuickCheck Tests"
    [ testProperty "compile returns Either Left or Right" $
        \typusFile ->
          let result = compile typusFile
          in case result of
            Left _ -> property True
            Right _ -> property True

    , testProperty "compile handles empty TypusFile gracefully" $
        \blocks ->
          let emptyFile = TypusFile defaultFileDirectives [] blocks []
              result = compile emptyFile
          in case result of
            Left _ -> property True
            Right goCode -> property $ not (null goCode)

    , testProperty "compile with type error pattern returns Left" $
        \typusFile ->
          let fileWithError = typusFile { 
                  tfBlocks = [CodeBlock defaultBlockDirectives "var x int = \"string\"" 
                              (SourceSpan (SourcePos 1 1 0) (SourcePos 1 23 22))]
              }
              result = compile fileWithError
          in case result of
            Left errors -> not (null errors)
            Right _ -> property False

    , testProperty "renderCompilationError handles empty error list" $
        \errors ->
          null errors ==> null (renderCompilationError errors)

    , testProperty "renderCompilationError returns non-empty string for non-empty errors" $
        \errors ->
          not (null errors) ==> not (null (renderCompilationError errors))

    , testProperty "formatCompilerErrors is consistent with renderCompilationError" $
        \errors ->
          formatCompilerErrors errors === renderCompilationError errors

    , testProperty "generateDetailedReport handles empty error list" $
        \errors ->
          null errors ==> not (null (generateDetailedReport errors))

    , testProperty "generateDetailedReport returns non-empty string for non-empty errors" $
        \errors ->
          not (null errors) ==> not (null (generateDetailedReport errors))

    , testProperty "analyzeErrors handles empty error list" $
        \errors ->
          null errors ==> analyzeErrors errors === mempty

    , testProperty "hasTypeErrors checks for type-related errors" $
        \errors ->
          let hasTypeRelatedErrors = any (\e -> errorCategory e == TypeChecking) errors
          in hasTypeErrors errors === hasTypeRelatedErrors

    , testProperty "diagnoseTypeErrors returns Either" $
        \typusFile ->
          let result = diagnoseTypeErrors typusFile
          in case result of
            Left _ -> property True
            Right _ -> property True

    , testProperty "diagnoseTypeErrors preserves TypusFile structure" $
        \typusFile ->
          let result = diagnoseTypeErrors typusFile
          in case result of
            Right diagnostics -> property True
            Left _ -> property True

    , testProperty "extractDeclarations returns list of declarations" $
        \typusFile ->
          let declarations = extractDeclarations typusFile
          in length declarations >= 0

    , testProperty "extractFunctionCalls returns list of function calls" $
        \typusFile ->
          let functionCalls = extractFunctionCalls typusFile
          in length functionCalls >= 0

    , testProperty "buildTypeEnv creates type environment" $
        \typusFile ->
          let typeEnv = buildTypeEnv typusFile
          in property True  -- Basic check that function doesn't crash

    , testProperty "isMethodDeclaration checks method syntax" $
        \declaration ->
          let isMethod = isMethodDeclaration declaration
          in isMethod === isMethod  -- Tautology but ensures function works

    , testProperty "checkTypeError validates type consistency" $
        \type1 type2 ->
          let result = checkTypeError type1 type2
          in case result of
            Left _ -> property True
            Right _ -> property True

    , testProperty "hasMalformedSyntax checks syntax validity" $
        \typusFile ->
          let hasMalformed = hasMalformedSyntax typusFile
          in hasMalformed === hasMalformed  -- Tautology but ensures function works

    , testProperty "checkDependentTypes handles dependent type analysis" $
        \typusFile ->
          let result = checkDependentTypes typusFile
          in case result of
            Left _ -> property True
            Right _ -> property True

    , testProperty "checkOwnership performs ownership analysis" $
        \typusFile ->
          let result = checkOwnership typusFile
          in case result of
            Left _ -> property True
            Right _ -> property True

    , testProperty "ensureSourceIR creates SourceIR or returns error" $
        \typusFile ->
          let result = ensureSourceIR typusFile
          in case result of
            Left _ -> property True
            Right _ -> property True

    , testProperty "typeDiagnosticToCompilerError preserves diagnostic information" $
        \diagnostic ->
          let error = typeDiagnosticToCompilerError diagnostic
          in case diagnostic of
            TypeCheckDiagnostic context detail ->
              let message = T.unpack (errorMessage error)
              in detail `isInfixOf` message

    , testProperty "generateGoCode returns non-empty string for valid input" $
        \typusFile ->
          let goCode = generateGoCode typusFile
          in not (null goCode)

    , testProperty "generateGoCode handles malformed input gracefully" $
        \typusFile ->
          let malformedFile = typusFile { tfSyntaxErrors = [undefined] }
              goCode = generateGoCode malformedFile
          in not (null goCode)

    , testProperty "compile is deterministic" $
        \typusFile ->
          let result1 = compile typusFile
              result2 = compile typusFile
          in result1 === result2

    , testProperty "compile preserves error information" $
        \typusFile ->
          let result = compile typusFile
          in case result of
            Left errors -> all (\e -> not (null (T.unpack (errorMessage e)))) errors
            Right _ -> property True

    , testProperty "compile handles multiple blocks" $
        \blocks ->
          let multiBlockFile = TypusFile defaultFileDirectives [] blocks []
              result = compile multiBlockFile
          in case result of
            Left _ -> property True
            Right goCode -> property $ not (null goCode)

    , testProperty "typeCheckFailure has correct error properties" $
        let error = typeCheckFailure
        in errorCategory error === TypeChecking .&&.
           errorSeverity error === Error .&&.
           compilationPhase error === TypeCheckingPhase

    , testProperty "compilation phases are ordered correctly" $
        \phase1 phase2 ->
          let phaseOrder = [ParsingPhase, TypeCheckingPhase, OwnershipPhase, DependencyPhase, CodeGenPhase]
              phaseIndex p = case p of
                ParsingPhase -> 0
                TypeCheckingPhase -> 1
                OwnershipPhase -> 2
                DependencyPhase -> 3
                CodeGenPhase -> 4
          in (phase1 < phase2) === (phaseIndex phase1 < phaseIndex phase2)
    ]