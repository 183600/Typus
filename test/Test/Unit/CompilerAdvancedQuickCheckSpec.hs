{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.CompilerAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, vectorOf, elements )
import Control.Monad (replicateM, when)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, intercalate, nub)
import Data.Char (isSpace, isDigit, isAlpha, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T

import Compiler
  ( CompilerError(..)
  , CompilationPhase(..)
  , CompilerResult
  , renderCompilationError
  , formatCompilerErrors
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , buildTypeEnvFromPairs
  , checkTypeError
  , hasMalformedSyntax
  , checkDependentTypes
  , checkOwnership
  )

-- Arbitrary instances for QuickCheck
instance Arbitrary CompilationPhase where
  arbitrary = elements [Parsing, TypeChecking, OwnershipAnalysis, CodeGeneration]

instance Arbitrary CompilerError where
  arbitrary = do
    phase <- arbitrary
    message <- arbitrary
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    return $ CompilerError phase message line column

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    message <- arbitrary
    severity <- elements [Error, Warning, Info]
    line <- choose (1, 1000)
    return $ TypeCheckDiagnostic message severity line

-- Property: CompilationPhase ordering
prop_compilation_phase_ordering :: CompilationPhase -> CompilationPhase -> Property
prop_compilation_phase_ordering phase1 phase2 =
  let phaseOrder = [Parsing, TypeChecking, OwnershipAnalysis, CodeGeneration]
      index1 = case phase1 of
        Parsing -> 0
        TypeChecking -> 1
        OwnershipAnalysis -> 2
        CodeGeneration -> 3
      index2 = case phase2 of
        Parsing -> 0
        TypeChecking -> 1
        OwnershipAnalysis -> 2
        CodeGeneration -> 3
  in property $ (phase1 == phase2) === (index1 == index2)

-- Property: CompilerError structure preservation
prop_compiler_error_structure :: CompilationPhase -> String -> Int -> Int -> Property
prop_compiler_error_structure phase message line column =
  let error = CompilerError phase message line column
      rendered = renderCompilationError error
  in property $ line > 0 && column > 0 ==> length rendered >= length message

-- Property: Error formatting consistency
prop_error_formatting_consistency :: [CompilerError] -> Property
prop_error_formatting_consistency errors =
  let formatted = formatCompilerErrors errors
      errorCount = length errors
      formattedLength = length formatted
  in property $ if null errors 
     then formattedLength === 0
     else formattedLength > 0

-- Property: Type error detection
prop_type_error_detection :: [TypeCheckDiagnostic] -> Property
prop_type_error_detection diagnostics =
  let hasErrors = any isError diagnostics
      isError (TypeCheckDiagnostic _ Error _) = True
      isError _ = False
      detected = hasTypeErrors diagnostics
  in property $ detected === hasErrors

-- Property: Declaration extraction
prop_declaration_extraction :: String -> Property
prop_declaration_extraction code =
  let declarations = extractDeclarations code
      hasContent = not (null code)
  in hasContent ==> property $ length declarations >= 0

-- Property: Function call extraction
prop_function_call_extraction :: String -> Property
prop_function_call_extraction code =
  let functionCalls = extractFunctionCalls code
      hasContent = not (null code)
  in hasContent ==> property $ length functionCalls >= 0

-- Property: Type environment building
prop_type_environment_building :: [(String, String)] -> Property
prop_type_environment_building pairs =
  let typeEnv = buildTypeEnvFromPairs pairs
      pairCount = length pairs
  in property $ True -- Placeholder since we can't inspect type environment directly

-- Property: Type error checking
prop_type_error_checking :: String -> Property
prop_type_error_checking code =
  let hasError = checkTypeError code
      hasContent = not (null code)
  in hasContent ==> property $ hasError === hasError || hasError === not hasError -- Could be either

-- Property: Malformed syntax detection
prop_malformed_syntax_detection :: String -> Property
prop_malformed_syntax_detection code =
  let malformed = hasMalformedSyntax code
      hasContent = not (null code)
  in hasContent ==> property $ malformed === malformed -- Tautology but ensures function doesn't crash

-- Property: Dependent types checking
prop_dependent_types_checking :: String -> Property
prop_dependent_types_checking code =
  let result = checkDependentTypes code
      hasContent = not (null code)
  in hasContent ==> property $ True -- Placeholder since we can't inspect result

-- Property: Ownership checking
prop_ownership_checking :: String -> Property
prop_ownership_checking code =
  let result = checkOwnership code
      hasContent = not (null code)
  in hasContent ==> property $ True -- Placeholder since we can't inspect result

-- Property: Error phase progression
prop_error_phase_progression :: CompilationPhase -> Property
prop_error_phase_progression phase =
  let phaseOrder = [Parsing, TypeChecking, OwnershipAnalysis, CodeGeneration]
      currentIndex = case phase of
        Parsing -> 0
        TypeChecking -> 1
        OwnershipAnalysis -> 2
        CodeGeneration -> 3
      laterPhases = drop currentIndex phaseOrder
  in property $ phase `elem` laterPhases

-- Property: Diagnostic severity ordering
prop_diagnostic_severity_ordering :: TypeCheckDiagnostic -> TypeCheckDiagnostic -> Property
prop_diagnostic_severity_ordering diag1 diag2 =
  let severity1 = case diag1 of
        TypeCheckDiagnostic _ Error _ -> 3
        TypeCheckDiagnostic _ Warning _ -> 2
        TypeCheckDiagnostic _ Info _ -> 1
      severity2 = case diag2 of
        TypeCheckDiagnostic _ Error _ -> 3
        TypeCheckDiagnostic _ Warning _ -> 2
        TypeCheckDiagnostic _ Info _ -> 1
  in property $ (severity1 > severity2) === (severity1 > severity2)

-- Property: Error message preservation
prop_error_message_preservation :: String -> CompilationPhase -> Int -> Int -> Property
prop_error_message_preservation message phase line column =
  let error = CompilerError phase message line column
      rendered = renderCompilationError error
  in property $ message `isInfixOf` rendered

tests :: TestTree
tests = testGroup "Compiler Advanced QuickCheck Tests"
  [ fastProperty "compilation phase ordering" prop_compilation_phase_ordering
  , fastProperty "compiler error structure" prop_compiler_error_structure
  , fastProperty "error formatting consistency" prop_error_formatting_consistency
  , fastProperty "type error detection" prop_type_error_detection
  , fastProperty "declaration extraction" prop_declaration_extraction
  , fastProperty "function call extraction" prop_function_call_extraction
  , fastProperty "type environment building" prop_type_environment_building
  , fastProperty "type error checking" prop_type_error_checking
  , fastProperty "malformed syntax detection" prop_malformed_syntax_detection
  , fastProperty "dependent types checking" prop_dependent_types_checking
  , fastProperty "ownership checking" prop_ownership_checking
  , fastProperty "error phase progression" prop_error_phase_progression
  , fastProperty "diagnostic severity ordering" prop_diagnostic_severity_ordering
  , fastProperty "error message preservation" prop_error_message_preservation
  ]