module Test.Unit.CompilerErrorHandlingSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler

-- Test compiler error formatting
prop_error_formatting_consistency :: String -> Property
prop_error_formatting_consistency errorMsg =
  let error = malformedSyntaxError errorMsg
      formatted = renderCompilationError error
  in property $ errorMsg `isInfixOf` formatted

-- Test error analysis
prop_error_analysis_detection :: [String] -> Property
prop_error_analysis_detection errorMessages =
  let errors = map malformedSyntaxError errorMessages
      analysis = analyzeErrors errors
      hasSyntaxErrors = hasMalformedSyntax analysis
  in property $ (not (null errorMessages)) ==> hasSyntaxErrors

-- Test type error diagnostics
prop_type_error_detection :: Bool -> Property
prop_type_error_detection hasErrors =
  let diagnostic = TypeCheckDiagnostic hasErrors [] []
      compilerError = typeDiagnosticToCompilerError diagnostic
      hasTypeErrs = checkTypeError compilerError
  in property $ hasTypeErrs === hasErrors

-- Test compilation phases
prop_compilation_phase_ordering :: Property
prop_compilation_phase_ordering =
  let phases = [minBound .. maxBound] :: [CompilationPhase]
  in property $ length phases === length (nub phases)

-- Helper function
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

tests :: TestTree
tests = testGroup "Compiler Error Handling Tests"
  [ testProperty "error formatting consistency" prop_error_formatting_consistency
  , testProperty "error analysis detection" prop_error_analysis_detection
  , testProperty "type error detection" prop_type_error_detection
  , testProperty "compilation phase ordering" prop_compilation_phase_ordering
  ]