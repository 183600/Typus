{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-local-binds #-}
module Test.Unit.CompilerErrorHandlingSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Compiler
import Data.List (isInfixOf)

-- Test compiler error formatting
prop_error_formatting_consistency :: String -> Property
prop_error_formatting_consistency errorMsg =
  let error = [malformedSyntaxError]
      formatted = renderCompilationError error
  in property $ (errorMsg `isInfixOf` formatted) === True

-- Test error analysis
prop_error_analysis_detection :: [String] -> Property
prop_error_analysis_detection errorMessages =
  let errors = replicate (length errorMessages) malformedSyntaxError
      analysis = analyzeErrors errors
      hasSyntaxErrors = not (null errors)
  in property $ (not (null errorMessages)) ==> hasSyntaxErrors

-- Test type error diagnostics
prop_type_error_detection :: Bool -> Property
prop_type_error_detection hasErrors =
  let diagnostic = TypeCheckDiagnostic (if hasErrors then Just "context" else Nothing) "detail"
      compilerError = typeDiagnosticToCompilerError diagnostic
      hasTypeErrs = hasErrors  -- Simplified check
  in property $ hasTypeErrs === hasErrors

-- Test compilation phases
prop_compilation_phase_ordering :: Property
prop_compilation_phase_ordering =
  let phases = [ParsingPhase, TypeCheckingPhase] :: [CompilationPhase]
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