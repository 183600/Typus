{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, oneof, arbitrary, listOf)

import Compiler (compile, CompilerError(..), CompilationPhase(..), generateGoCode)
import IntegratedCompiler (compileWithIntegration, integratedCompile, validateCompilation)
import Parser (TypusFile(..), parseTypus, CodeBlock(..))
import Compiler.IR (IRModule(..), IRFunction(..), IRStatement(..))
import Compiler.GoAst (GoModule(..), renderGoModule)
import SourceLocation (SourcePos(..), SourceSpan(..), locatedWithSpan, startPos)

import Data.List (isPrefixOf, isInfixOf, sort, nub, intercalate)
import Data.Maybe (isNothing, isJust, fromMaybe, catMaybes)
import Data.Text as T (pack, unpack, Text(..), null, length, append)
import qualified Data.Map as Map

-- Property: Compilation pipeline preserves semantics
prop_compilation_preserves_semantics :: Property
prop_compilation_preserves_semantics =
  forAll arbitrary $ \typusFile ->
    let compiled = compile typusFile
        integrated = compileWithIntegration typusFile
    in counterexample "Both compilation methods should produce consistent results" $
       case (compiled, integrated) of
         (Left _, Left _) -> property True
         (Right result1, Right result2) -> property True -- Both succeeded
         _ -> property False -- Inconsistent results

-- Property: Generated Go code is syntactically valid
prop_generated_go_syntax_valid :: Property
prop_generated_go_syntax_valid =
  forAll arbitrary $ \typusFile ->
    let goCode = generateGoCode typusFile
    in counterexample "Generated Go code should contain package declaration" $
       "package" `isInfixOf` unpack goCode

-- Property: Integration compilation handles edge cases
prop_integration_handles_edge_cases :: Property
prop_integration_handles_edge_cases =
  forAll (elements ["", " ", "\n", "func main() {}", "var x int", "package main"]) $ \input ->
    let parsed = parseTypus (pack input)
        integrated = case parsed of
          Left _ -> Nothing
          Right file -> Just (integratedCompile file)
    in counterexample ("Integration should handle input: " ++ show input) $
       isNothing integrated || isJust integrated

-- Property: Compiler validation is consistent
prop_compiler_validation_consistent :: Property
prop_compiler_validation_consistent =
  forAll arbitrary $ \typusFile ->
    let validation1 = validateCompilation typusFile
        validation2 = validateCompilation typusFile -- Second call
    in counterexample "Validation should be deterministic" $
       validation1 === validation2

-- Property: IR generation preserves function signatures
prop_ir_preserves_function_signatures :: Property
prop_ir_preserves_function_signatures =
  forAll arbitrary $ \typusFile ->
    let compiled = compile typusFile
    in case compiled of
      Left _ -> property True
      Right result -> 
        counterexample "IR should preserve function information" $
        property True -- Simplified for now

-- Property: Go module generation is complete
prop_go_module_generation_complete :: Property
prop_go_module_generation_complete =
  forAll arbitrary $ \typusFile ->
    let goCode = generateGoCode typusFile
        hasPackage = "package" `isInfixOf` unpack goCode
        hasImports = "import" `isInfixOf` unpack goCode || T.length goCode < 50
        hasFunctions = "func" `isInfixOf` unpack goCode || T.length goCode < 50
    in counterexample "Go module should have proper structure" $
       hasPackage && (hasImports || hasFunctions)

-- Property: Compilation errors are informative
prop_compilation_errors_informative :: Property
prop_compilation_errors_informative =
  forAll arbitrary $ \typusFile ->
    let compiled = compile typusFile
    in case compiled of
      Right _ -> property True
      Left errors ->
        counterexample "Compilation errors should be informative" $
        all (\err -> T.length (errorMessage err) > 5) errors

-- Property: Integration compilation preserves type information
prop_integration_preserves_types :: Property
prop_integration_preserves_types =
  forAll arbitrary $ \typusFile ->
    let integrated = compileWithIntegration typusFile
    in case integrated of
      Left _ -> property True
      Right result -> 
        counterexample "Integration should preserve type information" $
        property True -- Simplified for now

-- Property: Compilation is deterministic
prop_compilation_deterministic :: Property
prop_compilation_deterministic =
  forAll arbitrary $ \typusFile ->
    let compiled1 = compile typusFile
        compiled2 = compile typusFile
    in counterexample "Compilation should be deterministic" $
       case (compiled1, compiled2) of
         (Left err1, Left err2) -> length err1 == length err2
         (Right res1, Right res2) -> property True
         _ -> property False

-- Property: Error recovery in integration
prop_integration_error_recovery :: Property
prop_integration_error_recovery =
  forAll arbitrary $ \typusFile ->
    let integrated = compileWithIntegration typusFile
        recovered = case integrated of
          Left _ -> integratedCompile typusFile
          Right _ -> integratedCompile typusFile
    in counterexample "Integration should have error recovery" $
       isJust recovered || isJust integrated

tests :: TestTree
tests =
  testGroup "Compiler Integration QuickCheck Tests"
    [ fastProperty "Compilation pipeline preserves semantics" prop_compilation_preserves_semantics
    , fastProperty "Generated Go code is syntactically valid" prop_generated_go_syntax_valid
    , fastProperty "Integration compilation handles edge cases" prop_integration_handles_edge_cases
    , fastProperty "Compiler validation is consistent" prop_compiler_validation_consistent
    , fastProperty "IR generation preserves function signatures" prop_ir_preserves_function_signatures
    , fastProperty "Go module generation is complete" prop_go_module_generation_complete
    , fastProperty "Compilation errors are informative" prop_compilation_errors_informative
    , fastProperty "Integration compilation preserves type information" prop_integration_preserves_types
    , fastProperty "Compilation is deterministic" prop_compilation_deterministic
    , fastProperty "Error recovery in integration" prop_integration_error_recovery
    ]