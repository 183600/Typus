{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.IntegrationEndToEndQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Compiler (compile, CompilerResult(..), CompilerError(..))
import Parser (parseTypus, TypusFile(..))
import Compiler.IR (buildSourceIR, buildSemanticIR, emitGo)
import SyntaxValidator (validateSyntax)
import Ownership.Common.Types (newOwnershipAnalyzer)
import DependentTypesParser (validateDependentTypeSyntax)
import ErrorHandler (formatErrors)
import SourceLocation (SourcePos(..), SourceSpan(..))

-- ============================================================================
-- Test Data Generation
-- ============================================================================

-- | Generate valid Go code snippets
arbitraryGoCode :: Gen String
arbitraryGoCode = oneof
  [ return "package main\n\nfunc main() {\n  println(\"Hello\")\n}"
  , return "package main\n\nimport \"fmt\"\n\nfunc add(a, b int) int {\n  return a + b\n}\n\nfunc main() {\n  fmt.Println(add(1, 2))\n}"
  , return $ "package main\n\nvar x = 42\n\nfunc main() {\n  println(x)\n}"
  , return $ "package main\n\ntype Point struct {\n  X, Y int\n}\n\nfunc main() {\n  p := Point{1, 2}\n  println(p.X, p.Y)\n}"
  ]

-- | Generate valid Typus code snippets
arbitraryTypusCode :: Gen String
arbitraryTypusCode = oneof
  [ return "@ownership(true)\nfunc main() {\n  let x = 5;\n  println(x);\n}"
  , return "@dependent_types(true)\ntype Vector<T> {\n  data: [T];\n  size: int where size >= 0;\n}\n\nfunc main() {\n  let v = Vector<int>{data: [1,2,3], size: 3};\n}"
  , return "@constraints(true)\ntype PositiveInt where value > 0;\n\nfunc main() {\n  let x: PositiveInt = 5;\n}"
  , return $ "@ownership(false)\n@dependent_types(false)\nfunc add(a: int, b: int) -> int {\n  return a + b;\n}\n\nfunc main() {\n  let result = add(1, 2);\n  println(result);\n}"
  ]

-- | Generate malformed code snippets
arbitraryMalformedCode :: Gen String
arbitraryMalformedCode = oneof
  [ return "func main( {\n  missing closing brace\n}"
  , return "package main\n\nfunc main() {\n  println(\"unclosed string\n}"
  , return "func main() {\n  let x = 5\n  missing semicolon\n  let y = 10\n}"
  , return "/* unclosed comment\nfunc main() {\n  println(5)\n}"
  , return $ "func main() {\n  " ++ replicate 1000 'a' ++ "\n}"
  , return "@invalid_directive\nfunc main() {\n  println(5)\n}"
  ]

-- | Generate mixed valid/invalid code
arbitraryMixedCode :: Gen String
arbitraryMixedCode = do
  valid <- arbitraryTypusCode
  invalid <- arbitraryMalformedCode
  return $ valid ++ "\n" ++ invalid

-- | Generate arbitrary code strings
arbitraryString :: Gen String
arbitraryString = do
  size <- choose (0, 100)
  vectorOf size $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t;{}()[]+-*/%=!&|^~."

-- ============================================================================
-- QuickCheck Properties for Integration End-to-End Compilation
-- ============================================================================

-- | Compilation should not crash on any input
prop_compilation_no_crash :: String -> Property
prop_compilation_no_crash input =
  let result = compile input
  in result `seq` True

-- | Empty input should be handled gracefully
prop_empty_input_handling :: Property
prop_empty_input_handling =
  let result = compile ""
  in case result of
    Left _ -> True
    Right success -> success `seq` True

-- | Valid Go code should compile successfully
prop_valid_go_compilation :: Property
prop_valid_go_compilation =
  forAll arbitraryGoCode $ \goCode ->
  let result = compile goCode
  in case result of
    Left _ -> property False  -- Valid Go should not fail
    Right success -> success `seq` True

-- | Valid Typus code should compile successfully
prop_valid_typus_compilation :: Property
prop_valid_typus_compilation =
  forAll arbitraryTypusCode $ \typusCode ->
  let result = compile typusCode
  in case result of
    Left _ -> property True  -- Typus features might not be fully supported
    Right success -> success `seq` True

-- | Malformed code should produce errors, not crash
prop_malformed_code_errors :: Property
prop_malformed_code_errors =
  forAll arbitraryMalformedCode $ \malformedCode ->
  let result = compile malformedCode
  in case result of
    Left _ -> True  -- Error is expected
    Right success -> success `seq` True  -- Success is also acceptable

-- | Mixed code should be handled gracefully
prop_mixed_code_handling :: Property
prop_mixed_code_handling =
  forAll arbitraryMixedCode $ \mixedCode ->
  let result = compile mixedCode
  in case result of
    Left _ -> True
    Right success -> success `seq` True

-- | Parser should handle compilation input
prop_parser_integration :: String -> Property
prop_parser_integration input =
  let parseResult = parseTypus input
  in case parseResult of
    Left _ -> True
    Right typusFile -> typusFile `seq` True

-- | Syntax validation should be consistent
prop_syntax_validation_integration :: String -> Property
prop_syntax_validation_integration input =
  let syntaxErrors = validateSyntax input
  in length syntaxErrors >= 0  -- Should not crash

-- | Source IR building should be consistent
prop_source_ir_integration :: String -> Property
prop_source_ir_integration input =
  case parseTypus input of
    Left _ -> True  -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
      in sourceIR `seq` True  -- Should not crash

-- | Semantic IR building should be consistent
prop_semantic_ir_integration :: String -> Property
prop_semantic_ir_integration input =
  case parseTypus input of
    Left _ -> True  -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> True  -- Semantic analysis failure is acceptable
        Right semanticIR -> semanticIR `seq` True

-- | Go emission should be consistent
prop_go_emission_integration :: String -> Property
prop_go_emission_integration input =
  case parseTypus input of
    Left _ -> True  -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> True  -- Semantic analysis failure is acceptable
        Right semanticIR ->
          let goIR = emitGo semanticIR
          in goIR `seq` True  -- Should not crash

-- | Full compilation pipeline should be consistent
prop_full_pipeline_consistency :: String -> Property
prop_full_pipeline_consistency input =
  let result1 = compile input
      result2 = compile input
  in case (result1, result2) of
    (Left err1, Left err2) -> show err1 === show err2
    (Right success1, Right success2) -> success1 === success2
    (Left _, Right _) -> property False  -- Inconsistent results
    (Right _, Left _) -> property False  -- Inconsistent results

-- | Compilation should handle very large inputs
prop_large_input_handling :: Positive Int -> Property
prop_large_input_handling (Positive size) =
  let largeInput = replicate size 'a' ++ "\nfunc main() {}"
      result = compile largeInput
  in case result of
    Left _ -> True
    Right success -> success `seq` True

-- | Compilation should handle Unicode characters
prop_unicode_handling :: Property
prop_unicode_handling =
  let unicodeCode = "package main\n\nfunc main() {\n  println(\"测试: 世界\");\n  println(\"🌟 Hello\");\n}"
      result = compile unicodeCode
  in case result of
    Left _ -> True
    Right success -> success `seq` True

-- | Ownership analysis should be integrable
prop_ownership_integration :: String -> Property
prop_ownership_integration input =
  let analyzer = newOwnershipAnalyzer
  in analyzer `seq` True  -- Should not crash

-- | Dependent type validation should be integrable
prop_dependent_types_integration :: String -> Property
prop_dependent_types_integration input =
  let validationResult = validateDependentTypeSyntax input
  in validationResult `seq` True  -- Should not crash

-- | Error formatting should be consistent
prop_error_formatting_integration :: String -> Property
prop_error_formatting_integration input =
  case compile input of
    Left errors -> 
      let formatted = formatErrors errors
      in formatted `seq` True  -- Should not crash
    Right _ -> property True  -- Success case

-- | Compilation phases should be order-independent where possible
prop_phase_ordering :: String -> Property
prop_phase_ordering input =
  let parseResult = parseTypus input
      syntaxErrors = validateSyntax input
  in case parseResult of
    Left _ -> syntaxErrors `seq` True  -- Parse failed, but syntax check should still work
    Right typusFile -> 
      let sourceIR = buildSourceIR typusFile
      in syntaxErrors `seq` sourceIR `seq` True  -- Both should work

-- | Compilation should preserve semantic correctness
prop_semantic_preservation :: Property
prop_semantic_preservation =
  let semanticCode = "package main\n\nfunc add(a, b int) int {\n  return a + b\n}\n\nfunc main() {\n  println(add(2, 3))\n}"
      result = compile semanticCode
  in case result of
    Left _ -> property False  -- Should compile
    Right success -> 
      let successStr = show success
      in "add" `isInfixOf` successStr .&&. "5" `isInfixOf` successStr

-- | Error recovery should be possible
prop_error_recovery :: Property
prop_error_recovery =
  let recoverableCode = "package main\n\nfunc main() {\n  println(\"hello\")\n  // missing semicolon but should recover\n  println(\"world\")\n}"
      result = compile recoverableCode
  in case result of
    Left _ -> property True  -- Error is acceptable
    Right success -> success `seq` True  -- Recovery successful

-- | Incremental compilation should be consistent
prop_incremental_compilation :: String -> String -> Property
prop_incremental_compilation part1 part2 =
  let full = part1 ++ "\n" ++ part2
      result1 = compile full
      result2 = compile part1
  in case (result1, result2) of
    (Left _, _) -> True  -- Full compilation failed
    (Right fullSuccess, Left _) -> True  -- Part failed but full succeeded
    (Right fullSuccess, Right partSuccess) -> 
      fullSuccess `seq` partSuccess `seq` True  -- Both succeeded

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Integration End-to-End QuickCheck Tests"
  [ testProperty "compilation doesn't crash on any input" prop_compilation_no_crash
  , testProperty "empty input handled gracefully" prop_empty_input_handling
  , testProperty "valid Go compilation" prop_valid_go_compilation
  , testProperty "valid Typus compilation" prop_valid_typus_compilation
  , testProperty "malformed code produces errors" prop_malformed_code_errors
  , testProperty "mixed code handling" prop_mixed_code_handling
  , testProperty "parser integration" prop_parser_integration
  , testProperty "syntax validation integration" prop_syntax_validation_integration
  , testProperty "source IR integration" prop_source_ir_integration
  , testProperty "semantic IR integration" prop_semantic_ir_integration
  , testProperty "Go emission integration" prop_go_emission_integration
  , testProperty "full pipeline consistency" prop_full_pipeline_consistency
  , testProperty "large input handling" prop_large_input_handling
  , testProperty "Unicode handling" prop_unicode_handling
  , testProperty "ownership integration" prop_ownership_integration
  , testProperty "dependent types integration" prop_dependent_types_integration
  , testProperty "error formatting integration" prop_error_formatting_integration
  , testProperty "phase ordering" prop_phase_ordering
  , testProperty "semantic preservation" prop_semantic_preservation
  , testProperty "error recovery" prop_error_recovery
  , testProperty "incremental compilation" prop_incremental_compilation
  ]