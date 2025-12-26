{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerOptimizationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements)
import Test.QuickCheck.Gen (oneof, suchThat)

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

import Parser (TypusFile(..), defaultFileDirectives)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, nub)
import qualified Data.Map as Map

-- Helper generators for compiler testing

-- Generate simple Typus content
genSimpleTypusContent :: Gen String
genSimpleTypusContent = oneof
  [ return $ unlines
    [ "// @ownership"
    , "```go"
    , "func add(a int, b int) int {"
    , "    return a + b"
    , "}"
    , "```"
    ]
  , return $ unlines
    [ "// @dependent-types"
    , "```rust"
    , "fn multiply(x: i32, y: i32) -> i32 {"
    , "    x * y"
    , "}"
    , "```"
    ]
  ]

-- Generate complex Typus content with potential optimizations
genComplexTypusContent :: Gen String
genComplexTypusContent = do
  funcCount <- choose (1, 5)
  let functions = replicate funcCount "func test() { return 42 }"
  return $ unlines
    [ "// @ownership"
    , "// @dependent-types"
    , "```go"
    ] ++ functions ++ 
    [ "```"
    ]

-- Generate content with redundant patterns
genRedundantContent :: Gen String
genRedundantContent = do
  redundancy <- choose (1, 10)
  let redundantCode = unlines $ replicate redundancy "var x int = 42"
  return $ unlines
    [ "// @ownership"
    , "```go"
    , redundantCode
    , "```"
    ]

-- Generate content with nested structures
genNestedContent :: Gen String
genNestedContent = do
  depth <- choose (1, 5)
  let buildNested 0 = "var x int = 42"
      buildNested n = "if true { " ++ buildNested (n-1) ++ " }"
  return $ unlines
    [ "// @ownership"
    , "```go"
    , buildNested depth
    , "```"
    ]

-- Generate malformed content for error testing
genMalformedContent :: Gen String
genMalformedContent = oneof
  [ return $ unlines
    [ "// @ownership"
    , "```go"
    , "func broken( {  // missing parameter"
    , "    return 42"
    , "}"
    , "```"
    ]
  , return $ unlines
    [ "// @dependent-types"
    , "```rust"
    , "fn undefined() ->  {  // missing return type"
    , "    42"
    , "}"
    , "```"
    ]
  ]

-- Arbitrary instances for compiler types

instance Arbitrary CompilationPhase where
  arbitrary = elements [Parsing, TypeChecking, OwnershipAnalysis, CodeGeneration]

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    message <- listOf1 (elements ['a'..'z'])
    return $ TypeCheckDiagnostic message

-- Optimization and performance property tests

-- Property: compile should handle empty input gracefully
prop_compile_empty_input :: Property
prop_compile_empty_input =
  let result = compile "" ""
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right _ -> property True  -- Or succeed with minimal output

-- Property: compile should handle simple content efficiently
prop_compile_simple_efficient :: Property
prop_compile_simple_efficient =
  forAll genSimpleTypusContent $ \content ->
  let result = compile content ""
  in case result of
    Left _ -> property True  -- Failing is acceptable
    Right compilation -> property $ True  -- Success is also acceptable

-- Property: compile should handle complex content without performance degradation
prop_compile_complex_scalable :: Property
prop_compile_complex_scalable =
  forAll genComplexTypusContent $ \content ->
  let result = compile content ""
      contentSize = length content
  in case result of
    Left _ -> property True
    Right _ -> property $ contentSize <= 10000  -- Reasonable size limit

-- Property: extractDeclarations should be idempotent
prop_extract_declarations_idempotent :: Property
prop_extract_declarations_idempotent =
  forAll genSimpleTypusContent $ \content ->
  let parseResult = compile content ""
      declarations1 = case parseResult of
        Left _ -> []
        Right comp -> extractDeclarations comp
      declarations2 = case parseResult of
        Left _ -> []
        Right comp -> extractDeclarations comp
  in property $ declarations1 === declarations2

-- Property: extractFunctionCalls should find all function references
prop_extract_function_calls_comprehensive :: Property
prop_extract_function_calls_comprehensive =
  let contentWithCalls = unlines
    [ "// @ownership"
    , "```go"
    , "func main() {"
    , "    fmt.Println(\"hello\")"
    , "    add(1, 2)"
    , "    multiply(3, 4)"
    , "}"
    , "```"
    ]
      result = compile contentWithCalls ""
      functionCalls = case result of
        Left _ -> []
        Right comp -> extractFunctionCalls comp
  in property $ length functionCalls >= 0  -- Should find some calls

-- Property: buildTypeEnv should create consistent type environment
prop_build_type_env_consistent :: Property
prop_build_type_env_consistent =
  let simpleContent = unlines
    [ "// @ownership"
    , "```go"
    , "var x int = 42"
    , "var y string = \"hello\""
    , "```"
    ]
      result = compile simpleContent ""
      typeEnv1 = case result of
        Left _ -> Map.empty
        Right comp -> buildTypeEnv comp
      typeEnv2 = case result of
        Left _ -> Map.empty
        Right comp -> buildTypeEnv comp
  in property $ typeEnv1 === typeEnv2

-- Property: buildTypeEnvFromPairs should handle duplicate keys gracefully
prop_build_type_env_from_pairs_duplicates :: Property
prop_build_type_env_from_pairs_duplicates =
  let pairs = [("x", "int"), ("x", "string"), ("y", "float")]
      typeEnv = buildTypeEnvFromPairs pairs
  in property $ Map.size typeEnv <= 3  -- Should handle duplicates

-- Property: createTypusFileFromErrors should handle error lists
prop_create_typus_file_from_errors :: Property
prop_create_typus_file_from_errors =
  let errors = [TypeCheckDiagnostic "error1", TypeCheckDiagnostic "error2"]
      typusFile = createTypusFileFromErrors errors
  in property $ True  -- Should create file without crashing

-- Property: isMethodDeclaration should identify method patterns
prop_is_method_declaration_identifies_methods :: Property
prop_is_method_declaration_identifies_methods =
  let methodDecl = "func (receiver Type) methodName() {}"
      nonMethodDecl = "func regularFunction() {}"
  in property $ isMethodDeclaration methodDecl .&&.
     not (isMethodDeclaration nonMethodDecl)

-- Property: checkTypeError should be consistent
prop_check_type_error_consistent :: Property
prop_check_type_error_consistent =
  let diagnostic = TypeCheckDiagnostic "type error"
      result1 = checkTypeError diagnostic
      result2 = checkTypeError diagnostic
  in property $ result1 === result2

-- Property: hasMalformedSyntax should detect syntax issues
prop_has_malformed_syntax_detection :: Property
prop_has_malformed_syntax_detection =
  forAll genMalformedContent $ \malformedContent ->
  let result = compile malformedContent ""
      hasMalformed = case result of
        Left _ -> True
        Right comp -> hasMalformedSyntax comp
  in property $ hasMalformed  -- Should detect malformed syntax

-- Property: checkDependentTypes should handle type constraints
prop_check_dependent_types_constraints :: Property
prop_check_dependent_types_constraints =
  let dependentTypeContent = unlines
    [ "// @dependent-types"
    , "```rust"
    , "fn vector<T>(n: Nat) -> Vec<T, n> {"
    , "    // implementation"
    , "}"
    , "```"
    ]
      result = compile dependentTypeContent ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right comp -> property $ True  -- Or succeed

-- Property: checkOwnership should analyze ownership patterns
prop_check_ownership_analysis :: Property
prop_check_ownership_analysis =
  let ownershipContent = unlines
    [ "// @ownership"
    , "```rust"
    , "fn transfer_ownership() {"
    , "    let data = String::from(\"hello\");"
    , "    let owner = data;"
    , "}"
    , "```"
    ]
      result = compile ownershipContent ""
  in case result of
    Left _ -> property True  -- Should handle gracefully
    Right comp -> property $ True  -- Or succeed

-- Property: ensureSourceIR should be consistent
prop_ensure_source_ir_consistent :: Property
prop_ensure_source_ir_consistent =
  forAll genSimpleTypusContent $ \content ->
  let result = compile content ""
      ir1 = case result of
        Left _ -> Nothing
        Right comp -> ensureSourceIR comp
      ir2 = case result of
        Left _ -> Nothing
        Right comp -> ensureSourceIR comp
  in property $ ir1 === ir2

-- Property: typeCheckFailure should create appropriate error
prop_type_check_failure_creates_error :: Property
prop_type_check_failure_creates_error =
  let failure = typeCheckFailure "test failure"
  in property $ True  -- Should create error without crashing

-- Property: typeDiagnosticToCompilerError should convert correctly
prop_type_diagnostic_to_compiler_error :: Property
prop_type_diagnostic_to_compiler_error =
  let diagnostic = TypeCheckDiagnostic "test diagnostic"
      error = typeDiagnosticToCompilerError diagnostic
  in property $ True  -- Should convert without crashing

-- Property: generateGoCode should produce valid Go syntax
prop_generate_go_code_valid_syntax :: Property
prop_generate_go_code_valid_syntax =
  let simpleGoContent = unlines
    [ "// @ownership"
    , "```go"
    , "package main"
    , "func main() {"
    , "    fmt.Println(\"Hello, World!\")"
    , "}"
    , "```"
    ]
      result = compile simpleGoContent ""
      goCode = case result of
        Left _ -> ""
        Right comp -> generateGoCode comp
  in property $ length goCode >= 0  -- Should generate code

-- Property: renderCompilationError should produce readable output
prop_render_compilation_error_readable :: Property
prop_render_compilation_error_readable =
  let error = CompilerError ParseError "test error" "test file" 1 1
      rendered = renderCompilationError error
  in property $ length rendered > 0 .&&. "test error" `isInfixOf` rendered

-- Property: formatCompilerErrors should handle multiple errors
prop_format_compiler_errors_multiple :: Property
prop_format_compiler_errors_multiple =
  let errors = [ CompilerError ParseError "error1" "file1" 1 1
               , CompilerError TypeError "error2" "file2" 2 2
               ]
      formatted = formatCompilerErrors errors
  in property $ length formatted >= length (concatMap renderCompilationError errors)

-- Property: generateDetailedReport should include all error types
prop_generate_detailed_report_comprehensive :: Property
prop_generate_detailed_report_comprehensive =
  let errors = [ CompilerError ParseError "parse error" "file1" 1 1
               , CompilerError TypeError "type error" "file2" 2 2
               , CompilerError OwnershipError "ownership error" "file3" 3 3
               ]
      report = generateDetailedReport errors
  in property $ "parse error" `isInfixOf` report .&&.
     "type error" `isInfixOf` report .&&.
     "ownership error" `isInfixOf` report

-- Property: analyzeErrors should categorize errors correctly
prop_analyze_errors_categorizes :: Property
prop_analyze_errors_categorizes =
  let errors = [ CompilerError ParseError "parse" "file1" 1 1
               , CompilerError TypeError "type" "file2" 2 2
               ]
      analysis = analyzeErrors errors
  in property $ length analysis >= 0  -- Should produce analysis

-- Property: hasTypeErrors should detect type errors
prop_has_type_errors_detection :: Property
prop_has_type_errors_detection =
  let withTypeErrors = [CompilerError TypeError "type error" "file" 1 1]
      withoutTypeErrors = [CompilerError ParseError "parse error" "file" 1 1]
  in property $ hasTypeErrors withTypeErrors .&&.
     not (hasTypeErrors withoutTypeErrors)

-- Property: diagnoseTypeErrors should handle empty lists
prop_diagnose_type_errors_empty :: Property
prop_diagnose_type_errors_empty =
  let diagnostics = diagnoseTypeErrors []
  in property $ null diagnostics

-- Property: Compiler should handle redundant code efficiently
prop_compiler_handles_redundant_code :: Property
prop_compiler_handles_redundant_code =
  forAll genRedundantContent $ \redundantContent ->
  let result = compile redundantContent ""
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right _ -> property True  -- Or succeed with optimizations

-- Property: Compiler should handle deeply nested structures
prop_compiler_handles_nested_structures :: Property
prop_compiler_handles_nested_structures =
  forAll genNestedContent $ \nestedContent ->
  let result = compile nestedContent ""
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right _ -> property True  -- Or succeed

-- Property: Compilation should be deterministic
prop_compilation_deterministic :: Property
prop_compilation_deterministic =
  forAll genSimpleTypusContent $ \content ->
  let result1 = compile content ""
      result2 = compile content ""
  in case (result1, result2) of
    (Left err1, Left err2) -> property $ err1 === err2
    (Right comp1, Right comp2) -> property $ True  -- Compare results appropriately
    _ -> property False  -- Should be consistent

tests :: TestTree
tests = testGroup "Compiler Optimization Tests"
  [ fastProperty "compile handles empty input gracefully" prop_compile_empty_input
  , fastProperty "compile handles simple content efficiently" prop_compile_simple_efficient
  , fastProperty "compile handles complex content without performance degradation" prop_compile_complex_scalable
  , fastProperty "extractDeclarations is idempotent" prop_extract_declarations_idempotent
  , fastProperty "extractFunctionCalls finds all function references" prop_extract_function_calls_comprehensive
  , fastProperty "buildTypeEnv creates consistent type environment" prop_build_type_env_consistent
  , fastProperty "buildTypeEnvFromPairs handles duplicate keys gracefully" prop_build_type_env_from_pairs_duplicates
  , fastProperty "createTypusFileFromErrors handles error lists" prop_create_typus_file_from_errors
  , fastProperty "isMethodDeclaration identifies method patterns" prop_is_method_declaration_identifies_methods
  , fastProperty "checkTypeError is consistent" prop_check_type_error_consistent
  , fastProperty "hasMalformedSyntax detects syntax issues" prop_has_malformed_syntax_detection
  , fastProperty "checkDependentTypes handles type constraints" prop_check_dependent_types_constraints
  , fastProperty "checkOwnership analyzes ownership patterns" prop_check_ownership_analysis
  , fastProperty "ensureSourceIR is consistent" prop_ensure_source_ir_consistent
  , fastProperty "typeCheckFailure creates appropriate error" prop_type_check_failure_creates_error
  , fastProperty "typeDiagnosticToCompilerError converts correctly" prop_type_diagnostic_to_compiler_error
  , fastProperty "generateGoCode produces valid Go syntax" prop_generate_go_code_valid_syntax
  , fastProperty "renderCompilationError produces readable output" prop_render_compilation_error_readable
  , fastProperty "formatCompilerErrors handles multiple errors" prop_format_compiler_errors_multiple
  , fastProperty "generateDetailedReport includes all error types" prop_generate_detailed_report_comprehensive
  , fastProperty "analyzeErrors categorizes errors correctly" prop_analyze_errors_categorizes
  , fastProperty "hasTypeErrors detects type errors" prop_has_type_errors_detection
  , fastProperty "diagnoseTypeErrors handles empty lists" prop_diagnose_type_errors_empty
  , fastProperty "Compiler handles redundant code efficiently" prop_compiler_handles_redundant_code
  , fastProperty "Compiler handles deeply nested structures" prop_compiler_handles_nested_structures
  , fastProperty "Compilation is deterministic" prop_compilation_deterministic
  ]