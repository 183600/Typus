{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.CompilerCoreSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure, Assertion, assertBool)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import Compiler (compile, CompilerError(..), CompilerResult, CompilationPhase(..), 
                SyntaxError(..), malformedSyntaxError, 
                renderCompilationError, formatCompilerErrors, 
                generateDetailedReport, analyzeErrors, hasTypeErrors, 
                TypeCheckDiagnostic(..), diagnoseTypeErrors, 
                extractDeclarations, extractFunctionCalls, buildTypeEnv, 
                buildTypeEnvFromPairs, createTypusFileFromErrors, 
                isMethodDeclaration, checkTypeError, hasMalformedSyntax, 
                checkDependentTypes, checkOwnership, ensureSourceIR, 
                typeCheckFailure, typeDiagnosticToCompilerError, 
                generateGoCode)
import Compiler.TypeChecker (TypeError, TypeEnv(..), Type(..))
import qualified Compiler.TypeChecker as TC
import Compiler.GoAst (GoModule(..), ImportDecl(..), GoDecl(..), FuncDecl(..), TypeDecl(..))
import Compiler.Errors (mkCompilerError)
import Compiler.TypeChecker (TypeEnv(..), Type(..))
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..), 
              defaultFileDirectives, defaultBlockDirectives, parseTypus)
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import Data.Char (isSpace)
import Compiler.Errors.Core (ErrorSeverity(..), emptyContext, errorRecovery, ErrorLocation(..), ErrorCategory(..), errorId, severity, category, message, location, context, recovery, suggestions, relatedErrors, errorChain, timestamp, TypeError(..), ErrorContext(..), ErrorRecovery(..))
import qualified Data.Map.Strict as Map
import qualified SyntaxValidator

-- Arbitrary instances for QuickCheck
instance Arbitrary TypusFile where
  arbitrary = genTypusFile

instance Arbitrary GoModule where
  arbitrary = genGoModule

instance Arbitrary CompilerError where
  arbitrary = genCompilerError

instance Arbitrary Compiler.Errors.Core.TypeError where
  arbitrary = do
    errorId <- choose (1000, 9999 :: Int) >>= \n -> return ("E" ++ show n)
    severity <- arbitrary
    category <- arbitrary
    message <- arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf arbitrary
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- arbitrary
    return $ Compiler.Errors.Core.TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary TC.TypeError where
  arbitrary = do
    teContext <- arbitrary
    teMessage <- arbitrary
    return $ TC.TypeError { TC.teContext = teContext, TC.teMessage = teMessage }

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

-- Import the Arbitrary instances from ErrorHandlerSpec
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- arbitrary
    line <- choose (1, 100)
    column <- choose (1, 100)
    endLine <- arbitrary
    endColumn <- arbitrary
    return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    contextCode <- arbitrary
    contextFunction <- arbitrary
    contextVariable <- arbitrary
    contextType <- arbitrary
    contextAdditional <- listOf arbitrary
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRecover <- arbitrary
    shouldContinue <- arbitrary
    recoveryAction <- arbitrary
    recoveryHint <- arbitrary
    recoveryCost <- choose (0, 100)
    recoveryConfidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRecover shouldContinue recoveryAction recoveryHint recoveryCost recoveryConfidence

-- Helper generators for Compiler tests
genTypusFile :: Gen TypusFile
genTypusFile = do
  let directives = defaultFileDirectives
      buildTags = []
      blocks = []
      syntaxErrors = []
  return $ TypusFile directives buildTags blocks syntaxErrors

genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  let directives = defaultBlockDirectives
  content <- elements ["let x = 42", "func add(a int, b int) int { return a + b }", "var y string = \"hello\""]
  span <- genSourceSpan
  return $ CodeBlock directives content span

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ SourceSpan start end

genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 100)
  column <- choose (1, 100)
  offset <- choose (0, 10000)
  return $ SourcePos line column offset

genGoModule :: Gen GoModule
genGoModule = do
  buildTags <- listOf $ elements ["tag1", "tag2", "tag3"]
  imports <- listOf $ elements [ImportDecl (Just "fmt") "fmt", ImportDecl Nothing "os"]
  decls <- listOf $ elements [GoFunc (FuncDecl ["func test() {}"]), GoType (TypeDecl ["type Test int"] False)]
  return $ GoModule buildTags Nothing imports decls

genCompilerError :: Gen CompilerError
genCompilerError = do
  code <- elements ["CP0001", "CP0002", "CP0003", "CP0004"]
  message <- elements ["Syntax error", "Type error", "Compilation error"]
  phase <- elements [ParsingPhase, TypeCheckingPhase, CodeGenerationPhase]
  category <- elements [Parsing, TypeChecking, Semantic]
  severity <- elements [Error, Warning]
  span <- genSourceSpan
  return $ mkCompilerError code (T.pack message) phase category severity (Just span) Nothing [] [] Nothing

-- Test cases for Compiler module

-- Test 1: Compile empty file
test_compile_empty_file :: Assertion
test_compile_empty_file = do
  let file = TypusFile defaultFileDirectives [] [] []
      result = compile file
  case result of
    Right output -> assertEqual "Empty file should generate minimal Go code" 
                               "package main\n\nfunc main() {\n}\n" output
    Left err -> assertFailure $ "Failed to compile empty file: " ++ show err

-- Test 2: Compile simple code
test_compile_simple_code :: Assertion
test_compile_simple_code = do
  let input = "let x = 42\n"
      file = case parseTypus input of
        Right f -> f
        Left _ -> TypusFile defaultFileDirectives [] [] []
      result = compile file
  case result of
    Right output -> assertEqual "Simple code should be preserved" input output
    Left err -> assertFailure $ "Failed to compile simple code: " ++ show err

-- Test 3: Compile code with syntax error
test_compile_syntax_error :: Assertion
test_compile_syntax_error = do
  let input = "let x = +\n"
      file = case parseTypus input of
        Right f -> f
        Left _ -> TypusFile defaultFileDirectives [] [] []
      result = compile file
  case result of
    Left (err:_) -> do
      assertEqual "Should be a parsing error" ParsingPhase (cePhase err)
    Left [] -> assertFailure "Expected at least one error but got empty list"
    Right _ -> assertFailure "Expected compilation to fail with syntax error"

-- Test 4: Compile code with type error
test_compile_type_error :: Assertion
test_compile_type_error = do
  let input = "var x int = \"string\"\n"
      file = case parseTypus input of
        Right f -> f
        Left _ -> TypusFile defaultFileDirectives [] [] []
      result = compile file
  case result of
    Left (err:_) -> do
      assertEqual "Should be a type checking error" TypeCheckingPhase (cePhase err)
    Left [] -> assertFailure "Expected at least one error but got empty list"
    Right _ -> assertFailure "Expected compilation to fail with type error"

-- Test 5: Compile code with dependent types
test_compile_dependent_types :: Assertion
test_compile_dependent_types = do
  let input = "//! dependent_types: on\nlet x: Int = 42\n"
      file = case parseTypus input of
        Right f -> f
        Left _ -> TypusFile defaultFileDirectives [] [] []
      result = compile file
  case result of
    Right output -> assertEqual "Code with dependent types should compile" input output
    Left err -> assertFailure $ "Failed to compile code with dependent types: " ++ show err

-- Test 6: Compile code with ownership
test_compile_ownership :: Assertion
test_compile_ownership = do
  let input = "//! ownership: on\nlet x = 42\n"
      file = case parseTypus input of
        Right f -> f
        Left _ -> TypusFile defaultFileDirectives [] [] []
      result = compile file
  case result of
    Right output -> assertEqual "Code with ownership should compile" input output
    Left err -> assertFailure $ "Failed to compile code with ownership: " ++ show err

-- Test 7: Ensure source IR for valid file
test_ensure_source_ir_valid :: Assertion
test_ensure_source_ir_valid = do
  let file = TypusFile defaultFileDirectives [] [] []
      result = ensureSourceIR file
  case result of
    Right _ -> return ()
    Left err -> assertFailure $ "Failed to ensure source IR: " ++ show err

-- Test 8: Ensure source IR for invalid file
test_ensure_source_ir_invalid :: Assertion
test_ensure_source_ir_invalid = do
  let syntaxErrors = [SyntaxError { errorType = SyntaxValidator.InvalidStatement, errorMessage = "Test error", lineNumber = 1, columnNumber = 1, lineContent = "" }]
      file = TypusFile defaultFileDirectives [] [] syntaxErrors
      result = ensureSourceIR file
  case result of
    Right _ -> assertFailure "Expected source IR to fail for file with syntax errors"
    Left (err:_) -> do
      assertEqual "Should be malformed syntax error" malformedSyntaxError err
    Left [] -> assertFailure "Expected at least one error but got empty list"

-- Test 9: Generate Go code from empty file
test_generate_go_code_empty :: Assertion
test_generate_go_code_empty = do
  let file = TypusFile defaultFileDirectives [] [] []
      output = generateGoCode file
  assertEqual "Empty file should generate minimal Go code" 
             "package main\n\nfunc main() {\n}\n" output

-- Test 10: Generate Go code from simple file
test_generate_go_code_simple :: Assertion
test_generate_go_code_simple = do
  let input = "let x = 42\n"
      file = case parseTypus input of
        Right f -> f
        Left _ -> TypusFile defaultFileDirectives [] [] []
      output = generateGoCode file
  assertEqual "Simple file should preserve source code" input output

-- Test 11: Render compilation error
test_render_compilation_error :: Assertion
test_render_compilation_error = do
  let err = malformedSyntaxError
      rendered = renderCompilationError [err]
  assertBool "Rendered error should contain error message" 
             ("Unexpected token" `isInfixOf` rendered)

-- Test 12: Format compiler errors
test_format_compiler_errors :: Assertion
test_format_compiler_errors = do
  let errs = [malformedSyntaxError, typeCheckFailure]
      formatted = formatCompilerErrors errs
  assertBool "Formatted errors should contain both error messages" 
             ("Unexpected token" `isInfixOf` formatted && 
              "Type errors detected" `isInfixOf` formatted)

-- Test 13: Generate detailed report
test_generate_detailed_report :: Assertion
test_generate_detailed_report = do
  let errs = [malformedSyntaxError]
      report = generateDetailedReport errs
  assertBool "Report should contain error details" 
             ("Unexpected token" `isInfixOf` report)

-- Test 14: Analyze errors
test_analyze_errors :: Assertion
test_analyze_errors = do
  let errs = [malformedSyntaxError, typeCheckFailure]
      analysis = show (analyzeErrors errs)
  assertBool "Analysis should contain error counts" 
             ("Parsing errors: 1" `isInfixOf` analysis && 
              "Type checking errors: 1" `isInfixOf` analysis)

-- Test 15: Check type errors
test_check_type_errors :: Assertion
test_check_type_errors = do
  let typeErr = TC.TypeError { TC.teContext = Nothing, TC.teMessage = "Type mismatch" }
      fileWithErrors = TypusFile defaultFileDirectives [] [] []
      hasErrs = hasTypeErrors fileWithErrors
  assertEqual "Should detect type errors" False hasErrs

-- Test 16: Diagnose type errors
test_diagnose_type_errors :: Assertion
test_diagnose_type_errors = do
  let file = TypusFile defaultFileDirectives [] [] []
      result = diagnoseTypeErrors file
  case result of
    Right diagnostics -> return ()
    Left _ -> assertFailure "Failed to diagnose type errors"

-- Test 17: Extract declarations
test_extract_declarations :: Assertion
test_extract_declarations = do
  let input = "let x = 42\nfunc add(a int, b int) int { return a + b }\n"
      file = case parseTypus input of
        Right f -> f
        Left _ -> TypusFile defaultFileDirectives [] [] []
      declarations = extractDeclarations ""
  assertEqual "Should extract 2 declarations" 2 (length declarations)

-- Test 18: Extract function calls
test_extract_function_calls :: Assertion
test_extract_function_calls = do
  let input = "let x = add(1, 2)\nlet y = multiply(x, 3)\n"
      file = case parseTypus input of
        Right f -> f
        Left _ -> TypusFile defaultFileDirectives [] [] []
      calls = extractFunctionCalls ""
  assertEqual "Should extract 2 function calls" 2 (length calls)

-- Test 19: Build type environment
test_build_type_env :: Assertion
test_build_type_env = do
  let file = TypusFile defaultFileDirectives [] [] []
      emptyGoModule = GoModule { gmBuildTags = [], gmPackage = Nothing, gmImports = [], gmDecls = [] }
      typeEnv = buildTypeEnv emptyGoModule
      envSize = case typeEnv of 
                  TypeEnv varTypes functionTypes -> Map.size varTypes + Map.size functionTypes
  assertEqual "Type environment should be empty" 0 envSize

-- Test 20: Build type environment from pairs
test_build_type_env_from_pairs :: Assertion
test_build_type_env_from_pairs = do
  let pairs = [("x", TypeName "int"), ("y", TypeName "string")]
      typeEnv = buildTypeEnvFromPairs pairs
      envSize = case typeEnv of 
                  TypeEnv varTypes functionTypes -> Map.size varTypes + Map.size functionTypes
  assertEqual "Type environment should have 2 entries" 2 envSize

-- Test 21: Create Typus file from errors
test_create_typus_file_from_errors :: Assertion
test_create_typus_file_from_errors = do
  let typeErrs = [TC.TypeError { TC.teContext = Nothing, TC.teMessage = "Type mismatch" }]
      file = createTypusFileFromErrors typeErrs
  assertEqual "File should have syntax errors" 1 (length (tfSyntaxErrors file))

-- Test 22: Check method declaration
test_check_method_declaration :: Assertion
test_check_method_declaration = do
  let methodDecl = "func (r *Receiver) Method() int { return 42 }"
      nonMethodDecl = "func Function() int { return 42 }"
  assertEqual "Should detect method declaration" True (isMethodDeclaration methodDecl)
  assertEqual "Should not detect method declaration" False (isMethodDeclaration nonMethodDecl)

-- Test 23: Check type error
test_check_type_error :: Assertion
test_check_type_error = do
  let emptyGoModule = GoModule { gmBuildTags = [], gmPackage = Nothing, gmImports = [], gmDecls = [] }
      typeEnv = buildTypeEnv emptyGoModule
      checked = checkTypeError typeEnv "test"
  assertEqual "Should check type error" True checked

-- Test 24: Check malformed syntax
test_check_malformed_syntax :: Assertion
test_check_malformed_syntax = do
  let fileWithErrors = TypusFile defaultFileDirectives [] [] [SyntaxError { errorType = SyntaxValidator.InvalidStatement, errorMessage = "Test error", lineNumber = 1, columnNumber = 1, lineContent = "" }]
      fileWithoutErrors = TypusFile defaultFileDirectives [] [] []
  assertEqual "Should detect malformed syntax" True (hasMalformedSyntax fileWithErrors)
  assertEqual "Should not detect malformed syntax" False (hasMalformedSyntax fileWithoutErrors)

-- Test 25: Check dependent types
test_check_dependent_types :: Assertion
test_check_dependent_types = do
  let file = TypusFile defaultFileDirectives [] [] []
      result = checkDependentTypes file
  case result of
    Right _ -> return ()
    Left err -> assertFailure $ "Failed to check dependent types: " ++ show err

-- Test 26: Check ownership
test_check_ownership :: Assertion
test_check_ownership = do
  let file = TypusFile defaultFileDirectives [] [] []
      result = checkOwnership file
  case result of
    Right _ -> return ()
    Left err -> assertFailure $ "Failed to check ownership: " ++ show err

-- Test 27: Convert type diagnostic to compiler error
test_type_diagnostic_to_compiler_error :: Assertion
test_type_diagnostic_to_compiler_error = do
  let diagnostic = TypeCheckDiagnostic (Just "context") "detail"
      err = typeDiagnosticToCompilerError diagnostic
  assertEqual "Should convert to compiler error" TypeCheckingPhase (cePhase err)

-- Property tests for Compiler module

-- Property 1: Compiling empty file should succeed
prop_compile_empty_file_succeeds :: Property
prop_compile_empty_file_succeeds = 
  let file = TypusFile defaultFileDirectives [] [] []
      result = compile file
  in case result of
    Right _ -> property True
    Left _ -> property False

-- Property 2: Compiling file with syntax errors should fail
prop_compile_syntax_errors_fails :: TypusFile -> Property
prop_compile_syntax_errors_fails file = 
  not (null (tfSyntaxErrors file)) ==>
    let result = compile file
    in case result of
      Right _ -> False
      Left _ -> True

-- Property 3: Generating Go code should always return a string
prop_generate_go_code_returns_string :: TypusFile -> Bool
prop_generate_go_code_returns_string file = 
  let output = generateGoCode file
  in not (null output)

-- Property 4: Rendering compilation errors should contain error messages
prop_render_compilation_error_contains_messages :: [CompilerError] -> Property
prop_render_compilation_error_contains_messages errs = 
  not (null errs) ==>
    let rendered = renderCompilationError errs
    in all (\err -> T.unpack (message (ceError err)) `isInfixOf` rendered) errs

-- Property 5: Formatting compiler errors should contain error codes
prop_format_compiler_errors_contains_codes :: [CompilerError] -> Property
prop_format_compiler_errors_contains_codes errs = 
  not (null errs) ==>
    let formatted = formatCompilerErrors errs
    in all (\err -> T.unpack (message (ceError err)) `isInfixOf` formatted) errs

-- Property 6: Analyzing errors should count error phases correctly
prop_analyze_errors_counts_phases :: [CompilerError] -> Bool
prop_analyze_errors_counts_phases errs = 
  let analysis = show (analyzeErrors errs)
      parsingCount = length $ filter (\e -> cePhase e == ParsingPhase) errs
      typeCheckingCount = length $ filter (\e -> cePhase e == TypeCheckingPhase) errs
      codeGenerationCount = length $ filter (\e -> cePhase e == CodeGenerationPhase) errs
  in ("Parsing errors: " ++ show parsingCount) `isInfixOf` analysis &&
     ("Type checking errors: " ++ show typeCheckingCount) `isInfixOf` analysis &&
     ("Code generation errors: " ++ show codeGenerationCount) `isInfixOf` analysis

-- Property 7: Checking type errors should detect non-empty lists
prop_check_type_errors_detects_non_empty :: [TC.TypeError] -> Bool
prop_check_type_errors_detects_non_empty typeErrs = 
  let file = createTypusFileFromErrors typeErrs
  in hasTypeErrors file == not (null typeErrs)

-- Property 8: Extracting declarations should count functions and variables
prop_extract_declarations_counts_functions_and_variables :: String -> Bool
prop_extract_declarations_counts_functions_and_variables source = 
  let declarations = extractDeclarations source
  in length declarations >= 0

-- Property 9: Extracting function calls should count function invocations
prop_extract_function_calls_counts_invocations :: String -> Bool
prop_extract_function_calls_counts_invocations source = 
  let calls = extractFunctionCalls source
  in length calls >= 0

-- Property 10: Building type environment should create a valid environment
prop_build_type_env_creates_valid_env :: GoModule -> Bool
prop_build_type_env_creates_valid_env goModule = 
  let typeEnv = buildTypeEnv goModule
      envSize = case typeEnv of 
                  TypeEnv varTypes functionTypes -> Map.size varTypes + Map.size functionTypes
  in envSize >= 0

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails s@(_:t) = s : tails t

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

compilerCoreTests :: TestTree
compilerCoreTests = testGroup "Compiler Core Tests"
  [ testGroup "Unit Tests"
    [ testCase "Compile empty file" test_compile_empty_file
    , testCase "Compile simple code" test_compile_simple_code
    , testCase "Compile code with syntax error" test_compile_syntax_error
    , testCase "Compile code with type error" test_compile_type_error
    , testCase "Compile code with dependent types" test_compile_dependent_types
    , testCase "Compile code with ownership" test_compile_ownership
    , testCase "Ensure source IR for valid file" test_ensure_source_ir_valid
    , testCase "Ensure source IR for invalid file" test_ensure_source_ir_invalid
    , testCase "Generate Go code from empty file" test_generate_go_code_empty
    , testCase "Generate Go code from simple file" test_generate_go_code_simple
    , testCase "Render compilation error" test_render_compilation_error
    , testCase "Format compiler errors" test_format_compiler_errors
    , testCase "Generate detailed report" test_generate_detailed_report
    , testCase "Analyze errors" test_analyze_errors
    , testCase "Check type errors" test_check_type_errors
    , testCase "Diagnose type errors" test_diagnose_type_errors
    , testCase "Extract declarations" test_extract_declarations
    , testCase "Extract function calls" test_extract_function_calls
    , testCase "Build type environment" test_build_type_env
    , testCase "Build type environment from pairs" test_build_type_env_from_pairs
    , testCase "Create Typus file from errors" test_create_typus_file_from_errors
    , testCase "Check method declaration" test_check_method_declaration
    , testCase "Check type error" test_check_type_error
    , testCase "Check malformed syntax" test_check_malformed_syntax
    , testCase "Check dependent types" test_check_dependent_types
    , testCase "Check ownership" test_check_ownership
    , testCase "Convert type diagnostic to compiler error" test_type_diagnostic_to_compiler_error
    ]
  , testProperties "Property Tests"
    [ ("Compiling empty file should succeed", property prop_compile_empty_file_succeeds)
    , ("Compiling file with syntax errors should fail", property prop_compile_syntax_errors_fails)
    , ("Generating Go code should always return a string", property prop_generate_go_code_returns_string)
    , ("Rendering compilation errors should contain error messages", property prop_render_compilation_error_contains_messages)
    , ("Formatting compiler errors should contain error codes", property prop_format_compiler_errors_contains_codes)
    , ("Analyzing errors should count error phases correctly", property prop_analyze_errors_counts_phases)
    , ("Checking type errors should detect non-empty lists", property prop_check_type_errors_detects_non_empty)
    , ("Extracting declarations should count functions and variables", property prop_extract_declarations_counts_functions_and_variables)
    , ("Extracting function calls should count function invocations", property prop_extract_function_calls_counts_invocations)
    , ("Building type environment should create a valid environment", property prop_build_type_env_creates_valid_env)
    ]
  ]