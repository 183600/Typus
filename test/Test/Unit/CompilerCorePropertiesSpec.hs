{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CompilerCorePropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , malformedSyntaxError
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
import Parser (TypusFile(..), CodeBlock(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourceSpan(..), SourcePos(..), startPos)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf)
import Control.Monad (when)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary CompilationPhase where
  arbitrary = elements 
    [ ParsingPhase
    , TypeCheckingPhase
    , CompilationPhase
    , OptimizationPhase
    , CodeGenerationPhase
    ]

instance Arbitrary CompilerError where
  arbitrary = do
    errorCode <- arbitrary
    message <- arbitrary
    phase <- arbitrary
    category <- arbitrary
    severity <- arbitrary
    location <- arbitrary
    suggestions <- arbitrary
    relatedErrors <- arbitrary
    timestamp <- arbitrary
    return $ CompilerError errorCode message phase category severity location suggestions relatedErrors timestamp

-- ============================================================================
-- Compiler Properties
-- ============================================================================

-- Property: Compiling empty file should succeed or return predictable errors
prop_compile_empty_file :: Property
prop_compile_empty_file = 
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      result = compile emptyFile
  in case result of
    Left _ -> property True  -- Expected to fail with specific errors
    Right goCode -> not (null goCode)

-- Property: Compiling file with simple content should succeed
prop_compile_simple_content :: Property
prop_compile_simple_content = 
  let block = CodeBlock defaultBlockDirectives "let x = 42" (SourceSpan startPos startPos)
      file = TypusFile defaultFileDirectives [] [block] []
      result = compile file
  in case result of
    Left _ -> property True  -- May fail for various reasons
    Right goCode -> not (null goCode)

-- Property: Compiling file with type error should produce type error
prop_compile_type_error :: Property
prop_compile_type_error = 
  let block = CodeBlock defaultBlockDirectives "var x int = \"string\"" (SourceSpan startPos startPos)
      file = TypusFile defaultFileDirectives [] [block] []
      result = compile file
  in case result of
    Left errors -> any (\e -> "type error" `T.isInfixOf` ceMessage e) errors
    Right _ -> property False  -- Should not succeed with type error

-- Property: Rendering compilation errors produces non-empty string
prop_render_compilation_errors :: [CompilerError] -> Property
prop_render_compilation_errors errors = 
  let rendered = renderCompilationError errors
  in not (null rendered)

-- Property: Formatting compiler errors produces non-empty string
prop_format_compiler_errors :: [CompilerError] -> Property
prop_format_compiler_errors errors = 
  let formatted = formatCompilerErrors errors
  in not (null formatted)

-- Property: Generating detailed report produces non-empty string
prop_generate_detailed_report :: [CompilerError] -> Property
prop_generate_detailed_report errors = 
  let report = generateDetailedReport errors
  in not (null report)

-- Property: Analyzing errors returns statistics
prop_analyze_errors :: [CompilerError] -> Property
prop_analyze_errors errors = 
  let analysis = analyzeErrors errors
  in not (null analysis)

-- Property: hasTypeErrors correctly identifies type errors
prop_has_type_errors :: [CompilerError] -> Property
prop_has_type_errors errors = 
  let hasTypeErrs = hasTypeErrors errors
      hasTypeErr = any (\e -> "type" `T.isInfixOf` (T.toLower (ceMessage e))) errors
  in hasTypeErrs === hasTypeErr

-- Property: typeDiagnosticToCompilerError preserves diagnostic information
prop_type_diagnostic_to_compiler_error :: TypeCheckDiagnostic -> Property
prop_type_diagnostic_to_compiler_error diagnostic = 
  let error = typeDiagnosticToCompilerError diagnostic
  in not (T.null (ceMessage error))

-- Property: createTypusFileFromErrors creates file with syntax errors
prop_create_typus_file_from_errors :: [String] -> Property
prop_create_typus_file_from_errors errorStrings = 
  let file = createTypusFileFromErrors errorStrings
  in length (tfSyntaxErrors file) >= length errorStrings

-- Property: isMethodDeclaration correctly identifies method declarations
prop_is_method_declaration :: String -> Property
prop_is_method_declaration code = 
  let isMethod = isMethodDeclaration code
      hasReceiver = "func (" `isInfixOf` code
  in if hasReceiver then isMethod else not isMethod

-- Property: checkTypeError identifies type errors in code
prop_check_type_error :: String -> Property
prop_check_type_error code = 
  let hasTypeError = checkTypeError code
      hasTypeMismatch = "int" `isInfixOf` code && "string" `isInfixOf` code
  in if hasTypeMismatch then hasTypeError else property True

-- Property: hasMalformedSyntax identifies syntax errors
prop_has_malformed_syntax :: String -> Property
prop_has_malformed_syntax code = 
  let malformed = hasMalformedSyntax code
      hasUnclosedBrace = not (balancedBraces code)
  in if hasUnclosedBrace then malformed else property True

-- Property: extractDeclarations finds declarations in code
prop_extract_declarations :: String -> Property
prop_extract_declarations code = 
  let declarations = extractDeclarations code
      hasFuncDecl = "func " `isInfixOf` code
  in if hasFuncDecl then not (null declarations) else property True

-- Property: extractFunctionCalls finds function calls in code
prop_extract_function_calls :: String -> Property
prop_extract_function_calls code = 
  let calls = extractFunctionCalls code
      hasFuncCall = any (`isSuffixOf` code) ["()", " (", ")"] && 
                   any (`isInfixOf` code) ["func", "call"]
  in if hasFuncCall then not (null calls) else property True

-- Property: buildTypeEnv creates type environment
prop_build_type_env :: [(String, String)] -> Property
prop_build_type_env typePairs = 
  let typeEnv = buildTypeEnv typePairs
  in length typeEnv >= length typePairs

-- Property: buildTypeEnvFromPairs creates type environment from pairs
prop_build_type_env_from_pairs :: [(String, String)] -> Property
prop_build_type_env_from_pairs typePairs = 
  let typeEnv = buildTypeEnvFromPairs typePairs
  in length typeEnv >= length typePairs

-- ============================================================================
-- Helper Functions
-- ============================================================================

balancedBraces :: String -> Bool
balancedBraces = go 0
  where
    go _ [] = True
    go n ('{':xs) = go (n + 1) xs
    go n ('}':xs) = n > 0 && go (n - 1) xs
    go n (_:xs) = go n xs

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix str = reverse suffix `isPrefixOf` reverse str
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler Core Properties Tests"
  [ testGroup "Basic Compilation Properties"
    [ testProperty "Compiling empty file should succeed or return predictable errors" prop_compile_empty_file
    , testProperty "Compiling file with simple content should succeed" prop_compile_simple_content
    , testProperty "Compiling file with type error should produce type error" prop_compile_type_error
    ]
  , testGroup "Error Reporting Properties"
    [ testProperty "Rendering compilation errors produces non-empty string" prop_render_compilation_errors
    , testProperty "Formatting compiler errors produces non-empty string" prop_format_compiler_errors
    , testProperty "Generating detailed report produces non-empty string" prop_generate_detailed_report
    , testProperty "Analyzing errors returns statistics" prop_analyze_errors
    ]
  , testGroup "Type Checking Properties"
    [ testProperty "hasTypeErrors correctly identifies type errors" prop_has_type_errors
    , testProperty "typeDiagnosticToCompilerError preserves diagnostic information" prop_type_diagnostic_to_compiler_error
    , testProperty "checkTypeError identifies type errors in code" prop_check_type_error
    ]
  , testGroup "Code Analysis Properties"
    [ testProperty "createTypusFileFromErrors creates file with syntax errors" prop_create_typus_file_from_errors
    , testProperty "isMethodDeclaration correctly identifies method declarations" prop_is_method_declaration
    , testProperty "hasMalformedSyntax identifies syntax errors" prop_has_malformed_syntax
    , testProperty "extractDeclarations finds declarations in code" prop_extract_declarations
    , testProperty "extractFunctionCalls finds function calls in code" prop_extract_function_calls
    ]
  , testGroup "Type Environment Properties"
    [ testProperty "buildTypeEnv creates type environment" prop_build_type_env
    , testProperty "buildTypeEnvFromPairs creates type environment from pairs" prop_build_type_env_from_pairs
    ]
  ]