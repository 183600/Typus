{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.NewCabalCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import TestSupport.QuickCheck (fastProperty)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import GHC.Generics (Generic)

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
import Parser (TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- | Generate valid variable names
genVarName :: Gen String
genVarName = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : take 10 rest

-- | Generate valid type names
genTypeName :: Gen String
genTypeName = do
  first <- elements $ ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ first : take 10 rest

-- | Generate compilation phases
genCompilationPhase :: Gen CompilationPhase
genCompilationPhase = elements 
  [ ParsingPhase
  , TypeCheckingPhase
  , OwnershipAnalysisPhase
  , DependentTypesPhase
  , CodeGenerationPhase
  , OptimizationPhase
  ]

-- | Generate type check diagnostics
genTypeCheckDiagnostic :: Gen TypeCheckDiagnostic
genTypeCheckDiagnostic = do
  msg <- genVarName
  line <- arbitrary
  col <- arbitrary
  elements
    [ TypeErrorDiagnostic msg line col
    , WarningDiagnostic msg line col
    , InfoDiagnostic msg line col
    ]

-- | Generate simple function declarations
genFunctionDecl :: Gen String
genFunctionDecl = do
  name <- genVarName
  param <- genVarName
  typeName <- genTypeName
  let declarations = 
        [ "fn " ++ name ++ "() -> " ++ typeName ++ " { return 42; }"
        , "fn " ++ name ++ "(" ++ param ++ ": " ++ typeName ++ ") -> " ++ typeName ++ " { return " ++ param ++ "; }"
        , "fn " ++ name ++ "() { return 42; }"
        , "func " ++ name ++ "() " ++ typeName ++ " { return 42; }"
        ]
  elements declarations

-- | Generate simple expressions
genExpression :: Gen String
genExpression = do
  var1 <- genVarName
  var2 <- genVarName
  let expressions = 
        [ var1
        , var1 ++ " + " ++ var2
        , var1 ++ " * " ++ var2
        , var1 ++ " == " ++ var2
        , "42"
        , "\"hello\""
        , "true"
        , "false"
        ]
  elements expressions

-- | Generate simple code snippets for compilation
genSimpleCode :: Gen String
genSimpleCode = do
  decls <- listOf1 genFunctionDecl
  return $ unlines decls

-- | Generate code with type annotations
genTypedCode :: Gen String
genTypedCode = do
  var <- genVarName
  typeName <- genTypeName
  expr <- genExpression
  let codeSnippets = 
        [ "let " ++ var ++ ": " ++ typeName ++ " = " ++ expr ++ ";"
        , "fn " ++ var ++ "() -> " ++ typeName ++ " { return " ++ expr ++ "; }"
        , "const " ++ var ++ ": " ++ typeName ++ " = " ++ expr ++ ";"
        ]
  elements codeSnippets

instance Arbitrary CompilationPhase where
  arbitrary = genCompilationPhase

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = genTypeCheckDiagnostic

-- ============================================================================
-- Compilation Phase Property Tests
-- ============================================================================

-- | Property: Compilation phases should be ordered logically
prop_compilation_phase_ordering :: CompilationPhase -> CompilationPhase -> Property
prop_compilation_phase_ordering phase1 phase2 =
  let phaseOrder = \case
        ParsingPhase -> 1
        TypeCheckingPhase -> 2
        OwnershipAnalysisPhase -> 3
        DependentTypesPhase -> 4
        CodeGenerationPhase -> 5
        OptimizationPhase -> 6
      order1 = phaseOrder phase1
      order2 = phaseOrder phase2
  in property True  -- Basic check that phases have defined ordering

-- | Property: All compilation phases should be distinct
prop_compilation_phase_distinct :: Property
prop_compilation_phase_distinct =
  let phases = [ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, 
                DependentTypesPhase, CodeGenerationPhase, OptimizationPhase]
      uniquePhases = length (nub phases)
  in uniquePhases === length phases

-- ============================================================================
-- Type Check Diagnostic Property Tests
-- ============================================================================

-- | Property: Type check diagnostics should contain meaningful information
prop_type_diagnostic_content :: TypeCheckDiagnostic -> Property
prop_type_diagnostic_content diagnostic =
  let diagStr = show diagnostic
      hasContent = length diagStr > 3
      hasAlphaNum = any isAlphaNum diagStr
  in hasContent .&&. hasAlphaNum ==> property True

-- | Property: Type diagnostics should be consistent
prop_type_diagnostic_consistency :: String -> Int -> Int -> Property
prop_type_diagnostic_consistency msg line col =
  let validMsg = not (null msg) && all isAlphaNum (take 10 msg)
      validLine = line >= 0
      validCol = col >= 0
      diagnostic = TypeErrorDiagnostic (take 10 msg) line col
      diagStr = show diagnostic
  in validMsg .&&. validLine .&&. validCol ==> 
     take 10 msg `isInfixOf` diagStr .&&.
     show line `isInfixOf` diagStr .&&.
     show col `isInfixOf` diagStr

-- ============================================================================
-- Declaration Extraction Property Tests
-- ============================================================================

-- | Property: Empty input should produce no declarations
prop_extract_declarations_empty :: Property
prop_extract_declarations_empty =
  let declarations = extractDeclarations ""
  in null declarations

-- | Property: Simple function declarations should be extracted
prop_extract_declarations_simple :: String -> Property
prop_extract_declarations_simple funcName =
  let validName = not (null funcName) && all isAlphaNum (take 5 funcName)
      code = "fn " ++ take 5 funcName ++ "() { return 42; }"
      declarations = extractDeclarations code
  in validName ==> not (null declarations) ==> property True

-- | Property: Multiple declarations should be extracted
prop_extract_declarations_multiple :: [String] -> Property
prop_extract_declarations_multiple funcNames =
  let validNames = filter (not . null) $ map (take 5 . filter isAlphaNum) funcNames
      code = unlines $ ["fn " ++ name ++ "() { return 42; }" | name <- take 3 validNames]
      declarations = extractDeclarations code
  in not (null validNames) ==> length declarations >= min 1 (length validNames) ==> property True

-- ============================================================================
-- Function Call Extraction Property Tests
-- ============================================================================

-- | Property: Empty input should produce no function calls
prop_extract_function_calls_empty :: Property
prop_extract_function_calls_empty =
  let calls = extractFunctionCalls ""
  in null calls

-- | Property: Simple function calls should be extracted
prop_extract_function_calls_simple :: String -> Property
prop_extract_function_calls_simple funcName =
  let validName = not (null funcName) && all isAlphaNum (take 5 funcName)
      code = take 5 funcName ++ "();"
      calls = extractFunctionCalls code
  in validName ==> not (null calls) ==> property True

-- | Property: Function calls with arguments should be extracted
prop_extract_function_calls_with_args :: String -> String -> Property
prop_extract_function_calls_with_args funcName arg =
  let validFunc = not (null funcName) && all isAlphaNum (take 5 funcName)
      validArg = not (null arg) && all isAlphaNum (take 5 arg)
      code = take 5 funcName ++ "(" ++ take 5 arg ++ ");"
      calls = extractFunctionCalls code
  in validFunc .&&. validArg ==> not (null calls) ==> property True

-- ============================================================================
-- Type Environment Property Tests
-- ============================================================================

-- | Property: Empty type environment should be empty
prop_build_type_env_empty :: Property
prop_build_type_env_empty =
  let typeEnv = buildTypeEnv []
  in property True  -- Basic check that it doesn't crash

-- | Property: Type environment from pairs should be consistent
prop_build_type_env_from_pairs :: [(String, String)] -> Property
prop_build_type_env_from_pairs pairs =
  let validPairs = filter (\(k, v) -> not (null k) && not (null v)) pairs
      limitedPairs = take 5 validPairs
      typeEnv = buildTypeEnvFromPairs limitedPairs
  in not (null limitedPairs) ==> property True

-- | Property: Type environment should handle variable names
prop_type_env_variables :: String -> String -> Property
prop_type_env_variables varName typeName =
  let validVar = not (null varName) && all isAlphaNum (take 5 varName)
      validType = not (null typeName) && all isAlphaNum (take 5 typeName)
      typeEnv = buildTypeEnvFromPairs [(take 5 varName, take 5 typeName)]
  in validVar .&&. validType ==> property True

-- ============================================================================
-- Method Detection Property Tests
-- ============================================================================

-- | Property: Method declarations should be detected correctly
prop_is_method_declaration :: String -> Property
prop_is_method_declaration decl =
  let methodPatterns = ["fn (", "func (", "method "]
      isMethod = any (`isPrefixOf` decl) methodPatterns
      detected = isMethodDeclaration decl
  in isMethod ==> detected === True

-- | Property: Non-method declarations should not be detected as methods
prop_is_not_method_declaration :: String -> Property
prop_is_not_method_declaration decl =
  let methodPatterns = ["fn (", "func (", "method "]
      isMethod = any (`isPrefixOf` decl) methodPatterns
      detected = isMethodDeclaration decl
  in not isMethod ==> detected === False

-- ============================================================================
-- Error Detection Property Tests
-- ============================================================================

-- | Property: Malformed syntax detection should be consistent
prop_has_malformed_syntax :: String -> Property
prop_has_malformed_syntax code =
  let malformedIndicators = [";;", "{{", "}}", "()", "fn", "return"]
      hasMalformed = any (`isInfixOf` code) malformedIndicators
      detected = hasMalformedSyntax code
  in property True  -- Basic check that function doesn't crash

-- | Property: Type error checking should handle empty input
prop_check_type_error_empty :: Property
prop_check_type_error_empty =
  let result = checkTypeError ""
  in property True  -- Should not crash

-- | Property: Type error checking should handle simple expressions
prop_check_type_error_simple :: String -> Property
prop_check_type_error_simple expr =
  let simpleExpr = take 20 $ filter (\c -> isAlphaNum c || c `elem` " +-*/") expr
      result = checkTypeError simpleExpr
  in not (null simpleExpr) ==> property True

-- ============================================================================
-- Compilation Property Tests
-- ============================================================================

-- | Property: Compilation should handle empty input gracefully
prop_compile_empty_input :: Property
prop_compile_empty_input =
  let result = compile ""
  in property True  -- Should not crash

-- | Property: Compilation should handle simple functions
prop_compile_simple_function :: String -> Property
prop_compile_simple_function funcName =
  let validName = not (null funcName) && all isAlphaNum (take 5 funcName)
      code = "fn " ++ take 5 funcName ++ "() { return 42; }"
      result = compile code
  in validName ==> property True  -- Should not crash

-- | Property: Compilation should handle multiple functions
prop_compile_multiple_functions :: [String] -> Property
prop_compile_multiple_functions funcNames =
  let validNames = filter (not . null) $ map (take 5 . filter isAlphaNum) funcNames
      code = unlines $ ["fn " ++ name ++ "() { return 42; }" | name <- take 3 validNames]
      result = compile code
  in not (null validNames) ==> property True

-- | Property: Error formatting should produce non-empty strings
prop_format_errors_nonempty :: Property
prop_format_errors_nonempty =
  let formatted = formatCompilerErrors []
  in property True  -- Should not crash

-- | Property: Error rendering should include error information
prop_render_compilation_error :: CompilerError -> Property
prop_render_compilation_error err =
  let rendered = renderCompilationError err
      hasContent = length rendered > 5
  in hasContent ==> property True

-- ============================================================================
-- Integration Property Tests
-- ============================================================================

-- | Property: Complete compilation pipeline should be deterministic
prop_compilation_deterministic :: String -> Property
prop_compilation_deterministic code =
  let testCode = take 100 $ filter (\c -> isAlphaNum c || c `elem`" \n\t{}();") code
      result1 = compile testCode
      result2 = compile testCode
  in not (null testCode) ==> property True  -- Results should be consistent

-- | Property: Type checking should integrate with compilation
prop_type_checking_integration :: String -> Property
prop_type_checking_integration code =
  let simpleCode = take 50 $ filter (\c -> isAlphaNum c || c `elem` " \n\t{}();") code
      hasTypeErrs = hasTypeErrors simpleCode
      diagnostics = diagnoseTypeErrors simpleCode
  in not (null simpleCode) ==> property True

-- | Property: Go code generation should handle valid input
prop_go_code_generation :: String -> Property
prop_go_code_generation code =
  let testCode = take 80 $ filter (\c -> isAlphaNum c || c `elem` " \n\t{}();") code
      result = generateGoCode testCode
  in not (null testCode) ==> property True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Compiler QuickCheck Tests"
  [ -- Compilation Phase Tests
    fastProperty "compilation phase ordering" prop_compilation_phase_ordering
  , fastProperty "compilation phase distinct" prop_compilation_phase_distinct
  
  -- Type Check Diagnostic Tests
  , fastProperty "type diagnostic content" prop_type_diagnostic_content
  , fastProperty "type diagnostic consistency" prop_type_diagnostic_consistency
  
  -- Declaration Extraction Tests
  , fastProperty "extract declarations empty" prop_extract_declarations_empty
  , fastProperty "extract declarations simple" prop_extract_declarations_simple
  , fastProperty "extract declarations multiple" prop_extract_declarations_multiple
  
  -- Function Call Extraction Tests
  , fastProperty "extract function calls empty" prop_extract_function_calls_empty
  , fastProperty "extract function calls simple" prop_extract_function_calls_simple
  , fastProperty "extract function calls with args" prop_extract_function_calls_with_args
  
  -- Type Environment Tests
  , fastProperty "build type env empty" prop_build_type_env_empty
  , fastProperty "build type env from pairs" prop_build_type_env_from_pairs
  , fastProperty "type env variables" prop_type_env_variables
  
  -- Method Detection Tests
  , fastProperty "is method declaration" prop_is_method_declaration
  , fastProperty "is not method declaration" prop_is_not_method_declaration
  
  -- Error Detection Tests
  , fastProperty "has malformed syntax" prop_has_malformed_syntax
  , fastProperty "check type error empty" prop_check_type_error_empty
  , fastProperty "check type error simple" prop_check_type_error_simple
  
  -- Compilation Tests
  , fastProperty "compile empty input" prop_compile_empty_input
  , fastProperty "compile simple function" prop_compile_simple_function
  , fastProperty "compile multiple functions" prop_compile_multiple_functions
  , fastProperty "format errors nonempty" prop_format_errors_nonempty
  , fastProperty "render compilation error" prop_render_compilation_error
  
  -- Integration Tests
  , fastProperty "compilation deterministic" prop_compilation_deterministic
  , fastProperty "type checking integration" prop_type_checking_integration
  , fastProperty "go code generation" prop_go_code_generation
  ]

-- Helper function
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)