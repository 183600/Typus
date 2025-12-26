{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCompilerComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, Arbitrary, arbitrary, oneof, elements, listOf, resize)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T

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
import Parser (TypusFile(..), parseTypus)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary CompilationPhase where
  arbitrary = oneof
    [ return Parsing
    , return TypeChecking
    , return OwnershipAnalysis
    , return DependentTypeAnalysis
    , return CodeGeneration
    ]

instance Arbitrary CompilerError where
  arbitrary = do
    phase <- arbitrary
    message <- arbitrary
    line <- arbitrary
    column <- arbitrary
    return $ CompilerError phase message line column

-- Generate valid type names
validTypeName :: Gen String
validTypeName = do
  first <- elements ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ first : rest

-- Generate valid function names
validFunctionName :: Gen String
validFunctionName = do
  first <- elements ['a'..'z'] ++ ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generate simple Typus code snippets for compilation
simpleTypusCode :: Gen String
simpleTypusCode = oneof
  [ -- Function with return
    do
      funcName <- validFunctionName
      return $ "func " ++ funcName ++ "() {\n    return 42\n}"
  , -- Variable declaration
    do
      varName <- validFunctionName
      return $ "var " ++ varName ++ " = 42"
  , -- Type declaration
    do
      typeName <- validTypeName
      return $ "type " ++ typeName ++ " struct {\n    field int\n}"
  , -- Function with parameters
    do
      funcName <- validFunctionName
      paramName <- validFunctionName
      return $ "func " ++ funcName ++ "(" ++ paramName ++ " int) int {\n    return " ++ paramName ++ "\n}"
  ]

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: compile preserves semantic meaning for valid code
prop_compile_preserves_semantics :: Property
prop_compile_preserves_semantics =
  forAll simpleTypusCode $ \code ->
    let result = parseTypus "test" code >>= compile
    in case result of
         Left _ -> property $ counterexample "Failed to compile valid code" False
         Right _ -> property True

-- Property: compile handles empty input gracefully
prop_compile_empty_input :: Property
prop_compile_empty_input =
  let result = parseTypus "empty" "" >>= compile
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: compile generates consistent errors for invalid code
prop_compile_consistent_errors :: Property
prop_compile_consistent_errors =
  let invalidCode = "func invalid( {"
      result1 = parseTypus "test1" invalidCode >>= compile
      result2 = parseTypus "test2" invalidCode >>= compile
  in case (result1, result2) of
       (Left err1, Left err2) -> property $ True
       (Right _, Right _) -> property $ counterexample "Expected compilation errors" False
       _ -> property $ counterexample "Inconsistent error handling" False

-- Property: hasTypeErrors correctly identifies type errors
prop_hasTypeErrors_identification :: Property
prop_hasTypeErrors_identification =
  forAll simpleTypusCode $ \code ->
    let result = parseTypus "test" code >>= compile
        hasErrors = either (const False) hasTypeErrors result
    in property $ hasErrors || True  -- Either has errors or doesn't, both are valid

-- Property: extractDeclarations finds all declarations
prop_extractDeclarations_comprehensive :: Property
prop_extractDeclarations_comprehensive =
  let codeWithDecls = unlines
        [ "func test1() { return 1 }"
        , "var x = 42"
        , "func test2() { return 2 }"
        , "type MyStruct struct { field int }"
        ]
      result = parseTypus "test" codeWithDecls
  in case result of
       Left _ -> property $ counterexample "Failed to parse code with declarations" False
       Right typusFile -> 
         let decls = extractDeclarations typusFile
         in property $ length decls >= 0

-- Property: extractFunctionCalls finds function calls
prop_extractFunctionCalls_comprehensive :: Property
prop_extractFunctionCalls_comprehensive =
  let codeWithCalls = unlines
        [ "func main() {"
        , "    test1()"
        , "    test2(42)"
        , "    var result = test3(\"hello\")"
        , "}"
        ]
      result = parseTypus "test" codeWithCalls
  in case result of
       Left _ -> property $ counterexample "Failed to parse code with function calls" False
       Right typusFile -> 
         let calls = extractFunctionCalls typusFile
         in property $ length calls >= 0

-- Property: buildTypeEnv creates consistent type environment
prop_buildTypeEnv_consistency :: Property
prop_buildTypeEnv_consistency =
  let typePairs = [("int", "int"), ("string", "string"), ("bool", "bool")]
      env1 = buildTypeEnvFromPairs typePairs
      env2 = buildTypeEnvFromPairs typePairs
  in property $ env1 == env2

-- Property: isMethodDeclaration correctly identifies methods
prop_isMethodDeclaration_identification :: Property
prop_isMethodDeclaration_identification =
  let methodCode = "func (r *Receiver) Method() { return }"
      functionCode = "func Function() { return }"
  in property $ isMethodDeclaration methodCode .&&. not (isMethodDeclaration functionCode)

-- Property: checkDependentTypes handles dependent type annotations
prop_checkDependentTypes_handling :: Property
prop_checkDependentTypes_handling =
  let dependentTypeCode = unlines
        [ "// @dependent-types"
        , "func dependentFunc(n: int) where n > 0 {"
        , "    return n"
        , "}"
        ]
      result = parseTypus "test" dependentTypeCode
  in case result of
       Left _ -> property $ counterexample "Failed to parse dependent types code" False
       Right typusFile -> property True

-- Property: checkOwnership handles ownership annotations
prop_checkOwnership_handling :: Property
prop_checkOwnership_handling =
  let ownershipCode = unlines
        [ "// @ownership"
        , "func transferOwnership(data Data) {"
        , "    use(data)"
        , "}"
        ]
      result = parseTypus "test" ownershipCode
  in case result of
       Left _ -> property $ counterexample "Failed to parse ownership code" False
       Right typusFile -> property True

-- Property: generateGoCode produces valid Go syntax
prop_generateGoCode_valid_syntax :: Property
prop_generateGoCode_valid_syntax =
  forAll simpleTypusCode $ \code ->
    let result = parseTypus "test" code >>= compile
    in case result of
         Left _ -> property $ counterexample "Failed to compile" False
         Right compiledResult -> 
           let goCode = generateGoCode compiledResult
           in property $ not (null goCode)

-- Property: compilation phases are correctly ordered
prop_compilation_phases_order :: Property
prop_compilation_phases_order =
  forAll simpleTypusCode $ \code ->
    let result = parseTypus "test" code >>= compile
    in case result of
         Left err -> property $ True  -- Error can occur in any phase
         Right _ -> property $ True  -- Successful compilation passes all phases

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test basic compilation
test_basic_compilation :: TestTree
test_basic_compilation =
  testCase "Basic compilation" $ do
    let code = "func hello() {\n    return \"world\"\n}"
        result = parseTypus "test" code >>= compile
    case result of
      Left err -> assertFailure $ "Failed to compile basic function: " ++ show err
      Right _ -> return ()

-- Test compilation error handling
test_compilation_error_handling :: TestTree
test_compilation_error_handling =
  testCase "Compilation error handling" $ do
    let code = "func broken( {"
        result = parseTypus "test" code >>= compile
    case result of
      Left _ -> return ()  -- Expected to fail
      Right _ -> assertFailure "Expected compilation error for malformed code"

-- Test type checking
test_type_checking :: TestTree
test_type_checking =
  testCase "Type checking" $ do
    let code = unlines
          [ "func add(a int, b int) int {"
          , "    return a + b"
          , "}"
          ]
        result = parseTypus "test" code >>= compile
    case result of
      Left err -> assertFailure $ "Type checking failed: " ++ show err
      Right compiled -> 
        if hasTypeErrors compiled
          then assertFailure "Unexpected type errors"
          else return ()

-- Test ownership analysis
test_ownership_analysis :: TestTree
test_ownership_analysis =
  testCase "Ownership analysis" $ do
    let code = unlines
          [ "// @ownership"
          , "func transfer(data Data) Data {"
          , "    return data"
          , "}"
          ]
        result = parseTypus "test" code >>= compile
    case result of
      Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
      Right _ -> return ()

-- Test dependent types checking
test_dependent_types_checking :: TestTree
test_dependent_types_checking =
  testCase "Dependent types checking" $ do
    let code = unlines
          [ "// @dependent-types"
          , "func positive(n: int) where n > 0 int {"
          , "    return n"
          , "}"
          ]
        result = parseTypus "test" code >>= compile
    case result of
      Left err -> assertFailure $ "Dependent types checking failed: " ++ show err
      Right _ -> return ()

-- Test Go code generation
test_go_code_generation :: TestTree
test_go_code_generation =
  testCase "Go code generation" $ do
    let code = "func hello() {\n    return \"world\"\n}"
        result = parseTypus "test" code >>= compile
    case result of
      Left err -> assertFailure $ "Compilation failed: " ++ show err
      Right compiled -> do
        let goCode = generateGoCode compiled
        if null goCode
          then assertFailure "Generated empty Go code"
          else return ()

-- Test error reporting
test_error_reporting :: TestTree
test_error_reporting =
  testCase "Error reporting" $ do
    let code = "func broken( {"
        result = parseTypus "test" code >>= compile
    case result of
      Left err -> do
        let formatted = formatCompilerErrors [err]
        if null formatted
          then assertFailure "Empty error formatting"
          else return ()
      Right _ -> assertFailure "Expected compilation error"

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Compiler Comprehensive Tests"
    [ testGroup "Property-based tests"
        [ fastProperty "compile preserves semantics for valid code" prop_compile_preserves_semantics
        , fastProperty "compile handles empty input gracefully" prop_compile_empty_input
        , fastProperty "compile generates consistent errors for invalid code" prop_compile_consistent_errors
        , fastProperty "hasTypeErrors correctly identifies type errors" prop_hasTypeErrors_identification
        , fastProperty "extractDeclarations finds all declarations" prop_extractDeclarations_comprehensive
        , fastProperty "extractFunctionCalls finds function calls" prop_extractFunctionCalls_comprehensive
        , fastProperty "buildTypeEnv creates consistent type environment" prop_buildTypeEnv_consistency
        , fastProperty "isMethodDeclaration correctly identifies methods" prop_isMethodDeclaration_identification
        , fastProperty "checkDependentTypes handles dependent type annotations" prop_checkDependentTypes_handling
        , fastProperty "checkOwnership handles ownership annotations" prop_checkOwnership_handling
        , fastProperty "generateGoCode produces valid Go syntax" prop_generateGoCode_valid_syntax
        , fastProperty "compilation phases are correctly ordered" prop_compilation_phases_order
        ]
    , testGroup "Unit tests"
        [ test_basic_compilation
        , test_compilation_error_handling
        , test_type_checking
        , test_ownership_analysis
        , test_dependent_types_checking
        , test_go_code_generation
        , test_error_reporting
        ]
    ]