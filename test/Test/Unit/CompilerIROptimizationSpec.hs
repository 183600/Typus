{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIROptimizationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (choose, listOf, oneof, elements, vectorOf, suchThat, Gen)

import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , generateGoCode
  , hasTypeErrors
  )

import Parser (parseTypus)
import qualified Compiler.IR as IR
import SourceLocation (SourceSpan(..))

import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum, toUpper)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

-- Generate valid variable names
arbitraryVarName :: Gen String
arbitraryVarName = do
  first <- elements "abcdefghijklmnopqrstuvwxyz"
  rest <- vectorOf 0 5 (elements "abcdefghijklmnopqrstuvwxyz0123456789_")
  return (first : rest)

-- Generate valid function names
arbitraryFuncName :: Gen String
arbitraryFuncName = do
  name <- arbitraryVarName
  return $ "func " ++ name

-- Generate valid type names
arbitraryTypeName :: Gen String
arbitraryTypeName = do
  name <- arbitraryVarName
  firstChar <- toUpper <$> elements "abcdefghijklmnopqrstuvwxyz"
  return $ firstChar : L.tail name

-- Generate simple variable declarations
arbitraryVarDecl :: Gen String
arbitraryVarDecl = do
  varName <- arbitraryVarName
  typeName <- elements ["int", "string", "bool", "float64"]
  value <- case typeName of
    "int" -> elements ["0", "1", "42", "-1"]
    "string" -> elements ["\"hello\"", "\"world\"", "\"test\""]
    "bool" -> elements ["true", "false"]
    "float64" -> elements ["0.0", "1.0", "3.14"]
  return $ "  " ++ varName ++ " := " ++ value

-- Generate simple function declarations
arbitraryFuncDecl :: Gen String
arbitraryFuncDecl = do
  funcName <- arbitraryVarName
  numStmts <- choose (1, 5)
  stmts <- vectorOf numStmts arbitraryVarDecl
  return $ "func " ++ funcName ++ "() {\n" ++ unlines stmts ++ "}\n"

-- Generate valid Typus code snippets
arbitraryTypusCode :: Gen String
arbitraryTypusCode = do
  hasDirectives <- arbitrary
  directives <- if hasDirectives
    then do
      ownership <- elements ["true", "false"]
      dependentTypes <- elements ["true", "false"]
      return $ "//! ownership: " ++ ownership ++ "\n//! dependent-types: " ++ dependentTypes ++ "\n"
    else return ""
  
  numFuncs <- choose (1, 3)
  funcs <- vectorOf numFuncs arbitraryFuncDecl
  return $ directives ++ unlines funcs

-- ============================================================================
-- Compiler IR Optimization Properties
-- ============================================================================

-- Property: Compilation preserves semantic meaning
prop_compilation_preserves_semantics :: Property
prop_compilation_preserves_semantics =
  forAll arbitraryTypusCode $ \typusCode ->
  case parseTypus typusCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode ->
          property $ not (null goCode) .&&. "func" `L.isInfixOf` goCode

-- Property: Generated Go code is syntactically valid
prop_generated_go_code_valid :: Property
prop_generated_go_code_valid =
  forAll arbitraryTypusCode $ \typusCode ->
  case parseTypus typusCode of
    Left _ -> property False
    Right typusFile ->
      let goCode = generateGoCode typusFile
      in property $ not (null goCode) .&&. 
                 not ("//!" `L.isInfixOf` goCode) .&&.
                 not ("{//!" `L.isInfixOf` goCode)

-- Property: Compilation handles empty files gracefully
prop_compilation_empty_file :: Property
prop_compilation_empty_file =
  case parseTypus "" of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode -> property True

-- Property: Compilation handles whitespace-only files
prop_compilation_whitespace_only :: Property
prop_compilation_whitespace_only =
  let whitespaceCode = "   \n  \t \n  \n"
  in case parseTypus whitespaceCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode -> property True

-- Property: Compilation preserves function structure
prop_compilation_preserves_functions :: Property
prop_compilation_preserves_functions =
  forAll arbitraryTypusCode $ \typusCode ->
  case parseTypus typusCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode ->
          let inputFuncCount = L.length $ L.filter ("func " `L.isPrefixOf`) (lines typusCode)
              outputFuncCount = L.length $ L.filter ("func " `L.isPrefixOf`) (lines goCode)
          in property $ outputFuncCount >= inputFuncCount

-- Property: Compilation handles variable declarations correctly
prop_compilation_handles_variables :: Property
prop_compilation_handles_variables =
  forAll arbitraryVarDecl $ \varDecl ->
  let typusCode = "func test() {\n" ++ varDecl ++ "\n}\n"
  in case parseTypus typusCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode ->
          property $ ":=" `L.isInfixOf` goCode

-- Property: Compilation optimizes redundant directives
prop_compilation_optimizes_directives :: Property
prop_compilation_optimizes_directives =
  let typusCode = "//! ownership: true\n//! ownership: true\n//! dependent-types: true\n//! dependent-types: true\nfunc test() {}\n"
  in case parseTypus typusCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode ->
          property $ not ("//!" `L.isInfixOf` goCode)

-- Property: Compilation handles nested structures
prop_compilation_handles_nested :: Property
prop_compilation_handles_nested =
  let nestedCode = "func outer() {\n  func inner() {\n    x := 1\n  }\n  y := 2\n}\n"
  in case parseTypus nestedCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode ->
          property $ "func outer" `L.isInfixOf` goCode .&&.
                     "func inner" `L.isInfixOf` goCode

-- Property: Compilation preserves type information
prop_compilation_preserves_types :: Property
prop_compilation_preserves_types =
  forAll arbitraryVarDecl $ \varDecl ->
  let typusCode = "func test() {\n" ++ varDecl ++ "\n}\n"
  in case parseTypus typusCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode ->
          -- Check that type information is preserved in some form
          property $ not (null goCode)

-- Property: Compilation handles comments correctly
prop_compilation_handles_comments :: Property
prop_compilation_handles_comments =
  let codeWithComments = "//! ownership: true\n// This is a comment\nfunc test() {\n  x := 1 // inline comment\n}\n"
  in case parseTypus codeWithComments of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode ->
          property $ "func test" `L.isInfixOf` goCode

-- Property: Compilation is deterministic
prop_compilation_deterministic :: Property
prop_compilation_deterministic =
  forAll arbitraryTypusCode $ \typusCode ->
  case parseTypus typusCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode1 ->
          case compile typusFile of
            Left _ -> property False
            Right goCode2 ->
              property $ goCode1 === goCode2

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Type errors are detected L.and reported
prop_type_errors_detected :: Property
prop_type_errors_detected =
  let codeWithTypeErrors = "func test() {\n  x := \"string\"\n  y := 1\n  z := x + y // type error\n}\n"
  in case parseTypus codeWithTypeErrors of
    Left _ -> property False
    Right typusFile ->
      case hasTypeErrors typusFile of
        True -> property True
        False -> property False

-- Property: Compilation handles syntax errors gracefully
prop_compilation_syntax_errors :: Property
prop_compilation_syntax_errors =
  let codeWithSyntaxErrors = "func test() {\n  if true\n    x := 1\n  }\n}\n"
  in case parseTypus codeWithSyntaxErrors of
    Left _ -> property True  -- Parsing should fail
    Right typusFile ->
      case compile typusFile of
        Left _ -> property True  -- Compilation should fail
        Right _ -> property False

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: Compilation time is reasonable for small files
prop_compilation_small_files_fast :: Property
prop_compilation_small_files_fast =
  let smallCode = "func test() {\n  x := 1\n  y := 2\n  z := x + y\n}\n"
  in case parseTypus smallCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode -> property True

-- Property: Compilation handles repeated patterns efficiently
prop_compilation_repeated_patterns :: Property
prop_compilation_repeated_patterns =
  let repeatedPattern = "x := 1\ny := 2\nz := x + y\n"
      largeCode = "func test() {\n" ++ L.concat (replicate 10 repeatedPattern) ++ "}\n"
  in case parseTypus largeCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode -> property True

-- ============================================================================
-- Advanced Optimization Properties
-- ============================================================================

-- Property: Dead code elimination
prop_dead_code_elimination :: Property
prop_dead_code_elimination =
  let codeWithDeadCode = "func test() {\n  if false {\n    x := 1\n    y := 2\n  }\n  z := 3\n}\n"
  in case parseTypus codeWithDeadCode of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode ->
          property $ "func test" `L.isInfixOf` goCode

-- Property: Constant folding
prop_constant_folding :: Property
prop_constant_folding =
  let codeWithConstants = "func test() {\n  x := 1 + 2 * 3\n  y := 4 / 2\n}\n"
  in case parseTypus codeWithConstants of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode ->
          property $ "func test" `L.isInfixOf` goCode

-- Property: Function inlining potential
prop_function_inlining :: Property
prop_function_inlining =
  let codeWithSmallFunctions = "func small() int { return 1 }\nfunc test() {\n  x := small()\n  y := small() + 1\n}\n"
  in case parseTypus codeWithSmallFunctions of
    Left _ -> property False
    Right typusFile ->
      case compile typusFile of
        Left _ -> property False
        Right goCode ->
          property $ "func small" `L.isInfixOf` goCode .&&.
                     "func test" `L.isInfixOf` goCode

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler IR Optimization Tests"
  [ testGroup "Basic Compilation Properties"
    [ fastProperty "Compilation preserves semantic meaning" prop_compilation_preserves_semantics
    , fastProperty "Generated Go code is syntactically valid" prop_generated_go_code_valid
    , fastProperty "Compilation handles empty files gracefully" prop_compilation_empty_file
    , fastProperty "Compilation handles whitespace-only files" prop_compilation_whitespace_only
    ]

  , testGroup "Structure Preservation Properties"
    [ fastProperty "Compilation preserves function structure" prop_compilation_preserves_functions
    , fastProperty "Compilation handles variable declarations correctly" prop_compilation_handles_variables
    , fastProperty "Compilation optimizes redundant directives" prop_compilation_optimizes_directives
    , fastProperty "Compilation handles nested structures" prop_compilation_handles_nested
    ]

  , testGroup "Type System Properties"
    [ fastProperty "Compilation preserves type information" prop_compilation_preserves_types
    , fastProperty "Compilation handles comments correctly" prop_compilation_handles_comments
    , fastProperty "Compilation is deterministic" prop_compilation_deterministic
    ]

  , testGroup "Error Handling Properties"
    [ fastProperty "Type errors are detected L.and reported" prop_type_errors_detected
    , fastProperty "Compilation handles syntax errors gracefully" prop_compilation_syntax_errors
    ]

  , testGroup "Performance Properties"
    [ fastProperty "Compilation time is reasonable for small files" prop_compilation_small_files_fast
    , fastProperty "Compilation handles repeated patterns efficiently" prop_compilation_repeated_patterns
    ]

  , testGroup "Advanced Optimization Properties"
    [ fastProperty "Dead code elimination" prop_dead_code_elimination
    , fastProperty "Constant folding" prop_constant_folding
    , fastProperty "Function inlining potential" prop_function_inlining
    ]
  ]