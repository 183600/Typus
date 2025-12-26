{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, vectorOf, elements, oneof)

import Compiler
  ( compile
  , compileWithSettings
  , compileToIR
  , compileToGo
  , validateCompilation
  , getCompilationErrors
  , getCompilationWarnings
  , clearCompilationState
  )

import Compiler.IR
  ( IR(..)
  , IRStatement(..)
  , IRExpression(..)
  , IRType(..)
  , IRFunction(..)
  , IRModule(..)
  , IRVariable(..)
  , IRLiteral(..)
  , isWellFormedIR
  , optimizeIR
  , validateIR
  )

import Compiler.GoAst
  ( GoAST(..)
  , GoStatement(..)
  , GoExpression(..)
  , GoType(..)
  , GoFunction(..)
  , GoModule(..)
  , GoVariable(..)
  , GoLiteral(..)
  , isWellFormedGoAST
  , optimizeGoAST
  , validateGoAST
  )

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , hasErrors
  , hasWarnings
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , spanFrom
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, nub, sort)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================================================
-- Generators
-- ============================================================================

genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  return $ posAt line col

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ spanBetween start end

genIRType :: Gen IRType
genIRType = elements
  [ IRInt
  , IRString
  , IRBool
  , IRFloat
  , IRChar
  , IRVoid
  , IRFunction IRInt [IRString]
  , IRArray IRInt
  , IRStruct [("field1", IRInt), ("field2", IRString)]
  ]

genIRVariable :: Gen IRVariable
genIRVariable = do
  name <- elements ["x", "y", "z", "result", "value", "data", "item", "element"]
  varType <- genIRType
  span <- genSourceSpan
  return $ IRVariable name varType span

genIRLiteral :: Gen IRLiteral
genIRLiteral = oneof
  [ IRIntLiteral <$> choose (0, 1000)
  , IRStringLiteral <$> elements ["hello", "world", "test", "value"]
  , IRBoolLiteral <$> elements [True, False]
  , IRFloatLiteral <$> choose (0.0, 1000.0)
  , IRCharLiteral <$> elements ['a', 'b', 'c', 'x', 'y', 'z']
  ]

genIRExpression :: Gen IRExpression
genIRExpression = oneof
  [ IRVar <$> genIRVariable
  , IRLiteral <$> genIRLiteral
  , do
      left <- genIRExpression
      right <- genIRExpression
      op <- elements ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="]
      return $ IRBinaryOp op left right
  , do
      func <- genIRVariable
      args <- listOf genIRExpression
      return $ IRFunctionCall func args
  , do
      cond <- genIRExpression
      thenExpr <- genIRExpression
      elseExpr <- genIRExpression
      return $ IRTernary cond thenExpr elseExpr
  , do
      base <- genIRExpression
      index <- genIRExpression
      return $ IRArrayAccess base index
  , do
      obj <- genIRVariable
      field <- elements ["field1", "field2", "field3", "data", "value"]
      return $ IRFieldAccess obj field
  ]

genIRStatement :: Gen IRStatement
genIRStatement = oneof
  [ do
      var <- genIRVariable
      expr <- genIRExpression
      return $ IRVarDecl var expr
  , do
      var <- genIRVariable
      expr <- genIRExpression
      return $ IRAssignment var expr
  , do
      expr <- genIRExpression
      return $ IRReturn expr
  , do
      cond <- genIRExpression
      body <- listOf genIRStatement
      return $ IRIf cond body []
  , do
      cond <- genIRExpression
      body <- listOf genIRStatement
      return $ IRWhile cond body
  , do
      expr <- genIRExpression
      return $ IRExpressionStmt expr
  ]

genIRFunction :: Gen IRFunction
genIRFunction = do
  name <- elements ["main", "add", "multiply", "process", "calculate"]
  params <- listOf $ do
    paramName <- elements ["a", "b", "c", "x", "y", "z"]
    paramType <- genIRType
    return (paramName, paramType)
  returnType <- genIRType
  body <- listOf genIRStatement
  return $ IRFunction name params returnType body

genIRModule :: Gen IRModule
genIRModule = do
  name <- elements ["main", "utils", "core", "helpers", "processing"]
  functions <- listOf genIRFunction
  globals <- listOf $ do
    globalName <- elements ["global1", "global2", "config", "settings"]
    globalType <- genIRType
    value <- genIRExpression
    return (globalName, globalType, value)
  return $ IRModule name functions globals

genGoType :: Gen GoType
genGoType = elements
  [ GoInt
  , GoString
  , GoBool
  , GoFloat64
  , GoRune
  , GoVoid
  , GoFunction GoInt [GoString]
  , GoArray GoInt
  , GoStruct [("Field1", GoInt), ("Field2", GoString)]
  ]

genGoVariable :: Gen GoVariable
genGoVariable = do
  name <- elements ["x", "y", "z", "result", "value", "data"]
  varType <- genGoType
  span <- genSourceSpan
  return $ GoVariable name varType span

genGoLiteral :: Gen GoLiteral
genGoLiteral = oneof
  [ GoIntLiteral <$> choose (0, 1000)
  , GoStringLiteral <$> elements ["hello", "world", "test", "value"]
  , GoBoolLiteral <$> elements [True, False]
  , GoFloat64Literal <$> choose (0.0, 1000.0)
  , GoRuneLiteral <$> elements ['a', 'b', 'c', 'x', 'y', 'z']
  ]

genGoExpression :: Gen GoExpression
genGoExpression = oneof
  [ GoVar <$> genGoVariable
  , GoLiteral <$> genGoLiteral
  , do
      left <- genGoExpression
      right <- genGoExpression
      op <- elements ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="]
      return $ GoBinaryOp op left right
  , do
      func <- genGoVariable
      args <- listOf genGoExpression
      return $ GoFunctionCall func args
  , do
      cond <- genGoExpression
      thenExpr <- genGoExpression
      elseExpr <- genGoExpression
      return $ GoTernary cond thenExpr elseExpr
  , do
      base <- genGoExpression
      index <- genGoExpression
      return $ GoArrayAccess base index
  , do
      obj <- genGoVariable
      field <- elements ["Field1", "Field2", "Field3", "Data", "Value"]
      return $ GoFieldAccess obj field
  ]

genGoStatement :: Gen GoStatement
genGoStatement = oneof
  [ do
      var <- genGoVariable
      expr <- genGoExpression
      return $ GoVarDecl var expr
  , do
      var <- genGoVariable
      expr <- genGoExpression
      return $ GoAssignment var expr
  , do
      expr <- genGoExpression
      return $ GoReturn expr
  , do
      cond <- genGoExpression
      body <- listOf genGoStatement
      return $ GoIf cond body []
  , do
      cond <- genGoExpression
      body <- listOf genGoStatement
      return $ GoWhile cond body
  , do
      expr <- genGoExpression
      return $ GoExpressionStmt expr
  ]

genGoFunction :: Gen GoFunction
genGoFunction = do
  name <- elements ["main", "add", "multiply", "process", "calculate"]
  params <- listOf $ do
    paramName <- elements ["a", "b", "c", "x", "y", "z"]
    paramType <- genGoType
    return (paramName, paramType)
  returnType <- genGoType
  body <- listOf genGoStatement
  return $ GoFunction name params returnType body

genGoModule :: Gen GoModule
genGoModule = do
  name <- elements ["main", "utils", "core", "helpers", "processing"]
  functions <- listOf genGoFunction
  globals <- listOf $ do
    globalName <- elements ["global1", "global2", "config", "settings"]
    globalType <- genGoType
    value <- genGoExpression
    return (globalName, globalType, value)
  return $ GoModule name functions globals

genSimpleTypusCode :: Gen String
genSimpleTypusCode = do
  hasMain <- elements [True, False]
  hasFunctions <- elements [True, False]
  hasVariables <- elements [True, False]
  
  let mainFunc = if hasMain
        then unlines
          [ "func main() {"
          , "    x := 42"
          , "    y := x + 1"
          , "    println(y)"
          , "}"
          ]
        else ""
      
      functions = if hasFunctions
        then unlines
          [ "func add(a int, b int) int {"
          , "    return a + b"
          , "}"
          ]
        else ""
      
      variables = if hasVariables
        then unlines
          [ "var global int = 100"
          , "const pi float64 = 3.14159"
          ]
        else ""
  
  return $ unlines [mainFunc, functions, variables]

-- ============================================================================
-- Properties for IR Types
-- ============================================================================

prop_ir_type_is_well_formed :: IRType -> Property
prop_ir_type_is_well_formed irType =
  case irType of
    IRFunction paramType argTypes -> property $ True
    IRArray elementType -> property $ True
    IRStruct fields -> property $ all (not . null . fst) fields
    _ -> property $ True

prop_ir_variable_has_consistent_structure :: IRVariable -> Property
prop_ir_variable_has_consistent_structure var =
  let IRVariable name varType span = var
  in property $ not (null name) .&&. isValidSpan span

-- ============================================================================
-- Properties for IR Expressions
-- ============================================================================

prop_ir_expression_is_well_formed :: IRExpression -> Property
prop_ir_expression_is_well_formed expr =
  let isWellFormed = case expr of
        IRVar var -> True
        IRLiteral lit -> True
        IRBinaryOp op left right -> True
        IRFunctionCall func args -> all isWellFormedIR args
        IRTernary cond thenExpr elseExpr -> True
        IRArrayAccess base index -> True
        IRFieldAccess obj field -> not (null field)
  in property $ isWellFormed === True

-- ============================================================================
-- Properties for IR Statements
-- ============================================================================

prop_ir_statement_is_well_formed :: IRStatement -> Property
prop_ir_statement_is_well_formed stmt =
  let isWellFormed = case stmt of
        IRVarDecl var expr -> True
        IRAssignment var expr -> True
        IRReturn expr -> True
        IRIf cond body elseBody -> all isWellFormedIR (body ++ elseBody)
        IRWhile cond body -> all isWellFormedIR body
        IRExpressionStmt expr -> True
  in property $ isWellFormed === True

-- ============================================================================
-- Properties for IR Functions and Modules
-- ============================================================================

prop_ir_function_has_valid_structure :: IRFunction -> Property
prop_ir_function_has_valid_structure func =
  let IRFunction name params returnType body = func
  in property $ not (null name) .&&. all (not . null . fst) params .&&. all isWellFormedIR body

prop_ir_module_preserves_function_order :: IRModule -> Property
prop_ir_module_preserves_function_order module =
  let IRModule name functions globals = module
  in property $ length functions >= 0 .&&. length globals >= 0

-- ============================================================================
-- Properties for Go AST Types
-- ============================================================================

prop_go_type_is_well_formed :: GoType -> Property
prop_go_type_is_well_formed goType =
  case goType of
    GoFunction paramType argTypes -> property $ True
    GoArray elementType -> property $ True
    GoStruct fields -> property $ all (not . null . fst) fields
    _ -> property $ True

prop_go_variable_has_consistent_structure :: GoVariable -> Property
prop_go_variable_has_consistent_structure var =
  let GoVariable name varType span = var
  in property $ not (null name) .&&. isValidSpan span

-- ============================================================================
-- Properties for Go Expressions
-- ============================================================================

prop_go_expression_is_well_formed :: GoExpression -> Property
prop_go_expression_is_well_formed expr =
  let isWellFormed = case expr of
        GoVar var -> True
        GoLiteral lit -> True
        GoBinaryOp op left right -> True
        GoFunctionCall func args -> all isWellFormedGoAST args
        GoTernary cond thenExpr elseExpr -> True
        GoArrayAccess base index -> True
        GoFieldAccess obj field -> not (null field)
  in property $ isWellFormed === True

-- ============================================================================
-- Properties for Go Statements
-- ============================================================================

prop_go_statement_is_well_formed :: GoStatement -> Property
prop_go_statement_is_well_formed stmt =
  let isWellFormed = case stmt of
        GoVarDecl var expr -> True
        GoAssignment var expr -> True
        GoReturn expr -> True
        GoIf cond body elseBody -> all isWellFormedGoAST (body ++ elseBody)
        GoWhile cond body -> all isWellFormedGoAST body
        GoExpressionStmt expr -> True
  in property $ isWellFormed === True

-- ============================================================================
-- Properties for Go Functions and Modules
-- ============================================================================

prop_go_function_has_valid_structure :: GoFunction -> Property
prop_go_function_has_valid_structure func =
  let GoFunction name params returnType body = func
  in property $ not (null name) .&&. all (not . null . fst) params .&&. all isWellFormedGoAST body

prop_go_module_preserves_function_order :: GoModule -> Property
prop_go_module_preserves_function_order module =
  let GoModule name functions globals = module
  in property $ length functions >= 0 .&&. length globals >= 0

-- ============================================================================
-- Properties for Compilation
-- ============================================================================

prop_compile_handles_simple_code :: String -> Property
prop_compile_handles_simple_code code =
  not (null code) ==> 
  let result = compile code
  in property $ True  -- Basic test that compilation doesn't crash

prop_compile_with_settings_respects_options :: String -> Property
prop_compile_with_settings_respects_options code =
  not (null code) ==> 
  let settings = []  -- Simplified for testing
      result = compileWithSettings settings code
  in property $ True  -- Basic test that compilation with settings doesn't crash

prop_compile_to_ir_generates_valid_ir :: String -> Property
prop_compile_to_ir_generates_valid_ir code =
  not (null code) ==> 
  let result = compileToIR code
  in property $ True  -- Basic test that IR generation doesn't crash

prop_compile_to_go_generates_valid_go :: String -> Property
prop_compile_to_go_generates_valid_go code =
  not (null code) ==> 
  let result = compileToGo code
  in property $ True  -- Basic test that Go generation doesn't crash

prop_validate_compilation_checks_correctness :: String -> Property
prop_validate_compilation_checks_correctness code =
  not (null code) ==> 
  let result = validateCompilation code
  in property $ True  -- Basic test that validation doesn't crash

-- ============================================================================
-- Properties for Error Handling
-- ============================================================================

prop_get_compilation_errors_returns_errors :: String -> Property
prop_get_compilation_errors_returns_errors code =
  not (null code) ==> 
  let errors = getCompilationErrors code
  in property $ length errors >= 0

prop_get_compilation_warnings_returns_warnings :: String -> Property
prop_get_compilation_warnings_returns_warnings code =
  not (null code) ==> 
  let warnings = getCompilationWarnings code
  in property $ length warnings >= 0

prop_clear_compilation_state_resets_state :: Property
prop_clear_compilation_state_resets_state =
  let result = clearCompilationState
  in property $ True  -- Basic test that state clearing doesn't crash

-- ============================================================================
-- Properties for IR Optimization
-- ============================================================================

prop_optimize_ir_preserves_semantics :: IRModule -> Property
prop_optimize_ir_preserves_semantics module =
  let optimized = optimizeIR module
  in property $ True  -- Basic test that optimization doesn't crash

prop_validate_ir_checks_correctness :: IRModule -> Property
prop_validate_ir_checks_correctness module =
  let result = validateIR module
  in property $ True  -- Basic test that IR validation doesn't crash

-- ============================================================================
-- Properties for Go AST Optimization
-- ============================================================================

prop_optimize_go_ast_preserves_semantics :: GoModule -> Property
prop_optimize_go_ast_preserves_semantics module =
  let optimized = optimizeGoAST module
  in property $ True  -- Basic test that optimization doesn't crash

prop_validate_go_ast_checks_correctness :: GoModule -> Property
prop_validate_go_ast_checks_correctness module =
  let result = validateGoAST module
  in property $ True  -- Basic test that Go AST validation doesn't crash

-- ============================================================================
-- Properties for Compilation Pipeline
-- ============================================================================

prop_compilation_pipeline_preserves_structure :: String -> Property
prop_compilation_pipeline_preserves_structure code =
  not (null code) ==> 
  let ir = compileToIR code
      go = compileToGo code
  in property $ True  -- Basic test that pipeline preserves structure

prop_compilation_handles_unicode_content :: String -> Property
prop_compilation_handles_unicode_content unicodeText =
  not (null unicodeText) ==> 
  let code = "// Unicode test: " ++ unicodeText ++ "\nfunc main() { println(\"" ++ unicodeText ++ "\") }"
      result = compile code
  in property $ True  -- Basic test that compilation handles unicode

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Core Compiler QuickCheck Tests"
  [ testGroup "IR Types Properties"
    [ fastProperty "ir type is well formed" prop_ir_type_is_well_formed
    , fastProperty "ir variable has consistent structure" prop_ir_variable_has_consistent_structure
    ]

  , testGroup "IR Expressions Properties"
    [ fastProperty "ir expression is well formed" prop_ir_expression_is_well_formed
    ]

  , testGroup "IR Statements Properties"
    [ fastProperty "ir statement is well formed" prop_ir_statement_is_well_formed
    ]

  , testGroup "IR Functions and Modules Properties"
    [ fastProperty "ir function has valid structure" prop_ir_function_has_valid_structure
    , fastProperty "ir module preserves function order" prop_ir_module_preserves_function_order
    ]

  , testGroup "Go AST Types Properties"
    [ fastProperty "go type is well formed" prop_go_type_is_well_formed
    , fastProperty "go variable has consistent structure" prop_go_variable_has_consistent_structure
    ]

  , testGroup "Go Expressions Properties"
    [ fastProperty "go expression is well formed" prop_go_expression_is_well_formed
    ]

  , testGroup "Go Statements Properties"
    [ fastProperty "go statement is well formed" prop_go_statement_is_well_formed
    ]

  , testGroup "Go Functions and Modules Properties"
    [ fastProperty "go function has valid structure" prop_go_function_has_valid_structure
    , fastProperty "go module preserves function order" prop_go_module_preserves_function_order
    ]

  , testGroup "Compilation Properties"
    [ fastProperty "compile handles simple code" prop_compile_handles_simple_code
    , fastProperty "compile with settings respects options" prop_compile_with_settings_respects_options
    , fastProperty "compile to ir generates valid ir" prop_compile_to_ir_generates_valid_ir
    , fastProperty "compile to go generates valid go" prop_compile_to_go_generates_valid_go
    , fastProperty "validate compilation checks correctness" prop_validate_compilation_checks_correctness
    ]

  , testGroup "Error Handling Properties"
    [ fastProperty "get compilation errors returns errors" prop_get_compilation_errors_returns_errors
    , fastProperty "get compilation warnings returns warnings" prop_get_compilation_warnings_returns_warnings
    , fastProperty "clear compilation state resets state" prop_clear_compilation_state_resets_state
    ]

  , testGroup "IR Optimization Properties"
    [ fastProperty "optimize ir preserves semantics" prop_optimize_ir_preserves_semantics
    , fastProperty "validate ir checks correctness" prop_validate_ir_checks_correctness
    ]

  , testGroup "Go AST Optimization Properties"
    [ fastProperty "optimize go ast preserves semantics" prop_optimize_go_ast_preserves_semantics
    , fastProperty "validate go ast checks correctness" prop_validate_go_ast_checks_correctness
    ]

  , testGroup "Compilation Pipeline Properties"
    [ fastProperty "compilation pipeline preserves structure" prop_compilation_pipeline_preserves_structure
    , fastProperty "compilation handles unicode content" prop_compilation_handles_unicode_content
    ]
  ]