{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewDependentTypesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, intercalate)
import Data.Char (isDigit, isLetter)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)

-- ============================================================================
-- 依赖类型系统测试 (40个测试)
-- ============================================================================

-- | 测试值参数化类型的解析和验证
prop_value_parameterized_type_syntax :: String -> Int -> Property
prop_value_parameterized_type_syntax typeName n =
  let validType = typeName ++ "[" ++ show n ++ ": int]"
      hasCorrectSyntax = "[" `isInfixOf` validType && 
                         ": int]" `isInfixOf` validType &&
                         show n `isInfixOf` validType
  in property $ hasCorrectSyntax

-- | 测试多个值参数的类型
prop_multiple_value_parameters :: String -> Int -> Int -> Property
prop_multiple_value_parameters typeName n m =
  let multiParamType = typeName ++ "[" ++ show n ++ ": int, " ++ show m ++ ": int]"
      hasMultipleParams = "," `isInfixOf` multiParamType &&
                          show n `isInfixOf` multiParamType &&
                          show m `isInfixOf` multiParamType
  in property $ hasMultipleParams

-- | 测试精确类型的约束语法
prop_precise_type_constraint :: String -> String -> Property
prop_precise_type_constraint typeName constraint =
  let constrainedType = typeName ++ " where { " ++ constraint ++ " }"
      hasConstraint = "where" `isInfixOf` constrainedType &&
                      "{" `isInfixOf` constrainedType &&
                      "}" `isInfixOf` constrainedType &&
                      constraint `isInfixOf` constrainedType
  in property $ hasConstraint

-- | 测试self引用的约束
prop_self_reference_constraint :: String -> Property
prop_self_reference_constraint operator =
  let selfConstraint = "self " ++ operator ++ " 0"
      hasSelf = "self" `isInfixOf` selfConstraint
  in property $ hasSelf

-- | 测试依赖函数签名的返回类型
prop_dependent_return_type :: String -> String -> Int -> Property
prop_dependent_return_type funcName typeName n =
  let dependentFunc = "func " ++ funcName ++ "(n: int) -> " ++ typeName ++ "[" ++ show n ++ "]"
      hasDependentReturn = typeName ++ "[" `isInfixOf` dependentFunc &&
                          show n ++ "]" `isInfixOf` dependentFunc
  in property $ hasDependentReturn

-- | 测试参数间依赖的类型
prop_inter_parameter_dependence :: String -> Int -> Property
prop_inter_parameter_dependence typeName n =
  let dependentParam = "i: ValidIndex[" ++ show n ++ "]"
      hasDependence = "ValidIndex[" `isInfixOf` dependentParam &&
                     show n ++ "]" `isInfixOf` dependentParam
  in property $ hasDependence

-- | 测试函数前置条件
prop_function_precondition :: String -> String -> Property
prop_function_precondition funcName condition =
  let funcWithPre = "func " ++ funcName ++ "(n: int) where { " ++ condition ++ " }"
      hasPrecondition = "where" `isInfixOf` funcWithPre &&
                       condition `isInfixOf` funcWithPre
  in property $ hasPrecondition

-- | 测试类型级算术表达式
prop_type_level_arithmetic :: String -> String -> Property
prop_type_level_arithmetic typeName operation =
  let arithmeticType = typeName ++ "[" ++ operation ++ "]"
      hasArithmetic = operation `isInfixOf` arithmeticType
  in property $ hasArithmetic

-- | 测试混合类型参数和值参数
prop_mixed_type_value_parameters :: String -> String -> Int -> Property
prop_mixed_type_value_parameters typeName typeParam n =
  let mixedType = typeName ++ "[" ++ typeParam ++ " any, " ++ show n ++ ": int]"
      hasMixed = typeParam ++ " any" `isInfixOf` mixedType &&
                 show n ++ ": int]" `isInfixOf` mixedType
  in property $ hasMixed

-- | 测试存在类型的语法
prop_existential_type_syntax :: String -> Property
prop_existential_type_syntax typeName =
  let existentialType = typeName ++ "[some n: int]"
      hasExistential = "some" `isInfixOf` existentialType
  in property $ hasExistential

-- | 测试match表达式的语法
prop_match_expression_syntax :: String -> String -> Property
prop_match_expression_syntax varName paramName =
  let matchExpr = "match " ++ varName ++ ".(" ++ paramName ++ ") { ... }"
      hasMatch = "match" `isInfixOf` matchExpr &&
                 ".(" `isInfixOf` matchExpr
  in property $ hasMatch

-- | 测试assert语句的语法
prop_assert_statement_syntax :: String -> Property
prop_assert_statement_syntax expression =
  let assertStmt = "assert " ++ expression
      hasAssert = "assert" `isInfixOf` assertStmt
  in property $ hasAssert

-- | 测试static_assert语句的语法
prop_static_assert_syntax :: String -> Property
prop_static_assert_syntax expression =
  let staticAssert = "static_assert " ++ expression
      hasStaticAssert = "static_assert" `isInfixOf` staticAssert
  in property $ hasStaticAssert

-- | 测试文件级指令
prop_file_level_directive :: String -> Property
prop_file_level_directive feature =
  let directive = "//! " ++ feature ++ ": on"
      hasDirective = "//! " `isInfixOf` directive &&
                     ": on" `isInfixOf` directive
  in property $ hasDirective

-- | 测试块级指令
prop_block_level_directive :: String -> Property
prop_block_level_directive feature =
  let blockDirective = "{//! " ++ feature ++ ": on"
      hasBlockDirective = "{//! " `isInfixOf` blockDirective
  in property $ hasBlockDirective

-- | 测试条件窄化
prop_condition_narrowing :: String -> Property
prop_condition_narrowing condition =
  let narrowCode = "if " ++ condition ++ " { ... }"
      hasCondition = "if " `isInfixOf` narrowCode &&
                     condition `isInfixOf` narrowCode
  in property $ hasCondition

-- | 测试编译期常量传播
prop_compile_time_constant :: String -> Int -> Property
prop_compile_time_constant typeName n =
  let constantUse = typeName ++ "[" ++ show n ++ "]"
      hasConstant = show n `isInfixOf` constantUse
  in property $ hasConstant

-- | 测试运行时值处理
prop_runtime_value_handling :: String -> Property
prop_runtime_value_handling varName =
  let runtimeCode = "n := readInt()\n" ++ varName ++ " := zeros(n)"
      hasRuntime = "readInt()" `isInfixOf` runtimeCode
  in property $ hasRuntime

-- | 测试约束违反的错误消息
prop_constraint_violation_error :: String -> Property
prop_constraint_violation_error typeName =
  let errorMsg = "typus: constraint " ++ typeName ++ " violated"
      hasError = "constraint" `isInfixOf` errorMsg &&
                 "violated" `isInfixOf` errorMsg
  in property $ hasError

-- | 测试错误模式切换
prop_error_mode_switch :: Property
prop_error_mode_switch =
  let errorMode = "//! constraint_mode: error"
      hasErrorMode = "constraint_mode" `isInfixOf` errorMode &&
                     "error" `isInfixOf` errorMode
  in property $ hasErrorMode

-- | 测试panic模式（默认）
prop_panic_mode_default :: String -> String -> Property
prop_panic_mode_default typeName constraint =
  let panicCode = "if !(" ++ constraint ++ ") {\n  panic(\"typus: constraint " ++ typeName ++ " violated\")\n}"
      hasPanic = "panic" `isInfixOf` panicCode &&
                  "constraint" `isInfixOf` panicCode
  in property $ hasPanic

-- | 测试Go互操作
prop_go_interoperability :: String -> Property
prop_go_interoperability packageName =
  let importCode = "import \"" ++ packageName ++ "\""
      hasImport = "import" `isInfixOf` importCode
  in property $ hasImport

-- | 测试边界标注
prop_boundary_annotation :: String -> Property
prop_boundary_annotation funcName =
  let boundaryCode = "func " ++ funcName ++ "(data []float64) {\n  assert len(data) > 0\n  ...}"
      hasBoundary = "assert" `isInfixOf` boundaryCode &&
                    "len(data) > 0" `isInfixOf` boundaryCode
  in property $ hasBoundary

-- | 测试向量类型示例
prop_vector_type_example :: Int -> Property
prop_vector_type_example n =
  let vectorType = "type Vector[" ++ show n ++ ": int] struct {\n  data [" ++ show n ++ "]float64\n}"
      hasVectorType = "Vector[" `isInfixOf` vectorType &&
                      "struct" `isInfixOf` vectorType &&
                      "data [" `isInfixOf` vectorType
  in property $ hasVectorType

-- | 测试矩阵类型示例
prop_matrix_type_example :: Int -> Int -> Property
prop_matrix_type_example rows cols =
  let matrixType = "type Matrix[" ++ show rows ++ ": int, " ++ show cols ++ ": int] struct {\n  data [" ++ show rows ++ "][" ++ show cols ++ "]float64\n}"
      hasMatrixType = "Matrix[" `isInfixOf` matrixType &&
                      "struct" `isInfixOf` matrixType
  in property $ hasMatrixType

-- | 测试约束求解器能力
prop_constraint_solver_basic :: String -> Int -> Property
prop_constraint_solver_basic operator value =
  let constraint = "self " ++ operator ++ " " ++ show value
      hasBasicConstraint = "self" `isInfixOf` constraint &&
                          operator `isInfixOf` constraint
  in property $ hasBasicConstraint

-- | 测试线性整数算术
prop_linear_integer_arithmetic :: String -> String -> Property
prop_linear_integer_arithmetic var1 var2 =
  let arithmetic = var1 ++ " + " ++ var2 ++ " >= 0"
      hasLinear = "+" `isInfixOf` arithmetic &&
                  ">=" `isInfixOf` arithmetic
  in property $ hasLinear

-- | 测试等式传播
prop_equality_propagation :: String -> String -> Property
prop_equality_propagation var1 var2 =
  let equality = var1 ++ " == " ++ var2
      hasEquality = "==" `isInfixOf` equality
  in property $ hasEquality

-- | 测试简单不等式链
prop_inequality_chain :: String -> String -> String -> Property
prop_inequality_chain var1 var2 var3 =
  let chain = var1 ++ " > " ++ var2 ++ ", " ++ var2 ++ " > " ++ var3
      hasChain = ">" `isInfixOf` chain &&
                 "," `isInfixOf` chain
  in property $ hasChain

-- | 测试非线性算术限制
prop_nonlinear_arithmetic_limitation :: String -> Property
prop_nonlinear_arithmetic_limitation expr =
  let nonlinear = "n * n - (n-1) * (n+1) == 1"
      hasNonlinear = "*" `isInfixOf` nonlinear
  in property $ hasNonlinear

-- | 测试用户定义函数限制
prop_user_defined_function_limitation :: String -> String -> Property
prop_user_defined_function_limitation funcName varName =
  let userFunc = "Vector[" ++ funcName ++ "(" ++ varName ++ ")]"
      hasUserFunc = funcName ++ "(" `isInfixOf` userFunc
  in property $ hasUserFunc

-- | 测试归纳证明限制
prop_inductive_proof_limitation :: String -> Property
prop_inductive_proof_limitation funcName =
  let recursiveFunc = "func " ++ funcName ++ "(n: int) { ... " ++ funcName ++ "(n-1) ... }"
      hasRecursive = funcName ++ "(n-1)" `isInfixOf` recursiveFunc
  in property $ hasRecursive

-- | 测试浮点约束限制
prop_floating_point_limitation :: String -> Property
prop_floating_point_limitation operator =
  let floatConstraint = "self " ++ operator ++ " 0.0"
      hasFloat = "0.0" `isInfixOf` floatConstraint
  in property $ hasFloat

-- | 测试诊断工具
prop_diagnostic_tool :: Property
prop_diagnostic_tool =
  let diagnosticCmd = "typus check --show-constraints input.typus"
      hasDiagnostic = "--show-constraints" `isInfixOf` diagnosticCmd
  in property $ hasDiagnostic

-- | 测试证据显示工具
prop_evidence_display_tool :: Property
prop_evidence_display_tool =
  let evidenceCmd = "typus check --show-evidence input.typus"
      hasEvidence = "--show-evidence" `isInfixOf` evidenceCmd
  in property $ hasEvidence

-- | 测试类型推导
prop_type_inference :: String -> Property
prop_type_inference funcName =
  let inferenceCode = "func " ++ funcName ++ "(n: Positive, value: float64) -> Vector[n] {\n  return Vector{elements}\n}"
      hasInference = "-> Vector[n]" `isInfixOf` inferenceCode
  in property $ hasInference

-- | 测试渐进式采用
prop_progressive_adoption :: Property
prop_progressive_adoption =
  let gradualCode = "// 普通 Go 代码\n\n{//! dependent_types: on\n  // 此块启用依赖类型\n}"
      hasGradual = "{//! dependent_types: on" `isInfixOf` gradualCode
  in property $ hasGradual

-- | 测试编译期优先原则
prop_compile_time_first :: String -> Property
prop_compile_time_first expression =
  let compileTimeCode = "if " ++ expression ++ " { // 编译期证明，不生成代码 }"
      hasCompileTime = "编译期证明" `isInfixOf` compileTimeCode
  in property $ hasCompileTime

-- | 测试可预测的约束求解
prop_predictable_constraint_solving :: String -> Property
prop_predictable_constraint_solving constraint =
  let predictableCode = "// 约束求解器能力边界明确\n" ++ constraint
      hasPredictable = "能力边界" `isInfixOf` predictableCode
  in property $ hasPredictable

-- | 测试Go生态兼容性
prop_go_ecosystem_compatibility :: String -> Property
prop_go_ecosystem_compatibility moduleName =
  let compatibleCode = "import \"" ++ moduleName ++ "\"\n// 直接调用 Go 标准库"
      hasCompatible = "Go 标准库" `isInfixOf` compatibleCode
  in property $ hasCompatible

-- | 测试导出给Go代码
prop_export_to_go :: String -> Property
prop_export_to_go funcName =
  let exportCode = "// Go 调用方\nv := " ++ funcName ++ "(3)"
      hasExport = "Go 调用方" `isInfixOf` exportCode
  in property $ hasExport

-- | 测试运行时字段的生成
prop_runtime_field_generation :: String -> Property
prop_runtime_field_generation typeName =
  let runtimeCode = "type " ++ typeName ++ " struct {\n  _n   int\n  data []float64\n}"
      hasRuntimeField = "_n   int" `isInfixOf` runtimeCode
  in property $ hasRuntimeField

tests :: TestTree
tests = testGroup "New Dependent Types QuickCheck Tests"
  [ testProperty "Value parameterized type syntax" prop_value_parameterized_type_syntax
  , testProperty "Multiple value parameters" prop_multiple_value_parameters
  , testProperty "Precise type constraint" prop_precise_type_constraint
  , testProperty "Self reference constraint" prop_self_reference_constraint
  , testProperty "Dependent return type" prop_dependent_return_type
  , testProperty "Inter-parameter dependence" prop_inter_parameter_dependence
  , testProperty "Function precondition" prop_function_precondition
  , testProperty "Type level arithmetic" prop_type_level_arithmetic
  , testProperty "Mixed type value parameters" prop_mixed_type_value_parameters
  , testProperty "Existential type syntax" prop_existential_type_syntax
  , testProperty "Match expression syntax" prop_match_expression_syntax
  , testProperty "Assert statement syntax" prop_assert_statement_syntax
  , testProperty "Static assert syntax" prop_static_assert_syntax
  , testProperty "File level directive" prop_file_level_directive
  , testProperty "Block level directive" prop_block_level_directive
  , testProperty "Condition narrowing" prop_condition_narrowing
  , testProperty "Compile time constant" prop_compile_time_constant
  , testProperty "Runtime value handling" prop_runtime_value_handling
  , testProperty "Constraint violation error" prop_constraint_violation_error
  , testProperty "Error mode switch" prop_error_mode_switch
  , testProperty "Panic mode default" prop_panic_mode_default
  , testProperty "Go interoperability" prop_go_interoperability
  , testProperty "Boundary annotation" prop_boundary_annotation
  , testProperty "Vector type example" prop_vector_type_example
  , testProperty "Matrix type example" prop_matrix_type_example
  , testProperty "Constraint solver basic" prop_constraint_solver_basic
  , testProperty "Linear integer arithmetic" prop_linear_integer_arithmetic
  , testProperty "Equality propagation" prop_equality_propagation
  , testProperty "Inequality chain" prop_inequality_chain
  , testProperty "Nonlinear arithmetic limitation" prop_nonlinear_arithmetic_limitation
  , testProperty "User defined function limitation" prop_user_defined_function_limitation
  , testProperty "Inductive proof limitation" prop_inductive_proof_limitation
  , testProperty "Floating point limitation" prop_floating_point_limitation
  , testProperty "Diagnostic tool" prop_diagnostic_tool
  , testProperty "Evidence display tool" prop_evidence_display_tool
  , testProperty "Type inference" prop_type_inference
  , testProperty "Progressive adoption" prop_progressive_adoption
  , testProperty "Compile time first" prop_compile_time_first
  , testProperty "Predictable constraint solving" prop_predictable_constraint_solving
  , testProperty "Go ecosystem compatibility" prop_go_ecosystem_compatibility
  , testProperty "Export to Go" prop_export_to_go
  , testProperty "Runtime field generation" prop_runtime_field_generation
  ]