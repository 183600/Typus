{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.TypusIntegrationTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )
import TestSupport.EnhancedMemoryOptimization 
  ( enhancedMemoryCleanup
  , strategicMemoryCleanup
  , cleanupBetweenTests
  , withEnhancedMemoryControl
  , withStrictMemoryLimits
  , applyMemoryOptimizations
  )
import TestSupport.OptimizedStringOperations 
  ( genMinimalString
  , genUltraMinimalString
  , safeTake
  , safeLength
  , efficientTrim
  , efficientIsEmpty
  , withUltraStringLimit
  , minimizeStringUsage
  , optimizeStringProperty
  )
import TestSupport.TestPropertyMemoryCleanup 
  ( testGroupWithCleanup
  , testGroupWithStrategicCleanup
  , memoryAwareProperty
  , memoryOptimizedProperty
  , withPropertyMemoryCleanup
  )

import Parser (parseTypus)
import DependentTypesParser
import Ownership
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort, group, nub, intersperse, partition)
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum, toUpper, toLower, ord, chr)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when, unless)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- ============================================================================
-- 1. 端到端编译测试 (End-to-End Compilation Tests)
-- ============================================================================

-- | 测试完整程序编译
prop_complete_program_compilation :: String -> Property
prop_complete_program_compilation programName =
  let validProgramName = not (null programName) && all isAlphaNum programName
      completeProgramExpr = "package main\n\nimport \"fmt\"\n\nfunc " ++ programName ++ "() {\n  fmt.Println(\"Hello, World!\")\n}\n\nfunc main() {\n  " ++ programName ++ "()\n}"
      parseResult = parseTypus completeProgramExpr
  in classify validProgramName "valid program name" $
     if validProgramName
        then property $ isRight parseResult
        else property True

-- | 测试多文件项目结构
prop_multi_file_project_structure :: String -> Property
prop_multi_file_project_structure moduleName =
  let validModuleName = not (null moduleName) && all isAlphaNum moduleName
      multiFileExpr = "package " ++ moduleName ++ "\n\nimport \"fmt\"\n\ntype Data struct { Value int }\n\nfunc Process(d Data) Data {\n  d.Value++\n  return d\n}"
      parseResult = parseTypus multiFileExpr
  in classify validModuleName "valid module name" $
     if validModuleName
        then property $ isRight parseResult
        else property True

-- | 测试库包编译
prop_library_package_compilation :: String -> Property
prop_library_package_compilation packageName =
  let validPackageName = not (null packageName) && all isAlphaNum packageName
      libraryExpr = "package " ++ packageName ++ "\n\n// Exported function\nfunc Exported() int {\n  return 42\n}\n\n// Internal function\nfunc internal() string {\n  return \"internal\"\n}"
      parseResult = parseTypus libraryExpr
  in classify validPackageName "valid package name" $
     if validPackageName
        then property $ isRight parseResult
        else property True

-- | 测试主程序与库集成
prop_main_library_integration :: String -> String -> Property
prop_main_library_integration mainName libName =
  let validNames = not (null mainName) && not (null libName) && 
                  all isAlphaNum mainName && all isAlphaNum libName
      integrationExpr = "package main\n\nimport \"./" ++ libName ++ "\"\n\nfunc " ++ mainName ++ "() {\n  result := " ++ libName ++ ".Exported()\n  println(result)\n}\n\nfunc main() {\n  " ++ mainName ++ "()\n}"
      parseResult = parseTypus integrationExpr
  in classify validNames "valid names" $
     if validNames
        then property $ isRight parseResult
        else property True

-- | 测试外部依赖集成
prop_external_dependency_integration :: String -> Property
prop_external_dependency_integration dependencyName =
  let validDependencyName = not (null dependencyName) && length dependencyName <= 15
      externalDepExpr = "package main\n\nimport \"" ++ dependencyName ++ "\"\n\nfunc main() {\n  // Use external dependency\n  " ++ dependencyName ++ ".Function()\n}"
      parseResult = parseTypus externalDepExpr
  in classify validDependencyName "valid dependency name" $
     if validDependencyName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 2. 依赖类型与所有权集成测试 (Dependent Types and Ownership Integration)
-- ============================================================================

-- | 测试依赖类型中的所有权转移
prop_ownership_transfer_in_dependent_types :: String -> Int -> Property
prop_ownership_transfer_in_dependent_types typeName size =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      validSize = size > 0 && size <= 10
      transferExpr = "{//! ownership: on\n//! dependent_types: on\ntype " ++ typeName ++ "[" ++ show size ++ "] struct { data [" ++ show size ++ "]int }\n\nfunc process(v: " ++ typeName ++ "[" ++ show size ++ "]) {\n  v2 := v  // Ownership transfer\n  // v is no longer accessible\n}"
      parseResult = parseTypus transferExpr
  in classify validTypeName "valid type name" $
     classify validSize "valid size" $
     if validTypeName && validSize
        then property $ isRight parseResult
        else property True

-- | 测试借用与类型约束交互
prop_borrowing_with_type_constraints :: String -> Property
prop_borrowing_with_type_constraints typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      borrowingExpr = "{//! ownership: on\n//! dependent_types: on\ntype Positive = int where { self > 0 }\ntype " ++ typeName ++ " struct { value Positive }\n\nfunc process(x: " ++ typeName ++ ") {\n  r := &x  // Immutable borrow\n  // Can still read x.value\n}"
      parseResult = parseTypus borrowingExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试可变借用与约束验证
prop_mutable_borrowing_with_constraint_validation :: String -> Property
prop_mutable_borrowing_with_constraint_validation typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      mutableBorrowExpr = "{//! ownership: on\n//! dependent_types: on\ntype Bounded = int where { self >= 0 && self <= 100 }\ntype " ++ typeName ++ " struct { value Bounded }\n\nfunc process(x: " ++ typeName ++ ") {\n  m := &mut x  // Mutable borrow\n  // Can modify x.value but must maintain constraints\n}"
      parseResult = parseTypus mutableBorrowExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试生命周期与依赖类型
prop_lifetimes_with_dependent_types :: String -> Int -> Property
prop_lifetimes_with_dependent_types typeName size =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      validSize = size > 0 && size <= 10
      lifetimeExpr = "{//! ownership: on\n//! dependent_types: on\ntype Vector[" ++ show size ++ "] struct { data [" ++ show size ++ "]int }\n\nfunc process() -> &Vector[" ++ show size ++ "] {\n  v := Vector[" ++ show size ++ "]{data: [" ++ show size ++ "]int{}}\n  return &v  // Returning reference\n}"
      parseResult = parseTypus lifetimeExpr
  in classify validTypeName "valid type name" $
     classify validSize "valid size" $
     if validTypeName && validSize
        then property $ isRight parseResult
        else property True

-- | 测试所有权转移与存在类型
prop_ownership_transfer_with_existential_types :: String -> Property
prop_ownership_transfer_with_existential_types typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      existentialExpr = "{//! ownership: on\n//! dependent_types: on\ntype " ++ typeName ++ "[some n: int] struct { data [n]int }\n\nfunc process(v: " ++ typeName ++ "[some n]) {\n  v2 := v  // Ownership transfer\n  match v2.(len) {\n    // Use len within this scope\n  }\n}"
      parseResult = parseTypus existentialExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 3. 错误处理与类型系统集成测试 (Error Handling and Type System Integration)
-- ============================================================================

-- | 测试约束违反错误处理
prop_constraint_violation_error_handling :: String -> Property
prop_constraint_violation_error_handling typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      errorHandlingExpr = "//! dependent_types: on\n//! constraint_mode: error\n\ntype Positive = int where { self > 0 }\n\ntype " ++ typeName ++ " struct { value Positive }\n\nfunc create" ++ typeName ++ "(x: int) (" ++ typeName ++ ", error) {\n  if x <= 0 {\n    return " ++ typeName ++ "{}, errors.New(\"value must be positive\")\n  }\n  return " ++ typeName ++ "{value: x}, nil\n}"
      parseResult = parseTypus errorHandlingExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试所有权错误与类型约束
prop_ownership_errors_with_type_constraints :: String -> Property
prop_ownership_errors_with_type_constraints typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      errorExpr = "{//! ownership: on\n//! dependent_types: on\ntype NonZero = int where { self != 0 }\ntype " ++ typeName ++ " struct { value NonZero }\n\nfunc process(x: " ++ typeName ++ ") {\n  x2 := x  // Move\n  // x is no longer accessible\n  // Using x here would be an ownership error\n}"
      parseResult = parseTypus errorExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试错误恢复与类型推导
prop_error_recovery_with_type_inference :: String -> Property
prop_error_recovery_with_type_inference funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      recoveryExpr = "//! dependent_types: on\n\nfunc " ++ funcName ++ "(x: int) {\n  if x <= 0 {\n    return  // Early return\n  }\n  // x is known to be positive here\n  v := zeros(x)  // Type inference works\n}"
      parseResult = parseTypus recoveryExpr
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight parseResult
        else property True

-- | 测试错误传播与所有权
prop_error_propagation_with_ownership :: String -> Property
prop_error_propagation_with_ownership funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      propagationExpr = "{//! ownership: on\n\nfunc " ++ funcName ++ "() (Data, error) {\n  data, err := createData()\n  if err != nil {\n    return Data{}, err  // Error propagation\n  }\n  return process(data), nil  // Ownership transfer\n}"
      parseResult = parseTypus propagationExpr
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight parseResult
        else property True

-- | 测试错误上下文与依赖类型
prop_error_context_with_dependent_types :: String -> Property
prop_error_context_with_dependent_types typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      contextExpr = "//! dependent_types: on\n\ntype Bounded[min: int, max: int] = int where { self >= min && self <= max }\n\ntype " ++ typeName ++ "Error[min: int, max: int] struct {\n  value int\n  min min\n  max max\n}\n\nfunc (e " ++ typeName ++ "Error[min, max]) Error() string {\n  return fmt.Sprintf(\"value %d is outside bounds [%d, %d]\", e.value, e.min, e.max)\n}"
      parseResult = parseTypus contextExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 4. 性能优化与类型系统测试 (Performance Optimization and Type System)
-- ============================================================================

-- | 测试零成本抽象
prop_zero_cost_abstractions :: String -> Property
prop_zero_cost_abstractions typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      zeroCostExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ "[n: int] struct { data [n]int }\n\nfunc process" ++ typeName ++ "[n: int](x: " ++ typeName ++ "[n]) int {\n  sum := 0\n  for i := 0; i < n; i++ {\n    sum += x.data[i]\n  }\n  return sum\n}"
      parseResult = parseTypus zeroCostExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试编译时优化
prop_compile_time_optimization :: String -> Property
prop_compile_time_optimization funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      optimizationExpr = "//! dependent_types: on\n\nfunc " ++ funcName ++ "[n: int]() -> [n]int {\n  // Compile-time optimization: unroll loop for small n\n  if n == 2 {\n    return [2]int{0, 1}\n  }\n  // General case\n  result := [n]int{}\n  for i := 0; i < n; i++ {\n    result[i] = i\n  }\n  return result\n}"
      parseResult = parseTypus optimizationExpr
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight parseResult
        else property True

-- | 测试内联优化与类型系统
prop_inlining_optimization_with_type_system :: String -> Property
prop_inlining_optimization_with_type_system funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      inliningExpr = "//! dependent_types: on\n\n// This function should be inlined\nfunc " ++ funcName ++ "[n: int](x: int) -> int where { x >= 0 } {\n  return x * 2\n}\n\nfunc process[n: int](x: int) -> int {\n  return " ++ funcName ++ "[n](x)  // Should be inlined\n}"
      parseResult = parseTypus inliningExpr
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight parseResult
        else property True

-- | 测试内存布局优化
prop_memory_layout_optimization :: String -> Property
prop_memory_layout_optimization typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      layoutExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ "[n: int] struct {\n  // Optimized memory layout\n  smallFields [8]byte  // Group small fields together\n  largeFields [n]int  // Then larger fields\n  // No padding between fields of the same size\n}"
      parseResult = parseTypus layoutExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试所有权优化
prop_ownership_optimization :: String -> Property
prop_ownership_optimization funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      optimizationExpr = "{//! ownership: on\n\nfunc " ++ funcName ++ "(x: Data) Data {\n  // Ownership optimization: move instead of copy\n  processData(x)  // x is moved here\n  return Data{}    // Return new data\n}"
      parseResult = parseTypus optimizationExpr
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 5. 并发安全与类型系统测试 (Concurrency Safety and Type System)
-- ============================================================================

-- | 测试并发安全的数据结构
prop_concurrent_safe_data_structures :: String -> Property
prop_concurrent_safe_data_structures typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      concurrentExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ "[n: int] struct {\n  data [n]int\n  mu sync.Mutex  // Protects access to data\n}\n\nfunc (c *" ++ typeName ++ "[n]) Get(i: ValidIndex[n]) int {\n  c.mu.Lock()\n  defer c.mu.Unlock()\n  return c.data[i]\n}"
      parseResult = parseTypus concurrentExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试所有权与goroutine
prop_ownership_with_goroutines :: String -> Property
prop_ownership_with_goroutines varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      goroutineExpr = "{//! ownership: on\n\nfunc process() {\n  " ++ varName ++ " := newData()\n  go func() {\n    // " ++ varName ++ " is moved into this goroutine\n    processData(" ++ varName ++ ")\n  }()\n  // " ++ varName ++ " is no longer accessible here\n}"
      parseResult = parseTypus goroutineExpr
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- | 测试channel类型安全
prop_channel_type_safety :: String -> Property
prop_channel_type_safety typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      channelExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ " = int where { self > 0 }\n\nfunc process() {\n  ch := make(chan " ++ typeName ++ ")\n  go func() { ch <- 42 }()  // Type-safe channel communication\n  value := <-ch\n  // value is known to be Positive\n}"
      parseResult = parseTypus channelExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试并发与依赖类型
prop_concurrency_with_dependent_types :: String -> Int -> Property
prop_concurrency_with_dependent_types typeName workerCount =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      validWorkerCount = workerCount > 0 && workerCount <= 10
      concurrentExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ "[n: int] struct { data [n]int }\n\nfunc process[n: int](x: " ++ typeName ++ "[n]) {\n  ch := make(chan int, " ++ show workerCount ++ ")\n  \n  // Start " ++ show workerCount ++ " workers\n  for i := 0; i < " ++ show workerCount ++ "; i++ {\n    go func(worker int) {\n      // Each worker processes a portion of the data\n      start := worker * (n / " ++ show workerCount ++ ")\n      end := (worker + 1) * (n / " ++ show workerCount ++ ")\n      sum := 0\n      for j := start; j < end; j++ {\n        sum += x.data[j]\n      }\n      ch <- sum\n    }(i)\n  }\n  \n  // Collect results\n  total := 0\n  for i := 0; i < " ++ show workerCount ++ "; i++ {\n    total += <-ch\n  }\n}"
      parseResult = parseTypus concurrentExpr
  in classify validTypeName "valid type name" $
     classify validWorkerCount "valid worker count" $
     if validTypeName && validWorkerCount
        then property $ isRight parseResult
        else property True

-- | 测试原子操作与类型约束
prop_atomic_operations_with_type_constraints :: String -> Property
prop_atomic_operations_with_type_constraints typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      atomicExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ " = int64 where { self >= 0 }\n\ntype Counter struct {\n  value " ++ typeName ++ "\n}\n\nfunc (c *Counter) Increment() {\n  // Atomic operation maintains constraint\n  atomic.AddInt64((*int64)(&c.value), 1)\n}"
      parseResult = parseTypus atomicExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 6. 模块化与类型系统测试 (Modularity and Type System)
-- ============================================================================

-- | 测试模块边界与类型导出
prop_module_boundaries_with_type_export :: String -> String -> Property
prop_module_boundaries_with_type_export moduleName typeName =
  let validNames = not (null moduleName) && not (null typeName) && 
                  all isAlphaNum moduleName && all isAlphaNum typeName
      moduleExpr = "package " ++ moduleName ++ "\n\n// Exported type\ntype " ++ typeName ++ " struct {\n  Value int  // Exported field\n  secret int  // Internal field\n}\n\n// Exported method\nfunc (t " ++ typeName ++ ") GetValue() int {\n  return t.Value\n}\n\n// Internal method\nfunc (t " ++ typeName ++ ") getSecret() int {\n  return t.secret\n}"
      parseResult = parseTypus moduleExpr
  in classify validNames "valid names" $
     if validNames
        then property $ isRight parseResult
        else property True

-- | 测试跨模块类型约束
prop_cross_module_type_constraints :: String -> String -> Property
prop_cross_module_type_constraints moduleName typeName =
  let validNames = not (null moduleName) && not (null typeName) && 
                  all isAlphaNum moduleName && all isAlphaNum typeName
      crossModuleExpr = "package " ++ moduleName ++ "\n\n//! dependent_types: on\n\ntype " ++ typeName ++ " = int where { self > 0 }\n\n// Exported function with constraint\nfunc Create" ++ typeName ++ "(x: int) (" ++ typeName ++ ", error) {\n  if x <= 0 {\n    return 0, errors.New(\"value must be positive\")\n  }\n  return " ++ typeName ++ "(x), nil\n}"
      parseResult = parseTypus crossModuleExpr
  in classify validNames "valid names" $
     if validNames
        then property $ isRight parseResult
        else property True

-- | 测试模块化所有权
prop_modular_ownership :: String -> String -> Property
prop_modular_ownership moduleName moduleName2 =
  let validNames = not (null moduleName) && not (null moduleName2) && 
                  all isAlphaNum moduleName && all isAlphaNum moduleName2
      modularExpr = "package " ++ moduleName ++ "\n\n//! ownership: on\n\ntype Data struct { value int }\n\n// Ownership transfer across module boundary\nfunc TransferData() Data {\n  return Data{value: 42}\n}\n\n// Borrowing across module boundary\nfunc BorrowData(d: &Data) int {\n  return d.value\n}"
      parseResult = parseTypus modularExpr
  in classify validNames "valid names" $
     if validNames
        then property $ isRight parseResult
        else property True

-- | 测试模块化错误处理
prop_modular_error_handling :: String -> Property
prop_modular_error_handling moduleName =
  let validModuleName = not (null moduleName) && all isAlphaNum moduleName
      errorHandlingExpr = "package " ++ moduleName ++ "\n\n//! dependent_types: on\n\ntype Error[T any] struct {\n  data T\n  message string\n}\n\nfunc (e Error[T]) Error() string {\n  return e.message\n}\n\nfunc NewError[T any](data T, message string) Error[T] {\n  return Error[T]{data: data, message: message}\n}"
      parseResult = parseTypus errorHandlingExpr
  in classify validModuleName "valid module name" $
     if validModuleName
        then property $ isRight parseResult
        else property True

-- | 测试模块化并发
prop_modular_concurrency :: String -> Property
prop_modular_concurrency moduleName =
  let validModuleName = not (null moduleName) && all isAlphaNum moduleName
      concurrencyExpr = "package " ++ moduleName ++ "\n\n//! ownership: on\n\ntype SafeData struct {\n  data int\n  mu sync.Mutex\n}\n\nfunc NewSafeData(value int) *SafeData {\n  return &SafeData{data: value}\n}\n\nfunc (s *SafeData) Update(newValue int) {\n  s.mu.Lock()\n  defer s.mu.Unlock()\n  s.data = newValue\n}"
      parseResult = parseTypus concurrencyExpr
  in classify validModuleName "valid module name" $
     if validModuleName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 7. 元编程与类型系统集成测试 (Metaprogramming and Type System Integration)
-- ============================================================================

-- | 测试宏与依赖类型
prop_macros_with_dependent_types :: String -> Property
prop_macros_with_dependent_types typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      macroExpr = "//! dependent_types: on\n\n// Macro to generate vector operations\n//go:generate vectormac " ++ typeName ++ "\n\ntype " ++ typeName ++ "[n: int] struct { data [n]int }\n\n// Generated functions:\n// func (v " ++ typeName ++ "[n]) Add(other " ++ typeName ++ "[n]) " ++ typeName ++ "[n]\n// func (v " ++ typeName ++ "[n]) Scale(factor int) " ++ typeName ++ "[n]"
      parseResult = parseTypus macroExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试反射与类型约束
prop_reflection_with_type_constraints :: String -> Property
prop_reflection_with_type_constraints typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      reflectionExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ " = int where { self > 0 }\n\nfunc Validate" ++ typeName ++ "(x interface{}) bool {\n  v := reflect.ValueOf(x)\n  if v.Kind() != reflect.Int {\n    return false\n  }\n  return v.Int() > 0  // Runtime constraint check\n}"
      parseResult = parseTypus reflectionExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试代码生成与所有权
prop_code_generation_with_ownership :: String -> Property
prop_code_generation_with_ownership typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      codeGenExpr = "{//! ownership: on\n\n//go:generate ownershipgen " ++ typeName ++ "\n\ntype " ++ typeName ++ " struct { value int }\n\n// Generated functions:\n// func New" ++ typeName ++ "(value int) " ++ typeName ++ "\n// func (t " ++ typeName ++ ") Clone() " ++ typeName ++ "\n// func (t * " ++ typeName ++ ") Update(value int)"
      parseResult = parseTypus codeGenExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试动态类型创建与约束
prop_dynamic_type_creation_with_constraints :: String -> Property
prop_dynamic_type_creation_with_constraints typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      dynamicTypeExpr = "//! dependent_types: on\n\nfunc CreateBoundedType(min int, max int) reflect.Type {\n  // Create a type with dynamic bounds\n  fields := []reflect.StructField{\n    {\n      Name: \"value\",\n      Type: reflect.TypeOf(0),\n      Tag: reflect.StructTag(\"bound:\\\"" ++ "+ strconv.Itoa(min) + \"," ++ "+ strconv.Itoa(max) + \\\"\"),\n    },\n  }\n  return reflect.StructOf(fields)\n}\n\ntype " ++ typeName ++ " struct {\n  value int\n  min int\n  max int\n}"
      parseResult = parseTypus dynamicTypeExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试插件系统与类型安全
prop_plugin_system_with_type_safety :: String -> Property
prop_plugin_system_with_type_safety pluginName =
  let validPluginName = not (null pluginName) && all isAlphaNum pluginName
      pluginExpr = "//! dependent_types: on\n\ntype Plugin[T any] interface {\n  Process(input T) T\n  Name() string\n}\n\ntype " ++ pluginName ++ "Plugin[T any] struct {\n  name string\n  processFunc func(T) T\n}\n\nfunc (p " ++ pluginName ++ "Plugin[T]) Process(input T) T {\n  return p.processFunc(input)\n}\n\nfunc (p " ++ pluginName ++ "Plugin[T]) Name() string {\n  return p.name\n}"
      parseResult = parseTypus pluginExpr
  in classify validPluginName "valid plugin name" $
     if validPluginName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 8. 安全性与类型系统测试 (Security and Type System)
-- ============================================================================

-- | 测试内存安全与类型约束
prop_memory_safety_with_type_constraints :: String -> Property
prop_memory_safety_with_type_constraints typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      memorySafetyExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ "[n: int] struct { data [n]int }\n\nfunc (v " ++ typeName ++ "[n]) SafeGet(i: ValidIndex[n]) int {\n  // Type system ensures bounds safety\n  return v.data[i]\n}\n\nfunc Process" ++ typeName ++ "[n: int](v: " ++ typeName ++ "[n]) int {\n  // No bounds checking needed due to type system\n  sum := 0\n  for i := 0; i < n; i++ {\n    sum += v.SafeGet(i)\n  }\n  return sum\n}"
      parseResult = parseTypus memorySafetyExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试并发安全与所有权
prop_concurrency_safety_with_ownership :: String -> Property
prop_concurrency_safety_with_ownership typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      concurrencyExpr = "{//! ownership: on\n\ntype " ++ typeName ++ " struct { data int }\n\nfunc Process(data: " ++ typeName ++ ") {\n  // data is moved into this goroutine\n  go func() {\n    // Safe: only this goroutine has access to data\n    processData(data)\n  }()\n  // data is no longer accessible here, preventing data races\n}"
      parseResult = parseTypus concurrencyExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试信息流安全与类型系统
prop_information_flow_security_with_type_system :: String -> Property
prop_information_flow_security_with_type_system typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      infoFlowExpr = "//! dependent_types: on\n\ntype Public struct { value int }\ntype Private struct { secret int }\n\ntype " ++ typeName ++ " struct {\n  public Public\n  private Private\n}\n\nfunc (t " ++ typeName ++ ") GetPublic() Public {\n  return t.public  // Safe: public data flow\n}\n\nfunc (t " ++ typeName ++ ") GetPrivate() Private {\n  return t.private  // Safe: private data flow\n}"
      parseResult = parseTypus infoFlowExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试类型擦除与安全性
prop_type_erasure_with_security :: String -> Property
prop_type_erasure_with_security typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      typeErasureExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ " = int where { self > 0 }\n\nfunc Process" ++ typeName ++ "(x " ++ typeName ++ ") int {\n  // Type constraint is checked at compile time\n  // At runtime, x is just an int, but we know it's positive\n  return x * 2  // Safe: x is guaranteed to be positive\n}"
      parseResult = parseTypus typeErasureExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试安全审计与类型系统
prop_security_audit_with_type_system :: String -> Property
prop_security_audit_with_type_system typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      auditExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ " = string where { len(self) <= 100 && !containsMalicious(self) }\n\nfunc Process" ++ typeName ++ "(input " ++ typeName ++ ") string {\n  // Type system ensures input is safe\n  return \"Processed: \" + input\n}\n\nfunc containsMalicious(s string) bool {\n  // Check for common injection patterns\n  malicious := []string{\"<script>\", \"javascript:\", \"data:\"}\n  for _, m := range malicious {\n    if strings.Contains(strings.ToLower(s), m) {\n      return true\n    }\n  }\n  return false\n}"
      parseResult = parseTypus auditExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 9. 可维护性与类型系统测试 (Maintainability and Type System)
-- ============================================================================

-- | 测试代码文档与类型约束
prop_code_documentation_with_type_constraints :: String -> Property
prop_code_documentation_with_type_constraints typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      documentationExpr = "//! dependent_types: on\n\n// " ++ typeName ++ " represents a positive integer value\n// \n// The constraint self > 0 ensures that the value is always positive,\n// preventing division by zero and other mathematical errors.\ntype " ++ typeName ++ " = int where { self > 0 }\n\n// SafeDiv divides two numbers, ensuring the divisor is positive\n// \n// Parameters:\n//   - a: dividend\n//   - b: divisor (guaranteed to be positive by the type system)\n// \n// Returns:\n//   - The result of a / b\nfunc SafeDiv(a int, b " ++ typeName ++ ") int {\n  return a / b  // Safe: b is guaranteed to be non-zero\n}"
      parseResult = parseTypus documentationExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试重构与类型系统
prop_refactoring_with_type_system :: String -> Property
prop_refactoring_with_type_system typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      refactoringExpr = "//! dependent_types: on\n\n// Before refactoring:\n// type UserID = int\n// func ProcessUser(id UserID) error { ... }\n\n// After refactoring with dependent types:\ntype " ++ typeName ++ " = int where { self > 0 }\n\nfunc Process" ++ typeName ++ "(id " ++ typeName ++ ") error {\n  // Type system ensures id is always positive\n  // No need for runtime validation\n  return lookupUser(id)\n}"
      parseResult = parseTypus refactoringExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试API设计与类型系统
prop_api_design_with_type_system :: String -> Property
prop_api_design_with_type_system typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      apiDesignExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ "[n: int] struct { data [n]float64 }\n\n// API design with dependent types\nfunc (v " ++ typeName ++ "[n]) Average() float64 where { n > 0 } {\n  sum := 0.0\n  for i := 0; i < n; i++ {\n    sum += v.data[i]\n  }\n  return sum / float64(n)  // Safe: n > 0 guaranteed by type system\n}\n\nfunc (v " ++ typeName ++ "[n]) Scale(factor float64) " ++ typeName ++ "[n] {\n  result := " ++ typeName ++ "[n]{data: [n]float64{}}\n  for i := 0; i < n; i++ {\n    result.data[i] = v.data[i] * factor\n  }\n  return result\n}"
      parseResult = parseTypus apiDesignExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试测试与类型系统
prop_testing_with_type_system :: String -> Property
prop_testing_with_type_system typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      testingExpr = "//! dependent_types: on\n\ntype " ++ typeName ++ " = int where { self > 0 }\n\nfunc Test" ++ typeName ++ "Operations(t *testing.T) {\n  // Type system ensures test inputs are valid\n  a: " ++ typeName ++ " = 5\n  b: " ++ typeName ++ " = 10\n  \n  result := Add" ++ typeName ++ "(a, b)\n  expected: " ++ typeName ++ " = 15\n  \n  if result != expected {\n    t.Errorf(\"Add(%d, %d) = %d, expected %d\", a, b, result, expected)\n  }\n  \n  // Type system prevents invalid test cases\n  // invalid: " ++ typeName ++ " = -1  // Compile error\n}"
      parseResult = parseTypus testingExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试演进与类型系统
prop_evolution_with_type_system :: String -> Property
prop_evolution_with_type_system typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      evolutionExpr = "//! dependent_types: on\n\n// Version 1:\n// type " ++ typeName ++ " = int where { self > 0 }\n\n// Version 2: Extended constraint\ntype " ++ typeName ++ "V2 = int where { self > 0 && self < 1000 }\n\n// Migration function with type system safety\nfunc MigrateTo" ++ typeName ++ "V2(old " ++ typeName ++ ") " ++ typeName ++ "V2 {\n  if old >= 1000 {\n    panic(\"value too large for new version\")\n  }\n  return " ++ typeName ++ "V2(old)  // Safe: constraint satisfied\n}"
      parseResult = parseTypus evolutionExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 10. 生态系统集成测试 (Ecosystem Integration)
-- ============================================================================

-- | 测试标准库集成
prop_standard_library_integration :: String -> Property
prop_standard_library_integration funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      stdLibExpr = "//! dependent_types: on\n\ntype NonEmpty = string where { len(self) > 0 }\n\nfunc " ++ funcName ++ "(s NonEmpty) {\n  // Use standard library with Typus types\n  trimmed := strings.TrimSpace(string(s))\n  upper := strings.ToUpper(trimmed)\n  fmt.Printf(\"Processed: %s\", upper)\n}"
      parseResult = parseTypus stdLibExpr
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight parseResult
        else property True

-- | 测试第三方库集成
prop_third_party_library_integration :: String -> String -> Property
prop_third_party_library_integration libraryName funcName =
  let validNames = not (null libraryName) && not (null funcName) && 
                  all isAlphaNum libraryName && all isAlphaNum funcName
      thirdPartyExpr = "//! dependent_types: on\n\nimport \"" ++ libraryName ++ "\"\n\ntype " ++ funcName ++ "Result[T any] struct {\n  data T\n  error error\n}\n\nfunc " ++ funcName ++ "[T any](input T) " ++ funcName ++ "Result[T] {\n  // Use third-party library with Typus types\n  result, err := " ++ libraryName ++ ".Process(input)\n  return " ++ funcName ++ "Result[T]{data: result, error: err}\n}"
      parseResult = parseTypus thirdPartyExpr
  in classify validNames "valid names" $
     if validNames
        then property $ isRight parseResult
        else property True

-- | 测试构建系统集成
prop_build_system_integration :: String -> Property
prop_build_system_integration packageName =
  let validPackageName = not (null packageName) && all isAlphaNum packageName
      buildSystemExpr = "//go:generate typus -package " ++ packageName ++ "\n\n//! dependent_types: on\n\ntype " ++ packageName ++ "Type = int where { self > 0 }\n\n// This file is processed by the Typus compiler\ntype Generated struct {\n  field " ++ packageName ++ "Type\n}"
      parseResult = parseTypus buildSystemExpr
  in classify validPackageName "valid package name" $
     if validPackageName
        then property $ isRight parseResult
        else property True

-- | 测试CI/CD集成
prop_cicd_integration :: String -> Property
prop_cicd_integration projectName =
  let validProjectName = not (null projectName) && all isAlphaNum projectName
      cicdExpr = "// +build typus_test\n\n//! dependent_types: on\n\ntype " ++ projectName ++ "Config = struct {\n  debug bool\n  port int where { self > 0 && self < 65536 }\n}\n\nfunc Test" ++ projectName ++ "Config(t *testing.T) {\n  config := " ++ projectName ++ "Config{\n    debug: true,\n    port: 8080,  // Valid: 0 < 8080 < 65536\n  }\n  \n  // Test configuration\n  if !config.debug {\n    t.Error(\"Debug should be enabled\")\n  }\n}"
      parseResult = parseTypus cicdExpr
  in classify validProjectName "valid project name" $
     if validProjectName
        then property $ isRight parseResult
        else property True

-- | 测试工具链集成
prop_toolchain_integration :: String -> Property
prop_toolchain_integration toolName =
  let validToolName = not (null toolName) && all isAlphaNum toolName
      toolchainExpr = "//go:generate " ++ toolName ++ " -generate-types\n\n//! dependent_types: on\n\ntype " ++ toolName ++ "Generated[n: int] struct {\n  data [n]int\n}\n\n// This type is generated by " ++ toolName ++ "\nfunc Process" ++ toolName ++ "Generated[n: int](x: " ++ toolName ++ "Generated[n]) int {\n  sum := 0\n  for i := 0; i < n; i++ {\n    sum += x.data[i]\n  }\n  return sum\n}"
      parseResult = parseTypus toolchainExpr
  in classify validToolName "valid tool name" $
     if validToolName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 测试套件组合
-- ============================================================================

-- | 端到端编译测试组
endToEndCompilationTestGroup :: TestTree
endToEndCompilationTestGroup = testGroup "End-to-End Compilation Tests"
  [ testProperty "Complete program compilation" prop_complete_program_compilation
  , testProperty "Multi-file project structure" prop_multi_file_project_structure
  , testProperty "Library package compilation" prop_library_package_compilation
  , testProperty "Main library integration" prop_main_library_integration
  , testProperty "External dependency integration" prop_external_dependency_integration
  ]

-- | 依赖类型与所有权集成测试组
dependentTypesAndOwnershipIntegrationTestGroup :: TestTree
dependentTypesAndOwnershipIntegrationTestGroup = testGroup "Dependent Types and Ownership Integration Tests"
  [ testProperty "Ownership transfer in dependent types" prop_ownership_transfer_in_dependent_types
  , testProperty "Borrowing with type constraints" prop_borrowing_with_type_constraints
  , testProperty "Mutable borrowing with constraint validation" prop_mutable_borrowing_with_constraint_validation
  , testProperty "Lifetimes with dependent types" prop_lifetimes_with_dependent_types
  , testProperty "Ownership transfer with existential types" prop_ownership_transfer_with_existential_types
  ]

-- | 错误处理与类型系统集成测试组
errorHandlingAndTypeSystemIntegrationTestGroup :: TestTree
errorHandlingAndTypeSystemIntegrationTestGroup = testGroup "Error Handling and Type System Integration Tests"
  [ testProperty "Constraint violation error handling" prop_constraint_violation_error_handling
  , testProperty "Ownership errors with type constraints" prop_ownership_errors_with_type_constraints
  , testProperty "Error recovery with type inference" prop_error_recovery_with_type_inference
  , testProperty "Error propagation with ownership" prop_error_propagation_with_ownership
  , testProperty "Error context with dependent types" prop_error_context_with_dependent_types
  ]

-- | 性能优化与类型系统测试组
performanceOptimizationAndTypeSystemTestGroup :: TestTree
performanceOptimizationAndTypeSystemTestGroup = testGroup "Performance Optimization and Type System Tests"
  [ testProperty "Zero cost abstractions" prop_zero_cost_abstractions
  , testProperty "Compile time optimization" prop_compile_time_optimization
  , testProperty "Inlining optimization with type system" prop_inlining_optimization_with_type_system
  , testProperty "Memory layout optimization" prop_memory_layout_optimization
  , testProperty "Ownership optimization" prop_ownership_optimization
  ]

-- | 并发安全与类型系统测试组
concurrencySafetyAndTypeSystemTestGroup :: TestTree
concurrencySafetyAndTypeSystemTestGroup = testGroup "Concurrency Safety and Type System Tests"
  [ testProperty "Concurrent safe data structures" prop_concurrent_safe_data_structures
  , testProperty "Ownership with goroutines" prop_ownership_with_goroutines
  , testProperty "Channel type safety" prop_channel_type_safety
  , testProperty "Concurrency with dependent types" prop_concurrency_with_dependent_types
  , testProperty "Atomic operations with type constraints" prop_atomic_operations_with_type_constraints
  ]

-- | 模块化与类型系统测试组
modularityAndTypeSystemTestGroup :: TestTree
modularityAndTypeSystemTestGroup = testGroup "Modularity and Type System Tests"
  [ testProperty "Module boundaries with type export" prop_module_boundaries_with_type_export
  , testProperty "Cross-module type constraints" prop_cross_module_type_constraints
  , testProperty "Modular ownership" prop_modular_ownership
  , testProperty "Modular error handling" prop_modular_error_handling
  , testProperty "Modular concurrency" prop_modular_concurrency
  ]

-- | 元编程与类型系统集成测试组
metaprogrammingAndTypeSystemIntegrationTestGroup :: TestTree
metaprogrammingAndTypeSystemIntegrationTestGroup = testGroup "Metaprogramming and Type System Integration Tests"
  [ testProperty "Macros with dependent types" prop_macros_with_dependent_types
  , testProperty "Reflection with type constraints" prop_reflection_with_type_constraints
  , testProperty "Code generation with ownership" prop_code_generation_with_ownership
  , testProperty "Dynamic type creation with constraints" prop_dynamic_type_creation_with_constraints
  , testProperty "Plugin system with type safety" prop_plugin_system_with_type_safety
  ]

-- | 安全性与类型系统测试组
securityAndTypeSystemTestGroup :: TestTree
securityAndTypeSystemTestGroup = testGroup "Security and Type System Tests"
  [ testProperty "Memory safety with type constraints" prop_memory_safety_with_type_constraints
  , testProperty "Concurrency safety with ownership" prop_concurrency_safety_with_ownership
  , testProperty "Information flow security with type system" prop_information_flow_security_with_type_system
  , testProperty "Type erasure with security" prop_type_erasure_with_security
  , testProperty "Security audit with type system" prop_security_audit_with_type_system
  ]

-- | 可维护性与类型系统测试组
maintainabilityAndTypeSystemTestGroup :: TestTree
maintainabilityAndTypeSystemTestGroup = testGroup "Maintainability and Type System Tests"
  [ testProperty "Code documentation with type constraints" prop_code_documentation_with_type_constraints
  , testProperty "Refactoring with type system" prop_refactoring_with_type_system
  , testProperty "API design with type system" prop_api_design_with_type_system
  , testProperty "Testing with type system" prop_testing_with_type_system
  , testProperty "Evolution with type system" prop_evolution_with_type_system
  ]

-- | 生态系统集成测试组
ecosystemIntegrationTestGroup :: TestTree
ecosystemIntegrationTestGroup = testGroup "Ecosystem Integration Tests"
  [ testProperty "Standard library integration" prop_standard_library_integration
  , testProperty "Third party library integration" prop_third_party_library_integration
  , testProperty "Build system integration" prop_build_system_integration
  , testProperty "CI/CD integration" prop_cicd_integration
  , testProperty "Toolchain integration" prop_toolchain_integration
  ]

-- | 主测试套件
testSuite :: TestTree
testSuite = testGroup "Typus Integration Test Suite"
  [ memoryLevelTestGroup Minimal "End-to-End Compilation Tests" [endToEndCompilationTestGroup]
  , memoryLevelTestGroup Ultra "Dependent Types and Ownership Integration Tests" [dependentTypesAndOwnershipIntegrationTestGroup]
  , memoryLevelTestGroup Minimal "Error Handling and Type System Integration Tests" [errorHandlingAndTypeSystemIntegrationTestGroup]
  , memoryLevelTestGroup Ultra "Performance Optimization and Type System Tests" [performanceOptimizationAndTypeSystemTestGroup]
  , memoryLevelTestGroup Aggressive "Concurrency Safety and Type System Tests" [concurrencySafetyAndTypeSystemTestGroup]
  , memoryLevelTestGroup Minimal "Modularity and Type System Tests" [modularityAndTypeSystemTestGroup]
  , memoryLevelTestGroup Ultra "Metaprogramming and Type System Integration Tests" [metaprogrammingAndTypeSystemIntegrationTestGroup]
  , memoryLevelTestGroup Aggressive "Security and Type System Tests" [securityAndTypeSystemTestGroup]
  , memoryLevelTestGroup Ultra "Maintainability and Type System Tests" [maintainabilityAndTypeSystemTestGroup]
  , memoryLevelTestGroup Minimal "Ecosystem Integration Tests" [ecosystemIntegrationTestGroup]
  ]

-- | 导出测试套件
tests :: TestTree
tests = testSuite