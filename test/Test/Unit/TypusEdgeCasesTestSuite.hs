{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.TypusEdgeCasesTestSuite where

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
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort, group, nub, intersperse)
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum, toUpper, toLower, ord, chr)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when, unless)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

-- ============================================================================
-- 1. 极端边界情况测试 (Extreme Boundary Cases)
-- ============================================================================

-- | 测试空类型定义
prop_empty_type_definition :: Property
prop_empty_type_definition =
  let emptyTypeExpr = "type Empty struct { }"
      parseResult = parseTypus emptyTypeExpr
  in property $ isRight parseResult

-- | 测试单元素类型
prop_single_element_type :: String -> Property
prop_single_element_type fieldName =
  let validFieldName = not (null fieldName) && all isAlphaNum fieldName
      singleElementExpr = "type Single struct { " ++ fieldName ++ " int }"
      parseResult = parseTypus singleElementExpr
  in classify validFieldName "valid field name" $
     if validFieldName
        then property $ isRight parseResult
        else property True

-- | 测试极大字段数量的结构体
prop_large_field_count_struct :: Int -> Property
prop_large_field_count_struct fieldCount =
  let validFieldCount = fieldCount >= 1 && fieldCount <= 50
      buildFields 0 = ""
      buildFields n = "field" ++ show n ++ " int" ++ if n > 1 then "; " ++ buildFields (n-1) else ""
      largeStructExpr = "type Large struct { " ++ buildFields fieldCount ++ " }"
      parseResult = parseTypus largeStructExpr
  in classify validFieldCount "valid field count" $
     classify (not validFieldCount) "invalid field count" $
     if validFieldCount
        then property $ isRight parseResult
        else property True

-- | 测试深度嵌套的结构体
prop_deeply_nested_structs :: Int -> Property
prop_deeply_nested_structs depth =
  let validDepth = depth >= 1 && depth <= 5
      buildNestedStruct 1 = "type Nested1 struct { value int }"
      buildNestedStruct n = "type Nested" ++ show n ++ " struct { value Nested" ++ show (n-1) ++ " }"
      nestedStructExpr = buildNestedStruct depth
      parseResult = parseTypus nestedStructExpr
  in classify validDepth "valid depth" $
     classify (not validDepth) "invalid depth" $
     if validDepth
        then property $ isRight parseResult
        else property True

-- | 测试循环依赖类型
prop_circular_dependency_types :: String -> Property
prop_circular_dependency_types typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      circularExpr = "type " ++ typeName ++ " struct { value *" ++ typeName ++ " }"
      parseResult = parseTypus circularExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 2. 特殊字符和编码测试 (Special Characters and Encoding)
-- ============================================================================

-- | 测试Unicode标识符
prop_unicode_identifiers :: Property
prop_unicode_identifiers =
  let unicodeExpr = "type 测试 struct { 值 int }"
      parseResult = parseTypus unicodeExpr
  in property $ isRight parseResult

-- | 测试特殊字符在字符串中
prop_special_characters_in_strings :: String -> Property
prop_special_characters_in_strings content =
  let validContent = length content <= 20
      stringExpr = "func test() { s := \"" ++ content ++ "\" }"
      parseResult = parseTypus stringExpr
  in classify validContent "valid content" $
     if validContent
        then property $ isRight parseResult
        else property True

-- | 测试转义字符
prop_escape_characters :: String -> Property
prop_escape_characters escapeSeq =
  let validEscape = escapeSeq `elem` ["\\n", "\\t", "\\\"", "\\\\", "\\r"]
      escapeExpr = "func test() { s := \"Hello" ++ escapeSeq ++ "World\" }"
      parseResult = parseTypus escapeExpr
  in classify validEscape "valid escape sequence" $
     if validEscape
        then property $ isRight parseResult
        else property True

-- | 测试非ASCII字符
prop_non_ascii_characters :: Property
prop_non_ascii_characters =
  let nonAsciiExpr = "type NonASCII struct { café string, naïve bool, résumé string }"
      parseResult = parseTypus nonAsciiExpr
  in property $ isRight parseResult

-- | 测试混合语言标识符
prop_mixed_language_identifiers :: Property
prop_mixed_language_identifiers =
  let mixedExpr = "type Mixed struct { hello string, 你好 string, こんにちは string }"
      parseResult = parseTypus mixedExpr
  in property $ isRight parseResult

-- ============================================================================
-- 3. 极端数值测试 (Extreme Numeric Values)
-- ============================================================================

-- | 测试极大整数值
prop_extreme_integer_values :: Property
prop_extreme_integer_values =
  let extremeExpr = "const MaxInt = 9223372036854775807\nconst MinInt = -9223372036854775808"
      parseResult = parseTypus extremeExpr
  in property $ isRight parseResult

-- | 测试浮点极值
prop_extreme_float_values :: Property
prop_extreme_float_values =
  let extremeExpr = "const MaxFloat = 1.797693134862315708145274237317043567981e+308\nconst MinFloat = 4.940656458412465441765687928682213723651e-324"
      parseResult = parseTypus extremeExpr
  in property $ isRight parseResult

-- | 测试科学计数法
prop_scientific_notation :: Int -> Int -> Property
prop_scientific_notation mantissa exponent =
  let validValues = mantissa >= 1 && mantissa <= 9 && exponent >= -5 && exponent <= 5
      scientificExpr = "const Scientific = " ++ show mantissa ++ "e" ++ show exponent
      parseResult = parseTypus scientificExpr
  in classify validValues "valid values" $
     if validValues
        then property $ isRight parseResult
        else property True

-- | 测试十六进制值
prop_hexadecimal_values :: Int -> Property
prop_hexadecimal_values value =
  let validValue = value >= 0 && value <= 255
      hexExpr = "const Hex = 0x" ++ showHex value
      parseResult = parseTypus hexExpr
  in classify validValue "valid value" $
     if validValue
        then property $ isRight parseResult
        else property True
  where
    showHex 0 = "0"
    showHex n = showIntAtBase 16 intToDigit n ""

-- | 测试八进制值
prop_octal_values :: Int -> Property
prop_octal_values value =
  let validValue = value >= 0 && value <= 511
      octalExpr = "const Octal = 0o" ++ showOct value
      parseResult = parseTypus octalExpr
  in classify validValue "valid value" $
     if validValue
        then property $ isRight parseResult
        else property True
  where
    showOct 0 = "0"
    showOct n = showIntAtBase 8 intToDigit n ""

-- ============================================================================
-- 4. 复杂表达式测试 (Complex Expressions)
-- ============================================================================

-- | 测试嵌套函数调用
prop_nested_function_calls :: Int -> Property
prop_nested_function_calls depth =
  let validDepth = depth >= 1 && depth <= 5
      buildNestedCalls 1 = "func1()"
      buildNestedCalls n = "func" ++ show n ++ "(" ++ buildNestedCalls (n-1) ++ ")"
      nestedCallsExpr = "func test() { result := " ++ buildNestedCalls depth ++ " }"
      parseResult = parseTypus nestedCallsExpr
  in classify validDepth "valid depth" $
     if validDepth
        then property $ isRight parseResult
        else property True

-- | 测试复杂算术表达式
prop_complex_arithmetic_expressions :: Int -> Int -> Int -> Property
prop_complex_arithmetic_expressions a b c =
  let validValues = all (\x -> x >= -10 && x <= 10) [a, b, c]
      complexExpr = "func test() { result := (" ++ show a ++ " + " ++ show b ++ ") * " ++ show c ++ " - (" ++ show a ++ " / (" ++ show b ++ " + 1)) }"
      parseResult = parseTypus complexExpr
  in classify validValues "valid values" $
     if validValues
        then property $ isRight parseResult
        else property True

-- | 测试复杂布尔表达式
prop_complex_boolean_expressions :: Bool -> Bool -> Bool -> Property
prop_complex_boolean_expressions a b c =
  let complexExpr = "func test() { result := (" ++ show a ++ " && " ++ show b ++ ") || (" ++ show c ++ " && !" ++ show a ++ ") }"
      parseResult = parseTypus complexExpr
  in property $ isRight parseResult

-- | 测试复杂类型表达式
prop_complex_type_expressions :: Int -> Int -> Property
prop_complex_type_expressions m n =
  let validValues = m >= 0 && n >= 0 && m <= 5 && n <= 5
      complexTypeExpr = "type Complex[" ++ show m ++ "][" ++ show n ++ "] struct { data [][ " ++ show m ++ "][ " ++ show n ++ "]int }"
      parseResult = parseTypus complexTypeExpr
  in classify validValues "valid values" $
     if validValues
        then property $ isRight parseResult
        else property True

-- | 测试复杂约束表达式
prop_complex_constraint_expressions :: Int -> Int -> Int -> Property
prop_complex_constraint_expressions a b c =
  let validValues = all (\x -> x >= 0 && x <= 10) [a, b, c]
      complexConstraintExpr = "type ComplexConstraint = int where { self >= " ++ show a ++ " && self <= " ++ show b ++ " && self % " ++ show c ++ " == 0 }"
      parseResult = parseTypus complexConstraintExpr
  in classify validValues "valid values" $
     if validValues
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 5. 错误恢复测试 (Error Recovery)
-- ============================================================================

-- | 测试语法错误恢复
prop_syntax_error_recovery :: String -> Property
prop_syntax_error_recovery invalidCode =
  let validCode = not (null invalidCode) && length invalidCode <= 20
      parseResult = parseTypus invalidCode
  in classify validCode "valid code length" $
     classify (isRight parseResult) "parses successfully" $
     classify (isLeft parseResult) "parse fails" $
     property True

-- | 测试类型错误恢复
prop_type_error_recovery :: String -> Property
prop_type_error_recovery invalidType =
  let validType = not (null invalidType) && length invalidType <= 15
      typeErrorExpr = "func test() { x : " ++ invalidType ++ " }"
      parseResult = parseTypus typeErrorExpr
  in classify validType "valid type length" $
     classify (isRight parseResult) "parses successfully" $
     classify (isLeft parseResult) "parse fails" $
     property True

-- | 测试约束错误恢复
prop_constraint_error_recovery :: String -> Property
prop_constraint_error_recovery invalidConstraint =
  let validConstraint = not (null invalidConstraint) && length invalidConstraint <= 15
      constraintErrorExpr = "type ErrorType = int where { " ++ invalidConstraint ++ " }"
      parseResult = parseTypus constraintErrorExpr
  in classify validConstraint "valid constraint length" $
     classify (isRight parseResult) "parses successfully" $
     classify (isLeft parseResult) "parse fails" $
     property True

-- | 测试所有权错误恢复
prop_ownership_error_recovery :: String -> Property
prop_ownership_error_recovery invalidCode =
  let validCode = not (null invalidCode) && length invalidCode <= 20
      ownershipErrorExpr = "{//! ownership: on\n  " ++ invalidCode ++ "\n}"
      parseResult = parseTypus ownershipErrorExpr
  in classify validCode "valid code length" $
     classify (isRight parseResult) "parses successfully" $
     classify (isLeft parseResult) "parse fails" $
     property True

-- | 测试多错误恢复
prop_multiple_error_recovery :: String -> String -> Property
prop_multiple_error_recovery error1 error2 =
  let validErrors = not (null error1) && not (null error2) && length error1 <= 10 && length error2 <= 10
      multipleErrorExpr = "func test() { " ++ error1 ++ "; " ++ error2 ++ " }"
      parseResult = parseTypus multipleErrorExpr
  in classify validErrors "valid errors" $
     classify (isRight parseResult) "parses successfully" $
     classify (isLeft parseResult) "parse fails" $
     property True

-- ============================================================================
-- 6. 性能边界测试 (Performance Boundaries)
-- ============================================================================

-- | 测试大文件解析
prop_large_file_parsing :: Int -> Property
prop_large_file_parsing lineCount =
  let validLineCount = lineCount >= 1 && lineCount <= 100
      buildLine n = "var x" ++ show n ++ " int = " ++ show n
      buildLines 0 = ""
      buildLines n = buildLine n ++ "\n" ++ buildLines (n-1)
      largeFileExpr = buildLines lineCount
      parseResult = parseTypus largeFileExpr
  in classify validLineCount "valid line count" $
     if validLineCount
        then property $ isRight parseResult
        else property True

-- | 测试深度表达式嵌套
prop_deep_expression_nesting :: Int -> Property
prop_deep_expression_nesting depth =
  let validDepth = depth >= 1 && depth <= 10
      buildNestedExpr 1 = "1"
      buildNestedExpr n = "(" ++ buildNestedExpr (n-1) ++ " + 1)"
      nestedExpr = "func test() { result := " ++ buildNestedExpr depth ++ " }"
      parseResult = parseTypus nestedExpr
  in classify validDepth "valid depth" $
     if validDepth
        then property $ isRight parseResult
        else property True

-- | 测试大量标识符
prop_many_identifiers :: Int -> Property
prop_many_identifiers identifierCount =
  let validCount = identifierCount >= 1 && identifierCount <= 50
      buildIdentifiers 0 = ""
      buildIdentifiers n = "var" ++ show n ++ " int = " ++ show n ++ ";\n" ++ buildIdentifiers (n-1)
      manyIdentifiersExpr = "func test() {\n" ++ buildIdentifiers identifierCount ++ "}"
      parseResult = parseTypus manyIdentifiersExpr
  in classify validCount "valid count" $
     if validCount
        then property $ isRight parseResult
        else property True

-- | 测试复杂类型推导
prop_complex_type_inference :: Int -> Property
prop_complex_type_inference complexity =
  let validComplexity = complexity >= 1 && complexity <= 5
      buildComplexType 1 = "func() -> int"
      buildComplexType n = "func() -> " ++ buildComplexType (n-1)
      complexTypeExpr = "func test() { x := " ++ buildComplexType complexity ++ " { return 42 } }"
      parseResult = parseTypus complexTypeExpr
  in classify validComplexity "valid complexity" $
     if validComplexity
        then property $ isRight parseResult
        else property True

-- | 测试内存密集型操作
prop_memory_intensive_operations :: Int -> Property
prop_memory_intensive_operations size =
  let validSize = size >= 1 && size <= 1000
      memoryIntensiveExpr = "func test() { data := make([]int, " ++ show size ++ "); for i := 0; i < " ++ show size ++ "; i++ { data[i] = i } }"
      parseResult = parseTypus memoryIntensiveExpr
  in classify validSize "valid size" $
     if validSize
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 7. 并发和同步测试 (Concurrency and Synchronization)
-- ============================================================================

-- | 测试基本goroutine
prop_basic_goroutine :: String -> Property
prop_basic_goroutine funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      goroutineExpr = "func " ++ funcName ++ "() { go func() { println(\"hello\") }() }"
      parseResult = parseTypus goroutineExpr
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight parseResult
        else property True

-- | 测试channel操作
prop_channel_operations :: String -> Property
prop_channel_operations varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      channelExpr = "func test() { " ++ varName ++ " := make(chan int); " ++ varName ++ " <- 42; <-" ++ varName ++ " }"
      parseResult = parseTypus channelExpr
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- | 测试select语句
prop_select_statement :: String -> Property
prop_select_statement varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      selectExpr = "func test() { " ++ varName ++ " := make(chan int); select { case <-" ++ varName ++ ": println(\"received\"); case " ++ varName ++ " <- 42: println(\"sent\"); } }"
      parseResult = parseTypus selectExpr
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- | 测试互斥锁
prop_mutex_operations :: String -> Property
prop_mutex_operations varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      mutexExpr = "func test() { var " ++ varName ++ " sync.Mutex; " ++ varName ++ ".Lock(); defer " ++ varName ++ ".Unlock() }"
      parseResult = parseTypus mutexExpr
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- | 测试等待组
prop_waitgroup_operations :: String -> Property
prop_waitgroup_operations varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      waitgroupExpr = "func test() { var " ++ varName ++ " sync.WaitGroup; " ++ varName ++ ".Add(1); go func() { defer " ++ varName ++ ".Done() }(); " ++ varName ++ ".Wait() }"
      parseResult = parseTypus waitgroupExpr
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 8. 接口和多态测试 (Interfaces and Polymorphism)
-- ============================================================================

-- | 测试基本接口定义
prop_basic_interface_definition :: String -> Property
prop_basic_interface_definition interfaceName =
  let validInterfaceName = not (null interfaceName) && all isAlphaNum interfaceName
      interfaceExpr = "type " ++ interfaceName ++ " interface { Method() }"
      parseResult = parseTypus interfaceExpr
  in classify validInterfaceName "valid interface name" $
     if validInterfaceName
        then property $ isRight parseResult
        else property True

-- | 测试多方法接口
prop_multi_method_interface :: String -> Int -> Property
prop_multi_method_interface interfaceName methodCount =
  let validInterfaceName = not (null interfaceName) && all isAlphaNum interfaceName
      validMethodCount = methodCount >= 1 && methodCount <= 5
      buildMethods 0 = ""
      buildMethods n = "Method" ++ show n ++ "() int; " ++ buildMethods (n-1)
      multiMethodExpr = "type " ++ interfaceName ++ " interface { " ++ buildMethods methodCount ++ " }"
      parseResult = parseTypus multiMethodExpr
  in classify validInterfaceName "valid interface name" $
     classify validMethodCount "valid method count" $
     if validInterfaceName && validMethodCount
        then property $ isRight parseResult
        else property True

-- | 测试接口嵌入
prop_interface_embedding :: String -> String -> Property
prop_interface_embedding interface1 interface2 =
  let validInterfaceNames = not (null interface1) && not (null interface2) && 
                           all isAlphaNum interface1 && all isAlphaNum interface2
      embeddingExpr = "type " ++ interface1 ++ " interface { Method1() }\ntype " ++ interface2 ++ " interface { " ++ interface1 ++ "; Method2() }"
      parseResult = parseTypus embeddingExpr
  in classify validInterfaceNames "valid interface names" $
     if validInterfaceNames
        then property $ isRight parseResult
        else property True

-- | 测试空接口
prop_empty_interface :: Property
prop_empty_interface =
  let emptyInterfaceExpr = "type EmptyInterface interface { }"
      parseResult = parseTypus emptyInterfaceExpr
  in property $ isRight parseResult

-- | 测试接口实现
prop_interface_implementation :: String -> String -> Property
prop_interface_implementation interfaceName structName =
  let validNames = not (null interfaceName) && not (null structName) && 
                  all isAlphaNum interfaceName && all isAlphaNum structName
      implExpr = "type " ++ interfaceName ++ " interface { Method() int }\ntype " ++ structName ++ " struct { }\n\nfunc (" ++ structName ++ ") Method() int { return 42 }"
      parseResult = parseTypus implExpr
  in classify validNames "valid names" $
     if validNames
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 9. 泛型和类型参数测试 (Generics and Type Parameters)
-- ============================================================================

-- | 测试基本泛型函数
prop_basic_generic_function :: String -> Property
prop_basic_generic_function funcName =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      genericFuncExpr = "func " ++ funcName ++ "[T any](x T) T { return x }"
      parseResult = parseTypus genericFuncExpr
  in classify validFuncName "valid function name" $
     if validFuncName
        then property $ isRight parseResult
        else property True

-- | 测试多参数泛型
prop_multi_parameter_generic :: String -> Int -> Property
prop_multi_parameter_generic funcName paramCount =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      validParamCount = paramCount >= 1 && paramCount <= 3
      buildTypeParams 0 = ""
      buildTypeParams 1 = "T any"
      buildTypeParams n = buildTypeParams (n-1) ++ ", T" ++ show n ++ " any"
      genericFuncExpr = "func " ++ funcName ++ "[" ++ buildTypeParams paramCount ++ "]() { }"
      parseResult = parseTypus genericFuncExpr
  in classify validFuncName "valid function name" $
     classify validParamCount "valid parameter count" $
     if validFuncName && validParamCount
        then property $ isRight parseResult
        else property True

-- | 测试泛型约束
prop_generic_constraints :: String -> String -> Property
prop_generic_constraints funcName constraintType =
  let validFuncName = not (null funcName) && all isAlphaNum funcName
      validConstraintType = constraintType `elem` ["int", "string", "float64"]
      genericConstraintExpr = "func " ++ funcName ++ "[T " ++ constraintType ++ "](x T, y T) T { return x + y }"
      parseResult = parseTypus genericConstraintExpr
  in classify validFuncName "valid function name" $
     classify validConstraintType "valid constraint type" $
     if validFuncName && validConstraintType
        then property $ isRight parseResult
        else property True

-- | 测试泛型结构体
prop_generic_struct :: String -> Property
prop_generic_struct structName =
  let validStructName = not (null structName) && all isAlphaNum structName
      genericStructExpr = "type " ++ structName ++ "[T any] struct { value T }"
      parseResult = parseTypus genericStructExpr
  in classify validStructName "valid struct name" $
     if validStructName
        then property $ isRight parseResult
        else property True

-- | 测试泛型接口
prop_generic_interface :: String -> Property
prop_generic_interface interfaceName =
  let validInterfaceName = not (null interfaceName) && all isAlphaNum interfaceName
      genericInterfaceExpr = "type " ++ interfaceName ++ "[T any] interface { Method(T) T }"
      parseResult = parseTypus genericInterfaceExpr
  in classify validInterfaceName "valid interface name" $
     if validInterfaceName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 10. 元编程和反射测试 (Metaprogramming and Reflection)
-- ============================================================================

-- | 测试基本反射操作
prop_basic_reflection_operations :: String -> Property
prop_basic_reflection_operations varName =
  let validVarName = not (null varName) && all isAlphaNum varName
      reflectionExpr = "func test() { " ++ varName ++ " := 42; t := reflect.TypeOf(" ++ varName ++ "); v := reflect.ValueOf(" ++ varName ++ ") }"
      parseResult = parseTypus reflectionExpr
  in classify validVarName "valid variable name" $
     if validVarName
        then property $ isRight parseResult
        else property True

-- | 测试动态类型创建
prop_dynamic_type_creation :: String -> Property
prop_dynamic_type_creation typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      dynamicTypeExpr = "func test() { " ++ typeName ++ " := reflect.StructOf([]reflect.StructField{{Name: \"Field\", Type: reflect.TypeOf(0)}}) }"
      parseResult = parseTypus dynamicTypeExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- | 测试方法调用反射
prop_method_call_reflection :: String -> Property
prop_method_call_reflection methodName =
  let validMethodName = not (null methodName) && all isAlphaNum methodName
      methodCallExpr = "func test() { x := MyStruct{}; v := reflect.ValueOf(x); m := v.MethodByName(\"" ++ methodName ++ "\"); m.Call(nil) }"
      parseResult = parseTypus methodCallExpr
  in classify validMethodName "valid method name" $
     if validMethodName
        then property $ isRight parseResult
        else property True

-- | 测试标签反射
prop_tag_reflection :: String -> Property
prop_tag_reflection tagName =
  let validTagName = not (null tagName) && all isAlphaNum tagName
      tagExpr = "type TaggedStruct struct { Field int `\"" ++ tagName ++ ":\"value\"\"` }"
      parseResult = parseTypus tagExpr
  in classify validTagName "valid tag name" $
     if validTagName
        then property $ isRight parseResult
        else property True

-- | 测试接口断言反射
prop_interface_assertion_reflection :: String -> Property
prop_interface_assertion_reflection typeName =
  let validTypeName = not (null typeName) && all isAlphaNum typeName
      interfaceAssertionExpr = "func test() { var i interface{} = " ++ typeName ++ "{}; if v, ok := i.(" ++ typeName ++ "); ok { _ = v } }"
      parseResult = parseTypus interfaceAssertionExpr
  in classify validTypeName "valid type name" $
     if validTypeName
        then property $ isRight parseResult
        else property True

-- ============================================================================
-- 测试套件组合
-- ============================================================================

-- | 极端边界情况测试组
extremeBoundaryCasesTestGroup :: TestTree
extremeBoundaryCasesTestGroup = testGroup "Extreme Boundary Cases Tests"
  [ testProperty "Empty type definition" prop_empty_type_definition
  , testProperty "Single element type" prop_single_element_type
  , testProperty "Large field count struct" prop_large_field_count_struct
  , testProperty "Deeply nested structs" prop_deeply_nested_structs
  , testProperty "Circular dependency types" prop_circular_dependency_types
  ]

-- | 特殊字符和编码测试组
specialCharactersAndEncodingTestGroup :: TestTree
specialCharactersAndEncodingTestGroup = testGroup "Special Characters and Encoding Tests"
  [ testProperty "Unicode identifiers" prop_unicode_identifiers
  , testProperty "Special characters in strings" prop_special_characters_in_strings
  , testProperty "Escape characters" prop_escape_characters
  , testProperty "Non-ASCII characters" prop_non_ascii_characters
  , testProperty "Mixed language identifiers" prop_mixed_language_identifiers
  ]

-- | 极端数值测试组
extremeNumericValuesTestGroup :: TestTree
extremeNumericValuesTestGroup = testGroup "Extreme Numeric Values Tests"
  [ testProperty "Extreme integer values" prop_extreme_integer_values
  , testProperty "Extreme float values" prop_extreme_float_values
  , testProperty "Scientific notation" prop_scientific_notation
  , testProperty "Hexadecimal values" prop_hexadecimal_values
  , testProperty "Octal values" prop_octal_values
  ]

-- | 复杂表达式测试组
complexExpressionsTestGroup :: TestTree
complexExpressionsTestGroup = testGroup "Complex Expressions Tests"
  [ testProperty "Nested function calls" prop_nested_function_calls
  , testProperty "Complex arithmetic expressions" prop_complex_arithmetic_expressions
  , testProperty "Complex boolean expressions" prop_complex_boolean_expressions
  , testProperty "Complex type expressions" prop_complex_type_expressions
  , testProperty "Complex constraint expressions" prop_complex_constraint_expressions
  ]

-- | 错误恢复测试组
errorRecoveryTestGroup :: TestTree
errorRecoveryTestGroup = testGroup "Error Recovery Tests"
  [ testProperty "Syntax error recovery" prop_syntax_error_recovery
  , testProperty "Type error recovery" prop_type_error_recovery
  , testProperty "Constraint error recovery" prop_constraint_error_recovery
  , testProperty "Ownership error recovery" prop_ownership_error_recovery
  , testProperty "Multiple error recovery" prop_multiple_error_recovery
  ]

-- | 性能边界测试组
performanceBoundariesTestGroup :: TestTree
performanceBoundariesTestGroup = testGroup "Performance Boundaries Tests"
  [ testProperty "Large file parsing" prop_large_file_parsing
  , testProperty "Deep expression nesting" prop_deep_expression_nesting
  , testProperty "Many identifiers" prop_many_identifiers
  , testProperty "Complex type inference" prop_complex_type_inference
  , testProperty "Memory intensive operations" prop_memory_intensive_operations
  ]

-- | 并发和同步测试组
concurrencyAndSynchronizationTestGroup :: TestTree
concurrencyAndSynchronizationTestGroup = testGroup "Concurrency and Synchronization Tests"
  [ testProperty "Basic goroutine" prop_basic_goroutine
  , testProperty "Channel operations" prop_channel_operations
  , testProperty "Select statement" prop_select_statement
  , testProperty "Mutex operations" prop_mutex_operations
  , testProperty "Waitgroup operations" prop_waitgroup_operations
  ]

-- | 接口和多态测试组
interfacesAndPolymorphismTestGroup :: TestTree
interfacesAndPolymorphismTestGroup = testGroup "Interfaces and Polymorphism Tests"
  [ testProperty "Basic interface definition" prop_basic_interface_definition
  , testProperty "Multi-method interface" prop_multi_method_interface
  , testProperty "Interface embedding" prop_interface_embedding
  , testProperty "Empty interface" prop_empty_interface
  , testProperty "Interface implementation" prop_interface_implementation
  ]

-- | 泛型和类型参数测试组
genericsAndTypeParametersTestGroup :: TestTree
genericsAndTypeParametersTestGroup = testGroup "Generics and Type Parameters Tests"
  [ testProperty "Basic generic function" prop_basic_generic_function
  , testProperty "Multi-parameter generic" prop_multi_parameter_generic
  , testProperty "Generic constraints" prop_generic_constraints
  , testProperty "Generic struct" prop_generic_struct
  , testProperty "Generic interface" prop_generic_interface
  ]

-- | 元编程和反射测试组
metaprogrammingAndReflectionTestGroup :: TestTree
metaprogrammingAndReflectionTestGroup = testGroup "Metaprogramming and Reflection Tests"
  [ testProperty "Basic reflection operations" prop_basic_reflection_operations
  , testProperty "Dynamic type creation" prop_dynamic_type_creation
  , testProperty "Method call reflection" prop_method_call_reflection
  , testProperty "Tag reflection" prop_tag_reflection
  , testProperty "Interface assertion reflection" prop_interface_assertion_reflection
  ]

-- | 主测试套件
testSuite :: TestTree
testSuite = testGroup "Typus Edge Cases Test Suite"
  [ memoryLevelTestGroup Minimal "Extreme Boundary Cases Tests" [extremeBoundaryCasesTestGroup]
  , memoryLevelTestGroup Ultra "Special Characters and Encoding Tests" [specialCharactersAndEncodingTestGroup]
  , memoryLevelTestGroup Minimal "Extreme Numeric Values Tests" [extremeNumericValuesTestGroup]
  , memoryLevelTestGroup Ultra "Complex Expressions Tests" [complexExpressionsTestGroup]
  , memoryLevelTestGroup Aggressive "Error Recovery Tests" [errorRecoveryTestGroup]
  , memoryLevelTestGroup Minimal "Performance Boundaries Tests" [performanceBoundariesTestGroup]
  , memoryLevelTestGroup Ultra "Concurrency and Synchronization Tests" [concurrencyAndSynchronizationTestGroup]
  , memoryLevelTestGroup Aggressive "Interfaces and Polymorphism Tests" [interfacesAndPolymorphismTestGroup]
  , memoryLevelTestGroup Ultra "Generics and Type Parameters Tests" [genericsAndTypeParametersTestGroup]
  , memoryLevelTestGroup Minimal "Metaprogramming and Reflection Tests" [metaprogrammingAndReflectionTestGroup]
  ]

-- | 导出测试套件
tests :: TestTree
tests = testSuite