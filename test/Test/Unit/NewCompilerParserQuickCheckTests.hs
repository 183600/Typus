{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewCompilerParserQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Monad (when, replicateM, forM_)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort, intercalate)
import Data.Char (isSpace, isDigit, isLetter, isAlphaNum, isAlpha)
import Data.Either (isLeft, isRight, fromRight)
import Data.Maybe (isJust, isNothing, listToMaybe, fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- Import Typus modules
import Parser (parseTypus, parseTypusFile)
import Compiler (compile)
import DependentTypesParser (runDependentTypesParser)
import SyntaxValidator (validateSyntax)
import SourceLocation (SourcePos(..))

-- ============================================================================
-- 1. 基本解析器测试
-- ============================================================================

-- | 测试基本包声明解析
prop_parsePackageDeclaration :: String -> Property
prop_parsePackageDeclaration packageNameStr =
  let limitedPackageName = take 10 $ filter isAlphaNum packageNameStr
      validPackageName = if null limitedPackageName then "main" else limitedPackageName
      code = "package " ++ validPackageName
      result = parseTypusFile code
  in property $ length validPackageName > 0 ==> isRight result

-- | 测试基本变量声明解析
prop_parseVariableDeclaration :: String -> String -> Property
prop_parseVariableDeclaration varNameStr typeStr =
  let limitedVarName = take 10 $ filter isAlpha varNameStr
      limitedType = take 10 $ filter isAlpha typeStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      validType = if null limitedType then "int" else limitedType
      code = "package main\n\nvar " ++ validVarName ++ " " ++ validType
      result = parseTypusFile code
  in property $ (length validVarName > 0 && length validType > 0) ==> isRight result

-- | 测试基本变量声明与初始化
prop_parseVariableDeclarationWithInit :: String -> String -> Int -> Property
prop_parseVariableDeclarationWithInit varNameStr typeStr value =
  let limitedVarName = take 10 $ filter isAlpha varNameStr
      limitedType = take 10 $ filter isAlpha typeStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      validType = if null limitedType then "int" else limitedType
      validValue = abs value `mod` 100
      code = "package main\n\nvar " ++ validVarName ++ " " ++ validType ++ " = " ++ show validValue
      result = parseTypusFile code
  in property $ (length validVarName > 0 && length validType > 0) ==> isRight result

-- | 测试基本函数声明解析
prop_parseFunctionDeclaration :: String -> String -> Property
prop_parseFunctionDeclaration funcNameStr returnTypeStr =
  let limitedFuncName = take 10 $ filter isAlpha funcNameStr
      limitedReturnType = take 10 $ filter isAlpha returnTypeStr
      validFuncName = if null limitedFuncName then "test" else limitedFuncName
      validReturnType = if null limitedReturnType then "int" else limitedReturnType
      code = "package main\n\nfunc " ++ validFuncName ++ "() " ++ validReturnType ++ " { return 0 }"
      result = parseTypusFile code
  in property $ (length validFuncName > 0 && length validReturnType > 0) ==> isRight result

-- | 测试带参数的函数声明解析
prop_parseFunctionWithParameters :: String -> String -> String -> Property
prop_parseFunctionWithParameters funcNameStr paramNameStr paramTypeStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      limitedParamName = take 8 $ filter isAlpha paramNameStr
      limitedParamType = take 8 $ filter isAlpha paramTypeStr
      validFuncName = if null limitedFuncName then "test" else limitedFuncName
      validParamName = if null limitedParamName then "x" else limitedParamName
      validParamType = if null limitedParamType then "int" else limitedParamType
      code = "package main\n\nfunc " ++ validFuncName ++ "(" ++ validParamName ++ " " ++ validParamType ++ ") int { return 0 }"
      result = parseTypusFile code
  in property $ (length validFuncName > 0 && length validParamName > 0 && length validParamType > 0) ==> isRight result

-- ============================================================================
-- 2. 表达式解析测试
-- ============================================================================

-- | 测试基本算术表达式解析
prop_parseArithmeticExpression :: Int -> Int -> Property
prop_parseArithmeticExpression a b =
  let valA = abs a `mod` 100
      valB = abs b `mod` 100
      code = "package main\n\nfunc test() int { return " ++ show valA ++ " + " ++ show valB ++ " }"
      result = parseTypusFile code
  in property $ True ==> isRight result

-- | 测试复杂算术表达式解析
prop_parseComplexArithmeticExpression :: Int -> Int -> Int -> Property
prop_parseComplexArithmeticExpression a b c =
  let valA = abs a `mod` 50
      valB = abs b `mod` 50
      valC = abs c `mod` 50
      code = "package main\n\nfunc test() int { return (" ++ show valA ++ " + " ++ show valB ++ ") * " ++ show valC ++ " }"
      result = parseTypusFile code
  in property $ True ==> isRight result

-- | 测试布尔表达式解析
prop_parseBooleanExpression :: Bool -> Bool -> Property
prop_parseBooleanExpression a b =
  let valA = a
      valB = b
      code = "package main\n\nfunc test() bool { return " ++ show valA ++ " && " ++ show valB ++ " }"
      result = parseTypusFile code
  in property $ True ==> isRight result

-- | 测试字符串表达式解析
prop_parseStringExpression :: String -> String -> Property
prop_parseStringExpression str1 str2 =
  let limitedStr1 = take 5 $ filter isAlpha str1
      limitedStr2 = take 5 $ filter isAlpha str2
      validStr1 = if null limitedStr1 then "hello" else limitedStr1
      validStr2 = if null limitedStr2 then "world" else limitedStr2
      code = "package main\n\nfunc test() string { return \"" ++ validStr1 ++ "\" + \"" ++ validStr2 ++ "\" }"
      result = parseTypusFile code
  in property $ (length validStr1 > 0 && length validStr2 > 0) ==> isRight result

-- ============================================================================
-- 3. 控制流解析测试
-- ============================================================================

-- | 测试基本if语句解析
prop_parseBasicIfStatement :: String -> Int -> Property
prop_parseBasicIfStatement varNameStr value =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      validValue = abs value `mod` 100
      code = "package main\n\nfunc test() { " ++ validVarName ++ " := " ++ show validValue ++ "\nif " ++ validVarName ++ " > 0 { fmt.Println(\"positive\") } }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result

-- | 测试if-else语句解析
prop_parseIfElseStatement :: String -> Int -> Property
prop_parseIfElseStatement varNameStr value =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      validValue = abs value `mod` 100
      code = "package main\n\nfunc test() { " ++ validVarName ++ " := " ++ show validValue ++ "\nif " ++ validVarName ++ " > 0 { fmt.Println(\"positive\") } else { fmt.Println(\"non-positive\") } }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result

-- | 测试基本for循环解析
prop_parseBasicForLoop :: String -> Int -> Property
prop_parseBasicForLoop varNameStr limit =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "i" else limitedVarName
      validLimit = max 1 (abs limit `mod` 20)
      code = "package main\n\nfunc test() { for " ++ validVarName ++ " := 0; " ++ validVarName ++ " < " ++ show validLimit ++ "; " ++ validVarName ++ "++ { fmt.Println(" ++ validVarName ++ ") } }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result

-- | 测试range for循环解析
prop_parseRangeForLoop :: String -> Property
prop_parseRangeForLoop varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "v" else limitedVarName
      code = "package main\n\nfunc test() { slice := []int{1, 2, 3}\nfor _, " ++ validVarName ++ " := range slice { fmt.Println(" ++ validVarName ++ ") } }"
      result = parseTypusFile code
  in property $ length validVarName > 0 ==> isRight result

-- ============================================================================
-- 4. 结构体和接口解析测试
-- ============================================================================

-- | 测试基本结构体解析
prop_parseBasicStruct :: String -> Property
prop_parseBasicStruct structNameStr =
  let limitedStructName = take 10 $ filter isAlpha structNameStr
      validStructName = if null limitedStructName then "MyStruct" else limitedStructName
      code = "package main\n\ntype " ++ validStructName ++ " struct { field1 int\nfield2 string }"
      result = parseTypusFile code
  in property $ length validStructName > 0 ==> isRight result

-- | 测试带方法的结构体解析
prop_parseStructWithMethods :: String -> String -> Property
prop_parseStructWithMethods structNameStr methodNameStr =
  let limitedStructName = take 10 $ filter isAlpha structNameStr
      limitedMethodName = take 10 $ filter isAlpha methodNameStr
      validStructName = if null limitedStructName then "MyStruct" else limitedStructName
      validMethodName = if null limitedMethodName then "method" else limitedMethodName
      code = "package main\n\ntype " ++ validStructName ++ " struct { field int }\n\nfunc (s " ++ validStructName ++ ") " ++ validMethodName ++ "() int { return s.field }"
      result = parseTypusFile code
  in property $ (length validStructName > 0 && length validMethodName > 0) ==> isRight result

-- | 测试基本接口解析
prop_parseBasicInterface :: String -> Property
prop_parseBasicInterface interfaceNameStr =
  let limitedInterfaceName = take 10 $ filter isAlpha interfaceNameStr
      validInterfaceName = if null limitedInterfaceName then "MyInterface" else limitedInterfaceName
      code = "package main\n\ntype " ++ validInterfaceName ++ " interface { Method1() int\nMethod2() string }"
      result = parseTypusFile code
  in property $ length validInterfaceName > 0 ==> isRight result

-- ============================================================================
-- 5. 依赖类型解析测试
-- ============================================================================

-- | 测试依赖类型指令解析
prop_parseDependentTypesDirective :: Property
prop_parseDependentTypesDirective =
  let code = "package main\n\n//! dependent_types: on"
      result = parseTypusFile code
  in property $ True ==> isRight result

-- | 测试值参数化类型解析
prop_parseValueParameterizedType :: String -> Int -> Property
prop_parseValueParameterizedType typeNameStr size =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "Vector" else limitedTypeName
      validSize = max 1 (abs size `mod` 50)
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ "[" ++ show validSize ++ ": int] struct { data [" ++ show validSize ++ "]int }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- | 测试精确类型解析
prop_parseRefinementType :: String -> Property
prop_parseRefinementType typeNameStr =
  let limitedTypeName = take 10 $ filter isAlpha typeNameStr
      validTypeName = if null limitedTypeName then "Positive" else limitedTypeName
      code = "package main\n\n//! dependent_types: on\ntype " ++ validTypeName ++ " = int where { self > 0 }"
      result = parseTypusFile code
  in property $ length validTypeName > 0 ==> isRight result

-- ============================================================================
-- 6. 所有权解析测试
-- ============================================================================

-- | 测试所有权指令解析
prop_parseOwnershipDirective :: Property
prop_parseOwnershipDirective =
  let code = "package main\n\n//! ownership: on"
      result = parseTypusFile code
  in property $ True ==> isRight result

-- | 测试移动语义解析
prop_parseMoveSemantics :: String -> String -> Property
prop_parseMoveSemantics var1Str var2Str =
  let limitedVar1 = take 8 $ filter isAlpha var1Str
      limitedVar2 = take 8 $ filter isAlpha var2Str
      validVar1 = if null limitedVar1 then "s" else limitedVar1
      validVar2 = if null limitedVar2 then "t" else limitedVar2
      code = "package main\n\n//! ownership: on\nfunc test() { " ++ validVar1 ++ " := 1\n" ++ validVar2 ++ " := " ++ validVar1 ++ "\nfmt.Println(" ++ validVar2 ++ ") }"
      result = parseTypusFile code
  in property $ (length validVar1 > 0 && length validVar2 > 0) ==> isRight result

-- | 测试借用解析
prop_parseBorrowing :: String -> String -> Property
prop_parseBorrowing varNameStr borrowNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      limitedBorrowName = take 8 $ filter isAlpha borrowNameStr
      validVarName = if null limitedVarName then "s" else limitedVarName
      validBorrowName = if null limitedBorrowName then "r" else limitedBorrowName
      code = "package main\n\n//! ownership: on\nfunc test() { " ++ validVarName ++ " := 1\n" ++ validBorrowName ++ " := &" ++ validVarName ++ "\nfmt.Println(*" ++ validBorrowName ++ ") }"
      result = parseTypusFile code
  in property $ (length validVarName > 0 && length validBorrowName > 0) ==> isRight result

-- ============================================================================
-- 7. 编译器测试
-- ============================================================================

-- | 测试基本编译
prop_testBasicCompilation :: String -> Property
prop_testBasicCompilation varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      code = "package main\n\nvar " ++ validVarName ++ " int = 1"
      parseResult = parseTypusFile code
  in property $ (length validVarName > 0 && isRight parseResult) ==> 
    isRight (compile (fromMaybe undefined (listToMaybe [fromRight undefined parseResult])))

-- | 测试函数编译
prop_testFunctionCompilation :: String -> Property
prop_testFunctionCompilation funcNameStr =
  let limitedFuncName = take 8 $ filter isAlpha funcNameStr
      validFuncName = if null limitedFuncName then "test" else limitedFuncName
      code = "package main\n\nfunc " ++ validFuncName ++ "() int { return 1 }"
      parseResult = parseTypusFile code
  in property $ (length validFuncName > 0 && isRight parseResult) ==> 
    isRight (compile (fromMaybe undefined (listToMaybe [fromRight undefined parseResult])))

-- | 测试结构体编译
prop_testStructCompilation :: String -> Property
prop_testStructCompilation structNameStr =
  let limitedStructName = take 8 $ filter isAlpha structNameStr
      validStructName = if null limitedStructName then "MyStruct" else limitedStructName
      code = "package main\n\ntype " ++ validStructName ++ " struct { field int }"
      parseResult = parseTypusFile code
  in property $ (length validStructName > 0 && isRight parseResult) ==> 
    isRight (compile (fromMaybe undefined (listToMaybe [fromRight undefined parseResult])))

-- ============================================================================
-- 8. 错误处理测试
-- ============================================================================

-- | 测试语法错误检测
prop_testSyntaxErrorDetection :: String -> Property
prop_testSyntaxErrorDetection invalidStr =
  let limitedStr = take 10 $ filter (not . isAlphaNum) (invalidStr ++ "@#$%^&*()")
      code = if null limitedStr then "@@@@" else take 5 limitedStr
      result = parseTypusFile code
  in property $ True ==> isLeft result

-- | 测试编译错误检测
prop_testCompilationErrorDetection :: String -> Property
prop_testCompilationErrorDetection varNameStr =
  let limitedVarName = take 8 $ filter isAlpha varNameStr
      validVarName = if null limitedVarName then "x" else limitedVarName
      code = "package main\n\nvar " ++ validVarName ++ " int = undefined_variable"
      parseResult = parseTypusFile code
  in property $ (length validVarName > 0 && isRight parseResult) ==> 
    isLeft (compile (fromMaybe undefined (listToMaybe [fromRight undefined parseResult])))

-- | 测试错误恢复
prop_testErrorRecovery :: String -> String -> Property
prop_testErrorRecovery var1Str var2Str =
  let limitedVar1 = take 8 $ filter isAlpha var1Str
      limitedVar2 = take 8 $ filter isAlpha var2Str
      validVar1 = if null limitedVar1 then "x" else limitedVar1
      validVar2 = if null limitedVar2 then "y" else limitedVar2
      code = "package main\n\nvar " ++ validVar1 ++ " int = undefined_variable\nvar " ++ validVar2 ++ " int = 1"
      result = parseTypusFile code
  in property $ (length validVar1 > 0 && length validVar2 > 0) ==> isLeft result

-- ============================================================================
-- 测试套件组装
-- ============================================================================

-- | 基本解析器测试套件
basicParserTests :: TestTree
basicParserTests = testGroup "基本解析器测试"
  [ testProperty "基本包声明解析" prop_parsePackageDeclaration
  , testProperty "基本变量声明解析" prop_parseVariableDeclaration
  , testProperty "基本变量声明与初始化" prop_parseVariableDeclarationWithInit
  , testProperty "基本函数声明解析" prop_parseFunctionDeclaration
  , testProperty "带参数的函数声明解析" prop_parseFunctionWithParameters
  ]

-- | 表达式解析测试套件
expressionParserTests :: TestTree
expressionParserTests = testGroup "表达式解析测试"
  [ testProperty "基本算术表达式解析" prop_parseArithmeticExpression
  , testProperty "复杂算术表达式解析" prop_parseComplexArithmeticExpression
  , testProperty "布尔表达式解析" prop_parseBooleanExpression
  , testProperty "字符串表达式解析" prop_parseStringExpression
  ]

-- | 控制流解析测试套件
controlFlowParserTests :: TestTree
controlFlowParserTests = testGroup "控制流解析测试"
  [ testProperty "基本if语句解析" prop_parseBasicIfStatement
  , testProperty "if-else语句解析" prop_parseIfElseStatement
  , testProperty "基本for循环解析" prop_parseBasicForLoop
  , testProperty "range for循环解析" prop_parseRangeForLoop
  ]

-- | 结构体和接口解析测试套件
structInterfaceParserTests :: TestTree
structInterfaceParserTests = testGroup "结构体和接口解析测试"
  [ testProperty "基本结构体解析" prop_parseBasicStruct
  , testProperty "带方法的结构体解析" prop_parseStructWithMethods
  , testProperty "基本接口解析" prop_parseBasicInterface
  ]

-- | 依赖类型解析测试套件
dependentTypesParserTests :: TestTree
dependentTypesParserTests = testGroup "依赖类型解析测试"
  [ testProperty "依赖类型指令解析" prop_parseDependentTypesDirective
  , testProperty "值参数化类型解析" prop_parseValueParameterizedType
  , testProperty "精确类型解析" prop_parseRefinementType
  ]

-- | 所有权解析测试套件
ownershipParserTests :: TestTree
ownershipParserTests = testGroup "所有权解析测试"
  [ testProperty "所有权指令解析" prop_parseOwnershipDirective
  , testProperty "移动语义解析" prop_parseMoveSemantics
  , testProperty "借用解析" prop_parseBorrowing
  ]

-- | 编译器测试套件
compilerTests :: TestTree
compilerTests = testGroup "编译器测试"
  [ testProperty "基本编译" prop_testBasicCompilation
  , testProperty "函数编译" prop_testFunctionCompilation
  , testProperty "结构体编译" prop_testStructCompilation
  ]

-- | 错误处理测试套件
errorHandlingTests :: TestTree
errorHandlingTests = testGroup "错误处理测试"
  [ testProperty "语法错误检测" prop_testSyntaxErrorDetection
  , testProperty "编译错误检测" prop_testCompilationErrorDetection
  , testProperty "错误恢复" prop_testErrorRecovery
  ]

-- | 主测试套件
tests :: TestTree
tests = testGroup "新编译器和解析器QuickCheck测试套件"
  [ basicParserTests
  , expressionParserTests
  , controlFlowParserTests
  , structInterfaceParserTests
  , dependentTypesParserTests
  , ownershipParserTests
  , compilerTests
  , errorHandlingTests
  ]