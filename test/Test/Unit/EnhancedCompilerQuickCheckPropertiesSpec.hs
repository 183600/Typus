{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.EnhancedCompilerQuickCheckPropertiesSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements, oneof, suchThat)

import Compiler (compile, CompilerError(..), CompilationPhase(..), SyntaxError(..), TypeError(..))
import Parser (parseTypus, tfContents)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)

-- 生成有效的标识符
genValidIdentifier :: Gen String
genValidIdentifier = suchThat (listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") (not . null)

-- 生成有效的类型名
genValidType :: Gen String
genValidType = oneof 
  [ return "Int"
  , return "String"
  , return "Bool"
  , return "Float"
  , return "Double"
  , return "Void"
  , genValidIdentifier
  ]

-- 生成有效的表达式
genValidExpression :: Gen String
genValidExpression = oneof
  [ return "42"
  , return "\"hello\""
  , return "true"
  , return "false"
  , return "3.14"
  , genValidIdentifier
  , do
      var <- genValidIdentifier
      return $ var ++ " + 1"
  ]

-- 生成简单的变量声明
genVariableDeclaration :: Gen String
genVariableDeclaration = do
  varName <- genValidIdentifier
  varType <- genValidType
  expr <- genValidExpression
  return $ "let " ++ varName ++ " : " ++ varType ++ " = " ++ expr

-- 生成函数声明
genFunctionDeclaration :: Gen String
genFunctionDeclaration = do
  funcName <- genValidIdentifier
  paramType <- genValidType
  returnType <- genValidType
  let body = "  return " ++ if returnType == "Void" then "" else "42"
  return $ "func " ++ funcName ++ "(param: " ++ paramType ++ ") : " ++ returnType ++ " {\n" ++ body ++ "\n}"

-- 生成有效的Typus代码
genValidTypusCode :: Gen String
genValidTypusCode = oneof
  [ return ""
  , genVariableDeclaration
  , genFunctionDeclaration
  , do
      decl1 <- genVariableDeclaration
      decl2 <- genVariableDeclaration
      return $ decl1 ++ "\n\n" ++ decl2
  ]

-- 生成包含语法错误的代码
genSyntaxErrorCode :: Gen String
genSyntaxErrorCode = oneof
  [ return "let x = +"  -- 不完整的表达式
  , return "var x int = \"string\""  -- 类型不匹配
  , return "let x: Int = \"hello\""  -- 类型不匹配
  , return "func missingReturn() int {"  -- 缺少返回语句
  , do
      var <- genValidIdentifier
      return $ "let " ++ var ++ " : = 42"  -- 缺少类型
  ]

-- 属性1: 编译空字符串应该成功
prop_compile_empty_string :: Property
prop_compile_empty_string =
  case parseTypus "" of
    Right file -> 
      case compile file of
        Right _ -> property True
        Left _ -> property False
    Left _ -> property False

-- 属性2: 编译有效的变量声明应该成功
prop_compile_valid_variable_declaration :: Property
prop_compile_valid_variable_declaration = forAll genVariableDeclaration $ \decl ->
  case parseTypus decl of
    Right file -> 
      case compile file of
        Right _ -> property True  -- 有效声明应该编译成功
        Left _ -> property False
    Left _ -> property False

-- 属性3: 编译有效的函数声明应该成功
prop_compile_valid_function_declaration :: Property
prop_compile_valid_function_declaration = forAll genFunctionDeclaration $ \func ->
  case parseTypus func of
    Right file -> 
      case compile file of
        Right _ -> property True  -- 有效函数应该编译成功
        Left _ -> property False
    Left _ -> property False

-- 属性4: 编译包含语法错误的代码应该失败
prop_compile_syntax_error :: Property
prop_compile_syntax_error = forAll genSyntaxErrorCode $ \code ->
  case parseTypus code of
    Right file -> 
      case compile file of
        Right _ -> property False  -- 语法错误应该导致编译失败
        Left _ -> property True
    Left _ -> property True  -- 解析失败也是预期的

-- 属性5: 编译结果应该是确定性的
prop_compile_deterministic :: Property
prop_compile_deterministic = forAll genValidTypusCode $ \code ->
  case parseTypus code of
    Right file -> 
      let result1 = compile file
          result2 = compile file
      in property $ result1 === result2
    Left _ -> property False

-- 属性6: 编译多个声明应该成功
prop_compile_multiple_declarations :: Property
prop_compile_multiple_declarations = 
  forAll (choose (1, 5)) $ \n ->
  forAll (replicateM n genVariableDeclaration) $ \decls ->
  let code = unlines decls
  in case parseTypus code of
       Right file -> 
         case compile file of
           Right _ -> property True  -- 多个声明应该编译成功
           Left _ -> property False
       Left _ -> property False

-- 属性7: 编译包含注释的代码应该成功
prop_compile_with_comments :: Property
prop_compile_with_comments = forAll genValidTypusCode $ \code ->
  let commentedCode = "// This is a comment\n" ++ code ++ "\n/* Another comment */"
  in case parseTypus commentedCode of
       Right file -> 
         case compile file of
           Right _ -> property True  -- 包含注释的代码应该编译成功
           Left _ -> property False
       Left _ -> property False

-- 属性8: 编译错误应该包含错误信息
prop_compile_error_contains_info :: Property
prop_compile_error_contains_info = forAll genSyntaxErrorCode $ \code ->
  case parseTypus code of
    Right file -> 
      case compile file of
        Right _ -> property False  -- 语法错误不应该编译成功
        Left errs -> property $ not (null errs)  -- 应该包含错误信息
    Left _ -> property True  -- 解析错误也是预期的

-- 属性9: 编译结果应该生成有效的Go代码
prop_compile_generates_go_code :: Property
prop_compile_generates_go_code = forAll genValidTypusCode $ \code ->
  case parseTypus code of
    Right file -> 
      case compile file of
        Right goCode -> property $ "package main" `isInfixOf` goCode || null goCode
        Left _ -> property False
    Left _ -> property False

-- 属性10: 编译复杂程序应该成功
prop_compile_complex_program :: Property
prop_compile_complex_program = 
  forAll (choose (1, 3)) $ \n ->
  forAll (choose (1, 3)) $ \m ->
  forAll (replicateM n genFunctionDeclaration) $ \funcs ->
  forAll (replicateM m genVariableDeclaration) $ \decls ->
  let code = unlines (funcs ++ decls)
  in case parseTypus code of
       Right file -> 
         case compile file of
           Right _ -> property True  -- 复杂程序应该编译成功
           Left _ -> property False
       Left _ -> property False

-- 测试套件
tests :: TestTree
tests = testGroup "Compiler QuickCheck Properties Tests"
  [ testProperty "Compile empty string" prop_compile_empty_string
  , testProperty "Compile valid variable declaration" prop_compile_valid_variable_declaration
  , testProperty "Compile valid function declaration" prop_compile_valid_function_declaration
  , testProperty "Compile syntax error" prop_compile_syntax_error
  , testProperty "Compile deterministic" prop_compile_deterministic
  , testProperty "Compile multiple declarations" prop_compile_multiple_declarations
  , testProperty "Compile with comments" prop_compile_with_comments
  , testProperty "Compile error contains info" prop_compile_error_contains_info
  , testProperty "Compile generates Go code" prop_compile_generates_go_code
  , testProperty "Compile complex program" prop_compile_complex_program
  ]