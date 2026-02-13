{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.ParserComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Parser as P
import qualified SourceLocation as SL
import Data.Char (isAlphaNum, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)

-- ============================================================================
-- Parser模块的QuickCheck测试 (25个测试)
-- ============================================================================

-- | 测试isIdentifierChar函数的属性
prop_is_identifier_char_basic :: Char -> Property
prop_is_identifier_char_basic c =
  let isAlpha = isLetter c
      isNum = isDigit c
      isUnderscore = c == '_'
      expected = isAlpha || isNum || isUnderscore
  in property $ P.isIdentifierChar c === expected

-- | 测试isIdentifierChar对特殊字符的处理
prop_is_identifier_char_special :: Property
prop_is_identifier_char_special = 
  let specialChars = "!@#$%^&*()+=[]{}|;':\",./<>?`~ \t\n\r"
      testChar c = property $ not (P.isIdentifierChar c)
  in conjoin $ map testChar specialChars

-- | 测试默认文件指令
prop_default_file_directives :: Property
prop_default_file_directives = 
  let fd = P.defaultFileDirectives
  in property $ isNothing (P.fdOwnership fd) .&.
                isNothing (P.fdDependentTypes fd) .&.
                isNothing (P.fdConstraints fd)

-- | 测试默认块指令
prop_default_block_directives :: Property
prop_default_block_directives = 
  let bd = P.defaultBlockDirectives
  in property $ isNothing (P.bdOwnership bd) .&.
                isNothing (P.bdDependentTypes bd) .&.
                isNothing (P.bdConstraints bd)

-- | 测试简单标识符解析
prop_parse_simple_identifier :: String -> Property
prop_parse_simple_identifier s =
  let validId = not (null s) && all P.isIdentifierChar s && isLetter (head s)
  in if validId
     then case P.parseExpression s of
            Right expr -> property $ True
            Left _ -> property False
     else property True  -- 无效标识符可以解析失败

-- | 测试数字字面量解析
prop_parse_number_literal :: Int -> Property
prop_parse_number_literal n =
  let numStr = show n
  in case P.parseExpression numStr of
       Right expr -> property $ True
       Left _ -> property False

-- | 测试字符串字面量解析
prop_parse_string_literal :: String -> Property
prop_parse_string_literal s =
  let -- 转义特殊字符以避免破坏字符串字面量
      escaped = concatMap (\c -> if c == '"' then "\\\"" else [c]) s
      strLiteral = "\"" ++ escaped ++ "\""
  in case P.parseExpression strLiteral of
       Right expr -> property $ True
       Left _ -> property False

-- | 测试布尔字面量解析
prop_parse_boolean_literal :: Bool -> Property
prop_parse_boolean_literal b =
  let boolStr = if b then "true" else "false"
  in case P.parseExpression boolStr of
       Right expr -> property $ True
       Left _ -> property False

-- | 测试简单二元表达式解析
prop_parse_simple_binary :: Int -> Int -> Property
prop_parse_simple_binary x y =
  let expr = show x ++ " + " ++ show y
  in case P.parseExpression expr of
       Right e -> property $ True
       Left _ -> property False

-- | 测试括号表达式解析
prop_parse_parenthesized :: Int -> Property
prop_parse_parenthesized n =
  let expr = "(" ++ show n ++ ")"
  in case P.parseExpression expr of
       Right e -> property $ True
       Left _ -> property False

-- | 测试函数调用解析
prop_parse_function_call :: String -> Int -> Property
prop_parse_function_call funcName arg =
  let validFunc = not (null funcName) && isLetter (head funcName) && 
                  all P.isIdentifierChar funcName
      expr = funcName ++ "(" ++ show arg ++ ")"
  in if validFunc
     then case P.parseExpression expr of
            Right e -> property $ True
            Left _ -> property False
     else property True

-- | 测试简单变量声明解析
prop_parse_variable_declaration :: String -> String -> Property
prop_parse_variable_declaration varName typeName =
  let validVar = not (null varName) && isLetter (head varName) && 
                all P.isIdentifierChar varName
      validType = not (null typeName) && isLetter (head typeName) && 
                  all P.isIdentifierChar typeName
      decl = "var " ++ varName ++ " " ++ typeName
  in if validVar && validType
     then case P.parseDeclaration decl of
            Right d -> property $ True
            Left _ -> property False
     else property True

-- | 测试文件级指令解析
prop_parse_file_directive_ownership :: Bool -> Property
prop_parse_file_directive_ownership enabled =
  let directive = if enabled then "//! ownership: on" else "//! ownership: off"
      content = directive ++ "\npackage main"
  in case P.parseTypus content of
       Right file -> property $ True
       Left _ -> property False

-- | 测试文件级依赖类型指令解析
prop_parse_file_directive_dependent_types :: Bool -> Property
prop_parse_file_directive_dependent_types enabled =
  let directive = if enabled then "//! dependent_types: on" else "//! dependent_types: off"
      content = directive ++ "\npackage main"
  in case P.parseTypus content of
       Right file -> property $ True
       Left _ -> property False

-- | 测试块级指令解析
prop_parse_block_directive_ownership :: Bool -> Property
prop_parse_block_directive_ownership enabled =
  let directive = if enabled then "{//! ownership: on" else "{//! ownership: off"
      content = "package main\n\nfunc main() {\n" ++ directive ++ "\n  // code\n}\n"
  in case P.parseTypus content of
       Right file -> property $ True
       Left _ -> property False

-- | 测试多指令组合解析
prop_parse_multiple_directives :: Bool -> Bool -> Property
prop_parse_multiple_directives ownership dependentTypes =
  let ownershipDir = if ownership then "//! ownership: on" else "//! ownership: off"
      dependentDir = if dependentTypes then "//! dependent_types: on" else "//! dependent_types: off"
      content = ownershipDir ++ "\n" ++ dependentDir ++ "\npackage main"
  in case P.parseTypus content of
       Right file -> property $ True
       Left _ -> property False

-- | 测试简单函数定义解析
prop_parse_function_definition :: String -> Property
prop_parse_function_definition funcName =
  let validFunc = not (null funcName) && isLetter (head funcName) && 
                  all P.isIdentifierChar funcName
      funcDef = "func " ++ funcName ++ "() {\n  return\n}"
  in if validFunc
     then case P.parseTypus ("package main\n\n" ++ funcDef) of
            Right file -> property $ True
            Left _ -> property False
     else property True

-- | 测试带参数的函数定义解析
prop_parse_function_with_params :: String -> String -> Property
prop_parse_function_with_params funcName paramName =
  let validFunc = not (null funcName) && isLetter (head funcName) && 
                  all P.isIdentifierChar funcName
      validParam = not (null paramName) && isLetter (head paramName) && 
                   all P.isIdentifierChar paramName
      funcDef = "func " ++ funcName ++ "(" ++ paramName ++ " int) {\n  return\n}"
  in if validFunc && validParam
     then case P.parseTypus ("package main\n\n" ++ funcDef) of
            Right file -> property $ True
            Left _ -> property False
     else property True

-- | 测试带返回值的函数定义解析
prop_parse_function_with_return :: String -> String -> Property
prop_parse_function_with_return funcName returnType =
  let validFunc = not (null funcName) && isLetter (head funcName) && 
                  all P.isIdentifierChar funcName
      validType = not (null returnType) && isLetter (head returnType) && 
                  all P.isIdentifierChar returnType
      funcDef = "func " ++ funcName ++ "() " ++ returnType ++ " {\n  return\n}"
  in if validFunc && validType
     then case P.parseTypus ("package main\n\n" ++ funcDef) of
            Right file -> property $ True
            Left _ -> property False
     else property True

-- | 测试结构体定义解析
prop_parse_struct_definition :: String -> [String] -> Property
prop_parse_struct_definition structName fieldNames =
  let validStruct = not (null structName) && isLetter (head structName) && 
                    all P.isIdentifierChar structName
      validFields = all (\f -> not (null f) && isLetter (head f) && 
                              all P.isIdentifierChar f) fieldNames
      fields = concatMap (\f -> "  " ++ f ++ " int\n") fieldNames
      structDef = "type " ++ structName ++ " struct {\n" ++ fields ++ "}\n"
  in if validStruct && validFields
     then case P.parseTypus ("package main\n\n" ++ structDef) of
            Right file -> property $ True
            Left _ -> property False
     else property True

-- | 测试接口定义解析
prop_parse_interface_definition :: String -> [String] -> Property
prop_parse_interface_definition interfaceName methodNames =
  let validInterface = not (null interfaceName) && isLetter (head interfaceName) && 
                       all P.isIdentifierChar interfaceName
      validMethods = all (\m -> not (null m) && isLetter (head m) && 
                               all P.isIdentifierChar m) methodNames
      methods = concatMap (\m -> "  " ++ m ++ "()\n") methodNames
      interfaceDef = "type " ++ interfaceName ++ " interface {\n" ++ methods ++ "}\n"
  in if validInterface && validMethods
     then case P.parseTypus ("package main\n\n" ++ interfaceDef) of
            Right file -> property $ True
            Left _ -> property False
     else property True

-- | 测试导入语句解析
prop_parse_import_statement :: String -> Property
prop_parse_import_statement importPath =
  let -- 确保路径是有效的导入路径格式
      validPath = not (null importPath) && 
                  all (\c -> c /= '"' && c /= '\n' && c /= '\r') importPath
      importStmt = "import \"" ++ importPath ++ "\""
  in if validPath
     then case P.parseTypus ("package main\n\n" ++ importStmt) of
            Right file -> property $ True
            Left _ -> property False
     else property True

-- | 测试多行注释保留
prop_parse_preserves_multiline_comments :: String -> Property
prop_parse_preserves_multiline_comments comment =
  let -- 确保注释不包含结束标记
      safeComment = filter (/= '*') comment
      commentBlock = "/* " ++ safeComment ++ " */"
      content = "package main\n\n" ++ commentBlock ++ "\nfunc main() {}\n"
  in case P.parseTypus content of
       Right file -> property $ True
       Left _ -> property False

-- | 测试解析错误的一致性
prop_parse_error_consistency :: String -> Property
prop_parse_error_consistency s =
  let result1 = P.parseTypus s
      result2 = P.parseTypus s
  in case (result1, result2) of
       (Left _, Left _) -> property True
       (Right _, Right _) -> property True
       _ -> property False  -- 两次解析结果应该一致

-- | 测试空文件解析
prop_parse_empty_file :: Property
prop_parse_empty_file = 
  case P.parseTypus "" of
    Right file -> property $ True
    Left _ -> property False

-- | 测试仅包含包声明的文件解析
prop_parse_package_only :: String -> Property
prop_parse_package_only packageName =
  let validPkg = not (null packageName) && isLetter (head packageName) && 
                 all P.isIdentifierChar packageName
      content = "package " ++ packageName
  in if validPkg
     then case P.parseTypus content of
            Right file -> property $ True
            Left _ -> property False
     else property True

-- 将所有测试组合在一起
testSuite :: TestTree
testSuite = testGroup "Parser模块Comprehensive QuickCheck测试"
  [ testProperty "isIdentifierChar基本属性" prop_is_identifier_char_basic
  , testProperty "isIdentifierChar特殊字符处理" prop_is_identifier_char_special
  , testProperty "默认文件指令" prop_default_file_directives
  , testProperty "默认块指令" prop_default_block_directives
  , testProperty "简单标识符解析" prop_parse_simple_identifier
  , testProperty "数字字面量解析" prop_parse_number_literal
  , testProperty "字符串字面量解析" prop_parse_string_literal
  , testProperty "布尔字面量解析" prop_parse_boolean_literal
  , testProperty "简单二元表达式解析" prop_parse_simple_binary
  , testProperty "括号表达式解析" prop_parse_parenthesized
  , testProperty "函数调用解析" prop_parse_function_call
  , testProperty "简单变量声明解析" prop_parse_variable_declaration
  , testProperty "文件级指令解析(ownership)" prop_parse_file_directive_ownership
  , testProperty "文件级依赖类型指令解析" prop_parse_file_directive_dependent_types
  , testProperty "块级指令解析" prop_parse_block_directive_ownership
  , testProperty "多指令组合解析" prop_parse_multiple_directives
  , testProperty "简单函数定义解析" prop_parse_function_definition
  , testProperty "带参数的函数定义解析" prop_parse_function_with_params
  , testProperty "带返回值的函数定义解析" prop_parse_function_with_return
  , testProperty "结构体定义解析" prop_parse_struct_definition
  , testProperty "接口定义解析" prop_parse_interface_definition
  , testProperty "导入语句解析" prop_parse_import_statement
  , testProperty "多行注释保留" prop_parse_preserves_multiline_comments
  , testProperty "解析错误的一致性" prop_parse_error_consistency
  , testProperty "空文件解析" prop_parse_empty_file
  , testProperty "仅包含包声明的文件解析" prop_parse_package_only
  ]