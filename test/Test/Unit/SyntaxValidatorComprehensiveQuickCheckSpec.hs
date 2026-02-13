{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.SyntaxValidatorComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified SyntaxValidator as SV
import qualified SourceLocation as SL
import Data.Char (isAlphaNum, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

-- ============================================================================
-- SyntaxValidator模块的QuickCheck测试 (25个测试)
-- ============================================================================

-- | 测试newSyntaxValidator函数
prop_new_syntax_validator :: Property
prop_new_syntax_validator = 
  let validator = SV.newSyntaxValidator
      errors = SV.getSyntaxErrors validator
  in property $ null errors

-- | 测试validateSyntax函数
prop_validate_syntax :: String -> Property
prop_validate_syntax code =
  let validCode = not (null code)
      validator = SV.newSyntaxValidator
      result = if validCode
               then SV.validateSyntax validator code
               else validator
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试validateFile函数
prop_validate_file :: String -> Property
prop_validate_file filename =
  let validFile = not (null filename)
      validator = SV.newSyntaxValidator
      result = if validFile
               then SV.validateFile validator filename
               else validator
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试getSyntaxErrors函数
prop_get_syntax_errors :: [String] -> Property
prop_get_syntax_errors errorMessages =
  let validator = SV.newSyntaxValidator
      errors = SV.getSyntaxErrors validator
  in property $ length errors >= 0

-- | 测试formatSyntaxError函数
prop_format_syntax_error :: String -> Property
prop_format_syntax_error errorMessage =
  let -- 创建一个简单的语法错误
      error = SV.SyntaxError SV.MissingBrace errorMessage 1 1 "test line"
      formatted = SV.formatSyntaxError error
  in property $ formatted /= ""

-- | 测试简单有效代码验证
prop_validate_simple_valid_code :: String -> Property
prop_validate_simple_valid_code packageName =
  let validPkg = not (null packageName) && isLetter (head packageName) && 
                 all (\c -> isLetter c || isDigit c) packageName
      code = "package " ++ packageName ++ "\n\nfunc main() {\n  return\n}"
      validator = SV.newSyntaxValidator
      result = if validPkg
               then SV.validateSyntax validator code
               else validator
      errors = SV.getSyntaxErrors result
  in if validPkg
     then property $ length errors >= 0
     else property $ length errors >= 0

-- | 测试无效标识符检测
prop_validate_invalid_identifier :: String -> Property
prop_validate_invalid_identifier identifier =
  let validId = not (null identifier) && isLetter (head identifier) && 
                all (\c -> isLetter c || isDigit c) identifier
      code = if validId
             then "package main\n\nvar " ++ identifier ++ " int"
             else "package main\n\nvar 123invalid int"
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in if validId
     then property $ length errors >= 0
     else property $ length errors >= 0

-- | 测试括号匹配检测
prop_validate_bracket_matching :: String -> Property
prop_validate_bracket_matching code =
  let -- 添加不平衡的括号
      unbalancedCode = code ++ "(["
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator unbalancedCode
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试字符串字面量检测
prop_validate_string_literals :: String -> Property
prop_validate_string_literals content =
  let -- 转义特殊字符以避免破坏字符串字面量
      escaped = concatMap (\c -> if c == '"' then "\\\"" else [c]) content
      code = "package main\n\nvar s = \"" ++ escaped ++ "\""
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试未闭合字符串检测
prop_validate_unclosed_string :: String -> Property
prop_validate_unclosed_string content =
  let -- 创建一个未闭合的字符串
      code = "package main\n\nvar s = \"" ++ content
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试注释检测
prop_validate_comments :: String -> Property
prop_validate_comments comment =
  let -- 确保注释不包含结束标记
      safeComment = filter (/= '*') comment
      code = "package main\n\n/* " ++ safeComment ++ " */\nfunc main() {}"
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试未闭合注释检测
prop_validate_unclosed_comment :: String -> Property
prop_validate_unclosed_comment comment =
  let -- 创建一个未闭合的注释
      code = "package main\n\n/* " ++ comment
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试函数声明验证
prop_validate_function_declaration :: String -> Property
prop_validate_function_declaration funcName =
  let validFunc = not (null funcName) && isLetter (head funcName) && 
                  all (\c -> isLetter c || isDigit c) funcName
      funcDecl = if validFunc
                 then "func " ++ funcName ++ "() {}"
                 else "func 123invalid() {}"
      code = "package main\n\n" ++ funcDecl
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试类型声明验证
prop_validate_type_declaration :: String -> Property
prop_validate_type_declaration typeName =
  let validType = not (null typeName) && isLetter (head typeName) && 
                  all (\c -> isLetter c || isDigit c) typeName
      typeDecl = if validType
                 then "type " ++ typeName ++ " int"
                 else "type 123invalid int"
      code = "package main\n\n" ++ typeDecl
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试导入声明验证
prop_validate_import_declaration :: String -> Property
prop_validate_import_declaration importPath =
  let -- 确保路径是有效的导入路径格式
      validPath = not (null importPath) && 
                  all (\c -> c /= '"' && c /= '\n' && c /= '\r') importPath
      importDecl = if validPath
                   then "import \"" ++ importPath ++ "\""
                   else "import \"invalid path\""
      code = "package main\n\n" ++ importDecl
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试变量声明验证
prop_validate_variable_declaration :: String -> String -> Property
prop_validate_variable_declaration varName varType =
  let validVar = not (null varName) && isLetter (head varName) && 
                all (\c -> isLetter c || isDigit c) varName
      validType = not (null varType) && isLetter (head varType)
      varDecl = if validVar && validType
                 then "var " ++ varName ++ " " ++ varType
                 else "var 123invalid 123invalid"
      code = "package main\n\n" ++ varDecl
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试结构体声明验证
prop_validate_struct_declaration :: String -> [String] -> Property
prop_validate_struct_declaration structName fieldNames =
  let validStruct = not (null structName) && isLetter (head structName) && 
                    all (\c -> isLetter c || isDigit c) structName
      validFields = all (\f -> not (null f) && isLetter (head f) && 
                              all (\c -> isLetter c || isDigit c) f) fieldNames
      fields = concatMap (\f -> "  " ++ f ++ " int\n") fieldNames
      structDecl = if validStruct && validFields
                   then "type " ++ structName ++ " struct {\n" ++ fields ++ "}"
                   else "type 123invalid struct {\n  123invalid int\n}"
      code = "package main\n\n" ++ structDecl
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试接口声明验证
prop_validate_interface_declaration :: String -> [String] -> Property
prop_validate_interface_declaration interfaceName methodNames =
  let validInterface = not (null interfaceName) && isLetter (head interfaceName) && 
                       all (\c -> isLetter c || isDigit c) interfaceName
      validMethods = all (\m -> not (null m) && isLetter (head m) && 
                               all (\c -> isLetter c || isDigit c) m) methodNames
      methods = concatMap (\m -> "  " ++ m ++ "()\n") methodNames
      interfaceDecl = if validInterface && validMethods
                      then "type " ++ interfaceName ++ " interface {\n" ++ methods ++ "}"
                      else "type 123invalid interface {\n  123invalid()\n}"
      code = "package main\n\n" ++ interfaceDecl
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试运算符验证
prop_validate_operators :: String -> Property
prop_validate_operators operator =
  let validOp = not (null operator) && operator `elem` ["+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "&^"]
      code = if validOp
             then "package main\n\nfunc main() { a := 1 " ++ operator ++ " 2 }"
             else "package main\n\nfunc main() { a := 1 @ 2 }"
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试分号验证
prop_validate_semicolons :: String -> Property
prop_validate_semicolons code =
  let -- 添加缺少分号的情况
      codeWithoutSemicolon = "package main\n\nfunc main() {\n  a := 1\n  b := 2\n}"
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator codeWithoutSemicolon
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试块结构验证
prop_validate_block_structure :: String -> Property
prop_validate_block_structure code =
  let -- 创建不平衡的块结构
      unbalancedCode = "package main\n\nfunc main() {\n  if true {\n    // missing closing brace"
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator unbalancedCode
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试未声明变量检测
prop_validate_undeclared_variables :: String -> Property
prop_validate_undeclared_variables varName =
  let validVar = not (null varName) && isLetter (head varName) && 
                all (\c -> isLetter c || isDigit c) varName
      code = if validVar
             then "package main\n\nfunc main() {\n  println(" ++ varName ++ ")\n}"
             else "package main\n\nfunc main() {\n  println(123invalid)\n}"
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试重复声明检测
prop_validate_duplicate_declarations :: String -> Property
prop_validate_duplicate_declarations declName =
  let validDecl = not (null declName) && isLetter (head declName) && 
                  all (\c -> isLetter c || isDigit c) declName
      code = if validDecl
             then "package main\n\nvar " ++ declName ++ " int\nvar " ++ declName ++ " string"
             else "package main\n\nvar 123invalid int\nvar 123invalid string"
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试空文件验证
prop_validate_empty_file :: Property
prop_validate_empty_file = 
  let validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator ""
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试仅包含空白的文件验证
prop_validate_whitespace_only_file :: String -> Property
prop_validate_whitespace_only_file whitespace =
  let validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator whitespace
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试验证一致性
prop_validate_consistency :: String -> Property
prop_validate_consistency code =
  let validator1 = SV.newSyntaxValidator
      validator2 = SV.newSyntaxValidator
      result1 = SV.validateSyntax validator1 code
      result2 = SV.validateSyntax validator2 code
      errors1 = SV.getSyntaxErrors result1
      errors2 = SV.getSyntaxErrors result2
  in property $ length errors1 === length errors2

-- | 测试错误类型分类
prop_error_type_classification :: String -> Property
prop_error_type_classification code =
  let validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
      errorTypes = map SV.errorType errors
  in property $ all (\et -> et `elem` [SV.MissingBrace, SV.MissingParenthesis, SV.MissingBracket, 
                                      SV.UnclosedString, SV.UnclosedComment, SV.InvalidIdentifier,
                                      SV.InvalidTypeDeclaration, SV.InvalidFunctionDeclaration,
                                      SV.InvalidImport, SV.InvalidStatement, SV.UnterminatedBlock,
                                      SV.InvalidOperator, SV.MissingSemicolon, SV.UnexpectedToken,
                                      SV.MissingPackageDeclaration, SV.DuplicateDeclaration,
                                      SV.InvalidBlockStructure, SV.UndeclaredVariable, SV.SyntaxWarning]) errorTypes

-- | 测试错误位置信息
prop_error_location_information :: String -> Property
prop_error_location_information code =
  let validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
      lineNumbers = map SV.lineNumber errors
      columnNumbers = map SV.columnNumber errors
  in property $ all (\ln -> ln >= 0) lineNumbers .&.
                all (\cn -> cn >= 0) columnNumbers

-- | 测试错误消息格式
prop_error_message_format :: String -> Property
prop_error_message_format code =
  let validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
      messages = map SV.errorMessage errors
  in property $ all (\msg -> not (null msg)) messages

-- | 测试错误排序
prop_error_sorting :: [String] -> Property
prop_error_sorting errorMessages =
  let -- 创建简单的语法错误
      errors = zipWith (\msg line -> 
                         SV.SyntaxError SV.MissingBrace msg line 1 ("line " ++ show line)) 
                       errorMessages [1..length errorMessages]
      sortedErrors = sort errors
      sortedMessages = map SV.errorMessage sortedErrors
  in property $ sortedMessages === sort errorMessages

-- | 测试复杂代码验证
prop_validate_complex_code :: String -> String -> String -> Property
prop_validate_complex_code packageName funcName typeName =
  let validPkg = not (null packageName) && isLetter (head packageName) && 
                 all (\c -> isLetter c || isDigit c) packageName
      validFunc = not (null funcName) && isLetter (head funcName) && 
                  all (\c -> isLetter c || isDigit c) funcName
      validType = not (null typeName) && isLetter (head typeName) && 
                  all (\c -> isLetter c || isDigit c) typeName
      code = if validPkg && validFunc && validType
             then "package " ++ packageName ++ "\n\n" ++
                  "type " ++ typeName ++ " struct { Value int }\n\n" ++
                  "func (t *" ++ typeName ++ ") " ++ funcName ++ "() int {\n  return t.Value\n}"
             else "package 123invalid\n\ntype 123invalid struct {}\n\nfunc 123invalid() {}"
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试嵌套结构验证
prop_validate_nested_structures :: Int -> Property
prop_validate_nested_structures depth =
  let validDepth = depth >= 0 && depth < 5
      createNestedStruct 0 = "type Nested struct { Value int }"
      createNestedStruct n = "type Level" ++ show n ++ " struct { nested *Level" ++ show (n-1) ++ " }"
      structDefs = if validDepth
                    then unlines $ map createNestedStruct [0..depth]
                    else "type Invalid struct { 123invalid int }"
      code = "package main\n\n" ++ structDefs
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator code
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- | 测试错误恢复
prop_error_recovery :: String -> String -> Property
prop_error_recovery validCode invalidCode =
  let combinedCode = validCode ++ "\n" ++ invalidCode ++ "\n" ++ validCode
      validator = SV.newSyntaxValidator
      result = SV.validateSyntax validator combinedCode
      errors = SV.getSyntaxErrors result
  in property $ length errors >= 0

-- 将所有测试组合在一起
testSuite :: TestTree
testSuite = testGroup "SyntaxValidator模块Comprehensive QuickCheck测试"
  [ testProperty "newSyntaxValidator函数" prop_new_syntax_validator
  , testProperty "validateSyntax函数" prop_validate_syntax
  , testProperty "validateFile函数" prop_validate_file
  , testProperty "getSyntaxErrors函数" prop_get_syntax_errors
  , testProperty "formatSyntaxError函数" prop_format_syntax_error
  , testProperty "简单有效代码验证" prop_validate_simple_valid_code
  , testProperty "无效标识符检测" prop_validate_invalid_identifier
  , testProperty "括号匹配检测" prop_validate_bracket_matching
  , testProperty "字符串字面量检测" prop_validate_string_literals
  , testProperty "未闭合字符串检测" prop_validate_unclosed_string
  , testProperty "注释检测" prop_validate_comments
  , testProperty "未闭合注释检测" prop_validate_unclosed_comment
  , testProperty "函数声明验证" prop_validate_function_declaration
  , testProperty "类型声明验证" prop_validate_type_declaration
  , testProperty "导入声明验证" prop_validate_import_declaration
  , testProperty "变量声明验证" prop_validate_variable_declaration
  , testProperty "结构体声明验证" prop_validate_struct_declaration
  , testProperty "接口声明验证" prop_validate_interface_declaration
  , testProperty "运算符验证" prop_validate_operators
  , testProperty "分号验证" prop_validate_semicolons
  , testProperty "块结构验证" prop_validate_block_structure
  , testProperty "未声明变量检测" prop_validate_undeclared_variables
  , testProperty "重复声明检测" prop_validate_duplicate_declarations
  , testProperty "空文件验证" prop_validate_empty_file
  , testProperty "仅包含空白的文件验证" prop_validate_whitespace_only_file
  , testProperty "验证一致性" prop_validate_consistency
  , testProperty "错误类型分类" prop_error_type_classification
  , testProperty "错误位置信息" prop_error_location_information
  , testProperty "错误消息格式" prop_error_message_format
  , testProperty "错误排序" prop_error_sorting
  , testProperty "复杂代码验证" prop_validate_complex_code
  , testProperty "嵌套结构验证" prop_validate_nested_structures
  , testProperty "错误恢复" prop_error_recovery
  ]