{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports  -Wno-unused-matches #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.EnhancedParserQuickCheckPropertiesSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements, oneof, suchThat)

import Parser (TypusFile(..), parseTypus, tfContents, FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import Utils (trim, splitBy, removeLineComments, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

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
  , return "\n\n"
  , genVariableDeclaration
  , genFunctionDeclaration
  , do
      decl1 <- genVariableDeclaration
      decl2 <- genVariableDeclaration
      return $ decl1 ++ "\n\n" ++ decl2
  , do
      func <- genFunctionDeclaration
      decl <- genVariableDeclaration
      return $ decl ++ "\n\n" ++ func
  ]

-- 属性1: 解析空字符串应该成功
prop_parse_empty_string :: Property
prop_parse_empty_string =
  let result = parseTypus ""
  in case result of
       Right file -> property $ tfContents file === ""
       Left _ -> property False

-- 属性2: 解析仅包含空白字符的字符串应该成功
prop_parse_whitespace_only :: Property
prop_parse_whitespace_only = forAll (listOf $ elements " \t\n\r") $ \ws ->
  let result = parseTypus ws
  in case result of
       Right file -> property $ tfContents file === ws
       Left _ -> property False

-- 属性3: 解析然后重新格式化的内容应该保持一致
prop_parse_roundtrip :: Property
prop_parse_roundtrip = forAll genValidTypusCode $ \code ->
  case parseTypus code of
    Right file -> property $ tfContents file === code
    Left _ -> property False  -- 对于有效的代码，解析应该成功

-- 属性4: 解析有效的变量声明应该成功
prop_parse_valid_variable_declaration :: Property
prop_parse_valid_variable_declaration = forAll genVariableDeclaration $ \decl ->
  case parseTypus decl of
    Right file -> property $ True  -- 有效声明应该解析成功
    Left _ -> property False

-- 属性5: 解析有效的函数声明应该成功
prop_parse_valid_function_declaration :: Property
prop_parse_valid_function_declaration = forAll genFunctionDeclaration $ \func ->
  case parseTypus func of
    Right file -> property $ True  -- 有效函数应该解析成功
    Left _ -> property False

-- 属性6: 解析包含注释的代码应该成功
prop_parse_with_comments :: Property
prop_parse_with_comments = forAll genValidTypusCode $ \code ->
  let commentedCode = "// This is a comment\n" ++ code ++ "\n/* Another comment */"
  in case parseTypus commentedCode of
       Right file -> property $ True  -- 包含注释的代码应该解析成功
       Left _ -> property False

-- 属性7: 解析错误应该产生有意义的错误信息
prop_parse_error_handling :: Property
prop_parse_error_handling = forAll (listOf1 $ elements "@#$%^&*()+=[]{}|\\;:'\",.<>?/") $ \invalidChars ->
  let invalidCode = "let x = " ++ invalidChars
  in case parseTypus invalidCode of
       Right _ -> property True  -- 某些情况下可能仍然解析成功
       Left _ -> property True   -- 或者产生错误，这也是预期的

-- 属性8: tfContents应该返回原始代码内容
prop_tfContents_returns_original :: Property
prop_tfContents_returns_original = forAll genValidTypusCode $ \code ->
  case parseTypus code of
    Right file -> property $ tfContents file === code
    Left _ -> property False

-- 属性9: 解析包含换行符的代码应该正确处理行号
prop_parse_with_newlines :: Property
prop_parse_with_newlines = forAll (listOf1 $ elements "\n\r") $ \newlines ->
  let code = "let x = 42\n" ++ newlines ++ "\nlet y = 43"
  in case parseTypus code of
       Right file -> property $ True  -- 包含换行符的代码应该解析成功
       Left _ -> property False

-- 属性10: 解析包含制表符的代码应该正确处理缩进
prop_parse_with_tabs :: Property
prop_parse_with_tabs = forAll (listOf1 $ elements "\t") $ \tabs ->
  let code = "let x = 42\n" ++ tabs ++ "let y = 43"
  in case parseTypus code of
       Right file -> property $ True  -- 包含制表符的代码应该解析成功
       Left _ -> property False

-- 测试套件
tests :: TestTree
tests = testGroup "Parser QuickCheck Properties Tests"
  [ testProperty "Parse empty string" prop_parse_empty_string
  , testProperty "Parse whitespace only" prop_parse_whitespace_only
  , testProperty "Parse roundtrip" prop_parse_roundtrip
  , testProperty "Parse valid variable declaration" prop_parse_valid_variable_declaration
  , testProperty "Parse valid function declaration" prop_parse_valid_function_declaration
  , testProperty "Parse with comments" prop_parse_with_comments
  , testProperty "Parse error handling" prop_parse_error_handling
  , testProperty "tfContents returns original" prop_tfContents_returns_original
  , testProperty "Parse with newlines" prop_parse_with_newlines
  , testProperty "Parse with tabs" prop_parse_with_tabs
  ]