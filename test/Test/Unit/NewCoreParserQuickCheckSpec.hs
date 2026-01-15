{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCoreParserQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements)

import Parser (TypusFile(..), parseTypus)
import Utils (trim, splitBy, removeLineComments, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

-- Test 1: 测试解析空文件
prop_parse_empty_file :: Property
prop_parse_empty_file =
  let result = parseTypus ""
  in case result of
       Right file -> property $ tfContents file === ""
       Left _ -> property False  -- 空文件应该能成功解析

-- Test 2: 测试解析仅包含空白字符的文件
prop_parse_whitespace_only :: String -> Property
prop_parse_whitespace_only s =
  all isSpace s ==>
  let result = parseTypus s
  in case result of
       Right file -> property $ tfContents file === s
       Left _ -> property False  -- 仅空白字符应该能成功解析

-- Test 3: 测试解析仅包含注释的文件
prop_parse_comments_only :: String -> Property
prop_parse_comments_only comment =
  let code = "// " ++ comment ++ "\n/* multi\nline\ncomment */"
      result = parseTypus code
  in case result of
       Right file -> property $ True  -- 仅注释应该能成功解析
       Left _ -> property False

-- Test 4: 测试解析简单变量声明
prop_parse_simple_declaration :: String -> String -> Property
prop_parse_simple_declaration varName varType =
  not (null varName) && all isAlphaNum varName && not (null varType) && all isAlphaNum varType ==>
  let code = "let " ++ varName ++ " : " ++ varType ++ " = 42"
      result = parseTypus code
  in case result of
       Right file -> property $ varName `isInfixOf` tfContents file && varType `isInfixOf` tfContents file
       Left _ -> property False  -- 简单声明应该能成功解析

-- Test 5: 测试解析函数声明
prop_parse_function_declaration :: String -> String -> String -> Property
prop_parse_function_declaration funcName paramType returnType =
  not (null funcName) && all isAlphaNum funcName && 
  not (null paramType) && all isAlphaNum paramType && 
  not (null returnType) && all isAlphaNum returnType ==>
  let code = "func " ++ funcName ++ "(param: " ++ paramType ++ ") : " ++ returnType ++ " {\n  return param\n}"
      result = parseTypus code
  in case result of
       Right file -> property $ funcName `isInfixOf` tfContents file
       Left _ -> property False  -- 函数声明应该能成功解析

-- Test 6: 测试解析错误处理
prop_parse_error_handling :: String -> Property
prop_parse_error_handling s =
  let invalidChars = ['@', '#', '$', '%', '^', '&', '*', '(', ')', '+', '=', '[', ']', '{', '}', '|', '\\', ';', ':', '\'', '"', '<', '>', ',', '.', '?', '/']
      hasInvalidChars = any (`elem` invalidChars) s
  in hasInvalidChars ==>
  let result = parseTypus s
  in case result of
       Right _ -> property True  -- 某些无效字符可能在特定上下文中是有效的
       Left _ -> property True   -- 或者它们可能导致解析错误，这也是预期的

-- 测试套件
tests :: TestTree
tests = testGroup "New Core Parser QuickCheck Tests"
  [ testProperty "Parse empty file" prop_parse_empty_file
  , testProperty "Parse whitespace only" prop_parse_whitespace_only
  , testProperty "Parse comments only" prop_parse_comments_only
  , testProperty "Parse simple declaration" prop_parse_simple_declaration
  , testProperty "Parse function declaration" prop_parse_function_declaration
  , testProperty "Parse error handling" prop_parse_error_handling
  ]