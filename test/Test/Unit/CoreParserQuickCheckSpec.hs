{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , locatedValue
  , startPos
  )

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isPrefixOf)
import Data.List (sort)
import Data.Char (isSpace, isAlpha, isAlphaNum)

-- ============================================================================
-- 生成器定义
-- ============================================================================

-- 生成简单的标识符
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- 生成简单的Typus代码块
genSimpleCodeBlock :: Gen String
genSimpleCodeBlock = do
  funcName <- genIdentifier
  return $ "func " ++ funcName ++ "() {\n    return 42\n}"

-- 生成文件级指令
genFileDirective :: Gen String
genFileDirective = oneof
  [ return "//! ownership: on"
  , return "//! ownership: off"
  , return "//! dependent_types: on"
  , return "//! dependent_types: off"
  , return "//! constraints: on"
  , return "//! constraints: off"
  ]

-- 生成块级指令
genBlockDirective :: Gen String
genBlockDirective = oneof
  [ return "//@ ownership: on"
  , return "//@ ownership: off"
  , return "//@ dependent_types: on"
  , return "//@ dependent_types: off"
  , return "//@ constraints: on"
  , return "//@ constraints: off"
  ]

-- 生成基本的Typus文件内容
genBasicTypusContent :: Gen String
genBasicTypusContent = do
  numDirectives <- choose (0, 3)
  directives <- listOf genFileDirective
  codeBlock <- genSimpleCodeBlock
  return $ unlines directives ++ "\n" ++ codeBlock

-- ============================================================================
-- QuickCheck 属性测试
-- ============================================================================

-- 属性: 解析空内容应该返回默认指令
prop_parse_empty_content :: Property
prop_parse_empty_content =
  case parseTypus "" of
    Left _ -> property False
    Right typusFile -> tfDirectives typusFile === defaultFileDirectives

-- 属性: 解析只有注释的文件应该保持默认指令
prop_parse_comments_only :: Property
prop_parse_comments_only =
  forAll genFileDirective $ \directive ->
    case parseTypus directive of
      Left _ -> property False
      Right typusFile -> tfDirectives typusFile /= defaultFileDirectives

-- 属性: 解析有效的简单代码应该成功
prop_parse_valid_simple_code :: Property
prop_parse_valid_simple_code =
  forAll genBasicTypusContent $ \content ->
    case parseTypus content of
      Left err -> counterexample ("Parse failed: " ++ err) $ property False
      Right _ -> property True

-- 属性: 所有权指令解析一致性
prop_ownership_directive_parsing :: Property
prop_ownership_directive_parsing =
  forAll (elements ["on", "off"]) $ \value ->
    let source = "//! ownership: " ++ value ++ "\npackage main\nfunc main() {}"
    in case parseTypus source of
         Left _ -> property False
         Right typusFile -> 
           case fdOwnership (tfDirectives typusFile) of
             Nothing -> property False
             Just loc -> locatedValue loc === (value == "on")

-- 属性: 依赖类型指令解析一致性
prop_dependent_types_directive_parsing :: Property
prop_dependent_types_directive_parsing =
  forAll (elements ["on", "off"]) $ \value ->
    let source = "//! dependent_types: " ++ value ++ "\npackage main\nfunc main() {}"
    in case parseTypus source of
         Left _ -> property False
         Right typusFile -> 
           case fdDependentTypes (tfDirectives typusFile) of
             Nothing -> property False
             Just loc -> locatedValue loc === (value == "on")

-- 属性: 约束指令解析一致性（作为依赖类型的别名）
prop_constraints_directive_parsing :: Property
prop_constraints_directive_parsing =
  forAll (elements ["on", "off"]) $ \value ->
    let source = "//! constraints: " ++ value ++ "\npackage main\nfunc main() {}"
    in case parseTypus source of
         Left _ -> property False
         Right typusFile -> 
           case fdConstraints (tfDirectives typusFile) of
             Nothing -> property False
             Just loc -> locatedValue loc === (value == "on")

-- 属性: 多个指令解析正确性
prop_multiple_directives_parsing :: Property
prop_multiple_directives_parsing =
  forAll (listOf genFileDirective) $ \directives ->
    let source = unlines directives ++ "\npackage main\nfunc main() {}"
    in case parseTypus source of
         Left _ -> counterexample "Failed to parse multiple directives" $ property False
         Right _ -> property True

-- 属性: 解析结果位置信息一致性
prop_parse_location_consistency :: Property
prop_parse_location_consistency =
  let source = "//! ownership: on\npackage main\nfunc main() {}"
  in case parseTypus source of
       Left _ -> property False
       Right typusFile ->
         case fdOwnership (tfDirectives typusFile) of
           Nothing -> property False
           Just loc -> 
             let span = locSpan loc
             in posLine (spanStart span) === 1 .&&.
                posColumn (spanStart span) === 1

-- 属性: 错误输入应该产生解析错误
prop_invalid_input_produces_error :: Property
prop_invalid_input_produces_error =
  forAll (elements ["@", "$", "%", "^", "&", "*"]) $ \invalidChar ->
    let source = invalidChar ++ " invalid syntax"
    in case parseTypus source of
         Left _ -> property True
         Right _ -> property False

-- 属性: 解析包含换行符的代码
prop_parse_with_newlines :: Property
prop_parse_with_newlines =
  forAll (choose (1, 10)) $ \numLines ->
    let source = unlines $ replicate numLines "//! ownership: on"
    in case parseTypus source of
         Left _ -> property False
         Right typusFile ->
           case fdOwnership (tfDirectives typusFile) of
             Nothing -> property False
             Just _ -> property True

-- 属性: 解析包含空格的指令
prop_parse_whitespace_in_directives :: Property
prop_parse_whitespace_in_directives =
  let source = "//!    ownership:    on   \npackage main\nfunc main() {}"
  in case parseTypus source of
       Left _ -> property False
       Right typusFile ->
         case fdOwnership (tfDirectives typusFile) of
           Nothing -> property False
           Just loc -> locatedValue loc === True

-- ============================================================================
-- 测试套件
-- ============================================================================

tests :: TestTree
tests = testGroup "Core Parser QuickCheck Tests"
  [ fastProperty "Default directives for empty content" prop_parse_empty_content
  , fastProperty "Comments only parsing" prop_parse_comments_only
  , fastProperty "Valid simple code parsing" prop_parse_valid_simple_code
  , fastProperty "Ownership directive parsing consistency" prop_ownership_directive_parsing
  , fastProperty "Dependent types directive parsing consistency" prop_dependent_types_directive_parsing
  , fastProperty "Constraints directive parsing consistency" prop_constraints_directive_parsing
  , fastProperty "Multiple directives parsing" prop_multiple_directives_parsing
  , fastProperty "Parse location consistency" prop_parse_location_consistency
  , fastProperty "Invalid input produces error" prop_invalid_input_produces_error
  , fastProperty "Parse with newlines" prop_parse_with_newlines
  , fastProperty "Parse whitespace in directives" prop_parse_whitespace_in_directives
  ]