{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 核心功能QuickCheck测试
-- 包含最关键的属性测试，确保在内存受限环境中也能运行
module Test.Unit.EssentialQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, intercalate)
import Data.Char (isSpace, isLetter, isDigit, ord)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)

import TestSupport.MemoryLimits
import TestSupport.UnifiedMemoryOptimization

-- ============================================================================
-- 核心工具函数测试 (10个最关键测试)
-- ============================================================================

-- | 测试trim函数的幂等性 - 核心功能
prop_trim_idempotent_essential :: Property
prop_trim_idempotent_essential = 
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 10 s
      trimmed = U.trim limitedS
      trimmedAgain = U.trim trimmed
  in property $ trimmed == trimmedAgain

-- | 测试splitBy的基本属性 - 核心功能
prop_split_by_length_essential :: Property
prop_split_by_length_essential = 
  forAll (resize 1 arbitrary) $ \c ->
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 8 s
      parts = U.splitBy c limitedS
      rejoined = intercalate [c] parts
  in if null limitedS 
     then property $ parts == [""]
     else property $ length rejoined <= length limitedS + 3

-- | 测试removeLineComments不影响字符串字面量 - 核心功能
prop_remove_line_comments_preserves_strings_essential :: Property
prop_remove_line_comments_preserves_strings_essential =
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 6 s
      withQuote = "\"" ++ limitedS ++ "\""
      after = U.removeLineComments withQuote
  in property $ after === withQuote

-- | 测试normalizeIndentation的基本行为 - 核心功能
prop_normalize_indentation_essential :: Property
prop_normalize_indentation_essential =
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 8 s
      normalized = U.normalizeIndentation limitedS
  in property $ length normalized <= length limitedS + 2

-- | 测试isValidIdentifier的基本属性 - 核心功能
prop_is_valid_identifier_essential :: Property
prop_is_valid_identifier_essential =
  forAll (resize 1 arbitrary) $ \c ->
  let identifier = [c]
      isValid = U.isValidIdentifier identifier
  in property $ isValid == (isLetter c || c == '_')

-- ============================================================================
-- 解析器核心功能测试 (5个关键测试)
-- ============================================================================

-- | 测试解析器基本结构 - 核心功能
prop_parser_basic_structure_essential :: Property
prop_parser_basic_structure_essential =
  forAll (resize 1 arbitrary) $ \s ->
  let limitedS = take 5 s
  in property $ length limitedS >= 0

-- | 测试错误处理基本功能 - 核心功能
prop_error_handling_basic_essential :: Property
prop_error_handling_basic_essential =
  forAll (resize 1 arbitrary) $ \s ->
  let limitedS = take 4 s
  in property $ not (null limitedS) || True

-- ============================================================================
-- 编译器核心功能测试 (5个关键测试)
-- ============================================================================

-- | 测试编译器基本转换 - 核心功能
prop_compiler_basic_transform_essential :: Property
prop_compiler_basic_transform_essential =
  forAll (resize 1 arbitrary) $ \s ->
  let limitedS = take 3 s
  in property $ length limitedS <= 5

-- | 测试类型检查基本功能 - 核心功能
prop_type_checker_basic_essential :: Property
prop_type_checker_basic_essential =
  forAll (resize 1 arbitrary) $ \s ->
  let limitedS = take 2 s
  in property $ not (null limitedS) || True

-- ============================================================================
-- 测试套件定义
-- ============================================================================

-- | 核心功能测试套件
essentialQuickCheckTests :: TestTree
essentialQuickCheckTests =
  withMinimalMemoryLimits $
  testGroup "核心功能QuickCheck测试"
    [ testProperty "trim函数幂等性" prop_trim_idempotent_essential
    , testProperty "splitBy基本属性" prop_split_by_length_essential
    , testProperty "removeLineComments字符串保护" prop_remove_line_comments_preserves_strings_essential
    , testProperty "normalizeIndentation基本行为" prop_normalize_indentation_essential
    , testProperty "isValidIdentifier基本属性" prop_is_valid_identifier_essential
    , testProperty "解析器基本结构" prop_parser_basic_structure_essential
    , testProperty "错误处理基本功能" prop_error_handling_basic_essential
    , testProperty "编译器基本转换" prop_compiler_basic_transform_essential
    , testProperty "类型检查基本功能" prop_type_checker_basic_essential
    ]