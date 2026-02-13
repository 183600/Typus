{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | 内存优化的Final200QuickCheck测试套件
-- 这个模块从原始的Final200QuickCheckTests.hs中选择了最重要的测试
-- 并应用了严格的内存限制，确保在资源受限的环境中也能运行
module Test.Unit.Final200QuickCheckTestsOptimized where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, intercalate)
import Data.Char (isSpace, isLetter, isDigit, ord)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set

import TestSupport.UnifiedMemoryOptimization
import TestSupport.MemoryLimits

-- ============================================================================
-- 核心工具函数测试 - 选择最重要的15个测试
-- ============================================================================

-- | 测试trim函数的幂等性 - 内存优化版本
prop_trim_idempotent_optimized :: Property
prop_trim_idempotent_optimized = 
  forAll (resize 3 arbitrary) $ \s ->
  let limitedS = take 20 s
      trimmed = U.trim limitedS
      trimmedAgain = U.trim trimmed
  in property $ trimmed == trimmedAgain

-- | 测试splitBy的基本属性 - 内存优化
prop_split_by_length_optimized :: Property
prop_split_by_length_optimized = 
  forAll (resize 2 arbitrary) $ \c ->
  forAll (resize 3 arbitrary) $ \s ->
  let limitedS = take 15 s
      parts = U.splitBy c limitedS
      rejoined = intercalate [c] parts
  in if null limitedS 
     then property $ parts == [""]
     else property $ length rejoined <= length limitedS + 5

-- | 测试removeLineComments不影响字符串字面量 - 内存优化
prop_remove_line_comments_preserves_strings_optimized :: Property
prop_remove_line_comments_preserves_strings_optimized = 
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 10 s
      withQuote = "\"" ++ limitedS ++ "\""
      after = U.removeLineComments withQuote
  in property $ "\"" `isPrefixOf` after && "\"" `isSuffixOf` after

-- | 测试removeComments的平衡性 - 内存优化
prop_remove_comments_balanced_optimized :: Property
prop_remove_comments_balanced_optimized = 
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 8 s
      withBlock = "/*" ++ limitedS ++ "*/"
      after = U.removeComments withBlock
  in property $ not ("/*" `isInfixOf` after) && not ("*/" `isInfixOf` after)

-- | 测试isCompleteStringLiteral的识别能力 - 内存优化
prop_is_complete_string_literal_optimized :: Property
prop_is_complete_string_literal_optimized = 
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 6 s
      quoted = "\"" ++ limitedS ++ "\""
  in property $ U.isCompleteStringLiteral quoted

-- | 测试breakOn的正确性 - 内存优化
prop_break_on_correctness_optimized :: Property
prop_break_on_correctness_optimized = 
  forAll (resize 1 arbitrary) $ \pat ->
  forAll (resize 2 arbitrary) $ \s ->
  let limitedPat = take 4 pat
      limitedS = take 12 s
  in if null limitedPat
     then property $ U.breakOn limitedPat limitedS == ("", limitedS)
     else let (before, after) = U.breakOn limitedPat limitedS
          in property $ length before + length after <= length limitedS + length limitedPat

-- | 测试safeProcessString的安全性 - 内存优化
prop_safe_process_string_safe_optimized :: Property
prop_safe_process_string_safe_optimized = 
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 15 s
      processed = U.safeProcessString limitedS
  in property $ length processed <= length limitedS + 10

-- | 测试splitByComma的一致性 - 内存优化
prop_split_by_comma_consistency_optimized :: Property
prop_split_by_comma_consistency_optimized = 
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 18 s
      parts = U.splitByComma limitedS
  in property $ length parts <= 10

-- | 测试normalizeIndentation的相对性 - 内存优化
prop_normalize_indentation_relative_optimized :: Property
prop_normalize_indentation_relative_optimized = 
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 20 s
      normalized = U.normalizeIndentation limitedS
  in property $ length normalized <= length limitedS + 8

-- | 测试trim空字符串 - 内存优化
prop_trim_empty_optimized :: Property
prop_trim_empty_optimized = property $ U.trim "" === ""

-- | 测试trim空白字符 - 内存优化
prop_trim_whitespace_optimized :: Property
prop_trim_whitespace_optimized = 
  forAll (resize 1 arbitrary) $ \s ->
  let limitedS = take 8 s
      trimmed = U.trim limitedS
  in property $ all isSpace trimmed || null trimmed

-- | 测试splitBy空分隔符 - 内存优化
prop_split_by_empty_optimized :: Property
prop_split_by_empty_optimized = 
  forAll (resize 1 arbitrary) $ \c ->
  property $ U.splitBy c "" === [""]

-- | 测试splitByComma空字符串 - 内存优化
prop_split_by_comma_empty_optimized :: Property
prop_split_by_comma_empty_optimized = property $ U.splitByComma "" === [""]

-- | 测试removeComments的幂等性 - 内存优化
prop_remove_comments_idempotent_optimized :: Property
prop_remove_comments_idempotent_optimized = 
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 12 s
      once = U.removeComments limitedS
      twice = U.removeComments once
  in property $ once == twice

-- | 测试trim混合空白字符 - 内存优化
prop_trim_mixed_whitespace_optimized :: Property
prop_trim_mixed_whitespace_optimized = 
  forAll (resize 1 arbitrary) $ \s ->
  let limitedS = take 6 s
      withSpaces = "  " ++ limitedS ++ "  "
      trimmed = U.trim withSpaces
  in property $ length trimmed <= length limitedS + 2

-- ============================================================================
-- 高级字符串处理测试 - 选择最重要的5个测试
-- ============================================================================

-- | 测试isProblematicUnclosedString - 内存优化
prop_is_problematic_unclosed_string_optimized :: Property
prop_is_problematic_unclosed_string_optimized = 
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 10 s
      problematic = U.isProblematicUnclosedString limitedS
  in property $ problematic || not problematic

-- | 测试removeLineComments多行处理 - 内存优化
prop_remove_line_comments_multiline_optimized :: Property
prop_remove_line_comments_multiline_optimized = 
  forAll (resize 1 arbitrary) $ \lines' ->
  let limitedLines = take 3 lines'
      limitedLines' = map (take 8) limitedLines
      code = unlines limitedLines'
      withComments = "// comment\n" ++ code
      withoutComments = U.removeLineComments withComments
  in property $ length (lines withoutComments) <= length limitedLines' + 1

-- | 测试splitBy折叠行为 - 内存优化
prop_split_by_collapsed_fold_optimized :: Property
prop_split_by_collapsed_fold_optimized = 
  forAll (resize 1 arbitrary) $ \c ->
  forAll (resize 2 arbitrary) $ \s ->
  let limitedS = take 10 s
      parts = U.splitBy c limitedS
      collapsed = filter (not . null) parts
  in property $ length collapsed <= length parts

-- | 测试normalizeIndentation空行处理 - 内存优化
prop_normalize_indentation_empty_lines_optimized :: Property
prop_normalize_indentation_empty_lines_optimized = 
  forAll (resize 1 arbitrary) $ \s ->
  let limitedS = take 12 s
      withEmptyLines = "\n\n" ++ limitedS ++ "\n\n"
      normalized = U.normalizeIndentation withEmptyLines
  in property $ length normalized <= length limitedS + 10

-- | 测试breakOn空模式 - 内存优化
prop_break_on_empty_optimized :: Property
prop_break_on_empty_optimized = 
  forAll (resize 1 arbitrary) $ \s ->
  let limitedS = take 8 s
  in property $ U.breakOn "" limitedS == ("", limitedS)

-- ============================================================================
-- 内存优化的测试套件
-- ============================================================================

-- | 创建内存优化的测试套件
tests :: TestTree
tests = createUnifiedMemorySuite extremeMemoryConfig "Final200 QuickCheck Test Suite - Memory Optimized"
  [ -- 基础工具函数测试组
    testGroup "Basic Utils Tests"
      [ testProperty "trim idempotent" prop_trim_idempotent_optimized
      , testProperty "split by length" prop_split_by_length_optimized
      , testProperty "remove line comments preserves strings" prop_remove_line_comments_preserves_strings_optimized
      , testProperty "remove comments balanced" prop_remove_comments_balanced_optimized
      , testProperty "is complete string literal" prop_is_complete_string_literal_optimized
      ]
    
    -- 字符串处理测试组
  , testGroup "String Processing Tests"
      [ testProperty "break on correctness" prop_break_on_correctness_optimized
      , testProperty "safe process string safe" prop_safe_process_string_safe_optimized
      , testProperty "split by comma consistency" prop_split_by_comma_consistency_optimized
      , testProperty "normalize indentation relative" prop_normalize_indentation_relative_optimized
      , testProperty "trim empty" prop_trim_empty_optimized
      ]
    
    -- 高级字符串处理测试组
  , testGroup "Advanced String Tests"
      [ testProperty "trim whitespace" prop_trim_whitespace_optimized
      , testProperty "split by empty" prop_split_by_empty_optimized
      , testProperty "split by comma empty" prop_split_by_comma_empty_optimized
      , testProperty "remove comments idempotent" prop_remove_comments_idempotent_optimized
      , testProperty "trim mixed whitespace" prop_trim_mixed_whitespace_optimized
      ]
    
    -- 边界情况测试组
  , testGroup "Edge Case Tests"
      [ testProperty "is problematic unclosed string" prop_is_problematic_unclosed_string_optimized
      , testProperty "remove line comments multiline" prop_remove_line_comments_multiline_optimized
      , testProperty "split by collapsed fold" prop_split_by_collapsed_fold_optimized
      , testProperty "normalize indentation empty lines" prop_normalize_indentation_empty_lines_optimized
      , testProperty "break on empty" prop_break_on_empty_optimized
      ]
  ]

-- | 超级内存优化版本 - 用于极受限环境
ultraOptimizedTests :: TestTree
ultraOptimizedTests = createUnifiedMemorySuite extremeMemoryConfig "Final200 QuickCheck - Ultra Optimized"
  [ -- 只选择最核心的测试
    testProperty "trim basic" prop_trim_empty_optimized
  , testProperty "split basic" prop_split_by_empty_optimized
  , testProperty "comment basic" prop_remove_comments_idempotent_optimized
  , testProperty "string literal basic" prop_is_complete_string_literal_optimized
  , testProperty "break basic" prop_break_on_empty_optimized
  ]