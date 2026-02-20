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

-- 导入内存优化配置
import TestSupport.MemoryOptimizedQuickCheck 
  ( QuickCheckMemoryConfig(..)
  , emergencyMemoryConfig
  , ultraLowMemoryConfig
  , criticalMemoryConfig
  , lowMemoryConfig
  , moderateMemoryConfig
  , applyQuickCheckMemoryConfig
  , withQuickCheckMemoryConfig
  , genSmallString
  , genSmallList
  , genSmallInt
  , genLimitedChar
  )

-- ============================================================================
-- Parser模块的核心QuickCheck测试 (减少到5个测试)
-- ============================================================================

-- | 测试isIdentifierChar函数的属性 (简化版本)
prop_is_identifier_char_basic :: Char -> Property
prop_is_identifier_char_basic c =
  let isAlpha = isLetter c
      isNum = isDigit c
      isUnderscore = c == '_'
      expected = isAlpha || isNum || isUnderscore
  in property $ P.isIdentifierChar c === expected

-- | 测试简单标识符解析 (内存优化版本)
prop_parse_simple_identifier :: String -> Property
prop_parse_simple_identifier s =
  let validId = not (null s) && all P.isIdentifierChar s && isLetter (head s)
      limitedS = take 3 s  -- 限制字符串长度
  in if validId
     then case P.parseExpression limitedS of
            Right expr -> property $ True
            Left _ -> property False
     else property True

-- | 测试数字字面量解析 (简化版本)
prop_parse_number_literal :: Int -> Property
prop_parse_number_literal n =
  let limitedN = mod n 100  -- 限制数字范围
      numStr = show limitedN
  in case P.parseExpression numStr of
       Right expr -> property $ True
       Left _ -> property False

-- | 测试文件级指令解析 (简化版本)
prop_parse_file_directive_ownership :: Bool -> Property
prop_parse_file_directive_ownership enabled =
  let directive = if enabled then "//! ownership: on" else "//! ownership: off"
      content = directive ++ "\npackage main"
  in case P.parseTypus content of
       Right file -> property $ True
       Left _ -> property False

-- | 测试空文件解析
prop_parse_empty_file :: Property
prop_parse_empty_file = 
  case P.parseTypus "" of
    Right file -> property $ True
    Left _ -> property False

-- ============================================================================
-- 内存优化测试套件配置
-- ============================================================================

-- | 根据环境变量获取内存配置
getMemoryConfig :: QuickCheckMemoryConfig
getMemoryConfig = 
  case lookupEnv "TYPUS_MEMORY_LEVEL" of
    Just "emergency" -> emergencyMemoryConfig
    Just "ultra-low" -> ultraLowMemoryConfig
    Just "critical" -> criticalMemoryConfig
    Just "low" -> lowMemoryConfig
    Just "moderate" -> moderateMemoryConfig
    _ -> lowMemoryConfig  -- 默认使用低内存配置

-- | 创建内存优化的测试套件
createMemoryOptimizedTestSuite :: TestTree
createMemoryOptimizedTestSuite = 
  let config = getMemoryConfig
  in applyQuickCheckMemoryConfig config $ testGroup "内存优化的Parser核心测试"
       [ testProperty "isIdentifierChar基本属性" prop_is_identifier_char_basic
       , testProperty "简单标识符解析" prop_parse_simple_identifier
       , testProperty "数字字面量解析" prop_parse_number_literal
       , testProperty "文件级指令解析" prop_parse_file_directive_ownership
       , testProperty "空文件解析" prop_parse_empty_file
       ]

-- | 主测试套件 - 使用内存优化配置
testSuite :: TestTree
testSuite = createMemoryOptimizedTestSuite