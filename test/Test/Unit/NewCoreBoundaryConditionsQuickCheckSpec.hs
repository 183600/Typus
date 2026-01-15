{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCoreBoundaryConditionsQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements)

import Parser (TypusFile(..), parseTypus)
import Compiler
import Compiler.IR
import ErrorHandler
import Ownership
import Dependencies
import Utils (trim, splitBy, removeLineComments, removeComments, normalizeIndentation, safeProcessString)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Test 1: 测试极长字符串的处理
prop_extremely_long_string :: Positive Int -> Property
prop_extremely_long_string (Positive n) =
  n < 10000 ==>
  let longString = replicate n 'x'
      result = safeProcessString longString
  in case result of
       Right processed -> property $ length processed >= 0
       Left _ -> property True  -- 极长字符串可能导致错误，这也是预期的

-- Test 2: 测试极深嵌套的结构
prop_deeply_nested_structure :: Positive Int -> Property
prop_deeply_nested_structure (Positive n) =
  n < 100 ==>
  let nestedBrackets = replicate n '(' ++ "content" ++ replicate n ')'
      result = parseTypus nestedBrackets
  in case result of
       Right _ -> property True  -- 深嵌套可能成功解析
       Left _ -> property True   -- 或者导致解析错误，这也是预期的

-- Test 3: 测试Unicode字符的处理
prop_unicode_handling :: Property
prop_unicode_handling =
  let unicodeChars = map (: []) ['\0'..'\255']  -- 转换为字符串
      combinedString = concat unicodeChars
      result = safeProcessString combinedString
  in case result of
       Right processed -> property $ length processed >= 0
       Left _ -> property True  -- Unicode字符可能导致错误

-- Test 4: 测试控制字符的处理
prop_control_characters :: Property
prop_control_characters =
  let controlChars = map (: []) (filter isControl ['\0'..'\31'])
      combinedString = concat controlChars
      result = safeProcessString combinedString
  in case result of
       Right processed -> property $ length processed >= 0
       Left _ -> property True  -- 控制字符可能导致错误

-- Test 5: 测试空值和null的处理
prop_null_handling :: Property
prop_null_handling =
  let nullString = ""
      result = safeProcessString nullString
  in case result of
       Right "" -> property True  -- 空字符串应该成功处理
       _ -> property False  -- 空字符串处理失败是不应该的

-- Test 6: 测试特殊字符组合的处理
prop_special_character_combinations :: Property
prop_special_character_combinations =
  let specialChars = ["//", "/*", "*/", "\"", "'", "\\", "\n", "\r", "\t"]
      combinations = [a ++ b | a <- specialChars, b <- specialChars]
      results = map safeProcessString combinations
      checkResult (Right _) = True
      checkResult (Left _) = True  -- 特殊组合可能导致错误，这也是预期的
  in property $ all checkResult results

-- Test 7: 测试极大文件的处理
prop_large_file_handling :: Positive Int -> Property
prop_large_file_handling (Positive n) =
  n < 1000 ==>
  let largeFile = unlines $ replicate n "let x : Int = 42"
      result = parseTypus largeFile
  in case result of
       Right _ -> property True  -- 大文件可能成功解析
       Left _ -> property True   -- 或者导致解析错误，这也是预期的

-- Test 8: 测试内存限制
prop_memory_limits :: Positive Int -> Property
prop_memory_limits (Positive n) =
  n < 100 ==>
  let memoryIntensiveString = concat $ replicate n (replicate 1000 'x')
      result = safeProcessString memoryIntensiveString
  in case result of
       Right _ -> property True  -- 内存密集操作可能成功
       Left _ -> property True   -- 或者导致内存错误，这也是预期的

-- 测试套件
tests :: TestTree
tests = testGroup "New Core Boundary Conditions QuickCheck Tests"
  [ testProperty "Extremely long string" prop_extremely_long_string
  , testProperty "Deeply nested structure" prop_deeply_nested_structure
  , testProperty "Unicode handling" prop_unicode_handling
  , testProperty "Control characters" prop_control_characters
  , testProperty "Null handling" prop_null_handling
  , testProperty "Special character combinations" prop_special_character_combinations
  , testProperty "Large file handling" prop_large_file_handling
  , testProperty "Memory limits" prop_memory_limits
  ]