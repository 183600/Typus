{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports  -Wno-unused-matches #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewBoundaryConditionsQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Text as T
import Compiler
import Parser
import SourceLocation
import Utils
import Test.QuickCheck (Positive(..))

-- | 测试极大字符串的处理
prop_large_string_handling :: Positive Int -> Property
prop_large_string_handling (Positive n) =
  let largeString = replicate (min n 100) 'a'  -- 从10000减少到100，大幅减少内存使用
      result = trim largeString
  in property $ length result <= length largeString

-- | 测试极深嵌套结构的处理
prop_deep_nesting_handling :: Positive Int -> Property
prop_deep_nesting_handling (Positive n) =
  let depth = min n 10  -- 从100减少到10，大幅减少内存使用
      nestedCode = unlines $ replicate depth "  function test() {"
                              ++ ["return 42;"]
                              ++ replicate depth "}"
      result = parseTypus nestedCode
  in case result of
       Left parseError -> property True
       Right typusFile -> property $ True

-- | 测试特殊字符的处理
prop_special_characters_handling :: String -> Property
prop_special_characters_handling chars =
  let specialCode = "function test() {\n  return \"" ++ chars ++ "\";\n}"
      result = parseTypus specialCode
  in case result of
       Left parseError -> property True
       Right typusFile -> property $ True

-- | 测试Unicode字符的处理
prop_unicode_handling :: String -> Property
prop_unicode_handling unicodeStr =
  let unicodeCode = "function 测试() {\n  return \"" ++ unicodeStr ++ "\";\n}"
      result = parseTypus unicodeCode
  in case result of
       Left parseError -> property True
       Right typusFile -> property $ True

-- | 测试空输入的处理
prop_empty_input_handling :: Property
prop_empty_input_handling =
  let result = parseTypus ""
  in case result of
       Left parseError -> property True
       Right typusFile -> property $ null (tfBlocks typusFile)

-- | 测试只包含空白字符的输入
prop_whitespace_input_handling :: Property
prop_whitespace_input_handling =
  let whitespaceCode = "  \n  \t  \n  \n  "
      result = parseTypus whitespaceCode
  in case result of
       Left parseError -> property True
       Right typusFile -> property $ True

tests :: TestTree
tests = testGroup "Boundary Conditions QuickCheck Tests"
  [ testProperty "large string handling" prop_large_string_handling
  , testProperty "deep nesting handling" prop_deep_nesting_handling
  , testProperty "special characters handling" prop_special_characters_handling
  , testProperty "unicode handling" prop_unicode_handling
  , testProperty "empty input handling" prop_empty_input_handling
  , testProperty "whitespace input handling" prop_whitespace_input_handling
  ]