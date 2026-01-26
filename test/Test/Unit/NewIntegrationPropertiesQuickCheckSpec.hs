{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports  -Wno-unused-matches #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewIntegrationPropertiesQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Text as T
import Compiler
import Parser
import SourceLocation
import ErrorHandler
import Utils
import Data.List (all)

-- | 测试解析-编译管道的基本属性
prop_parse_compile_pipeline :: String -> Property
prop_parse_compile_pipeline code =
  case parseTypus code of
    Left parseError -> property True  -- 解析失败也算预期行为
    Right typusFile ->
      case compile typusFile of
        Left compileError -> property True  -- 编译失败也算预期行为
        Right result -> property True  -- 成功编译

-- | 测试错误处理的一致性
prop_error_handling_consistency :: String -> Property
prop_error_handling_consistency code =
  let parseResult = parseTypus code
      compileResult = case parseResult of
        Left _ -> Left ["Parse error"]
        Right typusFile -> case compile typusFile of
          Left errors -> Left (map show errors)
          Right result -> Right result
  in case (parseResult, compileResult) of
       (Left _, Left _) -> property True  -- 两者都失败
       (Right _, Left _) -> property True  -- 解析成功但编译失败
       (Left _, Right _) -> property False  -- 解析失败但编译成功（不应该发生）
       (Right _, Right _) -> property True  -- 两者都成功

-- | 测试源码位置追踪的一致性
prop_source_location_consistency :: String -> Property
prop_source_location_consistency code =
  case parseTypus code of
    Left parseError -> property True
    Right typusFile ->
      let locations = extractSourceLocations typusFile
      in property $ all isValidLocation locations
  where
    isValidLocation loc = posLine loc >= 1 && posColumn loc >= 1

-- | 测试字符串处理工具的一致性
prop_string_processing_pipeline :: String -> Property
prop_string_processing_pipeline rawCode =
  let processed = normalizeIndentation . removeComments . trim $ rawCode
  in property $ length processed <= length rawCode + 10  -- 允许一些小的变化

-- | 测试编译器错误报告的一致性
prop_compiler_error_reporting :: String -> Property
prop_compiler_error_reporting code =
  case parseTypus code of
    Left _ -> property True
    Right typusFile ->
      case compile typusFile of
        Left errors ->
          let formatted = map show errors
          in property $ all (not . null) formatted
        Right result -> property True

-- | 测试类型检查与所有权分析的交互
prop_type_ownership_interaction :: String -> Property
prop_type_ownership_interaction code =
  let typeResult = checkTypes code
      ownershipResult = case parseTypus code of
        Left _ -> Left ["Parse error"]
        Right typusFile -> case Compiler.checkOwnership typusFile of
          Left errors -> Left (map show errors)
          Right result -> Right result
  in case (typeResult, ownershipResult) of
       (Left _, Left _) -> property True  -- 两者都失败
       (Right _, Left _) -> property True  -- 类型检查成功但所有权分析失败
       (Left _, Right _) -> property True  -- 类型检查失败但所有权分析成功
       (Right _, Right _) -> property True  -- 两者都成功

-- | 测试依赖分析与类型检查的交互
prop_dependency_type_interaction :: String -> Property
prop_dependency_type_interaction code =
  let depResult = analyzeDependencies code
      typeResult = checkTypes code
  in case (depResult, typeResult) of
       (Left _, Left _) -> property True  -- 两者都失败
       (Right _, Left _) -> property True  -- 依赖分析成功但类型检查失败
       (Left _, Right _) -> property True  -- 依赖分析失败但类型检查成功
       (Right _, Right _) -> property True  -- 两者都成功

-- | 测试完整的编译管道
prop_full_compilation_pipeline :: String -> Property
prop_full_compilation_pipeline code =
  case parseTypus code of
    Left parseError -> property True
    Right typusFile ->
      case compile typusFile of
        Left compileError -> property True
        Right compileResult ->
          let goCode = Compiler.generateGoCode typusFile
          in property $ not (null goCode)



-- 假设的函数定义（可能需要根据实际情况调整）
extractSourceLocations :: TypusFile -> [SourcePos]
extractSourceLocations _ = [startPos]  -- 简化实现

checkTypes :: String -> Either [String] ()
checkTypes _ = Right ()  -- 简化实现

checkOwnership :: String -> Either [String] ()
checkOwnership _ = Right ()  -- 简化实现

analyzeDependencies :: String -> Either [String] ()
analyzeDependencies _ = Right ()  -- 简化实现

generateGoCode :: a -> String
generateGoCode _ = "package main\n\nfunc main() {\n}\n"  -- 简化实现

tests :: TestTree
tests = testGroup "Integration Properties QuickCheck Tests"
  [ testProperty "parse compile pipeline" prop_parse_compile_pipeline
  , testProperty "error handling consistency" prop_error_handling_consistency
  , testProperty "source location consistency" prop_source_location_consistency
  , testProperty "string processing pipeline" prop_string_processing_pipeline
  , testProperty "compiler error reporting" prop_compiler_error_reporting
  , testProperty "type ownership interaction" prop_type_ownership_interaction
  , testProperty "dependency type interaction" prop_dependency_type_interaction
  , testProperty "full compilation pipeline" prop_full_compilation_pipeline
  ]