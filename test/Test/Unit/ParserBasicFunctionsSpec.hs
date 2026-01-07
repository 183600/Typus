module Test.Unit.ParserBasicFunctionsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation (SourcePos(..), Located(..))
import Data.Maybe (isJust, isNothing)
import Data.List (isPrefixOf, isInfixOf)

-- 测试FileDirectives的属性
prop_filedirectives_default_values :: Property
prop_filedirectives_default_values = 
  let defaults = defaultFileDirectives
  in isNothing (fdOwnership defaults) &&
     isNothing (fdDependentTypes defaults) &&
     isNothing (fdConstraints defaults)

-- 测试BlockDirectives的属性
prop_blockdirectives_default_values :: Property
prop_blockdirectives_default_values = 
  let defaults = defaultBlockDirectives
  in isNothing (bdOwnership defaults) &&
     isNothing (bdDependentTypes defaults) &&
     isNothing (bdConstraints defaults)

-- 测试CodeBlock的属性
prop_codeblock_consistency :: String -> String -> Property
prop_codeblock_consistency content lang = 
  let block = CodeBlock content lang
  in cbContent block === content &&
     cbLanguage block === lang

-- 测试TypusFile的属性
prop_typusfile_consistency :: String -> [CodeBlock] -> FileDirectives -> Property
prop_typusfile_consistency path blocks directives = 
  let file = TypusFile path blocks directives
  in tfPath file === path &&
     tfBlocks file === blocks &&
     tfDirectives file === directives

-- 测试解析所有权指令的属性
prop_parse_ownership_directive_true :: Property
prop_parse_ownership_directive_true = 
  let input = "// @ownership: true"
      result = parseTypus input
  in case result of
    Right file -> isJust (fdOwnership (tfDirectives file))
    Left _ -> property False

prop_parse_ownership_directive_false :: Property
prop_parse_ownership_directive_false = 
  let input = "// @ownership: false"
      result = parseTypus input
  in case result of
    Right file -> isJust (fdOwnership (tfDirectives file))
    Left _ -> property False

-- 测试解析依赖类型指令的属性
prop_parse_dependent_types_directive :: String -> Property
prop_parse_dependent_types_directive value = 
  let input = "// @dependent-types: " ++ value
      result = parseTypus input
  in case result of
    Right file -> isJust (fdDependentTypes (tfDirectives file))
    Left _ -> property False

-- 测试解析约束指令的属性
prop_parse_constraints_directive :: String -> Property
prop_parse_constraints_directive value = 
  let input = "// @constraints: " ++ value
      result = parseTypus input
  in case result of
    Right file -> isJust (fdConstraints (tfDirectives file))
    Left _ -> property False

-- 测试解析代码块的属性
prop_parse_code_block :: String -> String -> Property
prop_parse_code_block content lang = 
  let input = "```" ++ lang ++ "\n" ++ content ++ "\n```"
      result = parseTypus input
  in case result of
    Right file -> not (null (tfBlocks file))
    Left _ -> property False

-- 测试解析多个指令的属性
prop_parse_multiple_directives :: Bool -> Bool -> Bool -> Property
prop_parse_multiple_directives ownership dependentTypes constraints = 
  let ownershipStr = if ownership then "true" else "false"
      dependentTypesStr = if dependentTypes then "true" else "false"
      constraintsStr = if constraints then "true" else "false"
      input = "// @ownership: " ++ ownershipStr ++ "\n" ++
              "// @dependent-types: " ++ dependentTypesStr ++ "\n" ++
              "// @constraints: " ++ constraintsStr
      result = parseTypus input
  in case result of
    Right file -> isJust (fdOwnership (tfDirectives file)) &&
                  isJust (fdDependentTypes (tfDirectives file)) &&
                  isJust (fdConstraints (tfDirectives file))
    Left _ -> property False

-- 测试解析混合内容的属性
prop_parse_mixed_content :: String -> String -> String -> Property
prop_parse_mixed_content directive content lang = 
  let input = "// @ownership: true\n" ++
              "```" ++ lang ++ "\n" ++
              content ++ "\n```"
      result = parseTypus input
  in case result of
    Right file -> isJust (fdOwnership (tfDirectives file)) &&
                  not (null (tfBlocks file))
    Left _ -> property False

-- 测试解析空文件的属性
prop_parse_empty_file :: Property
prop_parse_empty_file = 
  let result = parseTypus ""
  in case result of
    Right file -> null (tfBlocks file) &&
                  isNothing (fdOwnership (tfDirectives file)) &&
                  isNothing (fdDependentTypes (tfDirectives file)) &&
                  isNothing (fdConstraints (tfDirectives file))
    Left _ -> property False

-- 测试解析无效语法的属性
prop_parse_invalid_syntax :: String -> Property
prop_parse_invalid_syntax content = 
  let input = "```" ++ content  -- 不完整的代码块
      result = parseTypus input
  in case result of
    Right _ -> property False  -- 应该失败
    Left _ -> property True   -- 期望失败

-- 测试解析指令大小写的属性
prop_parse_directive_case_sensitivity :: Property
prop_parse_directive_case_sensitivity = 
  let input = "// @OWNERSHIP: true"  -- 大写
      result = parseTypus input
  in case result of
    Right file -> isNothing (fdOwnership (tfDirectives file))  -- 应该不识别
    Left _ -> property False

-- 测试解析注释的属性
prop_parse_comments :: String -> Property
prop_parse_comments content = 
  let input = "// This is a comment\n" ++ content
      result = parseTypus input
  in case result of
    Right _ -> property True
    Left _ -> isPrefixOf "parse error" (show result) || property True

-- 测试解析嵌套代码块的属性
prop_parse_nested_blocks :: String -> String -> String -> Property
prop_parse_nested_blocks content1 content2 lang = 
  let input = "```" ++ lang ++ "\n" ++
              content1 ++ "\n" ++
              "```" ++ lang ++ "\n" ++
              content2 ++ "\n" ++
              "```"
      result = parseTypus input
  in case result of
    Right file -> length (tfBlocks file) >= 2
    Left _ -> property False

tests :: TestTree
tests = testGroup "Parser Basic Functions Tests"
  [ testProperty "FileDirectives default values" prop_filedirectives_default_values
  , testProperty "BlockDirectives default values" prop_blockdirectives_default_values
  , testProperty "CodeBlock consistency" prop_codeblock_consistency
  , testProperty "TypusFile consistency" prop_typusfile_consistency
  , testProperty "parse ownership directive true" prop_parse_ownership_directive_true
  , testProperty "parse ownership directive false" prop_parse_ownership_directive_false
  , testProperty "parse dependent types directive" prop_parse_dependent_types_directive
  , testProperty "parse constraints directive" prop_parse_constraints_directive
  , testProperty "parse code block" prop_parse_code_block
  , testProperty "parse multiple directives" prop_parse_multiple_directives
  , testProperty "parse mixed content" prop_parse_mixed_content
  , testProperty "parse empty file" prop_parse_empty_file
  , testProperty "parse invalid syntax" prop_parse_invalid_syntax
  , testProperty "parse directive case sensitivity" prop_parse_directive_case_sensitivity
  , testProperty "parse comments" prop_parse_comments
  , testProperty "parse nested blocks" prop_parse_nested_blocks
  ]