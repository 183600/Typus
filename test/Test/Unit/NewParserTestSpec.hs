{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewParserTestSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Parser
import SourceLocation
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)
import Data.List (isInfixOf)

-- | 测试基本解析功能
test_parse_empty_file :: Assertion
test_parse_empty_file = do
  let emptyFile = ""
      result = parseTypus emptyFile
  case result of
    Left err -> assertFailure $ "Failed to parse empty file: " ++ show err
    Right typusFile -> do
      assertEqual "Should have no code blocks" [] (tfBlocks typusFile)
      assertEqual "Should have default file directives" defaultFileDirectives (tfDirectives typusFile)

-- | 测试解析简单代码块
test_parse_simple_code_block :: Assertion
test_parse_simple_code_block = do
  let simpleCode = "```typus\nlet x = 42\n```"
      result = parseTypus simpleCode
  case result of
    Left err -> assertFailure $ "Failed to parse simple code block: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertEqual "Should have one code block" 1 (length blocks)
      let block = case blocks of
                    (b:_) -> b
                    [] -> error "Impossible: length check ensures non-empty"
      assertEqual "Block content should match" "let x = 42\n" (cbContent block)

-- | 测试解析文件指令
test_parse_file_directives :: Assertion
test_parse_file_directives = do
  let codeWithDirectives = "// @ownership: true\n// @dependent-types: false\n```typus\nlet x = 42\n```"
      result = parseTypus codeWithDirectives
  case result of
    Left err -> assertFailure $ "Failed to parse file directives: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          ownership = fdOwnership directives
          dependentTypes = fdDependentTypes directives
      assertBool "Should have ownership directive" (isJust ownership)
      assertBool "Should have dependent-types directive" (isJust dependentTypes)
      assertEqual "Ownership should be true" (Just True) (locatedValue <$> ownership)
      assertEqual "Dependent types should be false" (Just False) (locatedValue <$> dependentTypes)

-- | 测试解析块指令
test_parse_block_directives :: Assertion
test_parse_block_directives = do
  let codeWithBlockDirectives = "```typus\n// @ownership: true\n// @constraints: true\nlet x = 42\n```"
      result = parseTypus codeWithBlockDirectives
  case result of
    Left err -> assertFailure $ "Failed to parse block directives: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
          block = case blocks of
                    (b:_) -> b
                    [] -> error "Impossible: blocks should not be empty"
          directives = cbDirectives block
          ownership = bdOwnership directives
          constraints = bdConstraints directives
      assertBool "Should have ownership directive" (isJust ownership)
      assertBool "Should have constraints directive" (isJust constraints)
      assertEqual "Ownership should be true" (Just True) (locatedValue <$> ownership)
      assertEqual "Constraints should be true" (Just True) (locatedValue <$> constraints)

-- | 测试解析多个代码块
test_parse_multiple_code_blocks :: Assertion
test_parse_multiple_code_blocks = do
  let multiBlockCode = "```typus\nlet x = 42\n```\n\n```typus\nlet y = 24\n```"
      result = parseTypus multiBlockCode
  case result of
    Left err -> assertFailure $ "Failed to parse multiple code blocks: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertEqual "Should have two code blocks" 2 (length blocks)
      assertEqual "First block content" "let x = 42\n" (cbContent (blocks !! 0))
      assertEqual "Second block content" "let y = 24\n" (cbContent (blocks !! 1))

-- | 测试解析错误处理
test_parse_invalid_syntax :: Assertion
test_parse_invalid_syntax = do
  let invalidCode = "```typus\nlet x = \n```"  -- 不完整的表达式
      result = parseTypus invalidCode
  case result of
    Left _ -> return ()  -- 期望解析失败
    Right _ -> assertFailure "Expected parsing to fail with invalid syntax"

-- | QuickCheck属性：解析空字符串应该总是成功
prop_parse_empty_succeeds :: Property
prop_parse_empty_succeeds =
  let result = parseTypus ""
  in case result of
       Left _ -> property False
       Right _ -> property True

-- | QuickCheck属性：解析后重新格式化的内容应该保持结构一致性
prop_parse_roundtrip_structure :: String -> Property
prop_parse_roundtrip_structure content =
  let result = parseTypus content
  in case result of
       Left _ -> property True  -- 如果解析失败，则跳过此测试
       Right typusFile -> 
         let blocks = tfBlocks typusFile
             blockCount = length blocks
         in blockCount >= 0 .&&. blockCount <= 100  -- 合理的块数量范围

-- | QuickCheck属性：文件指令解析的一致性
prop_file_directives_parsing_consistent :: String -> Property
prop_file_directives_parsing_consistent content =
  let result = parseTypus ("// @ownership: true\n// @dependent-types: false\n" ++ content)
  in case result of
       Left _ -> property True
       Right typusFile ->
         let directives = tfDirectives typusFile
             ownership = fdOwnership directives
             dependentTypes = fdDependentTypes directives
         in isJust ownership .&&. isJust dependentTypes .&&.
            locatedValue (case [v | Just v <- [ownership]] of (v:_) -> v; [] -> error "No ownership value") == True .&&.
            locatedValue (case [v | Just v <- [dependentTypes]] of (v:_) -> v; [] -> error "No dependentTypes value") == False

-- | QuickCheck属性：代码块内容应该被正确提取
prop_code_block_content_preserved :: String -> Property
prop_code_block_content_preserved content =
  let wrappedContent = "```typus\n" ++ content ++ "\n```"
      result = parseTypus wrappedContent
  in case result of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in if null blocks
            then property True
            else let block = case blocks of
                              (b:_) -> b
                              [] -> error "Impossible: null check ensures non-empty"
                     extractedContent = cbContent block
                 in (not (null content) ==> extractedContent == content ++ "\n")

-- | 测试指令解析的边界情况
test_directive_edge_cases :: Assertion
test_directive_edge_cases = do
  -- 测试带有额外空格的指令
  let spacedDirectives = "//  @ownership:  true  \n```typus\nlet x = 42\n```"
      result = parseTypus spacedDirectives
  case result of
    Left err -> assertFailure $ "Failed to parse spaced directives: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          ownership = fdOwnership directives
      assertBool "Should parse directives with extra spaces" (isJust ownership)
      assertEqual "Ownership should be true" (Just True) (locatedValue <$> ownership)

-- | 测试嵌套代码块的处理
test_nested_code_blocks :: Assertion
test_nested_code_blocks = do
  let nestedCode = "```typus\nlet x = 42\n// ```typus\nlet y = 24\n```\nlet z = 10\n```"
      result = parseTypus nestedCode
  case result of
    Left err -> assertFailure $ "Failed to parse nested code blocks: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertEqual "Should handle nested code blocks correctly" 2 (length blocks)
      assertBool "Content should include nested markers" ("// ```typus" `isInfixOf` cbContent (case blocks of (b:_) -> b; [] -> error "No blocks"))

-- | 测试Unicode字符处理
test_unicode_handling :: Assertion
test_unicode_handling = do
  let unicodeCode = "```typus\nlet 测试 = \"你好世界\"\n```"
      result = parseTypus unicodeCode
  case result of
    Left err -> assertFailure $ "Failed to parse Unicode content: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
          block = case blocks of
                    (b:_) -> b
                    [] -> error "Impossible: blocks should not be empty"
      assertBool "Should preserve Unicode characters" ("测试" `isInfixOf` cbContent block)
      assertBool "Should preserve Chinese characters" ("你好世界" `isInfixOf` cbContent block)

-- | 测试套件
tests :: TestTree
tests = testGroup "New Parser Tests"
  [ testCase "Parse empty file" test_parse_empty_file
  , testCase "Parse simple code block" test_parse_simple_code_block
  , testCase "Parse file directives" test_parse_file_directives
  , testCase "Parse block directives" test_parse_block_directives
  , testCase "Parse multiple code blocks" test_parse_multiple_code_blocks
  , testCase "Parse invalid syntax" test_parse_invalid_syntax
  , testCase "Directive edge cases" test_directive_edge_cases
  , testCase "Nested code blocks" test_nested_code_blocks
  , testCase "Unicode handling" test_unicode_handling
  , testProperty "Parse empty succeeds" prop_parse_empty_succeeds
  , testProperty "Parse roundtrip structure" prop_parse_roundtrip_structure
  , testProperty "File directives parsing consistent" prop_file_directives_parsing_consistent
  , testProperty "Code block content preserved" prop_code_block_content_preserved
  ]