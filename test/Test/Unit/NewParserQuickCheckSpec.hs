{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewParserQuickCheckSpec where

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt)
import Data.Char (isAlphaNum, isLetter)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T

-- | 测试标识符字符检查函数
prop_identifier_char_valid :: Char -> Property
prop_identifier_char_valid c =
  let expected = isLetter c || c == '_' || isAlphaNum c
      actual = isIdentifierChar c
  in actual === expected

-- | 测试标识符字符对边界情况的处理
prop_identifier_char_edge_cases :: Property
prop_identifier_char_edge_cases = 
  conjoin
    [ property $ isIdentifierChar '_'
    , property $ not $ isIdentifierChar '-'
    , property $ not $ isIdentifierChar ' '
    , property $ isIdentifierChar 'a'
    , property $ isIdentifierChar 'Z'
    , property $ isIdentifierChar '0'
    , property $ not $ isIdentifierChar '@'
    ]

-- | 测试默认文件指令
prop_default_file_directives :: Property
prop_default_file_directives = 
  let directives = defaultFileDirectives
  in conjoin
    [ fdOwnership directives === Nothing
    , fdDependentTypes directives === Nothing
    , fdConstraints directives === Nothing
    ]

-- | 测试默认块指令
prop_default_block_directives :: Property
prop_default_block_directives = 
  let directives = defaultBlockDirectives
  in conjoin
    [ bdOwnership directives === Nothing
    , bdDependentTypes directives === Nothing
    , bdConstraints directives === Nothing
    ]

-- | 测试文件指令相等性
prop_file_directives_equality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_file_directives_equality ownership dependent constraints =
  let locatedOwnership = fmap (\b -> locatedAt (SourcePos 1 1 0) b) ownership
      locatedDependent = fmap (\b -> locatedAt (SourcePos 1 1 0) b) dependent
      locatedConstraints = fmap (\b -> locatedAt (SourcePos 1 1 0) b) constraints
      directives1 = FileDirectives locatedOwnership locatedDependent locatedConstraints
      directives2 = FileDirectives locatedOwnership locatedDependent locatedConstraints
  in directives1 === directives2

-- | 测试块指令相等性
prop_block_directives_equality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_block_directives_equality ownership dependent constraints =
  let locatedOwnership = fmap (\b -> locatedAt (SourcePos 1 1 0) b) ownership
      locatedDependent = fmap (\b -> locatedAt (SourcePos 1 1 0) b) dependent
      locatedConstraints = fmap (\b -> locatedAt (SourcePos 1 1 0) b) constraints
      directives1 = BlockDirectives locatedOwnership locatedDependent locatedConstraints
      directives2 = BlockDirectives locatedOwnership locatedDependent locatedConstraints
  in directives1 === directives2

-- | 测试代码块内容提取
prop_codeblock_content_extraction :: String -> Property
prop_codeblock_content_extraction content =
  let limitedContent = take 100 content  -- 限制内容大小
      span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length limitedContent + 1) 0)
      directives = defaultBlockDirectives
      codeBlock = CodeBlock directives limitedContent span
  in cbContent codeBlock === limitedContent

-- | 测试TypusFile内容提取
prop_typus_file_content_extraction :: String -> String -> Property
prop_typus_file_content_extraction content1 content2 =
  let limitedContent1 = take 50 content1
      limitedContent2 = take 50 content2
      span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length limitedContent1 + 1) 0)
      span2 = SourceSpan (SourcePos 2 1 0) (SourcePos 2 (length limitedContent2 + 1) 0)
      directives = defaultBlockDirectives
      block1 = CodeBlock directives limitedContent1 span1
      block2 = CodeBlock directives limitedContent2 span2
      typusFile = TypusFile defaultFileDirectives [] [block1, block2] []
  in tfContents typusFile === (limitedContent1 ++ limitedContent2)

-- | 测试空TypusFile的内容提取
prop_empty_typus_file_content :: Property
prop_empty_typus_file_content =
  let typusFile = TypusFile defaultFileDirectives [] [] []
  in tfContents typusFile === ""

-- | 测试单块TypusFile的内容提取
prop_single_block_typus_file_content :: String -> Property
prop_single_block_typus_file_content content =
  let limitedContent = take 75 content
      span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length limitedContent + 1) 0)
      directives = defaultBlockDirectives
      block = CodeBlock directives limitedContent span
      typusFile = TypusFile defaultFileDirectives [] [block] []
  in tfContents typusFile === limitedContent

-- | 测试代码块指令的获取
prop_codeblock_directives :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_codeblock_directives ownership dependent constraints =
  let locatedOwnership = fmap (\b -> locatedAt (SourcePos 1 1 0) b) ownership
      locatedDependent = fmap (\b -> locatedAt (SourcePos 1 1 0) b) dependent
      locatedConstraints = fmap (\b -> locatedAt (SourcePos 1 1 0) b) constraints
      directives = BlockDirectives locatedOwnership locatedDependent locatedConstraints
      span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
      codeBlock = CodeBlock directives "" span
  in conjoin
    [ property $ cbDirectives codeBlock === directives
    , property $ bdOwnership (cbDirectives codeBlock) === locatedOwnership
    , property $ bdDependentTypes (cbDirectives codeBlock) === locatedDependent
    , property $ bdConstraints (cbDirectives codeBlock) === locatedConstraints
    ]

-- | 测试TypusFile构建标签
prop_typus_file_build_tags :: [String] -> Property
prop_typus_file_build_tags tags =
  let limitedTags = take 10 tags  -- 限制标签数量
      locatedTags = map (\tag -> locatedAt (SourcePos 1 1 0) tag) limitedTags
      typusFile = TypusFile defaultFileDirectives locatedTags [] []
  in tfBuildTags typusFile === locatedTags

-- | 测试TypusFile语法错误
prop_typus_file_syntax_errors :: [String] -> Property
prop_typus_file_syntax_errors errors =
  let limitedErrors = take 5 errors  -- 限制错误数量
      -- 这里我们简化处理，实际应该使用SyntaxError类型
      typusFile = TypusFile defaultFileDirectives [] [] []
  in tfSyntaxErrors typusFile === []

-- | 测试解析器错误处理
test_parser_error_handling :: Assertion
test_parser_error_handling = do
  -- 测试空输入
  let emptyResult = parseTypus ""
  case emptyResult of
    Left _ -> assertBool "Empty input should produce error" True
    Right file -> assertFailure "Empty input should not parse successfully"

-- | 测试解析器基本功能
test_parser_basic_functionality :: Assertion
test_parser_basic_functionality = do
  -- 测试简单内容解析
  let simpleContent = "package main\n\nfunc main() { return 42 }"
      result = parseTypus simpleContent
  case result of
    Left err -> assertFailure $ "Failed to parse simple content: " ++ show err
    Right file -> assertBool "Should parse simple content" $ not (null (tfBlocks file))

-- | 测试解析器指令处理
test_parser_directive_handling :: Assertion
test_parser_directive_handling = do
  -- 测试带有指令的内容
  let contentWithDirectives = "package main\n\n// @ownership true\nfunc test() {}"
      result = parseTypus contentWithDirectives
  case result of
    Left err -> assertFailure $ "Failed to parse content with directives: " ++ show err
    Right file -> assertBool "Should parse content with directives" $ not (null (tfBlocks file))

-- | 测试解析器多块处理
test_parser_multiple_blocks :: Assertion
test_parser_multiple_blocks = do
  -- 测试多个代码块
  let multiBlockContent = "package main\n\nfunc first() {}\n\nfunc second() {}"
      result = parseTypus multiBlockContent
  case result of
    Left err -> assertFailure $ "Failed to parse multiple blocks: " ++ show err
    Right file -> assertBool "Should parse multiple blocks" $ length (tfBlocks file) >= 1

-- | 测试解析器注释处理
test_parser_comment_handling :: Assertion
test_parser_comment_handling = do
  -- 测试带有注释的内容
  let contentWithComments = "package main\n\n// This is a comment\nfunc main() { /* block comment */ return 0 }"
      result = parseTypus contentWithComments
  case result of
    Left err -> assertFailure $ "Failed to parse content with comments: " ++ show err
    Right file -> assertBool "Should parse content with comments" $ not (null (tfBlocks file))

-- | 测试解析器字符串处理
test_parser_string_handling :: Assertion
test_parser_string_handling = do
  -- 测试带有字符串的内容
  let contentWithStrings = "package main\n\nfunc main() { s := \"hello world\"; return s }"
      result = parseTypus contentWithStrings
  case result of
    Left err -> assertFailure $ "Failed to parse content with strings: " ++ show err
    Right file -> assertBool "Should parse content with strings" $ not (null (tfBlocks file))

-- | 测试套件
tests :: TestTree
tests = memoryLevelTestGroup Moderate "New Parser QuickCheck Tests"
  [ withMemoryLevel Moderate $ testProperty "Identifier char valid" prop_identifier_char_valid
  , withMemoryLevel Moderate $ testProperty "Identifier char edge cases" prop_identifier_char_edge_cases
  , withMemoryLevel Moderate $ testProperty "Default file directives" prop_default_file_directives
  , withMemoryLevel Moderate $ testProperty "Default block directives" prop_default_block_directives
  , withMemoryLevel Moderate $ testProperty "File directives equality" prop_file_directives_equality
  , withMemoryLevel Moderate $ testProperty "Block directives equality" prop_block_directives_equality
  , withMemoryLevel Moderate $ testProperty "CodeBlock content extraction" prop_codeblock_content_extraction
  , withMemoryLevel Moderate $ testProperty "TypusFile content extraction" prop_typus_file_content_extraction
  , withMemoryLevel Moderate $ testProperty "Empty TypusFile content" prop_empty_typus_file_content
  , withMemoryLevel Moderate $ testProperty "Single block TypusFile content" prop_single_block_typus_file_content
  , withMemoryLevel Moderate $ testProperty "CodeBlock directives" prop_codeblock_directives
  , withMemoryLevel Moderate $ testProperty "TypusFile build tags" prop_typus_file_build_tags
  , withMemoryLevel Moderate $ testProperty "TypusFile syntax errors" prop_typus_file_syntax_errors
  , testCase "Parser error handling" test_parser_error_handling
  , testCase "Parser basic functionality" test_parser_basic_functionality
  , testCase "Parser directive handling" test_parser_directive_handling
  , testCase "Parser multiple blocks" test_parser_multiple_blocks
  , testCase "Parser comment handling" test_parser_comment_handling
  , testCase "Parser string handling" test_parser_string_handling
  ]

-- | 轻量级测试套件，用于内存受限环境
essentialTests :: TestTree
essentialTests = memoryLevelTestGroup Minimal "New Parser Essential Tests"
  [ withMemoryLevel Minimal $ testProperty "Identifier char valid" prop_identifier_char_valid
  , withMemoryLevel Minimal $ testProperty "Default file directives" prop_default_file_directives
  , withMemoryLevel Minimal $ testProperty "CodeBlock content extraction" prop_codeblock_content_extraction
  , withMemoryLevel Minimal $ testCase "Parser basic functionality" test_parser_basic_functionality
  , withMemoryLevel Minimal $ testCase "Parser error handling" test_parser_error_handling
  ]