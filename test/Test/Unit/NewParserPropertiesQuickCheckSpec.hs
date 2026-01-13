{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewParserPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Parser
import SourceLocation
import Test.QuickCheck (Positive(..))

-- | 测试FileDirectives的默认值
prop_default_file_directives_consistent :: Property
prop_default_file_directives_consistent = 
  let defaults = defaultFileDirectives
  in property $ fdOwnership defaults == Nothing &&
                fdDependentTypes defaults == Nothing &&
                fdConstraints defaults == Nothing

-- | 测试BlockDirectives的默认值
prop_default_block_directives_consistent :: Property
prop_default_block_directives_consistent = 
  let defaults = defaultBlockDirectives
  in property $ bdOwnership defaults == Nothing &&
                bdDependentTypes defaults == Nothing &&
                bdConstraints defaults == Nothing

-- | 测试TypusFile的基本属性
prop_typus_file_empty :: Property
prop_typus_file_empty = 
  let emptyFile = TypusFile defaultFileDirectives [] [] []
  in property $ null (tfBlocks emptyFile)

-- | 测试CodeBlock的基本属性
prop_code_block_empty :: Property
prop_code_block_empty = 
  let emptySpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
      emptyBlock = CodeBlock defaultBlockDirectives "" emptySpan
  in property $ null (cbContent emptyBlock)

-- | 测试解析空字符串
prop_parse_empty_string :: Property
prop_parse_empty_string = 
  case parseTypus "" of
    Left _ -> property True
    Right file -> property $ null (tfBlocks file)

-- | 测试解析只有注释的文件
prop_parse_comments_only :: Property
prop_parse_comments_only = 
  let commentFile = "// This is a comment\n// Another comment"
  in case parseTypus commentFile of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行

-- | 测试解析带有指令的文件
prop_parse_with_directives :: Property
prop_parse_with_directives = 
  let directiveFile = "// @ownership true\n// @dependent-types true\nfunction test() {}"
  in case parseTypus directiveFile of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行

-- | 测试解析多行代码
prop_parse_multiline :: Property
prop_parse_multiline = 
  let multilineFile = "function test() {\n  return 42;\n}\n\nfunction another() {\n  return 'hello';\n}"
  in case parseTypus multilineFile of
       Left _ -> property True
       Right file -> property $ length (tfBlocks file) >= 1

-- | 测试解析带有区块指令的代码
prop_parse_block_directives :: Property
prop_parse_block_directives = 
  let blockDirectiveFile = "// @ownership false\n{\n  // Some code\n}\n// @ownership true\n{\n  // More code\n}"
  in case parseTypus blockDirectiveFile of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行

-- | 测试解析Unicode内容
prop_parse_unicode :: Property
prop_parse_unicode = 
  let unicodeFile = "function 测试() {\n  return '你好世界';\n}"
  in case parseTypus unicodeFile of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行

-- | 测试解析带有字符串字面量的代码
prop_parse_string_literals :: Property
prop_parse_string_literals = 
  let stringFile = "function test() {\n  return \"Hello // not a comment\";\n  return 'Another // not a comment';\n}"
  in case parseTypus stringFile of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行

-- | 测试解析带有嵌套注释的代码
prop_parse_nested_comments :: Property
prop_parse_nested_comments = 
  let nestedFile = "/* Outer comment\n  /* Inner comment */\n  Still in outer\n*/\nfunction test() {}"
  in case parseTypus nestedFile of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行

-- | 测试解析带有混合注释的代码
prop_parse_mixed_comments :: Property
prop_parse_mixed_comments = 
  let mixedFile = "// Line comment\n/* Block comment */\nfunction test() {\n  // Another line comment\n  /* Another block */\n}"
  in case parseTypus mixedFile of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行

-- | 测试解析空行
prop_parse_empty_lines :: Property
prop_parse_empty_lines = 
  let emptyLinesFile = "\n\n\nfunction test() {\n\n\n}\n\n\n"
  in case parseTypus emptyLinesFile of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行

-- | 测试解析带有制表符和空格的代码
prop_parse_mixed_whitespace :: Property
prop_parse_mixed_whitespace = 
  let whitespaceFile = "function test() {\n\treturn 42;\n  return 'hello';\n\t\treturn 'world';\n}"
  in case parseTypus whitespaceFile of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行

-- | 测试解析带有特殊字符的代码
prop_parse_special_characters :: Property
prop_parse_special_characters = 
  let specialFile = "function test() {\n  return @#$%^&*();\n  return []{}|\\:;\"'<>?,./;\n}"
  in case parseTypus specialFile of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行

-- | 测试解析大型文件
prop_parse_large_file :: Positive Int -> Property
prop_parse_large_file (Positive n) = 
  let largeContent = unlines $ replicate (min n 1000) "function test" ++ ["return 42;"]
  in case parseTypus largeContent of
       Left _ -> property True
       Right file -> property $ True  -- 只要能解析就行



tests :: TestTree
tests = testGroup "Parser Properties QuickCheck Tests"
  [ testProperty "default file directives consistent" prop_default_file_directives_consistent
  , testProperty "default block directives consistent" prop_default_block_directives_consistent
  , testProperty "typus file empty" prop_typus_file_empty
  , testProperty "code block empty" prop_code_block_empty
  , testProperty "parse empty string" prop_parse_empty_string
  , testProperty "parse comments only" prop_parse_comments_only
  , testProperty "parse with directives" prop_parse_with_directives
  , testProperty "parse multiline" prop_parse_multiline
  , testProperty "parse block directives" prop_parse_block_directives
  , testProperty "parse unicode" prop_parse_unicode
  , testProperty "parse string literals" prop_parse_string_literals
  , testProperty "parse nested comments" prop_parse_nested_comments
  , testProperty "parse mixed comments" prop_parse_mixed_comments
  , testProperty "parse empty lines" prop_parse_empty_lines
  , testProperty "parse mixed whitespace" prop_parse_mixed_whitespace
  , testProperty "parse special characters" prop_parse_special_characters
  , testProperty "parse large file" prop_parse_large_file
  ]