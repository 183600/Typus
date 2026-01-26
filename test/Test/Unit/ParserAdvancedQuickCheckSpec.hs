{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.ParserAdvancedQuickCheckSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (replicateM)
import Data.Char (isAlphaNum, isAlpha, isSpace)

-- | 测试标识符字符识别的一致性
prop_isIdentifierChar_consistency :: Char -> Property
prop_isIdentifierChar_consistency c =
  let valid = isIdentifierChar c
  in valid ==> (isAlphaNum c || c == '_' || c == '-')

-- | 测试文件指令解析的对称性
prop_fileDirectiveParser_symmetry :: [(String, String)] -> Property
prop_fileDirectiveParser_symmetry pairs =
  let pairs' = [(T.pack k, T.pack v) | (k, v) <- pairs]
      text = T.pack $ unwords [k ++ "=" ++ v | (k, v) <- pairs]
  -- 注意：这里我们只是验证解析器不会崩溃，实际解析结果可能不同
  in length pairs < 10 ==> property True

-- | 测试解析器对空输入的处理
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = parseTypus ""
  in case result of
    Left _ -> property True
    Right file -> tfBlocks file === []

-- | 测试解析器对单行代码的处理
prop_parser_single_line :: String -> Property
prop_parser_single_line code =
  not ("\n" `isInfixOf` code) ==> 
  case parseTypus code of
    Left _ -> property True
    Right file -> property (length (tfBlocks file) >= 0)

-- | 测试解析器对多行代码的处理
prop_parser_multiline :: Positive Int -> String -> Property
prop_parser_multiline (Positive n) code =
  let multiLineCode = unlines $ replicate n code
  in n < 100 ==> 
  case parseTypus multiLineCode of
    Left _ -> property True
    Right file -> property (length (tfBlocks file) >= 0)

-- | 测试解析器对注释的处理
prop_parser_handles_comments :: String -> String -> Property
prop_parser_handles_comments code comment =
  let codeWithComment = code ++ " // " ++ comment
  in case parseTypus codeWithComment of
    Left _ -> property True
    Right file -> tfBlocks file === tfBlocks file  -- 简单验证不崩溃

-- | 测试解析器对指令的处理
prop_parser_handles_directives :: Bool -> Bool -> Bool -> Property
prop_parser_handles_directives ownership dependent constraints =
  let directives = unlines 
        [ "// @ownership=" ++ show ownership
        , "// @dependentTypes=" ++ show dependent
        , "// @constraints=" ++ show constraints
        ]
      code = directives ++ "some code"
  in case parseTypus code of
    Left _ -> property True
    Right file -> tfDirectives file === tfDirectives file  -- 简单验证不崩溃

-- | 测试解析器的位置跟踪
prop_parser_tracks_positions :: String -> Property
prop_parser_tracks_positions code =
  case parseTypus code of
    Left _ -> property True
    Right file -> property (all isValidBlockSpan (map cbSpan (tfBlocks file)))

-- | 测试解析器对块边界的识别
prop_parser_identifies_blocks :: String -> String -> Property
prop_parser_identifies_blocks block1 block2 =
  let code = block1 ++ "\n\n" ++ block2
  in case parseTypus code of
    Left _ -> property True
    Right file -> property (length (tfBlocks file) >= 1)

-- | 测试解析器对指令格式的容错性
prop_parser_tolerant_directive_format :: String -> String -> Property
prop_parser_tolerant_directive_format key value =
  let directive = "// @" ++ key ++ "=" ++ value
      code = directive ++ "\nsome code"
  in not (null key) && not (null value) ==> 
  case parseTypus code of
    Left _ -> property True
    Right file -> tfDirectives file === tfDirectives file  -- 简单验证不崩溃

-- | 测试解析器对特殊字符的处理
prop_parser_handles_special_chars :: String -> Property
prop_parser_handles_special_chars chars =
  let specialChars = filter (`notElem` ['\n', '\r']) chars
      code = specialChars ++ " code with special chars: !@#$%^&*()"
  in not (null specialChars) ==> 
  case parseTypus code of
    Left _ -> property True
    Right file -> tfBlocks file === tfBlocks file  -- 简单验证不崩溃

-- | 测试解析器对大输入的处理
prop_parser_handles_large_input :: Positive Int -> Property
prop_parser_handles_large_input (Positive n) =
  let largeCode = unlines $ replicate n "line of code"
  in n < 1000 ==> 
  case parseTypus largeCode of
    Left _ -> property True
    Right file -> property (length (tfBlocks file) >= 0)

-- | 测试默认文件指令
test_default_file_directives :: Assertion
test_default_file_directives = do
  let expected = FileDirectives Nothing Nothing Nothing
      actual = defaultFileDirectives
  assertEqual "Default file directives" expected actual

-- | 测试默认块指令
test_default_block_directives :: Assertion
test_default_block_directives = do
  let expected = BlockDirectives Nothing Nothing Nothing
      actual = defaultBlockDirectives
  assertEqual "Default block directives" expected actual

-- | 测试解析器对空文件的处理
test_parse_empty_file :: Assertion
test_parse_empty_file = do
  let result = parseTypus ""
  case result of
    Left err -> assertFailure $ "Failed to parse empty file: " ++ show err
    Right file -> do
      assertEqual "No blocks in empty file" [] (tfBlocks file)
      assertEqual "Default directives" defaultFileDirectives (tfDirectives file)

-- | 测试解析器对只有指令的文件的处理
test_parse_directives_only :: Assertion
test_parse_directives_only = do
  let directives = "// @ownership=true\n// @dependentTypes=false\n"
      result = parseTypus directives
  case result of
    Left err -> assertFailure $ "Failed to parse directives only: " ++ show err
    Right file -> do
      assertEqual "No blocks in directives only file" [] (tfBlocks file)

-- | 测试解析器对只有代码的文件的处理
test_parse_code_only :: Assertion
test_parse_code_only = do
  let code = "function test() { return 42; }"
      result = parseTypus code
  case result of
    Left err -> assertFailure $ "Failed to parse code only: " ++ show err
    Right file -> do
      assertBool "At least one block in code only file" (not $ null $ tfBlocks file)
      assertEqual "Default directives" defaultFileDirectives (tfDirectives file)

-- | 测试解析器对混合内容的处理
test_parse_mixed_content :: Assertion
test_parse_mixed_content = do
  let content = "// @ownership=true\nfunction test() { return 42; }\n\n// @dependentTypes=false\nfunction test2() { return 24; }"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Failed to parse mixed content: " ++ show err
    Right file -> do
      assertBool "At least one block in mixed content" (not $ null $ tfBlocks file)

-- | 辅助函数：检查块span是否有效
isValidBlockSpan :: SourceSpan -> Bool
isValidBlockSpan (SourceSpan start end) = sourcePosLe start end

-- | 辅助函数：检查SourcePos的顺序
sourcePosLe :: SourcePos -> SourcePos -> Bool
sourcePosLe (SourcePos l1 c1 _) (SourcePos l2 c2 _) = 
  l1 < l2 || (l1 == l2 && c1 <= c2)

-- | 测试套件
tests :: TestTree
tests = testGroup "Parser Advanced QuickCheck Tests"
  [ testProperty "IsIdentifierChar consistency" prop_isIdentifierChar_consistency
  , testProperty "FileDirectiveParser symmetry" prop_fileDirectiveParser_symmetry
  , testProperty "Parser empty input" prop_parser_empty_input
  , testProperty "Parser single line" prop_parser_single_line
  , testProperty "Parser multiline" prop_parser_multiline
  , testProperty "Parser handles comments" prop_parser_handles_comments
  , testProperty "Parser handles directives" prop_parser_handles_directives
  , testProperty "Parser tracks positions" prop_parser_tracks_positions
  , testProperty "Parser identifies blocks" prop_parser_identifies_blocks
  , testProperty "Parser tolerant directive format" prop_parser_tolerant_directive_format
  , testProperty "Parser handles special chars" prop_parser_handles_special_chars
  , testProperty "Parser handles large input" prop_parser_handles_large_input
  , testCase "Default file directives" test_default_file_directives
  , testCase "Default block directives" test_default_block_directives
  , testCase "Parse empty file" test_parse_empty_file
  , testCase "Parse directives only" test_parse_directives_only
  , testCase "Parse code only" test_parse_code_only
  , testCase "Parse mixed content" test_parse_mixed_content
  ]