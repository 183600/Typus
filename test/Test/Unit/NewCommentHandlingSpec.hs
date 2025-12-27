{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCommentHandlingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)

import Utils (removeLineComments, removeComments)
import Data.Char (chr)
import Data.List (isInfixOf)

tests :: TestTree
tests = testGroup "New Comment Handling Tests"
  [ testLineCommentBasic
  , testLineCommentInStrings
  , testLineCommentEdgeCases
  , testBlockCommentBasic
  , testBlockCommentInStrings
  , testBlockCommentNesting
  , testBlockCommentEdgeCases
  , testMixedComments
  , testCommentProperties
  ]

-- | 测试基本行注释处理
testLineCommentBasic :: TestTree
testLineCommentBasic = testCase "Basic line comment handling" $ do
  -- 简单行注释
  assertEqual "simple line comment" 
    "code line\n" (removeLineComments "code line\n// comment")
  
  -- 多行代码中的注释
  assertEqual "multiple lines with comments"
    "line1\nline2\nline3\n" 
    (removeLineComments "line1\nline2\n// comment\nline3\n// another comment")
  
  -- 行尾注释
  assertEqual "end of line comments"
    "code line\nanother line\n"
    (removeLineComments "code line // comment\nanother line // another comment")

-- | 测试字符串中的行注释不被移除
testLineCommentInStrings :: TestTree
testLineCommentInStrings = testCase "Line comments in strings" $ do
  -- 双引号字符串中的注释符号
  assertEqual "comment in double quotes"
    "\"// not a comment\"\n" 
    (removeLineComments "\"// not a comment\"\n")
  
  -- 单引号字符串中的注释符号
  assertEqual "comment in single quotes"
    "'// not a comment'\n"
    (removeLineComments "'// not a comment'\n")
  
  -- 转义的引号
  assertEqual "escaped quotes"
    "\"string with \\\" // not comment\"\n"
    (removeLineComments "\"string with \\\" // not comment\"\n")
  
  -- 复杂字符串情况
  let complexString = "code line\nstring := \"// comment inside \\\" // not comment\"\n// real comment\n"
  let expected = "code line\nstring := \"// comment inside \\\" // not comment\"\n"
  assertEqual "complex string case" expected (removeLineComments complexString)

-- | 测试行注释的边界情况
testLineCommentEdgeCases :: TestTree
testLineCommentEdgeCases = testCase "Line comment edge cases" $ do
  -- 空字符串
  assertEqual "empty string" "" (removeLineComments "")
  
  -- 只有注释
  assertEqual "only comments" "" (removeLineComments "// comment only\n// another comment")
  
  -- 连续的注释符号
  assertEqual "multiple comment markers" "" (removeLineComments "/// comment\n//// another")
  
  -- 注释符号在行首但无空格
  assertEqual "comment at start" "code\n" (removeLineComments "//comment\ncode")
  
  -- Unicode注释
  assertEqual "unicode comments" "code\n" (removeLineComments "code\n// 这是中文注释")

-- | 测试基本块注释处理
testBlockCommentBasic :: TestTree
testBlockCommentBasic = testCase "Basic block comment handling" $ do
  -- 简单块注释
  assertEqual "simple block comment"
    "code before\n code after\n" 
    (removeComments "code before\n/* comment */\n code after")
  
  -- 多行块注释
  assertEqual "multiline block comment"
    "before\n after\n"
    (removeComments "before\n/* line1\nline2\nline3 */\n after")
  
  -- 行内块注释
  assertEqual "inline block comment"
    "before  after\n"
    (removeComments "before /* comment */ after")

-- | 测试字符串中的块注释不被移除
testBlockCommentInStrings :: TestTree
testBlockCommentInStrings = testCase "Block comments in strings" $ do
  -- 双引号字符串中的块注释符号
  assertEqual "block comment in double quotes"
    "\"/* not a comment */\"\n"
    (removeComments "\"/* not a comment */\"\n")
  
  -- 单引号字符串中的块注释符号
  assertEqual "block comment in single quotes"
    "'/* not a comment */'\n"
    (removeComments "'/* not a comment */'\n")
  
  -- 转义字符
  assertEqual "escaped characters in strings"
    "\"string with \\\" /* not comment */\"\n"
    (removeComments "\"string with \\\" /* not comment */\"\n")

-- | 测试块注释嵌套（如果支持）
testBlockCommentNesting :: TestTree
testBlockCommentNesting = testCase "Block comment nesting" $ do
  -- 嵌套块注释
  let nested = "code /* outer /* inner */ still outer */ more code"
  let expected = "code  more code"
  result <- return $ removeComments nested
  assertBool "nested block comments" (expected == result)
  
  -- 多层嵌套
  let multiNested = "start /* level1 /* level2 /* level3 */ back2 */ back1 */ end"
  let expected2 = "start  end"
  result2 <- return $ removeComments multiNested
  assertBool "multi-level nested comments" (expected2 == result2)

-- | 测试块注释的边界情况
testBlockCommentEdgeCases :: TestTree
testBlockCommentEdgeCases = testCase "Block comment edge cases" $ do
  -- 空块注释
  assertEqual "empty block comment" "code code" (removeComments "code /**/ code")
  
  -- 只有块注释
  assertEqual "only block comment" "" (removeComments "/* only comment */")
  
  -- 不匹配的块注释（开始但无结束）
  let unmatchedOpen = "code /* comment\nmore code"
  result <- return $ removeComments unmatchedOpen
  assertBool "unmatched opening" ("code \nmore code" == result)
  
  -- 不匹配的块注释（结束但无开始）
  let unmatchedClose = "code comment */ more code"
  result2 <- return $ removeComments unmatchedClose
  assertEqual "unmatched closing" "code comment */ more code" result2

-- | 测试混合注释类型
testMixedComments :: TestTree
testMixedComments = testCase "Mixed comment types" $ do
  -- 行注释和块注释混合
  let mixed = "code // line comment\n/* block comment */\nmore code // another line"
  let expected = "code \n \nmore code "
  result <- return $ removeComments mixed
  assertEqual "mixed comment types" expected result
  
  -- 块注释中包含行注释符号
  let blockWithLine = "code /* // this is not a line comment */ more code"
  let expected2 = "code  more code"
  result2 <- return $ removeComments blockWithLine
  assertEqual "line comment inside block" expected2 result2
  
  -- 行注释中包含块注释符号
  let lineWithBlock = "code // /* this is not a block comment */\nmore code"
  let expected3 = "code \nmore code"
  result3 <- return $ removeComments lineWithBlock
  assertEqual "block comment inside line" expected3 result3

-- | QuickCheck 属性测试
testCommentProperties :: TestTree
testCommentProperties = testGroup "Comment Properties"
  [ testProperty "removeLineComments removes all line comments" $ \str ->
      let result = removeLineComments str
          hasLineComment = "//" `isInfixOf` result
      in not hasLineComment
      
  , testProperty "removeComments removes all block comments" $ \str ->
      let result = removeComments str
          hasBlockComment = "/*" `isInfixOf` result && "*/" `isInfixOf` result
      in not hasBlockComment
      
  , testProperty "removeLineComments preserves string literals" $ \str ->
      let simpleString = "\"// not a comment\""
          result = removeLineComments simpleString
      in result === simpleString
      
  , testProperty "removeComments preserves string literals" $ \str ->
      let simpleString = "\"/* not a comment */\""
          result = removeComments simpleString
      in result === simpleString
  ]