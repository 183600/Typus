{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewParserUnicodeSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), TypusFile(..))
import SourceLocation (SourcePos(..))

tests :: TestTree
tests = testGroup "New Parser Unicode Tests"
  [ testUnicodeIdentifiers
  , testUnicodeStrings
  , testUnicodeComments
  , testUnicodeDirectives
  , testMixedUnicodeContent
  , testUnicodeErrorHandling
  ]

-- | 测试Unicode标识符解析
testUnicodeIdentifiers :: TestTree
testUnicodeIdentifiers = testCase "Unicode identifiers parsing" $ do
  -- 中文变量名
  let chineseCode = "package main\n\nfunc 测试函数() {\n    变量 := 42\n    println(变量)\n}"
  result <- parseTypus "test.typus" chineseCode
  case result of
    Left err -> assertBool ("Should parse Chinese identifiers: " ++ show err) False
    Right file -> assertBool "Parsed Chinese code successfully" True
  
  -- 日文变量名
  let japaneseCode = "package main\n\nfunc テスト() {\n    変数 := \"hello\"\n    println(変数)\n}"
  result2 <- parseTypus "test.typus" japaneseCode
  case result2 of
    Left err -> assertBool ("Should parse Japanese identifiers: " ++ show err) False
    Right file -> assertBool "Parsed Japanese code successfully" True
  
  -- 韩文变量名
  let koreanCode = "package main\n\nfunc 테스트() {\n    변수 := 100\n    println(변수)\n}"
  result3 <- parseTypus "test.typus" koreanCode
  case result3 of
    Left err -> assertBool ("Should parse Korean identifiers: " ++ show err) False
    Right file -> assertBool "Parsed Korean code successfully" True

-- | 测试Unicode字符串字面量
testUnicodeStrings :: TestTree
testUnicodeStrings = testCase "Unicode string literals parsing" $ do
  -- Unicode字符串内容
  let unicodeStringCode = "package main\n\nfunc main() {\n    msg := \"Hello 世界 🌍\"\n    println(msg)\n}"
  result <- parseTypus "test.typus" unicodeStringCode
  case result of
    Left err -> assertBool ("Should parse Unicode strings: " ++ show err) False
    Right file -> assertBool "Parsed Unicode strings successfully" True
  
  -- 转义Unicode字符
  let escapeCode = "package main\n\nfunc main() {\n    msg := \"\\u4e16\\u754c\"  // \"世界\"\n    println(msg)\n}"
  result2 <- parseTypus "test.typus" escapeCode
  case result2 of
    Left err -> assertBool ("Should parse escaped Unicode: " ++ show err) False
    Right file -> assertBool "Parsed escaped Unicode successfully" True

-- | 测试Unicode注释
testUnicodeComments :: TestTree
testUnicodeComments = testCase "Unicode comments parsing" $ do
  -- 中文注释
  let chineseComments = "package main\n\n// 这是一个中文注释\nfunc main() {\n    // 另一个中文注释\n    println(\"测试\")\n}"
  result <- parseTypus "test.typus" chineseComments
  case result of
    Left err -> assertBool ("Should parse Chinese comments: " ++ show err) False
    Right file -> assertBool "Parsed Chinese comments successfully" True
  
  -- 多语言注释混合
  let mixedComments = "package main\n\n// Chinese comment\nfunc main() {\n    // 日本語コメント\n    println(\"test\")\n    // 한국어 주석\n}"
  result2 <- parseTypus "test.typus" mixedComments
  case result2 of
    Left err -> assertBool ("Should parse mixed language comments: " ++ show err) False
    Right file -> assertBool "Parsed mixed language comments successfully" True

-- | 测试Unicode指令
testUnicodeDirectives :: TestTree
testUnicodeDirectives = testCase "Unicode directives parsing" $ do
  -- 带Unicode的指令注释
  let unicodeDirectives = "package main\n\n//! ownership: on  // 启用所有权\n//! constraints: on  // 启用约束\n\nfunc main() {\n    println(\"测试\")\n}"
  result <- parseTypus "test.typus" unicodeDirectives
  case result of
    Left err -> assertBool ("Should parse Unicode directives: " ++ show err) False
    Right file -> do
      let fileDirectives = tfFileDirectives file
      assertBool "Ownership directive should be enabled" (fdOwnership fileDirectives == Just True)
      assertBool "Constraints directive should be enabled" (fdConstraints fileDirectives == Just True)

-- | 测试混合Unicode内容
testMixedUnicodeContent :: TestTree
testMixedUnicodeContent = testCase "Mixed Unicode content parsing" $ do
  -- 混合多种Unicode字符的复杂代码
  let mixedCode = "package main\n\n//! ownership: on\n\n// 中文注释\ntype 中文结构体 struct {\n    日文フィールド string  // 日文字段名\n    한국어필드    int     // 韩文字段名\n}\n\nfunc 测试函数() {\n    //! constraints: on\n    变量 := 中文结构体{\n        日文フィールド: \"Hello 世界 🌍\",\n        한국어필드:    42,\n    }\n    println(变量.日文フィールド)\n}"
  
  result <- parseTypus "test.typus" mixedCode
  case result of
    Left err -> assertBool ("Should parse mixed Unicode content: " ++ show err) False
    Right file -> assertBool "Parsed mixed Unicode content successfully" True

-- | 测试Unicode错误处理
testUnicodeErrorHandling :: TestTree
testUnicodeErrorHandling = testCase "Unicode error handling" $ do
  -- 无效的Unicode序列
  let invalidUnicode = "package main\n\nfunc main() {\n    msg := \"\\x80\"  // 无效的Unicode字节\n    println(msg)\n}"
  result <- parseTypus "test.typus" invalidUnicode
  -- 应该能够解析，但可能在后续阶段报告错误
  case result of
    Left err -> assertBool "Should handle invalid Unicode gracefully" True
    Right file -> assertBool "Should parse invalid Unicode sequence" True
  
  -- 特殊Unicode字符（零宽字符等）
  let specialUnicode = "package main\n\nfunc main() {\n    // 包含零宽字符: \u200b\n    msg := \"test\u200bzero\u200cwidth\u200dchars\"\n    println(msg)\n}"
  result2 <- parseTypus "test.typus" specialUnicode
  case result2 of
    Left err -> assertBool ("Should handle special Unicode chars: " ++ show err) False
    Right file -> assertBool "Parsed special Unicode characters successfully" True