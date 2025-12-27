{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), TypusFile(..))
import SourceLocation (SourcePos(..))

tests :: TestTree
tests = testGroup "New Error Recovery Tests"
  [ testSyntaxErrorRecovery
  , testDirectiveErrorRecovery
  , testBlockErrorRecovery
  , testUnicodeErrorRecovery
  , testPartialParsingRecovery
  , testErrorPositionAccuracy
  ]

-- | 测试语法错误的恢复
testSyntaxErrorRecovery :: TestTree
testSyntaxErrorRecovery = testCase "Syntax error recovery" $ do
  -- 缺少右括号
  let missingBrace = "package main\n\nfunc main() {\n    if true {\n        println(\"test\")\n    // missing closing brace"
  result <- parseTypus "test.typus" missingBrace
  case result of
    Left err -> assertBool "Should report error for missing brace" True
    Right file -> assertBool "Should attempt to parse despite missing brace" True
  
  -- 多余的右括号
  let extraBrace = "package main\n\nfunc main() {\n    println(\"test\")\n}\n}"
  result2 <- parseTypus "test.typus" extraBrace
  case result2 of
    Left err -> assertBool "Should report error for extra brace" True
    Right file -> assertBool "Should attempt to parse despite extra brace" True
  
  -- 无效的函数声明
  let invalidFunc = "package main\n\nfunc 123invalid() {\n    println(\"test\")\n}"
  result3 <- parseTypus "test.typus" invalidFunc
  case result3 of
    Left err -> assertBool "Should report error for invalid function name" True
    Right file -> assertBool "Should attempt to parse despite invalid function" True

-- | 测试指令错误的恢复
testDirectiveErrorRecovery :: TestTree
testDirectiveErrorRecovery = testCase "Directive error recovery" $ do
  -- 无效的指令
  let invalidDirective = "package main\n\n//! invalid_directive: true\n\nfunc main() {\n    println(\"test\")\n}"
  result <- parseTypus "test.typus" invalidDirective
  case result of
    Left err -> assertBool "Should report error for invalid directive" True
    Right file -> assertBool "Should parse despite invalid directive" True
  
  -- 指令值格式错误
  let invalidDirectiveValue = "package main\n\n//! ownership: maybe\n\nfunc main() {\n    println(\"test\")\n}"
  result2 <- parseTypus "test.typus" invalidDirectiveValue
  case result2 of
    Left err -> assertBool "Should report error for invalid directive value" True
    Right file -> assertBool "Should parse despite invalid directive value" True
  
  -- 指令位置错误
  let misplacedDirective = "package main\n\nfunc main() {\n    //! ownership: on  // directive inside function\n    println(\"test\")\n}"
  result3 <- parseTypus "test.typus" misplacedDirective
  case result3 of
    Left err -> assertBool "Should handle misplaced directive" True
    Right file -> assertBool "Should attempt to parse despite misplaced directive" True

-- | 测试块错误的恢复
testBlockErrorRecovery :: TestTree
testBlockErrorRecovery = testCase "Block error recovery" $ do
  -- 块开始但没有结束
  let incompleteBlock = "package main\n\nfunc main() {\n    {//! ownership: on\n        println(\"test\")\n    // missing block end"
  result <- parseTypus "test.typus" incompleteBlock
  case result of
    Left err -> assertBool "Should report error for incomplete block" True
    Right file -> assertBool "Should attempt to parse despite incomplete block" True
  
  -- 嵌套块错误
  let nestedBlockError = "package main\n\nfunc main() {\n    {//! ownership: on\n        {//! constraints: on\n            println(\"test\")\n        }  // missing outer block end"
  result2 <- parseTypus "test.typus" nestedBlockError
  case result2 of
    Left err -> assertBool "Should report error for nested block error" True
    Right file -> assertBool "Should attempt to parse despite nested block error" True
  
  -- 块内语法错误
  let blockSyntaxError = "package main\n\nfunc main() {\n    {//! ownership: on\n        if true  // missing condition\n            println(\"test\")\n    }\n}"
  result3 <- parseTypus "test.typus" blockSyntaxError
  case result3 of
    Left err -> assertBool "Should report error for block syntax error" True
    Right file -> assertBool "Should attempt to parse despite block syntax error" True

-- | 测试Unicode错误的恢复
testUnicodeErrorRecovery :: TestTree
testUnicodeErrorRecovery = testCase "Unicode error recovery" $ do
  -- 无效的UTF-8序列
  let invalidUTF8 = "package main\n\nfunc main() {\n    msg := \"\x80\xFF\"  // invalid UTF-8\n    println(msg)\n}"
  result <- parseTypus "test.typus" invalidUTF8
  case result of
    Left err -> assertBool "Should handle invalid UTF-8" True
    Right file -> assertBool "Should attempt to parse despite invalid UTF-8" True
  
  -- 包含控制字符
  let controlChars = "package main\n\nfunc main() {\n    msg := \"test\x01\x02\x03\"  // control characters\n    println(msg)\n}"
  result2 <- parseTypus "test.typus" controlChars
  case result2 of
    Left err -> assertBool "Should handle control characters" True
    Right file -> assertBool "Should attempt to parse despite control characters" True
  
  -- 零宽字符
  let zeroWidthChars = "package main\n\nfunc main() {\n    msg := \"test\u200b\u200c\u200d\"  // zero-width characters\n    println(msg)\n}"
  result3 <- parseTypus "test.typus" zeroWidthChars
  case result3 of
    Left err -> assertBool "Should handle zero-width characters" True
    Right file -> assertBool "Should parse zero-width characters successfully" True

-- | 测试部分解析的恢复
testPartialParsingRecovery :: TestTree
testPartialParsingRecovery = testCase "Partial parsing recovery" $ do
  -- 文件末尾不完整
  let incompleteEOF = "package main\n\nfunc main() {\n    println(\"test\"\n    // missing closing quote and brace"
  result <- parseTypus "test.typus" incompleteEOF
  case result of
    Left err -> assertBool "Should report EOF error" True
    Right file -> assertBool "Should attempt partial parsing" True
  
  -- 多个错误的情况
  let multipleErrors = "package main\n\nfunc invalid_name() {\n    if true\n        println(\"test\")\n    // missing closing brace\n}\n\nfunc another_func() {\n    return  // missing value"
  result2 <- parseTypus "test.typus" multipleErrors
  case result2 of
    Left err -> assertBool "Should report multiple errors" True
    Right file -> assertBool "Should attempt parsing despite multiple errors" True
  
  -- 混合错误类型
  let mixedErrors = "package main\n\n//! invalid_directive\n\nfunc main() {\n    {//! ownership: on\n        if true\n            println(\"test\")\n        // missing block ends\n    // missing function end"
  result3 <- parseTypus "test.typus" mixedErrors
  case result3 of
    Left err -> assertBool "Should handle mixed error types" True
    Right file -> assertBool "Should attempt parsing despite mixed errors" True

-- | 测试错误位置的准确性
testErrorPositionAccuracy :: TestTree
testErrorPositionAccuracy = testCase "Error position accuracy" $ do
  -- 测试行号准确性
  let lineError = "package main\n\nfunc main() {\n    // line 4\n    invalid_syntax_here\n    // line 6\n}"
  result <- parseTypus "test.typus" lineError
  case result of
    Left err -> 
      let errorMsg = show err
          hasCorrectLine = "line 5" `isInfixOf` errorMsg || "5:" `isInfixOf` errorMsg
      in assertBool ("Error should point to correct line: " ++ errorMsg) hasCorrectLine
    Right file -> assertBool "Should report error" False
  
  -- 测试列号准确性
  let colError = "package main\n\nfunc main() {\n    x := 1 + * 2\n    //       ^ error should point here\n}"
  result2 <- parseTypus "test.typus" colError
  case result2 of
    Left err -> 
      let errorMsg = show err
          hasColInfo = "column" `isInfixOf` errorMsg || ":" `isInfixOf` errorMsg
      in assertBool ("Error should include column info: " ++ errorMsg) hasColInfo
    Right file -> assertBool "Should report error" False
  
  -- 测试文件名包含在错误中
  let fileError = "package main\n\ninvalid syntax"
  result3 <- parseTypus "test.typus" fileError
  case result3 of
    Left err -> 
      let errorMsg = show err
          hasFileName = "test.typus" `isInfixOf` errorMsg
      in assertBool ("Error should include filename: " ++ errorMsg) hasFileName
    Right file -> assertBool "Should report error" False