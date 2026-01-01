{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewUtilsEdgeCaseSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Utils (trim, splitBy, splitByComma, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation)

tests :: TestTree
tests = testGroup "New Utils Edge Cases Tests"
  [ testTrimEdgeCases
  , testSplitByEdgeCases
  , testCommentHandlingEdgeCases
  , testIndentationEdgeCases
  ]

-- | 测试 trim 函数的边界情况
testTrimEdgeCases :: TestTree
testTrimEdgeCases = testCase "trim function edge cases" $ do
  -- 空字符串
  assertEqual "empty string" "" (trim "")
  
  -- 只有空白字符
  assertEqual "only spaces" "" (trim "   ")
  assertEqual "only tabs" "" (trim "\t\t")
  assertEqual "only newlines" "" (trim "\n\n")
  assertEqual "mixed whitespace" "" (trim " \t\n \t \n ")
  
  -- 单个字符
  assertEqual "single char" "a" (trim "a")
  assertEqual "single char with spaces" "a" (trim " a ")
  assertEqual "single char with tabs" "a" (trim "\ta\t")
  
  -- Unicode字符
  assertEqual "unicode chars" "中文测试" (trim "  中文测试  ")
  assertEqual "emoji" "🚀" (trim " 🚀 ")
  
  -- 特殊空白字符（Unicode空格）
  assertEqual "unicode spaces" "test" (trim "\x2000test\x2000")  -- U+2000 EN QUAD

-- | 测试 splitBy 函数的边界情况
testSplitByEdgeCases :: TestTree
testSplitByEdgeCases = testCase "splitBy function edge cases" $ do
  -- 空字符串
  assertEqual "empty string" [""] (splitBy ',' "")
  
  -- 只有分隔符
  assertEqual "only delimiter" ["", ""] (splitBy ',' ",")
  assertEqual "only multiple delimiters" ["", "", "", ""] (splitBy ',' ",,,")
  
  -- 无分隔符
  assertEqual "no delimiter" ["abc"] (splitBy ',' "abc")
  
  -- 开头和结尾的分隔符
  assertEqual "leading L.and trailing" ["", "a", "b", ""] (splitBy ',a,b,')
  
  -- 连续分隔符
  assertEqual "consecutive delimiters" ["a", "", "", "b"] (splitBy ',a,,,b,')
  
  -- 使用splitByCollapsed测试
  assertEqual "collapsed empty" [] (splitByCollapsed ',')
  assertEqual "collapsed consecutive" ["a", "b"] (splitByCollapsed ',a,,,b,')
  
  -- Unicode分隔符
  assertEqual "unicode delimiter" ["a", "b"] (splitBy '，' "a，b")  -- 中文逗号

-- | 测试注释处理函数的边界情况
testCommentHandlingEdgeCases :: TestTree
testCommentHandlingEdgeCases = testCase "comment handling edge cases" $ do
  -- 空字符串
  assertEqual "removeLineComments empty" "" (removeLineComments "")
  assertEqual "removeComments empty" "" (removeComments "")
  
  -- 没有注释
  assertEqual "no line comments" "code line" (removeLineComments "code line")
  assertEqual "no block comments" "code line" (removeComments "code line")
  
  -- 只有注释
  assertEqual "only line comment" "" (removeLineComments "// comment")
  assertEqual "only block comment" "" (removeComments "/* comment */")
  
  -- 注释在开头
  assertEqual "comment at start" " code" (removeLineComments "// comment\n code")
  assertEqual "block comment at start" " code" (removeComments "/* comment */ code")
  
  -- 字符串中的注释符号不应该被移除
  assertEqual "comment in string" "\"// not a comment\"" (removeLineComments "\"// not a comment\"")
  assertEqual "block comment in string" "\"/* not a comment */\"" (removeComments "\"/* not a comment */\"")
  
  -- 嵌套块注释（如果支持）
  assertEqual "nested comments" " code" (removeComments "/* outer /* inner */ */ code")

-- | 测试缩进处理函数的边界情况
testIndentationEdgeCases :: TestTree
testIndentationEdgeCases = testCase "indentation handling edge cases" $ do
  -- 空字符串
  assertEqual "normalize empty" "" (normalizeIndentation "")
  
  -- 没有缩进
  assertEqual "no indentation" "line1\nline2" (normalizeIndentation "line1\nline2")
  
  -- 混合缩进字符
  assertEqual "mixed indentation" "line1\n  line2\n\tline3" 
    (normalizeIndentation "  line1\n    line2\n\t\tline3")
  
  -- 只有空行
  assertEqual "only empty lines" "\n\n" (normalizeIndentation "  \n\t\n  \t")
  
  -- Unicode空格
  assertEqual "unicode spaces" "line1\nline2" 
    (normalizeIndentation "\x2000line1\n\x2000\x2000line2")
  
  -- 制表符和空格混合
  let mixed = "\tline1\n  \tline2\n\t  line3"
  let expected = "line1\n  line2\n  line3"
  assertEqual "tab space mixing" expected (normalizeIndentation mixed)