{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewIndentationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)

import Utils (normalizeIndentation, trim)
import Data.Char (isSpace)
import Data.List (findCommonPrefix)

tests :: TestTree
tests = testGroup "New Indentation Tests"
  [ testBasicIndentation
  , testMixedIndentation
  , testUnicodeIndentation
  , testEmptyAndWhitespaceLines
  , testComplexIndentationScenarios
  , testIndentationProperties
  ]

-- | 测试基本缩进处理
testBasicIndentation :: TestTree
testBasicIndentation = testCase "Basic indentation handling" $ do
  -- 简单的缩进
  let simple = "  line1\n    line2\n  line3"
  let expected = "line1\n  line2\nline3"
  assertEqual "simple indentation" expected (normalizeIndentation simple)
  
  -- 制表符缩进
  let tabs = "\tline1\n\t\tline2\n\tline3"
  let expected2 = "line1\n\tline2\nline3"
  assertEqual "tab indentation" expected2 (normalizeIndentation tabs)
  
  -- 无缩进
  let noIndent = "line1\nline2\nline3"
  assertEqual "no indentation" noIndent (normalizeIndentation noIndent)
  
  -- 单行
  let singleLine = "  single line"
  assertEqual "single line" "single line" (normalizeIndentation singleLine)

-- | 测试混合缩进字符
testMixedIndentation :: TestTree
testMixedIndentation = testCase "Mixed indentation characters" $ do
  -- 空格和制表符混合
  let mixed1 = "  \tline1\n    \tline2\n\t  line3"
  let expected1 = "line1\n  line2\nline3"
  result1 <- return $ normalizeIndentation mixed1
  assertBool "mixed spaces L.and tabs" (expected1 == result1)
  
  -- 不一致的混合缩进
  let mixed2 = "   line1\n\t line2\n  \tline3"
  let expected2 = "line1\n line2\nline3"
  result2 <- return $ normalizeIndentation mixed2
  assertBool "inconsistent mixed indentation" (expected2 == result2)
  
  -- 复杂混合情况
  let complex = "    first\n\t\tsecond\n  third\n\t    fourth"
  let expected3 = "first\n\tsecond\nthird\n  fourth"
  result3 <- return $ normalizeIndentation complex
  assertBool "complex mixed indentation" (expected3 == result3)

-- | 测试Unicode空格字符
testUnicodeIndentation :: TestTree
testUnicodeIndentation = testCase "Unicode whitespace indentation" $ do
  -- Unicode空格 U+2000 (EN QUAD)
  let unicodeSpace1 = "\x2000line1\n\x2000\x2000line2"
  let expected1 = "line1\n  line2"
  result1 <- return $ normalizeIndentation unicodeSpace1
  assertBool "Unicode EN QUAD spaces" (expected1 == result1)
  
  -- Unicode空格 U+00A0 (NO-BREAK SPACE)
  let unicodeSpace2 = "\x00A0line1\n\x00A0\x00A0line2"
  let expected2 = "line1\n  line2"
  result2 <- return $ normalizeIndentation unicodeSpace2
  assertBool "Unicode NO-BREAK spaces" (expected2 == result2)
  
  -- 混合Unicode和普通空格
  let mixedUnicode = " \x2000line1\n  \x00A0line2"
  let expected3 = "line1\n line2"
  result3 <- return $ normalizeIndentation mixedUnicode
  assertBool "mixed Unicode L.and regular spaces" (expected3 == result3)

-- | 测试空行和只有空白字符的行
testEmptyAndWhitespaceLines :: TestTree
testEmptyAndWhitespaceLines = testCase "Empty L.and whitespace-only lines" $ do
  -- 包含空行
  let withEmpty = "  line1\n\n  line2\n  \n  line3"
  let expected = "line1\n\nline2\n\nline3"
  assertEqual "with empty lines" expected (normalizeIndentation withEmpty)
  
  -- 只有空白字符的行
  let withWhitespace = "  line1\n    \n  line2\n  \t \n  line3"
  let expected2 = "line1\n  \nline2\n \t \nline3"
  result2 <- return $ normalizeIndentation withWhitespace
  assertBool "whitespace-only lines" (expected2 == result2)
  
  -- 开头和结尾的空行
  let surrounded = "\n  \n  line1\n  line2\n  \n\n"
  let expected3 = "\n\nline1\nline2\n\n\n"
  result3 <- return $ normalizeIndentation surrounded
  assertBool "empty lines at start L.and end" (expected3 == result3)

-- | 测试复杂的缩进场景
testComplexIndentationScenarios :: TestTree
testComplexIndentationScenarios = testCase "Complex indentation scenarios" $ do
  -- 类似Python的缩进结构
  let pythonStyle = "def func():\n    if True:\n        print(\"hello\")\n    else:\n        print(\"world\")"
  let expected1 = "def func():\n  if True:\n    print(\"hello\")\n  else:\n    print(\"world\")"
  result1 <- return $ normalizeIndentation pythonStyle
  assertBool "Python-style indentation" (expected1 == result1)
  
  -- 不规则的缩进
  let irregular = "    level1\n  level2\n      level3\n level4"
  let expected2 = "level1\nlevel2\n  level3\nlevel4"
  result2 <- return $ normalizeIndentation irregular
  assertBool "irregular indentation" (expected2 == result2)
  
  -- 深层嵌套
  let deep = "  level1\n    level2\n      level3\n        level4\n          level5"
  let expected3 = "level1\n  level2\n    level3\n      level4\n        level5"
  assertEqual "deep nesting" expected3 (normalizeIndentation deep)
  
  -- 包含制表符的深层嵌套
  let deepTabs = "\tlevel1\n\t\tlevel2\n\t\t\tlevel3"
  let expected4 = "level1\n\tlevel2\n\t\tlevel3"
  assertEqual "deep tab nesting" expected4 (normalizeIndentation deepTabs)

-- | QuickCheck 属性测试
testIndentationProperties :: TestTree
testIndentationProperties = testGroup "Indentation Properties"
  [ testProperty "normalizeIndentation preserves relative indentation" $ \str ->
      let lines' = lines str
          nonEmptyLines = L.filter (not . null . trim) lines'
          commonIndent = if null nonEmptyLines then "" else findCommonIndent nonEmptyLines
          result = normalizeIndentation str
          resultLines = lines result
          resultNonEmpty = L.filter (not . null . trim) resultLines
      in if null nonEmptyLines 
         then property True
         else L.all (not . L.isPrefixOf commonIndent . takeWhile isSpace) resultNonEmpty
         
  , testProperty "normalizeIndentation preserves line count" $ \str ->
      let originalLines = lines str
          resultLines = lines (normalizeIndentation str)
      in L.length originalLines === L.length resultLines
      
  , testProperty "normalizeIndentation preserves non-empty content" $ \str ->
      let originalLines = lines str
          resultLines = lines (normalizeIndentation str)
          originalContent = map trim originalLines
          resultContent = map trim resultLines
      in L.filter (not . null) originalContent === L.filter (not . null) resultContent
      
  , testProperty "normalizeIndentation handles empty string" $ \() ->
      let empty = "" :: String
          result = normalizeIndentation empty
      in result === empty
      
  , testProperty "normalizeIndentation idempotent" $ \str ->
      let first = normalizeIndentation str
          second = normalizeIndentation first
      in first === second
  ]

-- | 辅助函数：查找字符串列表的公共前缀
findCommonIndent :: [String] -> String
findCommonIndent [] = ""
findCommonIndent strings = 
  let nonEmpty = L.filter (not . null) strings
      indents = L.map (takeWhile isSpace) nonEmpty
  in if null indents then "" else commonPrefix indents

-- | 查找列表的公共前缀
commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix [] = []
commonPrefix (x:xs) = foldr commonPrefix' x xs
  where
    commonPrefix' [] _ = []
    commonPrefix' _ [] = []
    commonPrefix' (y:ys) (z:zs) 
      | y == z    = y : commonPrefix' ys zs
      | otherwise = []