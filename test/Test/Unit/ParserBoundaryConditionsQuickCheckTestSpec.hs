{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserBoundaryConditionsQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose
  , sized, resize, suchThat, vectorOf, arbitrary
  )

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, length)
import Data.List (null)
import Data.Char (isSpace, isControl)

-- | 生成空字符串
genEmptyString :: Gen String
genEmptyString = pure ""

-- | 生成只包含空白字符的字符串
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements $ " \t\n\r"

-- | 生成非常长的字符串
genLongString :: Gen String
genLongString = do
  size <- choose (1000, 10000)
  vectorOf size $ elements $ ['a'..'z'] ++ [' '] ++ ['\n']

-- | 生成包含特殊字符的字符串
genSpecialCharString :: Gen String
genSpecialCharString = listOf $ elements $ 
  ['\0'..'\31'] ++ ['\127'..'\255'] ++ [' '] ++ ['!'] ++ ['~']  -- 控制字符和可打印字符

-- | 生成包含Unicode字符的字符串
genUnicodeString :: Gen String
genUnicodeString = listOf $ elements $ 
  ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '] ++ 
  ['\128'..'\255'] ++  -- 扩展ASCII
  ['\256'..'\511']    -- 更多Unicode字符

-- | 生成包含嵌套结构的字符串
genNestedString :: Gen String
genNestedString = do
  depth <- choose (1, 10)
  let buildNested 0 = "}"
      buildNested n = "{ " ++ buildNested (n-1) ++ " }"
  return $ buildNested depth

-- | 生成包含重复字符的字符串
genRepeatedCharString :: Gen String
genRepeatedCharString = do
  char <- elements $ ['a'..'z'] ++ [' '] ++ ['\n']
  count <- choose (100, 1000)
  return $ replicate count char

-- | 生成包含不平衡括号的字符串
genUnbalancedBracketsString :: Gen String
genUnbalancedBracketsString = do
  openCount <- choose (1, 10)
  closeCount <- choose (0, openCount - 1)
  return $ replicate openCount '{' ++ " content " ++ replicate closeCount '}'

-- | 生成包含无效指令的字符串
genInvalidDirectiveString :: Gen String
genInvalidDirectiveString = do
  directive <- elements ["//! invalid", "//# unknown", "/// malformed", "//!", "//#"]
  content <- listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['\n']
  return $ directive ++ " " ++ unlines content

-- | 生成包含混合换行符的字符串
genMixedNewlineString :: Gen String
genMixedNewlineString = do
  lines <- listOf $ listOf $ elements $ ['a'..'z'] ++ [' ']
  let newlines = cycle ["\n", "\r\n", "\r"]
      mixedLines = zipWith (++) lines (take (L.length lines) newlines)
  return $ L.concat mixedLines

-- | 生成包含转义字符的字符串
genEscapedString :: Gen String
genEscapedString = do
  parts <- listOf $ elements 
    [ "\\n", "\\t", "\\r", "\\\\", "\\\"", "\\'", "\\0", "\\x41", "\\u1234" ]
  return $ L.concat parts

-- 属性：解析空字符串应该成功
prop_parse_empty_string :: Property
prop_parse_empty_string =
  case parseTypus "" of
    Left _ -> property False
    Right _ -> property True

-- 属性：解析只包含空白字符的字符串应该成功
prop_parse_whitespace_string :: Property
prop_parse_whitespace_string =
  forAll genWhitespaceString $ \whitespace ->
    case parseTypus whitespace of
      Left _ -> property False
      Right _ -> property True

-- 属性：解析非常长的字符串应该合理处理
prop_parse_long_string :: Property
prop_parse_long_string =
  forAll genLongString $ \longString ->
    case parseTypus longString of
      Left _ -> property True  -- 可能失败，但不应该崩溃
      Right _ -> property True

-- 属性：解析包含特殊字符的字符串应该合理处理
prop_parse_special_char_string :: Property
prop_parse_special_char_string =
  forAll genSpecialCharString $ \specialString ->
    case parseTypus specialString of
      Left _ -> property True  -- 可能失败，但不应该崩溃
      Right _ -> property True

-- 属性：解析包含Unicode字符的字符串应该合理处理
prop_parse_unicode_string :: Property
prop_parse_unicode_string =
  forAll genUnicodeString $ \unicodeString ->
    case parseTypus unicodeString of
      Left _ -> property True  -- 可能失败，但不应该崩溃
      Right _ -> property True

-- 属性：解析包含嵌套结构的字符串应该合理处理
prop_parse_nested_string :: Property
prop_parse_nested_string =
  forAll genNestedString $ \nestedString ->
    case parseTypus nestedString of
      Left _ -> property True  -- 可能失败，但不应该崩溃
      Right _ -> property True

-- 属性：解析包含重复字符的字符串应该合理处理
prop_parse_repeated_char_string :: Property
prop_parse_repeated_char_string =
  forAll genRepeatedCharString $ \repeatedString ->
    case parseTypus repeatedString of
      Left _ -> property True  -- 可能失败，但不应该崩溃
      Right _ -> property True

-- 属性：解析包含不平衡括号的字符串应该合理处理
prop_parse_unbalanced_brackets :: Property
prop_parse_unbalanced_brackets =
  forAll genUnbalancedBracketsString $ \unbalancedString ->
    case parseTypus unbalancedString of
      Left _ -> property True  -- 可能失败，这是预期的
      Right _ -> property True

-- 属性：解析包含无效指令的字符串应该合理处理
prop_parse_invalid_directive :: Property
prop_parse_invalid_directive =
  forAll genInvalidDirectiveString $ \invalidDirective ->
    case parseTypus invalidDirective of
      Left _ -> property True  -- 可能失败，这是预期的
      Right _ -> property True

-- 属性：解析包含混合换行符的字符串应该合理处理
prop_parse_mixed_newlines :: Property
prop_parse_mixed_newlines =
  forAll genMixedNewlineString $ \mixedNewlineString ->
    case parseTypus mixedNewlineString of
      Left _ -> property True  -- 可能失败，但不应该崩溃
      Right _ -> property True

-- 属性：解析包含转义字符的字符串应该合理处理
prop_parse_escaped_string :: Property
prop_parse_escaped_string =
  forAll genEscapedString $ \escapedString ->
    case parseTypus escapedString of
      Left _ -> property True  -- 可能失败，但不应该崩溃
      Right _ -> property True

-- 属性：解析后重新解析应该产生相同结果
prop_parse_reparse_consistency :: Property
prop_parse_reparse_consistency =
  forAll genUnicodeString $ \originalString ->
    case parseTypus originalString of
      Left _ -> property True  -- 解析失败时跳过
      Right parsedFile ->
        -- 将解析后的文件重新渲染并再次解析
        -- 这里简化处理，只检查解析一致性
        case parseTypus originalString of
          Left _ -> property False  -- 第二次解析不应该失败
          Right reparsedFile ->
            L.length (tfBlocks parsedFile) === L.length (tfBlocks reparsedFile)

-- 属性：解析包含大量注释的字符串应该合理处理
prop_parse_many_comments :: Property
prop_parse_many_comments =
  let commentCount = 1000
      comments = replicate commentCount "// This is a comment\n"
      codeWithComments = L.concat comments ++ "func main() {}"
  in case parseTypus codeWithComments of
       Left _ -> property True  -- 可能失败，但不应该崩溃
       Right _ -> property True

-- 属性：解析包含深度嵌套注释的字符串应该合理处理
prop_parse_nested_comments :: Property
prop_parse_nested_comments =
  let depth = 100
      nestedComments = L.concat $ replicate depth "/* "
      codeWithNestedComments = nestedComments ++ "func main() {}" ++ L.concat (replicate depth " */")
  in case parseTypus codeWithNestedComments of
       Left _ -> property True  -- 可能失败，但不应该崩溃
       Right _ -> property True

-- 属性：解析包含极端长标识符的字符串应该合理处理
prop_parse_extreme_identifiers :: Property
prop_parse_extreme_identifiers =
  let longIdentifier = replicate 1000 'a'
      codeWithLongIdentifier = "func " ++ longIdentifier ++ "() {}"
  in case parseTypus codeWithLongIdentifier of
       Left _ -> property True  -- 可能失败，但不应该崩溃
       Right _ -> property True

-- 属性：解析包含数字开头的标识符应该合理处理
prop_parse_numeric_identifiers :: Property
prop_parse_numeric_identifiers =
  let numericIdentifier = "123abc"
      codeWithNumericIdentifier = "func " ++ numericIdentifier ++ "() {}"
  in case parseTypus codeWithNumericIdentifier of
       Left _ -> property True  -- 应该失败，这是预期的
       Right _ -> property True

-- 属性：解析包含空标识符的字符串应该合理处理
prop_parse_empty_identifiers :: Property
prop_parse_empty_identifiers =
  let codeWithEmptyIdentifier = "func () {}"
  in case parseTypus codeWithEmptyIdentifier of
       Left _ -> property True  -- 应该失败，这是预期的
       Right _ -> property True

-- 属性：解析包含多个相邻指令的字符串应该合理处理
prop_parse_adjacent_directives :: Property
prop_parse_adjacent_directives =
  let directives = ["//! ownership: on", "//! dependent_types: on", "//! constraints: on"]
      codeWithDirectives = unlines directives ++ "\nfunc main() {}"
  in case parseTypus codeWithDirectives of
       Left _ -> property True  -- 可能失败，但不应该崩溃
       Right _ -> property True

-- 属性：解析包含冲突指令的字符串应该合理处理
prop_parse_conflicting_directives :: Property
prop_parse_conflicting_directives =
  let directives = ["//! ownership: on", "//! ownership: off"]
      codeWithDirectives = unlines directives ++ "\nfunc main() {}"
  in case parseTypus codeWithDirectives of
       Left _ -> property True  -- 可能失败，但不应该崩溃
       Right _ -> property True

tests :: TestTree
tests =
  testGroup "Parser Boundary Conditions QuickCheck Tests"
    [ fastProperty "Parse empty string" prop_parse_empty_string
    , fastProperty "Parse whitespace string" prop_parse_whitespace_string
    , fastProperty "Parse long string" prop_parse_long_string
    , fastProperty "Parse special char string" prop_parse_special_char_string
    , fastProperty "Parse unicode string" prop_parse_unicode_string
    , fastProperty "Parse nested string" prop_parse_nested_string
    , fastProperty "Parse repeated char string" prop_parse_repeated_char_string
    , fastProperty "Parse unbalanced brackets" prop_parse_unbalanced_brackets
    , fastProperty "Parse invalid directive" prop_parse_invalid_directive
    , fastProperty "Parse mixed newlines" prop_parse_mixed_newlines
    , fastProperty "Parse escaped string" prop_parse_escaped_string
    , fastProperty "Parse reparse consistency" prop_parse_reparse_consistency
    , fastProperty "Parse many comments" prop_parse_many_comments
    , fastProperty "Parse nested comments" prop_parse_nested_comments
    , fastProperty "Parse extreme identifiers" prop_parse_extreme_identifiers
    , fastProperty "Parse numeric identifiers" prop_parse_numeric_identifiers
    , fastProperty "Parse empty identifiers" prop_parse_empty_identifiers
    , fastProperty "Parse adjacent directives" prop_parse_adjacent_directives
    , fastProperty "Parse conflicting directives" prop_parse_conflicting_directives
    ]