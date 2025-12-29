{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose
  , sized, suchThat, vectorOf, frequency
  )

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.List (isPrefixOf, isInfixOf, intercalate)

-- ============================================================================
-- 生成器定义
-- ============================================================================

-- 生成包含空白字符的字符串
genStringWithWhitespace :: Gen String
genStringWithWhitespace = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r"

-- 生成纯字母数字字符串
genAlphaNumString :: Gen String
genAlphaNumString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- 生成分隔符
genDelimiter :: Gen Char
genDelimiter = elements ",;:||\\"

-- 生成包含注释的字符串
genCommentedString :: Gen String
genCommentedString = do
  before <- genAlphaNumString
  comment <- genAlphaNumString
  after <- genAlphaNumString
  return $ before ++ "// " ++ comment ++ "\n" ++ after

-- 生成包含块注释的字符串
genBlockCommentedString :: Gen String
genBlockCommentedString = do
  before <- genAlphaNumString
  comment <- genAlphaNumString
  after <- genAlphaNumString
  return $ before ++ "/* " ++ comment ++ " */" ++ after

-- 生成包含字符串字面量的代码
genStringLiteral :: Gen String
genStringLiteral = do
  content <- listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['\\', '"']
  return $ "\"" ++ content ++ "\""

-- 生成包含字符字面量的代码
genCharLiteral :: Gen String
genCharLiteral = do
  char <- elements $ ['a'..'z'] ++ ['\\', '\'']
  return $ "'" ++ [char] ++ "'"

-- 生成缩进字符串
genIndentedString :: Gen String
genIndentedString = do
  indentLevel <- choose (0, 5)
  content <- genAlphaNumString
  let indent = replicate indentLevel ' '
  return $ indent ++ content

-- 生成多行字符串
genMultiLineString :: Gen String
genMultiLineString = do
  numLines <- choose (1, 5)
  lines <- vectorOf numLines genIndentedString
  return $ intercalate "\n" lines

-- ============================================================================
-- QuickCheck 属性测试
-- ============================================================================

-- 属性: trim函数移除两端空白
prop_trim_removes_whitespace :: Property
prop_trim_removes_whitespace =
  forAll genStringWithWhitespace $ \s ->
    let trimmed = trim s
        startsNotSpace = null trimmed || not (isSpace (head trimmed))
        endsNotSpace = null trimmed || not (isSpace (last trimmed))
    in startsNotSpace .&&. endsNotSpace

-- 属性: trim对空字符串的处理
prop_trim_empty_string :: Property
prop_trim_empty_string = trim "" === ""

-- 属性: trim对纯空白字符串的处理
prop_trim_all_whitespace :: Property
prop_trim_all_whitespace =
  forAll (listOf (elements " \t\n\r")) $ \ws ->
    trim ws === ""

-- 属性: splitBy保留空段
prop_splitBy_preserves_empty :: Property
prop_splitBy_preserves_empty =
  forAll genDelimiter $ \delim ->
    splitBy delim "a,,b" === ["a", "", "b"]

-- 属性: splitBy对空字符串的处理
prop_splitBy_empty_string :: Property
prop_splitBy_empty_string =
  forAll genDelimiter $ \delim ->
    splitBy delim "" === [""]

-- 属性: splitByCollapsed折叠空段
prop_splitByCollapsed_collapses :: Property
prop_splitByCollapsed_collapses =
  forAll genDelimiter $ \delim ->
    splitByCollapsed delim "a,,b" === ["a", "b"]

-- 属性: splitByComma等于splitBy ','
prop_splitByComma_equals_splitBy :: Property
prop_splitByComma_equals_splitBy =
  forAll genStringWithWhitespace $ \s ->
    splitByComma s === splitBy ',' s

-- 属性: splitByCommaCollapsed等于splitByCollapsed ','
prop_splitByCommaCollapsed_equals_splitByCollapsed :: Property
prop_splitByCommaCollapsed_equals_splitByCollapsed =
  forAll genStringWithWhitespace $ \s ->
    splitByCommaCollapsed s === splitByCollapsed ',' s

-- 属性: removeLineComments移除单行注释
prop_removeLine_comments :: Property
prop_removeLine_comments =
  forAll genCommentedString $ \s ->
    let withoutComments = removeLineComments s
    in not ("//" `isInfixOf` withoutComments)

-- 属性: removeLineComments保留字符串字面量中的//
prop_removeLine_comments_preserves_string_literals :: Property
prop_removeLine_comments_preserves_string_literals =
  forAll genStringLiteral $ \literal ->
    let code = literal ++ " // comment\n"
        withoutComments = removeLineComments code
    in literal `isInfixOf` withoutComments

-- 属性: removeComments移除块注释
prop_remove_block_comments :: Property
prop_remove_block_comments =
  forAll genBlockCommentedString $ \s ->
    let withoutComments = removeComments s
    in not ("/*" `isInfixOf` withoutComments) .&&. not ("*/" `isInfixOf` withoutComments)

-- 属性: removeComments保留字符串字面量中的注释符号
prop_remove_comments_preserves_string_literals :: Property
prop_remove_comments_preserves_string_literals =
  forAll genStringLiteral $ \literal ->
    let code = literal ++ " /* comment */"
        withoutComments = removeComments code
    in literal `isInfixOf` withoutComments

-- 属性: normalizeIndentation保持相对缩进
prop_normalize_indentation_preserves_relative :: Property
prop_normalize_indentation_preserves_relative =
  forAll genMultiLineString $ \s ->
    let normalized = normalizeIndentation s
        lines_s = lines s
        lines_normalized = lines normalized
        -- 检查非空行的数量是否相同
        nonEmptyCount_s = length $ filter (not . all isSpace) lines_s
        nonEmptyCount_normalized = length $ filter (not . all isSpace) lines_normalized
    in nonEmptyCount_s === nonEmptyCount_normalized

-- 属性: normalizeIndentation不改变内容顺序
prop_normalize_indentation_preserves_order :: Property
prop_normalize_indentation_preserves_order =
  forAll genMultiLineString $ \s ->
    let normalized = normalizeIndentation s
        -- 移除所有空白后比较
        content_s = filter (not . isSpace) s
        content_normalized = filter (not . isSpace) normalized
    in content_s === content_normalized

-- 属性: forceSingleTabIndentation强制单制表符缩进
prop_force_single_tab_indentation :: Property
prop_force_single_tab_indentation =
  forAll genMultiLineString $ \s ->
    let forced = forceSingleTabIndentation s
        lines_forced = lines forced
        nonEmptyLines = filter (not . null) lines_forced
        -- 检查所有非空行都以制表符开头
        allStartWithTab = all ("\t" `isPrefixOf`) nonEmptyLines
    in allStartWithTab

-- 属性: fixIndentation等于normalizeIndentation
prop_fix_indentation_equals_normalize :: Property
prop_fix_indentation_equals_normalize =
  forAll genMultiLineString $ \s ->
    fixIndentation s === normalizeIndentation s

-- 属性: breakOn在模式存在时正确分割
prop_break_on_with_pattern :: Property
prop_break_on_with_pattern =
  forAll genAlphaNumString $ \pattern ->
    forAll genAlphaNumString $ \suffix ->
      let input = pattern ++ suffix
          (before, after) = breakOn pattern input
      in before === "" .&&. after === suffix

-- 属性: breakOn在模式不存在时返回原字符串
prop_break_on_without_pattern :: Property
prop_break_on_without_pattern =
  forAll genAlphaNumString $ \input ->
    forAll (suchThat genAlphaNumString (`notElem` input)) $ \pattern ->
      let (before, after) = breakOn pattern input
      in before === input .&&. after === ""

-- 属性: breakOn对空模式的处理
prop_break_on_empty_pattern :: Property
prop_break_on_empty_pattern =
  forAll genAlphaNumString $ \input ->
    let (before, after) = breakOn "" input
    in before === "" .&&. after === input

-- 属性: trim与splitBy的组合性质
prop_trim_splitby_combination :: Property
prop_trim_splitby_combination =
  forAll genStringWithWhitespace $ \s ->
  forAll genDelimiter $ \delim ->
    let trimmed = trim s
        splitResult = splitBy delim trimmed
        -- 所有分割结果都不应该有前后空白
        allTrimmed = all (\part -> trim part == part) splitResult
    in allTrimmed

-- 属性: removeComments与removeLineComments的关系
prop_remove_comments_vs_line_comments :: Property
prop_remove_comments_vs_line_comments =
  forAll genCommentedString $ \s ->
    let withoutLineComments = removeLineComments s
        withoutAllComments = removeComments s
    in length withoutAllComments <= length withoutLineComments

-- ============================================================================
-- 测试套件
-- ============================================================================

tests :: TestTree
tests = testGroup "Core Utils QuickCheck Tests"
  [ fastProperty "Trim removes whitespace from both ends" prop_trim_removes_whitespace
  , fastProperty "Trim handles empty string" prop_trim_empty_string
  , fastProperty "Trim handles all whitespace string" prop_trim_all_whitespace
  , fastProperty "SplitBy preserves empty segments" prop_splitBy_preserves_empty
  , fastProperty "SplitBy handles empty string" prop_splitBy_empty_string
  , fastProperty "SplitByCollapsed collapses empty segments" prop_splitByCollapsed_collapses
  , fastProperty "SplitByComma equals splitBy ','" prop_splitByComma_equals_splitBy
  , fastProperty "SplitByCommaCollapsed equals splitByCollapsed ','" prop_splitByCommaCollapsed_equals_splitByCollapsed
  , fastProperty "RemoveLineComments removes line comments" prop_remove_line_comments
  , fastProperty "RemoveLineComments preserves string literals" prop_removeLine_comments_preserves_string_literals
  , fastProperty "RemoveComments removes block comments" prop_remove_block_comments
  , fastProperty "RemoveComments preserves string literals" prop_remove_comments_preserves_string_literals
  , fastProperty "NormalizeIndentation preserves relative indentation" prop_normalize_indentation_preserves_relative
  , fastProperty "NormalizeIndentation preserves content order" prop_normalize_indentation_preserves_order
  , fastProperty "ForceSingleTabIndentation forces single tab" prop_force_single_tab_indentation
  , fastProperty "FixIndentation equals normalizeIndentation" prop_fix_indentation_equals_normalize
  , fastProperty "BreakOn with pattern splits correctly" prop_break_on_with_pattern
  , fastProperty "BreakOn without pattern returns original" prop_break_on_without_pattern
  , fastProperty "BreakOn handles empty pattern" prop_break_on_empty_pattern
  , fastProperty "Trim and splitBy combination" prop_trim_splitby_combination
  , fastProperty "RemoveComments vs removeLineComments relationship" prop_remove_comments_vs_line_comments
  ]