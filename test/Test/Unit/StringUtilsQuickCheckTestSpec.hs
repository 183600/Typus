{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.StringUtilsQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose
  , sized, resize, suchThat, vectorOf, arbitrary
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

import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate)

-- | 生成包含空白字符的字符串
genStringWithWhitespace :: Gen String
genStringWithWhitespace = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '] ++ ['\t'] ++ ['\n']
  return content

-- | 生成不包含特定分隔符的字符串
genStringWithout :: Char -> Gen String
genStringWithout delim = do
  content <- listOf $ elements $ L.filter (/= delim) $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '] ++ ['_']
  return content

-- | 生成包含特定分隔符的字符串
genStringWith :: Char -> Gen String
genStringWith delim = do
  parts <- listOf1 (genStringWithout delim)
  return $ intercalate [delim] parts

-- | 生成包含注释的代码字符串
genCommentedCode :: Gen String
genCommentedCode = do
  codeLines <- listOf1 $ listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  commentLines <- listOf $ listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  let mixed = interleave codeLines commentLines
  return $ unlines $ L.map (\(i, line) -> 
    if even i then line else "// " ++ line) (zip [0..] mixed)
  where
    interleave [] _ = []
    interleave (x:xs) [] = [x]
    interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- | 生成包含块注释的代码字符串
genBlockCommentedCode :: Gen String
genBlockCommentedCode = do
  beforeComment <- listOf $ arbitrary `suchThat` (/= '/')
  commentContent <- listOf $ arbitrary `suchThat` (/= '*')
  afterComment <- listOf $ arbitrary `suchThat` (/= '/')
  return $ beforeComment ++ "/*" ++ commentContent ++ "*/" ++ afterComment

-- | 生成包含缩进的代码字符串
genIndentedCode :: Gen String
genIndentedCode = do
  baseIndent <- choose (0, 4)
  lines <- listOf1 $ do
    indent <- choose (baseIndent, baseIndent + 3)
    content <- listOf $ elements $ ['a'..'z'] ++ [' ']
    return $ replicate indent ' ' ++ content
  return $ unlines lines

-- 属性：trim函数应该移除两端的空白字符
prop_trim_removes_whitespace :: Property
prop_trim_removes_whitespace =
  forAll genStringWithWhitespace $ \s ->
    let trimmed = trim s
        hasLeadingSpace = not (null s) && isSpace (L.head s)
        hasTrailingSpace = not (null s) && isSpace (last s)
    in classify (hasLeadingSpace || hasTrailingSpace) "had whitespace" $
       classify (trimmed == s) "no change" $
       not (null trimmed) ==> 
       (not (isSpace (L.head trimmed)) && not (isSpace (last trimmed)))

-- 属性：splitBy应该正确处理空字符串
prop_splitBy_empty_string :: Property
prop_splitBy_empty_string =
  forAll arbitrary $ \delim ->
    splitBy delim "" === [""]

-- 属性：splitByCollapsed应该移除空段
prop_splitByCollapsed_removes_empty :: Property
prop_splitByCollapsed_removes_empty =
  forAll (genStringWith ',') $ \s ->
    let normal = splitBy ',' s
        collapsed = splitByCollapsed ',' s
    in L.all (not . null) collapsed === True

-- 属性：splitByComma应该是splitBy ','的别名
prop_splitByComma_alias :: Property
prop_splitByComma_alias =
  forAll arbitrary $ \s ->
    splitByComma s === splitBy ',' s

-- 属性：splitByCommaCollapsed应该是splitByCollapsed ','的别名
prop_splitByCommaCollapsed_alias :: Property
prop_splitByCommaCollapsed_alias =
  forAll arbitrary $ \s ->
    splitByCommaCollapsed s === splitByCollapsed ',' s

-- 属性：removeLineComments应该移除//注释但保留字符串中的//
prop_removeLineComments_preserves_strings :: Property
prop_removeLineComments_preserves_strings =
  let codeWithCommentedString = "var s = \"hello // world\" // this is a comment\nvar x = 42"
      expected = "var s = \"hello // world\" \nvar x = 42"
  in removeLineComments codeWithCommentedString === expected

-- 属性：removeComments应该移除行注释和块注释
prop_removeComments_removes_both_types :: Property
prop_removeComments_removes_both_types =
  let code = "var x = 42 // line comment\nvar y = /* block comment */ 24"
      expected = "var x = 42 \nvar y =  24"
  in removeComments code === expected

-- 属性：removeComments应该保留字符串中的注释标记
prop_removeComments_preserves_string_comments :: Property
prop_removeComments_preserves_string_comments =
  let code = "var s = \"// not a comment\" /* also not a comment */"
      expected = "var s = \"// not a comment\" "
  in removeComments code === expected

-- 属性：normalizeIndentation应该保留相对缩进
prop_normalizeIndentation_preserves_relative :: Property
prop_normalizeIndentation_preserves_relative =
  forAll genIndentedCode $ \code ->
    let normalized = normalizeIndentation code
        originalLines = lines code
        normalizedLines = lines normalized
        -- 检查非空行的相对缩进是否保持
        calcIndent line = L.length $ takeWhile isSpace line
        originalIndents = map calcIndent $ L.filter (not . L.all isSpace) originalLines
        normalizedIndents = map calcIndent $ L.filter (not . L.all isSpace) normalizedLines
    in case (originalIndents, normalizedIndents) of
         ([], []) -> property True
         (orig, norm) -> 
           let minOrig = L.minimum orig
               minNorm = L.minimum norm
               relativeOrig = L.map (subtract minOrig) orig
               relativeNorm = L.map (subtract minNorm) norm
           in relativeOrig === relativeNorm

-- 属性：forceSingleTabIndentation应该将所有非空行转换为单个制表符
prop_forceSingleTabIndentation_single_tab :: Property
prop_forceSingleTabIndentation_single_tab =
  forAll genIndentedCode $ \code ->
    let forced = forceSingleTabIndentation code
        lines' = lines forced
    in L.all (\line -> null line || take 1 line == "\t") lines'

-- 属性：fixIndentation应该是normalizeIndentation的别名
prop_fixIndentation_alias :: Property
prop_fixIndentation_alias =
  forAll arbitrary $ \s ->
    fixIndentation s === normalizeIndentation s

-- 属性：breakOn应该正确处理空模式
prop_breakOn_empty_pattern :: Property
prop_breakOn_empty_pattern =
  forAll arbitrary $ \s ->
    breakOn "" s === ("", s)

-- 属性：breakOn应该正确处理模式不存在的情况
prop_breakOn_pattern_not_found :: Property
prop_breakOn_pattern_not_found =
  forAll arbitrary $ \s ->
    let pattern = "XYZ_NOT_IN_STRING"
    in breakOn pattern s === (s, "")

-- 属性：breakOn应该正确处理模式存在的情况
prop_breakOn_pattern_found :: Property
prop_breakOn_pattern_found =
  forAll arbitrary $ \s ->
    let notNull = not (null s)
        firstChar = if notNull then [L.head s] else ""
    in notNull ==> 
       let (before, after) = breakOn firstChar s
       in before === "" && after === L.tail s

-- 属性：trim的幂等性 - 多次trim应该产生相同结果
prop_trim_idempotent :: Property
prop_trim_idempotent =
  forAll arbitrary $ \s ->
    trim (trim s) === trim s

-- 属性：splitBy的一致性 - 重新连接应该产生原始字符串
prop_splitBy_consistency :: Property
prop_splitBy_consistency =
  forAll (genStringWith ',') $ \s ->
    splitBy ',' s === s

-- 属性：removeLineComments不应该改变没有注释的字符串
prop_removeLineComments_no_change_without_comments :: Property
prop_removeLineComments_no_change_without_comments =
  forAll (listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9'] ++ ['\n']) $ \s ->
    not ("//" `L.isInfixOf` s) ==> removeLineComments s === s

tests :: TestTree
tests =
  testGroup "String Utils QuickCheck Tests"
    [ fastProperty "trim removes whitespace" prop_trim_removes_whitespace
    , fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "splitBy empty string" prop_splitBy_empty_string
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
    , fastProperty "splitByComma alias" prop_splitByComma_alias
    , fastProperty "splitByCommaCollapsed alias" prop_splitByCommaCollapsed_alias
    , fastProperty "removeLineComments preserves strings" prop_removeLineComments_preserves_strings
    , fastProperty "removeComments removes both types" prop_removeComments_removes_both_types
    , fastProperty "removeComments preserves string comments" prop_removeComments_preserves_string_comments
    , fastProperty "normalizeIndentation preserves relative" prop_normalizeIndentation_preserves_relative
    , fastProperty "forceSingleTabIndentation single tab" prop_forceSingleTabIndentation_single_tab
    , fastProperty "fixIndentation alias" prop_fixIndentation_alias
    , fastProperty "breakOn empty pattern" prop_breakOn_empty_pattern
    , fastProperty "breakOn pattern not found" prop_breakOn_pattern_not_found
    , fastProperty "breakOn pattern found" prop_breakOn_pattern_found
    , fastProperty "removeLineComments no change without comments" prop_removeLineComments_no_change_without_comments
    ]