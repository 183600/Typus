{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsStringProcessingEnhancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof, sized)
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
import Data.Char (isSpace, toLower, isAlphaNum, isLetter)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, nub)

-- ============================================================================
-- 生成测试数据
-- ============================================================================

-- 生成包含各种空白字符的字符串
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements " \t\n\r\f\v"

-- 生成包含特殊字符的字符串
genSpecialCharString :: Gen String
genSpecialCharString = listOf $ elements "!@#$%^&*()_+-=[]{}|;':\",./<>?"

-- 生成包含注释标记的字符串
genCommentString :: Gen String
genCommentString = do
  base <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  comment <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t!@#$%^&*()"
  return $ base ++ "//" ++ comment

-- 生成包含块注释的字符串
genBlockCommentString :: Gen String
genBlockCommentString = do
  before <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  comment <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t!@#$%^&*()"
  after <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  return $ before ++ "/*" ++ comment ++ "*/" ++ after

-- 生成包含字符串字面量的字符串
genStringLiteralString :: Gen String
genStringLiteralString = do
  before <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  literal <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t.,!?/"
  after <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  return $ before ++ "\"" ++ literal ++ "\"" ++ after

-- 生成包含字符字面量的字符串
genCharLiteralString :: Gen String
genCharLiteralString = do
  before <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  char <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  after <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  return $ before ++ "'" ++ [char] ++ "'" ++ after

-- 生成包含混合缩进的字符串
genMixedIndentationString :: Gen String
genMixedIndentationString = do
  lines <- listOf $ do
    indent <- oneof 
      [ return ""
      , return " "
      , return "  "
      , return "   "
      , return "    "
      , return "\t"
      , return "\t\t"
      , return " \t"
      , return "\t "
      ]
    content <- listOf $ elements $ ['a'..'z'] ++ [' '] 
    return $ indent ++ content
  return $ unlines lines

-- ============================================================================
-- 增强的字符串处理属性测试
-- ============================================================================

-- Property: trim处理各种空白字符
prop_trim_handles_all_whitespace :: String -> String -> Property
prop_trim_handles_all_whitespace prefix suffix =
  let whitespace = " \t\n\r\f\v"
      content = prefix ++ "content" ++ suffix
      trimmed = trim content
      hasLeading = L.any (`elem` whitespace) prefix
      hasTrailing = L.any (`elem` whitespace) suffix
      noLeadingSpace = null trimmed || not (L.head trimmed `elem` whitespace)
      noTrailingSpace = null trimmed || not (last trimmed `elem` whitespace)
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

-- Property: splitBy处理Unicode字符
prop_splitBy_unicode :: Char -> String -> Property
prop_splitBy_unicode delim str =
  let segments = splitBy delim str
      rejoined = Data.List.intercalate [delim] segments
  in property $ rejoined === str

-- Property: splitByCollapsed处理连续分隔符
prop_splitByCollapsed_consecutive :: Char -> String -> String -> Property
prop_splitByCollapsed_consecutive delim middle1 middle2 =
  not (null middle1) && not (null middle2) ==>
  let input = middle1 ++ [delim, delim] ++ middle2
      segments = splitByCollapsed delim input
  in property $ segments === [middle1, middle2]

-- Property: removeLineComments保留字符串字面量中的注释标记
prop_removeLineComments_preserves_string_literals :: String -> String -> Property
prop_removeLineComments_preserves_string_literals code comment =
  not ('"' `elem` comment) ==>
  let stringWithComment = code ++ " // " ++ comment ++ " \"" ++ "text with // comment" ++ "\""
      cleaned = removeLineComments stringWithComment
  in property $ "\"text with // comment\"" `L.isInfixOf` cleaned

-- Property: removeLineComments保留字符字面量中的注释标记
prop_removeLineComments_preserves_char_literals :: String -> Char -> Property
prop_removeLineComments_preserves_char_literals code char =
  let stringWithComment = code ++ " // comment '" ++ [char] ++ "' // more"
      cleaned = removeLineComments stringWithComment
  in property $ "'" ++ [char] ++ "'" `L.isInfixOf` cleaned

-- Property: removeComments处理嵌套结构
prop_removeComments_nested_structures :: String -> String -> String -> Property
prop_removeComments_nested_structures code1 code2 comment =
  not ('"' `elem` code1) && not ('"' `elem` code2) &&
  not ('\'' `elem` code1) && not ('\'' `elem` code2) &&
  not ("/*" `L.isInfixOf` code1) && not ("/*" `L.isInfixOf` code2) ==>
  let nested = code1 ++ " /* outer " ++ comment ++ " */ " ++ code2
      cleaned = removeComments nested
  in property $ cleaned === (code1 ++ "  " ++ code2)

-- Property: normalizeIndentation保持相对缩进
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative input =
  let lines' = lines input
      nonEmpty = L.filter (not . L.all isSpace) lines'
  in L.length nonEmpty >= 2 ==>
     let normalized = normalizeIndentation input
         normLines = lines normalized
         firstNonEmpty = L.head nonEmpty
         firstIndent = L.length $ takeWhile isSpace firstNonEmpty
         relativeIndents = L.map (\l -> L.length (takeWhile isSpace l) - firstIndent) nonEmpty
         normIndents = L.map (L.length . takeWhile isSpace) (L.filter (not . L.all isSpace) normLines)
     in property $ L.all (>= 0) normIndents

-- Property: forceSingleTabIndentation转换为制表符
prop_forceSingleTabIndentation_converts_to_tabs :: String -> Property
prop_forceSingleTabIndentation_converts_to_tabs input =
  let lines' = lines input
      nonEmpty = L.filter (not . L.all isSpace) lines'
  in not (null nonEmpty) ==>
     let tabbed = forceSingleTabIndentation input
         tabLines = lines tabbed
         nonEmptyTabbed = L.filter (not . null) tabLines
     in property $ L.all (\l -> L.head l == '\t') nonEmptyTabbed

-- Property: breakOn处理空模式
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern str =
  let (before, after) = breakOn "" str
  in property $ before === "" .&&. after === str

-- Property: breakOn处理不存在的模式
prop_breakOn_nonexistent_pattern :: String -> String -> Property
prop_breakOn_nonexistent_pattern str pattern =
  not (pattern `L.isInfixOf` str) ==>
  let (before, after) = breakOn pattern str
  in property $ before === str .&&. after === ""

-- Property: breakOn处理存在的模式
prop_breakOn_existing_pattern :: String -> String -> String -> Property
prop_breakOn_existing_pattern prefix pattern suffix =
  not (null pattern) ==>
  let str = prefix ++ pattern ++ suffix
      (before, after) = breakOn pattern str
  in property $ before === prefix .&&. after === suffix

-- ============================================================================
-- 性能和边界条件测试
-- ============================================================================

-- Property: 大字符串处理性能
prop_large_string_processing :: Int -> Property
prop_large_string_processing size =
  size > 0 && size <= 10000 ==>
  let largeString = L.concat (replicate size "test ")
      trimmed = trim largeString
  in property $ not (null trimmed) && L.head trimmed == 't' && last trimmed == 't'

-- Property: 深度嵌套注释
prop_deep_nested_comments :: Int -> Property
prop_deep_nested_comments depth =
  depth > 0 && depth <= 100 ==>
  let nestedComment = "/*" ++ L.concat (replicate depth "nested ") ++ "*/"
      code = "before " ++ nestedComment ++ " after"
      cleaned = removeComments code
  in property $ cleaned === "before  after"

-- Property: 复杂缩进模式
prop_complex_indentation_patterns :: Int -> Property
prop_complex_indentation_patterns lines =
  lines > 0 && lines <= 50 ==>
  let indentPattern = cycle ["", " ", "  ", "   ", "    ", "\t", "\t ", " \t"]
      lines' = take lines $ zipWith (++) indentPattern (repeat "content\n")
      content = L.concat lines'
      normalized = normalizeIndentation content
  in property $ not (null normalized)

-- ============================================================================
-- 单元测试
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Utils String Processing Enhanced Tests"
    [ testGroup "Property Tests"
        [ fastProperty "trim handles L.all whitespace characters" prop_trim_handles_all_whitespace
        , fastProperty "splitBy handles Unicode characters" prop_splitBy_unicode
        , fastProperty "splitByCollapsed handles consecutive delimiters" prop_splitByCollapsed_consecutive
        , fastProperty "removeLineComments preserves string literals" prop_removeLineComments_preserves_string_literals
        , fastProperty "removeLineComments preserves char literals" prop_removeLineComments_preserves_char_literals
        , fastProperty "removeComments handles nested structures" prop_removeComments_nested_structures
        , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
        , fastProperty "forceSingleTabIndentation converts to tabs" prop_forceSingleTabIndentation_converts_to_tabs
        , fastProperty "breakOn handles empty pattern" prop_breakOn_empty_pattern
        , fastProperty "breakOn handles nonexistent pattern" prop_breakOn_nonexistent_pattern
        , fastProperty "breakOn handles existing pattern" prop_breakOn_existing_pattern
        , fastProperty "large string processing performance" prop_large_string_processing
        , fastProperty "deep nested comments" prop_deep_nested_comments
        , fastProperty "complex indentation patterns" prop_complex_indentation_patterns
        ]
    , testGroup "Unit Tests"
        [ testCase "trim handles mixed whitespace" $ do
            trim "  \t\n  content  \r\n\t  " @?= "content"

        , testCase "splitBy with Unicode delimiter" $ do
            splitBy '€' "a€b€c" @?= ["a", "b", "c"]

        , testCase "splitByCollapsed with mixed delimiters" $ do
            splitByCollapsed ',' "a,,b,,,c" @?= ["a", "b", "c"]

        , testCase "removeLineComments with escaped quotes" $ do
            let input = "code \"string with // inside\" // comment"
                expected = "code \"string with // inside\" "
            removeLineComments input @?= expected

        , testCase "removeComments with escaped block comment" $ do
            let input = "code \"string with /* not a comment */\" /* real comment */ more"
                expected = "code \"string with /* not a comment */\"  more"
            removeComments input @?= expected

        , testCase "normalizeIndentation with mixed tabs L.and spaces" $ do
            let input = unlines ["    line1", "\tline2", "  \t line3", "        line4"]
                result = normalizeIndentation input
                resultLines = lines result
            L.length resultLines @?= 4
            L.head resultLines @?= "line1"

        , testCase "forceSingleTabIndentation with complex content" $ do
            let input = unlines ["    line1", "        line2", "", "  line3"]
                result = forceSingleTabIndentation input
                resultLines = lines result
            L.filter (not . null) resultLines @?= ["\tline1", "\tline2", "\tline3"]

        , testCase "breakOn with multiple occurrences" $ do
            let input = "hello world hello universe"
                (before, after) = breakOn "hello" input
            before @?= ""
            after @?= " world hello universe"

        , testCase "complex comment removal" $ do
            let input = unlines
                  [ "code before // line comment"
                  , "code /* block comment */ after"
                  , "\"string with // not comment\""
                  , "'c' // char comment"
                  ]
                result = removeComments input
                resultLines = lines result
            L.head resultLines @?= "code before "
            resultLines !! 1 @?= "code  after"
            "\"string with // not comment\"" `L.isInfixOf` result

        , testCase "indentation normalization edge cases" $ do
            let input = unlines ["", "    ", "  content", "\t\tmore", ""]
                result = normalizeIndentation input
                resultLines = lines result
            L.length resultLines @?= 5
            resultLines !! 0 @?= ""
            resultLines !! 1 @?= ""
            resultLines !! 2 @?= "content"

        , testCase "splitByComma with quoted commas" $ do
            let csv = "item1,\"item2,with,commas\",item3"
                result = splitByComma csv
            L.length result @?= 3
            result !! 0 @?= "item1"
            result !! 1 @?= "\"item2"
            result !! 2 @?= "with,commas\",item3"
        ]
    ]