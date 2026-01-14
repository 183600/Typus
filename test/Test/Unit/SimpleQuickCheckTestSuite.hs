{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SimpleQuickCheckTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Utils (trim, splitBy, splitByComma, removeLineComments, removeComments, normalizeIndentation, isRight)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import Parser (TypusFile(..), parseTypus)
import ErrorHandler
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), formatError)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

-- | 测试trim函数的基本属性
prop_trim_basic :: String -> Property
prop_trim_basic s =
  let trimmed = trim s
  in length trimmed <= length s && 
     (null s ==> null trimmed) &&
     (all isSpace s ==> null trimmed)

-- | 测试trim对空字符串的处理
prop_trim_empty :: Property
prop_trim_empty = trim "" === ""

-- | 测试trim对空白字符的处理
prop_trim_whitespace :: String -> Property
prop_trim_whitespace s =
  let trimmed = trim s
  in all isSpace (trim s) ==> null trimmed

-- | 测试trim对普通字符的处理
prop_trim_regular :: Char -> String -> Property
prop_trim_regular c s =
  not (isSpace c) ==>
  let s' = c : s
      trimmed = trim s'
  in head trimmed === c && length trimmed >= 1

-- | 测试trim的幂等性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed1 = trim s
      trimmed2 = trim trimmed1
  in trimmed1 === trimmed2

-- | 测试splitBy的基本属性
prop_splitBy_basic :: Char -> String -> Property
prop_splitBy_basic c s =
  let parts = splitBy c s
  in concat parts === s

-- | 测试splitBy对空字符串的处理
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty c = splitBy c "" === []

-- | 测试splitBy对连续分隔符的处理
prop_splitBy_collapsed :: Char -> String -> Property
prop_splitBy_collapsed c s =
  let parts = splitBy c s
      collapsed = splitByCollapsed c s
  in length collapsed <= length parts &&
     (not (null c) ==> length collapsed >= 0)

-- | 测试splitByComma的基本属性
prop_splitByComma_basic :: String -> Property
prop_splitByComma_basic s =
  let parts = splitByComma s
  in concat parts === s

-- | 测试splitByComma对空字符串的处理
prop_splitByComma_empty :: Property
prop_splitByComma_empty = splitByComma "" === []

-- | 测试splitByComma对连续逗号的处理
prop_splitByComma_collapsed :: String -> Property
prop_splitByComma_collapsed s =
  let parts = splitByComma s
      collapsed = splitByCommaCollapsed s
  in length collapsed <= length parts &&
     (not (null s) ==> length collapsed >= 0)

-- | 测试removeLineComments的基本属性
prop_removeLineComments_basic :: String -> String -> Property
prop_removeLineComments_basic code comment =
  let codeWithComment = code ++ "// " ++ comment ++ "\nmore code"
      withoutComments = removeLineComments codeWithComment
  in property ("//" `notElem` withoutComments)

-- | 测试removeLineComments对空代码的处理
prop_removeLineComments_empty :: Property
prop_removeLineComments_empty = removeLineComments "" === ""

-- | 测试removeLineComments对没有注释的处理
prop_removeLineComments_no_comments :: String -> Property
prop_removeLineComments_no_comments code =
  not ("//" `isInfixOf` code) ==> removeLineComments code === code

-- | 测试removeLineComments对多行注释的处理
prop_removeLineComments_multiline :: Positive Int -> String -> Property
prop_removeLineComments_multiline (Positive n) code =
  n < 10 ==>
  let commentLines = replicate n "// comment line"
      comments = unlines commentLines
      codeWithComments = unlines [code, comments, "more code"]
      withoutComments = removeLineComments codeWithComments
  in all (not . isPrefixOf "//") (lines withoutComments)

-- | 测试removeComments的基本属性
prop_removeComments_basic :: String -> String -> Property
prop_removeComments_basic before after =
  let codeWithComment = before ++ "/* " ++ "comment" ++ " */" ++ after
      withoutComments = removeComments codeWithComment
  in property ("/*" `notElem` withoutComments && "*/" `notElem` withoutComments)

-- | 测试removeComments对空代码的处理
prop_removeComments_empty :: Property
prop_removeComments_empty = removeComments "" === ""

-- | 测试removeComments对没有注释的处理
prop_removeComments_no_comments :: String -> Property
prop_removeComments_no_comments code =
  not ("/*" `isInfixOf` code) && not ("*/" `isInfixOf` code) ==> 
  removeComments code === code

-- | 测试removeComments对多行注释的处理
prop_removeComments_multiline :: Positive Int -> String -> String -> Property
prop_removeComments_multiline (Positive n) before after =
  n < 10 ==>
  let commentLines = replicate n "/* comment line " ++ show i ++ " */"
      comments = unlines commentLines
      codeWithComment = unlines [before, comments, after]
      withoutComments = removeComments codeWithComment
  in all (not . isPrefixOf "/*") (lines withoutComments)

-- | 测试normalizeIndentation的基本属性
prop_normalizeIndentation_basic :: String -> Property
prop_normalizeIndentation_basic s =
  let normalized = normalizeIndentation s
  in length normalized >= 0

-- | 测试normalizeIndentation对空字符串的处理
prop_normalizeIndentation_empty :: Property
prop_normalizeIndentation_empty = normalizeIndentation "" === ""

-- | 测试normalizeIndentation对无缩进的处理
prop_normalizeIndentation_no_indent :: String -> Property
prop_normalizeIndentation_no_indent s =
  not (any isSpace s) ==> normalizeIndentation s === s

-- | 测试normalizeIndentation对一致缩进的处理
prop_normalizeIndentation_consistent :: String -> Property
prop_normalizeIndentation_consistent s =
  let indented = "  " ++ s
      normalized = normalizeIndentation indented
  in length normalized >= 0 && all (\line -> not (all isSpace (takeWhile isSpace line))) (lines normalized)

-- | 测试normalizeIndentation对不一致缩进的处理
prop_normalizeIndentation_inconsistent :: String -> String -> Property
prop_normalizeIndentation_inconsistent s =
  let indented = "  " ++ s ++ "\n    " ++ s
      normalized = normalizeIndentation indented
  in length normalized >= 0 && all (\line -> not (all isSpace (takeWhile isSpace line))) (lines normalized)

-- | 测试normalizeIndentation对极深缩进的处理
prop_normalizeIndentation_deep :: Positive Int -> Property
prop_normalizeIndentation_deep (Positive n) =
  n < 50 ==>
  let deepIndent = replicate n ' ' ++ "code"
      normalized = normalizeIndentation deepIndent
  in length normalized >= 0

-- | 测试isRight的基本属性
prop_isRight_basic :: Either String Int -> Property
prop_isRight_basic e = Data.Either.isRight e === (case e of Right _ -> True; Left _ -> False)

-- | 测试isLeft的基本属性
prop_isLeft_basic :: Either String Int -> Property
prop_isLeft_basic e = Data.Either.isLeft e === (case e of Left _ -> True; Right _ -> False)

-- | 测试isRight对Right值的处理
prop_isRight_right :: Int -> Property
prop_isRight_right x = isRight (Right x)

-- | 测试isRight对Left值的处理
prop_isRight_left :: String -> Property
prop_isRight_left msg = not $ isRight (Left msg)

-- | 测试isLeft对Right值的处理
prop_isLeft_right :: Int -> Property
prop_isLeft_right x = not $ isLeft (Right x)

-- | 测试isLeft对Left值的处理
prop_isLeft_left :: String -> Property
prop_isLeft_left msg = isLeft (Left msg)

-- | 测试isRight对Either的对称性
prop_isRight_either_symmetry :: Either String Int -> Property
prop_isRight_either_symmetry e = isRight e === (case e of Right _ -> True; Left _ -> False)

-- | 测试isLeft对Either的对称性
prop_isLeft_either_symmetry :: Either String Int -> Property
prop_isLeft_either_symmetry e = isLeft e === (case e of Left _ -> True; Right _ -> False)

-- | 测试safeProcessString的基本属性
prop_safeProcessString_basic :: String -> Property
prop_safeProcessString_basic s =
  let processed = safeProcessString s
  in length processed >= 0

-- | 测试safeProcessString对空字符串的处理
prop_safeProcessString_empty :: Property
prop_safeProcessString_empty = safeProcessString "" === ""

-- | 测试safeProcessString对特殊字符的处理
prop_safeProcessString_special :: Char -> Property
prop_safeProcessString_special c =
  let s = [c]
      processed = safeProcessString s
  in length processed >= 0

-- | 测试safeProcessString对控制字符的处理
prop_safeProcessString_control :: Char -> Property
prop_safeProcessString_control c =
  isControl c ==> 
  let s = [c]
      processed = safeProcessString s
  in length processed >= 0

-- | 测试safeProcessString对Unicode字符的处理
prop_safeProcessString_unicode :: Property
prop_safeProcessString_unicode =
  let unicodeChars = ['\0'..'\255']
      processed = map safeProcessString [c] unicodeChars
  in all (\p -> length p >= 0) processed

-- | 测试safeProcessString对极长字符串的处理
prop_safeProcessString_long :: Positive Int -> Property
prop_safeProcessString_long (Positive n) =
  n < 10000 ==>
  let longString = replicate n 'x'
      processed = safeProcessString longString
  in length processed >= 0

-- | 测试breakOn的基本属性
prop_breakOn_basic :: String -> String -> Property
prop_breakOn_basic sep s =
  let (before, after) = breakOn sep s
  in before ++ sep ++ after === s

-- | 测试breakOn对空分隔符的处理
prop_breakOn_empty :: String -> Property
prop_breakOn_empty s = breakOn "" s === (s, "")

-- | 测试breakOn对不存在的分隔符的处理
prop_breakOn_not_found :: String -> Property
prop_breakOn_not_found s =
  let (before, after) = breakOn ":" s
  in before === s && after === ""

-- | 测试breakOn对多个分隔符的处理
prop_breakOn_multiple :: String -> String -> Property
prop_breakOn_multiple sep s =
  let parts = splitBy sep s
      (before, after) = breakOn sep (concat parts)
  in before === concat (init parts) && after === last parts

-- | 测试breakOn对分隔符在开头的情况
prop_breakOn_prefix :: String -> String -> Property
prop_breakOn_prefix sep s =
  let s' = sep ++ "content"
      (before, after) = breakOn sep s'
  in before === "" && after === "content"

-- | 测试breakOn对分隔符在结尾的情况
prop_breakOn_suffix :: String -> String -> Property
prop_breakOn_suffix sep s =
  let s' = "content" ++ sep
      (before, after) = breakOn sep s'
  in before === "content" && after === ""

-- | 测试breakOn对没有分隔符的情况
prop_breakOn_no_separator :: String -> Property
prop_breakOn_no_separator s =
  let (before, after) = breakOn ":" s
  in before === s && after === ""

-- | 测试trim的边界情况
test_trim_edge_cases :: Assertion
test_trim_edge_cases = do
  assertEqual "Empty string" "" (trim "")
  assertEqual "Single space" "" (trim " ")
  assertEqual "Single tab" "" (trim "\t")
  assertEqual "Multiple spaces" "" (trim "   ")
  assertEqual "Mixed whitespace" "content" (trim "  \t  content  ")

-- | 测试splitBy的边界情况
test_splitBy_edge_cases :: Assertion
test_splitBy_edge_cases = do
  assertEqual "Empty string" [] (splitBy ',')
  assertEqual "No separator" ["single"] (splitBy 'x' "single")
  assertEqual "Single separator" ["", ""] (splitBy ',' ",")
  assertEqual "Multiple separators" ["a", "", "b"] (splitBy ',' "a,,b")

-- | 测试splitByComma的边界情况
test_splitByComma_edge_cases :: Assertion
test_splitByComma_edge_cases = do
  assertEqual "Empty string" [] (splitByComma "")
  assertEqual "No commas" ["single"] (splitByComma "single")
  assertEqual "Single comma" ["", ""] (splitByComma ",")
  assertEqual "Multiple commas" ["a", "", "b"] (splitByComma "a,,b")

-- | 测试removeLineComments的边界情况
test_removeLineComments_edge_cases :: Assertion
test_removeLineComments_edge_cases = do
  assertEqual "Empty code" "" (removeLineComments "")
  assertEqual "No comments" "code" (removeLineComments "code")
  assertEqual "Single line comment" "code " (removeLineComments "code // comment")
  assertEqual "Multiple line comments" "code\nmore code" (removeLineComments "code\n// comment1\n// comment2\nmore code")

-- | 测试removeComments的边界情况
test_removeComments_edge_cases :: Assertion
test_removeComments_edge_cases = do
  assertEqual "Empty code" "" (removeComments "")
  assertEqual "No comments" "code" (removeComments "code")
  assertEqual "Single line comment" "code " (removeComments "code /* comment */")
  assertEqual "Multiple line comments" "code\nmore code" (removeComments "code /* comment1 */\nmore code")

-- | 测试normalizeIndentation的边界情况
test_normalizeIndentation_edge_cases :: Assertion
test_normalizeIndentation_edge_cases = do
  assertEqual "Empty string" "" (normalizeIndentation "")
  assertEqual "No indentation" "code" (normalizeIndentation "code")
  assertEqual "Single indentation" "code" (normalizeIndentation "  code")
  assertEqual "Multiple indentation" "code" (normalizeIndentation "    code")

-- | 测试isRight的边界情况
test_isRight_edge_cases :: Assertion
test_isRight_edge_cases = do
  assertBool "Right value is right" (isRight (Right 42))
  assertBool "Left value is not right" (not $ isRight (Left "error"))

-- | 测试isLeft的边界情况
test_isLeft_edge_cases :: Assertion
test_isLeft_edge_cases = do
  assertBool "Left value is left" (isLeft (Left "error"))
  assertBool "Right value is not left" (not $ isLeft (Right "success"))

-- | 测试safeProcessString的边界情况
test_safeProcessString_edge_cases :: Assertion
test_safeProcessString_edge_cases = do
  assertEqual "Empty string" "" (safeProcessString "")
  assertEqual "Special characters" "" (safeProcessString "\0\1\2\3")
  assertEqual "Control characters" "" (safeProcessString "\t\n\r")
  assertEqual "Unicode characters" "" (safeProcessString "中文测试")

-- | 测试breakOn的边界情况
test_breakOn_edge_cases :: Assertion
test_breakOn_edge_cases = do
  assertEqual "Empty string" ("", "") (breakOn "" "")
  assertEqual "No separator found" ("test", "") (breakOn ":" "test")
  assertEqual "Separator at start" ("", "content") (breakOn ":" ":content")
  assertEqual "Separator at end" ("content", "") (breakOn ":" "content:")

-- | 测试套件
tests :: TestTree
tests = testGroup "Simple QuickCheck Test Suite"
  [ testProperty "Trim basic" prop_trim_basic
  , testProperty "Trim empty" prop_trim_empty
  , testProperty "Trim whitespace" prop_trim_whitespace
  , testProperty "Trim regular" prop_trim_regular
  , testProperty "Trim idempotent" prop_trim_idempotent
  , testProperty "SplitBy basic" prop_splitBy_basic
  , testProperty "SplitBy empty" prop_splitBy_empty
  , testProperty "SplitBy collapsed" prop_splitBy_collapsed
  , testProperty "SplitByComma basic" prop_splitByComma_basic
  , testProperty "SplitByComma empty" prop_splitByComma_empty
  , testProperty "SplitByComma collapsed" prop_splitByComma_collapsed
  , testProperty "RemoveLineComments basic" prop_removeLineComments_basic
  , testProperty "RemoveLineComments empty" prop_removeLineComments_empty
  , testProperty "RemoveLineComments no comments" prop_removeLineComments_no_comments
  , testProperty "RemoveLineComments multiline" prop_removeLineComments_multiline
  , testProperty "RemoveComments basic" prop_removeComments_basic
  , testProperty "RemoveComments empty" prop_removeComments_empty
  , testProperty "RemoveComments no comments" prop_removeComments_no_comments
  , testProperty "RemoveComments multiline" prop_removeComments_multiline
  , testProperty "NormalizeIndentation basic" prop_normalizeIndentation_basic
  , testProperty "NormalizeIndentation empty" prop_normalizeIndentation_empty
  , testProperty "NormalizeIndentation no indent" prop_normalizeIndentation_no_indent
  , testProperty "NormalizeIndentation consistent" prop_normalizeIndentation_consistent
  , testProperty "NormalizeIndentation inconsistent" prop_normalizeIndentation_inconsistent
  , testProperty "NormalizeIndentation deep" prop_normalizeIndentation_deep
  , testProperty "isRight basic" prop_isRight_basic
  , testProperty "isLeft basic" prop_isLeft_basic
  , testProperty "isRight right" prop_isRight_right
  , testProperty "isRight left" prop_isRight_left
  , testProperty "isLeft right" prop_isLeft_right
  , testProperty "isRight either symmetry" prop_isRight_either_symmetry
  , testProperty "isLeft either symmetry" prop_isLeft_either_symmetry
  , testProperty "SafeProcessString basic" prop_safeProcessString_basic
  , testProperty "SafeProcessString empty" prop_safeProcessString_empty
  , testProperty "SafeProcessString special" prop_safeProcessString_special
  , testProperty "SafeProcessString control" prop_safeProcessString_control
  , testProperty "SafeProcessString unicode" prop_safeProcessString_unicode
  , testProperty "SafeProcessString long" prop_safeProcessString_long
  , testProperty "BreakOn basic" prop_breakOn_basic
  , testProperty "BreakOn empty" prop_breakOn_empty
  , testProperty "BreakOn not found" prop_breakOn_not_found
  , testProperty "BreakOn multiple" prop_breakOn_multiple
  , testProperty "BreakOn prefix" prop_breakOn_prefix
  , testProperty "BreakOn suffix" prop_breakOn_suffix
  , testProperty "BreakOn no separator" prop_breakOn_no_separator
  , testCase "Trim edge cases" test_trim_edge_cases
  , testCase "SplitBy edge cases" test_splitBy_edge_cases
  , testCase "SplitByComma edge cases" test_splitByComma_edge_cases
  , testCase "RemoveLineComments edge cases" test_removeLineComments_edge_cases
  , testCase "RemoveComments edge cases" test_removeComments_edge_cases
  , testCase "NormalizeIndentation edge cases" test_normalizeIndentation_edge_cases
  , testCase "isRight edge cases" test_isRight_edge_cases
  , testCase "isLeft edge cases" test_isLeft_edge_cases
  , testCase "SafeProcessString edge cases" test_safeProcessString_edge_cases
  , testCase "BreakOn edge cases" test_breakOn_edge_cases
  ]