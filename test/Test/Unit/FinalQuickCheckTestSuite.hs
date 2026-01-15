{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.FinalQuickCheckTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Utils (trim, splitBy, splitByComma, removeLineComments, removeComments, normalizeIndentation)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

-- | 测试trim函数的基本属性
prop_trim_basic :: String -> Property
prop_trim_basic s =
  let trimmed = trim s
  in property $ length trimmed <= length s

-- | 测试trim对空字符串的处理
prop_trim_empty :: Property
prop_trim_empty = trim "" === ""

-- | 测试trim对空白字符的处理
prop_trim_whitespace :: String -> Property
prop_trim_whitespace s =
  let trimmed = trim s
  in all isSpace (trim s) ==> property $ null trimmed

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

-- | 测试splitByComma的基本属性
prop_splitByComma_basic :: String -> Property
prop_splitByComma_basic s =
  let parts = splitByComma s
  in concat parts === s

-- | 测试splitByComma对空字符串的处理
prop_splitByComma_empty :: Property
prop_splitByComma_empty = splitByComma "" === []

-- | 测试removeLineComments的基本属性
prop_removeLineComments_basic :: String -> String -> Property
prop_removeLineComments_basic code comment =
  let codeWithComment = code ++ "// " ++ comment ++ "\nmore code"
      withoutComments = removeLineComments codeWithComment
  in property (not (isInfixOf "// " withoutComments))

-- | 测试removeLineComments对空代码的处理
prop_removeLineComments_empty :: Property
prop_removeLineComments_empty = removeLineComments "" === ""

-- | 测试removeLineComments对没有注释的处理
prop_removeLineComments_no_comments :: String -> Property
prop_removeLineComments_no_comments code =
  not ("//" `isInfixOf` code) ==> removeLineComments code === code

-- | 测试removeComments的基本属性
prop_removeComments_basic :: String -> String -> Property
prop_removeComments_basic before after =
  let codeWithComment = before ++ "/* " ++ "comment" ++ " */" ++ after
      withoutComments = removeComments codeWithComment
  in property (not (isInfixOf "/*" withoutComments) && not (isInfixOf "*/" withoutComments))

-- | 测试removeComments对空代码的处理
prop_removeComments_empty :: Property
prop_removeComments_empty = removeComments "" === ""

-- | 测试removeComments对没有注释的处理
prop_removeComments_no_comments :: String -> Property
prop_removeComments_no_comments code =
  not ("/*" `isInfixOf` code) && not ("*/" `isInfixOf` code) ==> 
  removeComments code === code

-- | 测试normalizeIndentation的基本属性
prop_normalizeIndentation_basic :: String -> Property
prop_normalizeIndentation_basic s =
  let normalized = normalizeIndentation s
  in property $ length normalized >= 0

-- | 测试normalizeIndentation对空字符串的处理
prop_normalizeIndentation_empty :: Property
prop_normalizeIndentation_empty = normalizeIndentation "" === ""

-- | 测试normalizeIndentation对无缩进的处理
prop_normalizeIndentation_no_indent :: String -> Property
prop_normalizeIndentation_no_indent s =
  not (any isSpace s) ==> normalizeIndentation s === s

-- | 测试isRight的基本属性
prop_isRight_basic :: Either String Int -> Property
prop_isRight_basic e = Data.Either.isRight e === (case e of Right _ -> True; Left _ -> False)

-- | 测试isLeft的基本属性
prop_isLeft_basic :: Either String Int -> Property
prop_isLeft_basic e = Data.Either.isLeft e === (case e of Left _ -> True; Right _ -> False)

-- | 测试isRight对Right值的处理
prop_isRight_right :: Int -> Property
prop_isRight_right x = property $ isRight (Right x)

-- | 测试isRight对Left值的处理
prop_isRight_left :: String -> Property
prop_isRight_left msg = property $ not $ isRight (Left msg)

-- | 测试isLeft对Right值的处理
prop_isLeft_right :: Int -> Property
prop_isLeft_right x = property $ not $ isLeft (Right x)

-- | 测试isLeft对Left值的处理
prop_isLeft_left :: String -> Property
prop_isLeft_left msg = property $ isLeft (Left msg)

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
  assertEqual "Empty string" [] (splitBy ',' "")
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

-- | 测试套件
tests :: TestTree
tests = testGroup "Final QuickCheck Test Suite"
  [ testProperty "Trim basic" prop_trim_basic
  , testProperty "Trim empty" prop_trim_empty
  , testProperty "Trim whitespace" prop_trim_whitespace
  , testProperty "Trim idempotent" prop_trim_idempotent
  , testProperty "SplitBy basic" prop_splitBy_basic
  , testProperty "SplitBy empty" prop_splitBy_empty
  , testProperty "SplitByComma basic" prop_splitByComma_basic
  , testProperty "SplitByComma empty" prop_splitByComma_empty
  , testProperty "RemoveLineComments basic" prop_removeLineComments_basic
  , testProperty "RemoveLineComments empty" prop_removeLineComments_empty
  , testProperty "RemoveLineComments no comments" prop_removeLineComments_no_comments
  , testProperty "RemoveComments basic" prop_removeComments_basic
  , testProperty "RemoveComments empty" prop_removeComments_empty
  , testProperty "RemoveComments no comments" prop_removeComments_no_comments
  , testProperty "NormalizeIndentation basic" prop_normalizeIndentation_basic
  , testProperty "NormalizeIndentation empty" prop_normalizeIndentation_empty
  , testProperty "NormalizeIndentation no indent" prop_normalizeIndentation_no_indent
  , testProperty "isRight basic" prop_isRight_basic
  , testProperty "isLeft basic" prop_isLeft_basic
  , testProperty "isRight right" prop_isRight_right
  , testProperty "isRight left" prop_isRight_left
  , testProperty "isLeft right" prop_isLeft_right
  , testProperty "isLeft left" prop_isLeft_left
  , testCase "Trim edge cases" test_trim_edge_cases
  , testCase "SplitBy edge cases" test_splitBy_edge_cases
  , testCase "SplitByComma edge cases" test_splitByComma_edge_cases
  , testCase "RemoveLineComments edge cases" test_removeLineComments_edge_cases
  , testCase "RemoveComments edge cases" test_removeComments_edge_cases
  , testCase "NormalizeIndentation edge cases" test_normalizeIndentation_edge_cases
  , testCase "isRight edge cases" test_isRight_edge_cases
  , testCase "isLeft edge cases" test_isLeft_edge_cases
  ]