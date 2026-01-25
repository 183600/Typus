{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewCoreUtilsQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, safeProcessString, 
             breakOn, isRight, isValidChar)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

-- Test 1: 测试trim函数的复合属性
prop_trim_comprehensive :: String -> Property
prop_trim_comprehensive s =
  let trimmed = trim s
      trimmedTwice = trim trimmed
  in conjoin 
     [ property $ length trimmed <= length s
     , property $ trimmed === trimmedTwice  -- 幂等性
     , null s ==> null trimmed
     , all isSpace s ==> null trimmed
     , not (null s) && not (all isSpace s) ==> property $ length trimmed > 0
     ]

-- Test 2: 测试splitBy和splitByCollapsed的关系
prop_splitBy_relationship :: Char -> String -> Property
prop_splitBy_relationship c s =
  let parts = splitBy c s
      collapsed = splitByCollapsed c s
  in conjoin 
     [ property $ concat parts === s
     , property $ length collapsed <= length parts
     , property $ all (not . null) collapsed  -- collapsed不应有空段
     ]

-- Test 3: 测试splitByComma和splitByCommaCollapsed的一致性
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency s =
  let parts = splitByComma s
      collapsed = splitByCommaCollapsed s
  in conjoin 
     [ property $ concat parts === s
     , property $ length collapsed <= length parts
     , property $ not (',' `elem` s) ==> (parts == [s] && collapsed == [s])
     ]

-- Test 4: 测试removeLineComments的字符串字面量处理
prop_removeLineComments_string_literals :: String -> String -> Property
prop_removeLineComments_string_literals code comment =
  let codeWithString = code ++ " // " ++ comment ++ "\nlet x = \"// not a comment\""
      withoutComments = removeLineComments codeWithString
  in conjoin 
     [ property $ not $ "// comment" `isInfixOf` withoutComments
     , property $ "// not a comment" `isInfixOf` withoutComments
     ]

-- Test 5: 测试removeComments的嵌套注释处理
prop_removeComments_nested :: String -> String -> String -> Property
prop_removeComments_nested before middle after =
  let nestedComment = "/* outer /* inner */ still outer */"
      codeWithComment = before ++ nestedComment ++ middle ++ "/* simple */" ++ after
      withoutComments = removeComments codeWithComment
  in conjoin 
     [ property $ not $ "/* outer" `isInfixOf` withoutComments
     , property $ not $ "/* simple */" `isInfixOf` withoutComments
     , property $ before `isPrefixOf` withoutComments
     , property $ after `isSuffixOf` withoutComments
     ]

-- Test 6: 测试normalizeIndentation的相对缩进保持
prop_normalizeIndentation_relative :: String -> String -> Property
prop_normalizeIndentation_relative s1 s2 =
  let indentedCode = "  " ++ s1 ++ "\n    " ++ s2 ++ "\n  " ++ s1
      normalized = normalizeIndentation indentedCode
      lines' = lines normalized
  in case lines' of
        (first:second:rest) -> conjoin 
                               [ property $ length lines' === 3
                               , property $ all (\line -> not (all isSpace (takeWhile isSpace line))) lines'
                               , property $ s1 `isInfixOf` first
                               , property $ s2 `isInfixOf` second
                               , property $ s1 `isInfixOf` (last (first:second:rest))
                               ]
        _ -> property False

-- 测试套件
tests :: TestTree
tests = testGroup "New Core Utils QuickCheck Tests"
  [ testProperty "Trim comprehensive" prop_trim_comprehensive
  , testProperty "SplitBy relationship" prop_splitBy_relationship
  , testProperty "SplitByComma consistency" prop_splitByComma_consistency
  , testProperty "RemoveLineComments string literals" prop_removeLineComments_string_literals
  , testProperty "RemoveComments nested" prop_removeComments_nested
  , testProperty "NormalizeIndentation relative" prop_normalizeIndentation_relative
  ]