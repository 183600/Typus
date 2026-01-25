{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewAdditionalUtilsQuickCheckSpec where



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

-- Test 1: 测试trim函数的Unicode处理
prop_trim_unicode :: String -> Property
prop_trim_unicode s =
  let trimmed = trim s
      unicodeChars = filter (> '\127') s  -- 获取Unicode字符
  in conjoin 
     [ property $ length trimmed <= length s
     , not (null unicodeChars) ==> property $ length trimmed >= 0
     , property $ trim (trim s) === trimmed  -- 幂等性
     ]

-- Test 2: 测试splitBy对Unicode分隔符的处理
prop_splitBy_unicode :: String -> Property
prop_splitBy_unicode s =
  let unicodeDelimiter = '，'  -- 中文逗号
      parts = splitBy unicodeDelimiter s
  in conjoin 
     [ property $ concat parts === s
     , property $ length parts >= 1
     , not (unicodeDelimiter `elem` s) ==> property $ parts === [s]
     ]

-- Test 3: 测试removeLineComments对多行字符串的处理
prop_removeLineComments_multiline_strings :: String -> String -> Property
prop_removeLineComments_multiline_strings code comment =
  let codeWithMultilineString = code ++ "\nlet x = \"\"\"\n多行字符串\n// 不是注释\n\"\"\"\n// " ++ comment
      withoutComments = removeLineComments codeWithMultilineString
  in conjoin 
     [ property $ not $ comment `isInfixOf` withoutComments
     , property $ "多行字符串" `isInfixOf` withoutComments
     , property $ "不是注释" `isInfixOf` withoutComments
     ]

-- Test 4: 测试removeComments对嵌套块注释的处理
prop_removeComments_nested_blocks :: String -> String -> String -> Property
prop_removeComments_nested_blocks outer inner code =
  let nestedComment = "/* " ++ outer ++ " /* " ++ inner ++ " */ " ++ outer ++ " */"
      codeWithComment = code ++ nestedComment ++ code
      withoutComments = removeComments codeWithComment
  in conjoin 
     [ property $ not $ "/*" `isInfixOf` withoutComments
     , property $ not $ "*/" `isInfixOf` withoutComments
     , property $ code `isPrefixOf` withoutComments
     , property $ code `isSuffixOf` withoutComments
     ]

-- Test 5: 测试normalizeIndentation对混合缩进的处理
prop_normalizeIndentation_mixed :: String -> Property
prop_normalizeIndentation_mixed s =
  let mixedIndentCode = "  " ++ s ++ "\n\t" ++ s ++ "\n    " ++ s ++ "\n" ++ s
      normalized = normalizeIndentation mixedIndentCode
      lines' = lines normalized
  in case lines' of
        (first:_) -> conjoin 
                     [ property $ length lines' === 4
                     , property $ all (\line -> not (all isSpace (takeWhile isSpace line))) lines'
                     , property $ s `isInfixOf` first
                     ]
        [] -> property False

-- Test 6: 测试safeProcessString对特殊字符的处理
prop_safeProcessString_special :: String -> Property
prop_safeProcessString_special s =
  let specialChars = ['\0', '\1', '\2', '\3', '\4', '\5', '\6', '\7', '\8', '\11', '\12', '\14', '\15', '\16', '\17', '\18', '\19', '\20', '\21', '\22', '\23', '\24', '\25', '\26', '\27', '\28', '\29', '\30', '\31']
      hasSpecialChars = any (`elem` specialChars) s
  in hasSpecialChars ==>
  let processed = safeProcessString s
  in case processed of
       Right p -> property $ length p >= 0
       Left _ -> property True

-- 测试套件
tests :: TestTree
tests = testGroup "New Additional Utils QuickCheck Tests"
  [ testProperty "Trim unicode" prop_trim_unicode
  , testProperty "SplitBy unicode" prop_splitBy_unicode
  , testProperty "RemoveLineComments multiline strings" prop_removeLineComments_multiline_strings
  , testProperty "RemoveComments nested blocks" prop_removeComments_nested_blocks
  , testProperty "NormalizeIndentation mixed" prop_normalizeIndentation_mixed
  , testProperty "SafeProcessString special" prop_safeProcessString_special
  ]