{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewCoreErrorHandlerQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements)

import ErrorHandler
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), formatError)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

-- Test 1: 测试错误创建的基本属性
prop_error_creation :: String -> ErrorSeverity -> Property
prop_error_creation msg severity =
  not (null msg) ==>
  let pos = startPos
      span = SourceSpan pos pos
      error = TypeError msg severity span
  in conjoin 
     [ property $ errorMessage error === msg
     , property $ errorSeverity error === severity
     , property $ errorLocation error === span
     ]

-- Test 2: 测试错误格式化
prop_error_formatting :: String -> ErrorSeverity -> Property
prop_error_formatting msg severity =
  not (null msg) ==>
  let pos = startPos
      span = SourceSpan pos pos
      error = TypeError msg severity span
      formatted = formatError error
  in conjoin 
     [ property $ msg `isInfixOf` formatted
     , property $ show severity `isInfixOf` formatted
     , property $ length formatted > length msg
     ]

-- Test 3: 测试错误严重性排序
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let pos = startPos
      span = SourceSpan pos pos
      error1 = TypeError "test1" sev1 span
      error2 = TypeError "test2" sev2 span
  in conjoin 
     [ property $ errorSeverity error1 === sev1
     , property $ errorSeverity error2 === sev2
     , sev1 === sev2 ==> property $ compare sev1 sev2 === EQ
     ]

-- Test 4: 测试错误位置信息
prop_error_location :: Positive Int -> Positive Int -> Positive Int -> 
                      Positive Int -> Positive Int -> Positive Int -> Property
prop_error_location (Positive startLine) (Positive startCol) (Positive startOffset)
                    (Positive endLine) (Positive endCol) (Positive endOffset) =
  let startPos' = SourcePos startLine startCol startOffset
      endPos' = SourcePos endLine endCol endOffset
      span = SourceSpan startPos' endPos'
      error = TypeError "test" ErrorError span
  in conjoin 
     [ property $ spanStart (errorLocation error) === startPos'
     , property $ spanEnd (errorLocation error) === endPos'
     ]

-- Test 5: 测试错误消息中的特殊字符处理
prop_error_special_chars :: String -> Property
prop_error_special_chars s =
  let specialChars = ['\0', '\n', '\t', '"', '\\', '\'']
      hasSpecialChars = any (`elem` specialChars) s
  in hasSpecialChars ==>
  let pos = startPos
      span = SourceSpan pos pos
      error = TypeError s ErrorWarning span
      formatted = formatError error
  in conjoin 
     [ property $ length formatted > 0
     , property $ not (null formatted)
     ]

-- Test 6: 测试错误处理的一致性
prop_error_consistency :: String -> ErrorSeverity -> String -> ErrorSeverity -> Property
prop_error_consistency msg1 sev1 msg2 sev2 =
  not (null msg1) && not (null msg2) ==>
  let pos = startPos
      span = SourceSpan pos pos
      error1 = TypeError msg1 sev1 span
      error2 = TypeError msg2 sev2 span
      formatted1 = formatError error1
      formatted2 = formatError error2
  in conjoin 
     [ property $ msg1 `isInfixOf` formatted1
     , property $ msg2 `isInfixOf` formatted2
     , property $ show sev1 `isInfixOf` formatted1
     , property $ show sev2 `isInfixOf` formatted2
     , sev1 === sev2 && msg1 === msg2 ==> property $ formatted1 === formatted2
     ]

-- 测试套件
tests :: TestTree
tests = testGroup "New Core ErrorHandler QuickCheck Tests"
  [ testProperty "Error creation" prop_error_creation
  , testProperty "Error formatting" prop_error_formatting
  , testProperty "Error severity ordering" prop_error_severity_ordering
  , testProperty "Error location" prop_error_location
  , testProperty "Error special chars" prop_error_special_chars
  , testProperty "Error consistency" prop_error_consistency
  ]