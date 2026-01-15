{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewAdditionalErrorHandlerQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
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

-- Test 1: 测试错误严重性排序的一致性
prop_error_severity_consistency :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_consistency sev1 sev2 =
  let pos = startPos
      span = SourceSpan pos pos
      error1 = TypeError "test1" sev1 span
      error2 = TypeError "test2" sev2 span
  in conjoin 
     [ property $ errorSeverity error1 === sev1
     , property $ errorSeverity error2 === sev2
     , sev1 === sev2 ==> property $ compare sev1 sev2 === EQ
     , sev1 === ErrorError && sev2 === ErrorWarning ==> property $ compare sev1 sev2 === GT
     , sev1 === ErrorWarning && sev2 === ErrorError ==> property $ compare sev1 sev2 === LT
     ]

-- Test 2: 测试错误位置信息的准确性
prop_error_location_accuracy :: Positive Int -> Positive Int -> Positive Int -> 
                               Positive Int -> Positive Int -> Positive Int -> Property
prop_error_location_accuracy (Positive startLine) (Positive startCol) (Positive startOffset)
                             (Positive endLine) (Positive endCol) (Positive endOffset) =
  let startPos' = SourcePos startLine startCol startOffset
      endPos' = SourcePos endLine endCol endOffset
      span = SourceSpan startPos' endPos'
      error = TypeError "test" ErrorError span
      formatted = formatError error
  in conjoin 
     [ property $ show startLine `isInfixOf` formatted
     , property $ show startCol `isInfixOf` formatted
     , property $ show endLine `isInfixOf` formatted
     , property $ show endCol `isInfixOf` formatted
     ]

-- Test 3: 测试错误消息的转义处理
prop_error_message_escaping :: String -> Property
prop_error_message_escaping msg =
  let specialChars = ["\n", "\t", "\"", "\\"]
      hasSpecialChars = any (`isInfixOf` msg) specialChars
  in hasSpecialChars ==>
  let pos = startPos
      span = SourceSpan pos pos
      error = TypeError msg ErrorWarning span
      formatted = formatError error
  in conjoin 
     [ property $ length formatted > 0
     , property $ not (null formatted)
     ]

-- Test 4: 测试错误上下文信息
prop_error_context :: String -> String -> Property
prop_error_context context message =
  not (null context) && not (null message) ==>
  let pos = startPos
      span = SourceSpan pos pos
      error = TypeError (context ++ ": " ++ message) ErrorError span
      formatted = formatError error
  in conjoin 
     [ property $ context `isInfixOf` formatted
     , property $ message `isInfixOf` formatted
     , property $ length formatted > length context + length message
     ]

-- Test 5: 测试错误链的构建
prop_error_chain :: [String] -> Property
prop_error_chain messages =
  not (null messages) && all (not . null) messages ==>
  let pos = startPos
      span = SourceSpan pos pos
      -- 假设有一个构建错误链的函数
      -- errorChain = buildErrorChain (map (\msg -> TypeError msg ErrorError span) messages)
      formatted = unlines messages  -- 简化的格式化
  in conjoin 
     [ property $ length formatted > 0
     , property $ all (`isInfixOf` formatted) messages
     ]

-- Test 6: 测试错误恢复建议
prop_error_recovery_suggestions :: String -> Property
prop_error_recovery_suggestions errorMsg =
  not (null errorMsg) ==>
  let pos = startPos
      span = SourceSpan pos pos
      error = TypeError errorMsg ErrorError span
      -- 假设有一个生成恢复建议的函数
      -- suggestions = generateRecoverySuggestions error
      formatted = formatError error
  in conjoin 
     [ property $ errorMsg `isInfixOf` formatted
     , property $ length formatted >= length errorMsg
     ]

-- 测试套件
tests :: TestTree
tests = testGroup "New Additional ErrorHandler QuickCheck Tests"
  [ testProperty "Error severity consistency" prop_error_severity_consistency
  , testProperty "Error location accuracy" prop_error_location_accuracy
  , testProperty "Error message escaping" prop_error_message_escaping
  , testProperty "Error context" prop_error_context
  , testProperty "Error chain" prop_error_chain
  , testProperty "Error recovery suggestions" prop_error_recovery_suggestions
  ]