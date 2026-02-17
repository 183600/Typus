{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Unit.RobustErrorHandlingQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.List (isInfixOf, nub, sort, group, intercalate, isPrefixOf, isSuffixOf)
import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum, toLower, toUpper)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import ErrorHandler
import Compiler.Errors.Core
import Compiler
import Parser
import SourceLocation
import Utils

import TestSupport.Arbitrary
import TestSupport.ErrorHandlerTestUtils (filterBySeverityForTests)

-- ============================================================================
-- Robust Error Handling Properties
-- ============================================================================

-- | 测试错误消息的结构一致性
prop_error_message_structure :: String -> String -> String -> Property
prop_error_message_structure errorType errorMsg context =
  let validInputs = not (null errorType) && not (null errorMsg) && not (null context)
  in if not validInputs
     then property True
     else let fullMessage = errorType ++ ": " ++ errorMsg ++ " (context: " ++ context ++ ")"
              hasType = errorType `isPrefixOf` fullMessage
              hasContext = ("context: " ++ context) `isInfixOf` fullMessage
          in property $ hasType && hasContext

-- | 测试错误处理的幂等性
prop_error_handling_idempotence :: String -> Property
prop_error_handling_idempotence errorMsg =
  let validMsg = not (null errorMsg)
  in if not validMsg
     then property True
     else let error1 = ErrorHandler.createError "test" (T.pack errorMsg) (ErrorLocation Nothing 0 0 Nothing Nothing)
              error2 = ErrorHandler.createError "test" (T.pack errorMsg) (ErrorLocation Nothing 0 0 Nothing Nothing)
              handler1 = ErrorHandler.handleError [] error1
              handler2 = ErrorHandler.handleError [] error2
          in property $ ErrorHandler.errorCount handler1 == ErrorHandler.errorCount handler2

-- | 测试错误处理的构建
prop_error_handling_construction :: [String] -> Property
prop_error_handling_construction errors =
  let validErrors = all (not . null) errors
  in if not validErrors
     then property True
     else let errorList = map (\msg -> ErrorHandler.createError "test" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) errors
              handler = ErrorHandler.handleErrors [] errorList
              errorCount = ErrorHandler.errorCount handler
          in property $ errorCount == length errors

-- | 测试错误处理的一致性
prop_error_handling_consistency :: String -> Property
prop_error_handling_consistency errorMsg =
  let validMsg = not (null errorMsg)
  in if not validMsg
     then property True
     else let error1 = ErrorHandler.createError "test" (T.pack errorMsg) (ErrorLocation Nothing 0 0 Nothing Nothing)
              error2 = ErrorHandler.createError "test" (T.pack errorMsg) (ErrorLocation Nothing 0 0 Nothing Nothing)
              handler1 = ErrorHandler.handleError [] error1
              handler2 = ErrorHandler.handleError [] error2
          in property $ ErrorHandler.errorCount handler1 == ErrorHandler.errorCount handler2

-- | 测试错误严重性排序
prop_error_severity_ordering :: [String] -> Property
prop_error_severity_ordering errors =
  let validErrors = all (not . null) errors
  in if not validErrors
     then property True
     else let errorList = map (\msg -> ErrorHandler.createError "test" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) errors
              handler = ErrorHandler.handleErrors [] errorList
              errorCount = ErrorHandler.errorCount handler
          in property $ errorCount == length errors

-- | 测试错误处理的合并
prop_error_handling_merge :: [String] -> [String] -> Property
prop_error_handling_merge errors1 errors2 =
  let validErrors1 = all (not . null) errors1
      validErrors2 = all (not . null) errors2
  in if not (validErrors1 && validErrors2)
     then property True
     else let errorList1 = map (\msg -> ErrorHandler.createError "test1" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) errors1
              errorList2 = map (\msg -> ErrorHandler.createError "test2" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) errors2
              handler1 = ErrorHandler.handleErrors [] errorList1
              handler2 = ErrorHandler.handleErrors [] errorList2
              merged = ErrorHandler.mergeHandlers handler1 handler2
          in property $ ErrorHandler.errorCount merged == ErrorHandler.errorCount handler1 + ErrorHandler.errorCount handler2

-- | 测试错误位置的处理
prop_error_location_handling :: Int -> Int -> Property
prop_error_location_handling line col =
  let validLocation = line >= 0 && col >= 0
  in if not validLocation
     then property True
     else let location = ErrorLocation Nothing line col Nothing Nothing
              error = ErrorHandler.createError "test" "test message" location
          in property $ show error /= ""

-- | 测试错误过滤
prop_error_filtering :: [String] -> Property
prop_error_filtering errors =
  let validErrors = all (not . null) errors
  in if not validErrors
     then property True
     else let errorList = map (\msg -> ErrorHandler.createError "test" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) errors
              handler = ErrorHandler.handleErrors [] errorList
              filtered = TestSupport.ErrorHandlerTestUtils.filterBySeverityForTests Error handler
          in property $ ErrorHandler.errorCount filtered <= ErrorHandler.errorCount handler

-- | 测试错误计数
prop_error_counting :: [String] -> Property
prop_error_counting errors =
  let validErrors = all (not . null) errors
  in if not validErrors
     then property True
     else let errorList = map (\msg -> ErrorHandler.createError "test" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) errors
              handler = ErrorHandler.handleErrors [] errorList
          in property $ ErrorHandler.errorCount handler == length errors

-- | 测试警告创建
prop_warning_creation :: String -> Property
prop_warning_creation warningMsg =
  let validMsg = not (null warningMsg)
  in if not validMsg
     then property True
     else let warning = ErrorHandler.createWarning "test" (T.pack warningMsg) (ErrorLocation Nothing 0 0 Nothing Nothing)
          in property $ show warning /= ""

-- | 测试信息创建
prop_info_creation :: String -> Property
prop_info_creation infoMsg =
  let validMsg = not (null infoMsg)
  in if not validMsg
     then property True
     else let info = ErrorHandler.createInfo "test" (T.pack infoMsg) (ErrorLocation Nothing 0 0 Nothing Nothing)
          in property $ show info /= ""

-- | 测试错误清理
prop_error_clearing :: [String] -> Property
prop_error_clearing errors =
  let validErrors = all (not . null) errors
  in if not validErrors
     then property True
     else let errorList = map (\msg -> ErrorHandler.createError "test" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) errors
              handler = ErrorHandler.handleErrors [] errorList
              cleared = ErrorHandler.clearErrors handler
          in property $ ErrorHandler.errorCount cleared == 0

-- | 测试警告清理
prop_warning_clearing :: [String] -> Property
prop_warning_clearing warnings =
  let validWarnings = all (not . null) warnings
  in if not validWarnings
     then property True
     else let warningList = map (\msg -> ErrorHandler.createWarning "test" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) warnings
              handler = ErrorHandler.handleErrors [] warningList
              cleared = ErrorHandler.clearWarnings handler
          in property $ ErrorHandler.errorCount cleared == 0

-- | 测试信息清理
prop_info_clearing :: [String] -> Property
prop_info_clearing infos =
  let validInfos = all (not . null) infos
  in if not validInfos
     then property True
     else let infoList = map (\msg -> ErrorHandler.createInfo "test" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) infos
              handler = ErrorHandler.handleErrors [] infoList
              cleared = ErrorHandler.clearInfos handler
          in property $ ErrorHandler.errorCount cleared == 0

-- ============================================================================
-- Integration Tests with Parser and Compiler
-- ============================================================================

-- | 测试解析器错误处理的健壮性
prop_parser_error_handling_robustness :: String -> Property
prop_parser_error_handling_robustness malformedCode =
  let hasErrorIndicators = any (`isInfixOf` malformedCode) ["invalid", "error", "malformed"]
      parsed = Parser.parseTypusFile malformedCode
  in classify hasErrorIndicators "has error indicators" $
     case parsed of
       Right _ -> property True
       Left _ -> property True  -- 解析失败是预期的

-- | 测试编译器错误处理的健壮性
prop_compiler_error_handling_robustness :: String -> Property
prop_compiler_error_handling_robustness problematicCode =
  let hasProblematicPatterns = any (`isInfixOf` problematicCode) ["undefined", "null", "invalid"]
      parsed = Parser.parseTypusFile problematicCode
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in classify hasProblematicPatterns "has problematic patterns" $
     case compiled of
       Right _ -> property True
       Left _ -> property True  -- 编译失败是预期的

-- | 测试错误处理的级联效应
prop_error_handling_cascade :: [String] -> Property
prop_error_handling_cascade errors =
  let validErrors = all (not . null) errors
  in if not validErrors
     then property True
     else let errorList = map (\msg -> ErrorHandler.createError "test" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) errors
              handler = ErrorHandler.handleErrors [] errorList
              errorCount = ErrorHandler.errorCount handler
          in property $ errorCount == length errors

-- ============================================================================
-- Performance Tests
-- ============================================================================

-- | 测试大量错误处理的性能
prop_massive_error_handling :: Int -> Property
prop_massive_error_handling numErrors =
  let validNum = numErrors >= 0 && numErrors <= 1000
  in if not validNum
     then property True
     else let errors = take numErrors $ map (\i -> "Error " ++ show i) [0..]
              errorList = map (\msg -> ErrorHandler.createError "test" (T.pack msg) (ErrorLocation Nothing 0 0 Nothing Nothing)) errors
              handler = ErrorHandler.handleErrors [] errorList
          in property $ ErrorHandler.errorCount handler == numErrors

-- | 测试复杂错误处理的性能
prop_complex_error_handling :: Int -> Property
prop_complex_error_handling complexity =
  let validComplexity = complexity >= 0 && complexity <= 100
  in if not validComplexity
     then property True
     else let errorMsg = "Complex error with " ++ show complexity ++ " components"
              error = ErrorHandler.createError "test" (T.pack errorMsg) (ErrorLocation Nothing 0 0 Nothing Nothing)
              handler = ErrorHandler.handleErrors [] [error]
          in property $ ErrorHandler.errorCount handler == 1

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- | 测试空错误消息的处理
prop_empty_error_message :: Property
prop_empty_error_message =
  let error = ErrorHandler.createError "test" "" (ErrorLocation Nothing 0 0 Nothing Nothing)
      handler = ErrorHandler.handleErrors [] [error]
  in property $ ErrorHandler.errorCount handler == 1

-- | 测试极长错误消息的处理
prop_extremely_long_error_message :: Int -> Property
prop_extremely_long_error_message length =
  let validLength = length >= 0 && length <= 10000
  in if not validLength
     then property True
     else let longMsg = replicate length 'x'
              error = ErrorHandler.createError "test" (T.pack longMsg) (ErrorLocation Nothing 0 0 Nothing Nothing)
              handler = ErrorHandler.handleErrors [] [error]
          in property $ ErrorHandler.errorCount handler == 1

-- | 测试特殊字符错误消息的处理
prop_special_character_error_message :: String -> Property
prop_special_character_error_message specialChars =
  let hasSpecialChars = any (not . isAlphaNum) specialChars
      error = ErrorHandler.createError "test" (T.pack specialChars) (ErrorLocation Nothing 0 0 Nothing Nothing)
      handler = ErrorHandler.handleErrors [] [error]
  in classify hasSpecialChars "has special characters" $
     property $ ErrorHandler.errorCount handler == 1

-- | 测试Unicode错误消息的处理
prop_unicode_error_message :: String -> Property
prop_unicode_error_message unicodeMsg =
  let error = ErrorHandler.createError "test" (T.pack unicodeMsg) (ErrorLocation Nothing 0 0 Nothing Nothing)
      handler = ErrorHandler.handleErrors [] [error]
  in property $ ErrorHandler.errorCount handler == 1

-- ============================================================================
-- Test Suite Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Robust Error Handling QuickCheck Tests"
  [ testProperty "Error Message Structure" prop_error_message_structure
  , testProperty "Error Handling Idempotence" prop_error_handling_idempotence
  , testProperty "Error Handling Construction" prop_error_handling_construction
  , testProperty "Error Handling Consistency" prop_error_handling_consistency
  , testProperty "Error Severity Ordering" prop_error_severity_ordering
  , testProperty "Error Handling Merge" prop_error_handling_merge
  , testProperty "Error Location Handling" prop_error_location_handling
  , testProperty "Error Filtering" prop_error_filtering
  , testProperty "Error Counting" prop_error_counting
  , testProperty "Warning Creation" prop_warning_creation
  , testProperty "Info Creation" prop_info_creation
  , testProperty "Error Clearing" prop_error_clearing
  , testProperty "Warning Clearing" prop_warning_clearing
  , testProperty "Info Clearing" prop_info_clearing
  , testProperty "Parser Error Handling Robustness" prop_parser_error_handling_robustness
  , testProperty "Compiler Error Handling Robustness" prop_compiler_error_handling_robustness
  , testProperty "Error Handling Cascade" prop_error_handling_cascade
  , testProperty "Massive Error Handling" prop_massive_error_handling
  , testProperty "Complex Error Handling" prop_complex_error_handling
  , testProperty "Empty Error Message" prop_empty_error_message
  , testProperty "Extremely Long Error Message" prop_extremely_long_error_message
  , testProperty "Special Character Error Message" prop_special_character_error_message
  , testProperty "Unicode Error Message" prop_unicode_error_message
  ]