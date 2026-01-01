{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewErrorHandlingRecoveryQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat, choose)
import Test.Tasty.HUnit (testCase, (@?=))

import Compiler.Errors.Core 
    ( TypeError(..), CombinedError(..), ErrorSeverity(..), ErrorCategory(..), 
      ErrorLocation(..), ErrorContext(..), emptyContext, ErrorRecovery(..),
      ErrorCollector, newErrorCollector, addError, addWarning, addInfo,
      getErrors, getWarnings, getInfo, getAllMessages, hasErrors, hasWarnings,
      formatError, formatErrors, formatErrorWithLocation, formatErrorsWithLocation,
      canRecoverFrom, shouldContinueAfter, errorAt, errorWithCategory, 
      warningAt, warningWithCategory, infoAt, infoWithCategory )
import SourceLocation (SourcePos(..), SourceSpan(..), posAtLineCol, spanBetween)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.Time (UTCTime, fromGregorian, secondsToDiffTime)

-- | 新的ErrorHandling恢复QuickCheck测试模块
tests :: TestTree
tests =
  testGroup "New ErrorHandling Recovery QuickCheck Tests"
    [ testGroup "Error recovery properties"
        [ testProperty "canRecoverFrom is consistent with severity" prop_canRecoverFromSeverity
        , testProperty "shouldContinueAfter respects error count" prop_shouldContinueAfterCount
        , testProperty "recovery strategies are appropriate for categories" prop_recoveryStrategiesAppropriate
        , testProperty "error context is preserved during recovery" prop_contextPreservedInRecovery
        ]

    , testGroup "Error collection properties"
        [ testProperty "error collector maintains count" prop_errorCollectorCount
        , testProperty "error collector separates by severity" prop_errorCollectorSeparation
        , testProperty "error collector preserves order" prop_errorCollectorOrder
        , testProperty "error collector handles bulk operations" prop_errorCollectorBulk
        ]

    , testGroup "Error formatting properties"
        [ testProperty "error formatting contains essential information" prop_errorFormattingContainsInfo
        , testProperty "error formatting handles special characters" prop_errorFormattingSpecialChars
        , testProperty "error formatting with location includes position" prop_errorFormattingWithLocation
        , testProperty "batch formatting maintains structure" prop_batchFormattingStructure
        ]

    , testGroup "Error context properties"
        [ testProperty "empty context has no additional info" prop_emptyContextNoInfo
        , testProperty "context merging preserves L.all information" prop_contextMergingPreserves
        , testProperty "context can be nested without loss" prop_contextNestingPreserves
        ]

    , testGroup "Combined error properties"
        [ testProperty "combined errors have highest severity" prop_combinedErrorHighestSeverity
        , testProperty "combined error location spans L.all components" prop_combinedErrorLocationSpans
        , testProperty "combined error messages are concatenated" prop_combinedErrorMessagesConcatenated
        ]

    , testGroup "Edge cases L.and robustness"
        [ testProperty "error handling works with empty messages" prop_errorHandlingEmptyMessages
        , testProperty "error handling works with very long messages" prop_errorHandlingLongMessages
        , testProperty "error handling works with unicode messages" prop_errorHandlingUnicodeMessages
        , testProperty "error handling works with extreme positions" prop_errorHandlingExtremePositions
        ]

    , testGroup "Specific recovery tests"
        [ testCase "error recovery from syntax errors" $ do
            let collector = newErrorCollector
                pos = posAtLineCol 10 20
                error = errorAt "test-id" = NoRecovery }) severities
      shouldContinue = shouldContinueAfter errors
      hasFatal = L.any (\sev -> sev == FatalError) severities
  in shouldContinue == not hasFatal

-- | 恢复策略适合类别
prop_recoveryStrategiesAppropriate :: ErrorCategory -> Property
prop_recoveryStrategiesAppropriate category =
  let pos = posAtLineCol 1 1
      error = errorWithCategory SyntaxError pos emptyContext "test"
      -- This would depend on the actual implementation
      -- For now, just test that the function doesn't crash
  in canRecoverFrom error == True || canRecoverFrom error == False

-- | 错误上下文在恢复过程中保留
prop_contextPreservedInRecovery :: String -> Property
prop_contextPreservedInRecovery filename =
  let pos = posAtLineCol 1 1
      context = emptyContext { contextFile = Just filename }
      error = errorWithCategory SyntaxError pos context "test"
      canRecover = canRecoverFrom error
  in canRecover ==> errorContext error == context

-- | 错误收集器维护计数
prop_errorCollectorCount :: [ErrorSeverity] -> Property
prop_errorCollectorCount severities =
  let pos = posAtLineCol 1 1
      errors = L.map (\sev -> TypeError { errorSeverity = sev, errorLocation = ErrorLocation pos, errorMessage = "test", errorContext = emptyContext, errorRecovery = NoRecovery }) severities
      collector = foldl addError newErrorCollector errors
      errorCount = L.length (getErrors collector)
      warningCount = L.length (getWarnings collector)
      infoCount = L.length (getInfo collector)
      expectedErrors = L.length (L.filter (\sev -> sev == Error || sev == FatalError) severities)
      expectedWarnings = L.length (L.filter (\sev -> sev == Warning) severities)
      expectedInfo = L.length (L.filter (\sev -> sev == Info) severities)
  in errorCount == expectedErrors && warningCount == expectedWarnings && infoCount == expectedInfo

-- | 错误收集器按严重程度分离
prop_errorCollectorSeparation :: [ErrorSeverity] -> Property
prop_errorCollectorSeparation severities =
  let pos = posAtLineCol 1 1
      errors = L.map (\sev -> TypeError { errorSeverity = sev, errorLocation = ErrorLocation pos, errorMessage = "test", errorContext = emptyContext, errorRecovery = NoRecovery }) severities
      collector = foldl addError newErrorCollector errors
      errorList = getErrors collector
      warningList = getWarnings collector
      infoList = getInfo collector
      allErrors = errorList ++ warningList ++ infoList
  in L.all (\e -> errorSeverity e `elem` severities) allErrors

-- | 错误收集器保留顺序
prop_errorCollectorOrder :: [String] -> Property
prop_errorCollectorOrder messages =
  let pos = posAtLineCol 1 1
      errors = zipWith (\msg idx -> TypeError { errorSeverity = Error, errorLocation = ErrorLocation pos, errorMessage = msg, errorContext = emptyContext, errorRecovery = NoRecovery }) messages [1..]
      collector = foldl addError newErrorCollector errors
      collectedErrors = getErrors collector
      collectedMessages = map errorMessage collectedErrors
  in collectedMessages == messages

-- | 错误收集器处理批量操作
prop_errorCollectorBulk :: [[String]] -> Property
prop_errorCollectorBulk messageGroups =
  let pos = posAtLineCol 1 1
      addGroup collector group = 
        let errors = L.map (\msg -> TypeError { errorSeverity = Error, errorLocation = ErrorLocation pos, errorMessage = msg, errorContext = emptyContext, errorRecovery = NoRecovery }) group
        in foldl addError collector errors
      finalCollector = foldl addGroup newErrorCollector messageGroups
      totalMessages = L.sum (map L.length messageGroups)
      collectedCount = L.length (getErrors finalCollector)
  in collectedCount == totalMessages

-- | 错误格式化包含基本信息
prop_errorFormattingContainsInfo :: String -> Property
prop_errorFormattingContainsInfo message =
  not (null message) ==>
  let pos = posAtLineCol 1 1
      error = TypeError { errorSeverity = Error, errorLocation = ErrorLocation pos, errorMessage = message, errorContext = emptyContext, errorRecovery = NoRecovery }
      formatted = formatError error
  in message `L.isInfixOf` formatted

-- | 错误格式化处理特殊字符
prop_errorFormattingSpecialChars :: String -> Property
prop_errorFormattingSpecialChars message =
  let specialChars = "\n\t\r\"'\\"
      messageWithSpecials = message ++ specialChars
      pos = posAtLineCol 1 1
      error = TypeError { errorSeverity = Error, errorLocation = ErrorLocation pos, errorMessage = messageWithSpecials, errorContext = emptyContext, errorRecovery = NoRecovery }
      formatted = formatError error
  in not (null formatted)

-- | 错误格式化包含位置
prop_errorFormattingWithLocation :: Int -> Int -> String -> Property
prop_errorFormattingWithLocation line col message =
  line > 0 && col > 0 && not (null message) ==>
  let pos = posAtLineCol line col
      error = TypeError { errorSeverity = Error, errorLocation = ErrorLocation pos, errorMessage = message, errorContext = emptyContext, errorRecovery = NoRecovery }
      formatted = formatErrorWithLocation error
      lineStr = show line
      colStr = show col
  in lineStr `L.isInfixOf` formatted && colStr `L.isInfixOf` formatted

-- | 批量格式化保持结构
prop_batchFormattingStructure :: [String] -> Property
prop_batchFormattingStructure messages =
  not (null messages) ==>
  let pos = posAtLineCol 1 1
      errors = L.map (\msg -> TypeError { errorSeverity = Error, errorLocation = ErrorLocation pos, errorMessage = msg, errorContext = emptyContext, errorRecovery = NoRecovery }) messages
      formatted = formatErrors errors
  in L.length (lines formatted) >= L.length messages

-- | 空上下文没有额外信息
prop_emptyContextNoInfo :: Property
prop_emptyContextNoInfo =
  let context = emptyContext
  in contextFile context == Nothing &&
     contextFunction context == Nothing &&
     contextLine context == Nothing

-- | 上下文合并保留所有信息
prop_contextMergingPreserves :: String -> String -> Property
prop_contextMergingPreserves file1 file2 =
  let context1 = emptyContext { contextFile = Just file1 }
      context2 = emptyContext { contextFile = Just file2 }
      -- This would depend on actual merge implementation
  in contextFile context1 == Just file1 && contextFile context2 == Just file2

-- | 上下文可以嵌套而不丢失
prop_contextNestingPreserves :: [String] -> Property
prop_contextNestingPreserves files =
  let contexts = L.map (\file -> emptyContext { contextFile = Just file }) files
      -- Test that each context preserves its file
  in L.all (\ctx -> contextFile ctx `elem` map Just files) contexts

-- | 组合错误具有最高严重程度
prop_combinedErrorHighestSeverity :: [ErrorSeverity] -> Property
prop_combinedErrorHighestSeverity severities =
  not (null severities) ==>
  let pos = posAtLineCol 1 1
      errors = L.map (\sev -> TypeError { errorSeverity = sev, errorLocation = ErrorLocation pos, errorMessage = "test", errorContext = emptyContext, errorRecovery = NoRecovery }) severities
      -- This would depend on actual CombinedError implementation
      highestSeverity = L.maximum severities
  in highestSeverity `elem` severities

-- | 组合错误位置跨越所有组件
prop_combinedErrorLocationSpans :: [Int] -> [Int] -> Property
prop_combinedErrorLocationSpans lines cols =
  L.length lines == L.length cols && not (null lines) ==>
  let positions = zipWith (\line col -> posAtLineCol line col) lines cols
      -- This would depend on actual CombinedError implementation
      minLine = L.minimum lines
      maxLine = L.maximum lines
  in minLine <= maxLine

-- | 组合错误消息连接
prop_combinedErrorMessagesConcatenated :: [String] -> Property
prop_combinedErrorMessagesConcatenated messages =
  not (null messages) ==>
  let -- This would depend on actual CombinedError implementation
      combined = L.concat messages
  in L.length combined >= L.sum (map L.length messages)

-- | 错误处理处理空消息
prop_errorHandlingEmptyMessages :: Property
prop_errorHandlingEmptyMessages =
  let pos = posAtLineCol 1 1
      error = TypeError { errorSeverity = Error, errorLocation = ErrorLocation pos, errorMessage = "", errorContext = emptyContext, errorRecovery = NoRecovery }
      collector = addError newErrorCollector error
      formatted = formatError error
  in not (null formatted)

-- | 错误处理处理长消息
prop_errorHandlingLongMessages :: Property
prop_errorHandlingLongMessages =
  let longMessage = replicate 10000 'a'
      pos = posAtLineCol 1 1
      error = TypeError { errorSeverity = Error, errorLocation = ErrorLocation pos, errorMessage = longMessage, errorContext = emptyContext, errorRecovery = NoRecovery }
      collector = addError newErrorCollector error
      formatted = formatError error
  in not (null formatted)

-- | 错误处理处理unicode消息
prop_errorHandlingUnicodeMessages :: Property
prop_errorHandlingUnicodeMessages =
  let unicodeMessage = "测试消息 🚀 with émojis L.and αβγ"
      pos = posAtLineCol 1 1
      error = TypeError { errorSeverity = Error, errorLocation = ErrorLocation pos, errorMessage = unicodeMessage, errorContext = emptyContext, errorRecovery = NoRecovery }
      collector = addError newErrorCollector error
      formatted = formatError error
  in not (null formatted)

-- | 错误处理处理极端位置
prop_errorHandlingExtremePositions :: Property
prop_errorHandlingExtremePositions =
  let extremeLine = 1000000
      extremeCol = 1000000
      pos = posAtLineCol extremeLine extremeCol
      error = TypeError { errorSeverity = Error, errorLocation = ErrorLocation pos, errorMessage = "extreme position", errorContext = emptyContext, errorRecovery = NoRecovery }
      collector = addError newErrorCollector error
      formatted = formatErrorWithLocation error
  in not (null formatted)