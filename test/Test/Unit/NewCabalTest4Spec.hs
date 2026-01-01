{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTest4Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , emptyContext
  , ErrorRecovery(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getErrors
  , getWarnings
  , getInfo
  , getAllMessages
  , hasErrors
  , hasWarnings
  , formatError
  , formatErrors
  , formatErrorWithLocation
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , infoWithCategory
  )
import SourceLocation (SourcePos(..), startPos)

-- | 测试错误处理的属性和不变性
tests :: TestTree
tests =
  testGroup "NewCabalTest4 - 错误处理属性测试"
    [ testGroup "单元测试"
        [ testCase "错误收集器的基本功能" $ do
            collector <- newErrorCollector
            addError collector "Test error" ErrorSeverityError startPos
            addWarning collector "Test warning" startPos
            assertBool "Should have errors" $ hasErrors collector
            assertBool "Should have warnings" $ hasWarnings collector

        , testCase "错误格式化的正确性" $ do
            let error = TypeError "Test message" ErrorSeverityError ErrorCategoryTypeCheck 
                               (ErrorLocation 1 1 Nothing Nothing) emptyContext
                formatted = formatError error
            assertBool "Should contain error message" $ "Test message" `L.isInfixOf` formatted

        , testCase "错误恢复机制" $ do
            let recoverableError = TypeError "Recoverable" ErrorSeverityWarning ErrorCategorySyntax 
                                           (ErrorLocation 1 1 Nothing Nothing) emptyContext
                nonRecoverableError = TypeError "Critical" ErrorSeverityError ErrorCategoryTypeCheck 
                                             (ErrorLocation 1 1 Nothing Nothing) emptyContext
            assertBool "Should recover from warning" $ canRecoverFrom recoverableError
            assertBool "Should not recover from critical error" $ not $ canRecoverFrom nonRecoverableError

        , testCase "错误分类的正确性" $ do
            let syntaxError = errorWithCategory ErrorCategorySyntax "syntax error" startPos
                typeError = errorWithCategory ErrorCategoryTypeCheck "type error" startPos
            assertBool "Syntax error should have correct category" $ 
                case syntaxError of
                    TypeError _ _ cat _ _ -> cat == ErrorCategorySyntax
                    _ -> False
        ]

    , testGroup "QuickCheck属性测试"
        [ fastProperty "错误收集器的单调性" prop_error_collector_monotonic
        , fastProperty "错误格式化的信息保持" prop_error_formatting_preservation
        , fastProperty "错误恢复的一致性" prop_error_recovery_consistency
        , fastProperty "错误严重性的层次性" prop_error_severity_hierarchy
        , fastProperty "错误位置信息的有效性" prop_error_location_validity
        ]
    ]

-- QuickCheck属性测试

-- 错误收集器的单调性：添加错误后，错误数量不会减少
prop_error_collector_monotonic :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_collector_monotonic message severity category =
  not (null message) ==> 
  do
    collector <- newErrorCollector
    let initialErrorCount = L.length $ getErrors collector
        initialWarningCount = L.length $ getWarnings collector
    
    case severity of
        ErrorSeverityError -> do
            addError collector message severity startPos
            finalErrorCount <- return $ L.length $ getErrors collector
            return $ finalErrorCount >= initialErrorCount
        ErrorSeverityWarning -> do
            addWarning collector message startPos
            finalWarningCount <- return $ L.length $ getWarnings collector
            return $ finalWarningCount >= initialWarningCount
        _ -> return $ True

-- 错误格式化的信息保持：格式化后的错误应包含原始信息
prop_error_formatting_preservation :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_formatting_preservation message severity category =
  not (null message) ==>
  let error = TypeError message severity category (ErrorLocation 1 1 Nothing Nothing) emptyContext
      formatted = formatError error
  in property $ message `L.isInfixOf` formatted

-- 错误恢复的一致性：相同严重性和类别的错误应该有一致的恢复行为
prop_error_recovery_consistency :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_recovery_consistency message1 message2 severity category =
  not (null message1) && not (null message2) ==>
  let error1 = TypeError message1 severity category (ErrorLocation 1 1 Nothing Nothing) emptyContext
      error2 = TypeError message2 severity category (ErrorLocation 2 1 Nothing Nothing) emptyContext
      canRecover1 = canRecoverFrom error1
      canRecover2 = canRecoverFrom error2
  in property $ canRecover1 === canRecover2

-- 错误严重性的层次性：Error > Warning > Info
prop_error_severity_hierarchy :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_hierarchy severity1 severity2 =
  let severityOrder severity = case severity of
        ErrorSeverityError -> 3
        ErrorSeverityWarning -> 2
        ErrorSeverityInfo -> 1
  in property $ (severity1 > severity2) === (severityOrder severity1 > severityOrder severity2)

-- 错误位置信息的有效性：错误位置应该有有效的行号和列号
prop_error_location_validity :: Int -> Int -> Property
prop_error_location_validity line column =
  line > 0 && column > 0 ==>
  let location = ErrorLocation line column Nothing Nothing
  in property $ (line location) > 0 .&&. (column location) > 0