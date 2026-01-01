{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerErrorHandlingQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose
  , sized, resize, suchThat, vectorOf, arbitrary
  )

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , severityPriority
  , compareSeverity
  , isAtLeast
  , shouldContinueAfter
  )

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)

-- | 生成错误严重性级别
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- | 生成错误类别
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ TypeChecking, Ownership, Parsing, Semantic
  , Runtime, Constraint, Inference, Integration, Unknown
  ]

-- | 生成错误位置
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 1000)
  column <- choose (1, 100)
  filePath <- listOf1 $ elements $ ['a'..'z'] ++ ['_'] ++ ['0'..'9'] ++ ['/']
  return $ ErrorLocation line column filePath

-- | 生成错误上下文
genErrorContext :: Gen ErrorContext
genErrorContext = do
  functionName <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  variableName <- listOf1 $ elements $ ['a'..'z'] ++ ['_']
  expectedType <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z']
  actualType <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z']
  return $ ErrorContext functionName variableName expectedType actualType

-- | 生成错误恢复策略
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = elements 
  [ CanRecover, CannotRecover, RetryPossible, SkipContinue, FallbackAvailable
  ]

-- | 生成错误消息
genErrorMessage :: Gen Text
genErrorMessage = do
  words <- listOf1 $ listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z']
  return $ T.pack $ unwords words

-- | 生成建议列表
genSuggestions :: Gen [Text]
genSuggestions = listOf $ do
  suggestion <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['.']
  return $ T.pack suggestion

-- | 生成TypeError
genTypeError :: Gen TypeError
genTypeError = do
  errorId <- listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  severity <- genErrorSeverity
  category <- genErrorCategory
  message <- genErrorMessage
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  suggestions <- genSuggestions
  relatedErrors <- listOf genTypeError
  errorChain <- listOf genTypeError
  timestamp <- listOf1 $ elements $ ['0'..'9'] ++ [':'] ++ ['-'] ++ ['T'] ++ ['Z']
  return $ TypeError errorId severity category message location context recovery 
                     suggestions relatedErrors errorChain (Just timestamp)

-- | 生成TypeError列表
genTypeErrorList :: Gen [TypeError]
genTypeErrorList = listOf genTypeError

-- 属性：严重性优先级应该是正确的顺序
prop_severity_priority_order :: Property
prop_severity_priority_order =
  severityPriority Fatal > severityPriority Error .&&.
  severityPriority Error > severityPriority Warning .&&.
  severityPriority Warning > severityPriority Info

-- 属性：严重性比较应该与优先级一致
prop_severity_comparison_consistency :: Property
prop_severity_comparison_consistency =
  forAll genErrorSeverity $ \s1 ->
  forAll genErrorSeverity $ \s2 ->
    compareSeverity s1 s2 === compare (severityPriority s1) (severityPriority s2)

-- 属性：isAtLeast应该正确检查严重性级别
prop_isAtLeast_correctness :: Property
prop_isAtLeast_correctness =
  forAll genErrorSeverity $ \minSeverity ->
  forAll genErrorSeverity $ \severity ->
    isAtLeast minSeverity severity === (severityPriority severity >= severityPriority minSeverity)

-- 属性：errorAt应该创建具有指定位置的错误
prop_errorAt_location :: Property
prop_errorAt_location =
  forAll genErrorLocation $ \location ->
  forAll genErrorMessage $ \message ->
    let error = errorAt "test-id" (errorSeverity err)) filtered

-- 属性：hasCategory应该正确检查错误类别
prop_hasCategory_correctness :: Property
prop_hasCategory_correctness =
  forAll genTypeError $ \error ->
  forAll genErrorCategory $ \category ->
    let hasCat = hasCategory category error
        matches = errorCategory error == category
    in hasCat === matches

-- 属性：filterByCategory应该只保留指定类别的错误
prop_filterByCategory_correctness :: Property
prop_filterByCategory_correctness =
  forAll genTypeErrorList $ \errors ->
  forAll genErrorCategory $ \category ->
    let filtered = filterByCategory category errors
    in L.all (\err -> errorCategory err == category) filtered

-- 属性：formatError应该包含错误消息
prop_formatError_contains_message :: Property
prop_formatError_contains_message =
  forAll genTypeError $ \error ->
    let formatted = formatError error
        messageText = T.unpack $ errorMessage error
    in messageText `L.isInfixOf` formatted

-- 属性：canRecoverFrom应该根据ErrorRecovery返回正确结果
prop_canRecoverFrom_correctness :: Property
prop_canRecoverFrom_correctness =
  forAll genTypeError $ \error ->
    let recovery = errorRecovery error
        canRecover = canRecoverFrom error
    in case recovery of
         CanRecover -> canRecover === True
         RetryPossible -> canRecover === True
         FallbackAvailable -> canRecover === True
         _ -> canRecover === False

-- 属性：shouldContinueAfter应该根据ErrorRecovery返回正确结果
prop_shouldContinueAfter_correctness :: Property
prop_shouldContinueAfter_correctness =
  forAll genTypeError $ \error ->
    let recovery = errorRecovery error
        shouldContinue = shouldContinueAfter error
    in case recovery of
         CanRecover -> shouldContinue === True
         SkipContinue -> shouldContinue === True
         _ -> shouldContinue === False

-- 属性：空上下文应该有默认值
prop_emptyContext_values :: Property
prop_emptyContext_values =
  let ctx = emptyContext
  in contextFunction ctx === "" && 
     contextVariable ctx === "" && 
     contextExpectedType ctx === "" && 
     contextActualType ctx === ""

-- 属性：严重性排序应该与优先级一致
prop_severity_sorting :: Property
prop_severity_sorting =
  forAll genTypeErrorList $ \errors ->
    let sortedBySeverity = sort $ map errorSeverity errors
        sortedByPriority = sort $ map severityPriority $ map errorSeverity errors
        prioritiesFromSeverities = map severityPriority sortedBySeverity
    in prioritiesFromSeverities === sortedByPriority

-- 属性：相关错误应该保持关系
prop_related_errors_preserved :: Property
prop_related_errors_preserved =
  forAll genTypeError $ \error ->
  forAll genTypeErrorList $ \relatedErrors ->
    let updatedError = error { relatedErrors = relatedErrors }
    in L.length (relatedErrors updatedError) === L.length relatedErrors

tests :: TestTree
tests =
  testGroup "Compiler Error Handling QuickCheck Tests"
    [ fastProperty "Severity priority order" prop_severity_priority_order
    , fastProperty "Severity comparison consistency" prop_severity_comparison_consistency
    , fastProperty "isAtLeast correctness" prop_isAtLeast_correctness
    , fastProperty "errorAt "test-id" preserved" prop_related_errors_preserved
    ]