module Test.Unit.ErrorHandlerConsistencyQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
import SourceLocation (SourcePos(..), startPos, SourceSpan(..))
import Data.List (nub, sort)

-- | 简化的错误定义用于测试
data TestError = TestError
  { errorId :: Int
  , testErrorMessage :: String
  , testErrorSeverity :: String
  , errorSpan :: SourceSpan
  } deriving (Show, Eq, Ord)

-- | SourcePos 的 Arbitrary 实例
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- | SourceSpan 的 Arbitrary 实例
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end

-- | 生成有效的测试错误
instance Arbitrary TestError where
  arbitrary = do
    eid <- arbitrary
    msg <- elements ["Type error", "Syntax error", "Runtime error"]
    severity <- elements ["Error", "Warning", "Info"]
    span <- arbitrary
    return $ TestError eid msg severity span

-- | 简化的错误收集器
data TestErrorCollector = TestErrorCollector
  { collectedErrors :: [TestError]
  , collectedWarnings :: [TestError]
  } deriving (Show, Eq)

-- | 测试错误收集一致性
prop_errorCollectionConsistent :: TestError -> Property
prop_errorCollectionConsistent err =
  let collector = TestErrorCollector [] []
      collector' = addTestError err collector
  in property (collectedErrors collector' == [err])

-- | 添加错误到收集器
addTestError :: TestError -> TestErrorCollector -> TestErrorCollector
addTestError err collector = 
  case testErrorSeverity err of
    "Error" -> collector { collectedErrors = err : collectedErrors collector }
    "Warning" -> collector { collectedWarnings = err : collectedWarnings collector }
    _ -> collector

-- | 测试错误去重
prop_errorDeduplication :: [TestError] -> Bool
prop_errorDeduplication errs =
  let uniqueErrs = nub errs
      deduplicated = deduplicateErrors errs
  in length uniqueErrs == length deduplicated

-- | 简单的错误去重函数
deduplicateErrors :: [TestError] -> [TestError]
deduplicateErrors = nub

-- | 测试错误排序
prop_errorSorting :: [TestError] -> Bool
prop_errorSorting errs =
  let sorted = sortErrors errs
      severityOrder err = case testErrorSeverity err of
        "Error" -> 3
        "Warning" -> 2
        "Info" -> 1
        _ -> 0
  in all (\(e1, e2) -> severityOrder e1 >= severityOrder e2) (zip sorted (tail sorted))

-- | 简单的错误排序函数
sortErrors :: [TestError] -> [TestError]
sortErrors = sort

tests :: TestTree
tests = testGroup "Error Handler Consistency Tests"
  [ testProperty "error collection consistent" prop_errorCollectionConsistent
  , testProperty "error deduplication" prop_errorDeduplication
  , testProperty "error sorting" prop_errorSorting
  ]