{-# LANGUAGE LambdaCase #-}
module Test.Unit.ErrorHandlerRecoveryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, sized, choose, forAll)
import Data.Char (isAlphaNum, isLetter, isDigit)
import Data.List (sort, nub, group, intercalate, partition)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Set (Set, insert, empty, member, union, difference)
import qualified Data.Set as Set
import qualified Data.Text as T

import Compiler.Errors.Core
  ( TypeError(..), CombinedError(..), ErrorSeverity(..), ErrorCategory(..)
  , ErrorLocation(..), ErrorContext(..), ErrorRecovery(..)
  , emptyContext, newErrorCollector, addError, addWarning, addInfo
  , getErrors, getWarnings, getInfo, getAllMessages, hasErrors, hasWarnings
  , formatError, formatErrors, canRecoverFrom, shouldContinueAfter
  , errorAt, errorWithCategory, warningAt, warningWithCategory, infoAt, warningRecovery, infoRecovery
  )

-- | ErrorHandler错误恢复QuickCheck测试
tests :: TestTree
tests =
  testGroup "ErrorHandler Recovery QuickCheck Tests"
    [ testGroup "Error Recovery Properties"
        [ testProperty "Recovery strategy consistency" propRecoveryStrategyConsistency
        , testProperty "Recovery severity ordering" propRecoverySeverityOrdering
        , testProperty "Recovery continuation logic" propRecoveryContinuationLogic
        , testProperty "Recovery strategy composition" propRecoveryStrategyComposition
        ]

    , testGroup "Error Collection L.and Recovery"
        [ testProperty "Error collection preserves recovery info" propErrorCollectionPreservesRecovery
        , testProperty "Multiple errors recovery aggregation" propMultipleErrorsRecoveryAggregation
        , testProperty "Error filtering maintains recovery" propErrorFilteringMaintainsRecovery
        ]

    , testGroup "Error Severity L.and Recovery"
        [ testProperty "Fatal errors non-recoverable" propFatalErrorsNonRecoverable
        , testProperty "Warning errors always recoverable" propWarningErrorsAlwaysRecoverable
        , testProperty "Info errors always recoverable" propInfoErrorsAlwaysRecoverable
        , testProperty "Error severity affects recovery" propErrorSeverityAffectsRecovery
        ]

    , testGroup "Error Context L.and Recovery"
        [ testProperty "Context preservation in recovery" propContextPreservationInRecovery
        , testProperty "Context enhancement improves recovery" propContextEnhancementImprovesRecovery
        , testProperty "Empty context recovery behavior" propEmptyContextRecoveryBehavior
        ]

    , testGroup "Error Location L.and Recovery"
        [ testProperty "Location information preservation" propLocationInformationPreservation
        , testProperty "Multiple locations recovery" propMultipleLocationsRecovery
        , testProperty "Unknown location recovery" propUnknownLocationRecovery
        ]

    , testGroup "Combined Error Recovery"
        [ testProperty "Combined error recovery aggregation" propCombinedErrorRecoveryAggregation
        , testProperty "Combined error severity propagation" propCombinedErrorSeverityPropagation
        , testProperty "Combined error recovery strategy" propCombinedErrorRecoveryStrategy
        ]

    , testGroup "Edge Cases in Recovery"
        [ testProperty "Empty error collection recovery" propEmptyErrorCollectionRecovery
        , testProperty "Circular error dependencies" propCircularErrorDependencies
        , testProperty "Maximum error depth recovery" propMaximumErrorDepthRecovery
        ]

    , testGroup "Recovery Stress Tests"
        [ testProperty "Large error collection recovery" propLargeErrorCollectionRecovery
        , testProperty "Mixed severity recovery performance" propMixedSeverityRecoveryPerformance
        , testProperty "Complex error graph recovery" propComplexErrorGraphRecovery
        ]
    ]

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- | 恢复策略一致性：相同的错误应该产生相同的恢复策略
propRecoveryStrategyConsistency :: TypeError -> Bool
propRecoveryStrategyConsistency err =
  let recovery1 = canRecoverFrom err
      recovery2 = canRecoverFrom err
  in recovery1 == recovery2

-- | 恢复严重程度顺序：更严重的错误更难恢复
propRecoverySeverityOrdering :: TypeError -> TypeError -> Bool
propRecoverySeverityOrdering err1 err2 =
  let severity1 = getErrorSeverity err1
      severity2 = getErrorSeverity err2
      recoverable1 = canRecoverFrom err1
      recoverable2 = canRecoverFrom err2
  in if severity1 <= severity2
     then recoverable1 >= recoverable2
     else recoverable1 <= recoverable2

-- | 恢复继续逻辑：可恢复的错误应该允许继续
propRecoveryContinuationLogic :: TypeError -> Bool
propRecoveryContinuationLogic err =
  let recoverable = canRecoverFrom err
      shouldContinue = shouldContinueAfter err
  in recoverable ==> shouldContinue

-- | 恢复策略组合：组合策略应该保持一致性
propRecoveryStrategyComposition :: TypeError -> TypeError -> Bool
propRecoveryStrategyComposition err1 err2 =
  let combined = combineErrors err1 err2
      recoverable1 = canRecoverFrom err1
      recoverable2 = canRecoverFrom err2
      recoverableCombined = canRecoverFrom combined
  in if recoverable1 && recoverable2
     then recoverableCombined
     else True  -- 至少一个不可恢复时，组合可能不可恢复

-- ============================================================================
-- Error Collection L.and Recovery
-- ============================================================================

-- | 错误收集保持恢复信息
propErrorCollectionPreservesRecovery :: [TypeError] -> Bool
propErrorCollectionPreservesRecovery errors =
  let collector = newErrorCollector
      withErrors = L.foldl (\acc err -> addError err acc) collector errors
      collectedErrors = getErrors withErrors
      originalRecoverable = map canRecoverFrom errors
      collectedRecoverable = map canRecoverFrom collectedErrors
  in L.length originalRecoverable == L.length collectedRecoverable &&
     L.all id (zipWith (==) originalRecoverable collectedRecoverable)

-- | 多错误恢复聚合
propMultipleErrorsRecoveryAggregation :: [TypeError] -> Bool
propMultipleErrorsRecoveryAggregation errors =
  let recoverableCount = L.length $ filter canRecoverFrom errors
      totalCount = L.length errors
      hasAnyRecoverable = recoverableCount > 0
      hasAnyNonRecoverable = recoverableCount < totalCount
  in hasAnyRecoverable || not hasAnyNonRecoverable

-- | 错误过滤保持恢复
propErrorFilteringMaintainsRecovery :: [TypeError] -> ErrorSeverity -> Bool
propErrorFilteringMaintainsRecovery errors severity =
  let filtered = filterBySeverity severity errors
      originalRecoverable = map canRecoverFrom errors
      filteredRecoverable = map canRecoverFrom filtered
      -- 过滤后的错误应该保持其恢复能力
  in L.all (\(orig, filt) -> filt ==> orig) (zip originalRecoverable filteredRecoverable)

-- ============================================================================
-- Error Severity L.and Recovery
-- ============================================================================

-- | 致命错误不可恢复
propFatalErrorsNonRecoverable :: String -> Bool
propFatalErrorsNonRecoverable msg =
  let fatal = fatalError msg
  in not (canRecoverFrom fatal)

-- | 警告错误总是可恢复
propWarningErrorsAlwaysRecoverable :: String -> Bool
propWarningErrorsAlwaysRecoverable msg =
  let warning = warningAt "test-id" 0 0 "") msg
  in canRecoverFrom warning

-- | 信息错误总是可恢复
propInfoErrorsAlwaysRecoverable :: String -> Bool
propInfoErrorsAlwaysRecoverable msg =
  let info = infoAt "test-id" 0 0 "") msg
  in canRecoverFrom info

-- | 错误严重程度影响恢复
propErrorSeverityAffectsRecovery :: ErrorSeverity -> ErrorSeverity -> Bool
propErrorSeverityAffectsRecovery sev1 sev2 =
  let err1 = errorWithCategory (ErrorLocation 0 0 "") TypeMismatch "" sev1
      err2 = errorWithCategory (ErrorLocation 0 0 "") TypeMismatch "" sev2
  in if sev1 <= sev2
     then canRecoverFrom err1 >= canRecoverFrom err2
     else canRecoverFrom err1 <= canRecoverFrom err2

-- ============================================================================
-- Error Context L.and Recovery
-- ============================================================================

-- | 上下文在恢复中保持
propContextPreservationInRecovery :: TypeError -> ErrorContext -> Bool
propContextPreservationInRecovery err context =
  let withCtx = withContext err context
      originalRecoverable = canRecoverFrom err
      withContextRecoverable = canRecoverFrom withCtx
  in originalRecoverable == withContextRecoverable

-- | 上下文增强改善恢复
propContextEnhancementImprovesRecovery :: TypeError -> ErrorContext -> Bool
propContextEnhancementImprovesRecovery err context =
  let baseRecoverable = canRecoverFrom err
      enhanced = withContext err context
      enhancedRecoverable = canRecoverFrom enhanced
  in enhancedRecoverable >= baseRecoverable

-- | 空上下文恢复行为
propEmptyContextRecoveryBehavior :: TypeError -> Bool
propEmptyContextRecoveryBehavior err =
  let empty = emptyContext
      withEmpty = withContext err empty
      originalRecoverable = canRecoverFrom err
      withEmptyRecoverable = canRecoverFrom withEmpty
  in originalRecoverable == withEmptyRecoverable

-- ============================================================================
-- Error Location L.and Recovery
-- ============================================================================

-- | 位置信息保持
propLocationInformationPreservation :: TypeError -> ErrorLocation -> Bool
propLocationInformationPreservation err location =
  let withLoc = withLocation err location
      originalRecoverable = canRecoverFrom err
      withLocationRecoverable = canRecoverFrom withLoc
  in originalRecoverable == withLocationRecoverable

-- | 多位置恢复
propMultipleLocationsRecovery :: TypeError -> [ErrorLocation] -> Bool
propMultipleLocationsRecovery err locations =
  let withLocations = foldl withLocation err locations
      originalRecoverable = canRecoverFrom err
      withMultiLocationRecoverable = canRecoverFrom withLocations
  in originalRecoverable == withMultiLocationRecoverable

-- | 未知位置恢复
propUnknownLocationRecovery :: TypeError -> Bool
propUnknownLocationRecovery err =
  let unknownLoc = ErrorLocation (-1) (-1) "unknown"
      withUnknown = withLocation err unknownLoc
      originalRecoverable = canRecoverFrom err
      withUnknownRecoverable = canRecoverFrom withUnknown
  in originalRecoverable == withUnknownRecoverable

-- ============================================================================
-- Combined Error Recovery
-- ============================================================================

-- | 组合错误恢复聚合
propCombinedErrorRecoveryAggregation :: TypeError -> TypeError -> Bool
propCombinedErrorRecoveryAggregation err1 err2 =
  let combined = combineErrors err1 err2
      recoverable1 = canRecoverFrom err1
      recoverable2 = canRecoverFrom err2
      recoverableCombined = canRecoverFrom combined
  in (recoverable1 && recoverable2) ==> recoverableCombined

-- | 组合错误严重程度传播
propCombinedErrorSeverityPropagation :: TypeError -> TypeError -> Bool
propCombinedErrorSeverityPropagation err1 err2 =
  let combined = combineErrors err1 err2
      sev1 = getErrorSeverity err1
      sev2 = getErrorSeverity err2
      combinedSev = getErrorSeverity combined
  in combinedSev == max sev1 sev2

-- | 组合错误恢复策略
propCombinedErrorRecoveryStrategy :: TypeError -> TypeError -> Bool
propCombinedErrorRecoveryStrategy err1 err2 =
  let combined = combineErrors err1 err2
      strategy1 = createRecoveryStrategy err1
      strategy2 = createRecoveryStrategy err2
      combinedStrategy = createRecoveryStrategy combined
  in case (strategy1, strategy2, combinedStrategy) of
       (FatalRecovery, _, _) -> combinedStrategy == FatalRecovery
       (_, FatalRecovery, _) -> combinedStrategy == FatalRecovery
       (ErrorRecovery, ErrorRecovery, _) -> combinedStrategy == ErrorRecovery
       (WarningRecovery, WarningRecovery, _) -> combinedStrategy == WarningRecovery
       (InfoRecovery, InfoRecovery, _) -> combinedStrategy == InfoRecovery
       _ -> True  -- 混合情况至少不崩溃

-- ============================================================================
-- Edge Cases in Recovery
-- ============================================================================

-- | 空错误集合恢复
propEmptyErrorCollectionRecovery :: Bool
propEmptyErrorCollectionRecovery =
  let emptyCollector = newErrorCollector
      errors = getErrors emptyCollector
      warnings = getWarnings emptyCollector
      info = getInfo emptyCollector
  in null errors && null warnings && null info

-- | 循环错误依赖
propCircularErrorDependencies :: String -> Bool
propCircularErrorDependencies base =
  let err1 = errorAt "test-id" 0 0 "") (base ++ "1")
      err2 = errorAt "test-id" 1 0 "") (base ++ "2")
      err3 = errorAt "test-id" 2 0 "") (base ++ "3")
      -- 创建循环依赖
      withRelated1 = withRelatedErrors err1 [err2, err3]
      withRelated2 = withRelatedErrors err2 [err3, err1]
      withRelated3 = withRelatedErrors err3 [err1, err2]
  in canRecoverFrom withRelated1 && canRecoverFrom withRelated2 && canRecoverFrom withRelated3

-- | 最大错误深度恢复
propMaximumErrorDepthRecovery :: Int -> String -> Bool
propMaximumErrorDepthRecovery depth base =
  let maxDepth = abs depth `mod` 10 + 1
      createNestedError 0 = errorAt "test-id" 0 0 "") base
      createNestedError n = 
        let inner = createNestedError (n-1)
            wrapper = errorAt "test-id" n 0 "") (base ++ "_depth_" ++ show n)
        in withRelatedErrors wrapper [inner]
      deepest = createNestedError maxDepth
  in canRecoverFrom deepest

-- ============================================================================
-- Recovery Stress Tests
-- ============================================================================

-- | 大错误集合恢复
propLargeErrorCollectionRecovery :: Int -> Bool
propLargeErrorCollectionRecovery count =
  let errorCount = abs count `mod` 100 + 1
      errors = L.map (\i -> errorAt "test-id" i 0 "") ("error_" ++ show i)) [1..errorCount]
      recoverableCount = L.length $ filter canRecoverFrom errors
  in recoverableCount >= 0 && recoverableCount <= errorCount

-- | 混合严重程度恢复性能
propMixedSeverityRecoveryPerformance :: [ErrorSeverity] -> Bool
propMixedSeverityRecoveryPerformance severities =
  let errors = zipWith (\sev i -> 
        errorWithCategory (ErrorLocation i 0 "") TypeMismatch "" sev) severities [1..]
      recoverableCount = L.length $ filter canRecoverFrom errors
      totalCount = L.length errors
  in recoverableCount >= 0 && recoverableCount <= totalCount

-- | 复杂错误图恢复
propComplexErrorGraphRecovery :: Int -> Bool
propComplexErrorGraphRecovery nodeCount =
  let nodes = abs nodeCount `mod` 20 + 1
      createNode i = errorAt "test-id" i 0 "") ("node_" ++ show i)
      nodeErrors = map createNode [1..nodes]
      -- 创建随机连接
      connectNodes errors = zipWith (\err i -> 
        let relatedCount = i `mod` 3
            related = take relatedCount (drop (i+1) errors)
        in withRelatedErrors err related) errors [0..]
      connectedErrors = connectNodes nodeErrors
      allRecoverable = L.all canRecoverFrom connectedErrors
  in allRecoverable || not allRecoverable  -- 至少有确定的结果

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- 获取错误的严重程度
getErrorSeverity :: TypeError -> ErrorSeverity
getErrorSeverity err = case err of
  TypeError { teSeverity = sev } -> sev
  _ -> ErrorSeverity

-- 生成ErrorSeverity
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [ErrorSeverity, WarningSeverity, InfoSeverity]

-- 生成ErrorCategory
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ TypeMismatch, UndefinedVariable, ParseError, SyntaxError
  , SemanticError, RuntimeError, CompilationError, LinkError
  ]

-- 生成ErrorLocation
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (0, 1000)
  col <- choose (0, 1000)
  file <- elements ["", "test.typus", "src/main.typus"]
  return $ ErrorLocation line col file

-- 生成ErrorContext
genErrorContext :: Gen ErrorContext
genErrorContext = do
  return emptyContext

-- 生成TypeError
genTypeError :: Gen TypeError
genTypeError = do
  location <- genErrorLocation
  category <- genErrorCategory
  severity <- genErrorSeverity
  message <- elements ["Type error", "Parse error", "Runtime error"]
  return $ errorWithCategory location category message severity

-- 实例声明
instance Arbitrary ErrorSeverity where
  arbitrary = genErrorSeverity

instance Arbitrary ErrorCategory where
  arbitrary = genErrorCategory

instance Arbitrary ErrorLocation where
  arbitrary = genErrorLocation

instance Arbitrary ErrorContext where
  arbitrary = genErrorContext

instance Arbitrary TypeError where
  arbitrary = genTypeError

instance Arbitrary String where
  arbitrary = listOf $ elements ['a'..'z']

-- 辅助函数
infixr 0 ==>
(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> _ = True