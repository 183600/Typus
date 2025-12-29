{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompactErrorHandlerSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import ErrorHandler
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt, spanFrom)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)

-- | 生成任意的错误消息
instance Arbitrary String where
  arbitrary = do
    len <- choose (1, 50)
    elements $ map (:[]) ['a'..'z'] >>= \c -> 
      return $ concat (replicate len c)

-- | 生成任意的源位置用于错误测试
genErrorPos :: Gen SourcePos
genErrorPos = do
  line <- choose (1, 100)
  col <- choose (1, 100)
  return $ SourcePos line col

-- | 测试错误创建和格式化
testErrorCreation :: TestTree
testErrorCreation = testGroup "错误创建和格式化测试"
  [ testCase "创建基本错误" $
      let msg = "Test error message"
          pos = SourcePos 1 1
          error = createBasicError msg pos
      in assertBool "错误消息包含原始消息" (msg `isInfixOf` formatError error)
    
  , testCase "创建带位置的错误" $
      let msg = "Position error"
          pos = SourcePos 5 10
          error = createErrorWithLocation msg pos
          formatted = formatError error
      in assertBool "错误格式包含位置信息" ("5:10" `isInfixOf` formatted)
    
  , testCase "创建带跨度的错误" $
      let msg = "Span error"
          span = SourceSpan (SourcePos 3 1) (SourcePos 3 10)
          error = createErrorWithSpan msg span
          formatted = formatError error
      in assertBool "错误格式包含跨度信息" ("3:1-3:10" `isInfixOf` formatted)
  ]

-- | 测试错误分类
testErrorClassification :: TestTree
testErrorClassification = testGroup "错误分类测试"
  [ testCase "语法错误分类" $
      let error = createSyntaxError "Missing semicolon" (SourcePos 2 5)
      in assertBool "正确分类为语法错误" (isSyntaxError error)
    
  , testCase "语义错误分类" $
      let error = createSemanticError "Type mismatch" (SourcePos 1 10)
      in assertBool "正确分类为语义错误" (isSemanticError error)
    
  , testCase "警告分类" $
      let warning = createWarning "Unused variable" (SourcePos 3 8)
      in assertBool "正确分类为警告" (isWarning warning)
    
  , testCase "致命错误分类" $
      let fatal = createFatalError "Stack overflow" (SourcePos 10 1)
      in assertBool "正确分类为致命错误" (isFatalError fatal)
  ]

-- | 测试错误聚合
testErrorAggregation :: TestTree
testErrorAggregation = testGroup "错误聚合测试"
  [ testCase "聚合多个错误" $
      let errors = [ createBasicError "Error 1" (SourcePos 1 1)
                   , createBasicError "Error 2" (SourcePos 2 2)
                   , createBasicError "Error 3" (SourcePos 3 3)
                   ]
          aggregated = aggregateErrors errors
      in assertBool "聚合包含所有错误" (length aggregated == length errors)
    
  , testCase "按位置排序错误" $
      let errors = [ createBasicError "Error 1" (SourcePos 3 3)
                   , createBasicError "Error 2" (SourcePos 1 1)
                   , createBasicError "Error 3" (SourcePos 2 2)
                   ]
          sorted = sortErrorsByPosition errors
          positions = map getErrorPosition sorted
      in positions @?= [SourcePos 1 1, SourcePos 2 2, SourcePos 3 3]
    
  , testCase "过滤错误类型" $
      let errors = [ createSyntaxError "Syntax" (SourcePos 1 1)
                   , createSemanticError "Semantic" (SourcePos 2 2)
                   , createWarning "Warning" (SourcePos 3 3)
                   ]
          syntaxErrors = filterByErrorType isSyntaxError errors
      in length syntaxErrors @?= 1
  ]

-- | 测试错误恢复
testErrorRecovery :: TestTree
testErrorRecovery = testGroup "错误恢复测试"
  [ testCase "错误后的恢复点" $
      let error = createBasicError "Recoverable error" (SourcePos 1 1)
          recoveryPoint = createRecoveryPoint (SourcePos 1 5)
          canRecover = canRecoverFromError error recoveryPoint
      in assertBool "应该能够恢复" canRecover
    
  , testCase "致命错误不可恢复" $
      let fatal = createFatalError "Fatal error" (SourcePos 1 1)
          recoveryPoint = createRecoveryPoint (SourcePos 1 5)
          canRecover = canRecoverFromError fatal recoveryPoint
      in assertBool "致命错误不可恢复" (not canRecover)
    
  , testCase "错误恢复策略" $
      let errors = [ createBasicError "Error 1" (SourcePos 1 1)
                   , createFatalError "Fatal" (SourcePos 2 2)
                   , createBasicError "Error 2" (SourcePos 3 3)
                   ]
          strategy = determineRecoveryStrategy errors
      in assertBool "遇到致命错误应该停止" (strategy == StopCompilation)
  ]

-- | QuickCheck属性测试
testErrorHandlerProperties :: TestTree
testErrorHandlerProperties = testGroup "错误处理属性测试"
  [ testProperty "错误格式化包含位置信息" $
      forAll genErrorPos $ \pos ->
        let msg = "Test message"
            error = createBasicError msg pos
            formatted = formatError error
            posStr = show (spLine pos) ++ ":" ++ show (spColumn pos)
        in posStr `isInfixOf` formatted
  
  , testProperty "错误聚合保持数量" $
      \errors -> 
        let aggregated = aggregateErrors errors
        in length aggregated === length errors
  
  , testProperty "排序后位置单调递增" $
      \errors ->
        let sorted = sortErrorsByPosition errors
            positions = map getErrorPosition sorted
            isMonotonic [] = True
            isMonotonic [_] = True
            isMonotonic (p1:p2:ps) = p1 <= p2 && isMonotonic (p2:ps)
        in isMonotonic positions
  ]

-- | 测试错误上下文
testErrorContext :: TestTree
testErrorContext = testGroup "错误上下文测试"
  [ testCase "错误上下文包含源码行" $
      let error = createBasicError "Error" (SourcePos 2 5)
          context = getErrorContext ["line 1", "line 2 with error", "line 3"] error
      in assertBool "上下文包含错误行" ("line 2 with error" `isInfixOf` context)
    
  , testCase "错误上下文显示位置标记" $
      let error = createBasicError "Error" (SourcePos 2 10)
          context = getErrorContext ["line 2 with error here"] error
      in assertBool "上下文包含位置标记" ("^" `isInfixOf` context)
    
  , testCase "多行错误上下文" $
      let error = createErrorWithSpan "Multi-line error" 
                   (SourceSpan (SourcePos 2 1) (SourcePos 4 10))
          context = getErrorContext ["line 1", "line 2", "line 3", "line 4", "line 5"] error
      in assertBool "上下文包含多行" ("line 2" `isInfixOf` context && "line 4" `isInfixOf` context)
  ]

-- | 边界条件测试
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup "边界条件测试"
  [ testCase "空错误列表" $
      let aggregated = aggregateErrors []
      in length aggregated @?= 0
    
  , testCase "空错误消息" $
      let error = createBasicError "" (SourcePos 1 1)
          formatted = formatError error
      in assertBool "格式化仍然包含位置" ("1:1" `isInfixOf` formatted)
    
  , testCase "极大位置值" $
      let pos = SourcePos 999999 999999
          error = createBasicError "Large position" pos
          formatted = formatError error
      in assertBool "处理大位置值" ("999999:999999" `isInfixOf` formatted)
  ]

-- | 性能测试
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup "性能属性测试"
  [ testProperty "大量错误处理性能" $
      \n ->
        let numErrors = min 1000 (max 1 n)
            errors = replicate numErrors (createBasicError "Test" (SourcePos 1 1))
            aggregated = aggregateErrors errors
        in length aggregated === numErrors
  ]

-- | 组合所有测试
tests :: TestTree
tests = testGroup "ErrorHandler模块核心功能测试"
  [ testErrorCreation
  , testErrorClassification
  , testErrorAggregation
  , testErrorRecovery
  , testErrorHandlerProperties
  , testErrorContext
  , testBoundaryConditions
  , testPerformanceProperties
  ]

-- 辅助函数（假设这些函数在ErrorHandler模块中存在，如果不存在需要实现）
createBasicError :: String -> SourcePos -> ErrorHandler
createBasicError msg pos = undefined  -- 实际实现

createErrorWithLocation :: String -> SourcePos -> ErrorHandler
createErrorWithLocation msg pos = undefined  -- 实际实现

createErrorWithSpan :: String -> SourceSpan -> ErrorHandler
createErrorWithSpan msg span = undefined  -- 实际实现

createSyntaxError :: String -> SourcePos -> ErrorHandler
createSyntaxError msg pos = undefined  -- 实际实现

createSemanticError :: String -> SourcePos -> ErrorHandler
createSemanticError msg pos = undefined  -- 实际实现

createWarning :: String -> SourcePos -> ErrorHandler
createWarning msg pos = undefined  -- 实际实现

createFatalError :: String -> SourcePos -> ErrorHandler
createFatalError msg pos = undefined  -- 实际实现

formatError :: ErrorHandler -> String
formatError error = undefined  -- 实际实现

isSyntaxError :: ErrorHandler -> Bool
isSyntaxError error = undefined  -- 实际实现

isSemanticError :: ErrorHandler -> Bool
isSemanticError error = undefined  -- 实际实现

isWarning :: ErrorHandler -> Bool
isWarning error = undefined  -- 实际实现

isFatalError :: ErrorHandler -> Bool
isFatalError error = undefined  -- 实际实现

aggregateErrors :: [ErrorHandler] -> [ErrorHandler]
aggregateErrors errors = undefined  -- 实际实现

sortErrorsByPosition :: [ErrorHandler] -> [ErrorHandler]
sortErrorsByPosition errors = undefined  -- 实际实现

filterByErrorType :: (ErrorHandler -> Bool) -> [ErrorHandler] -> [ErrorHandler]
filterByErrorType predicate errors = undefined  -- 实际实现

getErrorPosition :: ErrorHandler -> SourcePos
getErrorPosition error = undefined  -- 实际实现

createRecoveryPoint :: SourcePos -> RecoveryPoint
createRecoveryPoint pos = undefined  -- 实际实现

canRecoverFromError :: ErrorHandler -> RecoveryPoint -> Bool
canRecoverFromError error recoveryPoint = undefined  -- 实际实现

determineRecoveryStrategy :: [ErrorHandler] -> RecoveryStrategy
determineRecoveryStrategy errors = undefined  -- 实际实现

getErrorContext :: [String] -> ErrorHandler -> String
getErrorContext lines error = undefined  -- 实际实现

-- 占位类型
type ErrorHandler = String
type RecoveryPoint = SourcePos
data RecoveryStrategy = StopCompilation | ContinueCompilation deriving (Eq, Show)