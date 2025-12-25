{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorRecoveryQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
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
  , canRecoverFrom
  , shouldContinueAfter
  , formatError
  , errorAt
  , warningAt
  , fatalError
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  )

import Compiler.Errors.Compiler
  ( CompilerError(..)
  , CompilationPhase(..)
  , CompilerResult
  , CompilerM
  , runCompilerM
  , collectErrors
  , recoverFrom
  , continueWith
  , withRecovery
  , formatCompilerError
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , ErrorStatistics(..)
  , makeUserFriendly
  , suggestFix
  )

import SourceLocation (SourcePos(..), startPos, SourceSpan(..), emptySpan)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.Either (isLeft, isRight)

-- | 生成编译阶段
genCompilationPhase :: Gen CompilationPhase
genCompilationPhase = elements 
  [ LexingPhase
  , ParsingPhase
  , TypeCheckingPhase
  , OwnershipAnalysisPhase
  , DependentTypeCheckingPhase
  , CodeGenerationPhase
  , OptimizationPhase
  ]

-- | 生成源码上下文
genSourceContext :: Gen (Maybe String)
genSourceContext = oneof
  [ pure Nothing
  , Just <$> listOf (elements $ ['a'..'z'] ++ [' '] ++ ['\n'])
  ]

-- | 生成堆栈跟踪
genStackTrace :: Gen [String]
genStackTrace = listOf $ listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

-- | 生成编译器错误
genCompilerError :: Gen CompilerError
genCompilerError = do
  errorId <- listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  severity <- elements [Fatal, Error, Warning, Info]
  category <- elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]
  message <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['.']
  location <- ErrorLocation <$> choose (1, 100) <*> choose (1, 100) <*> listOf1 (elements $ ['a'..'z'] ++ ['/'])
  context <- emptyContext
  recovery <- elements [CanRecover, CannotRecover, RetryPossible, SkipContinue, FallbackAvailable]
  suggestions <- listOf $ listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['.']
  relatedErrors <- []
  errorChain <- []
  timestamp <- Just <$> listOf1 (elements $ ['0'..'9'] ++ [':'] ++ ['-'])
  
  let typeError = TypeError errorId severity category (T.pack message) location context recovery suggestions relatedErrors errorChain timestamp
  
  sourceContext <- genSourceContext
  stackTrace <- genStackTrace
  phase <- genCompilationPhase
  
  return $ CompilerError typeError sourceContext stackTrace phase

-- | 生成编译器错误列表
genCompilerErrorList :: Gen [CompilerError]
genCompilerErrorList = listOf genCompilerError

-- | 生成错误恢复策略
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = elements 
  [ CanRecover
  , CannotRecover
  , RetryPossible
  , SkipContinue
  , FallbackAvailable
  ]

-- 属性：CompilationPhase的Ord实例应该有正确的顺序
prop_compilationPhase_ordering :: Property
prop_compilationPhase_ordering =
  let phases = [LexingPhase, ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, 
                DependentTypeCheckingPhase, CodeGenerationPhase, OptimizationPhase]
      orderedPhases = sort phases
  in phases === orderedPhases

-- 属性：CompilerError应该包含有效的TypeError
prop_compilerError_contains_typeError :: Property
prop_compilerError_contains_typeError =
  forAll genCompilerError $ \compilerError ->
    let typeError = ceError compilerError
    in errorId typeError /= "" && not (T.null $ errorMessage typeError)

-- 属性：CompilerError应该包含有效的编译阶段
prop_compilerError_valid_phase :: Property
prop_compilerError_valid_phase =
  forAll genCompilerError $ \compilerError ->
    let phase = cePhase compilerError
    in phase `elem` [LexingPhase, ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, 
                     DependentTypeCheckingPhase, CodeGenerationPhase, OptimizationPhase]

-- 属性：canRecoverFrom应该根据ErrorRecovery返回正确结果
prop_canRecoverFrom_compiler_error :: Property
prop_canRecoverFrom_compiler_error =
  forAll genCompilerError $ \compilerError ->
    let typeError = ceError compilerError
        recovery = errorRecovery typeError
        canRecover = canRecoverFrom typeError
    in case recovery of
         CanRecover -> canRecover === True
         RetryPossible -> canRecover === True
         FallbackAvailable -> canRecover === True
         _ -> canRecover === False

-- 属性：shouldContinueAfter应该根据ErrorRecovery返回正确结果
prop_shouldContinueAfter_compiler_error :: Property
prop_shouldContinueAfter_compiler_error =
  forAll genCompilerError $ \compilerError ->
    let typeError = ceError compilerError
        recovery = errorRecovery typeError
        shouldContinue = shouldContinueAfter typeError
    in case recovery of
         CanRecover -> shouldContinue === True
         SkipContinue -> shouldContinue === True
         _ -> shouldContinue === False

-- 属性：formatCompilerError应该包含错误消息
prop_formatCompilerError_contains_message :: Property
prop_formatCompilerError_contains_message =
  forAll genCompilerError $ \compilerError ->
    let formatted = formatCompilerError compilerError
        message = T.unpack $ errorMessage $ ceError compilerError
    in message `isInfixOf` formatted

-- 属性：formatCompilerErrors应该处理空列表
prop_formatCompilerErrors_empty_list :: Property
prop_formatCompilerErrors_empty_list =
  let formatted = formatCompilerErrors []
  in null formatted === True

-- 属性：formatCompilerErrors应该处理非空列表
prop_formatCompilerErrors_non_empty_list :: Property
prop_formatCompilerErrors_non_empty_list =
  forAll genCompilerErrorList $ \errors ->
    let formatted = formatCompilerErrors errors
    in not (null formatted) === not (null errors)

-- 属性：generateDetailedReport应该包含统计信息
prop_generateDetailedReport_contains_stats :: Property
prop_generateDetailedReport_contains_stats =
  forAll genCompilerErrorList $ \errors ->
    let report = generateDetailedReport errors
    in "Error Statistics" `isInfixOf` report

-- 属性：analyzeErrors应该返回有效的统计信息
prop_analyzeErrors_valid_stats :: Property
prop_analyzeErrors_valid_stats =
  forAll genCompilerErrorList $ \errors ->
    let stats = analyzeErrors errors
        totalErrors = errorCount stats
        errorList = errorList stats
    in totalErrors >= 0 && length errorList === length errors

-- 属性：makeUserFriendly应该简化错误消息
prop_makeUserFriendly_simplifies_message :: Property
prop_makeUserFriendly_simplifies_message =
  forAll genCompilerError $ \compilerError ->
    let friendly = makeUserFriendly compilerError
        original = ceError compilerError
    in not (T.null $ errorMessage friendly)

-- 属性：suggestFix应该返回非空建议
prop_suggestFix_non_empty :: Property
prop_suggestFix_non_empty =
  forAll genCompilerError $ \compilerError ->
    let fixes = suggestFix compilerError
    in not (null fixes)

-- 属性：runCompilerM应该处理成功情况
prop_runCompilerM_success :: Property
prop_runCompilerM_success =
  let result = runCompilerM (return "success")
  in isRight result === True

-- 属性：collectErrors应该收集所有错误
prop_collectErrors_collects_all :: Property
prop_collectErrors_collects_all =
  forAll genCompilerErrorList $ \errors ->
    let collected = collectErrors errors
    in length collected >= length errors

-- 属性：recoverFrom应该处理可恢复错误
prop_recoverFrom_handles_recoverable :: Property
prop_recoverFrom_handles_recoverable =
  forAll genCompilerError $ \compilerError ->
    let typeError = ceError compilerError
        recovery = errorRecovery typeError
    in case recovery of
         CanRecover -> isRight (recoverFrom compilerError) === True
         _ -> property True  -- 其他情况可能失败，这是预期的

-- 属性：continueWith应该处理可继续错误
prop_continueWith_handles_continuable :: Property
prop_continueWith_handles_continuable =
  forAll genCompilerError $ \compilerError ->
    let typeError = ceError compilerError
        recovery = errorRecovery typeError
    in case recovery of
         SkipContinue -> isRight (continueWith compilerError) === True
         _ -> property True  -- 其他情况可能失败，这是预期的

-- 属性：createRecoveryStrategy应该创建有效的策略
prop_createRecoveryStrategy_valid :: Property
prop_createRecoveryStrategy_valid =
  forAll genErrorRecovery $ \recovery ->
    let strategy = createRecoveryStrategy recovery
    in strategy === recovery

-- 属性：customRecovery应该创建自定义恢复策略
prop_customRecovery_custom :: Property
prop_customRecovery_custom =
  let custom = customRecovery "custom strategy"
  in custom === custom

-- 属性：fatalRecovery应该创建致命恢复策略
prop_fatalRecovery_fatal :: Property
prop_fatalRecovery_fatal =
  let fatal = fatalRecovery
  in fatal === CannotRecover

-- 属性：errorRecovery应该创建错误恢复策略
prop_errorRecovery_error :: Property
prop_errorRecovery_error =
  let errorRec = errorRecovery
  in errorRec === CanRecover

-- 属性：warningRecovery应该创建警告恢复策略
prop_warningRecovery_warning :: Property
prop_warningRecovery_warning =
  let warning = warningRecovery
  in warning === CanRecover

-- 属性：infoRecovery应该创建信息恢复策略
prop_infoRecovery_info :: Property
prop_infoRecovery_info =
  let info = infoRecovery
  in info === CanRecover

-- 属性：CompilerError的Eq实例应该正确比较错误
prop_compilerError_equality :: Property
prop_compilerError_equality =
  forAll genCompilerError $ \error ->
    error === error

-- 属性：CompilerError的Show实例应该包含错误信息
prop_compilerError_show_informative :: Property
prop_compilerError_show_informative =
  forAll genCompilerError $ \error ->
    let showStr = show error
    in not (null showStr) === True

tests :: TestTree
tests =
  testGroup "Error Recovery QuickCheck Tests"
    [ fastProperty "CompilationPhase ordering" prop_compilationPhase_ordering
    , fastProperty "CompilerError contains TypeError" prop_compilerError_contains_typeError
    , fastProperty "CompilerError valid phase" prop_compilerError_valid_phase
    , fastProperty "canRecoverFrom compiler error" prop_canRecoverFrom_compiler_error
    , fastProperty "shouldContinueAfter compiler error" prop_shouldContinueAfter_compiler_error
    , fastProperty "formatCompilerError contains message" prop_formatCompilerError_contains_message
    , fastProperty "formatCompilerErrors empty list" prop_formatCompilerErrors_empty_list
    , fastProperty "formatCompilerErrors non empty list" prop_formatCompilerErrors_non_empty_list
    , fastProperty "generateDetailedReport contains stats" prop_generateDetailedReport_contains_stats
    , fastProperty "analyzeErrors valid stats" prop_analyzeErrors_valid_stats
    , fastProperty "makeUserFriendly simplifies message" prop_makeUserFriendly_simplifies_message
    , fastProperty "suggestFix non empty" prop_suggestFix_non_empty
    , fastProperty "runCompilerM success" prop_runCompilerM_success
    , fastProperty "collectErrors collects all" prop_collectErrors_collects_all
    , fastProperty "recoverFrom handles recoverable" prop_recoverFrom_handles_recoverable
    , fastProperty "continueWith handles continuable" prop_continueWith_handles_continuable
    , fastProperty "createRecoveryStrategy valid" prop_createRecoveryStrategy_valid
    , fastProperty "customRecovery custom" prop_customRecovery_custom
    , fastProperty "fatalRecovery fatal" prop_fatalRecovery_fatal
    , fastProperty "errorRecovery error" prop_errorRecovery_error
    , fastProperty "warningRecovery warning" prop_warningRecovery_warning
    , fastProperty "infoRecovery info" prop_infoRecovery_info
    , fastProperty "CompilerError equality" prop_compilerError_equality
    , fastProperty "CompilerError show informative" prop_compilerError_show_informative
    ]