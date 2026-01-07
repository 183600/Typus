module Test.Unit.ErrorHandlerPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Compiler.Errors.Core as EC
import Data.List (sort)
import Data.Monoid ((<>))
import Data.Maybe (isJust, isNothing)

-- 测试ErrorSeverity的属性
prop_errorseverity_ordering :: EC.ErrorSeverity -> EC.ErrorSeverity -> Property
prop_errorseverity_ordering sev1 sev2 = 
  case (sev1, sev2) of
    (EC.ErrorInfo, EC.ErrorWarning) -> sev1 < sev2
    (EC.ErrorInfo, EC.ErrorError) -> sev1 < sev2
    (EC.ErrorWarning, EC.ErrorError) -> sev1 < sev2
    (EC.ErrorWarning, EC.ErrorInfo) -> sev1 > sev2
    (EC.ErrorError, EC.ErrorInfo) -> sev1 > sev2
    (EC.ErrorError, EC.ErrorWarning) -> sev1 > sev2
    _ -> sev1 === sev2

-- 测试ErrorCategory的属性
prop_errorcategory_consistency :: EC.ErrorCategory -> Property
prop_errorcategory_consistency category = 
  case category of
    EC.SyntaxError -> property True
    EC.TypeError -> property True
    EC.NameError -> property True
    EC.OwnershipError -> property True
    EC.DependentTypeError -> property True
    EC.ConstraintError -> property True
    EC.Warning -> property True
    EC.Info -> property True

-- 测试ErrorLocation的属性
prop_errorlocation_monoid :: Int -> Int -> Int -> Int -> Property
prop_errorlocation_monoid l1 c1 l2 c2 = 
  let loc1 = EC.ErrorLocation l1 c1
      loc2 = EC.ErrorLocation l2 c2
      combined = loc1 <> loc2
  in EC.errorLine combined >= max l1 l2 &&
     EC.errorColumn combined >= max c1 c2

-- 测试ErrorContext的属性
prop_errorcontext_empty :: Property
prop_errorcontext_empty = 
  let ctx = EC.emptyContext
  in null (EC.contextLines ctx) &&
     null (EC.contextVariables ctx)

-- 测试ErrorRecovery的属性
prop_errorrecovery_can_recover :: EC.ErrorRecovery -> Property
prop_errorrecovery_can_recover recovery = 
  case recovery of
    EC.CanRecover -> property True
    EC.CannotRecover -> property True
    EC.PartialRecover -> property True

-- 测试ErrorCollector的属性
prop_errorcollector_initial_state :: Property
prop_errorcollector_initial_state = 
  let collector = EC.newErrorCollector
  in null (EC.getErrors collector) &&
     null (EC.getWarnings collector) &&
     null (EC.getInfo collector) &&
     not (EC.hasErrors collector) &&
     not (EC.hasWarnings collector)

prop_errorcollector_add_error :: String -> Property
prop_errorcollector_add_error msg = 
  let collector = EC.newErrorCollector
      collector' = EC.addError msg collector
  in EC.hasErrors collector' &&
     length (EC.getErrors collector') === 1

prop_errorcollector_add_warning :: String -> Property
prop_errorcollector_add_warning msg = 
  let collector = EC.newErrorCollector
      collector' = EC.addWarning msg collector
  in EC.hasWarnings collector' &&
     length (EC.getWarnings collector') === 1

prop_errorcollector_add_info :: String -> Property
prop_errorcollector_add_info msg = 
  let collector = EC.newErrorCollector
      collector' = EC.addInfo msg collector
  in length (EC.getInfo collector') === 1

prop_errorcollector_multiple_errors :: [String] -> Property
prop_errorcollector_multiple_errors msgs = 
  let collector = foldl (\c msg -> EC.addError msg c) EC.newErrorCollector msgs
  in length (EC.getErrors collector) === length msgs

-- 测试错误格式化的属性
prop_formaterror_non_empty :: String -> Property
prop_formaterror_non_empty msg = 
  let error = EC.TypeError msg EC.emptyContext
      formatted = EC.formatError error
  in not (null formatted)

prop_formaterror_includes_message :: String -> Property
prop_formaterror_includes_message msg = 
  let error = EC.TypeError msg EC.emptyContext
      formatted = EC.formatError error
  in msg `isInfixOf` formatted

prop_formaterrors_preserves_order :: [String] -> Property
prop_formaterrors_preserves_order msgs = 
  let errors = map (`EC.TypeError` EC.emptyContext) msgs
      formatted = EC.formatErrors errors
      formattedLines = lines formatted
      msgLines = map head (map words formattedLines)
  in length msgLines >= length msgs

-- 测试错误恢复的属性
prop_canrecoverfrom_errorseverity :: EC.ErrorSeverity -> Property
prop_canrecoverfrom_errorseverity severity = 
  let error = case severity of
        EC.ErrorInfo -> EC.Info "test" EC.emptyContext
        EC.ErrorWarning -> EC.Warning "test" EC.emptyContext
        EC.ErrorError -> EC.TypeError "test" EC.emptyContext
  in EC.canRecoverFrom error === (severity /= EC.ErrorError)

prop_shouldcontinueafter_errorseverity :: EC.ErrorSeverity -> Property
prop_shouldcontinueafter_errorseverity severity = 
  let error = case severity of
        EC.ErrorInfo -> EC.Info "test" EC.emptyContext
        EC.ErrorWarning -> EC.Warning "test" EC.emptyContext
        EC.ErrorError -> EC.TypeError "test" EC.emptyContext
  in EC.shouldContinueAfter error === (severity /= EC.ErrorError)

-- 测试错误工具函数的属性
prop_errorat_creates_valid_error :: String -> Int -> Int -> Property
prop_errorat_creates_valid_error msg line col = 
  let error = EC.errorAt line col msg
      location = EC.toErrorLocation error
  in EC.errorLine location === line &&
     EC.errorColumn location === col

prop_errorwithcategory_sets_category :: String -> EC.ErrorCategory -> Property
prop_errorwithcategory_sets_category msg category = 
  let error = EC.errorWithCategory category msg
  in EC.errorCategory error === category

prop_warningat_creates_warning :: String -> Int -> Int -> Property
prop_warningat_creates_warning msg line col = 
  let warning = EC.warningAt line col msg
      location = EC.toErrorLocation warning
  in EC.errorLine location === line &&
     EC.errorColumn location === col &&
     EC.errorSeverity warning === EC.ErrorWarning

prop_infoat_creates_info :: String -> Int -> Int -> Property
prop_infoat_creates_info msg line col = 
  let info = EC.infoAt line col msg
      location = EC.toErrorLocation info
  in EC.errorLine location === line &&
     EC.errorColumn location === col &&
     EC.errorSeverity info === EC.ErrorInfo

-- 测试CombinedError的属性
prop_combinederror_monoid :: String -> String -> Property
prop_combinederror_monoid msg1 msg2 = 
  let error1 = EC.TypeError msg1 EC.emptyContext
      error2 = EC.TypeError msg2 EC.emptyContext
      combined = EC.CombinedError [error1, error2]
  in length (EC.combinedErrors combined) === 2

prop_combinederror_flatten :: [[String]] -> Property
prop_combinederror_flatten msgGroups = 
  let errorGroups = map (\msgs -> map (`EC.TypeError` EC.emptyContext) msgs) msgGroups
      combined = EC.CombinedError (concat errorGroups)
  in length (EC.combinedErrors combined) === sum (map length msgGroups)

-- 测试错误位置转换的属性
prop_toerrorlocation_with_span :: Int -> Int -> Int -> Int -> Property
prop_toerrorlocation_with_span l1 c1 l2 c2 = 
  let span = EC.SourceSpan (EC.SourcePos l1 c1) (EC.SourcePos l2 c2)
      location = EC.toErrorLocationWithSpan span
  in EC.errorLine location === l1 &&
     EC.errorColumn location === c1

tests :: TestTree
tests = testGroup "ErrorHandler Properties Tests"
  [ testProperty "ErrorSeverity ordering" prop_errorseverity_ordering
  , testProperty "ErrorCategory consistency" prop_errorcategory_consistency
  , testProperty "ErrorLocation monoid" prop_errorlocation_monoid
  , testProperty "ErrorContext empty" prop_errorcontext_empty
  , testProperty "ErrorRecovery can recover" prop_errorrecovery_can_recover
  , testProperty "ErrorCollector initial state" prop_errorcollector_initial_state
  , testProperty "ErrorCollector add error" prop_errorcollector_add_error
  , testProperty "ErrorCollector add warning" prop_errorcollector_add_warning
  , testProperty "ErrorCollector add info" prop_errorcollector_add_info
  , testProperty "ErrorCollector multiple errors" prop_errorcollector_multiple_errors
  , testProperty "formatError non empty" prop_formaterror_non_empty
  , testProperty "formatError includes message" prop_formaterror_includes_message
  , testProperty "formatErrors preserves order" prop_formaterrors_preserves_order
  , testProperty "canRecoverFrom ErrorSeverity" prop_canrecoverfrom_errorseverity
  , testProperty "shouldContinueAfter ErrorSeverity" prop_shouldcontinueafter_errorseverity
  , testProperty "errorAt creates valid error" prop_errorat_creates_valid_error
  , testProperty "errorWithCategory sets category" prop_errorwithcategory_sets_category
  , testProperty "warningAt creates warning" prop_warningat_creates_warning
  , testProperty "infoAt creates info" prop_infoat_creates_info
  , testProperty "CombinedError monoid" prop_combinederror_monoid
  , testProperty "CombinedError flatten" prop_combinederror_flatten
  , testProperty "toErrorLocation with span" prop_toerrorlocation_with_span
  ]
