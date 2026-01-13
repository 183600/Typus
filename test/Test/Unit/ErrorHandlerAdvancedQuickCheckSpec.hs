module Test.Unit.ErrorHandlerAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , CombinedError(..)
  , emptyContext
  , errorAt
  , errorAtWithTimestamp
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , withTimestamp
  , wrapError
  , combineErrors
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , formatError
  , formatErrorWithLocation
  , formatErrors
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , severityPriority
  , isAtLeast
  , compareSeverity
  , getErrorLine
  , getErrorColumn
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  )
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sortBy)
import Data.Ord (comparing)

-- | 生成错误ID
newtype ErrorId = ErrorId { getErrorId :: String }
  deriving Show

instance Arbitrary ErrorId where
  arbitrary = do
    num <- choose (1000, 9999)
    return $ ErrorId $ "ERR" ++ show num

-- | 生成错误消息
newtype ErrorMessage = ErrorMessage { getErrorMessage :: String }
  deriving Show

instance Arbitrary ErrorMessage where
  arbitrary = do
    words <- choose (1, 10)
    wordList <- vectorOf words $ elements ["error", "occurred", "in", "type", "checking", "parsing", "semantic", "analysis"]
    return $ ErrorMessage $ unwords wordList

-- | 生成错误位置
instance Arbitrary ErrorLocation where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    endLine <- choose (Just line, Just (line + 10))
    endColumn <- choose (Just column, Just (column + 20))
    filePath <- elements [Nothing, Just "test.typus", Just "module.typus"]
    return $ ErrorLocation filePath line column endLine endColumn

-- | 生成错误严重性
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

-- | 生成错误类别
instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

-- | 生成错误上下文
instance Arbitrary ErrorContext where
  arbitrary = do
    hasCode <- arbitrary
    hasFunction <- arbitrary
    hasVariable <- arbitrary
    hasType <- arbitrary
    numAdditional <- choose (0, 3)
    additional <- vectorOf numAdditional $ do
      key <- elements ["scope", "module", "phase"]
      value <- elements ["global", "main", "analysis"]
      return (key, value)
    
    code <- if hasCode then Just "let x = 42" else Nothing
    function <- if hasFunction then Just "testFunction" else Nothing
    variable <- if hasVariable then Just "x" else Nothing
    typ <- if hasType then Just "Int" else Nothing
    
    return $ ErrorContext code function variable typ additional

-- | 生成错误恢复策略
instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    hasAction <- arbitrary
    hasHint <- arbitrary
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    
    action <- if hasAction then Just "retry operation" else Nothing
    hint <- if hasHint then Just "check input" else Nothing
    
    return $ RecoveryStrategy canRec shouldCont action hint cost confidence

-- | 生成TypeError
instance Arbitrary TypeError where
  arbitrary = do
    ErrorId errId <- arbitrary
    ErrorMessage msg <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    numSuggestions <- choose (0, 3)
    suggestions <- vectorOf numSuggestions $ elements ["check syntax", "verify types", "review imports"]
    numRelated <- choose (0, 2)
    related <- vectorOf numRelated arbitrary
    hasTimestamp <- arbitrary
    timestamp <- if hasTimestamp then Just "2023-01-01 12:00:00" else Nothing
    
    return $ TypeError
      { errorId = errId
      , severity = severity
      , category = category
      , message = T.pack msg
      , location = location
      , context = context
      , recovery = recovery
      , suggestions = map T.pack suggestions
      , relatedErrors = related
      , errorChain = []
      , timestamp = timestamp
      }

-- | 生成CombinedError
instance Arbitrary CombinedError where
  arbitrary = do
    severity <- arbitrary
    errorType <- elements ["Ownership", "DependentType", "Integration", "CrossAnalyzer"]
    case errorType of
      "Ownership" -> do
        errMsg <- elements ["Ownership violation", "Borrow checker error"]
        return $ OwnershipErrorCombined severity errMsg
      "DependentType" -> do
        errMsg <- elements ["Type mismatch", "Constraint violation"]
        return $ DependentTypeErrorCombined severity errMsg
      "Integration" -> do
        errMsg <- elements ["Module integration error", "Cross-module type error"]
        return $ IntegrationError errMsg severity
      "CrossAnalyzer" -> do
        numErrors <- choose (1, 3)
        subErrors <- vectorOf numErrors arbitrary
        errMsg <- elements ["Cross-analyzer conflict", "Inconsistent analysis"]
        return $ CrossAnalyzerError errMsg severity subErrors
      _ -> return $ IntegrationError "Unknown error" severity

-- | 测试ErrorSeverity的比较属性
prop_severity_priority_order :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_order sev1 sev2 =
  let p1 = severityPriority sev1
      p2 = severityPriority sev2
      ordering = compareSeverity sev1 sev2
  in if p1 > p2
     then ordering === GT
     else if p1 < p2
          then ordering === LT
          else ordering === EQ

prop_severity_priority_range :: ErrorSeverity -> Property
prop_severity_priority_range sev =
  let priority = severityPriority sev
  in priority >= 0 .&&. priority <= 100

prop_is_at_least_reflexive :: ErrorSeverity -> Property
prop_is_at_least_reflexive sev =
  isAtLeast sev sev

prop_is_at_least_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least_transitive sev1 sev2 sev3 =
  (isAtLeast sev1 sev2 && isAtLeast sev2 sev3) ==> isAtLeast sev1 sev3

prop_severity_total_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_total_ordering sev1 sev2 =
  let ordering = compareSeverity sev1 sev2
  in ordering === EQ || ordering === LT || ordering === GT

-- | 测试ErrorLocation的属性
prop_error_location_accessors :: ErrorLocation -> Property
prop_error_location_accessors loc =
  getErrorLine loc === line loc .&&.
  getErrorColumn loc === column loc

prop_error_location_with_file :: Property
prop_error_location_with_file =
  let loc = errorAtLocation "test.typus" 10 20
  in filePath loc === Just "test.typus" .&&.
     line loc === 10 .&&.
     column loc === 20

prop_error_location_with_range :: Property
prop_error_location_with_range =
  let loc = errorAtRange 5 10 5 20
  in line loc === 5 .&&.
     column loc === 10 .&&.
     endLine loc === Just 5 .&&.
     endColumn loc === Just 20

-- | 测试ErrorRecovery的属性
prop_recovery_can_recover :: ErrorRecovery -> Property
prop_recovery_can_recover recovery =
  let canRec = canRecover recovery
      shouldCont = shouldContinue recovery
  in if not canRec
     then not shouldCont
     else property True

prop_recovery_cost_range :: ErrorRecovery -> Property
prop_recovery_cost_range recovery =
  let cost = recoveryCost recovery
  in cost >= 0 .&&. cost <= 100

prop_recovery_confidence_range :: ErrorRecovery -> Property
prop_recovery_confidence_range recovery =
  let confidence = recoveryConfidence recovery
  in confidence >= 0.0 .&&. confidence <= 1.0

prop_predefined_recovery_strategies :: Property
prop_predefined_recovery_strategies =
  let fatal = fatalRecovery
      error = errorRecovery
      warning = warningRecovery
      info = infoRecovery
  in not (canRecover fatal) .&&.
     not (shouldContinue fatal) .&&.
     canRecover error .&&.
     shouldContinue error .&&.
     canRecover warning .&&.
     shouldContinue warning .&&.
     canRecover info .&&.
     shouldContinue info

prop_custom_recovery :: Property
prop_custom_recovery =
  let recovery = customRecovery True True (Just "retry") (Just "check") 25 0.8
  in canRecover recovery .&&.
     shouldContinue recovery .&&.
     recoveryAction recovery === Just "retry" .&&.
     recoveryHint recovery === Just "check" .&&.
     recoveryCost recovery === 25 .&&.
     recoveryConfidence recovery === 0.8

-- | 测试TypeError的属性
prop_error_creation :: ErrorId -> ErrorMessage -> ErrorLocation -> Property
prop_error_creation (ErrorId errId) (ErrorMessage msg) loc =
  let err = errorAt errId (T.pack msg) loc
  in errorId err === errId .&&.
     message err === T.pack msg .&&.
     location err === loc .&&.
     severity err === Error .&&.
     category err === Unknown

prop_error_with_category :: ErrorId -> ErrorMessage -> ErrorCategory -> ErrorLocation -> Property
prop_error_with_category (ErrorId errId) (ErrorMessage msg) cat loc =
  let err = errorWithCategory errId cat (T.pack msg) loc
  in errorId err === errId .&&.
     message err === T.pack msg .&&.
     location err === loc .&&.
     severity err === Error .&&.
     category err === cat

prop_warning_creation :: ErrorId -> ErrorMessage -> ErrorLocation -> Property
prop_warning_creation (ErrorId errId) (ErrorMessage msg) loc =
  let warn = warningAt errId (T.pack msg) loc
  in errorId warn === errId .&&.
     message warn === T.pack msg .&&.
     location warn === loc .&&.
     severity warn === Warning .&&.
     category warn === Unknown

prop_info_creation :: ErrorId -> ErrorMessage -> ErrorLocation -> Property
prop_info_creation (ErrorId errId) (ErrorMessage msg) loc =
  let info = infoAt errId (T.pack msg) loc
  in errorId info === errId .&&.
     message info === T.pack msg .&&.
     location info === loc .&&.
     severity info === Info .&&.
     category info === Unknown

prop_error_with_suggestions :: ErrorId -> ErrorMessage -> ErrorLocation -> Property
prop_error_with_suggestions (ErrorId errId) (ErrorMessage msg) loc =
  let suggestions = ["check syntax", "verify types"]
      err = errorWithSuggestions errId (T.pack msg) (map T.pack suggestions) loc
  in suggestions err === map T.pack suggestions

prop_error_with_location :: TypeError -> ErrorLocation -> Property
prop_error_with_location err loc =
  let newErr = withLocation err loc
  in location newErr === loc .&&.
     errorId newErr === errorId err .&&.
     message newErr === message err

prop_error_with_context :: TypeError -> ErrorContext -> Property
prop_error_with_context err ctx =
  let newErr = withContext err ctx
  in context newErr === ctx .&&.
     errorId newErr === errorId err .&&.
     message newErr === message err

prop_error_with_timestamp :: TypeError -> Property
prop_error_with_timestamp err =
  let ts = "2023-01-01 12:00:00"
      newErr = withTimestamp ts err
  in timestamp newErr === Just ts .&&.
     errorId newErr === errorId err .&&.
     message newErr === message err

prop_wrap_error :: TypeError -> ErrorMessage -> Property
prop_wrap_error err (ErrorMessage wrapperMsg) =
  let wrapped = wrapError (T.pack wrapperMsg) err
  in message wrapped === T.pack wrapperMsg <> ": " <> message err .&&.
     errorChain wrapped === [err]

prop_error_with_related_errors :: TypeError -> [TypeError] -> Property
prop_error_with_related_errors err related =
  let newErr = withRelatedErrors related err
  in relatedErrors newErr === related ++ relatedErrors err

-- | 测试错误过滤的属性
prop_has_category :: ErrorCategory -> TypeError -> Property
prop_has_category cat err =
  hasCategory cat err === (category err == cat)

prop_filter_by_category :: ErrorCategory -> [TypeError] -> Property
prop_filter_by_category cat errors =
  let filtered = filterByCategory cat errors
  in all (\e -> category e == cat) filtered

prop_filter_by_severity :: ErrorSeverity -> [TypeError] -> Property
prop_filter_by_severity sev errors =
  let filtered = filterBySeverity sev errors
  in all (\e -> severity e == sev) filtered

prop_combine_errors :: [TypeError] -> Property
prop_combine_errors errors =
  let combined = combineErrors errors
      originalCount = length errors
      relatedCount = sum $ map (length . relatedErrors) errors
  in length combined === originalCount + relatedCount

-- | 测试错误统计的属性
prop_error_statistics_counts :: [TypeError] -> Property
prop_error_statistics_counts errors =
  let stats = getErrorStatistics errors
      totalCount = Map.findWithDefault 0 "total" stats
      errorCount = Map.findWithDefault 0 "errors" errors
      warningCount = Map.findWithDefault 0 "warnings" errors
      infoCount = Map.findWithDefault 0 "info" errors
      fatalCount = Map.findWithDefault 0 "fatal" errors
  in totalCount === length errors .&&.
     errorCount === length (filterBySeverity Error errors) .&&.
     warningCount === length (filterBySeverity Warning errors) .&&.
     infoCount === length (filterBySeverity Info errors) .&&.
     fatalCount === length (filterBySeverity Fatal errors)

-- | 测试错误格式化的属性
prop_format_error_includes_message :: TypeError -> Property
prop_format_error_includes_message err =
  let formatted = formatError err
      msg = T.unpack $ message err
  in msg `isInfixOf` formatted

prop_format_error_with_location_includes_location :: TypeError -> Property
prop_format_error_with_location_includes_location err =
  let formatted = formatErrorWithLocation err
      locStr = show (line $ location err) ++ ":" ++ show (column $ location err)
  in locStr `isInfixOf` formatted

prop_format_errors_includes_all :: [TypeError] -> Property
prop_format_errors_includes_all errors =
  let formatted = formatErrors errors
      messages = map (T.unpack . message) errors
  in all (`isInfixOf` formatted) messages

-- | 测试CombinedError的属性
prop_combined_error_severity :: CombinedError -> Property
prop_combined_error_severity combinedErr =
  let sev = combinedErrorSeverity combinedErr
  in sev `elem` [Fatal, Error, Warning, Info]

prop_filter_combined_errors :: ErrorSeverity -> [CombinedError] -> Property
prop_filter_combined_errors minSeverity combinedErrors =
  let filtered = filterCombinedErrorsBySeverity minSeverity combinedErrors
  in all (\e -> isAtLeast (combinedErrorSeverity e) minSeverity) filtered

-- | 测试错误恢复的属性
prop_can_recover_from :: TypeError -> Property
prop_can_recover_from err =
  canRecoverFrom err === canRecover (recovery err)

prop_should_continue_after :: TypeError -> Property
prop_should_continue_after err =
  shouldContinueAfter err === shouldContinue (recovery err)

-- 辅助函数
errorAtLocation :: String -> Int -> Int -> ErrorLocation
errorAtLocation file line col = ErrorLocation (Just file) line col Nothing Nothing

errorAtRange :: Int -> Int -> Int -> Int -> ErrorLocation
errorAtRange startLine startCol endLine endCol = 
  ErrorLocation Nothing startLine startCol (Just endLine) (Just endCol)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

tests :: TestTree
tests = testGroup "ErrorHandler Advanced QuickCheck Tests"
  -- ErrorSeverity tests
  [ testProperty "severity priority order" prop_severity_priority_order
  , testProperty "severity priority range" prop_severity_priority_range
  , testProperty "isAtLeast reflexive" prop_is_at_least_reflexive
  , testProperty "isAtLeast transitive" prop_is_at_least_transitive
  , testProperty "severity total ordering" prop_severity_total_ordering
  
  -- ErrorLocation tests
  , testProperty "error location accessors" prop_error_location_accessors
  , testProperty "error location with file" prop_error_location_with_file
  , testProperty "error location with range" prop_error_location_with_range
  
  -- ErrorRecovery tests
  , testProperty "recovery can recover" prop_recovery_can_recover
  , testProperty "recovery cost range" prop_recovery_cost_range
  , testProperty "recovery confidence range" prop_recovery_confidence_range
  , testProperty "predefined recovery strategies" prop_predefined_recovery_strategies
  , testProperty "custom recovery" prop_custom_recovery
  
  -- TypeError creation tests
  , testProperty "error creation" prop_error_creation
  , testProperty "error with category" prop_error_with_category
  , testProperty "warning creation" prop_warning_creation
  , testProperty "info creation" prop_info_creation
  , testProperty "error with suggestions" prop_error_with_suggestions
  
  -- TypeError modification tests
  , testProperty "error with location" prop_error_with_location
  , testProperty "error with context" prop_error_with_context
  , testProperty "error with timestamp" prop_error_with_timestamp
  , testProperty "wrap error" prop_wrap_error
  , testProperty "error with related errors" prop_error_with_related_errors
  
  -- Error filtering tests
  , testProperty "has category" prop_has_category
  , testProperty "filter by category" prop_filter_by_category
  , testProperty "filter by severity" prop_filter_by_severity
  , testProperty "combine errors" prop_combine_errors
  
  -- Error statistics tests
  , testProperty "error statistics counts" prop_error_statistics_counts
  
  -- Error formatting tests
  , testProperty "format error includes message" prop_format_error_includes_message
  , testProperty "format error with location includes location" prop_format_error_with_location_includes_location
  , testProperty "format errors includes all" prop_format_errors_includes_all
  
  -- CombinedError tests
  , testProperty "combined error severity" prop_combined_error_severity
  , testProperty "filter combined errors" prop_filter_combined_errors
  
  -- Error recovery tests
  , testProperty "can recover from" prop_can_recover_from
  , testProperty "should continue after" prop_should_continue_after
  ]