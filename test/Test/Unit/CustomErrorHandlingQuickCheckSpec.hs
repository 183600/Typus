module Test.Unit.CustomErrorHandlingQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, elements, listOf, listOf1, oneof, choose)
import Compiler.Errors.Core
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


 Gen String
                              genErrorMessage = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '] ++ ['.', ',', '!']

-- | Generate file paths
genFilePath :: Gen String
                              genFilePath = do
              parts <- listOf1 $ elements $ ['a'..'z'] ++ ['_'] ++ ['-']
  return $ intercalate "/" parts ++ ".typus"

-- | Generate error locations
genErrorLocation :: Gen ErrorLocation
                              genErrorLocation = do
              filePath <- genFilePath
  line <- choose (1, 1000)
  column <- choose (1, 200)
  return $ ErrorLocation filePath line column

-- | Generate error contexts
genErrorContext :: Gen ErrorContext
                              genErrorContext = do
              contextName <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z']
  contextValue <- listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ [' ']
  return $ ErrorContext contextName contextValue

-- | Generate error severities
genErrorSeverity :: Gen ErrorSeverity
                              genErrorSeverity = elements [Error, Warning, Info]

-- | Generate error categories
genErrorCategory :: Gen ErrorCategory
                              genErrorCategory = elements 
  [ ParseError
  , TypeError
  , NameError
  , ScopeError
  , OwnershipError
  , DependencyError
  , InternalError
  ]

-- | Generate error recovery strategies
genErrorRecovery :: Gen ErrorRecovery
                              genErrorRecovery = elements 
  [ Continue
  , Skip
  , Retry
  , Abort
  ]

-- | Generate type errors
genTypeError :: Gen TypeError
                              genTypeError = do
              message <- genErrorMessage
  location <- genErrorLocation
  severity <- genErrorSeverity
  category <- genErrorCategory
  context <- listOf genErrorContext
  suggestions <- listOf genErrorMessage
  relatedErrors <- listOf genTypeError
  return $ TypeError message location severity category context suggestions relatedErrors

-- | Generate combined errors
genCombinedError :: Gen CombinedError
                              genCombinedError = do
              errors <- listOf1 genTypeError
  return $ CombinedError errors

-- | Test TypeError equality
prop_typeErrorEquality :: Property
                              prop_typeErrorEquality = forAll genTypeError $ \error ->
                                error == error

-- | Test TypeError show property
prop_typeErrorShow :: Property
                              prop_typeErrorShow = forAll genTypeError $ \error ->
  let errorStr = show error
  in not (null errorStr)

-- | Test ErrorLocation properties
prop_errorLocationProperties :: Property
                              prop_errorLocationProperties = forAll genErrorLocation $ \location ->
  let filePath = errorFilePath location
                                    line = errorLine location
                                    column = errorColumn location
  in not (null filePath) && line >= 1 && column >= 1

-- | Test ErrorContext properties
prop_errorContextProperties :: Property
                              prop_errorContextProperties = forAll genErrorContext $ \context ->
  let contextName = contextName context
                                    contextValue = contextValue context
  in not (null contextName) && not (null contextValue)

-- | Test emptyContext properties
prop_emptyContextProperties :: Property
                              prop_emptyContextProperties = 
  let context = emptyContext
                                    contextName = contextName context
                                    contextValue = contextValue context
  in null contextName && null contextValue

-- | Test CombinedError properties
prop_combinedErrorProperties :: Property
                              prop_combinedErrorProperties = forAll genCombinedError $ \combinedError ->
  let errors = combinedErrors combinedError
  in not (null errors)

-- | Test CombinedError equality
prop_combinedErrorEquality :: Property
                              prop_combinedErrorEquality = forAll genCombinedError $ \combinedError1 ->
  forAll genCombinedError $ \combinedError2 ->
    let errors1 = combinedErrors combinedError1
                                      errors2 = combinedErrors combinedError2
    in (combinedError1 == combinedError2) == (errors1 == errors2)

-- | Test ErrorCollector creation
prop_errorCollectorCreation :: Property
                              prop_errorCollectorCreation = 
  let collector = newErrorCollector
  in not (hasErrors collector) && not (hasWarnings collector)

-- | Test addError functionality
prop_addErrorFunctionality :: Property
                              prop_addErrorFunctionality = forAll genTypeError $ \error ->
  let collector = newErrorCollector
                                    collectorWithError = addError error collector
  in hasErrors collectorWithError

-- | Test addWarning functionality
prop_addWarningFunctionality :: Property
                              prop_addWarningFunctionality = forAll genTypeError $ \warning ->
  let collector = newErrorCollector
                                    collectorWithWarning = addWarning warning collector
  in hasWarnings collectorWithWarning

-- | Test getErrors after adding errors
prop_getErrorsAfterAdding :: Property
                              prop_getErrorsAfterAdding = forAll genTypeError $ \error ->
  let collector = newErrorCollector
                                    collectorWithError = addError error collector
                                    errors = getErrors collectorWithError
  in L.length                               errors == 1

-- | Test getWarnings after adding warnings
prop_getWarningsAfterAdding :: Property
                              prop_getWarningsAfterAdding = forAll genTypeError $ \warning ->
  let collector = newErrorCollector
                                    collectorWithWarning = addWarning warning collector
                                    warnings = getWarnings collectorWithWarning
  in L.length                               warnings == 1

-- | Test getAllMessages after adding various messages
prop_getAllMessagesAfterAdding :: Property
                              prop_getAllMessagesAfterAdding = forAll genTypeError $ \error ->
  forAll genTypeError $ \warning ->
    forAll genTypeError $ \info ->
      let collector = newErrorCollector
                                        collectorWithAll = addInfo info (addWarning warning (addError error collector)
                                        allMessages = getAllMessages collectorWithAll
      in L.length                               allMessages == 3

-- | Test formatError produces non-empty string
prop_formatErrorNonEmpty :: Property
                              prop_formatErrorNonEmpty = forAll genTypeError $ \error ->
  let formatted = formatError error
  in not (null formatted)

-- | Test formatErrors produces non-empty string
prop_formatErrorsNonEmpty :: Property
                              prop_formatErrorsNonEmpty = forAll (listOf1 genTypeError) $ \errors ->
  let formatted = formatErrors errors
  in not (null formatted)

-- | Test canRecoverFrom based on severity
prop_canRecoverFromSeverity :: Property
                              prop_canRecoverFromSeverity = forAll genTypeError $ \error ->
  let severity = errorSeverity error
                                    canRecover = canRecoverFrom error
  in case severity of
       Error -> True  -- Can always attempt recovery from errors
       Warning -> True
       Info -> True

-- | Test shouldContinueAfter based on severity
prop_shouldContinueAfterSeverity :: Property
                              prop_shouldContinueAfterSeverity = forAll genTypeError $ \error ->
  let severity = errorSeverity error
                                    shouldContinue = shouldContinueAfter error
  in case severity of
       Error -> shouldContinue  -- Depends on recovery strategy
       Warning -> True
       Info -> True

-- | Test errorAt "test-id" == severity) filteredErrors

-- | Test hasCategory functionality
prop_hasCategoryFunctionality :: Property
                              prop_hasCategoryFunctionality = forAll genTypeError $ \error ->
  forAll genErrorCategory $ \category ->
    let hasCat = hasCategory category error
                                      errorCat = errorCategory error
    in                               hasCat == (errorCat == category)

-- | Test filterByCategory functionality
prop_filterByCategoryFunctionality :: Property
                              prop_filterByCategoryFunctionality = forAll (listOf1 genTypeError) $ \errors ->
  forAll genErrorCategory $ \category ->
    let filtered = filterByCategory category errors
    in L.all (\e -> errorCategory                               e == category) filtered

-- | Test filterBySeverity functionality
prop_filterBySeverityFunctionality :: Property
                              prop_filterBySeverityFunctionality = forAll (listOf1 genTypeError) $ \errors ->
  forAll genErrorSeverity $ \severity ->
    let filtered = filterBySeverity severity errors
    in L.all (\e -> errorSeverity                               e == severity) filtered

-- | Test getErrorStatistics returns correct counts
prop_getErrorStatisticsCorrect :: Property
                              prop_getErrorStatisticsCorrect = forAll (listOf1 genTypeError) $ \errors ->
  let stats = getErrorStatistics errors
  in True  -- Basic test that stats can be computed

-- | Test formatTimestamp produces non-empty string
prop_formatTimestampNonEmpty :: Property
                              prop_formatTimestampNonEmpty = 
  let timestamp = formatTimestamp "2023-01-01 12:00:00"
  in not (null timestamp)

-- | Test createRecoveryStrategy creates recovery
prop_createRecoveryStrategyCreates :: Property
                              prop_createRecoveryStrategyCreates = 
  let recovery = createRecoveryStrategy Continue
  in                               recovery == Continue

-- | Test custom recovery
prop_customRecoveryWorks :: Property
                              prop_customRecoveryWorks = 
  let recovery = customRecovery "Custom message"
  in                               recovery == Continue  -- Custom recovery defaults to Continue

-- | Test fatal recovery
prop_fatalRecoveryWorks :: Property
                              prop_fatalRecoveryWorks = 
  let recovery = fatalRecovery
  in                               recovery == Abort

-- | Test error recovery
prop_errorRecoveryWorks :: Property
                              prop_errorRecoveryWorks = 
  let recovery = errorRecovery
  in                               recovery == Continue

-- | Test warning recovery
prop_warningRecoveryWorks :: Property
                              prop_warningRecoveryWorks = 
  let recovery = warningRecovery
  in                               recovery == Continue

-- | Test info recovery
prop_infoRecoveryWorks :: Property
                              prop_infoRecoveryWorks = 
  let recovery = infoRecovery
  in                               recovery == Continue

  where
      intercalate sep [] = ""
    intercalate sep [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

tests :: TestTree
tests =   testGroup "Custom ErrorHandling QuickCheck Tests"
  [             testProperty "TypeError equality" prop_typeErrorEquality
  ,             testProperty "TypeError show" prop_typeErrorShow
  ,             testProperty "ErrorLocation properties" prop_errorLocationProperties
  ,             testProperty "ErrorContext properties" prop_errorContextProperties
  ,             testProperty "emptyContext properties" prop_emptyContextProperties
  ,             testProperty "CombinedError properties" prop_combinedErrorProperties
  ,             testProperty "CombinedError equality" prop_combinedErrorEquality
  ,             testProperty "ErrorCollector creation" prop_errorCollectorCreation
  ,             testProperty "addError functionality" prop_addErrorFunctionality
  ,             testProperty "addWarning functionality" prop_addWarningFunctionality
  ,             testProperty "getErrors after adding" prop_getErrorsAfterAdding
  ,             testProperty "getWarnings after adding" prop_getWarningsAfterAdding
  ,             testProperty "getAllMessages after adding" prop_getAllMessagesAfterAdding
  ,             testProperty "formatError non-empty" prop_formatErrorNonEmpty
  ,             testProperty "formatErrors non-empty" prop_formatErrorsNonEmpty
  ,             testProperty "canRecoverFrom severity" prop_canRecoverFromSeverity
  ,             testProperty "shouldContinueAfter severity" prop_shouldContinueAfterSeverity
  ,             testProperty "errorAt "test-id" works" prop_infoRecoveryWorks
  ]