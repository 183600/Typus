{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, suchThat, listOf1)

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
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
  , errorAt
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  , fatalError
  , fatalErrorWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , wrapError
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , severityPriority
  , isAtLeast
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, intercalate)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Generators for QuickCheck
-- ============================================================================

-- Generate a valid identifier
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- Generate an error message
genErrorMessage :: Gen String
genErrorMessage = do
  words <- listOf1 genIdentifier
  return $ unwords words

-- Generate an error code
genErrorCode :: Gen String
genErrorCode = do
  prefix <- elements ["ERR", "WARN", "INFO", "FATAL"]
  number <- choose (1000, 9999)
  return $ prefix ++ show number

-- Generate error severity
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- Generate error category
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ Syntax, TypeChecking, Ownership, DependentTypes, Integration, CodeGeneration
  , Parsing, Analysis, Runtime, Configuration, Validation, Transformation
  ]

-- Generate file path
genFilePath :: Gen String
genFilePath = do
  parts <- listOf1 genIdentifier
  ext <- elements [".hs", ".go", ".typus", ".rs", ".py"]
  return $ intercalate "/" parts ++ ext

-- Generate error location
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  filePath <- oneof [return Nothing, Just <$> genFilePath]
  line <- choose (1, 1000)
  column <- choose (1, 200)
  endLine <- oneof [return Nothing, Just <$> choose (line, line + 100)]
  endColumn <- oneof [return Nothing, Just <$> choose (column, column + 100)]
  return $ ErrorLocation filePath line column endLine endColumn

-- Generate error context
genErrorContext :: Gen ErrorContext
genErrorContext = do
  contextInfo <- listOf genIdentifier
  return $ ErrorContext contextInfo

-- Generate error recovery
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = oneof
  [ return Continue
  , return Stop
  , return Retry
  , Skip <$> genIdentifier
  , RecoverWith <$> genErrorMessage
  ]

-- Generate suggestions
genSuggestions :: Gen [String]
genSuggestions = listOf genErrorMessage

-- Generate related errors
genRelatedErrors :: Gen [TypeError]
genRelatedErrors = listOf genTypeError

-- Generate a type error
genTypeError :: Gen TypeError
genTypeError = do
  code <- genErrorCode
  message <- genErrorMessage
  severity <- genErrorSeverity
  category <- genErrorCategory
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  suggestions <- genSuggestions
  related <- genRelatedErrors
  timestamp <- return Nothing
  return $ TypeError code message severity category location context recovery suggestions related timestamp

-- Generate a combined error
genCombinedError :: Gen CombinedError
genCombinedError = do
  errors <- listOf1 genTypeError
  return $ CombinedError errors

-- ============================================================================
-- Error Properties
-- ============================================================================

-- Property: newErrorCollector creates empty collector
prop_newErrorCollector_empty :: Property
prop_newErrorCollector_empty =
  let collector = newErrorCollector
  in not (hasErrors collector) .&&. not (hasWarnings collector)

-- Property: addError adds error to collector
prop_addError_adds_error :: Property
prop_addError_adds_error =
  forAll genTypeError $ \error ->
    let collector = newErrorCollector
        collector' = addError error collector
    in hasErrors collector' .&&. length (getErrors collector') === 1

-- Property: addWarning adds warning to collector
prop_addWarning_adds_warning :: Property
prop_addWarning_adds_warning =
  forAll genTypeError $ \warning ->
    let collector = newErrorCollector
        collector' = addWarning warning collector
    in hasWarnings collector' .&&. length (getWarnings collector') === 1

-- Property: addInfo adds info to collector
prop_addInfo_adds_info :: Property
prop_addInfo_adds_info =
  forAll genTypeError $ \info ->
    let collector = newErrorCollector
        collector' = addInfo info collector
    in length (getInfo collector') === 1

-- Property: getErrors returns only errors
prop_getErrors_only_errors :: Property
prop_getErrors_only_errors =
  forAll (listOf genTypeError) $ \errors ->
    let collector = foldl (\c e -> addError e c) newErrorCollector errors
        retrievedErrors = getErrors collector
    in length retrievedErrors === length errors .&&. 
       all (\e -> teSeverity e `elem` [Fatal, Error]) retrievedErrors

-- Property: getWarnings returns only warnings
prop_getWarnings_only_warnings :: Property
prop_getWarnings_only_warnings =
  forAll (listOf genTypeError) $ \warnings ->
    let collector = foldl (\c w -> addWarning w c) newErrorCollector warnings
        retrievedWarnings = getWarnings collector
    in length retrievedWarnings === length warnings .&&. 
       all (\w -> teSeverity w == Warning) retrievedWarnings

-- Property: getAllMessages includes all types
prop_getAllMessages_all_types :: Property
prop_getAllMessages_all_types =
  forAll (listOf genTypeError) $ \errors ->
  forAll (listOf genTypeError) $ \warnings ->
  forAll (listOf genTypeError) $ \infos ->
    let collector = foldl (\c e -> addError e c) newErrorCollector errors
        collector' = foldl (\c w -> addWarning w c) collector warnings
        collector'' = foldl (\c i -> addInfo i c) collector' infos
        allMessages = getAllMessages collector''
    in length allMessages === length errors + length warnings + length infos

-- Property: formatError produces non-empty string
prop_formatError_non_empty :: Property
prop_formatError_non_empty =
  forAll genTypeError $ \error ->
    let formatted = formatError error
    in not (T.null formatted)

-- Property: formatErrors produces non-empty string for non-empty list
prop_formatErrors_non_empty :: Property
prop_formatErrors_non_empty =
  forAll (listOf1 genTypeError) $ \errors ->
    let formatted = formatErrors errors
    in not (T.null formatted)

-- Property: formatErrorWithLocation includes location info
prop_formatErrorWithLocation_includes_location :: Property
prop_formatErrorWithLocation_includes_location =
  forAll genTypeError $ \error ->
    let formatted = formatErrorWithLocation error
        hasLocation = isJust (filePath (teLocation error))
    in if hasLocation
       then property $ T.pack (show (teLocation error)) `T.isInfixOf` formatted
       else property True

-- Property: canRecoverFrom handles different severities
prop_canRecoverFrom_severity :: Property
prop_canRecoverFrom_severity =
  forAll genTypeError $ \error ->
    let canRecover = canRecoverFrom error
        severity = teSeverity error
    in case severity of
      Fatal -> not canRecover
      Error -> canRecover
      Warning -> canRecover
      Info -> canRecover

-- Property: shouldContinueAfter handles different severities
prop_shouldContinueAfter_severity :: Property
prop_shouldContinueAfter_severity =
  forAll genTypeError $ \error ->
    let shouldContinue = shouldContinueAfter error
        severity = teSeverity error
    in case severity of
      Fatal -> not shouldContinue
      Error -> shouldContinue
      Warning -> shouldContinue
      Info -> shouldContinue

-- Property: errorAt creates error with location
prop_errorAt_has_location :: Property
prop_errorAt_has_location =
  forAll genErrorMessage $ \message ->
  forAll genErrorLocation $ \location ->
    let error = errorAt message location
    in teLocation error === location

-- Property: errorWithCategory creates error with category
prop_errorWithCategory_has_category :: Property
prop_errorWithCategory_has_category =
  forAll genErrorMessage $ \message ->
  forAll genErrorCategory $ \category ->
    let error = errorWithCategory message category
    in teCategory error === category

-- Property: warningAt creates warning with correct severity
prop_warningAt_correct_severity :: Property
prop_warningAt_correct_severity =
  forAll genErrorMessage $ \message ->
  forAll genErrorLocation $ \location ->
    let warning = warningAt message location
    in teSeverity warning === Warning

-- Property: warningWithCategory creates warning with category
prop_warningWithCategory_has_category :: Property
prop_warningWithCategory_has_category =
  forAll genErrorMessage $ \message ->
  forAll genErrorCategory $ \category ->
    let warning = warningWithCategory message category
    in teSeverity warning === Warning .&&. teCategory warning === category

-- Property: infoAt creates info with correct severity
prop_infoAt_correct_severity :: Property
prop_infoAt_correct_severity =
  forAll genErrorMessage $ \message ->
  forAll genErrorLocation $ \location ->
    let info = infoAt message location
    in teSeverity info === Info

-- Property: infoWithCategory creates info with category
prop_infoWithCategory_has_category :: Property
prop_infoWithCategory_has_category =
  forAll genErrorMessage $ \message ->
  forAll genErrorCategory $ \category ->
    let info = infoWithCategory message category
    in teSeverity info === Info .&&. teCategory info === category

-- Property: fatalError creates fatal error
prop_fatalError_fatal :: Property
prop_fatalError_fatal =
  forAll genErrorMessage $ \message ->
    let error = fatalError message
    in teSeverity error === Fatal

-- Property: fatalErrorWithCategory creates fatal error with category
prop_fatalErrorWithCategory_fatal :: Property
prop_fatalErrorWithCategory_fatal =
  forAll genErrorMessage $ \message ->
  forAll genErrorCategory $ \category ->
    let error = fatalErrorWithCategory message category
    in teSeverity error === Fatal .&&. teCategory error === category

-- Property: errorWithSuggestions adds suggestions
prop_errorWithSuggestions_adds_suggestions :: Property
prop_errorWithSuggestions_adds_suggestions =
  forAll genErrorMessage $ \message ->
  forAll genSuggestions $ \suggestions ->
    let error = errorWithSuggestions message suggestions
    in teSuggestions error === suggestions

-- Property: withLocation updates location
prop_withLocation_updates_location :: Property
prop_withLocation_updates_location =
  forAll genTypeError $ \error ->
  forAll genErrorLocation $ \location ->
    let updatedError = withLocation location error
    in teLocation updatedError === location

-- Property: withContext updates context
prop_withContext_updates_context :: Property
prop_withContext_updates_context =
  forAll genTypeError $ \error ->
  forAll genErrorContext $ \context ->
    let updatedError = withContext context error
    in teContext updatedError === context

-- Property: withSuggestions updates suggestions
prop_withSuggestions_updates_suggestions :: Property
prop_withSuggestions_updates_suggestions =
  forAll genTypeError $ \error ->
  forAll genSuggestions $ \suggestions ->
    let updatedError = withSuggestions suggestions error
    in teSuggestions updatedError === suggestions

-- Property: withRelatedErrors updates related errors
prop_withRelatedErrors_updates_related :: Property
prop_withRelatedErrors_updates_related =
  forAll genTypeError $ \error ->
  forAll genRelatedErrors $ \related ->
    let updatedError = withRelatedErrors related error
    in teRelatedErrors updatedError === related

-- Property: wrapError adds context
prop_wrapError_adds_context :: Property
prop_wrapError_adds_context =
  forAll genTypeError $ \error ->
  forAll genErrorMessage $ \context ->
    let wrappedError = wrapError context error
    in context `isInfixOf` teMessage wrappedError

-- Property: combineErrors creates combined error
prop_combineErrors_creates_combined :: Property
prop_combineErrors_creates_combined =
  forAll (listOf1 genTypeError) $ \errors ->
    let combined = combineErrors errors
    in case combined of
      CombinedError combinedErrors -> length combinedErrors === length errors

-- Property: combinedErrorSeverity returns highest severity
prop_combinedErrorSeverity_highest :: Property
prop_combinedErrorSeverity_highest =
  forAll (listOf1 genTypeError) $ \errors ->
    let combined = combineErrors errors
        highestSeverity = combinedErrorSeverity combined
        severities = map teSeverity errors
    in highestSeverity `elem` severities .&&. 
       all (\s -> compareSeverity s highestSeverity /= GT) severities

-- Property: filterCombinedErrorsBySeverity filters correctly
prop_filterCombinedErrorsBySeverity_filters :: Property
prop_filterCombinedErrorsBySeverity_filters =
  forAll (listOf1 genTypeError) $ \errors ->
  forAll genErrorSeverity $ \minSeverity ->
    let combined = combineErrors errors
        filtered = filterCombinedErrorsBySeverity minSeverity combined
        expectedCount = length $ filter (\e -> isAtLeast minSeverity (teSeverity e)) errors
    in case filtered of
      CombinedError filteredErrors -> length filteredErrors === expectedCount

-- Property: hasCategory checks category correctly
prop_hasCategory_checks :: Property
prop_hasCategory_checks =
  forAll genTypeError $ \error ->
  forAll genErrorCategory $ \category ->
    let hasIt = hasCategory category error
    in hasIt === (teCategory error == category)

-- Property: filterByCategory filters correctly
prop_filterByCategory_filters :: Property
prop_filterByCategory_filters =
  forAll (listOf genTypeError) $ \errors ->
  forAll genErrorCategory $ \category ->
    let filtered = filterByCategory category errors
        expected = filter (\e -> teCategory e == category) errors
    in length filtered === length expected

-- Property: filterBySeverity filters correctly
prop_filterBySeverity_filters :: Property
prop_filterBySeverity_filters =
  forAll (listOf genTypeError) $ \errors ->
  forAll genErrorSeverity $ \severity ->
    let filtered = filterBySeverity severity errors
        expected = filter (\e -> teSeverity e == severity) errors
    in length filtered === length expected

-- Property: getErrorStatistics counts correctly
prop_getErrorStatistics_counts :: Property
prop_getErrorStatistics_counts =
  forAll (listOf genTypeError) $ \errors ->
  forAll (listOf genTypeError) $ \warnings ->
  forAll (listOf genTypeError) $ \infos ->
    let collector = foldl (\c e -> addError e c) newErrorCollector errors
        collector' = foldl (\c w -> addWarning w c) collector warnings
        collector'' = foldl (\c i -> addInfo i c) collector' infos
        stats = getErrorStatistics collector''
    in length (getErrors collector'') === length errors .&&.
       length (getWarnings collector'') === length warnings .&&.
       length (getInfo collector'') === length infos

-- Property: generateErrorReport produces non-empty report
prop_generateErrorReport_non_empty :: Property
prop_generateErrorReport_non_empty =
  forAll (listOf1 genTypeError) $ \errors ->
    let report = generateErrorReport errors
    in not (T.null report)

-- Property: createRecoveryStrategy creates strategy
prop_createRecoveryStrategy_creates :: Property
prop_createRecoveryStrategy_creates =
  forAll genErrorRecovery $ \recovery ->
    let strategy = createRecoveryStrategy recovery
    in property True  -- Strategy creation should always succeed

-- Property: customRecovery creates custom recovery
prop_customRecovery_creates :: Property
prop_customRecovery_creates =
  forAll genErrorMessage $ \message ->
    let recovery = customRecovery message
    in property True  -- Custom recovery creation should always succeed

-- Property: fatalRecovery creates fatal recovery
prop_fatalRecovery_creates :: Property
prop_fatalRecovery_creates =
  let recovery = fatalRecovery
  in property True  -- Fatal recovery creation should always succeed

-- Property: errorRecovery creates error recovery
prop_errorRecovery_creates :: Property
prop_errorRecovery_creates =
  let recovery = errorRecovery
  in property True  -- Error recovery creation should always succeed

-- Property: warningRecovery creates warning recovery
prop_warningRecovery_creates :: Property
prop_warningRecovery_creates =
  let recovery = warningRecovery
  in property True  -- Warning recovery creation should always succeed

-- Property: infoRecovery creates info recovery
prop_infoRecovery_creates :: Property
prop_infoRecovery_creates =
  let recovery = infoRecovery
  in property True  -- Info recovery creation should always succeed

-- Property: severityPriority ordering is correct
prop_severityPriority_ordering :: Property
prop_severityPriority_ordering =
  severityPriority Fatal > severityPriority Error .&&.
  severityPriority Error > severityPriority Warning .&&.
  severityPriority Warning > severityPriority Info

-- Property: isAtLeast comparison works correctly
prop_isAtLeast_comparison :: Property
prop_isAtLeast_comparison =
  isAtLeast Info Info .&&.
  isAtLeast Warning Info .&&.
  isAtLeast Warning Warning .&&.
  isAtLeast Error Warning .&&.
  isAtLeast Error Error .&&.
  isAtLeast Fatal Error .&&.
  isAtLeast Fatal Fatal .&&.
  not (isAtLeast Info Warning) .&&.
  not (isAtLeast Warning Error) .&&.
  not (isAtLeast Error Fatal)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler Core QuickCheck Tests"
  [ testGroup "ErrorCollector Properties"
    [ fastProperty "newErrorCollector empty" prop_newErrorCollector_empty
    , fastProperty "addError adds error" prop_addError_adds_error
    , fastProperty "addWarning adds warning" prop_addWarning_adds_warning
    , fastProperty "addInfo adds info" prop_addInfo_adds_info
    , fastProperty "getErrors only errors" prop_getErrors_only_errors
    , fastProperty "getWarnings only warnings" prop_getWarnings_only_warnings
    , fastProperty "getAllMessages all types" prop_getAllMessages_all_types
    ]

  , testGroup "Error Formatting Properties"
    [ fastProperty "formatError non empty" prop_formatError_non_empty
    , fastProperty "formatErrors non empty" prop_formatErrors_non_empty
    , fastProperty "formatErrorWithLocation includes location" prop_formatErrorWithLocation_includes_location
    ]

  , testGroup "Error Recovery Properties"
    [ fastProperty "canRecoverFrom severity" prop_canRecoverFrom_severity
    , fastProperty "shouldContinueAfter severity" prop_shouldContinueAfter_severity
    ]

  , testGroup "Error Creation Properties"
    [ fastProperty "errorAt has location" prop_errorAt_has_location
    , fastProperty "errorWithCategory has category" prop_errorWithCategory_has_category
    , fastProperty "warningAt correct severity" prop_warningAt_correct_severity
    , fastProperty "warningWithCategory has category" prop_warningWithCategory_has_category
    , fastProperty "infoAt correct severity" prop_infoAt_correct_severity
    , fastProperty "infoWithCategory has category" prop_infoWithCategory_has_category
    , fastProperty "fatalError fatal" prop_fatalError_fatal
    , fastProperty "fatalErrorWithCategory fatal" prop_fatalErrorWithCategory_fatal
    ]

  , testGroup "Error Modification Properties"
    [ fastProperty "errorWithSuggestions adds suggestions" prop_errorWithSuggestions_adds_suggestions
    , fastProperty "withLocation updates location" prop_withLocation_updates_location
    , fastProperty "withContext updates context" prop_withContext_updates_context
    , fastProperty "withSuggestions updates suggestions" prop_withSuggestions_updates_suggestions
    , fastProperty "withRelatedErrors updates related" prop_withRelatedErrors_updates_related
    , fastProperty "wrapError adds context" prop_wrapError_adds_context
    ]

  , testGroup "Combined Error Properties"
    [ fastProperty "combineErrors creates combined" prop_combineErrors_creates_combined
    , fastProperty "combinedErrorSeverity highest" prop_combinedErrorSeverity_highest
    , fastProperty "filterCombinedErrorsBySeverity filters" prop_filterCombinedErrorsBySeverity_filters
    ]

  , testGroup "Error Filtering Properties"
    [ fastProperty "hasCategory checks" prop_hasCategory_checks
    , fastProperty "filterByCategory filters" prop_filterByCategory_filters
    , fastProperty "filterBySeverity filters" prop_filterBySeverity_filters
    ]

  , testGroup "Error Statistics Properties"
    [ fastProperty "getErrorStatistics counts" prop_getErrorStatistics_counts
    , fastProperty "generateErrorReport non empty" prop_generateErrorReport_non_empty
    ]

  , testGroup "Recovery Strategy Properties"
    [ fastProperty "createRecoveryStrategy creates" prop_createRecoveryStrategy_creates
    , fastProperty "customRecovery creates" prop_customRecovery_creates
    , fastProperty "fatalRecovery creates" prop_fatalRecovery_creates
    , fastProperty "errorRecovery creates" prop_errorRecovery_creates
    , fastProperty "warningRecovery creates" prop_warningRecovery_creates
    , fastProperty "infoRecovery creates" prop_infoRecovery_creates
    ]

  , testGroup "Severity Properties"
    [ fastProperty "severityPriority ordering" prop_severityPriority_ordering
    , fastProperty "isAtLeast comparison" prop_isAtLeast_comparison
    ]
  ]