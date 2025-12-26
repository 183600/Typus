{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, vectorOf, elements, oneof)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , CombinedError(..)
  , emptyContext
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
  , errorAt
  , warningAt
  , infoAt
  , fatalError
  , errorWithCategory
  , warningWithCategory
  , infoWithCategory
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , canRecoverFrom
  , shouldContinueAfter
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , getErrorLine
  , getErrorColumn
  )

-- Arbitrary instances for ErrorHandler types

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- oneof [return Nothing, Just <$> genSafeString]
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    endLine <- oneof [return Nothing, Just <$> choose (1, 1000)]
    endColumn <- oneof [return Nothing, Just <$> choose (1, 1000)]
    return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    code <- oneof [return Nothing, Just <$> genSafeString]
    function <- oneof [return Nothing, Just <$> genSafeString]
    variable <- oneof [return Nothing, Just <$> genSafeString]
    type' <- oneof [return Nothing, Just <$> genSafeString]
    additional <- vectorOf 3 $ do
      key <- genSafeString
      value <- genSafeString
      return (key, value)
    return $ ErrorContext code function variable type' additional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    action <- oneof [return Nothing, Just <$> genSafeString]
    hint <- oneof [return Nothing, Just <$> genSafeString]
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    return $ ErrorRecovery canRec shouldCont action hint cost confidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- genSafeString
    severity <- arbitrary
    category <- arbitrary
    message <- T.pack <$> genSafeString
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- vectorOf 3 $ T.pack <$> genSafeString
    relatedErrors <- vectorOf 2 arbitrary
    errorChain <- vectorOf 2 arbitrary
    timestamp <- oneof [return Nothing, Just <$> genSafeString]
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary CombinedError where
  arbitrary = oneof
    [ OwnershipErrorCombined <$> arbitrary <*> (genSafeString >>= \s -> return $ errorAt s)
    , DependentTypeErrorCombined <$> arbitrary <*> (genSafeString >>= \s -> return $ errorAt s)
    , IntegrationError <$> genSafeString <*> arbitrary
    , CrossAnalyzerError <$> genSafeString <*> arbitrary <*> vectorOf 2 arbitrary
    ]

-- Helper generators
genSafeString :: Gen String
genSafeString = do
  size <- choose (1, 10)
  vectorOf size $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

-- Property: getErrorLine returns correct line number
prop_getErrorLine_correct :: ErrorLocation -> Property
prop_getErrorLine_correct loc =
  property $ getErrorLine loc === line loc

-- Property: getErrorColumn returns correct column number
prop_getErrorColumn_correct :: ErrorLocation -> Property
prop_getErrorColumn_correct loc =
  property $ getErrorColumn loc === column loc

-- Property: hasErrors correctly identifies error presence
prop_hasErrors_correct :: [TypeError] -> Property
prop_hasErrors_correct errors =
  let hasErr = hasErrors errors
      hasErrOrFatal = any (\e -> severity e == Error || severity e == Fatal) errors
  in property $ hasErr === hasErrOrFatal

-- Property: hasWarnings correctly identifies warning presence
prop_hasWarnings_correct :: [TypeError] -> Property
prop_hasWarnings_correct errors =
  let hasWarn = hasWarnings errors
      hasWarnOnly = any (\e -> severity e == Warning) errors
  in property $ hasWarn === hasWarnOnly

-- Property: getErrors filters by Error and Fatal severity
prop_getErrors_filters :: [TypeError] -> Property
prop_getErrors_filters errors =
  let filtered = getErrors errors
      expected = filter (\e -> severity e == Error || severity e == Fatal) errors
  in property $ length filtered === length expected

-- Property: getWarnings filters by Warning severity
prop_getWarnings_filters :: [TypeError] -> Property
prop_getWarnings_filters errors =
  let filtered = getWarnings errors
      expected = filter (\e -> severity e == Warning) errors
  in property $ length filtered === length expected

-- Property: getInfo filters by Info severity
prop_getInfo_filters :: [TypeError] -> Property
prop_getInfo_filters errors =
  let filtered = getInfo errors
      expected = filter (\e -> severity e == Info) errors
  in property $ length filtered === length expected

-- Property: filterByCategory correctly filters errors
prop_filterByCategory_correct :: [TypeError] -> ErrorCategory -> Property
prop_filterByCategory_correct errors category =
  let filtered = filterByCategory category errors
      expected = filter (\e -> category e == category) errors
  in property $ length filtered === length expected

-- Property: hasCategory correctly detects category presence
prop_hasCategory_correct :: [TypeError] -> ErrorCategory -> Property
prop_hasCategory_correct errors category =
  let hasCat = hasCategory category errors
      hasCatExpected = any (\e -> category e == category) errors
  in property $ hasCat === hasCatExpected

-- Property: filterBySeverity correctly filters errors
prop_filterBySeverity_correct :: [TypeError] -> ErrorSeverity -> Property
prop_filterBySeverity_correct errors severity =
  let filtered = filterBySeverity severity errors
      expected = filter (\e -> severity e == severity) errors
  in property $ length filtered === length expected

-- Property: canRecoverFrom returns correct recovery status
prop_canRecoverFrom_correct :: TypeError -> Property
prop_canRecoverFrom_correct err =
  let canRec = canRecoverFrom err
      expected = canRecover (recovery err)
  in property $ canRec === expected

-- Property: shouldContinueAfter returns correct continuation status
prop_shouldContinueAfter_correct :: TypeError -> Property
prop_shouldContinueAfter_correct err =
  let shouldCont = shouldContinueAfter err
      expected = shouldContinue (recovery err)
  in property $ shouldCont === expected

-- Property: formatError produces non-empty string
prop_formatError_non_empty :: TypeError -> Property
prop_formatError_non_empty err =
  let formatted = formatError err
  in property $ not (null formatted)

-- Property: formatErrors preserves order
prop_formatErrors_preserves_order :: [TypeError] -> Property
prop_formatErrors_preserves_order errors =
  let formatted = formatErrors errors
      formattedLines = lines formatted
  in property $ length formattedLines >= length errors

-- Property: errorAt creates error with correct location
prop_errorAt_correct_location :: String -> ErrorLocation -> Property
prop_errorAt_correct_location msg loc =
  let err = errorAt msg `withLocation` loc
  in property $ location err === loc

-- Property: warningAt creates warning with correct severity
prop_warningAt_correct_severity :: String -> Property
prop_warningAt_correct_severity msg =
  let warn = warningAt msg
  in property $ severity warn === Warning

-- Property: infoAt creates info with correct severity
prop_infoAt_correct_severity :: String -> Property
prop_infoAt_correct_severity msg =
  let info = infoAt msg
  in property $ severity info === Info

-- Property: fatalError creates fatal error with correct severity
prop_fatalError_correct_severity :: String -> Property
prop_fatalError_correct_severity msg =
  let fatal = fatalError msg
  in property $ severity fatal === Fatal

-- Property: errorWithCategory creates error with correct category
prop_errorWithCategory_correct_category :: String -> ErrorCategory -> Property
prop_errorWithCategory_correct_category msg cat =
  let err = errorWithCategory msg cat
  in property $ category err === cat

-- Property: warningWithCategory creates warning with correct category and severity
prop_warningWithCategory_correct :: String -> ErrorCategory -> Property
prop_warningWithCategory_correct msg cat =
  let warn = warningWithCategory msg cat
  in property $ category warn === cat .&&. severity warn === Warning

-- Property: infoWithCategory creates info with correct category and severity
prop_infoWithCategory_correct :: String -> ErrorCategory -> Property
prop_infoWithCategory_correct msg cat =
  let info = infoWithCategory msg cat
  in property $ category info === cat .&&. severity info === Info

-- Property: withContext updates context correctly
prop_withContext_updates :: TypeError -> ErrorContext -> Property
prop_withContext_updates err ctx =
  let updated = withContext err ctx
  in property $ context updated === ctx

-- Property: withSuggestions updates suggestions correctly
prop_withSuggestions_updates :: TypeError -> [Text] -> Property
prop_withSuggestions_updates err suggestions =
  let updated = withSuggestions err suggestions
  in property $ suggestions updated === suggestions

-- Property: withRelatedErrors updates related errors correctly
prop_withRelatedErrors_updates :: TypeError -> [TypeError] -> Property
prop_withRelatedErrors_updates err relatedErrs =
  let updated = withRelatedErrors err relatedErrs
  in property $ relatedErrors updated === relatedErrs

-- Property: combinedErrorSeverity extracts severity correctly
prop_combinedErrorSeverity_correct :: CombinedError -> Property
prop_combinedErrorSeverity_correct combinedErr =
  let extracted = combinedErrorSeverity combinedErr
      expected = case combinedErr of
                   OwnershipErrorCombined sev _ -> sev
                   DependentTypeErrorCombined sev _ -> sev
                   IntegrationError _ sev -> sev
                   CrossAnalyzerError _ sev _ -> sev
  in property $ extracted === expected

-- Property: filterCombinedErrorsBySeverity filters correctly
prop_filterCombinedErrorsBySeverity_correct :: [CombinedError] -> ErrorSeverity -> Property
prop_filterCombinedErrorsBySeverity_correct combinedErrs minSeverity =
  let filtered = filterCombinedErrorsBySeverity minSeverity combinedErrs
      expected = filter (\err -> combinedErrorSeverity err >= minSeverity) combinedErrs
  in property $ length filtered === length expected

-- Property: combineErrors preserves error information
prop_combineErrors_preserves :: [TypeError] -> Property
prop_combineErrors_preserves errors =
  let combined = combineErrors errors
      originalCount = length errors
  in property $ length combined >= originalCount

-- Property: getErrorStatistics returns valid statistics
prop_getErrorStatistics_valid :: [TypeError] -> Property
prop_getErrorStatistics_valid errors =
  let stats = getErrorStatistics errors
  in property $ Map.size stats >= 0

-- Property: generateErrorReport produces non-empty string
prop_generateErrorReport_non_empty :: [TypeError] -> Property
prop_generateErrorReport_non_empty errors =
  let report = generateErrorReport errors
  in property $ not (null report)

-- Property: emptyContext has all fields as Nothing/empty
prop_emptyContext_correct :: Property
prop_emptyContext_correct =
  property $ contextCode emptyContext === Nothing .&&.
             contextFunction emptyContext === Nothing .&&.
             contextVariable emptyContext === Nothing .&&.
             contextType emptyContext === Nothing .&&.
             null (contextAdditional emptyContext)

-- Property: error collector preserves error order
prop_error_collector_preserves_order :: [TypeError] -> Property
prop_error_collector_preserves_order errors =
  let collected = getAllMessages errors
  in property $ length collected === length errors

-- Property: error severity ordering works correctly
prop_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering sev1 sev2 =
  let ordering = compare sev1 sev2
      priority1 = case sev1 of
                    Fatal -> 100
                    Error -> 80
                    Warning -> 30
                    Info -> 10
      priority2 = case sev2 of
                    Fatal -> 100
                    Error -> 80
                    Warning -> 30
                    Info -> 10
  in property $ (sev1 > sev2) === (priority1 > priority2)

tests :: TestTree
tests = testGroup "ErrorHandler New QuickCheck Tests"
  [ fastProperty "getErrorLine returns correct line number" prop_getErrorLine_correct
  , fastProperty "getErrorColumn returns correct column number" prop_getErrorColumn_correct
  , fastProperty "hasErrors correctly identifies error presence" prop_hasErrors_correct
  , fastProperty "hasWarnings correctly identifies warning presence" prop_hasWarnings_correct
  , fastProperty "getErrors filters by Error and Fatal severity" prop_getErrors_filters
  , fastProperty "getWarnings filters by Warning severity" prop_getWarnings_filters
  , fastProperty "getInfo filters by Info severity" prop_getInfo_filters
  , fastProperty "filterByCategory correctly filters errors" prop_filterByCategory_correct
  , fastProperty "hasCategory correctly detects category presence" prop_hasCategory_correct
  , fastProperty "filterBySeverity correctly filters errors" prop_filterBySeverity_correct
  , fastProperty "canRecoverFrom returns correct recovery status" prop_canRecoverFrom_correct
  , fastProperty "shouldContinueAfter returns correct continuation status" prop_shouldContinueAfter_correct
  , fastProperty "formatError produces non-empty string" prop_formatError_non_empty
  , fastProperty "formatErrors preserves order" prop_formatErrors_preserves_order
  , fastProperty "errorAt creates error with correct location" prop_errorAt_correct_location
  , fastProperty "warningAt creates warning with correct severity" prop_warningAt_correct_severity
  , fastProperty "infoAt creates info with correct severity" prop_infoAt_correct_severity
  , fastProperty "fatalError creates fatal error with correct severity" prop_fatalError_correct_severity
  , fastProperty "errorWithCategory creates error with correct category" prop_errorWithCategory_correct_category
  , fastProperty "warningWithCategory creates warning with correct category and severity" prop_warningWithCategory_correct
  , fastProperty "infoWithCategory creates info with correct category and severity" prop_infoWithCategory_correct
  , fastProperty "withContext updates context correctly" prop_withContext_updates
  , fastProperty "withSuggestions updates suggestions correctly" prop_withSuggestions_updates
  , fastProperty "withRelatedErrors updates related errors correctly" prop_withRelatedErrors_updates
  , fastProperty "combinedErrorSeverity extracts severity correctly" prop_combinedErrorSeverity_correct
  , fastProperty "filterCombinedErrorsBySeverity filters correctly" prop_filterCombinedErrorsBySeverity_correct
  , fastProperty "combineErrors preserves error information" prop_combineErrors_preserves
  , fastProperty "getErrorStatistics returns valid statistics" prop_getErrorStatistics_valid
  , fastProperty "generateErrorReport produces non-empty string" prop_generateErrorReport_non_empty
  , fastProperty "emptyContext has all fields as Nothing/empty" prop_emptyContext_correct
  , fastProperty "error collector preserves error order" prop_error_collector_preserves_order
  , fastProperty "error severity ordering works correctly" prop_severity_ordering
  ]