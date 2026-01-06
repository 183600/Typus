module Test.Unit.ErrorHandlerAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, (===), forAll, Gen, choose, arbitrary, listOf, elements, oneof, suchThat)
import TestSupport.QuickCheck (fastProperty)

import Compiler.Errors.Core (
    TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), 
    ErrorContext(..), ErrorRecovery(..), CombinedError(..),
    emptyContext, fatalRecovery, errorRecovery, warningRecovery, infoRecovery,
    formatError, formatErrors, getErrors, getWarnings, getInfo, getAllMessages,
    hasErrors, hasWarnings, addError, addWarning, addInfo,
    errorAt, warningAt, infoAt, errorWithCategory, warningWithCategory,
    filterBySeverity, filterByCategory, hasCategory, canRecoverFrom, 
    shouldContinueAfter, combineErrors, combinedErrorSeverity,
    severityPriority, isAtLeast, _unknownLocation, _atLocation
  )
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Generators
-- ============================================================================

-- Generate error severities
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- Generate error categories
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

-- Generate error locations
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 1000)
  column <- choose (1, 200)
  endLine <- oneof [return Nothing, Just <$> choose (line, line + 10)]
  endColumn <- oneof [return Nothing, Just <$> choose (column, column + 50)]
  filePath <- oneof [return Nothing, Just <$> arbitrary]
  return $ ErrorLocation filePath line column endLine endColumn

-- Generate error context
genErrorContext :: Gen ErrorContext
genErrorContext = do
  code <- oneof [return Nothing, Just <$> arbitrary]
  function <- oneof [return Nothing, Just <$> arbitrary]
  variable <- oneof [return Nothing, Just <$> arbitrary]
  type' <- oneof [return Nothing, Just <$> arbitrary]
  additional <- listOf $ arbitrary `suchThat` (\(k, v) -> L.length k <= 20 && L.length v <= 50)
  return $ ErrorContext code function variable type' additional

-- Generate error recovery strategies
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = do
  canRecover <- arbitrary
  shouldContinue <- arbitrary
  action <- oneof [return Nothing, Just <$> arbitrary]
  hint <- oneof [return Nothing, Just <$> arbitrary]
  cost <- choose (0, 100)
  confidence <- choose (0.0, 1.0)
  return $ RecoveryStrategy canRecover shouldContinue action hint cost confidence

-- Generate text messages
genText :: Gen T.Text
genText = T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " ")

-- Generate type errors
genTypeError :: Gen TypeError
genTypeError = do
  errorId <- arbitrary `suchThat` (\s -> L.length s <= 20 && not (null s))
  severity <- genErrorSeverity
  category <- genErrorCategory
  message <- genText
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  suggestions <- listOf genText
  relatedErrors <- listOf genTypeError
  errorChain <- listOf genTypeError
  timestamp <- oneof [return Nothing, Just <$> arbitrary]
  return $ TypeError errId errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

-- Generate combined errors
genCombinedError :: Gen CombinedError
genCombinedError = oneof
  [ OwnershipErrorCombined <$> genErrorSeverity <*> arbitrary
  , DependentTypeErrorCombined <$> genErrorSeverity <*> arbitrary
  , IntegrationError <$> arbitrary <*> genErrorSeverity
  , CrossAnalyzerError <$> arbitrary <*> genErrorSeverity <*> listOf genCombinedError
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Severity priority ordering is consistent
prop_severityPriorityOrdering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severityPriorityOrdering sev1 sev2 =
  let p1 = severityPriority sev1
      p2 = severityPriority sev2
      ordering = compare sev1 sev2
      priorityOrdering = compare p1 p2
  in ordering == priorityOrdering

-- Property: isAtLeast is reflexive
prop_isAtLeastReflexive :: ErrorSeverity -> Bool
prop_isAtLeastReflexive sev = isAtLeast sev sev

-- Property: isAtLeast is transitive
prop_isAtLeastTransitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeastTransitive sev1 sev2 sev3 =
  isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

-- Property: Fatal is the highest severity
prop_fatalIsHighest :: ErrorSeverity -> Bool
prop_fatalIsHighest sev = isAtLeast sev Fatal || sev == Fatal

-- Property: Info is the lowest severity
prop_infoIsLowest :: ErrorSeverity -> Bool
prop_infoIsLowest sev = isAtLeast Info sev || sev == Info

-- Property: Filtering by severity preserves ordering
prop_filterBySeverityOrdering :: [TypeError] -> ErrorSeverity -> Bool
prop_filterBySeverityOrdering errors minSeverity =
  let filtered = filterBySeverity minSeverity errors
      filteredSeverities = map severity filtered
  in L.all (`isAtLeast` minSeverity) filteredSeverities

-- Property: Filtering by category only returns specified category
prop_filterByCategoryCorrectness :: [TypeError] -> ErrorCategory -> Bool
prop_filterByCategoryCorrectness errors cat =
  let filtered = filterByCategory cat errors
  in L.all (\e -> category e == cat) filtered

-- Property: hasCategory is consistent with filtering
prop_hasCategoryConsistency :: [TypeError] -> ErrorCategory -> Bool
prop_hasCategoryConsistency errors cat =
  hasCategory cat errors == not (L.null (filterByCategory cat errors))

-- Property: Error recovery strategies follow logical rules
prop_recoveryStrategyLogic :: ErrorRecovery -> Bool
prop_recoveryStrategyLogic recovery =
  let canRec = canRecover recovery
      shouldCont = shouldContinueAfter recovery
      cost = recoveryCost recovery
      confidence = recoveryConfidence recovery
  in cost >= 0 && cost <= 100 && confidence >= 0.0 && confidence <= 1.0 &&
     (not canRec ==> not shouldCont)

-- Property: Fatal errors have non-recoverable recovery
prop_fatalErrorsNonRecoverable :: TypeError -> Property
prop_fatalErrorsNonRecoverable err =
  severity err == Fatal ==> not (canRecoverFrom err)

-- Property: Combining errors preserves L.all errors
prop_combineErrorsPreservesAll :: [TypeError] -> Bool
prop_combineErrorsPreservesAll errors =
  let combined = combineErrors errors
      allOriginal = getAllMessages combined
  in L.length allOriginal >= L.length errors

-- Property: Combined error severity is L.maximum of components
prop_combinedErrorSeverityMax :: CombinedError -> Property
prop_combinedErrorSeverityMax combinedErr =
  case combinedErr of
    CrossAnalyzerError _ sev subErrors -> 
      let subSeverities = map combinedErrorSeverity subErrors
      in not (null subSeverities) ==> sev == L.maximum subSeverities
    _ -> property True

-- Property: Error formatting produces non-empty strings
prop_errorFormattingNonEmpty :: TypeError -> Bool
prop_errorFormattingNonEmpty err = not (L.null (formatError err))

-- Property: Error formatting includes severity information
prop_errorFormattingIncludesSeverity :: TypeError -> Bool
prop_errorFormattingIncludesSeverity err =
  let formatted = formatError err
      severityStr = case severity err of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
  in severityStr `L.isInfixOf` formatted

-- Property: getErrors only returns Error L.or Fatal severities
prop_getErrorsCorrectness :: [TypeError] -> Bool
prop_getErrorsCorrectness errors =
  let errorList = getErrors errors
  in L.all (\e -> severity e == Error || severity e == Fatal) errorList

-- Property: getWarnings only returns Warning severities
prop_getWarningsCorrectness :: [TypeError] -> Bool
prop_getWarningsCorrectness errors =
  let warningList = getWarnings errors
  in L.all (\e -> severity e == Warning) warningList

-- Property: getInfo only returns Info severities
prop_getInfoCorrectness :: [TypeError] -> Bool
prop_getInfoCorrectness errors =
  let infoList = getInfo errors
  in L.all (\e -> severity e == Info) infoList

-- Property: hasErrors is consistent with getResults
prop_hasErrorsConsistency :: [TypeError] -> Bool
prop_hasErrorsConsistency errors = hasErrors errors == not (L.null (getErrors errors))

-- Property: hasWarnings is consistent with getWarnings
prop_hasWarningsConsistency :: [TypeError] -> Bool
prop_hasWarningsConsistency errors = hasWarnings errors == not (L.null (getWarnings errors))

-- Property: Error location fields are valid
prop_errorLocationValidity :: ErrorLocation -> Bool
prop_errorLocationValidity loc =
  line loc >= 0 && column loc >= 0 &&
  L.all (\l -> l >= line loc) (endLine loc) &&
  L.all (\c -> c >= column loc) (endColumn loc)

-- Property: Error context preserves additional information
prop_errorContextPreservesAdditional :: [(String, String)] -> Bool
prop_errorContextPreservesAdditional additional =
  let context = emptyContext { contextAdditional = additional }
  in contextAdditional context == additional

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler Advanced QuickCheck Tests"
  [ testGroup "Severity Properties"
    [ testProperty "Severity priority ordering is consistent" prop_severityPriorityOrdering
    , testProperty "isAtLeast is reflexive" prop_isAtLeastReflexive
    , testProperty "isAtLeast is transitive" prop_isAtLeastTransitive
    , testProperty "Fatal is the highest severity" prop_fatalIsHighest
    , testProperty "Info is the lowest severity" prop_infoIsLowest
    ]

  , testGroup "Filtering Properties"
    [ testProperty "Filtering by severity preserves ordering" prop_filterBySeverityOrdering
    , testProperty "Filtering by category only returns specified category" prop_filterByCategoryCorrectness
    , testProperty "hasCategory is consistent with filtering" prop_hasCategoryConsistency
    ]

  , testGroup "Recovery Properties"
    [ testProperty "Error recovery strategies follow logical rules" prop_recoveryStrategyLogic
    , testProperty "Fatal errors have non-recoverable recovery" prop_fatalErrorsNonRecoverable
    ]

  , testGroup "Error Combination Properties"
    [ testProperty "Combining errors preserves L.all errors" prop_combineErrorsPreservesAll
    , testProperty "Combined error severity is L.maximum of components" prop_combinedErrorSeverityMax
    ]

  , testGroup "Formatting Properties"
    [ testProperty "Error formatting produces non-empty strings" prop_errorFormattingNonEmpty
    , testProperty "Error formatting includes severity information" prop_errorFormattingIncludesSeverity
    ]

  , testGroup "Error Retrieval Properties"
    [ testProperty "getErrors only returns Error L.or Fatal severities" prop_getErrorsCorrectness
    , testProperty "getWarnings only returns Warning severities" prop_getWarningsCorrectness
    , testProperty "getInfo only returns Info severities" prop_getInfoCorrectness
    , testProperty "hasErrors is consistent with getResults" prop_hasErrorsConsistency
    , testProperty "hasWarnings is consistent with getWarnings" prop_hasWarningsConsistency
    ]

  , testGroup "Location L.and Context Properties"
    [ testProperty "Error location fields are valid" prop_errorLocationValidity
    , testProperty "Error context preserves additional information" prop_errorContextPreservesAdditional
    ]

  , testGroup "Unit Tests"
    [ testCase "Create basic error" $ do
        let err = errorAt "test-id" 10 5) "Test error"
        severity err @?= Error
        message err @?= "Test error"
        location err @?= _atLocation 10 5

    , testCase "Create warning" $ do
        let warn = warningAt "test-id" 1 1) "Test warning"
        severity warn @?= Warning
        message warn @?= "Test warning"

    , testCase "Create info message" $ do
        let info = infoAt "test-id" 100 50) "Test info"
        severity info @?= Info
        message info @?= "Test info"

    , testCase "Error with category" $ do
        let err = errorWithCategory TypeChecking "Type error"
        category err @?= TypeChecking
        severity err @?= Error

    , testCase "Warning with category" $ do
        let warn = warningWithCategory Ownership "Ownership warning"
        category warn @?= Ownership
        severity warn @?= Warning

    , testCase "Filter errors by severity" $ do
        let fatal = errorAt "test-id" 1 1) "Fatal" { severity = Fatal }
            error = errorAt "test-id" 2 2) "Error"
            warning = warningAt "test-id" 3 3) "Warning"
            info = infoAt "test-id" 4 4) "Info"
            errors = [fatal, error, warning, info]
            filtered = filterBySeverity Error errors
        L.length filtered @?= 2
        Fatal `elem` map severity filtered @?= True
        Error `elem` map severity filtered @?= True

    , testCase "Filter errors by category" $ do
        let typeErr = errorWithCategory TypeChecking "Type error"
            ownErr = errorWithCategory Ownership "Ownership error"
            parseErr = errorWithCategory Parsing "Parse error"
            errors = [typeErr, ownErr, parseErr]
            filtered = filterByCategory Ownership errors
        L.length filtered @?= 1
        L.head filtered @?= ownErr

    , testCase "Check for specific category" $ do
        let typeErr = errorWithCategory TypeChecking "Type error"
            ownErr = errorWithCategory Ownership "Ownership error"
            errors = [typeErr, ownErr]
        hasCategory TypeChecking errors @?= True
        hasCategory Runtime errors @?= False

    , testCase "Error recovery strategies" $ do
        canRecoverFrom fatalRecovery @?= False
        shouldContinueAfter fatalRecovery @?= False
        canRecoverFrom errorRecovery @?= True
        shouldContinueAfter errorRecovery @?= True
        canRecoverFrom warningRecovery @?= True
        shouldContinueAfter warningRecovery @?= True
        canRecoverFrom infoRecovery @?= True
        shouldContinueAfter infoRecovery @?= True

    , testCase "Error formatting" $ do
        let err = errorAt "test-id" 10 5) "Test error"
            formatted = formatError err
        "[ERROR]" `L.isInfixOf` formatted @?= True
        "Test error" `L.isInfixOf` formatted @?= True
        "[TypeChecking]" `L.isInfixOf` formatted @?= True

    , testCase "Error collection" $ do
        let errors = [errorAt "test-id" 1 1) "Error 1", warningAt "test-id" 2 2) "Warning 1", infoAt "test-id" 3 3) "Info 1"]
        getErrors errors @?= [errorAt "test-id" 1 1) "Error 1"]
        getWarnings errors @?= [warningAt "test-id" 2 2) "Warning 1"]
        getInfo errors @?= [infoAt "test-id" 3 3) "Info 1"]
        hasErrors errors @?= True
        hasWarnings errors @?= True
    ]
  ]

-- Helper function to check if a string is contained in another
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack