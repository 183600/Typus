{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.ErrorHandlerConsistencyTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, choose, listOf, elements, oneof, sized, suchThat)

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , errorAt
  , errorAtWithTimestamp
  , errorWithCategory
  , warningAt
  , infoAt
  , fatalError
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
  , generateErrorReport
  , formatError
  , formatErrorWithLocation
  , formatErrors
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , severityPriority
  , isAtLeast
  , createRecoveryStrategy
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , getErrorLine
  , getErrorColumn
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate error severity levels
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- Generate error categories
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown ]

-- Generate error location
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 1000)
  column <- choose (1, 100)
  endLine <- choose (Just line, Just (line + 10))
  endColumn <- choose (Just column, Just (column + 50))
  filePath <- oneof [pure Nothing, Just <$> elements ["test.go", "main.go", "lib.go"]]
  return $ ErrorLocation filePath line column endLine endColumn

-- Generate error context
genErrorContext :: Gen ErrorContext
genErrorContext = do
  code <- oneof [pure Nothing, Just <$> elements ["x := 42", "func test() {}", "if condition {}"]]
  function <- oneof [pure Nothing, Just <$> elements ["main", "test", "helper"]]
  variable <- oneof [pure Nothing, Just <$> elements ["x", "y", "result"]]
  typeInfo <- oneof [pure Nothing, Just <$> elements ["int", "string", "bool"]]
  additional <- listOf $ elements [("hint", "check syntax"), ("suggestion", "add semicolon")]
  return $ ErrorContext code function variable typeInfo additional

-- Generate error recovery strategy
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = do
  canRec <- arbitrary
  shouldCont <- arbitrary
  action <- oneof [pure Nothing, Just <$> elements ["retry", "skip", "fallback"]]
  hint <- oneof [pure Nothing, Just <$> elements ["check input", "verify syntax"]]
  cost <- choose (0, 100)
  confidence <- choose (0.0, 1.0)
  return $ RecoveryStrategy canRec shouldCont action hint cost confidence

-- Generate basic error
genTypeError :: Gen TypeError
genTypeError = do
  errorId <- elements ["ERR001", "ERR002", "ERR003", "PARSE001", "TYPE001"]
  severity <- genErrorSeverity
  category <- genErrorCategory
  message <- T.pack <$> elements ["Type error", "Parse error", "Runtime error", "Unknown error"]
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  suggestions <- listOf $ T.pack <$> elements ["Add type annotation", "Check syntax", "Import missing module"]
  relatedErrors <- listOf genTypeError
  errorChain <- listOf genTypeError
  timestamp <- oneof [pure Nothing, Just <$> elements ["2023-01-01 12:00:00", "2023-12-31 23:59:59"]]
  return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

-- Generate combined error
genCombinedError :: Gen CombinedError
genCombinedError = oneof
  [ OwnershipErrorCombined <$> genErrorSeverity <*> arbitrary
  , DependentTypeErrorCombined <$> genErrorSeverity <*> arbitrary
  , IntegrationError <$> elements ["integration failed"] <*> genErrorSeverity
  , CrossAnalyzerError <$> elements ["cross analysis failed"] <*> genErrorSeverity <*> listOf genCombinedError
  ]

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test error severity ordering
testErrorSeverityOrdering :: TestTree
testErrorSeverityOrdering = testGroup "Error Severity Ordering"
  [ testCase "severity priority ordering" $ do
      severityPriority Fatal @?= 100
      severityPriority Error @?= 80
      severityPriority Warning @?= 30
      severityPriority Info @?= 10
      
  , testCase "isAtLeast comparison works" $ do
      assertBool "Fatal >= Fatal" $ isAtLeast Fatal Fatal
      assertBool "Fatal >= Error" $ isAtLeast Fatal Error
      assertBool "Fatal >= Warning" $ isAtLeast Fatal Warning
      assertBool "Fatal >= Info" $ isAtLeast Fatal Info
      assertBool "Error >= Error" $ isAtLeast Error Error
      assertBool "Error >= Warning" $ isAtLeast Error Warning
      assertBool "Error >= Info" $ isAtLeast Error Info
      assertBool "Warning >= Warning" $ isAtLeast Warning Warning
      assertBool "Warning >= Info" $ isAtLeast Warning Info
      assertBool "Info >= Info" $ isAtLeast Info Info
      assertBool "not (Info >= Warning)" $ not $ isAtLeast Info Warning
  ]

-- Test error creation utilities
testErrorCreation :: TestTree
testErrorCreation = testGroup "Error Creation"
  [ testCase "errorAt creates basic error" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          error = errorAt "ERR001" "test message" location
      errorId error @?= "ERR001"
      message error @?= "test message"
      severity error @?= Error
      category error @?= Unknown
      location error @?= location
      
  , testCase "errorWithCategory sets category" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          error = errorWithCategory "ERR001" TypeChecking "type error" location
      category error @?= TypeChecking
      
  , testCase "warningAt creates warning" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          warning = warningAt "WARN001" "warning message" location
      severity warning @?= Warning
      
  , testCase "infoAt creates info" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          info = infoAt "INFO001" "info message" location
      severity info @?= Info
      
  , testCase "fatalError creates fatal error" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          fatal = fatalError "FATAL001" "fatal error" location
      severity fatal @?= Fatal
      canRecoverFrom fatal @?= False
  ]

-- Test error modification utilities
testErrorModification :: TestTree
testErrorModification = testGroup "Error Modification"
  [ testCase "withLocation updates location" $ do
      let originalLoc = ErrorLocation Nothing 1 1 Nothing Nothing
          newLoc = ErrorLocation (Just "test.go") 5 10 Nothing Nothing
          error = errorAt "ERR001" "test" originalLoc
          updated = withLocation error newLoc
      location updated @?= newLoc
      
  , testCase "withContext updates context" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          context = ErrorContext (Just "code") (Just "func") (Just "var") (Just "type") []
          error = errorAt "ERR001" "test" location
          updated = withContext error context
      context updated @?= context
      
  , testCase "withSuggestions adds suggestions" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          error = errorAt "ERR001" "test" location
          suggestions = [T.pack "suggestion1", T.pack "suggestion2"]
          updated = withSuggestions suggestions error
      suggestions updated @?= suggestions
      
  , testCase "wrapError adds to message and chain" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          inner = errorAt "ERR001" "inner" location
          wrapped = wrapError "wrapper: " inner
      message wrapped @?= "wrapper: inner"
      errorChain wrapped @?= [inner]
  ]

-- Test error filtering and analysis
testErrorFiltering :: TestTree
testErrorFiltering = testGroup "Error Filtering"
  [ testCase "hasCategory checks category" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          typeError = errorWithCategory "ERR001" TypeChecking "type error" location
          parseError = errorWithCategory "ERR002" Parsing "parse error" location
      assertBool "typeError has TypeChecking category" $ hasCategory TypeChecking typeError
      assertBool "typeError doesn't have Parsing category" $ not $ hasCategory Parsing typeError
      assertBool "parseError has Parsing category" $ hasCategory Parsing parseError
      
  , testCase "filterByCategory filters correctly" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          errors = 
            [ errorWithCategory "ERR001" TypeChecking "type error" location
            , errorWithCategory "ERR002" Parsing "parse error" location
            , errorWithCategory "ERR003" TypeChecking "another type error" location
            , errorWithCategory "ERR004" Runtime "runtime error" location
            ]
          typeErrors = filterByCategory TypeChecking errors
      length typeErrors @?= 2
      all (hasCategory TypeChecking) typeErrors @?= True
      
  , testCase "filterBySeverity filters correctly" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          errors = 
            [ errorAt "ERR001" "error" location {severity = Error}
            , warningAt "WARN001" "warning" location
            , infoAt "INFO001" "info" location
            , fatalError "FATAL001" "fatal" location
            ]
          errorAndFatal = filterBySeverity Error errors
      length errorAndFatal @?= 2
      all (\e -> severity e == Error || severity e == Fatal) errorAndFatal @?= True
  ]

-- Test error statistics
testErrorStatistics :: TestTree
testErrorStatistics = testGroup "Error Statistics"
  [ testCase "getErrorStatistics counts errors correctly" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          errors = 
            [ errorAt "ERR001" "error1" location
            , warningAt "WARN001" "warning1" location
            , infoAt "INFO001" "info1" location
            , errorWithCategory "ERR002" TypeChecking "type error" location
            , errorWithCategory "ERR003" Parsing "parse error" location
            , fatalError "FATAL001" "fatal" location
            ]
          stats = getErrorStatistics errors
      Map.lookup "total" stats @?= Just 6
      Map.lookup "fatal" stats @?= Just 1
      Map.lookup "errors" stats @?= Just 2
      Map.lookup "warnings" stats @?= Just 1
      Map.lookup "info" stats @?= Just 1
      Map.lookup "typeChecking" stats @?= Just 1
      Map.lookup "parsing" stats @?= Just 1
  ]

-- Test error formatting
testErrorFormatting :: TestTree
testErrorFormatting = testGroup "Error Formatting"
  [ testCase "formatError includes severity and message" $ do
      let location = ErrorLocation Nothing 1 1 Nothing Nothing
          error = errorAt "ERR001" "test message" location
          formatted = formatError error
      assertBool "Format includes ERROR" $ "ERROR" `isInfixOf` formatted
      assertBool "Format includes message" $ "test message" `isInfixOf formatted
      
  , testCase "formatErrorWithLocation includes location" $ do
      let location = ErrorLocation (Just "test.go") 5 10 Nothing Nothing
          error = errorAt "ERR001" "test message" location
          formatted = formatErrorWithLocation error
      assertBool "Format includes file" $ "test.go" `isInfixOf` formatted
      assertBool "Format includes line" $ "5" `isInfixOf` formatted
      assertBool "Format includes column" $ "10" `isInfixOf` formatted
  ]

-- Test error recovery
testErrorRecovery :: TestTree
testErrorRecovery = testGroup "Error Recovery"
  [ testCase "recovery strategies have correct properties" $ do
      assertBool "fatal recovery cannot recover" $ not $ canRecover fatalRecovery
      assertBool "fatal recovery should not continue" $ not $ shouldContinueAfter fatalRecovery
      assertBool "error recovery can recover" $ canRecover errorRecovery
      assertBool "error recovery should continue" $ shouldContinueAfter errorRecovery
      assertBool "warning recovery can recover" $ canRecover warningRecovery
      assertBool "warning recovery should continue" $ shouldContinueAfter warningRecovery
      assertBool "info recovery can recover" $ canRecover infoRecovery
      assertBool "info recovery should continue" $ shouldContinueAfter infoRecovery
      
  , testCase "createRecoveryStrategy creates strategy" $ do
      let strategy = createRecoveryStrategy True True (Just "retry") (Just "check input")
      canRecover strategy @?= True
      shouldContinue strategy @?= True
      recoveryAction strategy @?= Just "retry"
      recoveryHint strategy @?= Just "check input"
  ]

-- Test combined errors
testCombinedErrors :: TestTree
testCombinedErrors = testGroup "Combined Errors"
  [ testCase "combinedErrorSeverity extracts severity" $ do
      let combined = OwnershipErrorCombined Error arbitrary
      combinedErrorSeverity combined @?= Error
      let combined2 = IntegrationError "test" Warning
      combinedErrorSeverity combined2 @?= Warning
      
  , testCase "filterCombinedErrorsBySeverity filters" $ do
      let errors = 
            [ OwnershipErrorCombined Error arbitrary
            , OwnershipErrorCombined Warning arbitrary
            , IntegrationError "test" Fatal
            , DependentTypeErrorCombined Info arbitrary
            ]
          filtered = filterCombinedErrorsBySeverity Error errors
      length filtered @?= 2
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Error severity ordering is total
prop_severity_total_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_total_ordering sev1 sev2 =
  let comparison = compare sev1 sev2
  in property $ (comparison == EQ || comparison == LT || comparison == GT)

-- Property: isAtLeast is reflexive
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive sev =
  property $ isAtLeast sev sev

-- Property: isAtLeast is transitive
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive sev1 sev2 sev3 =
  (isAtLeast sev1 sev2 && isAtLeast sev2 sev3) ==> isAtLeast sev1 sev3

-- Property: filterByCategory preserves category
prop_filterByCategory_preserves_category :: [TypeError] -> ErrorCategory -> Property
prop_filterByCategory_preserves_category errors category =
  let filtered = filterByCategory category errors
  in property $ all (hasCategory category) filtered

-- Property: filterBySeverity preserves severity
prop_filterBySeverity_preserves_severity :: [TypeError] -> ErrorSeverity -> Property
prop_filterBySeverity_preserves_severity errors severity =
  let filtered = filterBySeverity severity errors
  in property $ all (\e -> severity e == severity) filtered

-- Property: error statistics sum to total
prop_error_statistics_sum_to_total :: [TypeError] -> Property
prop_error_statistics_sum_to_total errors =
  let stats = getErrorStatistics errors
      total = Map.findWithDefault 0 "total" stats
      counted = sum $ Map.findWithDefault 0 "fatal" stats :
                       Map.findWithDefault 0 "errors" stats :
                       Map.findWithDefault 0 "warnings" stats :
                       Map.findWithDefault 0 "info" stats :
                       []
  in property $ total === length errors .&&. counted === total

-- Property: withLocation preserves other fields
prop_withLocation_preserves_fields :: TypeError -> ErrorLocation -> Property
prop_withLocation_preserves_fields error newLocation =
  let updated = withLocation error newLocation
  in property $ errorId updated === errorId error .&&.
                severity updated === severity error .&&.
                category updated === category error .&&.
                message updated === message error

-- Property: withContext preserves other fields
prop_withContext_preserves_fields :: TypeError -> ErrorContext -> Property
prop_withContext_preserves_fields error newContext =
  let updated = withContext error newContext
  in property $ errorId updated === errorId error .&&.
                severity updated === severity error .&&.
                category updated === category error .&&.
                message updated === message error .&&.
                location updated === location error

-- Property: wrapError preserves original error in chain
prop_wrapError_preserves_chain :: TypeError -> Text -> Property
prop_wrapError_preserves_chain error wrapperMsg =
  let wrapped = wrapError wrapperMsg error
  in property $ error `elem` errorChain wrapped

-- Property: formatError is deterministic
prop_formatError_deterministic :: TypeError -> Property
prop_formatError_deterministic error =
  let formatted1 = formatError error
      formatted2 = formatError error
  in property $ formatted1 === formatted2

-- Property: formatErrorWithLocation is deterministic
prop_formatErrorWithLocation_deterministic :: TypeError -> Property
prop_formatErrorWithLocation_deterministic error =
  let formatted1 = formatErrorWithLocation error
      formatted2 = formatErrorWithLocation error
  in property $ formatted1 === formatted2

-- Property: fatal errors cannot be recovered from
prop_fatal_no_recovery :: TypeError -> Property
prop_fatal_no_recovery error =
  severity error == Fatal ==> not (canRecoverFrom error)

-- Property: info errors can always be recovered from
prop_info_always_recovery :: TypeError -> Property
prop_info_always_recovery error =
  severity error == Info ==> canRecoverFrom error

-- Property: combinedErrorSeverity matches embedded severity
prop_combined_error_severity_matches :: CombinedError -> Property
prop_combined_error_severity_matches combined =
  case combined of
    OwnershipErrorCombined sev _ -> property $ combinedErrorSeverity combined === sev
    DependentTypeErrorCombined sev _ -> property $ combinedErrorSeverity combined === sev
    IntegrationError _ sev -> property $ combinedErrorSeverity combined === sev
    CrossAnalyzerError _ sev _ -> property $ combinedErrorSeverity combined === sev

-- Property: error creation sets correct defaults
prop_error_creation_defaults :: String -> Text -> ErrorLocation -> Property
prop_error_creation_defaults errId msg loc =
  let error = errorAt errId msg loc
  in property $ errorId error === errId .&&.
                message error === msg .&&.
                location error === loc .&&.
                severity error === Error .&&.
                category error === Unknown

-- Property: getErrorLine and getErrorColumn access location correctly
prop_error_location_accessors :: ErrorLocation -> Property
prop_error_location_accessors loc =
  let error = errorAt "TEST" "test" loc
  in property $ getErrorLine (location error) === line loc .&&.
                getErrorColumn (location error) === column loc

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handler Consistency Tests"
  [ testErrorSeverityOrdering
  , testErrorCreation
  , testErrorModification
  , testErrorFiltering
  , testErrorStatistics
  , testErrorFormatting
  , testErrorRecovery
  , testCombinedErrors
  , testGroup "QuickCheck Properties"
    [ fastProperty "Severity total ordering" prop_severity_total_ordering
    , fastProperty "isAtLeast reflexive" prop_isAtLeast_reflexive
    , fastProperty "isAtLeast transitive" prop_isAtLeast_transitive
    , fastProperty "filterByCategory preserves" prop_filterByCategory_preserves_category
    , fastProperty "filterBySeverity preserves" prop_filterBySeverity_preserves_severity
    , fastProperty "statistics sum to total" prop_error_statistics_sum_to_total
    , fastProperty "withLocation preserves fields" prop_withLocation_preserves_fields
    , fastProperty "withContext preserves fields" prop_withContext_preserves_fields
    , fastProperty "wrapError preserves chain" prop_wrapError_preserves_chain
    , fastProperty "formatError deterministic" prop_formatError_deterministic
    , fastProperty "formatErrorWithLocation deterministic" prop_formatErrorWithLocation_deterministic
    , fastProperty "fatal no recovery" prop_fatal_no_recovery
    , fastProperty "info always recovery" prop_info_always_recovery
    , fastProperty "combined error severity matches" prop_combined_error_severity_matches
    , fastProperty "error creation defaults" prop_error_creation_defaults
    , fastProperty "error location accessors" prop_error_location_accessors
    ]
  ]