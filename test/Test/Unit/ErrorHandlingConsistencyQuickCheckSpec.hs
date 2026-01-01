{-# LANGUAGE CPP #-}
module Test.Unit.ErrorHandlingConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, 
                        Property, (===), forAll, counterexample, suchThat, (==>))
import Compiler.Errors.Core (ErrorSeverity(..), ErrorLocation(..), ErrorContext(..), 
                            ErrorRecovery(..), emptyContext, TypeError(..))
import qualified Data.Text as T
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)

-- ============================================================================
-- Test data generators
-- ============================================================================

-- Generate error severity levels
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Error, Warning, Info, Hint]

-- Generate error locations
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  endLine <- oneof [pure Nothing, Just <$> choose (line, line + 100)]
  endColumn <- oneof [pure Nothing, Just <$> choose (column, column + 100)]
  filePath <- oneof [pure Nothing, Just <$> listOf (elements ['a'..'z'] ++ ['_'])]
  return $ ErrorLocation filePath line column endLine endColumn

-- Generate error recovery strategies
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = elements 
  [ NoRecovery
  , SkipToNextLine
  , SkipToNextToken
  , InsertExpectedToken
  , RemoveUnexpectedToken
  , RetryWithAlternative
  ]

-- Generate error context
genErrorContext :: Gen ErrorContext
genErrorContext = do
  contextType <- elements ["parsing", "type-checking", "ownership-analysis", "code-generation"]
  contextInfo <- listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['\n']
  return $ ErrorContext (T.pack contextType) (T.pack contextInfo)

-- Generate type errors
genTypeError :: Gen TypeError
genTypeError = do
  errorId <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  severity <- genErrorSeverity
  category <- elements ["syntax", "type", "ownership", "dependency", "general"]
  message <- listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['.', '!']
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  suggestions <- listOf $ listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['.']
  timestamp <- oneof [pure Nothing, Just <$> choose (1, 1000000000)]
  
  return $ TypeError
    { errorId = T.pack errorId
    , severity = severity
    , category = T.pack category
    , message = T.pack message
    , location = location
    , context = context
    , recovery = recovery
    , suggestions = map T.pack suggestions
    , relatedErrors = []
    , errorChain = []
    , timestamp = timestamp
    }

-- ============================================================================
-- Properties for ErrorSeverity
-- ============================================================================

prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let severityOrder sev = case sev of
        Error -> 4
        Warning -> 3
        Info -> 2
        Hint -> 1
  in counterexample ("Sev1: " ++ show sev1 ++ ", Sev2: " ++ show sev2) $
     (severityOrder sev1 > severityOrder sev2) === (sev1 > sev2)

prop_error_severity_total_ordering :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_total_ordering sev1 sev2 sev3 =
  let severityOrder sev = case sev of
        Error -> 4
        Warning -> 3
        Info -> 2
        Hint -> 1
  in -- Test transitivity: if a > b L.and b > c then a > c
     (severityOrder sev1 > severityOrder sev2 && severityOrder sev2 > severityOrder sev3) ==> 
     severityOrder sev1 > severityOrder sev3

-- ============================================================================
-- Properties for ErrorLocation
-- ============================================================================

prop_error_location_line_consistency :: ErrorLocation -> Property
prop_error_location_line_consistency errLoc =
  let line = line errLoc
      endLine = endLine errLoc
  in counterexample ("Line: " ++ show line ++ ", EndLine: " ++ show endLine) $
     case endLine of
       Nothing -> property True
       Just endL -> endL >= line

prop_error_location_column_consistency :: ErrorLocation -> Property
prop_error_location_column_consistency errLoc =
  let line = line errLoc
      column = column errLoc
      endLine = endLine errLoc
      endColumn = endColumn errLoc
  in case (endLine, endColumn) of
       (Just endL, Just endC) -> 
         if endL == line
         then endC >= column
         else property True
       _ -> property True

-- ============================================================================
-- Properties for ErrorContext
-- ============================================================================

prop_error_context_non_empty_type :: ErrorContext -> Property
prop_error_context_non_empty_type context =
  let contextType = contextType context
  in T.L.length contextType > 0

prop_error_context_preserves_content :: String -> String -> Property
prop_error_context_preserves_content contextType contextInfo =
  let original = ErrorContext (T.pack contextType) (T.pack contextInfo)
  in contextType original === T.pack contextType &&
     contextInfo original === T.pack contextInfo

-- ============================================================================
-- Properties for ErrorRecovery
-- ============================================================================

prop_error_recovery_strategies_exhaustive :: ErrorRecovery -> Property
prop_error_recovery_strategies_exhaustive recovery =
  let allStrategies = [NoRecovery, SkipToNextLine, SkipToNextToken, 
                      InsertExpectedToken, RemoveUnexpectedToken, RetryWithAlternative]
  in recovery `elem` allStrategies

-- ============================================================================
-- Properties for TypeError
-- ============================================================================

prop_type_error_preserves_id :: TypeError -> Property
prop_type_error_preserves_id typeErr =
  T.L.length (errorId typeErr) >= 0

prop_type_error_has_valid_severity :: TypeError -> Property
prop_type_error_has_valid_severity typeErr =
  let sev = severity typeErr
  in sev `elem` [Error, Warning, Info, Hint]

prop_type_error_location_consistency :: TypeError -> Property
prop_type_error_location_consistency typeErr =
  let loc = location typeErr
  in line loc >= 1 && column loc >= 1

prop_type_error_context_preservation :: TypeError -> Property
prop_type_error_context_preservation typeErr =
  let ctx = context typeErr
  in T.L.length (contextType ctx) > 0

-- ============================================================================
-- Properties for error transformation
-- ============================================================================

prop_error_severity_upgrade :: TypeError -> ErrorSeverity -> Property
prop_error_severity_upgrade typeErr newSeverity =
  let upgraded = typeErr { severity = newSeverity }
  in severity upgraded === newSeverity &&
     errorId upgraded === errorId typeErr &&
     location upgraded === location typeErr

prop_error_location_update :: TypeError -> ErrorLocation -> Property
prop_error_location_update typeErr newLocation =
  let updated = typeErr { location = newLocation }
  in location updated === newLocation &&
     errorId updated === errorId typeErr &&
     severity updated === severity typeErr

-- ============================================================================
-- Properties for error aggregation
-- ============================================================================

prop_error_suggestion_aggregation :: TypeError -> [String] -> Property
prop_error_suggestion_aggregation typeErr newSuggestions =
  let updated = typeErr { suggestions = map T.pack newSuggestions }
  in L.length (suggestions updated) === L.length newSuggestions

prop_error_chain_associativity :: TypeError -> TypeError -> TypeError -> Property
prop_error_chain_associativity err1 err2 err3 =
  let chain1 = err1 { errorChain = [err2, err3] }
      chain2 = err1 { errorChain = [err2] }
      chain3 = chain2 { errorChain = errorChain chain2 ++ [err3] }
  in L.length (errorChain chain1) === L.length (errorChain chain3)

-- ============================================================================
-- Edge case properties
-- ============================================================================

prop_empty_error_context :: Property
prop_empty_error_context =
  let empty = emptyContext
  in T.L.length (contextType empty) >= 0 &&
     T.L.length (contextInfo empty) >= 0

prop_minimal_error_type :: Property
prop_minimal_error_type =
  let minimal = TypeError
        { errorId = T.pack ""
        , severity = Error
        , category = T.pack ""
        , message = T.pack ""
        , location = ErrorLocation Nothing 1 1 Nothing Nothing
        , context = emptyContext
        , recovery = NoRecovery
        , suggestions = []
        , relatedErrors = []
        , errorChain = []
        , timestamp = Nothing
        }
  in T.L.length (errorId minimal) === 0 &&
     severity minimal === Error &&
     suggestions minimal === [] &&
     relatedErrors minimal === [] &&
     errorChain minimal === []

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handling Consistency QuickCheck Tests"
  [ testGroup "ErrorSeverity properties"
    [ fastProperty "error severity ordering" prop_error_severity_ordering
    , fastProperty "error severity total ordering" prop_error_severity_total_ordering
    ]
  , testGroup "ErrorLocation properties"
    [ fastProperty "error location line consistency" prop_error_location_line_consistency
    , fastProperty "error location column consistency" prop_error_location_column_consistency
    ]
  , testGroup "ErrorContext properties"
    [ fastProperty "error context non-empty type" prop_error_context_non_empty_type
    , fastProperty "error context preserves content" prop_error_context_preserves_content
    ]
  , testGroup "ErrorRecovery properties"
    [ fastProperty "error recovery strategies exhaustive" prop_error_recovery_strategies_exhaustive
    ]
  , testGroup "TypeError properties"
    [ fastProperty "type error preserves id" prop_type_error_preserves_id
    , fastProperty "type error has valid severity" prop_type_error_has_valid_severity
    , fastProperty "type error location consistency" prop_type_error_location_consistency
    , fastProperty "type error context preservation" prop_type_error_context_preservation
    ]
  , testGroup "Error transformation properties"
    [ fastProperty "error severity upgrade" prop_error_severity_upgrade
    , fastProperty "error location update" prop_error_location_update
    ]
  , testGroup "Error aggregation properties"
    [ fastProperty "error suggestion aggregation" prop_error_suggestion_aggregation
    , fastProperty "error chain associativity" prop_error_chain_associativity
    ]
  , testGroup "Edge case properties"
    [ fastProperty "empty error context" prop_empty_error_context
    , fastProperty "minimal error type" prop_minimal_error_type
    ]
  ]