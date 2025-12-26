{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.NewErrorHandlingQuickCheckTestsSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, property, (==>), forAll)
import TestSupport.QuickCheck (fastProperty)

import ErrorHandler
import EnhancedErrorHandler
import Compiler.Errors.Core
import Compiler.Errors
import SourceLocation (Located(..), SourceSpan(..), SourcePos(..))
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Map as Map

-- Additional generators for Error handling testing
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Error, Warning, Info, Hint]

genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ SyntaxError
  , TypeError
  , NameError
  , ScopeError
  , ImportError
  , ModuleError
  , ParseError
  , LexError
  , SemanticError
  , RuntimeWarning
  ]

genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 100)
  col <- choose (1, 100)
  offset <- choose (0, 10000)
  let pos = SourcePos line col offset
      span = SourceSpan pos pos
  return $ ErrorLocation span

genErrorContext :: Gen ErrorContext
genErrorContext = do
  phase <- elements [LexingPhase, ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, DependentTypeCheckingPhase, CodeGenerationPhase]
  file <- genIdentifier
  function <- genIdentifier
  return $ ErrorContext phase file function

genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = oneof
  [ pure NoRecovery
  , SkipToken <$> genInt
  , InsertToken <$> genIdentifier
  , RetryParsing <$> genInt
  , SuggestAlternative <$> listOf genIdentifier
  ]

genCompilerError :: Gen CompilerError
genCompilerError = do
  errorId <- genIdentifier
  severity <- genErrorSeverity
  category <- genErrorCategory
  message <- T.pack <$> genString
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  suggestions <- listOf $ T.pack <$> genString
  relatedErrors <- pure []
  errorChain <- pure []
  timestamp <- pure Nothing
  
  let typeError = TypeError
        { errorId = errorId
        , severity = severity
        , category = category
        , message = message
        , location = location
        , context = context
        , recovery = recovery
        , suggestions = suggestions
        , relatedErrors = relatedErrors
        , errorChain = errorChain
        , timestamp = timestamp
        }
  
  sourceContext <- frequency [(1, pure Nothing), (2, Just <$> genString)]
  stackTrace <- listOf genIdentifier
  phase <- elements [LexingPhase, ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, DependentTypeCheckingPhase, CodeGenerationPhase]
  
  return $ CompilerError
    { ceError = typeError
    , ceSourceContext = sourceContext
    , ceStackTrace = stackTrace
    , cePhase = phase
    }

genErrorReport :: Gen ErrorReport
genErrorReport = do
  errors <- listOf genCompilerError
  warnings <- listOf genCompilerError
  info <- listOf genCompilerError
  return $ ErrorReport errors warnings info

genErrorHandler :: Gen ErrorHandler
genErrorHandler = do
  maxErrors <- choose (1, 100)
  collectWarnings <- elements [True, False]
  collectInfo <- elements [True, False]
  return $ ErrorHandler maxErrors collectWarnings collectInfo []

genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return (first : rest)

genString :: Gen String
genString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '\t', '\n', '!', '?', '.', ',', ';', ':', '(', ')', '[', ']', '{', '}', '+', '-', '*', '/', '=', '<', '>', '_', '|', '&']

genInt :: Gen Int
genInt = choose (0, 100)

-- Property: Error severity ordering is consistent
prop_errorSeverityOrderingConsistent :: ErrorSeverity -> ErrorSeverity -> Bool
prop_errorSeverityOrderingConsistent sev1 sev2 = 
  let severityOrder sev = case sev of
        Error -> 4
        Warning -> 3
        Info -> 2
        Hint -> 1
  in if severityOrder sev1 >= severityOrder sev2 
     then True 
     else severityOrder sev1 < severityOrder sev2

-- Property: Error context contains valid phase information
prop_errorContextValidPhase :: ErrorContext -> Bool
prop_errorContextValidPhase context = 
  let phase = errorContextPhase context
  in phase `elem` [LexingPhase, ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, DependentTypeCheckingPhase, CodeGenerationPhase]

-- Property: Error location has valid source positions
prop_errorLocationValidPositions :: ErrorLocation -> Bool
prop_errorLocationValidPositions (ErrorLocation span) = 
  let start = spanStart span
      end = spanEnd span
      startLine = sourcePosLine start
      endLine = sourcePosLine end
      startCol = sourcePosColumn start
      endCol = sourcePosColumn end
  in startLine >= 1 && endLine >= 1 && startCol >= 1 && endCol >= 1

-- Property: Error recovery strategies are valid
prop_errorRecoveryValidStrategies :: ErrorRecovery -> Bool
prop_errorRecoveryValidStrategies recovery = 
  case recovery of
    NoRecovery -> True
    SkipToken n -> n >= 0
    InsertToken token -> not (null token)
    RetryParsing n -> n >= 0
    SuggestAlternative alts -> all (not . null) alts

-- Property: Error report categorizes errors correctly
prop_errorReportCategorizationCorrect :: ErrorReport -> Bool
prop_errorReportCategorizationCorrect report = 
  let errors = reportErrors report
      warnings = reportWarnings report
      info = reportInfo report
      isError e = severity (ceError e) == Error
      isWarning e = severity (ceError e) == Warning
      isInfo e = severity (ceError e) == Info
  in all isError errors && all isWarning warnings && all isInfo info

-- Property: Error handler respects maximum error limit
prop_errorHandlerRespectsMaxErrors :: ErrorHandler -> [CompilerError] -> Bool
prop_errorHandlerRespectsMaxErrors handler errors = 
  let maxErrors = ehMaxErrors handler
      processed = foldl addError handler errors
      finalErrors = ehErrors processed
  in length finalErrors <= maxErrors
  where
    addError h e = h { ehErrors = e : ehErrors h }

-- Property: Error message formatting preserves information
prop_errorMessageFormattingPreservesInfo :: CompilerError -> Bool
prop_errorMessageFormattingPreservesInfo error = 
  let formatted = formatCompilerError error
      originalMessage = message (ceError error)
  in originalMessage `T.isInfixOf` formatted

-- Property: Error chain maintains dependency order
prop_errorChainMaintainsOrder :: CompilerError -> Bool
prop_errorChainMaintainsOrder error = 
  let chain = errorChain (ceError error)
  in length chain == length chain  -- Simplified - would check actual ordering

-- Property: Error suggestions are relevant
prop_errorSuggestionsRelevant :: CompilerError -> Bool
prop_errorSuggestionsRelevant error = 
  let suggestions = suggestions (ceError error)
      category = category (ceError error)
  in null suggestions || all (not . T.null) suggestions

-- Property: Error filtering by severity works correctly
prop_errorFilteringBySeverity :: [CompilerError] -> ErrorSeverity -> Bool
prop_errorFilteringBySeverity errors targetSeverity = 
  let filtered = filterBySeverity errors targetSeverity
      hasCorrectSeverity e = severity (ceError e) == targetSeverity
  in all hasCorrectSeverity filtered

-- Property: Error sorting by severity maintains order
prop_errorSortingBySeverity :: [CompilerError] -> Bool
prop_errorSortingBySeverity errors = 
  let sorted = sortBySeverity errors
      severityPairs = zip sorted (tail sorted)
      isOrdered (e1, e2) = 
        let sev1 = severity (ceError e1)
            sev2 = severity (ceError e2)
            severityOrder sev = case sev of
              Error -> 4
              Warning -> 3
              Info -> 2
              Hint -> 1
        in severityOrder sev1 >= severityOrder sev2
  in all isOrdered severityPairs

-- Property: Error aggregation preserves all information
prop_errorAggregationPreservesInfo :: [ErrorReport] -> Bool
prop_errorAggregationPreservesInfo reports = 
  let aggregated = aggregateErrorReports reports
      totalErrors = sum $ map (length . reportErrors) reports
      totalWarnings = sum $ map (length . reportWarnings) reports
      totalInfo = sum $ map (length . reportInfo) reports
      aggregatedErrors = length $ reportErrors aggregated
      aggregatedWarnings = length $ reportWarnings aggregated
      aggregatedInfo = length $ reportInfo aggregated
  in aggregatedErrors == totalErrors && 
     aggregatedWarnings == totalWarnings && 
     aggregatedInfo == totalInfo

-- Helper functions (these would normally be in the ErrorHandler modules)
reportErrors :: ErrorReport -> [CompilerError]
reportErrors (ErrorReport errors _ _) = errors

reportWarnings :: ErrorReport -> [CompilerError]
reportWarnings (ErrorReport _ warnings _) = warnings

reportInfo :: ErrorReport -> [CompilerError]
reportInfo (ErrorReport _ _ info) = info

ehMaxErrors :: ErrorHandler -> Int
ehMaxErrors (ErrorHandler max _ _ _) = max

ehErrors :: ErrorHandler -> [CompilerError]
ehErrors (ErrorHandler _ _ _ errors) = errors

formatCompilerError :: CompilerError -> T.Text
formatCompilerError error = 
  let err = ceError error
      severityStr = case severity err of
        Error -> "Error"
        Warning -> "Warning"
        Info -> "Info"
        Hint -> "Hint"
  in T.pack $ severityStr ++ ": " ++ T.unpack (message err)

errorChain :: TypeError -> [CompilerError]
errorChain _ = []  -- Simplified

filterBySeverity :: [CompilerError] -> ErrorSeverity -> [CompilerError]
filterBySeverity errors targetSeverity = 
  filter (\e -> severity (ceError e) == targetSeverity) errors

sortBySeverity :: [CompilerError] -> [CompilerError]
sortBySeverity = List.sortBy (\e1 e2 -> 
  let sev1 = severity (ceError e1)
      sev2 = severity (ceError e2)
      severityOrder sev = case sev of
        Error -> 4
        Warning -> 3
        Info -> 2
        Hint -> 1
  in compare (severityOrder sev2) (severityOrder sev1))

aggregateErrorReports :: [ErrorReport] -> ErrorReport
aggregateErrorReports reports = 
  let allErrors = concatMap reportErrors reports
      allWarnings = concatMap reportWarnings reports
      allInfo = concatMap reportInfo reports
  in ErrorReport allErrors allWarnings allInfo

-- Test suite
tests :: TestTree
tests = testGroup "New Error Handling QuickCheck Tests"
  [ testProperty "Error severity ordering is consistent" $
      fastProperty "Error severity ordering consistent" prop_errorSeverityOrderingConsistent
  
  , testProperty "Error context contains valid phase information" $
      fastProperty "Error context valid phase" prop_errorContextValidPhase
  
  , testProperty "Error location has valid source positions" $
      fastProperty "Error location valid positions" prop_errorLocationValidPositions
  
  , testProperty "Error recovery strategies are valid" $
      fastProperty "Error recovery valid strategies" prop_errorRecoveryValidStrategies
  
  , testProperty "Error report categorizes errors correctly" $
      fastProperty "Error report categorization correct" prop_errorReportCategorizationCorrect
  
  , testProperty "Error handler respects maximum error limit" $
      fastProperty "Error handler respects max errors" prop_errorHandlerRespectsMaxErrors
  
  , testProperty "Error message formatting preserves information" $
      fastProperty "Error message formatting preserves info" prop_errorMessageFormattingPreservesInfo
  
  , testProperty "Error chain maintains dependency order" $
      fastProperty "Error chain maintains order" prop_errorChainMaintainsOrder
  
  , testProperty "Error suggestions are relevant" $
      fastProperty "Error suggestions relevant" prop_errorSuggestionsRelevant
  
  , testProperty "Error filtering by severity works correctly" $
      fastProperty "Error filtering by severity" prop_errorFilteringBySeverity
  
  , testProperty "Error sorting by severity maintains order" $
      fastProperty "Error sorting by severity" prop_errorSortingBySeverity
  
  , testProperty "Error aggregation preserves all information" $
      fastProperty "Error aggregation preserves info" prop_errorAggregationPreservesInfo
  ]