{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.NewCabalErrorHandlerQuickCheckTestsSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, counterexample, suchThat, oneof, elements, listOf, choose)
import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
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
  , filterByCategory
  , filterBySeverity
  , hasCategory
  , getErrorLine
  , getErrorColumn
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate error severity levels
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- Generate error categories  
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ ParseError
  , TypeError
  , NameError
  , ScopeError
  , ImportError
  , SyntaxError
  , SemanticError
  , RuntimeError
  , InternalError
  , UserError
  ]

-- Generate error locations
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 1000)
  column <- choose (1, 100)
  endLine <- choose (line, line + 10)  -- end line >= start line
  endColumn <- if endLine == line 
               then choose (column, column + 50)  -- end column >= start column
               else choose (1, 100)
  return $ ErrorLocation Nothing line column (Just endLine) (Just endColumn)

-- Generate error context
genErrorContext :: Gen ErrorContext
genErrorContext = do
  contextSize <- choose (0, 5)
  contextPairs <- sequence $ replicate contextSize $ do
    key <- choose (1, 10)
    value <- choose (1, 100)
    return (T.pack $ "key" ++ show key, T.pack $ "value" ++ show value)
  return $ ErrorContext $ Map.fromList contextPairs

-- Generate type errors
genTypeError :: Gen TypeError
genTypeError = do
  severity <- genErrorSeverity
  category <- genErrorCategory
  location <- genErrorLocation
  message <- T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
  suggestions <- listOf $ T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
  context <- genErrorContext
  return $ TypeError
    { errorSeverity = severity
    , errorCategory = category
    , errorLocation = location
    , errorMessage = message
    , errorSuggestions = suggestions
    , errorContext = context
    , errorTimestamp = Nothing
    }

-- Generate combined errors
genCombinedError :: Gen CombinedError
genCombinedError = do
  numErrors <- choose (1, 5)
  errors <- sequence $ replicate numErrors genTypeError
  return $ CombinedError errors

-- Generate strings for error messages
genErrorMessage :: Gen String
genErrorMessage = do
  length' <- choose (5, 50)
  listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,;:!?()-"

-- Generate suggestions
genSuggestions :: Gen [Text]
genSuggestions = do
  count <- choose (0, 3)
  sequence $ replicate count $ do
    length' <- choose (5, 30)
    T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")

-- ============================================================================
-- Properties for ErrorSeverity
-- ============================================================================

prop_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering sev1 sev2 =
  let ordering = [Fatal, Error, Warning, Info]
      index1 = length $ takeWhile (/= sev1) ordering
      index2 = length $ takeWhile (/= sev2) ordering
  in (sev1 <= sev2) === (index1 <= index2)

-- ============================================================================
-- Properties for ErrorLocation
-- ============================================================================

prop_error_location_valid :: ErrorLocation -> Property
prop_error_location_valid loc =
  let validLine = line loc > 0
      validColumn = column loc > 0
      validEndLine = case endLine loc of
                      Nothing -> True
                      Just el -> el >= line loc
      validEndColumn = case endColumn loc of
                        Nothing -> True
                        Just ec -> if endLine loc == Just (line loc) 
                                   then ec >= column loc
                                   else ec > 0
  in validLine && validColumn && validEndLine && validEndColumn

prop_get_error_line_consistent :: ErrorLocation -> Property
prop_get_error_line_consistent loc =
  getErrorLine loc === line loc

prop_get_error_column_consistent :: ErrorLocation -> Property
prop_get_error_column_consistent loc =
  getErrorColumn loc === column loc

-- ============================================================================
-- Properties for ErrorContext
-- ============================================================================

prop_empty_context_empty :: Property
prop_empty_context_empty =
  let ErrorContext ctx = emptyContext
  in Map.null ctx

prop_context_lookup :: Property
prop_context_lookup =
  forAll genErrorContext $ \ctx ->
    let ErrorContext ctxMap = ctx
        keyCount = Map.size ctxMap
    in if keyCount > 0
       then let (key, _) = head $ Map.toList ctxMap
            in hasContext ctx key
       else property True
  where
    hasContext (ErrorContext ctxMap) key = Map.member key ctxMap

-- ============================================================================
-- Properties for ErrorCollector
-- ============================================================================

prop_new_collector_empty :: Property
prop_new_collector_empty =
  let collector = newErrorCollector
  in not (hasErrors collector) && not (hasWarnings collector)

prop_add_error_increases_count :: Property
prop_add_error_increases_count =
  forAll genTypeError $ \err ->
    let collector = newErrorCollector
        collector' = addError err collector
    in hasErrors collector' && length (getErrors collector') >= 1

prop_add_warning_increases_count :: Property
prop_add_warning_increases_count =
  forAll genTypeError $ \warn ->
    let collector = newErrorCollector
        collector' = addWarning warn collector
    in hasWarnings collector' && length (getWarnings collector') >= 1

prop_add_info_increases_count :: Property
prop_add_info_increases_count =
  forAll genTypeError $ \info ->
    let collector = newErrorCollector
        collector' = addInfo info collector
    in length (getInfo collector') >= 1

prop_all_messages_include_all_types :: Property
prop_all_messages_include_all_types =
  forAll genTypeError $ \err ->
    forAll genTypeError $ \warn ->
      forAll genTypeError $ \info ->
        let collector = newErrorCollector
            collector1 = addError err collector
            collector2 = addWarning warn collector1
            collector3 = addInfo info collector2
            allMsgs = getAllMessages collector3
        in length allMsgs >= 3

-- ============================================================================
-- Properties for Error Creation
-- ============================================================================

prop_error_at_sets_location :: Property
prop_error_at_sets_location =
  forAll genErrorLocation $ \loc ->
    forAll genErrorMessage $ \msg ->
      let err = errorAt loc (T.pack msg)
      in errorLocation err === loc

prop_error_with_category_sets_category :: Property
prop_error_with_category_sets_category =
  forAll genErrorCategory $ \cat ->
    forAll genErrorMessage $ \msg ->
      let err = errorWithCategory cat (T.pack msg)
      in errorCategory err === cat

prop_warning_at_has_warning_severity :: Property
prop_warning_at_has_warning_severity =
  forAll genErrorLocation $ \loc ->
    forAll genErrorMessage $ \msg ->
      let warn = warningAt loc (T.pack msg)
      in errorSeverity warn === Warning

prop_fatal_error_has_fatal_severity :: Property
prop_fatal_error_has_fatal_severity =
  forAll genErrorMessage $ \msg ->
    let fatal = fatalError (T.pack msg)
    in errorSeverity fatal === Fatal

-- ============================================================================
-- Properties for Error Modification
-- ============================================================================

prop_with_location_updates_location :: Property
prop_with_location_updates_location =
  forAll genTypeError $ \err ->
    forAll genErrorLocation $ \loc ->
      let updated = withLocation loc err
      in errorLocation updated === loc

prop_with_suggestions_adds_suggestions :: Property
prop_with_suggestions_adds_suggestions =
  forAll genTypeError $ \err ->
    forAll genSuggestions $ \suggestions ->
      let updated = withSuggestions suggestions err
          oldCount = length $ errorSuggestions err
          newCount = length $ errorSuggestions updated
      in newCount >= oldCount

prop_with_context_adds_context :: Property
prop_with_context_adds_context =
  forAll genTypeError $ \err ->
    forAll genErrorContext $ \ctx ->
      let updated = withContext ctx err
      in errorContext updated === ctx

-- ============================================================================
-- Properties for Error Combination
-- ============================================================================

prop_combine_errors_preserves_all :: Property
prop_combine_errors_preserves_all =
  forAll genTypeError $ \err1 ->
    forAll genTypeError $ \err2 ->
      let combined = combineErrors err1 err2
          CombinedError errors = combined
      in length errors === 2 && err1 `elem` errors && err2 `elem` errors

prop_combined_severity_is_maximum :: Property
prop_combined_severity_is_maximum =
  forAll genTypeError $ \err1 ->
    forAll genTypeError $ \err2 ->
      let combined = combineErrors err1 err2
          severity = combinedErrorSeverity combined
          sev1 = errorSeverity err1
          sev2 = errorSeverity err2
      in severity === max sev1 sev2

-- ============================================================================
-- Properties for Error Filtering
-- ============================================================================

prop_filter_by_category :: Property
prop_filter_by_category =
  forAll (listOf1 genTypeError) $ \errors ->
    forAll genErrorCategory $ \cat ->
      let filtered = filterByCategory cat errors
      in all (\e -> errorCategory e === cat) filtered

prop_filter_by_severity :: Property
prop_filter_by_severity =
  forAll (listOf1 genTypeError) $ \errors ->
    forAll genErrorSeverity $ \sev ->
      let filtered = filterBySeverity sev errors
      in all (\e -> errorSeverity e === sev) filtered

prop_has_category_check :: Property
prop_has_category_check =
  forAll genTypeError $ \err ->
    let cat = errorCategory err
    in hasCategory err cat

-- ============================================================================
-- Properties for Error Formatting
-- ============================================================================

prop_format_error_includes_message :: Property
prop_format_error_includes_message =
  forAll genTypeError $ \err ->
    let formatted = formatError err
        msg = errorMessage err
    in T.unpack msg `isInfixOf` formatted

prop_format_errors_non_empty :: Property
prop_format_errors_non_empty =
  forAll (listOf1 genTypeError) $ \errors ->
    let formatted = formatErrors errors
    in not (null formatted)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck Tests"
  [ testGroup "ErrorSeverity"
    [ testProperty "severity ordering is consistent" prop_severity_ordering
    ]
  , testGroup "ErrorLocation"
    [ testProperty "error location is valid" prop_error_location_valid
    , testProperty "get error line consistent" prop_get_error_line_consistent
    , testProperty "get error column consistent" prop_get_error_column_consistent
    ]
  , testGroup "ErrorContext"
    [ testProperty "empty context is empty" prop_empty_context_empty
    , testProperty "context lookup works" prop_context_lookup
    ]
  , testGroup "ErrorCollector"
    [ testProperty "new collector is empty" prop_new_collector_empty
    , testProperty "add error increases count" prop_add_error_increases_count
    , testProperty "add warning increases count" prop_add_warning_increases_count
    , testProperty "add info increases count" prop_add_info_increases_count
    , testProperty "all messages include all types" prop_all_messages_include_all_types
    ]
  , testGroup "Error Creation"
    [ testProperty "error at sets location" prop_error_at_sets_location
    , testProperty "error with category sets category" prop_error_with_category_sets_category
    , testProperty "warning at has warning severity" prop_warning_at_has_warning_severity
    , testProperty "fatal error has fatal severity" prop_fatal_error_has_fatal_severity
    ]
  , testGroup "Error Modification"
    [ testProperty "with location updates location" prop_with_location_updates_location
    [ testProperty "with suggestions adds suggestions" prop_with_suggestions_adds_suggestions
    , testProperty "with context adds context" prop_with_context_adds_context
    ]
  , testGroup "Error Combination"
    [ testProperty "combine errors preserves all" prop_combine_errors_preserves_all
    , testProperty "combined severity is maximum" prop_combined_severity_is_maximum
    ]
  , testGroup "Error Filtering"
    [ testProperty "filter by category" prop_filter_by_category
    , testProperty "filter by severity" prop_filter_by_severity
    , testProperty "has category check" prop_has_category_check
    ]
  , testGroup "Error Formatting"
    [ testProperty "format error includes message" prop_format_error_includes_message
    , testProperty "format errors non empty" prop_format_errors_non_empty
    ]
  ]