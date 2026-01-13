module Test.Unit.ErrorHandlerBoundaryQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
import Compiler.Errors (CompilationPhase(..))
import SourceLocation (SourcePos(..), SourceSpan(..), emptySpan, startPos)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sort)

-- | Test that ErrorLocation constructor works correctly
prop_error_location_constructor :: Maybe String -> Int -> Int -> Maybe String -> Maybe String -> Property
prop_error_location_constructor file line col function message = 
  let loc = ErrorLocation file line col function message
  in property $ 
    getErrorFile loc == file &&
    getErrorLine loc == line &&
    getErrorColumn loc == col &&
    getErrorFunction loc == function &&
    getErrorMessage loc == message

-- | Test that CompilerError constructor works correctly
prop_compiler_error_constructor :: String -> T.Text -> ErrorSeverity -> ErrorCategory -> CompilationPhase -> Property
prop_compiler_error_constructor id msg severity category phase = 
  let err = CompilerError id msg severity category phase Nothing Nothing [] [] Nothing
  in property $ 
    errorId err == id &&
    errorMessage err == msg &&
    errorSeverity err == severity &&
    errorCategory err == category &&
    errorPhase err == phase

-- | Test that severityPriority returns consistent values
prop_severity_priority_consistency :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_consistency s1 s2 = 
  let p1 = severityPriority s1
      p2 = severityPriority s2
  in property $ 
    (p1 > p2) ==> (compareSeverity s1 s2 == GT) &&
    (p1 < p2) ==> (compareSeverity s1 s2 == LT) &&
    (p1 == p2) ==> (compareSeverity s1 s2 == EQ)

-- | Test that isAtLeast is transitive
prop_is_at_least_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least_transitive s1 s2 s3 = 
  let p1 = severityPriority s1
      p2 = severityPriority s2
      p3 = severityPriority s3
  in property $ 
    (p1 >= p2 && p2 >= p3) ==> isAtLeast s1 s3

-- | Test that isAtLeast is reflexive
prop_is_at_least_reflexive :: ErrorSeverity -> Property
prop_is_at_least_reflexive s = property $ isAtLeast s s

-- | Test that compareErrorSeverity is consistent with severityPriority
prop_compare_severity_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_compare_severity_consistent s1 s2 = 
  let p1 = severityPriority s1
      p2 = severityPriority s2
  in property $ compareSeverity s1 s2 == compare p1 p2

-- | Test that filterBySeverity works correctly
prop_filter_by_severity_correct :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_filter_by_severity_correct severities minSeverity = 
  let errors = map (\s -> CompilerError "TEST" (T.pack "test") s TypeChecking ParsingPhase Nothing Nothing [] [] Nothing) severities
      filtered = filterBySeverity minSeverity errors
      expected = filter (\e -> isAtLeast (errorSeverity e) minSeverity) errors
  in property $ length filtered == length expected

-- | Test that sortBySeverity orders errors correctly
prop_sort_by_severity_correct :: [ErrorSeverity] -> Property
prop_sort_by_severity_correct severities = 
  let errors = map (\s -> CompilerError "TEST" (T.pack "test") s TypeChecking ParsingPhase Nothing Nothing [] [] Nothing) severities
      sorted = sortBySeverity errors
      priorities = map (severityPriority . errorSeverity) sorted
  in property $ priorities == sort priorities

-- | Test that groupByCategory works correctly
prop_group_by_category_correct :: [ErrorCategory] -> Property
prop_group_by_category_correct categories = 
  let errors = zipWith (\c i -> CompilerError ("TEST" ++ show i) (T.pack "test") Error c ParsingPhase Nothing Nothing [] [] Nothing) categories [1..]
      grouped = groupByCategory errors
      groupSizes = map length (Map.elems grouped)
  in property $ sum groupSizes == length errors

-- | Test that hasCategory works correctly
prop_has_category_correct :: ErrorCategory -> [ErrorCategory] -> Property
prop_has_category_correct target categories = 
  let errors = zipWith (\c i -> CompilerError ("TEST" ++ show i) (T.pack "test") Error c ParsingPhase Nothing Nothing [] [] Nothing) categories [1..]
      hasTarget = any (\e -> errorCategory e == target) errors
      result = hasCategory target errors
  in property $ result == hasTarget

-- | Test that errorCount returns correct count
prop_error_count_correct :: Int -> Property
prop_error_count_correct n = 
  let errors = replicate n (CompilerError "TEST" (T.pack "test") Error TypeChecking ParsingPhase Nothing Nothing [] [] Nothing)
  in property $ errorCount errors == n

-- | Test that errorCountBySeverity works correctly
prop_error_count_by_severity_correct :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_error_count_by_severity_correct severities target = 
  let errors = map (\s -> CompilerError "TEST" (T.pack "test") s TypeChecking ParsingPhase Nothing Nothing [] [] Nothing) severities
      count = length (filter (\e -> errorSeverity e == target) errors)
  in property $ errorCountBySeverity target errors == count

-- | Test that errorCountByCategory works correctly
prop_error_count_by_category_correct :: [ErrorCategory] -> ErrorCategory -> Property
prop_error_count_by_category_correct categories target = 
  let errors = zipWith (\c i -> CompilerError ("TEST" ++ show i) (T.pack "test") Error c ParsingPhase Nothing Nothing [] [] Nothing) categories [1..]
      count = length (filter (\e -> errorCategory e == target) errors)
  in property $ errorCountByCategory target errors == count

-- | Test that getMostSevere works correctly
prop_get_most_severe_correct :: [ErrorSeverity] -> Property
prop_get_most_severe_correct severities = 
  let errors = map (\s -> CompilerError "TEST" (T.pack "test") s TypeChecking ParsingPhase Nothing Nothing [] [] Nothing) severities
      mostSevere = getMostSevere errors
  in property $ 
    case mostSevere of
      Nothing -> null errors
      Just err -> all (\e -> isAtLeast (errorSeverity err) (errorSeverity e)) errors

-- | Test that hasErrors works correctly
prop_has_errors_correct :: Int -> Property
prop_has_errors_correct n = 
  let errors = replicate n (CompilerError "TEST" (T.pack "test") Error TypeChecking ParsingPhase Nothing Nothing [] [] Nothing)
  in property $ hasErrors errors == (n > 0)

-- | Test that hasFatalErrors works correctly
prop_has_fatal_errors_correct :: Int -> Int -> Property
prop_has_fatal_errors_correct fatalCount totalCount = 
  let fatalErrors = replicate fatalCount (CompilerError "TEST" (T.pack "test") Fatal TypeChecking ParsingPhase Nothing Nothing [] [] Nothing)
      otherErrors = replicate (totalCount - fatalCount) (CompilerError "TEST" (T.pack "test") Error TypeChecking ParsingPhase Nothing Nothing [] [] Nothing)
      allErrors = fatalErrors ++ otherErrors
  in property $ 
    totalCount >= fatalCount && totalCount >= 0 && fatalCount >= 0 ==>
    hasFatalErrors allErrors == (fatalCount > 0)

-- | Test that formatError returns non-empty string
prop_format_error_non_empty :: String -> Property
prop_format_error_non_empty msg = 
  let err = CompilerError "TEST" (T.pack msg) Error TypeChecking ParsingPhase Nothing Nothing [] [] Nothing
      formatted = formatError err
  in property $ not (null formatted)

-- | Test that formatError includes error ID
prop_format_error_includes_id :: String -> String -> Property
prop_format_error_includes_id id msg = 
  let err = CompilerError id (T.pack msg) Error TypeChecking ParsingPhase Nothing Nothing [] [] Nothing
      formatted = formatError err
  in property $ id `isInfixOf` formatted

-- | Test that formatError includes message
prop_format_error_includes_message :: String -> String -> Property
prop_format_error_includes_message id msg = 
  let err = CompilerError id (T.pack msg) Error TypeChecking ParsingPhase Nothing Nothing [] [] Nothing
      formatted = formatError err
  in property $ not (null msg) ==> msg `isInfixOf` formatted

tests :: TestTree
tests = testGroup "ErrorHandler Boundary QuickCheck Tests"
  [ testProperty "ErrorLocation constructor" prop_error_location_constructor
  , testProperty "CompilerError constructor" prop_compiler_error_constructor
  , testProperty "severityPriority consistency" prop_severity_priority_consistency
  , testProperty "isAtLeast transitive" prop_is_at_least_transitive
  , testProperty "isAtLeast reflexive" prop_is_at_least_reflexive
  , testProperty "compareSeverity consistent" prop_compare_severity_consistent
  , testProperty "filterBySeverity correct" prop_filter_by_severity_correct
  , testProperty "sortBySeverity correct" prop_sort_by_severity_correct
  , testProperty "groupByCategory correct" prop_group_by_category_correct
  , testProperty "hasCategory correct" prop_has_category_correct
  , testProperty "errorCount correct" prop_error_count_correct
  , testProperty "errorCountBySeverity correct" prop_error_count_by_severity_correct
  , testProperty "errorCountByCategory correct" prop_error_count_by_category_correct
  , testProperty "getMostSevere correct" prop_get_most_severe_correct
  , testProperty "hasErrors correct" prop_has_errors_correct
  , testProperty "hasFatalErrors correct" prop_has_fatal_errors_correct
  , testProperty "formatError non-empty" prop_format_error_non_empty
  , testProperty "formatError includes ID" prop_format_error_includes_id
  , testProperty "formatError includes message" prop_format_error_includes_message
  ]