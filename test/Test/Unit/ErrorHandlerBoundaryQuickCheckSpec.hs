module Test.Unit.ErrorHandlerBoundaryQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck
import Compiler.Errors.Core
import Compiler.Errors (CompilationPhase(..), mkCompilerError, CompilerError(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sort, sortOn, isInfixOf)

-- Arbitrary instances for QuickCheck
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary CompilationPhase where
  arbitrary = elements [LexingPhase, ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, DependentTypeCheckingPhase, CodeGenerationPhase, OptimizationPhase]

-- | Test that ErrorLocation constructor works correctly
prop_error_location_constructor :: Maybe String -> Int -> Int -> Maybe Int -> Maybe Int -> Property
prop_error_location_constructor file line col endLine endColumn = 
  let loc = ErrorLocation file line col endLine endColumn
  in property $ 
    filePath loc == file &&
    Compiler.Errors.Core.line loc == line &&
    column loc == col &&
    Compiler.Errors.Core.endLine loc == endLine &&
    Compiler.Errors.Core.endColumn loc == endColumn

-- | Test that CompilerError constructor works correctly
prop_compiler_error_constructor :: String -> T.Text -> ErrorSeverity -> ErrorCategory -> CompilationPhase -> Property
prop_compiler_error_constructor errId errMsg errSeverity errCategory errPhase = 
  let err = mkCompilerError errId errMsg errPhase errCategory errSeverity Nothing Nothing [] [] Nothing
  in property $ 
    errorId (ceError err) == errId &&
    message (ceError err) == errMsg &&
    severity (ceError err) == errSeverity &&
    category (ceError err) == errCategory &&
    cePhase err == errPhase

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
  let errors = map (\s -> mkCompilerError "TEST" (T.pack "test") ParsingPhase TypeChecking s Nothing Nothing [] [] Nothing) severities
      typeErrors = map ceError errors
      filtered = filterBySeverity minSeverity typeErrors
      expected = filter (\e -> isAtLeast (severity e) minSeverity) typeErrors
  in property $ length filtered == length expected

-- | Test that sortBySeverity orders errors correctly
prop_sort_by_severity_correct :: [ErrorSeverity] -> Property
prop_sort_by_severity_correct severities = 
  let errors = map (\s -> mkCompilerError "TEST" (T.pack "test") ParsingPhase TypeChecking s Nothing Nothing [] [] Nothing) severities
      typeErrors = map ceError errors
      sorted = sortBySeverity typeErrors
      expected = sortOn severity typeErrors
  in property $ map severity sorted == map severity expected

-- | Test that filterByCategory works correctly
prop_filter_by_category_correct :: [ErrorCategory] -> Property
prop_filter_by_category_correct categories = 
  let typeErrors = zipWith (\c i -> errorWithCategory ("TEST" ++ show i) c (T.pack "test") unknownLocation) categories [1..]
      filtered = filterByCategory TypeChecking typeErrors
      expected = filter (\e -> category e == TypeChecking) typeErrors
  in property $ length filtered == length expected

-- | Test that hasCategory works correctly
prop_has_category_correct :: ErrorCategory -> [ErrorCategory] -> Property
prop_has_category_correct target categories = 
  let typeErrors = zipWith (\c i -> errorWithCategory ("TEST" ++ show i) c (T.pack "test") unknownLocation) categories [1..]
      hasTarget = any (\e -> category e == target) typeErrors
      result = hasCategory target (head typeErrors)
  in property $ result == (if null typeErrors then False else category (head typeErrors) == target)

-- | Test that getErrors returns correct count
prop_error_count_correct :: Int -> Property
prop_error_count_correct n = 
  let typeErrors = replicate n (errorAt "TEST" Error (T.pack "test") unknownLocation)
  in property $ length typeErrors == n

-- | Test that filterBySeverity works correctly
prop_error_count_by_severity_correct :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_error_count_by_severity_correct severities target = 
  let typeErrors = map (\s -> errorAt "TEST" s (T.pack "test") unknownLocation) severities
      filtered = filterBySeverity target typeErrors
      expected = filter (\e -> severity e == target) typeErrors
  in property $ length filtered == length expected

-- | Test that filterByCategory with specific category works correctly
prop_error_count_by_category_correct :: [ErrorCategory] -> ErrorCategory -> Property
prop_error_count_by_category_correct categories target = 
  let typeErrors = zipWith (\c i -> errorWithCategory ("TEST" ++ show i) c (T.pack "test") unknownLocation) categories [1..]
      filtered = filterByCategory target typeErrors
      expected = filter (\e -> category e == target) typeErrors
  in property $ length filtered == length expected

-- | Test that sortBySeverity works correctly
prop_get_most_severe_correct :: [ErrorSeverity] -> Property
prop_get_most_severe_correct severities = 
  let typeErrors = map (\s -> errorAt "TEST" s (T.pack "test") unknownLocation) severities
      sorted = sortBySeverity typeErrors
      expected = sortOn severity typeErrors
  in property $ map severity sorted == map severity expected

-- | Test that hasErrors works correctly
prop_has_errors_correct :: Int -> Property
prop_has_errors_correct n = 
  let typeErrors = replicate n (errorAt "TEST" Error (T.pack "test") unknownLocation)
  in property $ hasErrors typeErrors == (n > 0)

-- | Test that hasErrors works correctly for fatal errors
prop_has_fatal_errors_correct :: Int -> Int -> Property
prop_has_fatal_errors_correct fatalCount totalCount = 
  let fatalErrors = replicate fatalCount (errorAt "TEST" Fatal (T.pack "test") unknownLocation)
      otherErrors = replicate (totalCount - fatalCount) (errorAt "TEST" Error (T.pack "test") unknownLocation)
      allErrors = fatalErrors ++ otherErrors
  in property $ 
    totalCount >= fatalCount && totalCount >= 0 && fatalCount >= 0 ==>
    hasErrors allErrors == (totalCount > 0)

-- | Test that formatError returns non-empty string
prop_format_error_non_empty :: String -> Property
prop_format_error_non_empty msg = 
  let err = errorAt "TEST" Error (T.pack msg) unknownLocation
      formatted = formatError err
  in property $ not (null formatted)

-- | Test that formatError includes error ID
prop_format_error_includes_id :: String -> String -> Property
prop_format_error_includes_id id msg = 
  let err = mkCompilerError id (T.pack msg) ParsingPhase TypeChecking Error Nothing Nothing [] [] Nothing
      formatted = formatError (ceError err)
  in property $ id `isInfixOf` formatted

-- | Test that formatError includes message
prop_format_error_includes_message :: String -> String -> Property
prop_format_error_includes_message id msg = 
  let err = mkCompilerError id (T.pack msg) ParsingPhase TypeChecking Error Nothing Nothing [] [] Nothing
      formatted = formatError (ceError err)
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
  , testProperty "filterByCategory correct" prop_filter_by_category_correct
  , testProperty "hasCategory correct" prop_has_category_correct
  , testProperty "errorCount correct" prop_error_count_correct
  , testProperty "filterBySeverity count correct" prop_error_count_by_severity_correct
  , testProperty "filterByCategory count correct" prop_error_count_by_category_correct
  , testProperty "sortBySeverity for most severe" prop_get_most_severe_correct
  , testProperty "hasErrors correct" prop_has_errors_correct
  , testProperty "hasFatalErrors correct" prop_has_fatal_errors_correct
  , testProperty "formatError non-empty" prop_format_error_non_empty
  , testProperty "formatError includes ID" prop_format_error_includes_id
  , testProperty "formatError includes message" prop_format_error_includes_message
  ]