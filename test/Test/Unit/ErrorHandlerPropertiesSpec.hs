module Test.Unit.ErrorHandlerPropertiesSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import qualified ErrorHandler
import qualified ErrorHandler.Core as EH
import qualified ErrorHandler.Types as ET
import qualified SourceLocation as SL
import Data.Maybe (isJust, isNothing)
import Data.List (isPrefixOf, isInfixOf)
import Data.Set (Set)
import qualified Data.Set as Set

-- 测试错误处理器的属性
prop_error_creation :: String -> Int -> Int -> Property
prop_error_creation message line col = 
  let loc = SL.SourceLocation line col
      err = EH.createError message loc
  in property $ ET.errorMessage err === message && 
             ET.errorLine err === line && 
             ET.errorColumn err === col

prop_error_severity_levels :: String -> Property
prop_error_severity_levels message = 
  let errorLevels = [ET.Error, ET.Warning, ET.Info, ET.Debug]
  in property $ all (\level -> 
    let err = EH.createErrorWithSeverity message level
    in ET.errorSeverity err === level
  ) errorLevels

prop_error_context_addition :: String -> String -> Property
prop_error_context_addition message context = 
  let baseError = EH.createError message (SL.SourceLocation 1 1)
      contextualError = EH.addContext baseError context
  in property $ context `isInfixOf` ET.errorMessage contextualError

prop_error_chain_formation :: [String] -> Property
prop_error_chain_formation messages = 
  let baseError = EH.createError (head messages) (SL.SourceLocation 1 1)
      chainedError = foldr EH.chainError baseError (tail messages)
  in property $ length (ET.errorChain chainedError) === length messages

prop_error_aggregation :: [String] -> Property
prop_error_aggregation messages = 
  let errors = map (\msg -> EH.createError msg (SL.SourceLocation 1 1)) messages
      aggregated = EH.aggregateErrors errors
  in property $ length aggregated === length messages

prop_error_filtering_by_severity :: [ET.ErrorSeverity] -> Property
prop_error_filtering_by_severity severities = 
  let errors = map (\sev -> EH.createErrorWithSeverity "test" sev) severities
      filtered = EH.filterBySeverity errors ET.Error
  in property $ all (\err -> ET.errorSeverity err === ET.Error) filtered

prop_error_sorting_by_location :: [(Int, Int)] -> Property
prop_error_sorting_by_location locations = 
  let errors = map (\(line, col) -> EH.createError "test" (SL.SourceLocation line col)) locations
      sorted = EH.sortByLocation errors
      sortedLocs = map (\err -> (ET.errorLine err, ET.errorColumn err)) sorted
  in property $ sortedLocs === sort locations

prop_error_grouping_by_type :: [String] -> Property
prop_error_grouping_by_type messages = 
  let errors = map (\msg -> EH.createError msg (SL.SourceLocation 1 1)) messages
      grouped = EH.groupByType errors
  in property $ sum (map length (Map.elems grouped)) === length messages

prop_error_suppression :: String -> Property
prop_error_suppression message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      suppressed = EH.suppressError error
  in property $ ET.isSuppressed suppressed === True

prop_error_recovery :: String -> Property
prop_error_recovery message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      recovery = EH.suggestRecovery error
  in property $ isJust recovery

prop_error_formatting :: String -> Property
prop_error_formatting message = 
  let error = EH.createError message (SL.SourceLocation 5 10)
      formatted = EH.formatError error
  in property $ message `isInfixOf` formatted && 
             "5:10" `isInfixOf` formatted

prop_error_localization :: String -> Property
prop_error_localization message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      localized = EH.localizeError error "zh-CN"
  in property $ not (null localized)

prop_error_annotation :: String -> String -> Property
prop_error_annotation message annotation = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      annotated = EH.annotateError error annotation
  in property $ annotation `isInfixOf` ET.errorMessage annotated

prop_error_code_generation :: String -> Property
prop_error_code_generation message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      code = EH.generateErrorCode error
  in property $ length code >= 3

prop_error_statistics :: [String] -> Property
prop_error_statistics messages = 
  let errors = map (\msg -> EH.createError msg (SL.SourceLocation 1 1)) messages
      stats = EH.computeStatistics errors
  in property $ EH.totalErrors stats === length messages

prop_error_reporting :: [String] -> Property
prop_error_reporting messages = 
  let errors = map (\msg -> EH.createError msg (SL.SourceLocation 1 1)) messages
      report = EH.generateReport errors
  in property $ not (null report)

prop_error_categorization :: String -> Property
prop_error_categorization message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      category = EH.categorizeError error
  in property $ not (null category)

prop_error_priority :: String -> Property
prop_error_priority message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      priority = EH.assignPriority error
  in property $ priority >= 1 && priority <= 10

prop_error_threshold :: [String] -> Int -> Property
prop_error_threshold messages threshold = 
  let errors = map (\msg -> EH.createError msg (SL.SourceLocation 1 1)) messages
      exceeded = EH.exceedsThreshold errors threshold
  in property $ exceeded === (length messages > threshold)

prop_error_propagation :: String -> Property
prop_error_propagation message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      propagated = EH.propagateError error
  in property $ ET.errorMessage propagated === ET.errorMessage error

prop error_resolution :: String -> Property
prop_error_resolution message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      resolved = EH.markAsResolved error
  in property $ ET.isResolved resolved === True

prop_error_validation :: String -> Property
prop_error_validation message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      isValid = EH.validateError error
  in property $ isValid === True

prop_error_transformation :: String -> Property
prop_error_transformation message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      transformed = EH.transformError error (\msg -> "Transformed: " ++ msg)
  in property $ "Transformed:" `isPrefixOf` ET.errorMessage transformed

prop_error_accumulation :: [String] -> Property
prop_error_accumulation messages = 
  let baseError = EH.createError (head messages) (SL.SourceLocation 1 1)
      accumulated = foldr (\msg err -> EH.accumulateError err msg) baseError (tail messages)
  in property $ length (ET.errorAccumulations accumulated) === length messages - 1

prop_error_contextualization :: String -> String -> Property
prop_error_contextualization message context = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      contextualized = EH.contextualizeError error context
  in property $ context `isInfixOf` ET.errorMessage contextualized

prop_error_correlation :: [String] -> Property
prop_error_correlation messages = 
  let errors = map (\msg -> EH.createError msg (SL.SourceLocation 1 1)) messages
      correlated = EH.correlateErrors errors
  in property $ length correlated >= 1

prop_error_hierarchy :: String -> Property
prop_error_hierarchy message = 
  let error = EH.createError message (SL.SourceLocation 1 1)
      hierarchy = EH.buildErrorHierarchy error
  in property $ not (null hierarchy)

tests :: TestTree
tests = testGroup "Error Handler Properties Tests"
  [ testProperty "Error creation" prop_error_creation
  , testProperty "Error severity levels" prop_error_severity_levels
  , testProperty "Error context addition" prop_error_context_addition
  , testProperty "Error chain formation" prop_error_chain_formation
  , testProperty "Error aggregation" prop_error_aggregation
  , testProperty "Error filtering by severity" prop_error_filtering_by_severity
  , testProperty "Error sorting by location" prop_error_sorting_by_location
  , testProperty "Error grouping by type" prop_error_grouping_by_type
  , testProperty "Error suppression" prop_error_suppression
  , testProperty "Error recovery" prop_error_recovery
  , testProperty "Error formatting" prop_error_formatting
  , testProperty "Error localization" prop_error_localization
  , testProperty "Error annotation" prop_error_annotation
  , testProperty "Error code generation" prop_error_code_generation
  , testProperty "Error statistics" prop_error_statistics
  , testProperty "Error reporting" prop_error_reporting
  , testProperty "Error categorization" prop_error_categorization
  , testProperty "Error priority" prop_error_priority
  , testProperty "Error threshold" prop_error_threshold
  , testProperty "Error propagation" prop_error_propagation
  , testProperty "Error resolution" prop_error_resolution
  , testProperty "Error validation" prop_error_validation
  , testProperty "Error transformation" prop_error_transformation
  , testProperty "Error accumulation" prop_error_accumulation
  , testProperty "Error contextualization" prop_error_contextualization
  , testProperty "Error correlation" prop_error_correlation
  , testProperty "Error hierarchy" prop_error_hierarchy
  ]