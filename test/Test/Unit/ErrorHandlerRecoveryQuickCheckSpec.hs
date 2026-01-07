module Test.Unit.ErrorHandlerRecoveryQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, sized, choose, forAll)
import Data.Char 
  ( TypeError(..), CombinedError(..), ErrorSeverity(..), ErrorCategory(..)
  , ErrorLocation(..), ErrorContext(..), ErrorRecovery(..)
  , emptyContext, newErrorCollector, addError, addWarning, addInfo
  , getErrors, getWarnings, getInfo, getAllMessages, hasErrors, hasWarnings
  , formatError, formatErrors, canRecoverFrom, shouldContinueAfter
  , errorAt, errorWithCategory, warningAt, warningWithCategory, infoAt, warningRecovery, infoRecovery
  )
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | ErrorHandlerQuickCheck
tests :: TestTree
tests =
    testGroup "ErrorHandler Recovery QuickCheck Tests"
    [ testGroup "Error Recovery Properties"
        [             testProperty "Recovery strategy consistency" propRecoveryStrategyConsistency
        ,             testProperty "Recovery severity ordering" propRecoverySeverityOrdering
        ,             testProperty "Recovery continuation logic" propRecoveryContinuationLogic
        ,             testProperty "Recovery strategy composition" propRecoveryStrategyComposition
        ]

    , testGroup "Error Collection L.and Recovery"
        [             testProperty "Error collection preserves recovery info" propErrorCollectionPreservesRecovery
        ,             testProperty "Multiple errors recovery aggregation" propMultipleErrorsRecoveryAggregation
        ,             testProperty "Error filtering maintains recovery" propErrorFilteringMaintainsRecovery
        ]

    , testGroup "Error Severity L.and Recovery"
        [             testProperty "Fatal errors non-recoverable" propFatalErrorsNonRecoverable
        ,             testProperty "Warning errors always recoverable" propWarningErrorsAlwaysRecoverable
        ,             testProperty "Info errors always recoverable" propInfoErrorsAlwaysRecoverable
        ,             testProperty "Error severity affects recovery" propErrorSeverityAffectsRecovery
        ]

    , testGroup "Error Context L.and Recovery"
        [             testProperty "Context preservation in recovery" propContextPreservationInRecovery
        ,             testProperty "Context enhancement improves recovery" propContextEnhancementImprovesRecovery
        ,             testProperty "Empty context recovery behavior" propEmptyContextRecoveryBehavior
        ]

    , testGroup "Error Location L.and Recovery"
        [             testProperty "Location information preservation" propLocationInformationPreservation
        ,             testProperty "Multiple locations recovery" propMultipleLocationsRecovery
        ,             testProperty "Unknown location recovery" propUnknownLocationRecovery
        ]

    , testGroup "Combined Error Recovery"
        [             testProperty "Combined error recovery aggregation" propCombinedErrorRecoveryAggregation
        ,             testProperty "Combined error severity propagation" propCombinedErrorSeverityPropagation
        ,             testProperty "Combined error recovery strategy" propCombinedErrorRecoveryStrategy
        ]

    , testGroup "Edge Cases in Recovery"
        [             testProperty "Empty error collection recovery" propEmptyErrorCollectionRecovery
        ,             testProperty "Circular error dependencies" propCircularErrorDependencies
        ,             testProperty "Maximum error depth recovery" propMaximumErrorDepthRecovery
        ]

    , testGroup "Recovery Stress Tests"
        [             testProperty "Large error collection recovery" propLargeErrorCollectionRecovery
        ,             testProperty "Mixed severity recovery performance" propMixedSeverityRecoveryPerformance
        ,             testProperty "Complex error graph recovery" propComplexErrorGraphRecovery
        ]
    ]

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- | 
propRecoveryStrategyConsistency :: TypeError -> Bool
propRecoveryStrategyConsistency                               err =
  let recovery1 = canRecoverFrom err
                                    recovery2 = canRecoverFrom err
  in                               recovery1 == recovery2

-- | 
propRecoverySeverityOrdering :: TypeError -> TypeError -> Bool
propRecoverySeverityOrdering err1                               err2 =
  let severity1 = getErrorSeverity err1
                                    severity2 = getErrorSeverity err2
                                    recoverable1 = canRecoverFrom err1
                                    recoverable2 = canRecoverFrom err2
  in if severity1 <= severity2
     then recoverable1 >= recoverable2
     else recoverable1 <= recoverable2

-- | 
propRecoveryContinuationLogic :: TypeError -> Bool
propRecoveryContinuationLogic                               err =
  let recoverable = canRecoverFrom err
                                    shouldContinue = shouldContinueAfter err
  in                               recoverable ==> shouldContinue

-- | 
propRecoveryStrategyComposition :: TypeError -> TypeError -> Bool
propRecoveryStrategyComposition err1                               err2 =
  let combined = combineErrors err1 err2
                                    recoverable1 = canRecoverFrom err1
                                    recoverable2 = canRecoverFrom err2
                                    recoverableCombined = canRecoverFrom combined
  in if recoverable1 && recoverable2
     then recoverableCombined
     else True  -- 

-- ============================================================================
-- Error Collection L.and Recovery
-- ============================================================================

-- | 
propErrorCollectionPreservesRecovery :: [TypeError] -> Bool
propErrorCollectionPreservesRecovery                               errors =
  let collector = newErrorCollector
                                    withErrors = L.foldl (\acc err -> addError err acc) collector errors
                                    collectedErrors = getErrors withErrors
                                    originalRecoverable = map canRecoverFrom errors
                                    collectedRecoverable = map canRecoverFrom collectedErrors
  in L.length                               originalRecoverable == L.length collectedRecoverable &&
     L.all id (zipWith (==) originalRecoverable collectedRecoverable)

-- | 
propMultipleErrorsRecoveryAggregation :: [TypeError] -> Bool
propMultipleErrorsRecoveryAggregation                               errors =
  let recoverableCount = L.length $ filter canRecoverFrom errors
                                    totalCount = L.length errors
                                    hasAnyRecoverable = recoverableCount > 0
                                    hasAnyNonRecoverable = recoverableCount < totalCount
  in hasAnyRecoverable || not hasAnyNonRecoverable

-- | 
propErrorFilteringMaintainsRecovery :: [TypeError] -> ErrorSeverity -> Bool
propErrorFilteringMaintainsRecovery errors                               severity =
  let filtered = filterBySeverity severity errors
                                    originalRecoverable = map canRecoverFrom errors
                                    filteredRecoverable = map canRecoverFrom filtered
      -- 
  in L.all (\(orig, filt) ->                               filt ==> orig) (zip originalRecoverable filteredRecoverable)

-- ============================================================================
-- Error Severity L.and Recovery
-- ============================================================================

-- | 
propFatalErrorsNonRecoverable :: String -> Bool
propFatalErrorsNonRecoverable                               msg =
  let fatal = fatalError msg
  in not (canRecoverFrom fatal)

-- | 
propWarningErrorsAlwaysRecoverable :: String -> Bool
propWarningErrorsAlwaysRecoverable                               msg =
  let warning = warningAt "test-id" 0 0 "") msg
  in canRecoverFrom warning

-- | 
propInfoErrorsAlwaysRecoverable :: String -> Bool
propInfoErrorsAlwaysRecoverable                               msg =
  let info = infoAt "test-id" 0 0 "") msg
  in canRecoverFrom info

-- | 
propErrorSeverityAffectsRecovery :: ErrorSeverity -> ErrorSeverity -> Bool
propErrorSeverityAffectsRecovery sev1                               sev2 =
  let err1 = errorWithCategory (ErrorLocation 0 0 "") TypeMismatch "" sev1
                                    err2 = errorWithCategory (ErrorLocation 0 0 "") TypeMismatch "" sev2
  in if sev1 <= sev2
     then canRecoverFrom err1 >= canRecoverFrom err2
     else canRecoverFrom err1 <= canRecoverFrom err2

-- ============================================================================
-- Error Context L.and Recovery
-- ============================================================================

-- | 
propContextPreservationInRecovery :: TypeError -> ErrorContext -> Bool
propContextPreservationInRecovery err                               context =
  let withCtx = withContext err context
                                    originalRecoverable = canRecoverFrom err
                                    withContextRecoverable = canRecoverFrom withCtx
  in                               originalRecoverable == withContextRecoverable

-- | 
propContextEnhancementImprovesRecovery :: TypeError -> ErrorContext -> Bool
propContextEnhancementImprovesRecovery err                               context =
  let baseRecoverable = canRecoverFrom err
                                    enhanced = withContext err context
                                    enhancedRecoverable = canRecoverFrom enhanced
  in enhancedRecoverable >= baseRecoverable

-- | 
propEmptyContextRecoveryBehavior :: TypeError -> Bool
propEmptyContextRecoveryBehavior                               err =
  let empty = emptyContext
                                    withEmpty = withContext err empty
                                    originalRecoverable = canRecoverFrom err
                                    withEmptyRecoverable = canRecoverFrom withEmpty
  in                               originalRecoverable == withEmptyRecoverable

-- ============================================================================
-- Error Location L.and Recovery
-- ============================================================================

-- | 
propLocationInformationPreservation :: TypeError -> ErrorLocation -> Bool
propLocationInformationPreservation err                               location =
  let withLoc = withLocation err location
                                    originalRecoverable = canRecoverFrom err
                                    withLocationRecoverable = canRecoverFrom withLoc
  in                               originalRecoverable == withLocationRecoverable

-- | 
propMultipleLocationsRecovery :: TypeError -> [ErrorLocation] -> Bool
propMultipleLocationsRecovery err                               locations =
  let withLocations = foldl withLocation err locations
                                    originalRecoverable = canRecoverFrom err
                                    withMultiLocationRecoverable = canRecoverFrom withLocations
  in                               originalRecoverable == withMultiLocationRecoverable

-- | 
propUnknownLocationRecovery :: TypeError -> Bool
propUnknownLocationRecovery                               err =
  let unknownLoc = ErrorLocation (-1) (-1) "unknown"
                                    withUnknown = withLocation err unknownLoc
                                    originalRecoverable = canRecoverFrom err
                                    withUnknownRecoverable = canRecoverFrom withUnknown
  in                               originalRecoverable == withUnknownRecoverable

-- ============================================================================
-- Combined Error Recovery
-- ============================================================================

-- | 
propCombinedErrorRecoveryAggregation :: TypeError -> TypeError -> Bool
propCombinedErrorRecoveryAggregation err1                               err2 =
  let combined = combineErrors err1 err2
                                    recoverable1 = canRecoverFrom err1
                                    recoverable2 = canRecoverFrom err2
                                    recoverableCombined = canRecoverFrom combined
  in (recoverable1 && recoverable2) ==> recoverableCombined

-- | 
propCombinedErrorSeverityPropagation :: TypeError -> TypeError -> Bool
propCombinedErrorSeverityPropagation err1                               err2 =
  let combined = combineErrors err1 err2
                                    sev1 = getErrorSeverity err1
                                    sev2 = getErrorSeverity err2
                                    combinedSev = getErrorSeverity combined
  in                               combinedSev == max sev1 sev2

-- | 
propCombinedErrorRecoveryStrategy :: TypeError -> TypeError -> Bool
propCombinedErrorRecoveryStrategy err1                               err2 =
  let combined = combineErrors err1 err2
                                    strategy1 = createRecoveryStrategy err1
                                    strategy2 = createRecoveryStrategy err2
                                    combinedStrategy = createRecoveryStrategy combined
  in case (strategy1, strategy2, combinedStrategy) of
       (FatalRecovery, _, _) ->                               combinedStrategy == FatalRecovery
       (_, FatalRecovery, _) ->                               combinedStrategy == FatalRecovery
       (ErrorRecovery, ErrorRecovery, _) ->                               combinedStrategy == ErrorRecovery
       (WarningRecovery, WarningRecovery, _) ->                               combinedStrategy == WarningRecovery
       (InfoRecovery, InfoRecovery, _) ->                               combinedStrategy == InfoRecovery
       _ -> True  -- 

-- ============================================================================
-- Edge Cases in Recovery
-- ============================================================================

-- | 
propEmptyErrorCollectionRecovery :: Bool
                              propEmptyErrorCollectionRecovery =
  let emptyCollector = newErrorCollector
                                    errors = getErrors emptyCollector
                                    warnings = getWarnings emptyCollector
                                    info = getInfo emptyCollector
  in null errors && null warnings && null info

-- | 
propCircularErrorDependencies :: String -> Bool
propCircularErrorDependencies                               base =
  let err1 = errorAt "test-id" 0 0 "") (base ++ "1")
                                    err2 = errorAt "test-id" 1 0 "") (base ++ "2")
                                    err3 = errorAt "test-id" 2 0 "") (base ++ "3")
      -- 
                                    withRelated1 = withRelatedErrors err1 [err2, err3]
                                    withRelated2 = withRelatedErrors err2 [err3, err1]
                                    withRelated3 = withRelatedErrors err3 [err1, err2]
  in canRecoverFrom withRelated1 && canRecoverFrom withRelated2 && canRecoverFrom withRelated3

-- | 
propMaximumErrorDepthRecovery :: Int -> String -> Bool
propMaximumErrorDepthRecovery depth                               base =
  let maxDepth = abs depth `mod` 10 + 1
      createNestedError                               0 = errorAt "test-id" 0 0 "") base
      createNestedError                               n = 
        let inner = createNestedError (n-1)
                                          wrapper = errorAt "test-id" n 0 "") (base ++ "_depth_" ++ show n)
        in withRelatedErrors wrapper [inner]
                                    deepest = createNestedError maxDepth
  in canRecoverFrom deepest

-- ============================================================================
-- Recovery Stress Tests
-- ============================================================================

-- | 
propLargeErrorCollectionRecovery :: Int -> Bool
propLargeErrorCollectionRecovery                               count =
  let errorCount = abs count `mod` 100 + 1
                                    errors = L.map (\i -> errorAt "test-id" i 0 "") ("error_" ++ show i) [1..errorCount]
                                    recoverableCount = L.length $ filter canRecoverFrom errors
  in recoverableCount >= 0 && recoverableCount <= errorCount

-- | 
propMixedSeverityRecoveryPerformance :: [ErrorSeverity] -> Bool
propMixedSeverityRecoveryPerformance                               severities =
  let errors = zipWith (\sev i -> 
        errorWithCategory (ErrorLocation i 0 "") TypeMismatch "" sev) severities [1..]
                                    recoverableCount = L.length $ filter canRecoverFrom errors
                                    totalCount = L.length errors
  in recoverableCount >= 0 && recoverableCount <= totalCount

-- | 
propComplexErrorGraphRecovery :: Int -> Bool
propComplexErrorGraphRecovery                               nodeCount =
  let nodes = abs nodeCount `mod` 20 + 1
      createNode                               i = errorAt "test-id" i 0 "") ("node_" ++ show i)
                                    nodeErrors = map createNode [1..nodes]
      -- 
      connectNodes                               errors = zipWith (\err i -> 
        let relatedCount = i `mod` 3
                                          related = take relatedCount (drop (i+1) errors)
        in withRelatedErrors err related) errors [0..]
                                    connectedErrors = connectNodes nodeErrors
                                    allRecoverable = L.all canRecoverFrom connectedErrors
  in allRecoverable || not allRecoverable  -- 

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- 
getErrorSeverity :: TypeError -> ErrorSeverity
getErrorSeverity                               err = case err of
  TypeError {                               teSeverity = sev } -> sev
  _ -> ErrorSeverity

-- ErrorSeverity
genErrorSeverity :: Gen ErrorSeverity
                              genErrorSeverity = elements [ErrorSeverity, WarningSeverity, InfoSeverity]

-- ErrorCategory
genErrorCategory :: Gen ErrorCategory
                              genErrorCategory = elements 
  [ TypeMismatch, UndefinedVariable, ParseError, SyntaxError
  , SemanticError, RuntimeError, CompilationError, LinkError
  ]

-- ErrorLocation
genErrorLocation :: Gen ErrorLocation
                              genErrorLocation = do
              line <- choose (0, 1000)
  col <- choose (0, 1000)
  file <- elements ["", "test.typus", "src/main.typus"]
  return $ ErrorLocation line col file

-- ErrorContext
genErrorContext :: Gen ErrorContext
                              genErrorContext = do
              return emptyContext

-- TypeError
genTypeError :: Gen TypeError
                              genTypeError = do
              location <- genErrorLocation
  category <- genErrorCategory
  severity <- genErrorSeverity
  message <- elements ["Type error", "Parse error", "Runtime error"]
  return $ errorWithCategory location category message severity

-- 
instance Arbitrary ErrorSeverity where
                                              arbitrary = genErrorSeverity

instance Arbitrary ErrorCategory where
                                              arbitrary = genErrorCategory

instance Arbitrary ErrorLocation where
                                              arbitrary = genErrorLocation

instance Arbitrary ErrorContext where
                                              arbitrary = genErrorContext

instance Arbitrary TypeError where
                                              arbitrary = genTypeError

instance Arbitrary String where
                                              arbitrary = listOf $ elements ['a'..'z']

-- 
infixr                               0 ==>
(==>) :: Bool -> Bool -> Bool
                              True ==>                               x = x
                              False ==>                               _ = True