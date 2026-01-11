{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Compiler.Errors.Core (
    -- Error types
    TypeError(..),
    CombinedError(..),
    ErrorSeverity(..),
    ErrorCategory(..),
    ErrorLocation(..),
    ErrorContext(..),
    emptyContext,
    ErrorRecovery(..),
    
    -- Helper functions for testing
    getErrorLine,
    getErrorColumn,

    -- Error collection and management
    ErrorCollector,
    newErrorCollector,
    addError,
    addWarning,
    addInfo,
    getErrors,
    getWarnings,
    getInfo,
    getAllMessages,
    hasErrors,
    hasWarnings,

    -- Error formatting
    formatError,
    formatErrors,
    formatErrorWithLocation,
    formatErrorsWithLocation,

    -- Error recovery
    canRecoverFrom,
    shouldContinueAfter,

    -- Error utilities
    errorAt,
    errorAtWithTimestamp,
    errorAtWithUTCTime,
    errorWithCategory,
    warningAt,
    warningWithCategory,
    infoAt,
    infoWithCategory,
    fatalError,
    fatalErrorWithCategory,
    errorWithSuggestions,
    withLocation,
    withContext,
    withSuggestions,
    withRelatedErrors,
    withTimestamp,
    withUTCTimestamp,
    wrapError,
    combineErrors,
    combinedErrorSeverity,
    filterCombinedErrorsBySeverity,

    -- Error filtering and analysis
    hasCategory,
    filterByCategory,
    filterBySeverity,
    getErrorStatistics,
    generateErrorReport,
    generateErrorReportWithTimestamp,
    generateErrorReportWithUTCTime,
    generateErrorReportIO,

    -- Timestamp utilities
    formatTimestamp,
    getCurrentTimestamp,

    -- Recovery strategy utilities
    createRecoveryStrategy,
    customRecovery,
    fatalRecovery,
    errorRecovery,
    warningRecovery,
    infoRecovery,

    -- Severity comparison utilities
    isAtLeast,
    severityPriority,
    compareSeverity
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Control.Monad.State (State, modify)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import qualified Data.Map.Strict as Map
import qualified Ownership.Common.Types as Own
import qualified Dependencies.TypeSystem as Dep

-- ============================================================================
-- Error Severity Levels
-- ============================================================================

data ErrorSeverity = Fatal | Error | Warning | Info
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Error priority for ordering and filtering (higher number = higher priority)
severityPriority :: ErrorSeverity -> Int
severityPriority Fatal = 100
severityPriority Error = 80
severityPriority Warning = 30
severityPriority Info = 10

-- Error level with sub-levels for finer granularity
data ErrorSubLevel
    = Critical                    -- Critical errors that stop execution
    | High                        -- High priority errors
    | Medium                      -- Medium priority errors
    | Low                         -- Low priority errors
    | Notification                -- Informational notifications
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Enhanced severity with sub-levels
data DetailedSeverity = DetailedSeverity
    { baseSeverity :: ErrorSeverity
    , subLevel :: ErrorSubLevel
    , customLevel :: Maybe String      -- Custom level names
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Convert detailed severity to basic severity
_toBasicSeverity :: DetailedSeverity -> ErrorSeverity
_toBasicSeverity = baseSeverity

-- Get priority for detailed severity
detailedSeverityPriority :: DetailedSeverity -> Int
detailedSeverityPriority ds = severityPriority (baseSeverity ds) + subLevelPriority (subLevel ds)
  where
    subLevelPriority :: ErrorSubLevel -> Int
    subLevelPriority Critical = 50
    subLevelPriority High = 30
    subLevelPriority Medium = 15
    subLevelPriority Low = 5
    subLevelPriority Notification = 0

-- Severity comparison functions
compareSeverity :: ErrorSeverity -> ErrorSeverity -> Ordering
compareSeverity s1 s2 = compare (severityPriority s1) (severityPriority s2)

_compareDetailedSeverity :: DetailedSeverity -> DetailedSeverity -> Ordering
_compareDetailedSeverity d1 d2 = compare (detailedSeverityPriority d1) (detailedSeverityPriority d2)

-- Severity predicates
_isFatal :: ErrorSeverity -> Bool
_isFatal Fatal = True
_isFatal _ = False

_isError :: ErrorSeverity -> Bool
_isError Error = True
_isError _ = False

_isWarning :: ErrorSeverity -> Bool
_isWarning Warning = True
_isWarning _ = False

_isInfo :: ErrorSeverity -> Bool
_isInfo Info = True
_isInfo _ = False

isAtLeast :: ErrorSeverity -> ErrorSeverity -> Bool
isAtLeast severity minSeverity = compareSeverity severity minSeverity /= LT

-- Severity level predicates for detailed severity
_isCritical :: DetailedSeverity -> Bool
_isCritical ds = subLevel ds == Critical

_isHigh :: DetailedSeverity -> Bool
_isHigh ds = subLevel ds == High

_isMedium :: DetailedSeverity -> Bool
_isMedium ds = subLevel ds == Medium

_isLow :: DetailedSeverity -> Bool
_isLow ds = subLevel ds == Low

_isNotification :: DetailedSeverity -> Bool
_isNotification ds = subLevel ds == Notification

-- Create common detailed severity levels
_criticalFatal :: DetailedSeverity
_criticalFatal = DetailedSeverity Fatal Critical Nothing

_highFatal :: DetailedSeverity
_highFatal = DetailedSeverity Fatal High Nothing

_mediumFatal :: DetailedSeverity
_mediumFatal = DetailedSeverity Fatal Medium Nothing

_highError :: DetailedSeverity
_highError = DetailedSeverity Error High Nothing

_mediumError :: DetailedSeverity
_mediumError = DetailedSeverity Error Medium Nothing

_lowError :: DetailedSeverity
_lowError = DetailedSeverity Error Low Nothing

_highWarning :: DetailedSeverity
_highWarning = DetailedSeverity Warning High Nothing

_mediumWarning :: DetailedSeverity
_mediumWarning = DetailedSeverity Warning Medium Nothing

_lowWarning :: DetailedSeverity
_lowWarning = DetailedSeverity Warning Low Nothing

_infoNotification :: DetailedSeverity
_infoNotification = DetailedSeverity Info Notification Nothing

-- Create custom detailed severity
_customDetailedSeverity :: ErrorSeverity -> ErrorSubLevel -> String -> DetailedSeverity
_customDetailedSeverity base sub customName = DetailedSeverity base sub (Just customName)

-- Severity groupings
_isRecoverable :: ErrorSeverity -> Bool
_isRecoverable Fatal = False
_isRecoverable _ = True

_isUserActionRequired :: ErrorSeverity -> Bool
_isUserActionRequired Fatal = True
_isUserActionRequired Error = True
_isUserActionRequired _ = False

_isSystemIssue :: ErrorSeverity -> Bool
_isSystemIssue Fatal = True
_isSystemIssue Error = True
_isSystemIssue _ = False

-- Severity-based filtering
_filterBySeverityRange :: ErrorSeverity -> ErrorSeverity -> [TypeError] -> [TypeError]
_filterBySeverityRange minSeverity maxSeverity errors =
    filter (\e -> isAtLeast minSeverity (severity e) && not (isAtLeast (succSeverity maxSeverity) (severity e))) errors
  where
    succSeverity Fatal = Fatal  -- No higher than Fatal
    succSeverity Error = Fatal
    succSeverity Warning = Error
    succSeverity Info = Warning

_filterByDetailedPriority :: Int -> Int -> [DetailedSeverity] -> [DetailedSeverity]
_filterByDetailedPriority minPriority maxPriority =
    filter (\ds -> let p = detailedSeverityPriority ds in p >= minPriority && p <= maxPriority)

-- Severity statistics
_severityDistribution :: [TypeError] -> Map.Map ErrorSeverity Int
_severityDistribution errors = Map.fromList $
    [ (Fatal, length $ filterBySeverity Fatal errors)
    , (Error, length $ filterBySeverity Error errors)
    , (Warning, length $ filterBySeverity Warning errors)
    , (Info, length $ filterBySeverity Info errors)
    ]

_detailedSeverityDistribution :: [DetailedSeverity] -> Map.Map DetailedSeverity Int
_detailedSeverityDistribution severities = Map.fromListWith (+) $
    map (\s -> (s, 1)) severities

-- Get most severe error
_getMostSevere :: [TypeError] -> Maybe TypeError
_getMostSevere [] = Nothing
_getMostSevere errors = Just $ maximumBy (severityPriority . severity) errors
  where
    maximumBy :: Ord b => (a -> b) -> [a] -> a
    maximumBy f = foldl1 (\x y -> if f x >= f y then x else y)

-- Get least severe error
_getLeastSevere :: [TypeError] -> Maybe TypeError
_getLeastSevere [] = Nothing
_getLeastSevere errors = Just $ minimumBy (severityPriority . severity) errors
  where
    minimumBy :: Ord b => (a -> b) -> [a] -> a
    minimumBy f = foldl1 (\x y -> if f x <= f y then x else y)

-- ============================================================================
-- Error Location Tracking
-- ============================================================================

data ErrorLocation = ErrorLocation
    { filePath :: Maybe String
    , line :: Int
    , column :: Int
    , endLine :: Maybe Int
    , endColumn :: Maybe Int
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Default location (unknown)
_unknownLocation :: ErrorLocation
_unknownLocation = ErrorLocation Nothing 0 0 Nothing Nothing

-- Helper functions to access ErrorLocation fields (for testing)
getErrorLine :: ErrorLocation -> Int
getErrorLine = line

getErrorColumn :: ErrorLocation -> Int  
getErrorColumn = column

-- Format a timestamp using the default error-reporting format
formatTimestamp :: UTCTime -> String
formatTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%3q"

-- Get current timestamp for error tracking (in IO)
getCurrentTimestamp :: IO String
getCurrentTimestamp = formatTimestamp <$> getCurrentTime

-- Create location with just line and column
_atLocation :: Int -> Int -> ErrorLocation
_atLocation lineNum col = ErrorLocation Nothing lineNum col Nothing Nothing

-- Create location with file path
_atFileLocation :: String -> Int -> Int -> ErrorLocation
_atFileLocation file lineNum col = ErrorLocation (Just file) lineNum col Nothing Nothing

-- Create location with range
_atRange :: Int -> Int -> Int -> Int -> ErrorLocation
_atRange startLine startCol endLineNum endCol =
    ErrorLocation Nothing startLine startCol (Just endLineNum) (Just endCol)

-- ============================================================================
-- Error Context Information
-- ============================================================================

data ErrorContext = ErrorContext
    { contextCode :: Maybe String
    , contextFunction :: Maybe String
    , contextVariable :: Maybe String
    , contextType :: Maybe String
    , contextAdditional :: [(String, String)]
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

emptyContext :: ErrorContext
emptyContext = ErrorContext Nothing Nothing Nothing Nothing []

-- ============================================================================
-- Error Recovery Strategy
-- ============================================================================

data ErrorRecovery = RecoveryStrategy
    { canRecover :: Bool
    , shouldContinue :: Bool
    , recoveryAction :: Maybe String
    , recoveryHint :: Maybe String
    , recoveryCost :: Int              -- Cost of recovery (0-100)
    , recoveryConfidence :: Float        -- Confidence in recovery (0.0-1.0)
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Recovery strategies with enhanced information
fatalRecovery :: ErrorRecovery
fatalRecovery = RecoveryStrategy False False Nothing Nothing 100 0.0

errorRecovery :: ErrorRecovery
errorRecovery = RecoveryStrategy True True Nothing Nothing 50 0.7

warningRecovery :: ErrorRecovery
warningRecovery = RecoveryStrategy True True Nothing Nothing 10 0.9

infoRecovery :: ErrorRecovery
infoRecovery = RecoveryStrategy True True Nothing Nothing 0 1.0

-- Create custom recovery strategy
customRecovery :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> ErrorRecovery
customRecovery canRec shouldCont recAction recHint cost confidence = RecoveryStrategy
    canRec shouldCont recAction recHint cost confidence

-- Recovery strategy for specific scenarios
_retryRecovery :: Int -> ErrorRecovery
_retryRecovery maxAttempts = RecoveryStrategy
    True True (Just $ "Retry operation (max " ++ show maxAttempts ++ " attempts)")
    (Just "Consider increasing timeout or checking network connectivity")
    (20 * maxAttempts) 0.8

_skipRecovery :: ErrorRecovery
_skipRecovery = RecoveryStrategy
    True True (Just "Skip current operation")
    (Just "This operation can be safely skipped")
    5 0.95

_fallbackRecovery :: String -> ErrorRecovery
_fallbackRecovery fallbackMsg = RecoveryStrategy
    True True (Just $ "Use fallback: " ++ fallbackMsg)
    (Just "Using alternative implementation")
    15 0.75

_manualRecovery :: String -> ErrorRecovery
_manualRecovery instruction = RecoveryStrategy
    True False (Just "Manual intervention required")
    (Just instruction)
    80 0.5

-- Recovery strategy combinators
_sequenceRecovery :: ErrorRecovery -> ErrorRecovery -> ErrorRecovery
_sequenceRecovery r1 r2 = RecoveryStrategy
    (canRecover r1 && canRecover r2)
    (shouldContinue r1 && shouldContinue r2)
    (case (recoveryAction r1, recoveryAction r2) of
        (Just a1, Just a2) -> Just $ a1 ++ "; then " ++ a2
        (Just a1, Nothing) -> Just a1
        (Nothing, Just a2) -> Just a2
        _ -> Nothing)
    (case (recoveryHint r1, recoveryHint r2) of
        (Just h1, Just h2) -> Just $ h1 ++ "; " ++ h2
        (Just h1, Nothing) -> Just h1
        (Nothing, Just h2) -> Just h2
        _ -> Nothing)
    (recoveryCost r1 + recoveryCost r2)
    ((recoveryConfidence r1 + recoveryConfidence r2) / 2)

_chooseBestRecovery :: [ErrorRecovery] -> ErrorRecovery
_chooseBestRecovery [] = fatalRecovery
_chooseBestRecovery strategies = foldl1 chooseBest strategies
  where
    chooseBest r1 r2
        | not (canRecover r1) = r2
        | not (canRecover r2) = r1
        | recoveryConfidence r1 > recoveryConfidence r2 = r1
        | recoveryConfidence r2 > recoveryConfidence r1 = r2
        | recoveryCost r1 < recoveryCost r2 = r1
        | otherwise = r2

-- Recovery context for managing recovery operations
data RecoveryContext = RecoveryContext
    { recoveryAttempts :: Int
    , maxRecoveryAttempts :: Int
    , recoveryHistory :: [(ErrorRecovery, Bool)]  -- Recovery strategies and their success
    , currentStrategy :: Maybe ErrorRecovery
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Initial recovery context
_initialRecoveryContext :: Int -> RecoveryContext
_initialRecoveryContext maxAttempts = RecoveryContext
    { recoveryAttempts = 0
    , maxRecoveryAttempts = maxAttempts
    , recoveryHistory = []
    , currentStrategy = Nothing
    }

-- Add recovery attempt to context
_addRecoveryAttempt :: ErrorRecovery -> Bool -> RecoveryContext -> RecoveryContext
_addRecoveryAttempt strategy success recoveryCtx = recoveryCtx
    { recoveryAttempts = recoveryAttempts recoveryCtx + 1
    , recoveryHistory = (strategy, success) : recoveryHistory recoveryCtx
    , currentStrategy = Just strategy
    }

-- Check if more recovery attempts are allowed
_canRecoverMore :: RecoveryContext -> Bool
_canRecoverMore recoveryCtx = recoveryAttempts recoveryCtx < maxRecoveryAttempts recoveryCtx

-- Get successful recovery strategies
_getSuccessfulRecoveries :: RecoveryContext -> [ErrorRecovery]
_getSuccessfulRecoveries recoveryCtx = map fst $ filter snd (recoveryHistory recoveryCtx)

-- Get failed recovery strategies
_getFailedRecoveries :: RecoveryContext -> [ErrorRecovery]
_getFailedRecoveries recoveryCtx = map fst $ filter (not . snd) (recoveryHistory recoveryCtx)

-- Calculate recovery success rate
_recoverySuccessRate :: RecoveryContext -> Float
_recoverySuccessRate recoveryCtx
    | null history = 0.0
    | otherwise = fromIntegral (length $ filter snd history) / fromIntegral (length history)
  where
    history = recoveryHistory recoveryCtx

-- Generate recovery summary
_recoverySummary :: RecoveryContext -> String
_recoverySummary recoveryCtx =
    let successRate = _recoverySuccessRate recoveryCtx
        successful = _getSuccessfulRecoveries recoveryCtx
        failed = _getFailedRecoveries recoveryCtx
        successPct :: Int
        successPct = round (successRate * 100)
    in unlines $
        [ "Recovery Summary:"
        , "================="
        , "Attempts: " ++ show (recoveryAttempts recoveryCtx) ++ "/" ++ show (maxRecoveryAttempts recoveryCtx)
        , "Success rate: " ++ show successPct ++ "%"
        , "Successful strategies: " ++ show (length successful)
        , "Failed strategies: " ++ show (length failed)
        , if _canRecoverMore recoveryCtx then "More recovery attempts allowed" else "No more recovery attempts allowed"
        ]

-- ============================================================================
-- Enhanced Error Type
-- ============================================================================

data TypeError = TypeError
    { errorId :: String
    , severity :: ErrorSeverity
    , category :: ErrorCategory
    , message :: Text
    , location :: ErrorLocation
    , context :: ErrorContext
    , recovery :: ErrorRecovery
    , suggestions :: [Text]
    , relatedErrors :: [TypeError]
    , errorChain :: [TypeError]  -- For error wrapping and chaining
    , timestamp :: Maybe String  -- For debugging and logging
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ============================================================================
-- Combined Analyzer Errors
-- ============================================================================

data CombinedError
    = OwnershipErrorCombined ErrorSeverity Own.OwnershipError
    | DependentTypeErrorCombined ErrorSeverity Dep.DependentTypeError
    | IntegrationError String ErrorSeverity
    | CrossAnalyzerError String ErrorSeverity [CombinedError]
    deriving (Show, Eq)

combinedErrorSeverity :: CombinedError -> ErrorSeverity
combinedErrorSeverity (OwnershipErrorCombined sev _) = sev
combinedErrorSeverity (DependentTypeErrorCombined sev _) = sev
combinedErrorSeverity (IntegrationError _ sev) = sev
combinedErrorSeverity (CrossAnalyzerError _ sev _) = sev

filterCombinedErrorsBySeverity :: ErrorSeverity -> [CombinedError] -> [CombinedError]
filterCombinedErrorsBySeverity minimumSeverity =
    filter (\err -> isAtLeast minimumSeverity (combinedErrorSeverity err))

-- Error categories for better organization
data ErrorCategory
    = TypeChecking
    | Ownership
    | Parsing
    | Semantic
    | Runtime
    | Constraint
    | Inference
    | Integration
    | Unknown
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- ============================================================================
-- Error Collector Monad
-- ============================================================================

type ErrorCollector = State [TypeError]

newErrorCollector :: ErrorCollector ()
newErrorCollector = return ()

-- Add errors to collector
addError :: TypeError -> ErrorCollector ()
addError err = modify (err :)

addWarning :: TypeError -> ErrorCollector ()
addWarning err = addError err { severity = Warning }

addInfo :: TypeError -> ErrorCollector ()
addInfo err = addError err { severity = Info }

-- Get errors from collector
getErrors :: [TypeError] -> [TypeError]
getErrors = filter (\e -> severity e == Error || severity e == Fatal)

getWarnings :: [TypeError] -> [TypeError]
getWarnings = filter (\e -> severity e == Warning)

getInfo :: [TypeError] -> [TypeError]
getInfo = filter (\e -> severity e == Info)

getAllMessages :: [TypeError] -> [TypeError]
getAllMessages = id

hasErrors :: [TypeError] -> Bool
hasErrors = not . null . getErrors

hasWarnings :: [TypeError] -> Bool
hasWarnings = not . null . getWarnings

-- ============================================================================
-- Error Formatting
-- ============================================================================

-- Format single error without location
formatError :: TypeError -> String
formatError err =
    let severityStr :: String
        severityStr = case severity err of
          Fatal -> "FATAL"
          Error -> "ERROR"
          Warning -> "WARNING"
          Info -> "INFO"
        categoryStr = "[" ++ show (category err) ++ "]"
        msg = T.unpack (message err)
        suggestionsStr = if null (suggestions err)
                         then ""
                         else "\nSuggestions:\n" ++ unlines (map ("  - " ++) (map T.unpack (suggestions err)))
        chainStr = if null (errorChain err)
                   then ""
                   else "\nError Chain:\n" ++ unlines (map ("  " ++) (map formatError (errorChain err)))
    in "[" ++ severityStr ++ "] " ++ categoryStr ++ " " ++ msg ++ suggestionsStr ++ chainStr

-- Format single error with location
formatErrorWithLocation :: TypeError -> String
formatErrorWithLocation err =
    let locStr = formatLocation (location err)
        contextStr = formatContext (context err)
        timestampStr = maybe "" (\ts -> "[" ++ ts ++ "] ") (timestamp err)
        baseMsg = formatError err
    in timestampStr ++ locStr ++ baseMsg ++ contextStr

-- Format multiple errors
formatErrors :: [TypeError] -> String
formatErrors = intercalate "\n" . map formatError . sortBySeverity

-- Format multiple errors with locations
formatErrorsWithLocation :: [TypeError] -> String
formatErrorsWithLocation = intercalate "\n" . map formatErrorWithLocation . sortBySeverity

-- Helper functions
formatLocation :: ErrorLocation -> String
formatLocation loc =
    let fileStr = case filePath loc of
          Just file -> file ++ ":"
          Nothing -> ""
        lineStr = if line loc > 0 then show (line loc) else "?"
        colStr = if column loc > 0 then show (column loc) else "?"
        rangeStr = case (endLine loc, endColumn loc) of
          (Just endL, Just endC) -> "-" ++ show endL ++ ":" ++ show endC
          _ -> ""
    in fileStr ++ lineStr ++ ":" ++ colStr ++ rangeStr ++ ": "

formatContext :: ErrorContext -> String
formatContext ctx =
    let parts = mapMaybe (\(label, value) -> if null value then Nothing else Just (label ++ ": " ++ value))
                  [("function", maybe "" id (contextFunction ctx)),
                   ("variable", maybe "" id (contextVariable ctx)),
                   ("type", maybe "" id (contextType ctx))]
        codeStr = case contextCode ctx of
          Just code -> "\nCode:\n" ++ code
          Nothing -> ""
        additionalStr = if null (contextAdditional ctx) then ""
                       else "\nAdditional Info:\n" ++ unlines (map (\(k,v) -> "  " ++ k ++ ": " ++ v) (contextAdditional ctx))
    in if null parts && null codeStr && null additionalStr
       then ""
       else "\nContext: " ++ intercalate ", " parts ++ codeStr ++ additionalStr

sortBySeverity :: [TypeError] -> [TypeError]
sortBySeverity = sortBy (comparing severity)

-- ============================================================================
-- Error Recovery Functions
-- ============================================================================

canRecoverFrom :: TypeError -> Bool
canRecoverFrom = canRecover . recovery

shouldContinueAfter :: TypeError -> Bool
shouldContinueAfter = shouldContinue . recovery

-- ============================================================================
-- Error Construction Utilities
-- ============================================================================

-- Create error at specific location
errorAt :: String -> Text -> ErrorLocation -> TypeError
errorAt errId msg loc = TypeError
    { errorId = errId
    , severity = Error
    , category = Unknown
    , message = msg
    , location = loc
    , context = emptyContext
    , recovery = errorRecovery
    , suggestions = []
    , relatedErrors = []
    , errorChain = []
    , timestamp = Nothing
    }

-- Create error at specific location with provided timestamp
errorAtWithTimestamp :: String -> String -> Text -> ErrorLocation -> TypeError
errorAtWithTimestamp ts errId msg loc = (errorAt errId msg loc) { timestamp = Just ts }

-- Create error at specific location with a UTCTime timestamp
errorAtWithUTCTime :: UTCTime -> String -> Text -> ErrorLocation -> TypeError
errorAtWithUTCTime time errId msg loc = errorAtWithTimestamp (formatTimestamp time) errId msg loc

-- Attach or override timestamp on an error
withTimestamp :: String -> TypeError -> TypeError
withTimestamp ts err = err { timestamp = Just ts }

-- Attach or override timestamp on an error using UTCTime
withUTCTimestamp :: UTCTime -> TypeError -> TypeError
withUTCTimestamp time err = withTimestamp (formatTimestamp time) err

-- Create error with category
errorWithCategory :: String -> ErrorCategory -> Text -> ErrorLocation -> TypeError
errorWithCategory errId errCategory msg loc = (errorAt errId msg loc) { category = errCategory }

warningAt :: String -> Text -> ErrorLocation -> TypeError
warningAt errId msg loc = (errorAt errId msg loc) { severity = Warning }

warningWithCategory :: String -> ErrorCategory -> Text -> ErrorLocation -> TypeError
warningWithCategory errId errCategory msg loc = (errorWithCategory errId errCategory msg loc) { severity = Warning }

infoAt :: String -> Text -> ErrorLocation -> TypeError
infoAt errId msg loc = (errorAt errId msg loc) { severity = Info }

infoWithCategory :: String -> ErrorCategory -> Text -> ErrorLocation -> TypeError
infoWithCategory errId errCategory msg loc = (errorWithCategory errId errCategory msg loc) { severity = Info }

-- Add location to existing error
withLocation :: TypeError -> ErrorLocation -> TypeError
withLocation err loc = err { location = loc }

-- Add context to existing error
withContext :: TypeError -> ErrorContext -> TypeError
withContext err ctx = err { context = ctx }

-- Combine multiple errors
combineErrors :: [TypeError] -> [TypeError]
combineErrors = concatMap expandRelatedErrors
  where
    expandRelatedErrors err = err : relatedErrors err

-- Wrap an error with additional context
wrapError :: Text -> TypeError -> TypeError
wrapError wrapperMsg innerError = innerError
    { message = wrapperMsg <> ": " <> message innerError
    , errorChain = innerError : errorChain innerError
    }

-- Add suggestions to an error
withSuggestions :: [Text] -> TypeError -> TypeError
withSuggestions suggestionsList err = err { suggestions = suggestionsList ++ suggestions err }

-- Add related errors
withRelatedErrors :: [TypeError] -> TypeError -> TypeError
withRelatedErrors relatedList err = err { relatedErrors = relatedList ++ relatedErrors err }

-- Create error with suggestions
errorWithSuggestions :: String -> Text -> [Text] -> ErrorLocation -> TypeError
errorWithSuggestions errId msg suggestionsList loc =
    (errorAt errId msg loc) { suggestions = suggestionsList }

-- Check if error has specific category
hasCategory :: ErrorCategory -> TypeError -> Bool
hasCategory cat err = cat == category err

-- Filter errors by category
filterByCategory :: ErrorCategory -> [TypeError] -> [TypeError]
filterByCategory errCategory = filter (hasCategory errCategory)

-- Filter errors by severity
filterBySeverity :: ErrorSeverity -> [TypeError] -> [TypeError]
filterBySeverity target = filter (\e -> severity e == target)

-- Get error statistics
getErrorStatistics :: [TypeError] -> Map.Map String Int
getErrorStatistics errors = Map.fromList
    [ ("total", length errors)
    , ("fatal", length $ filterBySeverity Fatal errors)
    , ("errors", length $ filterBySeverity Error errors)
    , ("warnings", length $ filterBySeverity Warning errors)
    , ("info", length $ filterBySeverity Info errors)
    , ("typeChecking", length $ filterByCategory TypeChecking errors)
    , ("ownership", length $ filterByCategory Ownership errors)
    , ("parsing", length $ filterByCategory Parsing errors)
    , ("semantic", length $ filterByCategory Semantic errors)
    , ("runtime", length $ filterByCategory Runtime errors)
    , ("constraint", length $ filterByCategory Constraint errors)
    , ("inference", length $ filterByCategory Inference errors)
    , ("integration", length $ filterByCategory Integration errors)
    , ("unknown", length $ filterByCategory Unknown errors)
    ]

-- Create comprehensive error report
generateErrorReport :: [TypeError] -> String
generateErrorReport = generateErrorReportWithTimestamp Nothing

generateErrorReportWithTimestamp :: Maybe String -> [TypeError] -> String
generateErrorReportWithTimestamp maybeTimestamp errors =
    let stats = getErrorStatistics errors
        formattedErrors = formatErrorsWithLocation errors
        header :: [String]
        header =
            [ "Error Report"
            , "============"
            ]
        timestampLines = maybe [] (\ts -> ["Generated at: " ++ ts]) maybeTimestamp
    in unlines $
        header ++
        timestampLines ++
        [ ""
        , "Statistics:"
        ] ++
        map (\(key, count) -> "  " ++ key ++ ": " ++ show count) (Map.toList stats) ++
        [ ""
        , "Detailed Errors:"
        , "---------------"
        , formattedErrors
        ]

generateErrorReportWithUTCTime :: UTCTime -> [TypeError] -> String
generateErrorReportWithUTCTime time =
    generateErrorReportWithTimestamp (Just (formatTimestamp time))

-- Generate an error report including the current timestamp
generateErrorReportIO :: [TypeError] -> IO String
generateErrorReportIO errors = do
    ts <- getCurrentTimestamp
    pure $ generateErrorReportWithTimestamp (Just ts) errors

-- Enhanced error recovery strategies
createRecoveryStrategy :: Bool -> Bool -> Maybe String -> Maybe String -> ErrorRecovery
createRecoveryStrategy canRec shouldCont recAction recHint = RecoveryStrategy canRec shouldCont recAction recHint 50 0.5

-- Create fatal error
fatalError :: String -> Text -> ErrorLocation -> TypeError
fatalError errId msg loc = (errorAt errId msg loc)
    { severity = Fatal
    , recovery = fatalRecovery
    }

-- Create fatal error with category
fatalErrorWithCategory :: String -> ErrorCategory -> Text -> ErrorLocation -> TypeError
fatalErrorWithCategory errId errCategory msg loc = (errorWithCategory errId errCategory msg loc)
    { severity = Fatal
    , recovery = fatalRecovery
    }