{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Compiler.Errors.Core (
    -- Re-export from Types module
    module Compiler.Errors.Types,
    
    -- Error types
    TypeError(..),
    
    -- Helper functions for testing
    getErrorLine,
    getErrorColumn,
    
    -- Accessor functions for CompilerError compatibility
    errorPhase,
    errorCategory,
    errorSeverity,
    errorMessage,
    
    -- Utility functions
    unknownLocation,
    chooseBestRecovery,

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
    compareSeverity,
    sortBySeverity
) where

import Compiler.Errors.Types
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

-- ============================================================================
-- Error Severity Utilities
-- ============================================================================

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



-- Severity comparison functions
compareSeverity :: ErrorSeverity -> ErrorSeverity -> Ordering
compareSeverity s1 s2 = compare (severityPriority s1) (severityPriority s2)

isAtLeast :: ErrorSeverity -> ErrorSeverity -> Bool
isAtLeast severityVal minSeverity = compareSeverity severityVal minSeverity /= LT









-- ============================================================================
-- Error Location Utilities
-- ============================================================================



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



-- ============================================================================
-- Error Context Utilities
-- ============================================================================

-- ============================================================================
-- Error Recovery Utilities
-- ============================================================================

-- Recovery strategies with enhanced information
fatalRecovery :: ErrorRecovery
fatalRecovery = ErrorRecovery False False Nothing Nothing 100 0.0

errorRecovery :: ErrorRecovery
errorRecovery = ErrorRecovery True True Nothing Nothing 50 0.7

warningRecovery :: ErrorRecovery
warningRecovery = ErrorRecovery True True Nothing Nothing 10 0.9

infoRecovery :: ErrorRecovery
infoRecovery = ErrorRecovery True True Nothing Nothing 0 1.0

-- Create custom recovery strategy
customRecovery :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> ErrorRecovery
customRecovery canRec shouldCont recAction recHint cost confidence = ErrorRecovery
    canRec shouldCont recAction recHint cost confidence



-- Recovery strategy combinators
_sequenceRecovery :: ErrorRecovery -> ErrorRecovery -> ErrorRecovery
_sequenceRecovery r1 r2 = ErrorRecovery
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
-- CombinedError Utilities
-- ============================================================================

combinedErrorSeverity :: CombinedError -> ErrorSeverity
combinedErrorSeverity (OwnershipErrorCombined sev _) = sev
combinedErrorSeverity (DependentTypeErrorCombined sev _) = sev
combinedErrorSeverity (IntegrationError _ sev) = sev
combinedErrorSeverity (CrossAnalyzerError _ sev _) = sev

filterCombinedErrorsBySeverity :: ErrorSeverity -> [CombinedError] -> [CombinedError]
filterCombinedErrorsBySeverity minimumSeverity =
    filter (\err -> isAtLeast minimumSeverity (combinedErrorSeverity err))

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
        idStr = "[" ++ errorId err ++ "]"
        msg = T.unpack (message err)
        locStr = formatLocation (location err)
        suggestionsStr = if null (suggestions err)
                         then ""
                         else "\nSuggestions:\n" ++ unlines (map ("  - " ++) (map T.unpack (suggestions err)))
        chainStr = if null (errorChain err)
                   then ""
                   else "\nError Chain:\n" ++ unlines (map ("  " ++) (map formatError (errorChain err)))
    in locStr ++ "[" ++ severityStr ++ "] " ++ categoryStr ++ " " ++ idStr ++ " " ++ msg ++ suggestionsStr ++ chainStr

-- Format single error with location
formatErrorWithLocation :: TypeError -> String
formatErrorWithLocation err =
    let locStr = formatLocation (location err)
        contextStr = formatContext (context err)
        timestampStr = maybe "" (\ts -> "[" ++ ts ++ "] ") (timestamp err)
        severityStr :: String
        severityStr = case severity err of
          Fatal -> "FATAL"
          Error -> "ERROR"
          Warning -> "WARNING"
          Info -> "INFO"
        categoryStr = "[" ++ show (category err) ++ "]"
        idStr = "[" ++ errorId err ++ "]"
        msg = T.unpack (message err)
        suggestionsStr = if null (suggestions err)
                         then ""
                         else "\nSuggestions:\n" ++ unlines (map ("  - " ++) (map T.unpack (suggestions err)))
        chainStr = if null (errorChain err)
                   then ""
                   else "\nError Chain:\n" ++ unlines (map ("  " ++) (map formatError (errorChain err)))
    in timestampStr ++ locStr ++ "[" ++ severityStr ++ "] " ++ categoryStr ++ " " ++ idStr ++ " " ++ msg ++ suggestionsStr ++ chainStr ++ contextStr

-- Format multiple errors
formatErrors :: [TypeError] -> String
formatErrors = intercalate "\n" . map formatError . sortByLocation
  where
    sortByLocation = sortBy (\e1 e2 -> 
      let loc1 = location e1
          loc2 = location e2
          line1 = line loc1
          line2 = line loc2
          col1 = column loc1
          col2 = column loc2
      in if line1 == line2
         then compare col1 col2
         else compare line1 line2)

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
sortBySeverity = sortBy (comparing (negate . severityPriority . severity))

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
errorAt :: String -> ErrorSeverity -> Text -> ErrorLocation -> TypeError
errorAt errId sev msg loc = TypeError
    { errorId = errId
    , severity = sev
    , category = Unknown
    , message = msg
    , location = loc
    , context = emptyContext
    , recovery = case sev of
                    Fatal -> fatalRecovery
                    Error -> errorRecovery
                    Warning -> warningRecovery
                    Info -> infoRecovery
    , suggestions = []
    , relatedErrors = []
    , errorChain = []
    , timestamp = Nothing
    }

-- Create error at specific location with provided timestamp
errorAtWithTimestamp :: String -> String -> Text -> ErrorLocation -> TypeError
errorAtWithTimestamp ts errId msg loc = (errorAt errId Error msg loc) { timestamp = Just ts }

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
errorWithCategory errId errCategory msg loc = (errorAt errId Error msg loc) { category = errCategory }

warningAt :: String -> Text -> ErrorLocation -> TypeError
warningAt errId msg loc = errorAt errId Warning msg loc

warningWithCategory :: String -> ErrorCategory -> Text -> ErrorLocation -> TypeError
warningWithCategory errId errCategory msg loc = (errorWithCategory errId errCategory msg loc) { severity = Warning }

infoAt :: String -> Text -> ErrorLocation -> TypeError
infoAt errId msg loc = errorAt errId Info msg loc

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
errorWithSuggestions :: String -> ErrorSeverity -> [Text] -> ErrorLocation -> TypeError
errorWithSuggestions errId sev suggestionsList loc =
    (errorAt errId sev (T.pack "test") loc) { suggestions = suggestionsList }

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
    time <- getCurrentTime
    return $ generateErrorReportWithUTCTime time errors

-- ============================================================================
-- Accessor Functions for CompilerError Compatibility
-- ============================================================================

-- Extract phase from TypeError (for compatibility with CompilerError)
errorPhase :: TypeError -> String
errorPhase err = case category err of
    Parsing -> "Parsing"
    TypeChecking -> "TypeChecking"
    Ownership -> "Ownership"
    Semantic -> "Semantic"
    Runtime -> "Runtime"
    Constraint -> "Constraint"
    Inference -> "Inference"
    Integration -> "Integration"
    Unknown -> "Unknown"

-- Extract category from TypeError (alias for category field)
errorCategory :: TypeError -> ErrorCategory
errorCategory = category

-- Extract severity from TypeError (alias for severity field)
errorSeverity :: TypeError -> ErrorSeverity
errorSeverity = severity

-- Extract message from TypeError (alias for message field)
errorMessage :: TypeError -> Text
errorMessage = message

-- Export unknownLocation (renamed from _unknownLocation)
unknownLocation :: ErrorLocation
unknownLocation = ErrorLocation Nothing 0 0 Nothing Nothing

-- Export chooseBestRecovery (renamed from _chooseBestRecovery)
chooseBestRecovery :: [ErrorRecovery] -> ErrorRecovery
chooseBestRecovery [] = fatalRecovery
chooseBestRecovery strategies = foldl1 chooseBest strategies
  where
    chooseBest r1 r2
        | not (canRecover r1) = r2
        | not (canRecover r2) = r1
        | recoveryConfidence r1 > recoveryConfidence r2 = r1
        | recoveryConfidence r2 > recoveryConfidence r1 = r2
        | recoveryCost r1 < recoveryCost r2 = r1
        | otherwise = r2

-- Enhanced error recovery strategies
createRecoveryStrategy :: Bool -> Bool -> Maybe String -> Maybe String -> ErrorRecovery
createRecoveryStrategy canRec shouldCont recAction recHint = ErrorRecovery canRec shouldCont recAction recHint 50 0.5

-- Create fatal error
fatalError :: String -> Text -> ErrorLocation -> TypeError
fatalError errId msg loc = (errorAt errId Fatal msg loc) { recovery = fatalRecovery }

-- Create fatal error with category
fatalErrorWithCategory :: String -> ErrorCategory -> Text -> ErrorLocation -> TypeError
fatalErrorWithCategory errId errCategory msg loc = (errorAt errId Fatal msg loc) { category = errCategory }


