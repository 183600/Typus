{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EnhancedErrorHandler (
    -- Re-export core error types
    module ErrorHandler,
    module SourceLocation,
    
    -- Enhanced error construction
    CompilerError(..),
    CompilerResult,
    CompilerM,
    runCompilerM,
    
    -- Error construction helpers
    syntaxError,
    typeError,
    ownershipError,
    dependentTypeError,
    semanticError,
    
    -- Error collection and recovery
    collectErrors,
    recoverFrom,
    continueWith,
    withRecovery,
    
    -- Enhanced formatting
    formatCompilerError,
    formatCompilerErrors,
    generateDetailedReport,
    
    -- Location tracking integration
    withSourceLocation,
    trackLocation,
    
    -- Error statistics
    analyzeErrors,
    ErrorStatistics(..),
    
    -- User-friendly messages
    makeUserFriendly,
    suggestFix,
    
    -- Examples and documentation
    exampleErrors
) where

import ErrorHandler
import SourceLocation
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Compiler-Specific Error Type
-- ============================================================================

-- Enhanced error type that combines TypeError with compiler-specific context
data CompilerError = CompilerError
    { ceError :: TypeError
    , ceSourceContext :: Maybe String  -- Source code context
    , ceStackTrace :: [String]         -- Call stack for debugging
    , cePhase :: CompilationPhase      -- Which phase produced the error
    } deriving (Show, Eq)

data CompilationPhase
    = LexingPhase
    | ParsingPhase
    | TypeCheckingPhase
    | OwnershipAnalysisPhase
    | DependentTypeCheckingPhase
    | CodeGenerationPhase
    | OptimizationPhase
    deriving (Show, Eq, Ord)

-- Result type for compiler operations
type CompilerResult a = Either [CompilerError] a

-- Compiler monad that accumulates errors
type CompilerM a = StateT [CompilerError] (Either [CompilerError]) a

-- Run the compiler monad
runCompilerM :: CompilerM a -> CompilerResult a
runCompilerM action = case runStateT action [] of
    Left errs -> Left errs
    Right (result, []) -> Right result
    Right (_, errs) -> Left errs

-- ============================================================================
-- Enhanced Error Construction
-- ============================================================================

-- Create a syntax error with location
syntaxError :: String -> Text -> SourcePos -> CompilerError
syntaxError errId msg pos = CompilerError
    { ceError = errorWithCategory errId Parsing msg (toErrorLocation pos)
        { recovery = errorRecovery
        , severity = Error
        }
    , ceSourceContext = Nothing
    , ceStackTrace = []
    , cePhase = ParsingPhase
    }

-- Create a type error with location and context
typeError :: String -> Text -> SourceSpan -> Maybe String -> [Text] -> CompilerError
typeError errId msg span mContext suggestions = CompilerError
    { ceError = (errorWithCategory errId TypeChecking msg (toErrorLocationWithSpan span))
        { suggestions = suggestions
        , context = emptyContext { contextCode = mContext }
        , recovery = errorRecovery
        }
    , ceSourceContext = mContext
    , ceStackTrace = []
    , cePhase = TypeCheckingPhase
    }

-- Create an ownership error with recovery suggestions
ownershipError :: String -> Text -> SourceSpan -> String -> [Text] -> CompilerError
ownershipError errId msg span code suggestions = CompilerError
    { ceError = (errorWithCategory errId Ownership msg (toErrorLocationWithSpan span))
        { suggestions = suggestions
        , context = emptyContext { contextCode = Just code }
        , recovery = customRecovery True True 
            (Just "Try using references or cloning")
            (Just "Consider the lifetime of your variables")
            30 0.8
        }
    , ceSourceContext = Just code
    , ceStackTrace = []
    , cePhase = OwnershipAnalysisPhase
    }

-- Create a dependent type error
dependentTypeError :: String -> Text -> SourceSpan -> [Text] -> CompilerError
dependentTypeError errId msg span suggestions = CompilerError
    { ceError = (errorWithCategory errId Constraint msg (toErrorLocationWithSpan span))
        { suggestions = suggestions
        , severity = Error
        , recovery = errorRecovery
        }
    , ceSourceContext = Nothing
    , ceStackTrace = []
    , cePhase = DependentTypeCheckingPhase
    }

-- Create a semantic error
semanticError :: String -> Text -> SourceSpan -> Maybe String -> CompilerError
semanticError errId msg span mContext = CompilerError
    { ceError = (errorWithCategory errId Semantic msg (toErrorLocationWithSpan span))
        { context = emptyContext { contextCode = mContext }
        , recovery = errorRecovery
        }
    , ceSourceContext = mContext
    , ceStackTrace = []
    , cePhase = TypeCheckingPhase
    }

-- ============================================================================
-- Error Collection and Recovery
-- ============================================================================

-- Collect errors during compilation
collectErrors :: CompilerM () -> CompilerResult [CompilerError]
collectErrors action = case runStateT action [] of
    Left errs -> Left errs
    Right (_, errs) -> if null errs then Right [] else Left errs

-- Recover from an error and continue
recoverFrom :: CompilerError -> CompilerM ()
recoverFrom err = modify (++ [err])

-- Continue compilation with a default value after error
continueWith :: a -> CompilerError -> CompilerM a
continueWith defaultVal err = do
    recoverFrom err
    return defaultVal

-- Execute action with error recovery
withRecovery :: CompilerM a -> a -> CompilerM a
withRecovery action defaultVal = 
    catchError action (\_ -> return defaultVal)

-- ============================================================================
-- Enhanced Formatting
-- ============================================================================

-- Format a single compiler error with rich context
formatCompilerError :: CompilerError -> String
formatCompilerError CompilerError{..} =
    let baseError = formatErrorWithLocation ceError
        phaseStr = "\nPhase: " ++ show cePhase
        contextStr = case ceSourceContext of
            Just ctx -> "\n\nSource Context:\n" ++ addLineNumbers ctx
            Nothing -> ""
        stackStr = if null ceStackTrace 
            then "" 
            else "\n\nStack Trace:\n" ++ unlines (map ("  " ++) ceStackTrace)
        recoveryStr = formatRecoveryInfo (recovery ceError)
    in baseError ++ phaseStr ++ contextStr ++ stackStr ++ recoveryStr

-- Format recovery information
formatRecoveryInfo :: ErrorRecovery -> String
formatRecoveryInfo rec =
    let canRecStr = if canRecover rec
            then "\n✓ Recoverable error - compilation can continue"
            else "\n✗ Fatal error - compilation stopped"
        actionStr = case recoveryAction rec of
            Just action -> "\nRecovery Action: " ++ action
            Nothing -> ""
        hintStr = case recoveryHint rec of
            Just hint -> "\nHint: " ++ hint
            Nothing -> ""
        confidenceStr = "\nRecovery Confidence: " ++ 
            show (round (recoveryConfidence rec * 100)) ++ "%"
    in canRecStr ++ actionStr ++ hintStr ++ confidenceStr

-- Add line numbers to source code
addLineNumbers :: String -> String
addLineNumbers code = 
    unlines $ zipWith formatLine [1..] (lines code)
  where
    formatLine n line = "  " ++ show n ++ " | " ++ line

-- Format multiple compiler errors
formatCompilerErrors :: [CompilerError] -> String
formatCompilerErrors errs =
    let grouped = groupErrorsByPhase errs
        sections = map formatPhaseErrors (Map.toList grouped)
    in unlines $ 
        ["=== Compilation Errors ===", ""] ++ 
        sections

-- Group errors by compilation phase
groupErrorsByPhase :: [CompilerError] -> Map.Map CompilationPhase [CompilerError]
groupErrorsByPhase = foldr addToMap Map.empty
  where
    addToMap err = Map.insertWith (++) (cePhase err) [err]

-- Format errors for a specific phase
formatPhaseErrors :: (CompilationPhase, [CompilerError]) -> String
formatPhaseErrors (phase, errs) =
    let header = "\n--- " ++ show phase ++ " (" ++ show (length errs) ++ " errors) ---\n"
        formatted = unlines $ map formatCompilerError errs
    in header ++ formatted

-- ============================================================================
-- Detailed Error Report
-- ============================================================================

-- Generate a comprehensive error report
generateDetailedReport :: [CompilerError] -> String
generateDetailedReport errs =
    let stats = analyzeErrors errs
        summary = formatErrorSummary stats
        details = formatCompilerErrors errs
        recommendations = generateRecommendations stats
    in unlines [summary, "", details, "", recommendations]

-- Format error summary
formatErrorSummary :: ErrorStatistics -> String
formatErrorSummary ErrorStatistics{..} =
    unlines
        [ "=== Error Summary ==="
        , "Total Errors: " ++ show esTotal
        , "Fatal: " ++ show esFatal
        , "Errors: " ++ show esErrors
        , "Warnings: " ++ show esWarnings
        , "Info: " ++ show esInfo
        , ""
        , "By Phase:"
        ] ++ map formatPhaseStat (Map.toList esByPhase) ++
        [ ""
        , "By Category:"
        ] ++ map formatCategoryStat (Map.toList esByCategory) ++
        [ ""
        , "Recoverable: " ++ show esRecoverable ++ " / " ++ show esTotal
        ]

formatPhaseStat :: (CompilationPhase, Int) -> String
formatPhaseStat (phase, count) = "  " ++ show phase ++ ": " ++ show count

formatCategoryStat :: (ErrorCategory, Int) -> String
formatCategoryStat (cat, count) = "  " ++ show cat ++ ": " ++ show count

-- ============================================================================
-- Error Statistics
-- ============================================================================

data ErrorStatistics = ErrorStatistics
    { esTotal :: Int
    , esFatal :: Int
    , esErrors :: Int
    , esWarnings :: Int
    , esInfo :: Int
    , esByPhase :: Map.Map CompilationPhase Int
    , esByCategory :: Map.Map ErrorCategory Int
    , esRecoverable :: Int
    } deriving (Show)

-- Analyze errors and produce statistics
analyzeErrors :: [CompilerError] -> ErrorStatistics
analyzeErrors errs = ErrorStatistics
    { esTotal = length errs
    , esFatal = length $ filter isFatalError errs
    , esErrors = length $ filter isErrorLevel errs
    , esWarnings = length $ filter isWarningLevel errs
    , esInfo = length $ filter isInfoLevel errs
    , esByPhase = groupByPhase errs
    , esByCategory = groupByCategory errs
    , esRecoverable = length $ filter isRecoverableError errs
    }
  where
    isFatalError err = severity (ceError err) == Fatal
    isErrorLevel err = severity (ceError err) == Error
    isWarningLevel err = severity (ceError err) == Warning
    isInfoLevel err = severity (ceError err) == Info
    isRecoverableError err = canRecover $ recovery (ceError err)
    
    groupByPhase = Map.fromListWith (+) . map (\e -> (cePhase e, 1))
    groupByCategory = Map.fromListWith (+) . map (\e -> (category (ceError e), 1))

-- ============================================================================
-- Location Tracking Integration
-- ============================================================================

-- Add source location to an error
withSourceLocation :: CompilerError -> SourceSpan -> CompilerError
withSourceLocation err span = err
    { ceError = (ceError err) { location = toErrorLocationWithSpan span }
    }

-- Track location during parsing
trackLocation :: SourcePos -> CompilerM ()
trackLocation _pos = return ()  -- Can be extended for more detailed tracking

-- ============================================================================
-- User-Friendly Messages
-- ============================================================================

-- Make error messages more user-friendly
makeUserFriendly :: CompilerError -> CompilerError
makeUserFriendly err = err
    { ceError = (ceError err)
        { message = simplifyMessage (message (ceError err))
        , suggestions = improveSuggestions (suggestions (ceError err))
        }
    }

-- Simplify technical error messages
simplifyMessage :: Text -> Text
simplifyMessage msg
    | "Malformed" `T.isInfixOf` msg = 
        "Syntax error: The code structure is incorrect. Check for missing brackets or semicolons."
    | "Type mismatch" `T.isInfixOf` msg = 
        "Type error: The types don't match. Make sure you're using the right type of value."
    | "Ownership" `T.isInfixOf` msg = 
        "Ownership error: You're trying to use a value that's already been moved or borrowed."
    | otherwise = msg

-- Improve suggestions
improveSuggestions :: [Text] -> [Text]
improveSuggestions = map improveSuggestion
  where
    improveSuggestion s
        | T.null s = s
        | otherwise = "💡 " <> s

-- Suggest fixes for common errors
suggestFix :: CompilerError -> [Text]
suggestFix CompilerError{ceError=err} =
    case category err of
        TypeChecking -> 
            [ "Check the types of your variables"
            , "Make sure function return types match their declarations"
            , "Use type annotations to clarify intent"
            ]
        Ownership ->
            [ "Consider using references (&) instead of moving values"
            , "Clone the value if you need multiple owners"
            , "Check the lifetime of borrowed values"
            ]
        Parsing ->
            [ "Check for missing or extra brackets"
            , "Verify all statements end properly"
            , "Look for unclosed strings or comments"
            ]
        Semantic ->
            [ "Ensure variables are declared before use"
            , "Check function signatures match their usage"
            , "Verify all required imports are present"
            ]
        _ -> []

-- ============================================================================
-- Recommendations
-- ============================================================================

-- Generate recommendations based on error statistics
generateRecommendations :: ErrorStatistics -> String
generateRecommendations ErrorStatistics{..} =
    let recs = catMaybes
            [ if esTotal > 10 
                then Just "Consider breaking down your code into smaller functions"
                else Nothing
            , if esFatal > 0 
                then Just "Fix fatal errors first - they prevent compilation"
                else Nothing
            , case Map.lookup ParsingPhase esByPhase of
                Just n | n > 5 -> Just "Many parsing errors detected - check syntax carefully"
                _ -> Nothing
            , case Map.lookup TypeChecking esByCategory of
                Just n | n > 5 -> Just "Consider adding more type annotations"
                _ -> Nothing
            , case Map.lookup Ownership esByCategory of
                Just n | n > 3 -> Just "Review ownership and borrowing rules"
                _ -> Nothing
            ]
    in if null recs
        then "No specific recommendations."
        else "=== Recommendations ===\n" ++ unlines (map ("• " ++) recs)

-- ============================================================================
-- Example Errors for Documentation
-- ============================================================================

exampleErrors :: [(String, CompilerError)]
exampleErrors =
    [ ( "Syntax Error"
      , syntaxError "E001" "Unexpected token '}'" (posAt 10 5)
      )
    , ( "Type Mismatch"
      , typeError "E002" "Type mismatch: expected int, got string" 
          (spanBetween (posAt 15 10) (posAt 15 25))
          (Just "x := \"hello\"\ny := x + 5")
          ["Convert string to int using strconv.Atoi", "Use a numeric literal instead"]
      )
    , ( "Ownership Violation"
      , ownershipError "E003" "Value moved while borrowed"
          (spanBetween (posAt 20 5) (posAt 20 15))
          "x := []int{1,2,3}\ny := &x\nz := x"
          ["Use a reference to avoid moving", "Clone the slice if needed"]
      )
    , ( "Dependent Type Error"
      , dependentTypeError "E004" "Array index out of bounds"
          (spanBetween (posAt 25 8) (posAt 25 18))
          ["Add bounds check", "Use slice bounds"]
      )
    ]