{-# LANGUAGE RecordWildCards #-}

module IntegratedCompiler (
    compileWithIntegratedAnalyzers,
    IntegratedCompileResult(..),
    CompilerConfig(..),
    defaultCompilerConfig,
    AnalysisResult(..),
    CombinedError(..),
    ErrorSeverity(..),
    analysisToCombined,
    formatCompilationResult,
    getDetailedAnalysisSummary,
    showCombinedError,
    -- Legacy compatibility functions
    analyze,
    compileSource,
    getErrors,
    getWarnings,
    getAnalysisErrors,
    getCompilationErrors,
    getAllErrors
) where

import qualified Parser as P
import AnalyzerIntegration
    ( AnalysisResult(..)
    , CombinedError(..)
    , ErrorSeverity(..)
    , mkAnalysisInput
    , newIntegratedAnalyzer
    , runIntegratedAnalysis
    )
import Compiler (compile)
import Compiler.Errors.Compiler (CompilerError, formatCompilerErrors)
import Compiler.Errors.Core (combinedErrorSeverity, filterCombinedErrorsBySeverity)
import Data.List (intercalate, partition)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified SyntaxValidator as SV
import Text.Read (readMaybe)

-- | Configuration for the integrated compiler pipeline.
data CompilerConfig = CompilerConfig
    { enableOwnership :: Bool
    , enableDependentTypes :: Bool
    , errorReportingLevel :: ErrorSeverity
    } deriving (Show, Eq)

-- | Default configuration that enables both analyzers and reports warnings and above.
defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig = CompilerConfig
    { enableOwnership = True
    , enableDependentTypes = True
    , errorReportingLevel = Warning
    }

-- | Result returned by the integrated compiler entrypoint.
data IntegratedCompileResult = IntegratedCompileResult
    { success :: Bool
    , compiledCode :: String
    , analysisResult :: Maybe AnalysisResult
    , syntaxErrors :: [SV.SyntaxError]
    , filteredErrors :: [CombinedError]
    , compilerErrors :: [CompilerError]
    , compilationWarnings :: [String]
    , compilationInfo :: [String]
    } deriving (Show, Eq)

-- | Orchestrate parsing, analysis, and compilation using the unified pipeline.
compileWithIntegratedAnalyzers :: String -> CompilerConfig -> IO IntegratedCompileResult
compileWithIntegratedAnalyzers source CompilerConfig{..} =
    let syntaxFindings = SV.validateFile source
        (syntaxWarningsIssues, syntaxErrorIssues) = partition isSyntaxWarning syntaxFindings
        syntaxWarningMessages = map SV.formatSyntaxError syntaxWarningsIssues
    in if not (null syntaxErrorIssues)
        then
            pure
                IntegratedCompileResult
                    { success = False
                    , compiledCode = ""
                    , analysisResult = Nothing
                    , syntaxErrors = syntaxErrorIssues
                    , filteredErrors = []
                    , compilerErrors = []
                    , compilationWarnings = syntaxWarningMessages
                    , compilationInfo = []
                    }
        else
            case P.parseTypus source of
                Left parseErr ->
                    let parserIssue = parserErrorToSyntaxError parseErr
                    in pure
                        IntegratedCompileResult
                            { success = False
                            , compiledCode = ""
                            , analysisResult = Nothing
                            , syntaxErrors = [parserIssue]
                            , filteredErrors = []
                            , compilerErrors = []
                            , compilationWarnings = syntaxWarningMessages
                            , compilationInfo = []
                            }
                Right typusFile -> do
                    let analyzerState = newIntegratedAnalyzer enableOwnership enableDependentTypes
                        analysisInput = mkAnalysisInput source
                    analysisOutcome <- runIntegratedAnalysis analysisInput analyzerState
                    case analysisOutcome of
                        Left errMsg ->
                            pure
                                IntegratedCompileResult
                                    { success = False
                                    , compiledCode = ""
                                    , analysisResult = Nothing
                                    , syntaxErrors = []
                                    , filteredErrors = [IntegrationError errMsg Fatal]
                                    , compilerErrors = []
                                    , compilationWarnings = syntaxWarningMessages
                                    , compilationInfo = []
                                    }
                        Right analysis -> do
                            let allErrors = analysisToCombined analysis
                                filtered = filterCombinedErrorsBySeverity errorReportingLevel allErrors
                                blocking = any (\err -> combinedErrorSeverity err >= Error) filtered
                                analysisWarnings' = analysisWarnings analysis
                                info = analysisInfo analysis
                                combinedWarnings = syntaxWarningMessages ++ analysisWarnings'
                            if blocking
                                then
                                    pure
                                        IntegratedCompileResult
                                            { success = False
                                            , compiledCode = ""
                                            , analysisResult = Just analysis
                                            , syntaxErrors = []
                                            , filteredErrors = filtered
                                            , compilerErrors = []
                                            , compilationWarnings = combinedWarnings
                                            , compilationInfo = info
                                            }
                                else
                                    case Compiler.compile typusFile of
                                        Left compilerErrs ->
                                            pure
                                                IntegratedCompileResult
                                                    { success = False
                                                    , compiledCode = ""
                                                    , analysisResult = Just analysis
                                                    , syntaxErrors = []
                                                    , filteredErrors = filtered
                                                    , compilerErrors = compilerErrs
                                                    , compilationWarnings = combinedWarnings
                                                    , compilationInfo = info
                                                    }
                                        Right goCode ->
                                            pure
                                                IntegratedCompileResult
                                                    { success = True
                                                    , compiledCode = goCode
                                                    , analysisResult = Just analysis
                                                    , syntaxErrors = []
                                                    , filteredErrors = filtered
                                                    , compilerErrors = []
                                                    , compilationWarnings = combinedWarnings
                                                    , compilationInfo = info
                                                    }

isSyntaxWarning :: SV.SyntaxError -> Bool
isSyntaxWarning err = SV.errorType err == SV.SyntaxWarning

parserErrorToSyntaxError :: String -> SV.SyntaxError
parserErrorToSyntaxError msg =
    let ls = lines msg
        (lineNum, colNum) = positionFromHeader ls
        lineContent =
            case drop 2 ls of
                (codeLine:_) -> extractCodeLine codeLine
                _ -> ""
        messageText =
            case dropWhile null (drop 4 ls) of
                [] -> msg
                xs -> intercalate "\n" xs
    in SV.SyntaxError
        { SV.errorType = SV.UnexpectedToken
        , SV.errorMessage = messageText
        , SV.lineNumber = lineNum
        , SV.columnNumber = colNum
        , SV.lineContent = lineContent
        }
  where
    positionFromHeader :: [String] -> (Int, Int)
    positionFromHeader entries =
        case entries of
            (header:_) -> parseHeader header
            _ -> (0, 0)

    parseHeader :: (Num a, Num b, Read a, Read b) => [Char] -> (a, b)
    parseHeader header =
        case splitOnColon header of
            (_:lineStr:colStr:_) ->
                ( fromMaybe 0 (readMaybe lineStr)
                , fromMaybe 0 (readMaybe colStr)
                )
            _ -> (0, 0)

    extractCodeLine line =
        case break (== '|') line of
            (_, []) -> dropLeadingSpace line
            (_, _ : rest) -> dropLeadingSpace rest

    dropLeadingSpace [] = []
    dropLeadingSpace (' ' : xs) = xs
    dropLeadingSpace ('\t' : xs) = xs
    dropLeadingSpace xs = xs

    splitOnColon str =
        case break (== ':') str of
            (chunk, []) -> [chunk]
            (chunk, _ : rest) -> chunk : splitOnColon rest

-- | Transform analyzer output into combined errors for downstream consumers.
analysisToCombined :: AnalysisResult -> [CombinedError]
analysisToCombined AnalysisResult{ combinedErrors = errs } = errs

-- | Pretty print the integrated compilation result.
formatCompilationResult :: IntegratedCompileResult -> String
formatCompilationResult IntegratedCompileResult{..} =
    let sections = filter (not . null)
            [ formatSection "📝 Syntax Errors" (map (bullet . SV.formatSyntaxError) syntaxErrors)
            , formatSection "⚠️ Analysis Errors" (map (bullet . formatCombinedError) filteredErrors)
            , formatCompilerSection compilerErrors
            , formatSection "⚡ Warnings" (map bullet compilationWarnings)
            , formatSection "ℹ️ Info" (map bullet compilationInfo)
            ]
        body = if null sections then "" else "\n" ++ intercalate "\n\n" sections
        statusLine = if success then "✅ Compilation Successful" else "❌ Compilation Failed"
    in statusLine ++ body
  where
    bullet msg = "  • " ++ msg

    formatSection :: String -> [String] -> String
    formatSection _ [] = ""
    formatSection title lines' = title ++ "\n" ++ unlines lines'

    formatCombinedError :: CombinedError -> String
    formatCombinedError err =
        let severityLabel = showSeverity (combinedErrorSeverity err)
        in "[" ++ severityLabel ++ "] " ++ showCombinedError err

    formatCompilerSection :: [CompilerError] -> String
    formatCompilerSection [] = ""
    formatCompilerSection errs =
        let formatted = formatCompilerErrors errs
            formattedLines = filter (not . null) (lines formatted)
            indented = unlines (map bullet formattedLines)
        in "💥 Compiler Errors\n" ++ indented

    showSeverity :: ErrorSeverity -> String
    showSeverity Fatal = "FATAL"
    showSeverity Error = "ERROR"
    showSeverity Warning = "WARN"
    showSeverity Info = "INFO"

-- | Render a human readable view of a combined error.
showCombinedError :: CombinedError -> String
showCombinedError (OwnershipErrorCombined _ err) = "Ownership error: " ++ show err
showCombinedError (DependentTypeErrorCombined _ err) = "Dependent type error: " ++ show err
showCombinedError (IntegrationError msg _) = "Integration error: " ++ msg
showCombinedError (CrossAnalyzerError msg _ nested) =
    let nestedMessages = case nested of
            [] -> ""
            xs -> " (" ++ intercalate ", " (map showCombinedError xs) ++ ")"
    in "Cross-analyzer error: " ++ msg ++ nestedMessages

-- | Produce a high-level summary of analyzer findings.
getDetailedAnalysisSummary :: AnalysisResult -> String
getDetailedAnalysisSummary AnalysisResult
    { ownershipErrors = ownershipErrs
    , dependentTypeErrors = dependentErrs
    , analysisWarnings = warnings
    , analysisInfo = infoMessages
    , typeEnvironment = typeEnv
    } =
    let ownershipErrorCount = length ownershipErrs
        dependentErrorCount = length dependentErrs
        typeEnvSize = Map.size typeEnv
        warningCount = length warnings
        infoCount = length infoMessages
    in unlines
        [ "Analysis Summary"
        , "================"
        , "Ownership errors: " ++ show ownershipErrorCount
        , "Dependent type errors: " ++ show dependentErrorCount
        , "Warnings: " ++ show warningCount
        , "Info messages: " ++ show infoCount
        , "Type environment bindings: " ++ show typeEnvSize
        , "Status: " ++ analysisStatus ownershipErrorCount dependentErrorCount
        ]
  where
    analysisStatus :: (Eq a1, Eq a2, Num a1, Num a2) => a1 -> a2 -> String
    analysisStatus 0 0 = "All analyses passed"
    analysisStatus _ 0 = "Ownership analysis reported issues"
    analysisStatus 0 _ = "Dependent type analysis reported issues"
    analysisStatus _ _ = "Multiple analyses reported issues"

-- Legacy compatibility functions for test compatibility

-- | Analyze source code and return analysis result
analyze :: String -> IO (Either String AnalysisResult)
analyze source = do
    let config = defaultCompilerConfig
    result <- compileWithIntegratedAnalyzers source config
    case analysisResult result of
        Nothing -> return $ Left "No analysis result"
        Just analysis -> return $ Right analysis

-- | Compile source code and return compiled code
compileSource :: String -> IO (Either String String)
compileSource source = do
    let config = defaultCompilerConfig
    result <- compileWithIntegratedAnalyzers source config
    if success result
        then return $ Right (compiledCode result)
        else return $ Left "Compilation failed"

-- | Get all errors from analysis
getErrors :: AnalysisResult -> [String]
getErrors analysis = 
    let ownershipErrs = map show (ownershipErrors analysis)
        dependentErrs = map show (dependentTypeErrors analysis)
        combinedErrs = map showCombinedError (combinedErrors analysis)
    in ownershipErrs ++ dependentErrs ++ combinedErrs

-- | Get all warnings from analysis
getWarnings :: AnalysisResult -> [String]
getWarnings analysis = analysisWarnings analysis

-- | Get analysis errors specifically
getAnalysisErrors :: AnalysisResult -> [String]
getAnalysisErrors analysis = 
    let ownershipErrs = map show (ownershipErrors analysis)
        dependentErrs = map show (dependentTypeErrors analysis)
    in ownershipErrs ++ dependentErrs

-- | Get compilation errors specifically
getCompilationErrors :: AnalysisResult -> [String]
getCompilationErrors analysis = 
    map showCombinedError $ filter isCompilationError (combinedErrors analysis)
  where
    isCompilationError (IntegrationError _ _) = True
    isCompilationError _ = False

-- | Get all errors (analysis + compilation)
getAllErrors :: AnalysisResult -> [String]
getAllErrors analysis = getAnalysisErrors analysis ++ getCompilationErrors analysis
