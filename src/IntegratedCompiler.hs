{-# LANGUAGE RecordWildCards #-}

module IntegratedCompiler (
    compileWithIntegratedAnalyzers,
    IntegratedCompileResult(..),
    CompilerConfig(..),
    defaultCompilerConfig,
    AnalysisResult(..),
    CombinedError(..),
    ErrorSeverity(..),
    formatCompilationResult,
    getDetailedAnalysisSummary,
    showCombinedError
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
                                    case compile typusFile of
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
    analysisStatus 0 0 = "All analyses passed"
    analysisStatus _ 0 = "Ownership analysis reported issues"
    analysisStatus 0 _ = "Dependent type analysis reported issues"
    analysisStatus _ _ = "Multiple analyses reported issues"
