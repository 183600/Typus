{-# LANGUAGE OverloadedStrings #-}
module AnalyzerIntegration (
    IntegratedAnalyzer,
    AnalyzerState(..),
    AnalysisContext(..),
    AnalysisResult(..),
    CombinedError(..),
    ErrorSeverity(..),
    AnalysisInput(..),
    mkAnalysisInput,
    newIntegratedAnalyzer,
    runIntegratedAnalysis,
    analyzeCodeWithBothAnalyzers,
    getCombinedErrors,
    getAnalysisSummary,
    Own.OwnershipError(UseAfterMove, DoubleMove, BorrowWhileMoved, MutBorrowWhileBorrowed, BorrowWhileMutBorrowed, MultipleMutBorrows, UseWhileMutBorrowed, OutOfScope, ParseError),
    Own.OwnershipType(..),
    Dep.TypeVar(..),
    Dep.Constraint(..)
) where

import Control.Monad.Except
import Control.Monad.State

import Analyzer.State
import Analyzer.SymbolTable
import Analyzer.OwnershipBridge
import Analyzer.DependentTypeBridge
import Analyzer.CrossAnalysis
import Analyzer.Types ( AnalyzerState(..)
                      , AnalysisContext(..)
                      , AnalysisPhase(..)
                      , AnalysisResult(..)
                      , CombinedError(..)
                      , ErrorSeverity(..)
                      , IntegratedAnalyzer
                      )

import qualified Ownership as Own
import qualified Dependencies as Dep

-- | Input metadata for running the integrated analyzers.
data AnalysisInput = AnalysisInput
    { sourceCode :: String
    , sourceFilePath :: Maybe FilePath
    , sourceLabel :: Maybe String
    } deriving (Show, Eq)

-- | Construct a minimal analysis input from raw source code.
mkAnalysisInput :: String -> AnalysisInput
mkAnalysisInput code =
    AnalysisInput
        { sourceCode = code
        , sourceFilePath = Nothing
        , sourceLabel = Nothing
        }

runIntegratedAnalysis :: AnalysisInput -> AnalyzerState -> IO (Either String AnalysisResult)
runIntegratedAnalysis input initialState = do
    result <- runExceptT $ runStateT (analyzeCodeWithBothAnalyzers input) initialState
    pure $ case result of
        Left err -> Left err
        Right (analysisResult, _) -> Right analysisResult

analyzeCodeWithBothAnalyzers :: AnalysisInput -> IntegratedAnalyzer AnalysisResult
analyzeCodeWithBothAnalyzers input = do
    modify $ \s ->
        let ctx = analysisContext s
            updatedCtx = ctx { currentFile = resolveSourceDescriptor input }
        in s { analysisContext = updatedCtx }
    setPhase InitialPhase
    let code = sourceCode input
    symbols <- collectSymbolsAndTypes code
    modify $ \s -> s { symbolTable = symbols }
    ownershipResults <- ifEnableOwnership [] $ do
        setPhase OwnershipPhase
        runOwnershipAnalysis code
    dependentTypeResults <- ifEnableDependentTypes [] $ do
        setPhase DependentTypePhase
        runDependentTypeAnalysis code
    setPhase IntegrationPhase
    _ <- runCrossAnalysis code
    combineAllResults ownershipResults dependentTypeResults

combineAllResults :: [(ErrorSeverity, Own.OwnershipError)] -> [(ErrorSeverity, Dep.DependentTypeError)] -> IntegratedAnalyzer AnalysisResult
combineAllResults ownershipErrs typeErrs = do
    symbols <- gets symbolTable
    combinedErrs <- gets getCombinedErrors
    pure
        AnalysisResult
            { ownershipErrors = ownershipErrs
            , dependentTypeErrors = typeErrs
            , combinedErrors = combinedErrs
            , analysisWarnings = filterWarnings combinedErrs
            , analysisInfo = filterInfo combinedErrs
            , typeEnvironment = extractTypeEnvironment symbols
            }

resolveSourceDescriptor :: AnalysisInput -> String
resolveSourceDescriptor input =
    case sourceFilePath input of
        Just path -> path
        Nothing -> maybe "<input>" id (sourceLabel input)
