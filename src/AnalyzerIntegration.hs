{-# LANGUAGE OverloadedStrings #-}
module AnalyzerIntegration (
    IntegratedAnalyzer,
    AnalyzerState(..),
    AnalysisResult(..),
    CombinedError(..),
    ErrorSeverity(..),
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
                      , AnalysisResult(..)
                      , CombinedError(..)
                      , ErrorSeverity(..)
                      , IntegratedAnalyzer
                      , AnalysisContext(..)
                      , AnalysisPhase(..)
                      )

import qualified Ownership as Own
import qualified Dependencies as Dep

runIntegratedAnalysis :: String -> AnalyzerState -> IO (Either String AnalysisResult)
runIntegratedAnalysis code initialState = do
    result <- runExceptT $ runStateT (analyzeCodeWithBothAnalyzers code) initialState
    pure $ case result of
        Left err -> Left err
        Right (analysisResult, _) -> Right analysisResult

analyzeCodeWithBothAnalyzers :: String -> IntegratedAnalyzer AnalysisResult
analyzeCodeWithBothAnalyzers code = do
    modify $ \s -> s { analysisContext = (analysisContext s) { currentFile = "<input>" } }
    setPhase InitialPhase
    symbols <- collectSymbolsAndTypes code
    modify $ \s -> s { symbolTable = symbols }
    ownershipResults <- ifEnableOwnership [] $ do
        setPhase OwnershipPhase
        runOwnershipAnalysis code
    dependentTypeResults <- ifEnableDependentTypes [] $ do
        setPhase DependentTypePhase
        runDependentTypeAnalysis code
    setPhase IntegrationPhase
    integrationResults <- runCrossAnalysis code
    combineAllResults ownershipResults dependentTypeResults integrationResults

combineAllResults :: [Own.OwnershipError] -> [Dep.DependentTypeError] -> [CombinedError] -> IntegratedAnalyzer AnalysisResult
combineAllResults ownershipErrs typeErrs integrationErrs = do
    symbols <- gets symbolTable
    pure
        AnalysisResult
            { ownershipErrors = ownershipErrs
            , dependentTypeErrors = typeErrs
            , analysisWarnings = filterWarnings integrationErrs
            , analysisInfo = filterInfo integrationErrs
            , typeEnvironment = extractTypeEnvironment symbols
            }
