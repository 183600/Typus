module Analyzer.State (
    newIntegratedAnalyzer,
    setPhase,
    ifEnableOwnership,
    ifEnableDependentTypes,
    addOwnershipError,
    addDependentTypeError,
    addCombinedError,
    filterWarnings,
    filterInfo,
    collectMessages,
    getCombinedErrors,
    getAnalysisSummary
) where

import Analyzer.Types
import qualified Ownership as Own
import qualified Dependencies as Dep
import Compiler.Errors.Core (combinedErrorSeverity)

import Control.Monad.State
import qualified Data.Map.Strict as Map

newIntegratedAnalyzer :: Bool -> Bool -> AnalyzerState
newIntegratedAnalyzer enableOwnershipFlag enableDependentTypesFlag = AnalyzerState
    { ownershipAnalyzer = Own.newOwnershipAnalyzer
    , dependentTypeChecker = Dep.newDependentTypeChecker
    , currentScope = 0
    , symbolTable = Map.empty
    , analysisContext = AnalysisContext
        { enableOwnership = enableOwnershipFlag
        , enableDependentTypes = enableDependentTypesFlag
        , currentFile = ""
        , analysisPhase = InitialPhase
        }
    , combinedErrorsAcc = []
    , ownershipErrorsAcc = []
    , dependentTypeErrorsAcc = []
    }

setPhase :: AnalysisPhase -> IntegratedAnalyzer ()
setPhase phase = modify $ \s -> s { analysisContext = (analysisContext s) { analysisPhase = phase } }

ifEnableOwnership :: a -> IntegratedAnalyzer a -> IntegratedAnalyzer a
ifEnableOwnership def action = do
    enabled <- gets (enableOwnership . analysisContext)
    if enabled then action else pure def

ifEnableDependentTypes :: a -> IntegratedAnalyzer a -> IntegratedAnalyzer a
ifEnableDependentTypes def action = do
    enabled <- gets (enableDependentTypes . analysisContext)
    if enabled then action else pure def

addOwnershipError :: ErrorSeverity -> Own.OwnershipError -> IntegratedAnalyzer ()
addOwnershipError severity err = modify $ \s ->
    s { ownershipErrorsAcc = ownershipErrorsAcc s ++ [(severity, err)]
      , combinedErrorsAcc = combinedErrorsAcc s ++ [OwnershipErrorCombined severity err]
      }

addDependentTypeError :: ErrorSeverity -> Dep.DependentTypeError -> IntegratedAnalyzer ()
addDependentTypeError severity err = modify $ \s ->
    s { dependentTypeErrorsAcc = dependentTypeErrorsAcc s ++ [(severity, err)]
      , combinedErrorsAcc = combinedErrorsAcc s ++ [DependentTypeErrorCombined severity err]
      }

addCombinedError :: CombinedError -> IntegratedAnalyzer ()
addCombinedError err = modify $ \s -> s { combinedErrorsAcc = combinedErrorsAcc s ++ [err] }

filterWarnings :: [CombinedError] -> [String]
filterWarnings = collectMessages Warning

filterInfo :: [CombinedError] -> [String]
filterInfo = collectMessages Info

collectMessages :: ErrorSeverity -> [CombinedError] -> [String]
collectMessages sev = concatMap (go sev)
  where
    go target (OwnershipErrorCombined s e)
        | s == target = [show e]
        | otherwise = []
    go target (DependentTypeErrorCombined s e)
        | s == target = [show e]
        | otherwise = []
    go target (IntegrationError msg s)
        | s == target = [msg]
        | otherwise = []
    go target (CrossAnalyzerError msg s subs) =
        (if s == target then [msg] else []) ++ concatMap (go target) subs

getCombinedErrors :: AnalyzerState -> [CombinedError]
getCombinedErrors = combinedErrorsAcc

getAnalysisSummary :: AnalyzerState -> String
getAnalysisSummary state' =
    let (errorCount, warningCount, infoCount) = countBySeverity (combinedErrorsAcc state')
    in unlines
        [ "Analysis Summary:"
        , "================="
        , "Errors: " ++ show errorCount
        , "Warnings: " ++ show warningCount
        , "Info: " ++ show infoCount
        , "Total symbols: " ++ show (Map.size $ symbolTable state')
        ]
  where
    countBySeverity :: [CombinedError] -> (Int, Int, Int)
    countBySeverity = foldr add (0, 0, 0)
      where
        add :: CombinedError -> (Int, Int, Int) -> (Int, Int, Int)
        add ce (e, w, i) =
            case combinedErrorSeverity ce of
                Fatal -> (e + 1, w, i)
                Error -> (e + 1, w, i)
                Warning -> (e, w + 1, i)
                Info -> (e, w, i + 1)
