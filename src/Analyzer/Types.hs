{-# LANGUAGE OverloadedStrings #-}
module Analyzer.Types (
    ErrorSeverity(..),
    CombinedError(..),
    AnalysisResult(..),
    SymbolInfo(..),
    SymbolKind(..),
    AnalysisPhase(..),
    AnalysisContext(..),
    AnalyzerState(..),
    IntegratedAnalyzer
) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Ownership as Own
import qualified Dependencies as Dep
import Compiler.Errors.Core (ErrorSeverity(..), CombinedError(..))

-- Symbol kind classification
data SymbolKind
    = SymbolVariable
    | SymbolFunction
    | SymbolType
    | SymbolConstant
    | SymbolPackage
    | SymbolModule
    deriving (Show, Eq)

data AnalysisResult = AnalysisResult
    { ownershipErrors :: [(ErrorSeverity, Own.OwnershipError)]
    , dependentTypeErrors :: [(ErrorSeverity, Dep.DependentTypeError)]
    , combinedErrors :: [CombinedError]
    , analysisWarnings :: [String]
    , analysisInfo :: [String]
    , typeEnvironment :: Map.Map String Dep.TypeVar
    } deriving (Show, Eq)

data SymbolInfo = SymbolInfo
    { symbolName :: String
    , symbolType :: Maybe Dep.TypeVar
    , ownershipState :: Maybe Own.OwnershipType
    , symbolScope :: Int
    , isMoved :: Bool
    , isBorrowed :: Bool
    , constraints :: [Dep.Constraint]
    } deriving (Show, Eq)

data AnalysisPhase = InitialPhase | OwnershipPhase | DependentTypePhase | IntegrationPhase
    deriving (Show, Eq)

data AnalysisContext = AnalysisContext
    { enableOwnership :: Bool
    , enableDependentTypes :: Bool
    , currentFile :: String
    , analysisPhase :: AnalysisPhase
    } deriving (Show, Eq)

data AnalyzerState = AnalyzerState
    { ownershipAnalyzer :: Own.OwnershipAnalyzer
    , dependentTypeChecker :: Dep.DependentTypeChecker
    , currentScope :: Int
    , symbolTable :: Map.Map String SymbolInfo
    , analysisContext :: AnalysisContext
    , combinedErrorsAcc :: [CombinedError]
    , ownershipErrorsAcc :: [(ErrorSeverity, Own.OwnershipError)]
    , dependentTypeErrorsAcc :: [(ErrorSeverity, Dep.DependentTypeError)]
    } deriving (Show, Eq)

type IntegratedAnalyzer = StateT AnalyzerState (ExceptT String IO)
