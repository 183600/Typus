{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Compiler.Errors.Types (
    ErrorSeverity(..),
    CombinedError(..),
    ErrorCategory(..),
    ErrorLocation(..),
    ErrorContext(..),
    emptyContext,
    ErrorRecovery(..)
) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Ownership.Common.Types as Own
import qualified Dependencies.TypeSystem as Dep

-- ============================================================================
-- Error Severity Levels
-- ============================================================================

data ErrorSeverity = Fatal | Error | Warning | Info
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

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

data ErrorRecovery = ErrorRecovery
    { canRecover :: Bool
    , shouldContinue :: Bool
    , recoveryAction :: Maybe String
    , recoveryHint :: Maybe String
    , recoveryCost :: Int
    , recoveryConfidence :: Float
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