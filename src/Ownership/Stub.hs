{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Stub module for ownership testing
module Ownership.Stub (
  OwnershipAnalysis(..),
  OwnershipTransfer(..),
  OwnershipConstraint(..),
  analyzeOwnership,
  checkOwnershipTransfer,
  validateOwnershipConstraints
) where

import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import SourceLocation (SourceSpan)

-- | Ownership constraint
data OwnershipConstraint = OwnershipConstraint
  { ocVariable :: String
  , ocConstraint :: String
  } deriving (Eq, Show, Generic)

-- | Ownership transfer with optional location
data OwnershipTransfer = OwnershipTransfer
  { otFrom :: String
  , otTo :: String
  , otLocation :: Maybe SourceSpan
  } deriving (Eq, Show, Generic)

-- | Result of ownership analysis
data OwnershipAnalysis = OwnershipAnalysis
  { oaTransfers :: [OwnershipTransfer]
  , oaConstraints :: [OwnershipConstraint]
  , oaVariables :: Map String String
  } deriving (Eq, Show, Generic)

-- | Analyze ownership in a file (stub implementation)
analyzeOwnership :: a -> OwnershipAnalysis
analyzeOwnership _ = OwnershipAnalysis [] [] Map.empty

-- | Check ownership transfer (stub implementation)
checkOwnershipTransfer :: OwnershipTransfer -> Either String OwnershipTransfer
checkOwnershipTransfer transfer = Right transfer

-- | Validate ownership constraints (stub implementation)
validateOwnershipConstraints :: [OwnershipConstraint] -> Either String [OwnershipConstraint]
validateOwnershipConstraints constraints = Right constraints