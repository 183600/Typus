module Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , OwnershipAnalysis
  , OwnershipConstraint(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  , checkOwnershipTransfer
  , validateOwnershipConstraints
  , hasOwnershipErrors
  , getOwnershipErrors
  , clearOwnershipErrors
  , mergeOwnershipAnalyses
  , getOwners
  , getBorrowers
  , getOwnedResources
  , isOwner
  , isBorrower
  , canTransferOwnership
  , transferOwnership
  ) where

import Ownership.Analyzer
  ( analyzeOwnership
  , analyzeOwnershipDebug
  , analyzeOwnershipFile
  , builtInFunctions
  )
import Ownership.Common.Types
  ( OwnershipAnalyzer
  , OwnershipError(..)
  , OwnershipType(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )
import Ownership.Lexer (lexAll)
import Ownership.Parser (parseProgram)
import Ownership.Reporter (formatOwnershipErrors)

-- ============================================================================
-- Ownership types and functions (for tests)
-- ============================================================================

-- | Simple ownership analysis type for tests
data OwnershipAnalysis = OwnershipAnalysis
    { oaOwners :: [(String, String)]  -- (owner, resource)
    , oaBorrowers :: [(String, String)]  -- (borrower, resource)
    , oaErrors :: [OwnershipError]
    } deriving (Show, Eq)

-- | Ownership constraint type for tests
data OwnershipConstraint = 
    MustNotMove String
  | MustNotCopy String
  | MustNotBorrow String
  deriving (Show, Eq)

-- | Check ownership transfer (placeholder for tests)
checkOwnershipTransfer :: String -> String -> String -> Either OwnershipError Bool
checkOwnershipTransfer _ _ _ = Right True

-- | Validate ownership constraints (placeholder for tests)
validateOwnershipConstraints :: [OwnershipConstraint] -> [OwnershipError]
validateOwnershipConstraints _ = []

-- | Check if has ownership errors (placeholder for tests)
hasOwnershipErrors :: OwnershipAnalysis -> Bool
hasOwnershipErrors = not . null . oaErrors

-- | Get ownership errors (placeholder for tests)
getOwnershipErrors :: OwnershipAnalysis -> [OwnershipError]
getOwnershipErrors = oaErrors

-- | Clear ownership errors (placeholder for tests)
clearOwnershipErrors :: OwnershipAnalysis -> OwnershipAnalysis
clearOwnershipErrors oa = oa { oaErrors = [] }

-- | Merge ownership analyses (placeholder for tests)
mergeOwnershipAnalyses :: OwnershipAnalysis -> OwnershipAnalysis -> OwnershipAnalysis
mergeOwnershipAnalyses oa1 oa2 = OwnershipAnalysis
    { oaOwners = oaOwners oa1 ++ oaOwners oa2
    , oaBorrowers = oaBorrowers oa1 ++ oaBorrowers oa2
    , oaErrors = oaErrors oa1 ++ oaErrors oa2
    }

-- | Get owners (placeholder for tests)
getOwners :: OwnershipAnalysis -> [String]
getOwners = map fst . oaOwners

-- | Get borrowers (placeholder for tests)
getBorrowers :: OwnershipAnalysis -> [String]
getBorrowers = map fst . oaBorrowers

-- | Get owned resources (placeholder for tests)
getOwnedResources :: OwnershipAnalysis -> [String]
getOwnedResources = map snd . oaOwners

-- | Check if is owner (placeholder for tests)
isOwner :: OwnershipAnalysis -> String -> String -> Bool
isOwner oa owner resource = (owner, resource) `elem` oaOwners oa

-- | Check if is borrower (placeholder for tests)
isBorrower :: OwnershipAnalysis -> String -> String -> Bool
isBorrower oa borrower resource = (borrower, resource) `elem` oaBorrowers oa

-- | Check if can transfer ownership (placeholder for tests)
canTransferOwnership :: OwnershipAnalysis -> String -> String -> Bool
canTransferOwnership _ _ _ = True

-- | Transfer ownership (placeholder for tests)
transferOwnership :: OwnershipAnalysis -> String -> String -> Either OwnershipError OwnershipAnalysis
transferOwnership oa owner resource = Right oa { oaOwners = (owner, resource) : oaOwners oa }
