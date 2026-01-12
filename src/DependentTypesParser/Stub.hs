{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Stub module for dependent types testing
module DependentTypesParser.Stub (
  DependentType(..),
  TypeConstraint(..),
  DependentTypeChecker(..),
  parseDependentType,
  checkTypeConstraints,
  validateDependentType
) where

import GHC.Generics (Generic)

-- | Type constraint for dependent types
data TypeConstraint = TypeConstraint
  { tcName :: String
  , tcValue :: String
  } deriving (Eq, Ord, Show, Generic)

-- | Dependent type definition
data DependentType = DependentType
  { dtName :: String
  , dtBaseType :: String
  , dtConstraints :: [TypeConstraint]
  } deriving (Eq, Show, Generic)

-- | Dependent type checker
data DependentTypeChecker = DependentTypeChecker
  { dtcTypes :: [DependentType]
  , dtcConstraints :: [TypeConstraint]
  } deriving (Eq, Show, Generic)

-- | Parse a dependent type (stub implementation)
parseDependentType :: String -> Either String DependentType
parseDependentType _input = Right $ DependentType "stub" "StubType" []

-- | Check type constraints (stub implementation)
checkTypeConstraints :: DependentType -> [TypeConstraint] -> Either String [TypeConstraint]
checkTypeConstraints _ constraints = Right constraints

-- | Validate a dependent type (stub implementation)
validateDependentType :: DependentType -> Either String DependentType
validateDependentType dt = Right dt