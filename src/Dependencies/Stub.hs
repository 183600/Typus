{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Simple dependency analysis stub for testing
module Dependencies.Stub (
  DependencyAnalysis(..),
  Dependency(..),
  DependencyType(..),
  analyzeDependencies,
  checkCircularDependencies,
  resolveDependencyOrder,
  validateDependencies
) where

import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map

-- | Type of dependency
data DependencyType
  = FunctionDependency
  | VariableDependency
  | TypeDependency
  | ModuleDependency
  deriving (Eq, Ord, Show, Generic)

-- | Individual dependency
data Dependency = Dependency
  { dName :: String
  , dType :: DependencyType
  , dFromModule :: String
  } deriving (Eq, Ord, Show, Generic)

-- | Result of dependency analysis
data DependencyAnalysis = DependencyAnalysis
  { daDependencies :: Map String Dependency
  , daCircularDeps :: [String]
  , daOrder :: [String]
  } deriving (Eq, Show, Generic)

-- | Analyze dependencies in a file (stub implementation)
analyzeDependencies :: a -> DependencyAnalysis
analyzeDependencies _ = DependencyAnalysis Map.empty [] []

-- | Check for circular dependencies (stub implementation)
checkCircularDependencies :: [Dependency] -> [String]
checkCircularDependencies _ = []

-- | Resolve dependency order (stub implementation)
resolveDependencyOrder :: [Dependency] -> [String]
resolveDependencyOrder deps = map dName deps

-- | Validate dependencies (stub implementation)
validateDependencies :: [Dependency] -> Either String ()
validateDependencies _ = Right ()