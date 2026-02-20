{-# LANGUAGE OverloadedStrings #-}
-- Orphan instances are acceptable in test code to keep test utilities separate from production code
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Dependencies.Arbitrary () where

import Test.QuickCheck (Arbitrary(..), oneof, elements, listOf1, choose)

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )

import Dependencies.AST (AST(..), Statement(..), TypeExpr(..), Constraint(..))

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Dependencies.TypeSystem (TypeVar(..), TypeConstraint(..))

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )

instance Arbitrary TypeVar where
  arbitrary = elements [TVCon "int", TVCon "string", TVCon "bool", TVVar "T"]

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ TypeSizeGE <$> arbitrary <*> choose (0, 100)
    , TypeSizeGT <$> arbitrary <*> choose (0, 100)
    , TypeRange <$> arbitrary <*> choose (0, 50) <*> choose (51, 100)
    , Equal <$> arbitrary <*> arbitrary
    , Predicate <$> elements ["Ord", "Eq", "Show"] <*> listOf1 arbitrary
    ]

instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ SimpleT <$> elements ["int", "string", "bool", "float"]
    , GenericT <$> elements ["List", "Map", "Set"] <*> listOf1 arbitrary
    , FuncT <$> listOf1 ((,) <$> elements ["x", "y", "z"] <*> arbitrary) <*> arbitrary
    , RefineT <$> arbitrary <*> listOf1 arbitrary
    ]

instance Arbitrary Constraint where
  arbitrary = oneof
    [ SizeGT <$> elements ["x", "y", "z"] <*> choose (0, 100)
    , SizeGE <$> elements ["x", "y", "z"] <*> choose (0, 100)
    , RangeC <$> elements ["x", "y", "z"] <*> choose (0, 50) <*> choose (51, 100)
    , PredC <$> elements ["Ord", "Eq", "Show"] <*> listOf1 arbitrary
    ]

instance Arbitrary Statement where
  arbitrary = oneof
    [ pure (STypeDef "Buffer" ["T"] [])
    , pure (STypeAlias "IntAlias" (SimpleT "int") [])
    , pure (SVarDecl "input" (SimpleT "int"))
    , pure (SFuncDecl "idInt" [("value", SimpleT "int")] (Just (SimpleT "int")))
    ]

instance Arbitrary AST where
  arbitrary = do
    stmts <- listOf1 arbitrary
    pure (Program stmts)
