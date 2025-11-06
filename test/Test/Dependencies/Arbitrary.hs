{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Dependencies.Arbitrary () where

import Test.QuickCheck (Arbitrary(..), oneof, elements, listOf1, choose)

import Dependencies.AST
import Dependencies.TypeSystem

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

instance Arbitrary Statement where
  arbitrary = oneof
    [ pure (STypeDef "MyType" ["T"] [])
    , pure (SVarDecl "x" (SimpleT "int"))
    , pure (SFuncDecl "id" [("x", SimpleT "T")] (Just (SimpleT "T")))
    ]

instance Arbitrary AST where
  arbitrary = do
    stmts <- listOf1 arbitrary
    pure (Program stmts)
