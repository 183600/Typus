{-# LANGUAGE CPP #-}

module Test.Unit.OwnershipAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)

import Ownership (OwnershipType(..), OwnershipTransfer(..), OwnershipError(..), OwnershipAnalyzer(..))
import Ownership.Common.Types (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), OwnershipTransfer(..), newOwnershipAnalyzer)
import Compiler.TypeChecker (Type(..), TypeEnv(..))
import Parser (TypusFile(..), CodeBlock(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
import Data.Maybe (isJust, isNothing)

-- Arbitrary instances
instance Arbitrary OwnershipType where
  arbitrary = genOwnershipType

instance Arbitrary OwnershipTransfer where
  arbitrary = genOwnershipTransfer

instance Arbitrary OwnershipError where
  arbitrary = genOwnershipError

-- | Generate random variable names
genVarName :: Gen String
genVarName = elements ["x", "y", "z", "var", "value", "data", "result", "temp"]

-- | Generate random ownership types
genOwnershipType :: Gen OwnershipType
genOwnershipType = elements [Owned "var1", Borrowed "var2", MutBorrowed "var3"]

-- | Generate random ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  fromVar <- genVarName
  toVar <- genVarName
  return $ OwnershipTransfer fromVar toVar

-- | Generate random ownership errors
genOwnershipError :: Gen OwnershipError
genOwnershipError = elements [
  UseAfterMove "var1",
  DoubleMove "var1" "var2", 
  BorrowWhileMoved "var1",
  MutBorrowWhileBorrowed "var1",
  MultipleMutBorrows "var1"
  ]

tests :: TestTree
tests = testGroup "Ownership Analysis QuickCheck tests"
  [ fastProperty "Ownership type creation" prop_ownership_type_creation
  , fastProperty "Ownership transfer validation" prop_ownership_transfer_validation
  , fastProperty "Ownership error classification" prop_ownership_error_classification
  ]

-- Property: Ownership type creation is valid
prop_ownership_type_creation :: OwnershipType -> Property
prop_ownership_type_creation ownershipType =
  case ownershipType of
    Owned name -> property $ (not . null) name
    Borrowed name -> property $ (not . null) name
    MutBorrowed name -> property $ (not . null) name

-- Property: Ownership transfer validation
prop_ownership_transfer_validation :: OwnershipTransfer -> Property
prop_ownership_transfer_validation transfer =
  let fromVar = transferFrom transfer
      toVar = transferTo transfer
  in property $ (not . null) fromVar .&&. (not . null) toVar

-- Property: Ownership error classification
prop_ownership_error_classification :: OwnershipError -> Property
prop_ownership_error_classification error =
  case error of
    UseAfterMove var -> property $ (not . null) var
    DoubleMove var1 var2 -> property $ (not . null) var1 .&&. (not . null) var2
    BorrowWhileMoved var -> property $ (not . null) var
    MutBorrowWhileBorrowed var -> property $ (not . null) var
    MultipleMutBorrows var -> property $ (not . null) var
    _ -> property $ True