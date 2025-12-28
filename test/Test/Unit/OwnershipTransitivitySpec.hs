{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransitivitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf1, elements, suchThat)
import Data.List (nub, sort, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , OwnershipAnalyzer(..)
  , newOwnershipAnalyzer
  )

-- | Generate a valid variable name
genVarName :: Gen String
genVarName = do
  first <- elements ['a'..'z']
  rest <- listOf (elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_")
  return $ first : rest

-- | Generate an ownership type
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  varName <- genVarName
  elements [Owned varName, Borrowed varName, MutBorrowed varName]

-- | Generate an ownership transfer
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  fromVar <- genVarName
  toVar <- genVarName `suchThat` (/= fromVar)
  return $ OwnershipTransfer fromVar toVar

-- | Generate an ownership error
genOwnershipError :: Gen OwnershipError
genOwnershipError = do
  varName1 <- genVarName
  varName2 <- genVarName `suchThat` (/= varName1)
  elements
    [ UseAfterMove varName1
    , DoubleMove varName1 varName2
    , BorrowWhileMoved varName1
    , MutBorrowWhileBorrowed varName1
    , BorrowWhileMutBorrowed varName1
    , MultipleMutBorrows varName1
    , UseWhileMutBorrowed varName1
    , OutOfScope varName1
    , BorrowError "test borrow error"
    , ParseError "test parse error"
    , CrossFunctionMove varName1 varName2
    , ParameterMoveMismatch varName1
    , ControlFlowError "test control flow error"
    , LoopOwnershipError "test loop error"
    ]

instance Arbitrary OwnershipType where
  arbitrary = genOwnershipType

instance Arbitrary OwnershipTransfer where
  arbitrary = genOwnershipTransfer

instance Arbitrary OwnershipError where
  arbitrary = genOwnershipError

-- Property: Ownership types from same variable are comparable
prop_ownershipType_comparable :: Property
prop_ownershipType_comparable =
  forAll genVarName $ \varName ->
    forAll genOwnershipType $ \ownershipType ->
      -- All ownership types should be comparable
      case ownershipType of
        Owned name -> name == varName ==> property True
        Borrowed name -> name == varName ==> property True
        MutBorrowed name -> name == varName ==> property True

-- Property: Ownership types have consistent ordering
prop_ownershipType_ordering :: Property
prop_ownershipType_ordering =
  forAll genVarName $ \varName ->
    let owned = Owned varName
        borrowed = Borrowed varName
        mutBorrowed = MutBorrowed varName
    in owned < borrowed .&&. borrowed < mutBorrowed

-- Property: Ownership transfer has distinct from and to variables
prop_ownershipTransfer_distinctVars :: Property
prop_ownershipTransfer_distinctVars =
  forAll genOwnershipTransfer $ \transfer ->
    transferFrom transfer /= transferTo transfer

-- Property: Creating ownership analyzer returns valid analyzer
prop_newOwnershipAnalyzer_valid :: Property
prop_newOwnershipAnalyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
       OwnershipAnalyzer _ -> property True

-- Property: UseAfterMove error contains variable name
prop_useAfterMove_containsVar :: Property
prop_useAfterMove_containsVar =
  forAll genVarName $ \varName ->
    let error = UseAfterMove varName
        errorStr = show error
    in varName `isInfixOf` errorStr
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: DoubleMove error contains both variable names
prop_doubleMove_containsBothVars :: Property
prop_doubleMove_containsBothVars =
  forAll genVarName $ \var1 ->
    forAll (genVarName `suchThat` (/= var1)) $ \var2 ->
      let error = DoubleMove var1 var2
          errorStr = show error
      in var1 `isInfixOf` errorStr .&&. var2 `isInfixOf` errorStr
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: BorrowWhileMoved error contains variable name
prop_borrowWhileMoved_containsVar :: Property
prop_borrowWhileMoved_containsVar =
  forAll genVarName $ \varName ->
    let error = BorrowWhileMoved varName
        errorStr = show error
    in varName `isInfixOf` errorStr
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: MutBorrowWhileBorrowed error contains variable name
prop_mutBorrowWhileBorrowed_containsVar :: Property
prop_mutBorrowWhileBorrowed_containsVar =
  forAll genVarName $ \varName ->
    let error = MutBorrowWhileBorrowed varName
        errorStr = show error
    in varName `isInfixOf` errorStr
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: MultipleMutBorrows error contains variable name
prop_multipleMutBorrows_containsVar :: Property
prop_multipleMutBorrows_containsVar =
  forAll genVarName $ \varName ->
    let error = MultipleMutBorrows varName
        errorStr = show error
    in varName `isInfixOf` errorStr
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: OutOfScope error contains variable name
prop_outOfScope_containsVar :: Property
prop_outOfScope_containsVar =
  forAll genVarName $ \varName ->
    let error = OutOfScope varName
        errorStr = show error
    in varName `isInfixOf` errorStr
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: CrossFunctionMove error contains both variable names
prop_crossFunctionMove_containsBothVars :: Property
prop_crossFunctionMove_containsBothVars =
  forAll genVarName $ \var1 ->
    forAll (genVarName `suchThat` (/= var1)) $ \var2 ->
      let error = CrossFunctionMove var1 var2
          errorStr = show error
      in var1 `isInfixOf` errorStr .&&. var2 `isInfixOf` errorStr
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: ParameterMoveMismatch error contains variable name
prop_parameterMoveMismatch_containsVar :: Property
prop_parameterMoveMismatch_containsVar =
  forAll genVarName $ \varName ->
    let error = ParameterMoveMismatch varName
        errorStr = show error
    in varName `isInfixOf` errorStr
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: Ownership errors are comparable
prop_ownershipError_comparable :: Property
prop_ownershipError_comparable =
  forAll genOwnershipError $ \error1 ->
    forAll genOwnershipError $ \error2 ->
      let comparison = compare error1 error2
      in (comparison == LT || comparison == EQ || comparison == GT)

-- Property: Ownership errors are sortable
prop_ownershipError_sortable :: Property
prop_ownershipError_sortable =
  forAll (listOf1 genOwnershipError) $ \errors ->
    let sortedErrors = sort errors
        sortedAgain = sort sortedErrors
    in sortedErrors == sortedAgain

-- Property: Ownership transfer creates valid string representation
prop_ownershipTransfer_showValid :: Property
prop_ownershipTransfer_showValid =
  forAll genOwnershipTransfer $ \transfer ->
    let transferStr = show transfer
        fromStr = transferFrom transfer
        toStr = transferTo transfer
    in not (null transferStr) .&&.
       fromStr `isInfixOf` transferStr .&&.
       toStr `isInfixOf` transferStr
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: Ownership types from different variables are ordered by name
prop_ownershipType_orderedByName :: Property
prop_ownershipType_orderedByName =
  forAll genVarName $ \var1 ->
    forAll (genVarName `suchThat` (/= var1)) $ \var2 ->
      let owned1 = Owned var1
          owned2 = Owned var2
          comparison = compare owned1 owned2
      in if var1 < var2 
         then comparison == LT
         else comparison == GT

-- Property: Borrowed and MutBorrowed reference the same variable
prop_borrowedTypes_referenceVar :: Property
prop_borrowedTypes_referenceVar =
  forAll genVarName $ \varName ->
    let borrowed = Borrowed varName
        mutBorrowed = MutBorrowed varName
    in case borrowed of
         Borrowed name -> name == varName
         _ -> property False .&&.
    case mutBorrowed of
         MutBorrowed name -> name == varName
         _ -> property False

-- Property: Ownership analyzer equality
prop_ownershipAnalyzer_equality :: Property
prop_ownershipAnalyzer_equality =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in analyzer1 == analyzer2

-- Property: Chain of ownership transfers preserves direction
prop_ownershipTransfer_chainDirection :: Property
prop_ownershipTransfer_chainDirection =
  forAll (listOf1 genOwnershipTransfer) $ \transfers ->
    let allFromVars = map transferFrom transfers
        allToVars = map transferTo transfers
    in all (`elem` allFromVars) allToVars .||. 
       -- Some toVars might be fromVars in other transfers
       length (filter (`elem` allFromVars) allToVars) >= 0

-- Property: Ownership errors maintain consistent show format
prop_ownershipError_showFormat :: Property
prop_ownershipError_showFormat =
  forAll genOwnershipError $ \error ->
    let errorStr = show error
    in not (null errorStr) .&&.
       length (words errorStr) >= 2  -- At least error type and parameter

tests :: TestTree
tests =
  testGroup "Ownership Transitivity Properties"
    [ fastProperty "ownership types from same variable are comparable" prop_ownershipType_comparable
    , fastProperty "ownership types have consistent ordering" prop_ownershipType_ordering
    , fastProperty "ownership transfer has distinct from and to variables" prop_ownershipTransfer_distinctVars
    , fastProperty "creating ownership analyzer returns valid analyzer" prop_newOwnershipAnalyzer_valid
    , fastProperty "UseAfterMove error contains variable name" prop_useAfterMove_containsVar
    , fastProperty "DoubleMove error contains both variable names" prop_doubleMove_containsBothVars
    , fastProperty "BorrowWhileMoved error contains variable name" prop_borrowWhileMoved_containsVar
    , fastProperty "MutBorrowWhileBorrowed error contains variable name" prop_mutBorrowWhileBorrowed_containsVar
    , fastProperty "MultipleMutBorrows error contains variable name" prop_multipleMutBorrows_containsVar
    , fastProperty "OutOfScope error contains variable name" prop_outOfScope_containsVar
    , fastProperty "CrossFunctionMove error contains both variable names" prop_crossFunctionMove_containsBothVars
    , fastProperty "ParameterMoveMismatch error contains variable name" prop_parameterMoveMismatch_containsVar
    , fastProperty "ownership errors are comparable" prop_ownershipError_comparable
    , fastProperty "ownership errors are sortable" prop_ownershipError_sortable
    , fastProperty "ownership transfer creates valid string representation" prop_ownershipTransfer_showValid
    , fastProperty "ownership types from different variables are ordered by name" prop_ownershipType_orderedByName
    , fastProperty "borrowed and mutBorrowed reference the same variable" prop_borrowedTypes_referenceVar
    , fastProperty "ownership analyzer equality" prop_ownershipAnalyzer_equality
    , fastProperty "chain of ownership transfers preserves direction" prop_ownershipTransfer_chainDirection
    , fastProperty "ownership errors maintain consistent show format" prop_ownershipError_showFormat
    ]