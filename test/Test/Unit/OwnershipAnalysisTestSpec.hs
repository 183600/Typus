{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.OwnershipAnalysisTestSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Set as Set
import Data.List (nub, (\\))
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Ownership.Common.Types as Own

-- Helper generators for ownership analysis tests
genVarName :: Gen String
genVarName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
  return (first : rest)

genOwnershipType :: Gen Own.OwnershipType
genOwnershipType = oneof
  [ Own.Owned <$> genVarName
  , Own.Borrowed <$> genVarName
  , Own.MutBorrowed <$> genVarName
  ]

genOwnershipError :: Gen Own.OwnershipError
genOwnershipError = oneof
  [ Own.UseAfterMove <$> genVarName
  , Own.DoubleMove <$> genVarName <*> genVarName
  , Own.BorrowWhileMoved <$> genVarName
  , Own.MutBorrowWhileBorrowed <$> genVarName
  , Own.BorrowWhileMutBorrowed <$> genVarName
  , Own.MultipleMutBorrows <$> genVarName
  , Own.UseWhileMutBorrowed <$> genVarName
  , Own.OutOfScope <$> genVarName
  , Own.BorrowError <$> listOf (elements ['a'..'z'])
  , Own.ParseError <$> listOf (elements ['a'..'z'])
  , Own.CrossFunctionMove <$> genVarName <*> genVarName
  , Own.ParameterMoveMismatch <$> genVarName
  , Own.ControlFlowError <$> listOf (elements ['a'..'z'])
  , Own.PathSensitiveError <$> listOf (elements ['a'..'z'])
  , Own.LoopOwnershipError <$> listOf (elements ['a'..'z'])
  , Own.OwnershipError <$> listOf (elements ['a'..'z'])
  ]

genOwnershipTransfer :: Gen Own.OwnershipTransfer
genOwnershipTransfer = do
  fromVar <- genVarName
  toVar <- genVarName
  return $ Own.OwnershipTransfer fromVar toVar

instance Arbitrary Own.OwnershipType where
  arbitrary = genOwnershipType

instance Arbitrary Own.OwnershipError where
  arbitrary = genOwnershipError

instance Arbitrary Own.OwnershipTransfer where
  arbitrary = genOwnershipTransfer

-- Test properties for ownership analysis

-- Property 1: Ownership types have consistent ordering
prop_ownershipTypeOrdering :: Own.OwnershipType -> Own.OwnershipType -> Property
prop_ownershipTypeOrdering ot1 ot2 = property $
  let comparison = compare ot1 ot2
      reverseComparison = compare ot2 ot1
  in if comparison == EQ 
     then reverseComparison == EQ
     else comparison /= reverseComparison  -- Different elements should have different orderings

-- Property 2: Owned types are greater than borrowed types
prop_ownedGreaterThanBorrowed :: String -> Property
prop_ownedGreaterThanBorrowed name = property $
  let owned = Own.Owned name
      borrowed = Own.Borrowed name
      mutBorrowed = Own.MutBorrowed name
  in owned > borrowed && owned > mutBorrowed

-- Property 3: Borrowed types are greater than mutably borrowed types
prop_borrowedGreaterThanMutBorrowed :: String -> Property
prop_borrowedGreaterThanMutBorrowed name = property $
  let borrowed = Own.Borrowed name
      mutBorrowed = Own.MutBorrowed name
  in borrowed > mutBorrowed

-- Property 4: Ownership errors have consistent ordering
prop_ownershipErrorOrdering :: Own.OwnershipError -> Own.OwnershipError -> Bool
prop_ownershipErrorOrdering err1 err2 =
  let comparison = compare err1 err2
      reverseComparison = compare err2 err1
  in if comparison == EQ 
     then reverseComparison == EQ
     else comparison /= reverseComparison  -- Different elements should have different orderings

-- Property 5: Ownership transfers preserve source and destination
prop_ownershipTransferPreservation :: String -> String -> Bool
prop_ownershipTransferPreservation from to =
  let transfer = Own.OwnershipTransfer from to
  in Own.transferFrom transfer == from && Own.transferTo transfer == to

-- Property 6: Ownership analyzer creation is consistent
prop_ownershipAnalyzerCreation :: Bool
prop_ownershipAnalyzerCreation =
  let analyzer1 = Own.newOwnershipAnalyzer
      analyzer2 = Own.newOwnershipAnalyzer
  in analyzer1 == analyzer2

-- Property 7: Ownership types with different names are different
prop_ownershipTypeDifferentNames :: String -> String -> Property
prop_ownershipTypeDifferentNames name1 name2 =
  name1 /= name2 ==> 
    let owned1 = Own.Owned name1
        owned2 = Own.Owned name2
    in owned1 /= owned2

-- Property 8: Ownership errors with different messages are different
prop_ownershipErrorDifferentMessages :: String -> String -> Property
prop_ownershipErrorDifferentMessages msg1 msg2 =
  msg1 /= msg2 ==> 
    let err1 = Own.OwnershipError msg1
        err2 = Own.OwnershipError msg2
    in err1 /= err2

ownershipAnalysisTests :: TestTree
ownershipAnalysisTests = testGroup "Ownership Analysis Tests"
  [ testProperties "Ownership Type Properties"
    [ ("Ownership types have consistent ordering", property prop_ownershipTypeOrdering)
    , ("Owned types are greater than borrowed types", property prop_ownedGreaterThanBorrowed)
    , ("Borrowed types are greater than mutably borrowed types", property prop_borrowedGreaterThanMutBorrowed)
    , ("Ownership types with different names are different", property prop_ownershipTypeDifferentNames)
    ]
  , testProperties "Ownership Error Properties"
    [ ("Ownership errors have consistent ordering", property prop_ownershipErrorOrdering)
    , ("Ownership errors with different messages are different", property prop_ownershipErrorDifferentMessages)
    ]
  , testProperties "Ownership Transfer Properties"
    [ ("Ownership transfers preserve source and destination", property prop_ownershipTransferPreservation)
    ]
  , testProperties "Ownership Analyzer Properties"
    [ ("Ownership analyzer creation is consistent", property prop_ownershipAnalyzerCreation)
    ]
  ]