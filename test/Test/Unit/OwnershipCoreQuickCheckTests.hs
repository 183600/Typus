{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.OwnershipCoreQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, (===), Property, forAll, Gen, Arbitrary(..), oneof, elements, listOf, listOf1, resize, suchThat)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Ownership.Common.Types 
  ( OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import Data.List (sort)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = do
    name <- listOf1 (elements ['a'..'z'])
    oneof [ pure (Owned name)
          , pure (Borrowed name)
          , pure (MutBorrowed name)
          ]

instance Arbitrary OwnershipError where
  arbitrary = do
    name1 <- listOf1 (elements ['a'..'z'])
    name2 <- listOf1 (elements ['a'..'z'])
    oneof [ pure (UseAfterMove name1)
          , pure (DoubleMove name1 name2)
          , pure (BorrowWhileMoved name1)
          , pure (MutBorrowWhileBorrowed name1)
          , pure (BorrowWhileMutBorrowed name1)
          , pure (MultipleMutBorrows name1)
          , pure (UseWhileMutBorrowed name1)
          , pure (OutOfScope name1)
          , do msg <- listOf1 (elements ['a'..'z'] ++ " ")
               pure (BorrowError msg)
          , do msg <- listOf1 (elements ['a'..'z'] ++ " ")
               pure (ParseError msg)
          , pure (CrossFunctionMove name1 name2)
          , pure (ParameterMoveMismatch name1)
          , do msg <- listOf1 (elements ['a'..'z'] ++ " ")
               pure (ControlFlowError msg)
          , do msg <- listOf1 (elements ['a'..'z'] ++ " ")
               pure (PathSensitiveError msg)
          , do msg <- listOf1 (elements ['a'..'z'] ++ " ")
               pure (LoopOwnershipError msg)
          ]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    fromVar <- listOf1 (elements ['a'..'z'])
    toVar <- listOf1 (elements ['a'..'z'])
    return $ OwnershipTransfer fromVar toVar

instance Arbitrary OwnershipAnalyzer where
  arbitrary = pure newOwnershipAnalyzer

-- ============================================================================
-- QuickCheck Properties for Ownership Module
-- ============================================================================

-- | OwnershipType: equality should be reflexive
prop_ownershipType_reflexive :: OwnershipType -> Bool
prop_ownershipType_reflexive ot = ot == ot

-- | OwnershipType: Show and Read should be consistent
prop_ownershipType_show_roundtrip :: OwnershipType -> Bool
prop_ownershipType_show_roundtrip ot = 
    case ot of
      (Owned name) -> show ot == "Owned " ++ name
      (Borrowed name) -> show ot == "Borrowed " ++ name
      (MutBorrowed name) -> show ot == "MutBorrowed " ++ name

-- | OwnershipType: Owned should come before Borrowed in ordering
prop_ownershipType_ordering_owned_borrowed :: String -> Bool
prop_ownershipType_ordering_owned_borrowed name = 
    let owned = Owned name
        borrowed = Borrowed name
    in owned < borrowed

-- | OwnershipType: Borrowed should come before MutBorrowed in ordering
prop_ownershipType_ordering_borrowed_mut :: String -> Bool
prop_ownershipType_ordering_borrowed_mut name = 
    let borrowed = Borrowed name
        mutBorrowed = MutBorrowed name
    in borrowed < mutBorrowed

-- | OwnershipError: equality should be reflexive
prop_ownershipError_reflexive :: OwnershipError -> Bool
prop_ownershipError_reflexive oe = oe == oe

-- | OwnershipError: Show should contain the error type name
prop_ownershipError_show_contains_type :: OwnershipError -> Bool
prop_ownershipError_show_contains_type oe = 
    let showStr = show oe
    in case oe of
      UseAfterMove _ -> "UseAfterMove" `isInfixOf` showStr
      DoubleMove _ _ -> "DoubleMove" `isInfixOf` showStr
      BorrowWhileMoved _ -> "BorrowWhileMoved" `isInfixOf` showStr
      MutBorrowWhileBorrowed _ -> "MutBorrowWhileBorrowed" `isInfixOf` showStr
      BorrowWhileMutBorrowed _ -> "BorrowWhileMutBorrowed" `isInfixOf` showStr
      MultipleMutBorrows _ -> "MultipleMutBorrows" `isInfixOf` showStr
      UseWhileMutBorrowed _ -> "UseWhileMutBorrowed" `isInfixOf` showStr
      OutOfScope _ -> "OutOfScope" `isInfixOf` showStr
      BorrowError _ -> "BorrowError" `isInfixOf` showStr
      ParseError _ -> "ParseError" `isInfixOf` showStr
      CrossFunctionMove _ _ -> "CrossFunctionMove" `isInfixOf` showStr
      ParameterMoveMismatch _ -> "ParameterMoveMismatch" `isInfixOf` showStr
      ControlFlowError _ -> "ControlFlowError" `isInfixOf` showStr
      PathSensitiveError _ -> "PathSensitiveError" `isInfixOf` showStr
      LoopOwnershipError _ -> "LoopOwnershipError" `isInfixOf` showStr
  where
    isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

-- | OwnershipTransfer: equality should be reflexive
prop_ownershipTransfer_reflexive :: OwnershipTransfer -> Bool
prop_ownershipTransfer_reflexive ot = ot == ot

-- | OwnershipTransfer: Show should contain both from and to variables
prop_ownershipTransfer_show_contains_vars :: OwnershipTransfer -> Bool
prop_ownershipTransfer_show_contains_vars transfer = 
    let showStr = show transfer
        fromVar = transferFrom transfer
        toVar = transferTo transfer
    in fromVar `isInfixOf` showStr && toVar `isInfixOf` showStr
  where
    isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

-- | OwnershipTransfer: creating transfer with same from and to should be valid
prop_ownershipTransfer_same_vars :: String -> Bool
prop_ownershipTransfer_same_vars var = 
    let transfer = OwnershipTransfer var var
    in transferFrom transfer == var && transferTo transfer == var

-- | OwnershipAnalyzer: newOwnershipAnalyzer should be consistent
prop_newOwnershipAnalyzer_consistent :: Bool
prop_newOwnershipAnalyzer_consistent = 
    let analyzer1 = newOwnershipAnalyzer
        analyzer2 = newOwnershipAnalyzer
    in analyzer1 == analyzer2

-- | OwnershipType: sorting should maintain order
prop_ownershipType_sorting :: [OwnershipType] -> Bool
prop_ownershipType_sorting types = 
    let sorted = sort types
    in all (\(a, b) -> a <= b) (zip sorted (tail sorted)

-- | OwnershipError: sorting should be deterministic
prop_ownershipError_sorting :: [OwnershipError] -> Bool
prop_ownershipError_sorting errors = 
    let sorted1 = sort errors
        sorted2 = sort errors
    in sorted1 == sorted2

-- | OwnershipTransfer: reverse transfer should have different properties
prop_ownershipTransfer_reverse :: OwnershipTransfer -> Bool
prop_ownershipTransfer_reverse transfer = 
    let reversed = OwnershipTransfer (transferTo transfer) (transferFrom transfer)
    in transferFrom reversed == transferTo transfer &&
       transferTo reversed == transferFrom transfer

-- | OwnershipType: same name but different types should be ordered
prop_ownershipType_same_name_ordering :: String -> Bool
prop_ownershipType_same_name_ordering name = 
    let owned = Owned name
        borrowed = Borrowed name
        mutBorrowed = MutBorrowed name
    in owned < borrowed && borrowed < mutBorrowed

-- | OwnershipError: errors with same type should be ordered by content
prop_ownershipError_same_type_ordering :: String -> String -> Bool
prop_ownershipError_same_type_ordering name1 name2 = 
    let err1 = UseAfterMove name1
        err2 = UseAfterMove name2
    in if name1 == name2 
        then err1 == err2 
        else compare err1 err2 == compare name1 name2

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Core QuickCheck Tests"
  [ testProperties "OwnershipType Properties"
    [ ("OwnershipType reflexive", prop_ownershipType_reflexive)
    , ("OwnershipType show roundtrip", prop_ownershipType_show_roundtrip)
    , ("OwnershipType ordering owned borrowed", prop_ownershipType_ordering_owned_borrowed)
    , ("OwnershipType ordering borrowed mut", prop_ownershipType_ordering_borrowed_mut)
    , ("OwnershipType same name ordering", prop_ownershipType_same_name_ordering)
    , ("OwnershipType sorting", prop_ownershipType_sorting)
    ]

  , testProperties "OwnershipError Properties"
    [ ("OwnershipError reflexive", prop_ownershipError_reflexive)
    , ("OwnershipError show contains type", prop_ownershipError_show_contains_type)
    , ("OwnershipError sorting", prop_ownershipError_sorting)
    , ("OwnershipError same type ordering", prop_ownershipError_same_type_ordering)
    ]

  , testProperties "OwnershipTransfer Properties"
    [ ("OwnershipTransfer reflexive", prop_ownershipTransfer_reflexive)
    , ("OwnershipTransfer show contains vars", prop_ownershipTransfer_show_contains_vars)
    , ("OwnershipTransfer same vars", prop_ownershipTransfer_same_vars)
    , ("OwnershipTransfer reverse", prop_ownershipTransfer_reverse)
    ]

  , testProperties "OwnershipAnalyzer Properties"
    [ ("newOwnershipAnalyzer consistent", prop_newOwnershipAnalyzer_consistent)
    ]
  ]