{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..), Arbitrary(..), oneof, elements, Gen, suchThat)

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, length)
import Data.List (null, sort)
import Data.Char (isAlphaNum)

-- Property: OwnershipType ordering is consistent
prop_ownershipType_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownershipType_ordering ot1 ot2 =
  let ordering = compare ot1 ot2
      typeRank (Owned _) = 0
      typeRank (Borrowed _) = 1
      typeRank (MutBorrowed _) = 2
      expectedOrdering = compare (typeRank ot1) (typeRank ot2)
  in classify (ot1 == ot2) "same type" $
     classify (ot1 /= ot2) "different type" $
     property $ ordering === expectedOrdering

-- Property: OwnershipType equality is consistent
prop_ownershipType_equality :: String -> String -> OwnershipType -> Property
prop_ownershipType_equality name1 name2 baseType =
  not (null name1) ==> not (null name2) ==> 
  let ot1 = case baseType of
              Owned _ -> Owned name1
              Borrowed _ -> Borrowed name1
              MutBorrowed _ -> MutBorrowed name1
      ot2 = case baseType of
              Owned _ -> Owned name2
              Borrowed _ -> Borrowed name2
              MutBorrowed _ -> MutBorrowed name2
      areEqual = ot1 == ot2
      sameName = name1 == name2
      sameConstructor = case (ot1, ot2) of
                          (Owned _, Owned _) -> True
                          (Borrowed _, Borrowed _) -> True
                          (MutBorrowed _, MutBorrowed _) -> True
                          _ -> False
  in property $ areEqual === (sameName .&&. sameConstructor)

-- Property: OwnershipError equality is consistent
prop_ownershipError_equality :: String -> String -> OwnershipError -> Property
prop_ownershipError_equality name1 name2 baseError =
  not (null name1) ==> not (null name2) ==> 
  let error1 = case baseError of
                 UseAfterMove _ -> UseAfterMove name1
                 DoubleMove _ _ -> DoubleMove name1 name1
                 BorrowWhileMoved _ -> BorrowWhileMoved name1
                 MutBorrowWhileBorrowed _ -> MutBorrowWhileBorrowed name1
                 BorrowWhileMutBorrowed _ -> BorrowWhileMutBorrowed name1
                 MultipleMutBorrows _ -> MultipleMutBorrows name1
                 UseWhileMutBorrowed _ -> UseWhileMutBorrowed name1
                 OutOfScope _ -> OutOfScope name1
                 BorrowError _ -> BorrowError name1
                 ParseError _ -> ParseError name1
                 CrossFunctionMove _ _ -> CrossFunctionMove name1 name1
                 ParameterMoveMismatch _ -> ParameterMoveMismatch name1
                 ControlFlowError _ -> ControlFlowError name1
      error2 = case baseError of
                 UseAfterMove _ -> UseAfterMove name2
                 DoubleMove _ _ -> DoubleMove name2 name2
                 BorrowWhileMoved _ -> BorrowWhileMoved name2
                 MutBorrowWhileBorrowed _ -> MutBorrowWhileBorrowed name2
                 BorrowWhileMutBorrowed _ -> BorrowWhileMutBorrowed name2
                 MultipleMutBorrows _ -> MultipleMutBorrows name2
                 UseWhileMutBorrowed _ -> UseWhileMutBorrowed name2
                 OutOfScope _ -> OutOfScope name2
                 BorrowError _ -> BorrowError name2
                 ParseError _ -> ParseError name2
                 CrossFunctionMove _ _ -> CrossFunctionMove name2 name2
                 ParameterMoveMismatch _ -> ParameterMoveMismatch name2
                 ControlFlowError _ -> ControlFlowError name2
      areEqual = error1 == error2
  in classify (name1 == name2) "same names" $
     classify (name1 /= name2) "different names" $
     property $ areEqual === (name1 == name2)

-- Property: newOwnershipAnalyzer creates valid analyzer
prop_newOwnershipAnalyzer_valid :: Property
prop_newOwnershipAnalyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in property $ 
    case analyzer of
      OwnershipAnalyzer _ -> True

-- Property: OwnershipType Show roundtrip
prop_ownershipType_show_roundtrip :: String -> OwnershipType -> Property
prop_ownershipType_show_roundtrip name baseType =
  not (null name) ==> L.all (\c -> isAlphaNum c || c == '_') name ==>
  let ot = case baseType of
             Owned _ -> Owned name
             Borrowed _ -> Borrowed name
             MutBorrowed _ -> MutBorrowed name
      shown = show ot
      hasCorrectName = name `L.isInfixOf` shown
      hasCorrectType = case baseType of
                         Owned _ -> "Owned" `L.isPrefixOf` shown
                         Borrowed _ -> "Borrowed" `L.isPrefixOf` shown
                         MutBorrowed _ -> "MutBorrowed" `L.isPrefixOf` shown
  in property $ hasCorrectName .&&. hasCorrectType

-- Property: OwnershipError Show contains relevant information
prop_ownershipError_show_contains_info :: String -> OwnershipError -> Property
prop_ownershipError_show_contains_info name baseError =
  not (null name) ==> 
  let error = case baseError of
                UseAfterMove _ -> UseAfterMove name
                DoubleMove _ _ -> DoubleMove name name
                BorrowWhileMoved _ -> BorrowWhileMoved name
                MutBorrowWhileBorrowed _ -> MutBorrowWhileBorrowed name
                BorrowWhileMutBorrowed _ -> BorrowWhileMutBorrowed name
                MultipleMutBorrows _ -> MultipleMutBorrows name
                UseWhileMutBorrowed _ -> UseWhileMutBorrowed name
                OutOfScope _ -> OutOfScope name
                BorrowError _ -> BorrowError name
                ParseError _ -> ParseError name
                CrossFunctionMove _ _ -> CrossFunctionMove name name
                ParameterMoveMismatch _ -> ParameterMoveMismatch name
                ControlFlowError _ -> ControlFlowError name
      shown = show error
      containsName = name `L.isInfixOf` shown
  in property $ containsName

-- Property: OwnershipTransfer contains source L.and target
prop_ownershipTransfer_has_source_target :: String -> String -> Property
prop_ownershipTransfer_has_source_target source target =
  not (null source) ==> not (null target) ==> 
  let transfer = OwnershipTransfer source target
  in property $ 
    case transfer of
      OwnershipTransfer s t -> s === source .&&. t === target

-- Property: OwnershipTransfer equality is consistent
prop_ownershipTransfer_equality :: String -> String -> String -> String -> Property
prop_ownershipTransfer_equality source1 target1 source2 target2 =
  not (null source1) ==> not (null target1) ==> not (null source2) ==> not (null target2) ==> 
  let transfer1 = OwnershipTransfer source1 target1
      transfer2 = OwnershipTransfer source2 target2
      areEqual = transfer1 == transfer2
  in property $ areEqual === (source1 == source2 .&&. target1 == target2)

-- Property: Owned type has correct name
prop_owned_type_has_name :: String -> Property
prop_owned_type_has_name name =
  not (null name) ==> 
  let owned = Owned name
  in case owned of
       Owned n -> property $ n === name
       _ -> property $ False

-- Property: Borrowed type has correct name
prop_borrowed_type_has_name :: String -> Property
prop_borrowed_type_has_name name =
  not (null name) ==> 
  let borrowed = Borrowed name
  in case borrowed of
       Borrowed n -> property $ n === name
       _ -> property $ False

-- Property: MutBorrowed type has correct name
prop_mutBorrowed_type_has_name :: String -> Property
prop_mutBorrowed_type_has_name name =
  not (null name) ==> 
  let mutBorrowed = MutBorrowed name
  in case mutBorrowed of
       MutBorrowed n -> property $ n === name
       _ -> property $ False

-- Property: UseAfterMove error contains variable name
prop_useAfterMove_contains_name :: String -> Property
prop_useAfterMove_contains_name name =
  not (null name) ==> 
  let error = UseAfterMove name
  in case error of
       UseAfterMove n -> property $ n === name
       _ -> property $ False

-- Property: DoubleMove error contains both names
prop_doubleMove_contains_names :: String -> String -> Property
prop_doubleMove_contains_names name1 name2 =
  not (null name1) ==> not (null name2) ==> 
  let error = DoubleMove name1 name2
  in case error of
       DoubleMove n1 n2 -> property $ n1 === name1 .&&. n2 === name2
       _ -> property $ False

-- Property: CrossFunctionMove error contains function L.and variable names
prop_crossFunctionMove_contains_names :: String -> String -> Property
prop_crossFunctionMove_contains_names funcName varName =
  not (null funcName) ==> not (null varName) ==> 
  let error = CrossFunctionMove funcName varName
  in case error of
       CrossFunctionMove f v -> property $ f === funcName .&&. v === varName
       _ -> property $ False

-- Property: OwnershipType sorting preserves order
prop_ownershipType_sorting :: [OwnershipType] -> Property
prop_ownershipType_sorting types =
  let sorted = sort types
      isSorted = L.all (\(a, b) -> a <= b) (zip sorted (drop 1 sorted))
  in property $ isSorted

-- Property: OwnershipError types are distinguishable
prop_ownershipError_distinguishable :: String -> OwnershipError -> OwnershipError -> Property
prop_ownershipError_distinguishable name error1 error2 =
  not (null name) ==> error1 /= error2 ==>
  let e1 = case error1 of
             UseAfterMove _ -> UseAfterMove name
             DoubleMove _ _ -> DoubleMove name name
             BorrowWhileMoved _ -> BorrowWhileMoved name
             MutBorrowWhileBorrowed _ -> MutBorrowWhileBorrowed name
             BorrowWhileMutBorrowed _ -> BorrowWhileMutBorrowed name
             MultipleMutBorrows _ -> MultipleMutBorrows name
             UseWhileMutBorrowed _ -> UseWhileMutBorrowed name
             OutOfScope _ -> OutOfScope name
             BorrowError _ -> BorrowError name
             ParseError _ -> ParseError name
             CrossFunctionMove _ _ -> CrossFunctionMove name name
             ParameterMoveMismatch _ -> ParameterMoveMismatch name
             ControlFlowError _ -> ControlFlowError name
      e2 = case error2 of
             UseAfterMove _ -> UseAfterMove name
             DoubleMove _ _ -> DoubleMove name name
             BorrowWhileMoved _ -> BorrowWhileMoved name
             MutBorrowWhileBorrowed _ -> MutBorrowWhileBorrowed name
             BorrowWhileMutBorrowed _ -> BorrowWhileMutBorrowed name
             MultipleMutBorrows _ -> MultipleMutBorrows name
             UseWhileMutBorrowed _ -> UseWhileMutBorrowed name
             OutOfScope _ -> OutOfScope name
             BorrowError _ -> BorrowError name
             ParseError _ -> ParseError name
             CrossFunctionMove _ _ -> CrossFunctionMove name name
             ParameterMoveMismatch _ -> ParameterMoveMismatch name
             ControlFlowError _ -> ControlFlowError name
  in property $ e1 /= e2

tests :: TestTree
tests =
  testGroup "Ownership Transfer Core QuickCheck Tests"
    [ fastProperty "OwnershipType ordering is consistent" prop_ownershipType_ordering
    , fastProperty "OwnershipType equality is consistent" prop_ownershipType_equality
    , fastProperty "OwnershipError equality is consistent" prop_ownershipError_equality
    , fastProperty "newOwnershipAnalyzer creates valid analyzer" prop_newOwnershipAnalyzer_valid
    , fastProperty "OwnershipType Show roundtrip" prop_ownershipType_show_roundtrip
    , fastProperty "OwnershipError Show contains relevant information" prop_ownershipError_show_contains_info
    , fastProperty "OwnershipTransfer has source L.and target" prop_ownershipTransfer_has_source_target
    , fastProperty "OwnershipTransfer equality is consistent" prop_ownershipTransfer_equality
    , fastProperty "Owned type has correct name" prop_owned_type_has_name
    , fastProperty "Borrowed type has correct name" prop_borrowed_type_has_name
    , fastProperty "MutBorrowed type has correct name" prop_mutBorrowed_type_has_name
    , fastProperty "UseAfterMove error contains variable name" prop_useAfterMove_contains_name
    , fastProperty "DoubleMove error contains both names" prop_doubleMove_contains_names
    , fastProperty "CrossFunctionMove error contains function L.and variable names" prop_crossFunctionMove_contains_names
    , fastProperty "OwnershipType sorting preserves order" prop_ownershipType_sorting
    , fastProperty "OwnershipError types are distinguishable" prop_ownershipError_distinguishable
    ]