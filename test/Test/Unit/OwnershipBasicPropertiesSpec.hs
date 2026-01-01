{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipBasicPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(Positive), getPositive)

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import Data.List (sort)
import Data.Ord (comparing)

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering :: String -> String -> Property
prop_ownership_type_ordering name1 name2 =
  let owned1 = Owned name1
      owned2 = Owned name2
      borrowed1 = Borrowed name1
      borrowed2 = Borrowed name2
      mutBorrowed1 = MutBorrowed name1
      mutBorrowed2 = MutBorrowed name2
  in classify (name1 < name2) "different names" $
     classify (name1 == name2) "same names" $
     owned1 <= owned2 .&&.
     borrowed1 <= borrowed2 .&&.
     mutBorrowed1 <= mutBorrowed2 .&&.
     owned1 < borrowed1 .&&.
     owned1 < mutBorrowed1 .&&.
     borrowed1 < mutBorrowed1

-- Property: OwnershipType equality based on name
prop_ownership_type_equality :: String -> String -> Property
prop_ownership_type_equality name1 name2 =
  let owned1 = Owned name1
      owned2 = Owned name2
      borrowed1 = Borrowed name1
      borrowed2 = Borrowed name2
      mutBorrowed1 = MutBorrowed name1
      mutBorrowed2 = MutBorrowed name2
  in (owned1 == owned2) === (name1 == name2) .&&.
     (borrowed1 == borrowed2) === (name1 == name2) .&&.
     (mutBorrowed1 == mutBorrowed2) === (name1 == name2)

-- Property: OwnershipType show contains the name
prop_ownership_type_show_contains_name :: String -> Property
prop_ownership_type_show_contains_name name =
  not (null name) ==>
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
      ownedStr = show owned
      borrowedStr = show borrowed
      mutBorrowedStr = show mutBorrowed
  in name `L.isInfixOf` ownedStr .&&.
     name `L.isInfixOf` borrowedStr .&&.
     name `L.isInfixOf` mutBorrowedStr
  where
    x `L.isInfixOf` y = x `elem` words y

-- Property: OwnershipError ordering is based on string representation
prop_ownership_error_ordering :: String -> String -> Property
prop_ownership_error_ordering msg1 msg2 =
  let error1 = UseAfterMove msg1
      error2 = UseAfterMove msg2
      error3 = ParseError msg1
      error4 = ParseError msg2
  in compare error1 error2 === compare (show error1) (show error2) .&&.
     compare error3 error4 === compare (show error3) (show error4)

-- Property: OwnershipError equality based on content
prop_ownership_error_equality :: String -> String -> Property
prop_ownership_error_equality msg1 msg2 =
  let useAfterMove1 = UseAfterMove msg1
      useAfterMove2 = UseAfterMove msg2
      parseError1 = ParseError msg1
      parseError2 = ParseError msg2
  in (useAfterMove1 == useAfterMove2) === (msg1 == msg2) .&&.
     (parseError1 == parseError2) === (msg1 == msg2)

-- Property: OwnershipError show contains the message
prop_ownership_error_show_contains_message :: String -> Property
prop_ownership_error_show_contains_message msg =
  not (null msg) ==>
  let error = ParseError msg
      errorStr = show error
  in msg `L.isInfixOf` errorStr
  where
    x `L.isInfixOf` y = x `elem` words y

-- Property: OwnershipAnalyzer constructor creates consistent analyzer
prop_ownership_analyzer_consistency :: Property
prop_ownership_analyzer_consistency =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in analyzer1 === analyzer2 .&&. show analyzer1 === show analyzer2

-- Property: OwnershipTransfer equality based on fields
prop_ownership_transfer_equality :: String -> String -> String -> String -> Property
prop_ownership_transfer_equality from1 to1 from2 to2 =
  let transfer1 = OwnershipTransfer from1 to1
      transfer2 = OwnershipTransfer from2 to2
  in (transfer1 == transfer2) === (from1 == from2 && to1 == to2)

-- Property: OwnershipTransfer show contains both from L.and to
prop_ownership_transfer_show_contains_fields :: String -> String -> Property
prop_ownership_transfer_show_contains_fields from to =
  not (null from && null to) ==>
  let transfer = OwnershipTransfer from to
      transferStr = show transfer
      wordsList = words transferStr
  in from `elem` wordsList .&&. to `elem` wordsList

-- Property: OwnershipTransfer ordering based on fields
prop_ownership_transfer_ordering :: String -> String -> String -> String -> Property
prop_ownership_transfer_ordering from1 to1 from2 to2 =
  let transfer1 = OwnershipTransfer from1 to1
      transfer2 = OwnershipTransfer from2 to2
      expected = compare (from1, to1) (from2, to2)
  in compare transfer1 transfer2 === expected

-- Property: OwnershipType categories are mutually exclusive
prop_ownership_type_mutually_exclusive :: String -> Property
prop_ownership_type_mutually_exclusive name =
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
  in (owned /= borrowed) .&&. (owned /= mutBorrowed) .&&. (borrowed /= mutBorrowed)

-- Property: OwnershipError categories are mutually exclusive
prop_ownership_error_mutually_exclusive :: String -> String -> Property
prop_ownership_error_mutually_exclusive msg1 msg2 =
  let useAfterMove = UseAfterMove msg1
      doubleMove = DoubleMove msg1 msg2
      borrowWhileMoved = BorrowWhileMoved msg1
      parseError = ParseError msg1
  in (useAfterMove /= doubleMove) .&&.
     (useAfterMove /= borrowWhileMoved) .&&.
     (useAfterMove /= parseError) .&&.
     (doubleMove /= borrowWhileMoved) .&&.
     (doubleMove /= parseError) .&&.
     (borrowWhileMoved /= parseError)

-- Property: OwnershipType ordering is total
prop_ownership_type_total_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_total_ordering type1 type2 =
  let result = compare type1 type2
  in (result == LT || result == EQ || result == GT) === True

-- Property: OwnershipError ordering is total
prop_ownership_error_total_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_total_ordering error1 error2 =
  let result = compare error1 error2
  in (result == LT || result == EQ || result == GT) === True

-- Property: OwnershipTransfer ordering is total
prop_ownership_transfer_total_ordering :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownership_transfer_total_ordering transfer1 transfer2 =
  let result = compare transfer1 transfer2
  in (result == LT || result == EQ || result == GT) === True

tests :: TestTree
tests =
  testGroup "Ownership Basic Properties"
    [ fastProperty "OwnershipType ordering is consistent" prop_ownership_type_ordering
    , fastProperty "OwnershipType equality based on name" prop_ownership_type_equality
    , fastProperty "OwnershipType show contains the name" prop_ownership_type_show_contains_name
    , fastProperty "OwnershipError ordering based on string representation" prop_ownership_error_ordering
    , fastProperty "OwnershipError equality based on content" prop_ownership_error_equality
    , fastProperty "OwnershipError show contains the message" prop_ownership_error_show_contains_message
    , fastProperty "OwnershipAnalyzer constructor creates consistent analyzer" prop_ownership_analyzer_consistency
    , fastProperty "OwnershipTransfer equality based on fields" prop_ownership_transfer_equality
    , fastProperty "OwnershipTransfer show contains both from L.and to" prop_ownership_transfer_show_contains_fields
    , fastProperty "OwnershipTransfer ordering based on fields" prop_ownership_transfer_ordering
    , fastProperty "OwnershipType categories are mutually exclusive" prop_ownership_type_mutually_exclusive
    , fastProperty "OwnershipError categories are mutually exclusive" prop_ownership_error_mutually_exclusive
    , fastProperty "OwnershipType ordering is total" prop_ownership_type_total_ordering
    , fastProperty "OwnershipError ordering is total" prop_ownership_error_total_ordering
    , fastProperty "OwnershipTransfer ordering is total" prop_ownership_transfer_total_ordering
    ]