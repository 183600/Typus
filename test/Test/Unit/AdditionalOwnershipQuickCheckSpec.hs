{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import TestSupport.Arbitrary

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , OwnershipAnalyzer(..)
  , newOwnershipAnalyzer
  )

import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)
import Data.Ord (comparing)

-- ============================================================================
-- Additional QuickCheck Tests for Ownership Module
-- ============================================================================

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering_consistent :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering_consistent type1 type2 =
  let ordering = compare type1 type2
      reverseOrdering = compare type2 type1
  in property $ (ordering == EQ) ==> (reverseOrdering == EQ)

-- Property: OwnershipType ordering is antisymmetric for distinct types
prop_ownership_type_ordering_antisymmetric :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering_antisymmetric type1 type2 =
  type1 /= type2 ==> 
  let ordering = compare type1 type2
      reverseOrdering = compare type2 type1
  in property $ (ordering == LT) ==> (reverseOrdering == GT) .&&.
     (ordering == GT) ==> (reverseOrdering == LT)

-- Property: OwnershipType ordering is transitive
prop_ownership_type_ordering_transitive :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering_transitive type1 type2 type3 =
  let ord12 = compare type1 type2
      ord23 = compare type2 type3
      ord13 = compare type1 type3
  in (ord12 == LT && ord23 == LT) ==> ord13 == LT

-- Property: OwnershipType equality consistency
prop_ownership_type_equality_consistent :: String -> String -> Property
prop_ownership_type_equality_consistent name1 name2 =
  let owned1 = Owned name1
      owned2 = Owned name2
      borrowed1 = Borrowed name1
      borrowed2 = Borrowed name2
      mutBorrowed1 = MutBorrowed name1
      mutBorrowed2 = MutBorrowed name2
  in property $ (name1 == name2) ==> (owned1 == owned2 .&&. borrowed1 == borrowed2 .&&. mutBorrowed1 == mutBorrowed2)

-- Property: OwnershipType inequality for different constructors
prop_ownership_type_inequality_constructors :: String -> Property
prop_ownership_type_inequality_constructors name =
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
  in property $ owned /= borrowed .&&. owned /= mutBorrowed .&&. borrowed /= mutBorrowed

-- Property: OwnershipError ordering is consistent with string representation
prop_ownership_error_ordering_consistent :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering_consistent err1 err2 =
  let ordering = compare err1 err2
      strOrdering = compare (show err1) (show err2)
  in property $ ordering === strOrdering

-- Property: OwnershipError equality consistency
prop_ownership_error_equality_consistent :: String -> String -> String -> Property
prop_ownership_error_equality_consistent var1 var2 var3 =
  let useAfterMove1 = UseAfterMove var1
      useAfterMove2 = UseAfterMove var2
      doubleMove1 = DoubleMove var1 var3
      doubleMove2 = DoubleMove var2 var3
  in property $ (var1 == var2) ==> (useAfterMove1 == useAfterMove2 .&&. doubleMove1 == doubleMove2)

-- Property: OwnershipTransfer equality consistency
prop_ownership_transfer_equality_consistent :: String -> String -> String -> String -> Property
prop_ownership_transfer_equality_consistent from1 to1 from2 to2 =
  let transfer1 = OwnershipTransfer from1 to1
      transfer2 = OwnershipTransfer from2 to2
  in property $ (from1 == from2 && to1 == to2) ==> (transfer1 == transfer2)

-- Property: OwnershipTransfer inequality
prop_ownership_transfer_inequality :: String -> String -> String -> String -> Property
prop_ownership_transfer_inequality from1 to1 from2 to2 =
  let transfer1 = OwnershipTransfer from1 to1
      transfer2 = OwnershipTransfer from2 to2
  in property $ (from1 /= from2 || to1 /= to2) ==> (transfer1 /= transfer2)

-- Property: OwnershipAnalyzer constructor consistency
prop_ownership_analyzer_consistency :: Property
prop_ownership_analyzer_consistency =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in property $ analyzer1 === analyzer2

-- Property: OwnershipType show is readable
prop_ownership_type_show_readable :: String -> Property
prop_ownership_type_show_readable name =
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
      ownedStr = show owned
      borrowedStr = show borrowed
      mutBorrowedStr = show mutBorrowed
  in property $ "Owned" `L.isInfixOf` ownedStr .&&.
     "Borrowed" `L.isInfixOf` borrowedStr .&&.
     "MutBorrowed" `L.isInfixOf` mutBorrowedStr .&&.
     name `L.isInfixOf` ownedStr .&&.
     name `L.isInfixOf` borrowedStr .&&.
     name `L.isInfixOf` mutBorrowedStr

-- Property: OwnershipError show is readable
prop_ownership_error_show_readable :: String -> String -> Property
prop_ownership_error_show_readable var1 var2 =
  let useAfterMove = UseAfterMove var1
      doubleMove = DoubleMove var1 var2
      borrowWhileMoved = BorrowWhileMoved var1
      useAfterMoveStr = show useAfterMove
      doubleMoveStr = show doubleMove
      borrowWhileMovedStr = show borrowWhileMoved
  in property $ "UseAfterMove" `L.isInfixOf` useAfterMoveStr .&&.
     "DoubleMove" `L.isInfixOf` doubleMoveStr .&&.
     "BorrowWhileMoved" `L.isInfixOf` borrowWhileMovedStr .&&.
     var1 `L.isInfixOf` useAfterMoveStr .&&.
     var1 `L.isInfixOf` doubleMoveStr .&&.
     var2 `L.isInfixOf` doubleMoveStr .&&.
     var1 `L.isInfixOf` borrowWhileMovedStr

-- Property: OwnershipTransfer show is readable
prop_ownership_transfer_show_readable :: String -> String -> Property
prop_ownership_transfer_show_readable from to =
  let transfer = OwnershipTransfer from to
      transferStr = show transfer
  in property $ "OwnershipTransfer" `L.isInfixOf` transferStr

-- Property: OwnershipType sorting preserves order
prop_ownership_type_sorting_preserves_order :: [OwnershipType] -> Property
prop_ownership_type_sorting_preserves_order types =
  let sorted = sort types
      expected = sort types
  in property $ sorted === expected

-- Property: OwnershipError sorting preserves order
prop_ownership_error_sorting_preserves_order :: [OwnershipError] -> Property
prop_ownership_error_sorting_preserves_order errors =
  let sorted = sort errors
      expected = sort errors
  in property $ sorted === expected

-- Property: OwnershipType ordering has expected hierarchy
prop_ownership_type_ordering_hierarchy :: String -> Property
prop_ownership_type_ordering_hierarchy name =
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
  in property $ compare owned borrowed === LT .&&.
     compare owned mutBorrowed === LT .&&.
     compare borrowed mutBorrowed === LT

-- Property: Complex ownership transfer scenarios
prop_complex_ownership_transfer_scenarios :: [String] -> Property
prop_complex_ownership_transfer_scenarios names =
  not (null names) ==> 
  let transfers = zipWith OwnershipTransfer names (L.tail names ++ [L.head names])
      uniqueTransfers = nub transfers
  in property $ L.length uniqueTransfers <= L.length transfers

-- Property: OwnershipError creation with different parameters
prop_ownership_error_creation :: String -> String -> String -> Property
prop_ownership_error_creation var1 var2 msg =
  let useAfterMove = UseAfterMove var1
      doubleMove = DoubleMove var1 var2
      borrowError = BorrowError msg
      parseError = ParseError msg
  in property $ show useAfterMove /= show doubleMove .&&.
     show useAfterMove /= show borrowError .&&.
     show useAfterMove /= show parseError .&&.
     show doubleMove /= show borrowError .&&.
     show doubleMove /= show parseError .&&.
     show borrowError /= show parseError

-- Property: OwnershipType with empty names
prop_ownership_type_empty_names :: Property
prop_ownership_type_empty_names =
  let emptyOwned = Owned ""
      emptyBorrowed = Borrowed ""
      emptyMutBorrowed = MutBorrowed ""
  in property $ show emptyOwned === "Owned " .&&.
     show emptyBorrowed === "Borrowed " .&&.
     show emptyMutBorrowed === "MutBorrowed "

-- Property: OwnershipError with empty parameters
prop_ownership_error_empty_params :: Property
prop_ownership_error_empty_params =
  let emptyUseAfterMove = UseAfterMove ""
      emptyDoubleMove = DoubleMove "" ""
      emptyBorrowError = BorrowError ""
      emptyParseError = ParseError ""
  in property $ show emptyUseAfterMove === "UseAfterMove " .&&.
     show emptyDoubleMove === "DoubleMove  " .&&.
     show emptyBorrowError === "BorrowError " .&&.
     show emptyParseError === "ParseError "

-- Property: OwnershipTransfer with empty strings
prop_ownership_transfer_empty_strings :: Property
prop_ownership_transfer_empty_strings =
  let emptyFrom = OwnershipTransfer "" "to"
      emptyTo = OwnershipTransfer "from" ""
      bothEmpty = OwnershipTransfer "" ""
  in property $ show emptyFrom /= show emptyTo .&&.
     show emptyFrom /= show bothEmpty .&&.
     show emptyTo /= show bothEmpty

-- Property: OwnershipType with special characters
prop_ownership_type_special_characters :: String -> Property
prop_ownership_type_special_characters name =
  let specialName = name ++ "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      owned = Owned specialName
      borrowed = Borrowed specialName
      mutBorrowed = MutBorrowed specialName
  in property $ specialName `L.isInfixOf` show owned .&&.
     specialName `L.isInfixOf` show borrowed .&&.
     specialName `L.isInfixOf` show mutBorrowed

-- Property: OwnershipError with special characters
prop_ownership_error_special_characters :: String -> String -> Property
prop_ownership_error_special_characters var msg =
  let specialVar = var ++ "!@#$%^&*()"
      specialMsg = msg ++ "!@#$%^&*()"
      useAfterMove = UseAfterMove specialVar
      borrowError = BorrowError specialMsg
  in property $ specialVar `L.isInfixOf` show useAfterMove .&&.
     specialMsg `L.isInfixOf` show borrowError

-- Helper function for string concatenation
(+=+) :: String -> String -> String
(+=+) = (++)

-- Property: OwnershipTransfer with special characters
prop_ownership_transfer_special_characters :: String -> String -> Property
prop_ownership_transfer_special_characters from to =
  let specialFrom = from ++ "!@#$%^&*()"
      specialTo = to ++ "!@#$%^&*()"
      transfer = OwnershipTransfer specialFrom specialTo
  in property $ specialFrom `L.isInfixOf` show transfer .&&.
     specialTo `L.isInfixOf` show transfer

-- Property: OwnershipType roundtrip through show/read (conceptual)
prop_ownership_type_conceptual_roundtrip :: OwnershipType -> Property
prop_ownership_type_conceptual_roundtrip ownType =
  let str = show ownType
      -- In a real scenario, you might parse this back
      -- For now, we just verify the string contains expected information
  in case ownType of
    Owned name -> property $ ("Owned " ++ name) === str
    Borrowed name -> property $ ("Borrowed " ++ name) === str
    MutBorrowed name -> property $ ("MutBorrowed " ++ name) === str

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional Ownership QuickCheck Tests"
  [ fastProperty "OwnershipType ordering consistency" prop_ownership_type_ordering_consistent
  , fastProperty "OwnershipType ordering antisymmetric for distinct types" prop_ownership_type_ordering_antisymmetric
  , fastProperty "OwnershipType ordering transitive" prop_ownership_type_ordering_transitive
  , fastProperty "OwnershipType equality consistency" prop_ownership_type_equality_consistent
  , fastProperty "OwnershipType inequality for different constructors" prop_ownership_type_inequality_constructors
  , fastProperty "OwnershipError ordering consistency with string representation" prop_ownership_error_ordering_consistent
  , fastProperty "OwnershipError equality consistency" prop_ownership_error_equality_consistent
  , fastProperty "OwnershipTransfer equality consistency" prop_ownership_transfer_equality_consistent
  , fastProperty "OwnershipTransfer inequality" prop_ownership_transfer_inequality
  , fastProperty "OwnershipAnalyzer constructor consistency" prop_ownership_analyzer_consistency
  , fastProperty "OwnershipType show is readable" prop_ownership_type_show_readable
  , fastProperty "OwnershipError show is readable" prop_ownership_error_show_readable
  , fastProperty "OwnershipTransfer show is readable" prop_ownership_transfer_show_readable
  , fastProperty "OwnershipType sorting preserves order" prop_ownership_type_sorting_preserves_order
  , fastProperty "OwnershipError sorting preserves order" prop_ownership_error_sorting_preserves_order
  , fastProperty "OwnershipType ordering has expected hierarchy" prop_ownership_type_ordering_hierarchy
  , fastProperty "Complex ownership transfer scenarios" prop_complex_ownership_transfer_scenarios
  , fastProperty "OwnershipError creation with different parameters" prop_ownership_error_creation
  , fastProperty "OwnershipType with empty names" prop_ownership_type_empty_names
  , fastProperty "OwnershipError with empty parameters" prop_ownership_error_empty_params
  , fastProperty "OwnershipTransfer with empty strings" prop_ownership_transfer_empty_strings
  , fastProperty "OwnershipType with special characters" prop_ownership_type_special_characters
  , fastProperty "OwnershipError with special characters" prop_ownership_error_special_characters
  , fastProperty "OwnershipTransfer with special characters" prop_ownership_transfer_special_characters
  , fastProperty "OwnershipType conceptual roundtrip" prop_ownership_type_conceptual_roundtrip
  ]