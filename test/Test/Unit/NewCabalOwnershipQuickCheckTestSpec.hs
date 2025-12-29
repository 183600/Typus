module Test.Unit.NewCabalOwnershipQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)
import Data.List (sort)
import Data.Maybe (isNothing, isJust)

import Ownership.Common.Types
import TestSupport.QuickCheck (fastProperty)

-- | QuickCheck tests for Ownership module ownership analysis functions
tests :: TestTree
tests =
  testGroup "New Cabal Ownership QuickCheck Tests"
    [ testProperty "OwnershipType equality works correctly" prop_ownershipTypeEquality
    , testProperty "OwnershipType ordering is consistent" prop_ownershipTypeOrdering
    , testProperty "OwnershipError equality works correctly" prop_ownershipErrorEquality
    , testProperty "OwnershipError ordering is consistent" prop_ownershipErrorOrdering
    , testProperty "OwnershipTransfer equality works correctly" prop_ownershipTransferEquality
    , testProperty "newOwnershipAnalyzer creates analyzer" prop_newOwnershipAnalyzerWorks
    , testProperty "OwnershipType Show instance is invertible for simple cases" prop_ownershipTypeShowRoundtrip
    , testProperty "OwnershipError Show instance contains error type" prop_ownershipErrorShowContainsType
    , testProperty "OwnershipTransfer construction preserves fields" prop_ownershipTransferConstruction
    , testProperty "OwnershipType comparison respects ownership hierarchy" prop_ownershipTypeHierarchy
    , testGroup "Edge cases"
        [ testCase "Owned type shows correctly" $
            show (Owned "x") @?= "Owned x"
        , testCase "Borrowed type shows correctly" $
            show (Borrowed "y") @?= "Borrowed y"
        , testCase "MutBorrowed type shows correctly" $
            show (MutBorrowed "z") @?= "MutBorrowed z"
        , testCase "UseAfterMove error shows correctly" $
            show (UseAfterMove "var") @?= "UseAfterMove var"
        , testCase "DoubleMove error shows correctly" $
            show (DoubleMove "a" "b") @?= "DoubleMove a b"
        , testCase "OwnershipTransfer creates correct transfer" $ do
            let transfer = OwnershipTransfer "from" "to"
            transferFrom transfer @?= "from"
            transferTo transfer @?= "to"
        , testCase "newOwnershipAnalyzer creates analyzer" $ do
            let analyzer = newOwnershipAnalyzer
            case analyzer of
                OwnershipAnalyzer () -> pure ()
                _ -> assertFailure "Expected OwnershipAnalyzer ()"
        , testCase "OwnershipType ordering: Owned < Borrowed" $
            compare (Owned "x") (Borrowed "x") @?= LT
        , testCase "OwnershipType ordering: Borrowed < MutBorrowed" $
            compare (Borrowed "x") (MutBorrowed "x") @?= LT
        ]
    ]

-- | Property: OwnershipType equality works correctly
prop_ownershipTypeEquality :: OwnershipType -> OwnershipType -> Property
prop_ownershipTypeEquality ot1 ot2 = 
  (ot1 == ot2) === (ot1 `deepEqual` ot2)
  where
    deepEqual (Owned a) (Owned b) = a == b
    deepEqual (Borrowed a) (Borrowed b) = a == b
    deepEqual (MutBorrowed a) (MutBorrowed b) = a == b
    deepEqual _ _ = False

-- | Property: OwnershipType ordering is consistent
prop_ownershipTypeOrdering :: OwnershipType -> OwnershipType -> Property
prop_ownershipTypeOrdering ot1 ot2 = 
  let comparison = compare ot1 ot2
      reverseComparison = compare ot2 ot1
  in (comparison == EQ) === (ot1 == ot2) .&&.
     (comparison == LT) === (reverseComparison == GT) .&&.
     (comparison == GT) === (reverseComparison == LT)

-- | Property: OwnershipError equality works correctly
prop_ownershipErrorEquality :: OwnershipError -> OwnershipError -> Property
prop_ownershipErrorEquality oe1 oe2 = 
  (oe1 == oe2) === (oe1 `deepEqual` oe2)
  where
    deepEqual (UseAfterMove a) (UseAfterMove b) = a == b
    deepEqual (DoubleMove a b) (DoubleMove c d) = a == c && b == d
    deepEqual (BorrowWhileMoved a) (BorrowWhileMoved b) = a == b
    deepEqual (MutBorrowWhileBorrowed a) (MutBorrowWhileBorrowed b) = a == b
    deepEqual (BorrowWhileMutBorrowed a) (BorrowWhileMutBorrowed b) = a == b
    deepEqual (MultipleMutBorrows a) (MultipleMutBorrows b) = a == b
    deepEqual (UseWhileMutBorrowed a) (UseWhileMutBorrowed b) = a == b
    deepEqual (OutOfScope a) (OutOfScope b) = a == b
    deepEqual (BorrowError a) (BorrowError b) = a == b
    deepEqual (ParseError a) (ParseError b) = a == b
    deepEqual (CrossFunctionMove a b) (CrossFunctionMove c d) = a == c && b == d
    deepEqual (ParameterMoveMismatch a) (ParameterMoveMismatch b) = a == b
    deepEqual (ControlFlowError a) (ControlFlowError b) = a == b
    deepEqual (PathSensitiveError a) (PathSensitiveError b) = a == b
    deepEqual (LoopOwnershipError a) (LoopOwnershipError b) = a == b
    deepEqual _ _ = False

-- | Property: OwnershipError ordering is consistent
prop_ownershipErrorOrdering :: OwnershipError -> OwnershipError -> Property
prop_ownershipErrorOrdering oe1 oe2 = 
  let comparison = compare oe1 oe2
      reverseComparison = compare oe2 oe1
      show1 = show oe1
      show2 = show oe2
  in comparison === compare show1 show2 .&&.
     (comparison == EQ) === (oe1 == oe2) .&&.
     (comparison == LT) === (reverseComparison == GT) .&&.
     (comparison == GT) === (reverseComparison == LT)

-- | Property: OwnershipTransfer equality works correctly
prop_ownershipTransferEquality :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownershipTransferEquality ot1 ot2 = 
  (ot1 == ot2) === (transferFrom ot1 == transferFrom ot2 && transferTo ot1 == transferTo ot2)

-- | Property: newOwnershipAnalyzer creates analyzer
prop_newOwnershipAnalyzerWorks :: Property
prop_newOwnershipAnalyzerWorks = 
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
       OwnershipAnalyzer () -> property True
       _ -> property False

-- | Property: OwnershipType Show instance is invertible for simple cases
prop_ownershipTypeShowRoundtrip :: String -> Property
prop_ownershipTypeShowRoundtrip name = 
  not (null name) ==>
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
  in show owned === "Owned " ++ name .&&.
     show borrowed === "Borrowed " ++ name .&&.
     show mutBorrowed === "MutBorrowed " ++ name

-- | Property: OwnershipError Show instance contains error type
prop_ownershipErrorShowContainsType :: String -> Property
prop_ownershipErrorShowContainsType varName = 
  not (null varName) ==>
  let errors = [ UseAfterMove varName
               , BorrowWhileMoved varName
               , MutBorrowWhileBorrowed varName
               , BorrowWhileMutBorrowed varName
               , MultipleMutBorrows varName
               , UseWhileMutBorrowed varName
               , OutOfScope varName
               ]
  in all (\err -> varName `isInfixOf` show err) errors
  where
    isInfixOf needle haystack = needle `elem` (words haystack)

-- | Property: OwnershipTransfer construction preserves fields
prop_ownershipTransferConstruction :: String -> String -> Property
prop_ownershipTransferConstruction fromVar toVar = 
  not (null fromVar) && not (null toVar) ==>
  let transfer = OwnershipTransfer fromVar toVar
  in transferFrom transfer === fromVar .&&.
     transferTo transfer === toVar

-- | Property: OwnershipType comparison respects ownership hierarchy
prop_ownershipTypeHierarchy :: String -> Property
prop_ownershipTypeHierarchy varName = 
  not (null varName) ==>
  let owned = Owned varName
      borrowed = Borrowed varName
      mutBorrowed = MutBorrowed varName
  in compare owned borrowed === LT .&&.
     compare owned mutBorrowed === LT .&&.
     compare borrowed mutBorrowed === LT .&&.
     compare borrowed owned === GT .&&.
     compare mutBorrowed owned === GT .&&.
     compare mutBorrowed borrowed === GT

-- Helper operator for composing properties
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&)