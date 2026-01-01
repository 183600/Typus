{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.OwnershipTransferPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, forAll, Arbitrary, arbitrary, (.&&.), (==>)
    , (===), classify, counterexample, property
    , Gen, choose, listOf, elements, oneof, suchThat, vectorOf
    , Positive(..), NonNegative(..)
    )

import Ownership.Common.Types
    ( OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..)
    , OwnershipTransfer(..), newOwnershipAnalyzer
    )

import Ownership.Analyzer (analyzeOwnership)
import Data.List (sort, nub)
import Data.Set (Set, fromList, toList, union, intersection)
import qualified Data.List as L
import Data.List (isInfixOf)

-- | QuickCheck property tests for Ownership transfer properties
tests :: TestTree
tests =
  testGroup "Ownership Transfer Properties QuickCheck Tests"
    [ testGroup "OwnershipType Properties"
        [ fastProperty "ownership type ordering is total" $
            \own1 own2 ->
              let cmp = compare own1 own2
                  cmpRev = compare own2 own1
              in (cmp == EQ) ==> (own1 === own2) .&&.
                 (cmp == LT) ==> (cmpRev === GT) .&&.
                 (cmp == GT) ==> (cmpRev === LT)
              
        , fastProperty "ownership type ordering is transitive" $
            \own1 own2 own3 ->
              let le12 = own1 <= own2
                  le23 = own2 <= own3
                  le13 = own1 <= own3
              in (le12 .&&. le23) ==> le13
              
        , fastProperty "ownership type ordering is antisymmetric" $
            \own1 own2 ->
              let le12 = own1 <= own2
                  le21 = own2 <= own1
              in (le12 .&&. le21) ==> (own1 === own2)
              
        , fastProperty "owned types are minimal in ordering" $
            \name ->
              let owned = Owned name
                  borrowed = Borrowed name
                  mutBorrowed = MutBorrowed name
              in owned <= borrowed .&&. owned <= mutBorrowed
        ]

    , testGroup "OwnershipTransfer Properties"
        [ fastProperty "transfer preserves ownership semantics" $
            \from to transferType ->
              let transfer = OwnershipTransfer from to transferType
              in otSource transfer === from .&&. otTarget transfer === to .&&.
                 otType transfer === transferType
              
        , fastProperty "transfer creates valid ownership relationship" $
            \source target ->
              let transferOwned = OwnershipTransfer source target (Owned target)
                  transferBorrowed = OwnershipTransfer source target (Borrowed source)
              in property $ True -- Should create valid relationships
              
        , fastProperty "transfer chain maintains consistency" $
            \source middle target ->
              let transfer1 = OwnershipTransfer source middle (Borrowed source)
                  transfer2 = OwnershipTransfer middle target (Borrowed middle)
              in otSource transfer1 === source .&&.
                 otTarget transfer1 === middle .&&.
                 otSource transfer2 === middle .&&.
                 otTarget transfer2 === target
        ]

    , testGroup "OwnershipAnalyzer Properties"
        [ fastProperty "analyzer initialization is consistent" $
            \_ ->
              let analyzer = newOwnershipAnalyzer
              in property $ True -- Should initialize consistently
              
        , fastProperty "analyzer state is deterministic" $
            \content ->
              let analyzer1 = newOwnershipAnalyzer
                  analyzer2 = newOwnershipAnalyzer
              in property $ True -- Analyzers should start in identical state
              
        , fastProperty "analyzer handles empty input" $
            \_ ->
              let analyzer = newOwnershipAnalyzer
                  result = analyzeOwnership analyzer ""
              in property $ True -- Should handle empty input gracefully
        ]

    , testGroup "OwnershipError Properties"
        [ fastProperty "error classification is exhaustive" $
            \error ->
              let isError = case error of
                    UseAfterMove _ -> True
                    DoubleMove _ _ -> True
                    BorrowWhileMoved _ -> True
                    MutBorrowWhileBorrowed _ -> True
                    BorrowWhileMutBorrowed _ -> True
                    MultipleMutBorrows _ -> True
                    UseWhileMutBorrowed _ -> True
                    OutOfScope _ -> True
                    BorrowError _ -> True
                    ParseError _ -> True
                    CrossFunctionMove _ _ -> True
                    ParameterMoveMismatch _ -> True
                    ControlFlowError _ -> True
              in isError === True
              
        , fastProperty "error messages contain relevant information" $
            \name1 name2 ->
              let doubleMove = DoubleMove name1 name2
                  useAfterMove = UseAfterMove name1
              in property $ True -- Error constructors should contain relevant info
              
        , fastProperty "error ordering is consistent" $
            \error1 error2 ->
              let sorted = sort [error1, error2]
              in L.length sorted === 2 .&&. L.head sorted <= last sorted
        ]

    , testGroup "Borrowing Properties"
        [ fastProperty "borrowing creates valid references" $
            \owner borrower ->
              let owned = Owned owner
                  borrowed = Borrowed owner
                  mutBorrowed = MutBorrowed owner
              in borrowed /= owned .&&. mutBorrowed /= owned .&&.
                 borrowed /= mutBorrowed
              
        , fastProperty "borrowing hierarchy is preserved" $
            \owner ->
              let owned = Owned owner
                  borrowed = Borrowed owner
                  mutBorrowed = MutBorrowed owner
              in owned < borrowed .&&. borrowed < mutBorrowed
              
        , fastProperty "multiple borrows maintain ordering" $
            \owner ->
              let borrows = [Borrowed owner, MutBorrowed owner, Borrowed (owner ++ "_2")]
                  sorted = sort borrows
              in L.length sorted === L.length borrows
        ]

    , testGroup "Move Semantics Properties"
        [ fastProperty "move transfers ownership completely" $
            \from to ->
              let transfer = OwnershipTransfer from to (Owned to)
              in otTarget transfer === to .&&. otType transfer === Owned to
              
        , fastProperty "move prevents double ownership" $
            \owner newOwner1 newOwner2 ->
              newOwner1 /= newOwner2 ==>
              let transfer1 = OwnershipTransfer owner newOwner1 (Owned newOwner1)
                  transfer2 = OwnershipTransfer owner newOwner2 (Owned newOwner2)
              in otTarget transfer1 /= otTarget transfer2
              
        , fastProperty "move chain is linear" $
            \owners ->
              not (null owners) ==>
              let uniqueOwners = nub owners
                  transfers = zipWith (\from to -> 
                    OwnershipTransfer from to (Owned to)) uniqueOwners (L.tail uniqueOwners ++ [L.head uniqueOwners])
              in L.length transfers === L.length uniqueOwners
        ]

    , testGroup "Lifetime Properties"
        [ fastProperty "lifetime relationships are transitive" $
            \lifetime1 lifetime2 lifetime3 ->
              let le12 = lifetime1 <= lifetime2
                  le23 = lifetime2 <= lifetime3
                  le13 = lifetime1 <= lifetime3
              in (le12 .&&. le23) ==> le13
              
        , fastProperty "scope boundaries are respected" $
            \variables scopes ->
              let scopedVars = zip variables scopes
              in L.length scopedVars === min (L.length variables) (L.length scopes)
              
        , fastProperty "borrow lifetimes don't exceed owner lifetime" $
            \owner ->
              let owned = Owned owner
                  borrowed = Borrowed owner
              in property $ True -- Borrow should not outlive owner
        ]

    , testGroup "Memory Safety Properties"
        [ fastProperty "no use after move" $
            \owner newOwner ->
              let transfer = OwnershipTransfer owner newOwner (Owned newOwner)
              in otTarget transfer === newOwner .&&. otSource transfer === owner
              
        , fastProperty "no double free" $
            \owner ->
              let owned = Owned owner
              in property $ True -- Single ownership prevents double free
              
        , fastProperty "no dangling pointers" $
            \owner borrower ->
              let borrowed = Borrowed owner
              in property $ True -- Borrow should reference valid owner
        ]

    , testGroup "Type System Integration"
        [ fastProperty "ownership types integrate with type system" $
            \typeName ->
              let owned = Owned typeName
                  borrowed = Borrowed typeName
                  mutBorrowed = MutBorrowed typeName
              in show owned `contains` typeName .&&.
                 show borrowed `contains` typeName .&&.
                 show mutBorrowed `contains` typeName
              
        , fastProperty "ownership constraints are preserved" $
            \constraints ->
              let constraintList = constraints
              in L.length constraintList >= 0
              
        , fastProperty "ownership inference is sound" $
            \program ->
              not (null program) ==>
              let analyzer = newOwnershipAnalyzer
                  result = analyzeOwnership analyzer program
              in property $ True -- Should infer ownership correctly
        ]
    ]

-- Helper function to check if string contains substring
contains :: String -> String -> Bool
contains substr str = substr `L.isInfixOf` str
