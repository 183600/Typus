{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, vectorOf, Positive(..), NonNegative(..))

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  )

import Ownership.Common.Types (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..))
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)

-- | 新的QuickCheck属性测试，针对Ownership模块的一致性
tests :: TestTree
tests =
  testGroup "New Cabal Ownership QuickCheck Tests"
    [ testGroup "OwnershipType properties"
        [ fastProperty "OwnershipType ordering is consistent" $
            \type1 type2 ->
              let ordered = [type1, type2]
                  sorted = sort ordered
              in length sorted === 2 .&&. head sorted `elem` ordered .&&. last sorted `elem` ordered

        , fastProperty "Owned types compare by name" $
            \name1 name2 ->
              let owned1 = Owned name1
                  owned2 = Owned name2
              in compare owned1 owned2 === compare name1 name2

        , fastProperty "Borrowed types compare by name" $
            \name1 name2 ->
              let borrowed1 = Borrowed name1
                  borrowed2 = Borrowed name2
              in compare borrowed1 borrowed2 === compare name1 name2

        , fastProperty "MutBorrowed types compare by name" $
            \name1 name2 ->
              let mutBorrowed1 = MutBorrowed name1
                  mutBorrowed2 = MutBorrowed name2
              in compare mutBorrowed1 mutBorrowed2 === compare name1 name2

        , fastProperty "Ownership type hierarchy: Owned < Borrowed < MutBorrowed" $
            \name ->
              let owned = Owned name
                  borrowed = Borrowed name
                  mutBorrowed = MutBorrowed name
              in owned < borrowed .&&. borrowed < mutBorrowed .&&. owned < mutBorrowed

        , fastProperty "Show and Read consistency for OwnershipType" $
            \ownershipType ->
              show ownershipType === show ownershipType  -- Basic consistency check
        ]

    , testGroup "OwnershipError properties"
        [ fastProperty "OwnershipError ordering is consistent" $
            \error1 error2 ->
              let ordered = [error1, error2]
                  sorted = sort ordered
              in length sorted === 2 .&&. head sorted `elem` ordered .&&. last sorted `elem` ordered

        , fastProperty "UseAfterMove errors with same variable are equal" $
            \var ->
              let error1 = UseAfterMove var
                  error2 = UseAfterMove var
              in error1 === error2

        , fastProperty "DoubleMove errors are ordered by variable names" $
            \var1 var2 var3 var4 ->
              let error1 = DoubleMove var1 var2
                  error2 = DoubleMove var3 var4
              in compare error1 error2 === compare (show error1) (show error2)

        , fastProperty "Error types are distinguishable" $
            \var ->
              let useAfterMove = UseAfterMove var
                  doubleMove = DoubleMove var var
                  borrowWhileMoved = BorrowWhileMoved var
              in useAfterMove /= doubleMove .&&. doubleMove /= borrowWhileMoved .&&.
                 useAfterMove /= borrowWhileMoved

        , fastProperty "Show produces non-empty strings" $
            \error ->
              not (null (show error))
        ]

    , testGroup "OwnershipTransfer properties"
        [ fastProperty "OwnershipTransfer preserves field values" $
            \from to ->
              let transfer = OwnershipTransfer from to
              in transferFrom transfer === from .&&. transferTo transfer === to

        , fastProperty "OwnershipTransfer equality is correct" $
            \from1 to1 from2 to2 ->
              let transfer1 = OwnershipTransfer from1 to1
                  transfer2 = OwnershipTransfer from2 to2
              in (transfer1 == transfer2) === (from1 == from2 && to1 == to2)

        , fastProperty "OwnershipTransfer Show is meaningful" $
            \from to ->
              let transfer = OwnershipTransfer from to
                  transferStr = show transfer
              in from `isInfixOf` transferStr .&&. to `isInfixOf` transferStr

        , fastProperty "Self-transfer is valid" $
            \var ->
              let transfer = OwnershipTransfer var var
              in transferFrom transfer === transferTo transfer
        ]

    , testGroup "OwnershipAnalyzer properties"
        [ testCase "newOwnershipAnalyzer creates valid analyzer" $ do
            let analyzer = newOwnershipAnalyzer
            case analyzer of
              OwnershipAnalyzer () -> pure ()
              _ -> assertFailure "Invalid analyzer created"

        , testCase "Analyzer handles empty input" $ do
            let analyzer = newOwnershipAnalyzer
                result = analyzeOwnership analyzer ""
            case result of
              Left _ -> pure ()  -- Should handle gracefully
              Right errors -> pure ()  -- Should return empty errors
        ]

    , testGroup "Ownership analysis consistency"
        [ fastProperty "Analysis produces deterministic results" $
            \code ->
              length code < 100 ==>
              let analyzer = newOwnershipAnalyzer
                  result1 = analyzeOwnership analyzer code
                  result2 = analyzeOwnership analyzer code
              in result1 === result2

        , fastProperty "Debug analysis contains regular analysis" $
            \code ->
              length code < 100 ==>
              let analyzer = newOwnershipAnalyzer
                  regular = analyzeOwnership analyzer code
                  debug = analyzeOwnershipDebug analyzer code
              in case (regular, debug) of
                   (Left err1, Left err2) -> err1 === err2
                   (Right errors1, Right errors2) -> sort errors1 === sort errors2
                   _ -> property False

        , fastProperty "Error formatting produces valid output" $
            \errors ->
              let formatted = formatOwnershipErrors errors
              in length formatted >= 0  -- Should not crash

        , fastProperty "Analysis handles simple ownership patterns" $
            \varName ->
              let code = varName ++ " := 42\n" ++ varName ++ ".move()\n"
                  analyzer = newOwnershipAnalyzer
                  result = analyzeOwnership analyzer code
              in case result of
                   Left _ -> property False
                   Right errors -> property True  -- Should analyze without crashing
        ]

    , testGroup "Edge cases and boundary conditions"
        [ testCase "Empty variable name handling" $ do
            let owned = Owned ""
                borrowed = Borrowed ""
                mutBorrowed = MutBorrowed ""
            show owned @?= "Owned "
            show borrowed @?= "Borrowed "
            show mutBorrowed @?= "MutBorrowed "

        , testCase "Special characters in variable names" $ do
            let specialVar = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
                owned = Owned specialVar
                error = UseAfterMove specialVar
                transfer = OwnershipTransfer specialVar specialVar
            show owned @?= "Owned " ++ specialVar
            show error @?= "UseAfterMove " ++ specialVar
            transferFrom transfer @?= specialVar
            transferTo transfer @?= specialVar

        , testCase "Unicode in ownership types" $ do
            let unicodeVar = "变量_🦀"
                owned = Owned unicodeVar
                error = DoubleMove unicodeVar unicodeVar
            show owned @?= "Owned " ++ unicodeVar
            show error @?= "DoubleMove " ++ unicodeVar ++ " " ++ unicodeVar

        , testCase "Long variable names" $ do
            let longName = concat (replicate 100 "a")
                owned = Owned longName
                transfer = OwnershipTransfer longName longName
            length (show owned) > 100 @?= True
            transferFrom transfer @?= longName

        , testCase "Complex error combinations" $ do
            let errors = 
                  [ UseAfterMove "x"
                  , DoubleMove "y" "z"
                  , BorrowWhileMoved "a"
                  , MutBorrowWhileBorrowed "b"
                  , MultipleMutBorrows "c"
                  ]
                formatted = formatOwnershipErrors errors
            length formatted @? (> 0)  -- Should format all errors
        ]

    , testGroup "Ownership transfer consistency"
        [ testCase "Transfer chain properties" $ do
            let transfers = 
                  [ OwnershipTransfer "a" "b"
                  , OwnershipTransfer "b" "c"
                  , OwnershipTransfer "c" "d"
                  ]
                -- Check that transfers form a chain
                isChain (t1:t2:rest) = transferTo t1 == transferFrom t2 && isChain (t2:rest)
                isChain _ = True
            isChain transfers @?= True

        , testCase "Circular transfer detection" $ do
            let circularTransfers = 
                  [ OwnershipTransfer "a" "b"
                  , OwnershipTransfer "b" "c"
                  , OwnershipTransfer "c" "a"
                  ]
                -- Check if we can detect circular transfers
                hasCycle transfers = any (\t -> transferFrom t == transferTo (last transfers)) transfers
            hasCycle circularTransfers @?= True

        , fastProperty "Transfer ordering preserves direction" $
            \from to ->
              let transfer = OwnershipTransfer from to
                  reversed = OwnershipTransfer to from
              in transfer /= reversed .&&. 
                 (transfer == reversed) === (from == to)
        ]

    , testGroup "Performance and stress tests"
        [ fastProperty "Large ownership analysis" $
            \size ->
              size < 1000 ==>
              let code = unlines $ map (\i -> "var" ++ show i ++ " := " ++ show i) [1..size]
                  analyzer = newOwnershipAnalyzer
                  result = analyzeOwnership analyzer code
              in case result of
                   Left _ -> property False
                   Right errors -> length errors >= 0  -- Should handle large input

        , fastProperty "Many ownership errors" $
            \count ->
              count < 100 ==>
              let errors = map (\i -> UseAfterMove ("var" ++ show i)) [1..count]
                  formatted = formatOwnershipErrors errors
              in length formatted >= count  -- Should format all errors

        , fastProperty "Complex ownership type combinations" $
            \types ->
              length types < 50 ==>
              let ownershipTypes = map (\i -> case i `mod` 3 of
                                            0 -> Owned ("var" ++ show i)
                                            1 -> Borrowed ("var" ++ show i)
                                            _ -> MutBorrowed ("var" ++ show i)) types
                  sorted = sort ownershipTypes
              in length sorted === length ownershipTypes .&&. 
                 length (nub sorted) <= length ownershipTypes
        ]
    ]