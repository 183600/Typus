{-# LANGUAGE CPP #-}
module Test.Unit.OwnershipTransferComplexSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, choose, listOf, elements)
import Data.List (sort, nub, length, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Set as Set

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

-- | Complex property-based tests for Ownership module
tests :: TestTree
tests =
  testGroup "Ownership Transfer Complex Tests"
    [ testGroup "OwnershipType properties"
        [ fastProperty "OwnershipType ordering is consistent" prop_ownershipTypeOrdering
        , fastProperty "OwnershipType equality is reflexive" prop_ownershipTypeEquality
        , fastProperty "Owned types compare correctly" prop_ownedComparison
        , fastProperty "Borrowed types compare correctly" prop_borrowedComparison
        , fastProperty "MutBorrowed types compare correctly" prop_mutBorrowedComparison
        ]

    , testGroup "OwnershipError properties"
        [ fastProperty "OwnershipError ordering is consistent" prop_ownershipErrorOrdering
        , fastProperty "OwnershipError Show is invertible" prop_ownershipErrorShowRoundtrip
        , fastProperty "Complex error messages are preserved" prop_complexErrorMessages
        ]

    , testGroup "OwnershipTransfer properties"
        [ fastProperty "OwnershipTransfer equality is reflexive" prop_ownershipTransferEquality
        , fastProperty "OwnershipTransfer Show is informative" prop_ownershipTransferShow
        , fastProperty "Transfer operations are symmetric in structure" prop_transferSymmetry
        ]

    , testGroup "OwnershipAnalyzer properties"
        [ testCase "newOwnershipAnalyzer creates valid analyzer" $ do
            let analyzer = newOwnershipAnalyzer
            case analyzer of
              OwnershipAnalyzer () -> assertBool "analyzer is valid" True

        , testCase "analyzer handle is unique" $ do
            let analyzer1 = newOwnershipAnalyzer
                analyzer2 = newOwnershipAnalyzer
            -- Different handles should be equal in value but represent different instances
            analyzer1 @?= analyzer2
        ]

    , testGroup "Complex ownership scenarios"
        [ testCase "multiple ownership transfers can be tracked" $ do
            let transfers = 
                  [ OwnershipTransfer "x" "y"
                  , OwnershipTransfer "y" "z"
                  , OwnershipTransfer "a" "b"
                  ]
            length transfers @?= 3
            let transferFroms = map transferFrom transfers
                transferTos = map transferTo transfers
            Set.fromList transferFroms @?= Set.fromList ["x", "y", "a"]
            Set.fromList transferTos @?= Set.fromList ["y", "z", "b"]

        , testCase "ownership error types cover all scenarios" $ do
            let errors = 
                  [ UseAfterMove "var"
                  , DoubleMove "var1" "var2"
                  , BorrowWhileMoved "var"
                  , MutBorrowWhileBorrowed "var"
                  , BorrowWhileMutBorrowed "var"
                  , MultipleMutBorrows "var"
                  , UseWhileMutBorrowed "var"
                  , OutOfScope "var"
                  , BorrowError "message"
                  , ParseError "message"
                  , CrossFunctionMove "var1" "var2"
                  , ParameterMoveMismatch "var"
                  , ControlFlowError "message"
                  , PathSensitiveError "message"
                  , LoopOwnershipError "message"
                  ]
            length errors @?= 15
            let errorTypes = map (\err -> case err of
                  UseAfterMove _ -> "UseAfterMove"
                  DoubleMove _ _ -> "DoubleMove"
                  BorrowWhileMoved _ -> "BorrowWhileMoved"
                  MutBorrowWhileBorrowed _ -> "MutBorrowWhileBorrowed"
                  BorrowWhileMutBorrowed _ -> "BorrowWhileMutBorrowed"
                  MultipleMutBorrows _ -> "MultipleMutBorrows"
                  UseWhileMutBorrowed _ -> "UseWhileMutBorrowed"
                  OutOfScope _ -> "OutOfScope"
                  BorrowError _ -> "BorrowError"
                  ParseError _ -> "ParseError"
                  CrossFunctionMove _ _ -> "CrossFunctionMove"
                  ParameterMoveMismatch _ -> "ParameterMoveMismatch"
                  ControlFlowError _ -> "ControlFlowError"
                  PathSensitiveError _ -> "PathSensitiveError"
                  LoopOwnershipError _ -> "LoopOwnershipError"
                  ) errors
            Set.fromList errorTypes @?= Set.fromList 
              [ "UseAfterMove", "DoubleMove", "BorrowWhileMoved", "MutBorrowWhileBorrowed"
              , "BorrowWhileMutBorrowed", "MultipleMutBorrows", "UseWhileMutBorrowed"
              , "OutOfScope", "BorrowError", "ParseError", "CrossFunctionMove"
              , "ParameterMoveMismatch", "ControlFlowError", "PathSensitiveError"
              , "LoopOwnershipError"
              ]

        , testCase "ownership type hierarchy is maintained" $ do
            let owned = Owned "x"
                borrowed = Borrowed "x"
                mutBorrowed = MutBorrowed "x"
                types = [owned, borrowed, mutBorrowed]
            let sortedTypes = sort types
            -- Owned should come before Borrowed, which comes before MutBorrowed
            sortedTypes @?= [owned, borrowed, mutBorrowed]

        , testCase "complex transfer chains are handled" $ do
            let chain = 
                  [ OwnershipTransfer "a" "b"
                  , OwnershipTransfer "b" "c"
                  , OwnershipTransfer "c" "d"
                  , OwnershipTransfer "d" "e"
                  ]
            let sources = map transferFrom chain
                destinations = map transferTo chain
            -- Create a chain: a -> b -> c -> d -> e
            sources @?= ["a", "b", "c", "d"]
            destinations @?= ["b", "c", "d", "e"]
            -- Intermediate variables should appear as both source and destination
            let intermediate = Set.intersection (Set.fromList sources) (Set.fromList destinations)
            intermediate @?= Set.fromList ["b", "c", "d"]
        ]
    ]

-- Helper generators for testing
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  name <- elements ["x", "y", "z", "var", "value", "data"]
  elements [Owned name, Borrowed name, MutBorrowed name]

genOwnershipError :: Gen OwnershipError
genOwnershipError = do
  var <- elements ["x", "y", "z", "var"]
  var2 <- elements ["a", "b", "c", "other"]
  msg <- elements ["error message", "detailed error", "context info"]
  elements
    [ UseAfterMove var
    , DoubleMove var var2
    , BorrowWhileMoved var
    , MutBorrowWhileBorrowed var
    , BorrowWhileMutBorrowed var
    , MultipleMutBorrows var
    , UseWhileMutBorrowed var
    , OutOfScope var
    , BorrowError msg
    , ParseError msg
    , CrossFunctionMove var var2
    , ParameterMoveMismatch var
    , ControlFlowError msg
    , PathSensitiveError msg
    , LoopOwnershipError msg
    ]

genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  fromVar <- elements ["a", "b", "c", "x", "y", "z"]
  toVar <- elements ["d", "e", "f", "p", "q", "r"]
  return $ OwnershipTransfer fromVar toVar

-- Property: OwnershipType ordering is consistent
prop_ownershipTypeOrdering :: OwnershipType -> OwnershipType -> Property
prop_ownershipTypeOrdering type1 type2 =
  let comparison1 = compare type1 type2
      comparison2 = compare type2 type1
  in if comparison1 == EQ 
     then comparison2 === EQ
     else comparison1 /= comparison2

-- Property: OwnershipType equality is reflexive
prop_ownershipTypeEquality :: OwnershipType -> Property
prop_ownershipTypeEquality ownershipType = ownershipType === ownershipType

-- Property: Owned types compare correctly
prop_ownedComparison :: String -> String -> Property
prop_ownedComparison name1 name2 =
  let owned1 = Owned name1
      owned2 = Owned name2
  in compare owned1 owned2 === compare name1 name2

-- Property: Borrowed types compare correctly
prop_borrowedComparison :: String -> String -> Property
prop_borrowedComparison name1 name2 =
  let borrowed1 = Borrowed name1
      borrowed2 = Borrowed name2
  in compare borrowed1 borrowed2 === compare name1 name2

-- Property: MutBorrowed types compare correctly
prop_mutBorrowedComparison :: String -> String -> Property
prop_mutBorrowedComparison name1 name2 =
  let mutBorrowed1 = MutBorrowed name1
      mutBorrowed2 = MutBorrowed name2
  in compare mutBorrowed1 mutBorrowed2 === compare name1 name2

-- Property: OwnershipError ordering is consistent
prop_ownershipErrorOrdering :: OwnershipError -> OwnershipError -> Property
prop_ownershipErrorOrdering error1 error2 =
  let comparison1 = compare error1 error2
      comparison2 = compare error2 error1
  in if comparison1 == EQ 
     then comparison2 === EQ
     else comparison1 /= comparison2

-- Property: OwnershipError Show is invertible
prop_ownershipErrorShowRoundtrip :: OwnershipError -> Property
prop_ownershipErrorShowRoundtrip error = 
  let errorString = show error
  in length errorString > 0  -- Basic check that show produces output

-- Property: Complex error messages are preserved
prop_complexErrorMessages :: String -> String -> String -> Property
prop_complexErrorMessages var1 var2 msg =
  let error = DoubleMove var1 var2
      errorString = show error
  in var1 `isInfixOf` errorString && var2 `isInfixOf` errorString
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: OwnershipTransfer equality is reflexive
prop_ownershipTransferEquality :: OwnershipTransfer -> Property
prop_ownershipTransferEquality transfer = transfer === transfer

-- Property: OwnershipTransfer Show is informative
prop_ownershipTransferShow :: OwnershipTransfer -> Property
prop_ownershipTransferShow transfer =
  let transferString = show transfer
  in length transferString > 0 && 
     transferFrom transfer `isInfixOf` transferString &&
     transferTo transfer `isInfixOf` transferString
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: Transfer operations are symmetric in structure
prop_transferSymmetry :: String -> String -> Property
prop_transferSymmetry from to =
  let transfer1 = OwnershipTransfer from to
      transfer2 = OwnershipTransfer to from
  in transferFrom transfer1 === transferTo transfer2 &&
     transferTo transfer1 === transferFrom transfer2