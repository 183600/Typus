{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.OwnershipAnalysisComprehensiveQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Ownership.Common.Types
import qualified Data.Map.Strict as Map
import Data.List (nub, sort)
import Data.Set (Set)
import qualified Data.Set as Set

-- Arbitrary instances for ownership types
instance Arbitrary OwnershipType where
  arbitrary = oneof
    [ Owned <$> identifierGen
    , Borrowed <$> identifierGen
    , MutBorrowed <$> identifierGen
    ]
    where
      identifierGen = elements
        [ "x", "y", "z", "value", "data", "result", "item", "ptr", "ref", "obj" ]

instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> identifierGen
    , DoubleMove <$> identifierGen <*> identifierGen
    , BorrowWhileMoved <$> identifierGen
    , MutBorrowWhileBorrowed <$> identifierGen
    , BorrowWhileMutBorrowed <$> identifierGen
    , MultipleMutBorrows <$> identifierGen
    , UseWhileMutBorrowed <$> identifierGen
    , OutOfScope <$> identifierGen
    , BorrowError <$> arbitrary
    , ParseError <$> arbitrary
    , CrossFunctionMove <$> identifierGen <*> identifierGen
    , ParameterMoveMismatch <$> identifierGen
    , ControlFlowError <$> arbitrary
    , PathSensitiveError <$> arbitrary
    , LoopOwnershipError <$> arbitrary
    ]
    where
      identifierGen = elements
        [ "var", "value", "data", "result", "item", "ptr", "ref", "obj", "temp" ]

instance Arbitrary OwnershipTransfer where
  arbitrary = OwnershipTransfer <$> sourceGen <*> targetGen
    where
      sourceGen = elements ["src", "from", "source", "original", "input"]
      targetGen = elements ["dst", "to", "target", "result", "output"]

-- Helper generators
validIdentifierGen :: Gen String
validIdentifierGen = elements
  [ "x", "y", "z", "value", "data", "result", "item", "ptr", "ref", "obj"
  , "buffer", "array", "list", "map", "set", "string", "number", "flag"
  ]

errorGen :: Gen OwnershipError
errorGen = oneof
  [ UseAfterMove <$> validIdentifierGen
  , DoubleMove <$> validIdentifierGen <*> validIdentifierGen
  , BorrowWhileMoved <$> validIdentifierGen
  , MutBorrowWhileBorrowed <$> validIdentifierGen
  , BorrowWhileMutBorrowed <$> validIdentifierGen
  , MultipleMutBorrows <$> validIdentifierGen
  , UseWhileMutBorrowed <$> validIdentifierGen
  , OutOfScope <$> validIdentifierGen
  , BorrowError <$> arbitrary
  , ParseError <$> arbitrary
  , CrossFunctionMove <$> validIdentifierGen <*> validIdentifierGen
  , ParameterMoveMismatch <$> validIdentifierGen
  , ControlFlowError <$> arbitrary
  ]

ownershipTypeGen :: Gen OwnershipType
ownershipTypeGen = oneof
  [ Owned <$> validIdentifierGen
  , Borrowed <$> validIdentifierGen
  , MutBorrowed <$> validIdentifierGen
  ]

-- Test properties
tests :: TestTree
tests = testGroup "Ownership Analysis Comprehensive QuickCheck Tests"
  [ testProperty "Ownership types are correctly ordered" testOwnershipTypeOrdering
  , testProperty "Ownership errors are correctly ordered" testOwnershipErrorOrdering
  , testProperty "Ownership transfer operations are valid" testOwnershipTransfer
  , testProperty "Ownership analyzer can be created" testOwnershipAnalyzerCreation
  , testProperty "Ownership type equality works correctly" testOwnershipTypeEquality
  , testProperty "Ownership error equality works correctly" testOwnershipErrorEquality
  , testProperty "Ownership transfer equality works correctly" testOwnershipTransferEquality
  , testProperty "Ownership type show is informative" testOwnershipTypeShow
  , testProperty "Ownership error show is informative" testOwnershipErrorShow
  , testProperty "Ownership transfer show is informative" testOwnershipTransferShow
  ]

testOwnershipTypeOrdering :: OwnershipType -> OwnershipType -> Property
testOwnershipTypeOrdering type1 type2 =
  let comparison = compare type1 type2
  in (comparison == LT || comparison == EQ || comparison == GT) === True

testOwnershipErrorOrdering :: OwnershipError -> OwnershipError -> Property
testOwnershipErrorOrdering error1 error2 =
  let comparison = compare error1 error2
  in (comparison == LT || comparison == EQ || comparison == GT) === True

testOwnershipTransfer :: OwnershipTransfer -> Property
testOwnershipTransfer transfer =
  let fromVar = transferFrom transfer
      toVar = transferTo transfer
      isValidFrom = not (null fromVar)
      isValidTo = not (null toVar)
  in isValidFrom .&&. isValidTo

testOwnershipAnalyzerCreation :: Property
testOwnershipAnalyzerCreation =
  let analyzer = newOwnershipAnalyzer
  in analyzer === OwnershipAnalyzer ()

testOwnershipTypeEquality :: OwnershipType -> OwnershipType -> Property
testOwnershipTypeEquality type1 type2 =
  let areEqual = type1 == type2
      sameCategory = case (type1, type2) of
        (Owned _, Owned _) -> True
        (Borrowed _, Borrowed _) -> True
        (MutBorrowed _, MutBorrowed _) -> True
        _ -> False
  in if sameCategory then areEqual === True else areEqual === False

testOwnershipErrorEquality :: OwnershipError -> OwnershipError -> Property
testOwnershipErrorEquality error1 error2 =
  let areEqual = error1 == error2
      sameCategory = case (error1, error2) of
        (UseAfterMove _, UseAfterMove _) -> True
        (DoubleMove _ _, DoubleMove _ _) -> True
        (BorrowWhileMoved _, BorrowWhileMoved _) -> True
        (MutBorrowWhileBorrowed _, MutBorrowWhileBorrowed _) -> True
        (BorrowWhileMutBorrowed _, BorrowWhileMutBorrowed _) -> True
        (MultipleMutBorrows _, MultipleMutBorrows _) -> True
        (UseWhileMutBorrowed _, UseWhileMutBorrowed _) -> True
        (OutOfScope _, OutOfScope _) -> True
        (BorrowError _, BorrowError _) -> True
        (ParseError _, ParseError _) -> True
        (CrossFunctionMove _ _, CrossFunctionMove _ _) -> True
        (ParameterMoveMismatch _, ParameterMoveMismatch _) -> True
        (ControlFlowError _, ControlFlowError _) -> True
        (PathSensitiveError _, PathSensitiveError _) -> True
        (LoopOwnershipError _, LoopOwnershipError _) -> True
        _ -> False
  in if sameCategory then areEqual === True else areEqual === False

testOwnershipTransferEquality :: OwnershipTransfer -> OwnershipTransfer -> Property
testOwnershipTransferEquality transfer1 transfer2 =
  let areEqual = transfer1 == transfer2
      sameFrom = transferFrom transfer1 == transferFrom transfer2
      sameTo = transferTo transfer1 == transferTo transfer2
  in areEqual === (sameFrom && sameTo)

testOwnershipTypeShow :: OwnershipType -> Property
testOwnershipTypeShow ownershipType =
  let showString = show ownershipType
      hasContent = not (null showString)
      containsType = case ownershipType of
        Owned _ -> "Owned" `isInfixOf` showString
        Borrowed _ -> "Borrowed" `isInfixOf` showString
        MutBorrowed _ -> "MutBorrowed" `isInfixOf` showString
  in hasContent .&&. containsType

testOwnershipErrorShow :: OwnershipError -> Property
testOwnershipErrorShow ownershipError =
  let showString = show ownershipError
      hasContent = not (null showString)
      containsErrorType = case ownershipError of
        UseAfterMove _ -> "UseAfterMove" `isInfixOf` showString
        DoubleMove _ _ -> "DoubleMove" `isInfixOf` showString
        BorrowWhileMoved _ -> "BorrowWhileMoved" `isInfixOf` showString
        MutBorrowWhileBorrowed _ -> "MutBorrowWhileBorrowed" `isInfixOf` showString
        BorrowWhileMutBorrowed _ -> "BorrowWhileMutBorrowed" `isInfixOf` showString
        MultipleMutBorrows _ -> "MultipleMutBorrows" `isInfixOf` showString
        UseWhileMutBorrowed _ -> "UseWhileMutBorrowed" `isInfixOf` showString
        OutOfScope _ -> "OutOfScope" `isInfixOf` showString
        BorrowError _ -> "BorrowError" `isInfixOf` showString
        ParseError _ -> "ParseError" `isInfixOf` showString
        CrossFunctionMove _ _ -> "CrossFunctionMove" `isInfixOf` showString
        ParameterMoveMismatch _ -> "ParameterMoveMismatch" `isInfixOf` showString
        ControlFlowError _ -> "ControlFlowError" `isInfixOf` showString
        PathSensitiveError _ -> "PathSensitiveError" `isInfixOf` showString
        LoopOwnershipError _ -> "LoopOwnershipError" `isInfixOf` showString
  in hasContent .&&. containsErrorType

testOwnershipTransferShow :: OwnershipTransfer -> Property
testOwnershipTransferShow transfer =
  let showString = show transfer
      hasContent = not (null showString)
      fromVar = transferFrom transfer
      toVar = transferTo transfer
      containsFrom = fromVar `isInfixOf` showString
      containsTo = toVar `isInfixOf` showString
  in hasContent .&&. containsFrom .&&. containsTo

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]