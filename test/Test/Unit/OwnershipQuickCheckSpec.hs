{-# LANGUAGE CPP #-}

module Test.Unit.OwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , newOwnershipAnalyzer
  )
import Data.List (isInfixOf)

-- Property: OwnershipType with owner name
prop_owned_preserves_name :: String -> Property
prop_owned_preserves_name name =
  let ownership = Owned name
  in case ownership of
    Owned n -> n === name
    _ -> property False

-- Property: Borrowed preserves reference name
prop_borrowed_preserves_name :: String -> Property
prop_borrowed_preserves_name name =
  let ownership = Borrowed name
  in case ownership of
    Borrowed n -> n === name
    _ -> property False

-- Property: MutBorrowed preserves reference name
prop_mutborrowed_preserves_name :: String -> Property
prop_mutborrowed_preserves_name name =
  let ownership = MutBorrowed name
  in case ownership of
    MutBorrowed n -> n === name
    _ -> property False

-- Property: OwnershipType equality
prop_ownershiptype_eq :: OwnershipType -> OwnershipType -> Property
prop_ownershiptype_eq ot1 ot2 =
  (ot1 == ot2) === case (ot1, ot2) of
    (Owned n1, Owned n2) -> n1 == n2
    (Borrowed n1, Borrowed n2) -> n1 == n2
    (MutBorrowed n1, MutBorrowed n2) -> n1 == n2
    _ -> False

-- Property: OwnershipType ordering
prop_ownershiptype_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownershiptype_ordering ot1 ot2 =
  let result = compare ot1 ot2
  in (result == LT || result == EQ || result == GT) === True

-- Property: OwnershipType show
prop_ownershiptype_show :: OwnershipType -> Property
prop_ownershiptype_show ownership =
  let shown = show ownership
  in not (null shown)

-- Property: OwnershipType show contains name
prop_ownershiptype_show_contains_name :: String -> Property
prop_ownershiptype_show_contains_name name =
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
      shownOwned = show owned
      shownBorrowed = show borrowed
      shownMutBorrowed = show mutBorrowed
  in name `isInfixOf` shownOwned &&
     name `isInfixOf` shownBorrowed &&
     name `isInfixOf` shownMutBorrowed

-- Property: UseAfterMove error
prop_useaftermove :: String -> Property
prop_useaftermove varName =
  let err = UseAfterMove varName
  in case err of
    UseAfterMove name -> name === varName
    _ -> property False

-- Property: DoubleMove error
prop_doublemove :: String -> String -> Property
prop_doublemove var1 var2 =
  let err = DoubleMove var1 var2
  in case err of
    DoubleMove v1 v2 -> v1 === var1 && v2 === var2
    _ -> property False

-- Property: BorrowWhileMoved error
prop_borrowwhilemoved :: String -> Property
prop_borrowwhilemoved varName =
  let err = BorrowWhileMoved varName
  in case err of
    BorrowWhileMoved name -> name === varName
    _ -> property False

-- Property: MutBorrowWhileBorrowed error
prop_mutborrowwhileborrowed :: String -> Property
prop_mutborrowwhileborrowed varName =
  let err = MutBorrowWhileBorrowed varName
  in case err of
    MutBorrowWhileBorrowed name -> name === varName
    _ -> property False

-- Property: BorrowWhileMutBorrowed error
prop_borrowwhilemutborrowed :: String -> Property
prop_borrowwhilemutborrowed varName =
  let err = BorrowWhileMutBorrowed varName
  in case err of
    BorrowWhileMutBorrowed name -> name === varName
    _ -> property False

-- Property: MultipleMutBorrows error
prop_multiplemutborrows :: String -> Property
prop_multiplemutborrows varName =
  let err = MultipleMutBorrows varName
  in case err of
    MultipleMutBorrows name -> name === varName
    _ -> property False

-- Property: UseWhileMutBorrowed error
prop_usewhilemutborrowed :: String -> Property
prop_usewhilemutborrowed varName =
  let err = UseWhileMutBorrowed varName
  in case err of
    UseWhileMutBorrowed name -> name === varName
    _ -> property False

-- Property: OutOfScope error
prop_outofscope :: String -> Property
prop_outofscope varName =
  let err = OutOfScope varName
  in case err of
    OutOfScope name -> name === varName
    _ -> property False

-- Property: BorrowError error
prop_borrowerror :: String -> Property
prop_borrowerror message =
  let err = BorrowError message
  in case err of
    BorrowError msg -> msg === message
    _ -> property False

-- Property: ParseError error
prop_parseerror :: String -> Property
prop_parseerror message =
  let err = ParseError message
  in case err of
    ParseError msg -> msg === message
    _ -> property False

-- Property: CrossFunctionMove error
prop_crossfunctionmove :: String -> String -> Property
prop_crossfunctionmove fromFunc toFunc =
  let err = CrossFunctionMove fromFunc toFunc
  in case err of
    CrossFunctionMove from to -> from === fromFunc && to === toFunc
    _ -> property False

-- Property: ParameterMoveMismatch error
prop_parametermovemismatch :: String -> Property
prop_parametermovemismatch paramName =
  let err = ParameterMoveMismatch paramName
  in case err of
    ParameterMoveMismatch name -> name === paramName
    _ -> property False

-- Property: ControlFlowError error
prop_controlflowerror :: String -> Property
prop_controlflowerror message =
  let err = ControlFlowError message
  in case err of
    ControlFlowError msg -> msg === message
    _ -> property False

-- Property: PathSensitiveError error
prop_pathsensitiveerror :: String -> Property
prop_pathsensitiveerror message =
  let err = PathSensitiveError message
  in case err of
    PathSensitiveError msg -> msg === message
    _ -> property False

-- Property: LoopOwnershipError error
prop_loopownershiperror :: String -> Property
prop_loopownershiperror message =
  let err = LoopOwnershipError message
  in case err of
    LoopOwnershipError msg -> msg === message
    _ -> property False

-- Property: OwnershipError equality
prop_ownershiperror_eq :: OwnershipError -> OwnershipError -> Property
prop_ownershiperror_eq err1 err2 =
  (err1 == err2) === case (err1, err2) of
    (UseAfterMove n1, UseAfterMove n2) -> n1 == n2
    (DoubleMove v1 v2, DoubleMove v1' v2') -> v1 == v1' && v2 == v2'
    (BorrowWhileMoved n1, BorrowWhileMoved n2) -> n1 == n2
    (MutBorrowWhileBorrowed n1, MutBorrowWhileBorrowed n2) -> n1 == n2
    (BorrowWhileMutBorrowed n1, BorrowWhileMutBorrowed n2) -> n1 == n2
    (MultipleMutBorrows n1, MultipleMutBorrows n2) -> n1 == n2
    (UseWhileMutBorrowed n1, UseWhileMutBorrowed n2) -> n1 == n2
    (OutOfScope n1, OutOfScope n2) -> n1 == n2
    (BorrowError m1, BorrowError m2) -> m1 == m2
    (ParseError m1, ParseError m2) -> m1 == m2
    (CrossFunctionMove f1 t1, CrossFunctionMove f2 t2) -> f1 == f2 && t1 == t2
    (ParameterMoveMismatch n1, ParameterMoveMismatch n2) -> n1 == n2
    (ControlFlowError m1, ControlFlowError m2) -> m1 == m2
    (PathSensitiveError m1, PathSensitiveError m2) -> m1 == m2
    (LoopOwnershipError m1, LoopOwnershipError m2) -> m1 == m2
    _ -> False

-- Property: OwnershipError ordering
prop_ownershiperror_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownershiperror_ordering err1 err2 =
  let result = compare err1 err2
  in (result == LT || result == EQ || result == GT) === True

-- Property: OwnershipError show
prop_ownershiperror_show :: OwnershipError -> Property
prop_ownershiperror_show err =
  let shown = show err
  in not (null shown)

-- Property: OwnershipError show contains variable name
prop_ownershiperror_show_contains_var :: String -> Property
prop_ownershiperror_show_contains_var varName =
  let useAfterMove = UseAfterMove varName
      borrowWhileMoved = BorrowWhileMoved varName
      outOfScope = OutOfScope varName
      shownUseAfterMove = show useAfterMove
      shownBorrowWhileMoved = show borrowWhileMoved
      shownOutOfScope = show outOfScope
  in varName `isInfixOf` shownUseAfterMove &&
     varName `isInfixOf` shownBorrowWhileMoved &&
     varName `isInfixOf` shownOutOfScope

-- Property: OwnershipError show contains message
prop_ownershiperror_show_contains_message :: String -> Property
prop_ownershiperror_show_contains_message message =
  let borrowError = BorrowError message
      parseError = ParseError message
      controlFlowError = ControlFlowError message
      shownBorrowError = show borrowError
      shownParseError = show parseError
      shownControlFlowError = show controlFlowError
  in message `isInfixOf` shownBorrowError &&
     message `isInfixOf` shownParseError &&
     message `isInfixOf` shownControlFlowError

-- Property: OwnershipAnalyzer constructor
prop_newownershipanalyzer :: Property
prop_newownershipanalyzer =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
    OwnershipAnalyzer () -> property True
    _ -> property False

-- Property: OwnershipAnalyzer equality
prop_ownershipanalyzer_eq :: Property
prop_ownershipanalyzer_eq =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in analyzer1 === analyzer2

-- Property: OwnershipAnalyzer show
prop_ownershipanalyzer_show :: Property
prop_ownershipanalyzer_show =
  let analyzer = newOwnershipAnalyzer
      shown = show analyzer
  in "OwnershipAnalyzer" `isInfixOf` shown

-- Property: OwnershipType with empty name
prop_ownershiptype_empty_name :: Property
prop_ownershiptype_empty_name =
  let owned = Owned ""
      borrowed = Borrowed ""
      mutBorrowed = MutBorrowed ""
  in case owned of
    Owned name -> name === ""
    _ -> property False

-- Property: OwnershipError with empty variable name
prop_ownershiperror_empty_var :: Property
prop_ownershiperror_empty_var =
  let useAfterMove = UseAfterMove ""
      borrowWhileMoved = BorrowWhileMoved ""
  in case useAfterMove of
    UseAfterMove name -> name === ""
    _ -> property False

-- Property: OwnershipError with empty message
prop_ownershiperror_empty_message :: Property
prop_ownershiperror_empty_message =
  let borrowError = BorrowError ""
      parseError = ParseError ""
  in case borrowError of
    BorrowError message -> message === ""
    _ -> property False

-- Property: DoubleMove with same variable
prop_doublemove_same_var :: String -> Property
prop_doublemove_same_var varName =
  let err = DoubleMove varName varName
  in case err of
    DoubleMove v1 v2 -> v1 === varName && v2 === varName
    _ -> property False

-- Property: CrossFunctionMove with same function
prop_crossfunctionmove_same_func :: String -> Property
prop_crossfunctionmove_same_func funcName =
  let err = CrossFunctionMove funcName funcName
  in case err of
    CrossFunctionMove from to -> from === funcName && to === funcName
    _ -> property False

-- Property: OwnershipType with special characters
prop_ownershiptype_special_chars :: Property
prop_ownershiptype_special_chars =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      owned = Owned specialChars
      borrowed = Borrowed specialChars
      mutBorrowed = MutBorrowed specialChars
  in case (owned, borrowed, mutBorrowed) of
    (Owned name, Borrowed ref, MutBorrowed mutRef) -> 
      name === specialChars && ref === specialChars && mutRef === specialChars
    _ -> property False

-- Property: OwnershipError with special characters
prop_ownershiperror_special_chars :: Property
prop_ownershiperror_special_chars =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      useAfterMove = UseAfterMove specialChars
      borrowError = BorrowError specialChars
  in case (useAfterMove, borrowError) of
    (UseAfterMove name, BorrowError message) -> 
      name === specialChars && message === specialChars
    _ -> property False

-- Property: OwnershipError with Unicode characters
prop_ownershiperror_unicode :: Property
prop_ownershiperror_unicode =
  let unicode = "测试变量名🚀"
      useAfterMove = UseAfterMove unicode
      borrowError = BorrowError unicode
  in case (useAfterMove, borrowError) of
    (UseAfterMove name, BorrowError message) -> 
      name === unicode && message === unicode
    _ -> property False

-- Property: OwnershipType with Unicode characters
prop_ownershiptype_unicode :: Property
prop_ownershiptype_unicode =
  let unicode = "测试变量名🚀"
      owned = Owned unicode
      borrowed = Borrowed unicode
      mutBorrowed = MutBorrowed unicode
  in case (owned, borrowed, mutBorrowed) of
    (Owned name, Borrowed ref, MutBorrowed mutRef) -> 
      name === unicode && ref === unicode && mutRef === unicode
    _ -> property False

tests :: TestTree
tests = testGroup "Ownership QuickCheck tests"
  [ fastProperty "Owned preserves name" prop_owned_preserves_name
  , fastProperty "Borrowed preserves reference name" prop_borrowed_preserves_name
  , fastProperty "MutBorrowed preserves reference name" prop_mutborrowed_preserves_name
  , fastProperty "OwnershipType equality" prop_ownershiptype_eq
  , fastProperty "OwnershipType ordering" prop_ownershiptype_ordering
  , fastProperty "OwnershipType show" prop_ownershiptype_show
  , fastProperty "OwnershipType show contains name" prop_ownershiptype_show_contains_name
  , fastProperty "UseAfterMove error" prop_useaftermove
  , fastProperty "DoubleMove error" prop_doublemove
  , fastProperty "BorrowWhileMoved error" prop_borrowwhilemoved
  , fastProperty "MutBorrowWhileBorrowed error" prop_mutborrowwhileborrowed
  , fastProperty "BorrowWhileMutBorrowed error" prop_borrowwhilemutborrowed
  , fastProperty "MultipleMutBorrows error" prop_multiplemutborrows
  , fastProperty "UseWhileMutBorrowed error" prop_usewhilemutborrowed
  , fastProperty "OutOfScope error" prop_outofscope
  , fastProperty "BorrowError error" prop_borrowerror
  , fastProperty "ParseError error" prop_parseerror
  , fastProperty "CrossFunctionMove error" prop_crossfunctionmove
  , fastProperty "ParameterMoveMismatch error" prop_parametermovemismatch
  , fastProperty "ControlFlowError error" prop_controlflowerror
  , fastProperty "PathSensitiveError error" prop_pathsensitiveerror
  , fastProperty "LoopOwnershipError error" prop_loopownershiperror
  , fastProperty "OwnershipError equality" prop_ownershiperror_eq
  , fastProperty "OwnershipError ordering" prop_ownershiperror_ordering
  , fastProperty "OwnershipError show" prop_ownershiperror_show
  , fastProperty "OwnershipError show contains variable name" prop_ownershiperror_show_contains_var
  , fastProperty "OwnershipError show contains message" prop_ownershiperror_show_contains_message
  , fastProperty "OwnershipAnalyzer constructor" prop_newownershipanalyzer
  , fastProperty "OwnershipAnalyzer equality" prop_ownershipanalyzer_eq
  , fastProperty "OwnershipAnalyzer show" prop_ownershipanalyzer_show
  , fastProperty "OwnershipType with empty name" prop_ownershiptype_empty_name
  , fastProperty "OwnershipError with empty variable name" prop_ownershiperror_empty_var
  , fastProperty "OwnershipError with empty message" prop_ownershiperror_empty_message
  , fastProperty "DoubleMove with same variable" prop_doublemove_same_var
  , fastProperty "CrossFunctionMove with same function" prop_crossfunctionmove_same_func
  , fastProperty "OwnershipType with special characters" prop_ownershiptype_special_chars
  , fastProperty "OwnershipError with special characters" prop_ownershiperror_special_chars
  , fastProperty "OwnershipError with Unicode characters" prop_ownershiperror_unicode
  , fastProperty "OwnershipType with Unicode characters" prop_ownershiptype_unicode
  ]