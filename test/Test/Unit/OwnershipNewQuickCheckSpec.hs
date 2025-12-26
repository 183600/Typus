{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, vectorOf, elements, oneof)
import qualified Data.List as List
import Data.Ord (comparing)

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

-- Arbitrary instances for Ownership types

instance Arbitrary OwnershipType where
  arbitrary = oneof
    [ Owned <$> genSafeString
    , Borrowed <$> genSafeString
    , MutBorrowed <$> genSafeString
    ]

instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> genSafeString
    , DoubleMove <$> genSafeString <*> genSafeString
    , BorrowWhileMoved <$> genSafeString
    , MutBorrowWhileBorrowed <$> genSafeString
    , BorrowWhileMutBorrowed <$> genSafeString
    , MultipleMutBorrows <$> genSafeString
    , UseWhileMutBorrowed <$> genSafeString
    , OutOfScope <$> genSafeString
    , BorrowError <$> genSafeString
    , ParseError <$> genSafeString
    , CrossFunctionMove <$> genSafeString <*> genSafeString
    , ParameterMoveMismatch <$> genSafeString
    , ControlFlowError <$> genSafeString
    , PathSensitiveError <$> genSafeString
    , LoopOwnershipError <$> genSafeString
    ]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    fromVar <- genSafeString
    toVar <- genSafeString
    return $ OwnershipTransfer fromVar toVar

-- Helper generator
genSafeString :: Gen String
genSafeString = do
  size <- choose (1, 10)
  vectorOf size $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

-- Property: Owned constructor preserves owner name
prop_owned_preserves_name :: String -> Property
prop_owned_preserves_name name =
  let ownershipType = Owned name
  in case ownershipType of
       Owned n -> property $ n === name

-- Property: Borrowed constructor preserves borrower name
prop_borrowed_preserves_name :: String -> Property
prop_borrowed_preserves_name name =
  let ownershipType = Borrowed name
  in case ownershipType of
       Borrowed n -> property $ n === name

-- Property: MutBorrowed constructor preserves mutable borrower name
prop_mut_borrowed_preserves_name :: String -> Property
prop_mut_borrowed_preserves_name name =
  let ownershipType = MutBorrowed name
  in case ownershipType of
       MutBorrowed n -> property $ n === name

-- Property: OwnershipType Show instance produces expected format
prop_ownership_type_show :: OwnershipType -> Property
prop_ownership_type_show ownershipType =
  let showResult = show ownershipType
      expected = case ownershipType of
                   Owned name -> "Owned " ++ name
                   Borrowed name -> "Borrowed " ++ name
                   MutBorrowed name -> "MutBorrowed " ++ name
  in property $ showResult === expected

-- Property: OwnershipType Eq works correctly
prop_ownership_type_eq :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_eq ot1 ot2 =
  let isEqual = ot1 == ot2
      expectedEqual = case (ot1, ot2) of
                        (Owned n1, Owned n2) -> n1 == n2
                        (Borrowed n1, Borrowed n2) -> n1 == n2
                        (MutBorrowed n1, MutBorrowed n2) -> n1 == n2
                        _ -> False
  in property $ isEqual === expectedEqual

-- Property: OwnershipType Ord works correctly - Owned < Borrowed < MutBorrowed
prop_ownership_type_ord :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ord ot1 ot2 =
  let comparison = compare ot1 ot2
      rankOwnershipType ot = case ot of
                              Owned _ -> 1
                              Borrowed _ -> 2
                              MutBorrowed _ -> 3
      rank1 = rankOwnershipType ot1
      rank2 = rankOwnershipType ot2
      expected = if rank1 < rank2 then LT
                 else if rank1 > rank2 then GT
                 else case (ot1, ot2) of
                        (Owned n1, Owned n2) -> compare n1 n2
                        (Borrowed n1, Borrowed n2) -> compare n1 n2
                        (MutBorrowed n1, MutBorrowed n2) -> compare n1 n2
                        _ -> EQ
  in property $ comparison === expected

-- Property: UseAfterMove constructor preserves variable name
prop_use_after_move_preserves_name :: String -> Property
prop_use_after_move_preserves_name var =
  let error = UseAfterMove var
  in case error of
       UseAfterMove v -> property $ v === var

-- Property: DoubleMove constructor preserves both variable names
prop_double_move_preserves_names :: String -> String -> Property
prop_double_move_preserves_names var1 var2 =
  let error = DoubleMove var1 var2
  in case error of
       DoubleMove v1 v2 -> property $ v1 === var1 .&&. v2 === var2

-- Property: BorrowWhileMoved constructor preserves variable name
prop_borrow_while_moved_preserves_name :: String -> Property
prop_borrow_while_moved_preserves_name var =
  let error = BorrowWhileMoved var
  in case error of
       BorrowWhileMoved v -> property $ v === var

-- Property: MutBorrowWhileBorrowed constructor preserves variable name
prop_mut_borrow_while_borrowed_preserves_name :: String -> Property
prop_mut_borrow_while_borrowed_preserves_name var =
  let error = MutBorrowWhileBorrowed var
  in case error of
       MutBorrowWhileBorrowed v -> property $ v === var

-- Property: BorrowWhileMutBorrowed constructor preserves variable name
prop_borrow_while_mut_borrowed_preserves_name :: String -> Property
prop_borrow_while_mut_borrowed_preserves_name var =
  let error = BorrowWhileMutBorrowed var
  in case error of
       BorrowWhileMutBorrowed v -> property $ v === var

-- Property: MultipleMutBorrows constructor preserves variable name
prop_multiple_mut_borrows_preserves_name :: String -> Property
prop_multiple_mut_borrows_preserves_name var =
  let error = MultipleMutBorrows var
  in case error of
       MultipleMutBorrows v -> property $ v === var

-- Property: UseWhileMutBorrowed constructor preserves variable name
prop_use_while_mut_borrowed_preserves_name :: String -> Property
prop_use_while_mut_borrowed_preserves_name var =
  let error = UseWhileMutBorrowed var
  in case error of
       UseWhileMutBorrowed v -> property $ v === var

-- Property: OutOfScope constructor preserves variable name
prop_out_of_scope_preserves_name :: String -> Property
prop_out_of_scope_preserves_name var =
  let error = OutOfScope var
  in case error of
       OutOfScope v -> property $ v === var

-- Property: BorrowError constructor preserves message
prop_borrow_error_preserves_message :: String -> Property
prop_borrow_error_preserves_message msg =
  let error = BorrowError msg
  in case error of
       BorrowError m -> property $ m === msg

-- Property: ParseError constructor preserves message
prop_parse_error_preserves_message :: String -> Property
prop_parse_error_preserves_message msg =
  let error = ParseError msg
  in case error of
       ParseError m -> property $ m === msg

-- Property: CrossFunctionMove constructor preserves both function names
prop_cross_function_move_preserves_names :: String -> String -> Property
prop_cross_function_move_preserves_names func1 func2 =
  let error = CrossFunctionMove func1 func2
  in case error of
       CrossFunctionMove f1 f2 -> property $ f1 === func1 .&&. f2 === func2

-- Property: ParameterMoveMismatch constructor preserves variable name
prop_parameter_move_mismatch_preserves_name :: String -> Property
prop_parameter_move_mismatch_preserves_name var =
  let error = ParameterMoveMismatch var
  in case error of
       ParameterMoveMismatch v -> property $ v === var

-- Property: ControlFlowError constructor preserves message
prop_control_flow_error_preserves_message :: String -> Property
prop_control_flow_error_preserves_message msg =
  let error = ControlFlowError msg
  in case error of
       ControlFlowError m -> property $ m === msg

-- Property: PathSensitiveError constructor preserves message
prop_path_sensitive_error_preserves_message :: String -> Property
prop_path_sensitive_error_preserves_message msg =
  let error = PathSensitiveError msg
  in case error of
       PathSensitiveError m -> property $ m === msg

-- Property: LoopOwnershipError constructor preserves message
prop_loop_ownership_error_preserves_message :: String -> Property
prop_loop_ownership_error_preserves_message msg =
  let error = LoopOwnershipError msg
  in case error of
       LoopOwnershipError m -> property $ m === msg

-- Property: OwnershipError Show instance produces expected format
prop_ownership_error_show :: OwnershipError -> Property
prop_ownership_error_show error =
  let showResult = show error
      expected = case error of
                   UseAfterMove var -> "UseAfterMove " ++ var
                   DoubleMove var1 var2 -> "DoubleMove " ++ var1 ++ " " ++ var2
                   BorrowWhileMoved var -> "BorrowWhileMoved " ++ var
                   MutBorrowWhileBorrowed var -> "MutBorrowWhileBorrowed " ++ var
                   BorrowWhileMutBorrowed var -> "BorrowWhileMutBorrowed " ++ var
                   MultipleMutBorrows var -> "MultipleMutBorrows " ++ var
                   UseWhileMutBorrowed var -> "UseWhileMutBorrowed " ++ var
                   OutOfScope var -> "OutOfScope " ++ var
                   BorrowError msg -> "BorrowError " ++ msg
                   ParseError msg -> "ParseError " ++ msg
                   CrossFunctionMove func1 func2 -> "CrossFunctionMove " ++ func1 ++ " " ++ func2
                   ParameterMoveMismatch var -> "ParameterMoveMismatch " ++ var
                   ControlFlowError msg -> "ControlFlowError " ++ msg
                   PathSensitiveError msg -> "PathSensitiveError " ++ msg
                   LoopOwnershipError msg -> "LoopOwnershipError " ++ msg
  in property $ showResult === expected

-- Property: OwnershipError Eq works correctly
prop_ownership_error_eq :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_eq err1 err2 =
  let isEqual = err1 == err2
      expectedEqual = case (err1, err2) of
                        (UseAfterMove v1, UseAfterMove v2) -> v1 == v2
                        (DoubleMove v1a v1b, DoubleMove v2a v2b) -> v1a == v2a && v1b == v2b
                        (BorrowWhileMoved v1, BorrowWhileMoved v2) -> v1 == v2
                        (MutBorrowWhileBorrowed v1, MutBorrowWhileBorrowed v2) -> v1 == v2
                        (BorrowWhileMutBorrowed v1, BorrowWhileMutBorrowed v2) -> v1 == v2
                        (MultipleMutBorrows v1, MultipleMutBorrows v2) -> v1 == v2
                        (UseWhileMutBorrowed v1, UseWhileMutBorrowed v2) -> v1 == v2
                        (OutOfScope v1, OutOfScope v2) -> v1 == v2
                        (BorrowError m1, BorrowError m2) -> m1 == m2
                        (ParseError m1, ParseError m2) -> m1 == m2
                        (CrossFunctionMove f1a f1b, CrossFunctionMove f2a f2b) -> f1a == f2a && f1b == f2b
                        (ParameterMoveMismatch v1, ParameterMoveMismatch v2) -> v1 == v2
                        (ControlFlowError m1, ControlFlowError m2) -> m1 == m2
                        (PathSensitiveError m1, PathSensitiveError m2) -> m1 == m2
                        (LoopOwnershipError m1, LoopOwnershipError m2) -> m1 == m2
                        _ -> False
  in property $ isEqual === expectedEqual

-- Property: OwnershipError Ord works correctly using show comparison
prop_ownership_error_ord :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ord err1 err2 =
  let comparison = compare err1 err2
      expected = compare (show err1) (show err2)
  in property $ comparison === expected

-- Property: OwnershipTransfer constructor preserves from and to variables
prop_ownership_transfer_preserves_fields :: String -> String -> Property
prop_ownership_transfer_preserves_fields fromVar toVar =
  let transfer = OwnershipTransfer fromVar toVar
  in property $ transferFrom transfer === fromVar .&&.
             transferTo transfer === toVar

-- Property: OwnershipTransfer Show instance produces expected format
prop_ownership_transfer_show :: String -> String -> Property
prop_ownership_transfer_show fromVar toVar =
  let transfer = OwnershipTransfer fromVar toVar
      showResult = show transfer
  in property $ "OwnershipTransfer {transferFrom = " ++ fromVar ++ ", transferTo = " ++ toVar ++ "}" === showResult

-- Property: OwnershipTransfer Eq works correctly
prop_ownership_transfer_eq :: String -> String -> String -> String -> Property
prop_ownership_transfer_eq from1 to1 from2 to2 =
  let transfer1 = OwnershipTransfer from1 to1
      transfer2 = OwnershipTransfer from2 to2
      isEqual = transfer1 == transfer2
      expected = from1 == from2 && to1 == to2
  in property $ isEqual === expected

-- Property: newOwnershipAnalyzer creates a valid analyzer
prop_new_ownership_analyzer_valid :: Property
prop_new_ownership_analyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
       OwnershipAnalyzer () -> property True

-- Property: OwnershipAnalyzer Show instance works
prop_ownership_analyzer_show :: Property
prop_ownership_analyzer_show =
  let analyzer = newOwnershipAnalyzer
      showResult = show analyzer
  in property $ showResult === "OwnershipAnalyzer ()"

-- Property: OwnershipAnalyzer Eq works correctly
prop_ownership_analyzer_eq :: Property
prop_ownership_analyzer_eq =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in property $ analyzer1 == analyzer2

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering_consistent :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering_consistent ot1 ot2 =
  let comparison1 = compare ot1 ot2
      comparison2 = compare (show ot1) (show ot2)
  in property $ comparison1 === comparison2

-- Property: OwnershipError ordering is consistent with show
prop_ownership_error_ordering_consistent :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering_consistent err1 err2 =
  let comparison1 = compare err1 err2
      comparison2 = compare (show err1) (show err2)
  in property $ comparison1 === comparison2

tests :: TestTree
tests = testGroup "Ownership New QuickCheck Tests"
  [ fastProperty "Owned constructor preserves owner name" prop_owned_preserves_name
  , fastProperty "Borrowed constructor preserves borrower name" prop_borrowed_preserves_name
  , fastProperty "MutBorrowed constructor preserves mutable borrower name" prop_mut_borrowed_preserves_name
  , fastProperty "OwnershipType Show instance produces expected format" prop_ownership_type_show
  , fastProperty "OwnershipType Eq works correctly" prop_ownership_type_eq
  , fastProperty "OwnershipType Ord works correctly - Owned < Borrowed < MutBorrowed" prop_ownership_type_ord
  , fastProperty "UseAfterMove constructor preserves variable name" prop_use_after_move_preserves_name
  , fastProperty "DoubleMove constructor preserves both variable names" prop_double_move_preserves_names
  , fastProperty "BorrowWhileMoved constructor preserves variable name" prop_borrow_while_moved_preserves_name
  , fastProperty "MutBorrowWhileBorrowed constructor preserves variable name" prop_mut_borrow_while_borrowed_preserves_name
  , fastProperty "BorrowWhileMutBorrowed constructor preserves variable name" prop_borrow_while_mut_borrowed_preserves_name
  , fastProperty "MultipleMutBorrows constructor preserves variable name" prop_multiple_mut_borrows_preserves_name
  , fastProperty "UseWhileMutBorrowed constructor preserves variable name" prop_use_while_mut_borrowed_preserves_name
  , fastProperty "OutOfScope constructor preserves variable name" prop_out_of_scope_preserves_name
  , fastProperty "BorrowError constructor preserves message" prop_borrow_error_preserves_message
  , fastProperty "ParseError constructor preserves message" prop_parse_error_preserves_message
  , fastProperty "CrossFunctionMove constructor preserves both function names" prop_cross_function_move_preserves_names
  , fastProperty "ParameterMoveMismatch constructor preserves variable name" prop_parameter_move_mismatch_preserves_name
  , fastProperty "ControlFlowError constructor preserves message" prop_control_flow_error_preserves_message
  , fastProperty "PathSensitiveError constructor preserves message" prop_path_sensitive_error_preserves_message
  , fastProperty "LoopOwnershipError constructor preserves message" prop_loop_ownership_error_preserves_message
  , fastProperty "OwnershipError Show instance produces expected format" prop_ownership_error_show
  , fastProperty "OwnershipError Eq works correctly" prop_ownership_error_eq
  , fastProperty "OwnershipError Ord works correctly using show comparison" prop_ownership_error_ord
  , fastProperty "OwnershipTransfer constructor preserves from and to variables" prop_ownership_transfer_preserves_fields
  , fastProperty "OwnershipTransfer Show instance produces expected format" prop_ownership_transfer_show
  , fastProperty "OwnershipTransfer Eq works correctly" prop_ownership_transfer_eq
  , fastProperty "newOwnershipAnalyzer creates a valid analyzer" prop_new_ownership_analyzer_valid
  , fastProperty "OwnershipAnalyzer Show instance works" prop_ownership_analyzer_show
  , fastProperty "OwnershipAnalyzer Eq works correctly" prop_ownership_analyzer_eq
  , fastProperty "OwnershipType ordering is consistent" prop_ownership_type_ordering_consistent
  , fastProperty "OwnershipError ordering is consistent with show" prop_ownership_error_ordering_consistent
  ]