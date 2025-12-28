{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck.Gen (Gen(..), vectorOf)

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = do
    name <- listOf $ elements ['a'..'z'] ++ ['0'..'9'] ++ ['_']
    oneof [return $ Owned name, return $ Borrowed name, return $ MutBorrowed name]

instance Arbitrary OwnershipError where
  arbitrary = do
    var1 <- listOf $ elements ['a'..'z'] ++ ['0'..'9'] ++ ['_']
    var2 <- listOf $ elements ['a'..'z'] ++ ['0'..'9'] ++ ['_']
    msg <- listOf $ elements ['a'..'z'] ++ [' ']
    oneof
      [ return $ UseAfterMove var1
      , return $ DoubleMove var1 var2
      , return $ BorrowWhileMoved var1
      , return $ MutBorrowWhileBorrowed var1
      , return $ BorrowWhileMutBorrowed var1
      , return $ MultipleMutBorrows var1
      , return $ UseWhileMutBorrowed var1
      , return $ OutOfScope var1
      , return $ BorrowError msg
      , return $ ParseError msg
      , return $ CrossFunctionMove var1 var2
      , return $ ParameterMoveMismatch var1
      , return $ ControlFlowError msg
      , return $ PathSensitiveError msg
      , return $ LoopOwnershipError msg
      ]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    fromVar <- listOf $ elements ['a'..'z'] ++ ['0'..'9'] ++ ['_']
    toVar <- listOf $ elements ['a'..'z'] ++ ['0'..'9'] ++ ['_']
    return $ OwnershipTransfer fromVar toVar

instance Arbitrary OwnershipAnalyzer where
  arbitrary = return newOwnershipAnalyzer

-- Generate valid variable name
validVarName :: Gen String
validVarName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generate valid error message
validErrorMessage :: Gen String
validErrorMessage = listOf $ elements ['a'..'z'] ++ [' ']

-- ============================================================================
-- OwnershipType Property Tests
-- ============================================================================

-- Property: Show and Read consistency for OwnershipType
prop_ownership_type_show_read :: OwnershipType -> Property
prop_ownership_type_show_read ownType =
  let shown = show ownType
      parsed = case shown of
        'O':'w':'n':'e':'d':' ':name -> Owned name
        'B':'o':'r':'r':'o':'w':'e':'d':' ':name -> Borrowed name
        'M':'u':'t':'B':'o':'r':'r':'o':'w':'e':'d':' ':name -> MutBorrowed name
        _ -> undefined
  in parsed === ownType

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering own1 own2 =
  let ord1 = compare own1 own2
      ord2 = compare (show own1) (show own2)
  in property $ (ord1 == EQ) === (ord2 == EQ) .&&.
               (ord1 == LT) === (ord2 == LT) .&&.
               (ord1 == GT) === (ord2 == GT)

-- Property: Owned types are ordered by name
prop_owned_ordering_by_name :: String -> String -> Property
prop_owned_ordering_by_name name1 name2 =
  let own1 = Owned name1
      own2 = Owned name2
  in compare own1 own2 === compare name1 name2

-- Property: Borrowed types are ordered by name
prop_borrowed_ordering_by_name :: String -> String -> Property
prop_borrowed_ordering_by_name name1 name2 =
  let own1 = Borrowed name1
      own2 = Borrowed name2
  in compare own1 own2 === compare name1 name2

-- Property: MutBorrowed types are ordered by name
prop_mut_borrowed_ordering_by_name :: String -> String -> Property
prop_mut_borrowed_ordering_by_name name1 name2 =
  let own1 = MutBorrowed name1
      own2 = MutBorrowed name2
  in compare own1 own2 === compare name1 name2

-- Property: Owned < Borrowed < MutBorrowed ordering
prop_ownership_type_hierarchy :: String -> Property
prop_ownership_type_hierarchy name =
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
  in property $ compare owned borrowed === LT .&&.
               compare borrowed mutBorrowed === LT .&&.
               compare owned mutBorrowed === LT

-- ============================================================================
-- OwnershipError Property Tests
-- ============================================================================

-- Property: Show and Read consistency for OwnershipError
prop_ownership_error_show_read :: OwnershipError -> Property
prop_ownership_error_show_read err =
  let shown = show err
      parsed = case words shown of
        ["UseAfterMove", var] -> UseAfterMove var
        ["DoubleMove", var1, var2] -> DoubleMove var1 var2
        ["BorrowWhileMoved", var] -> BorrowWhileMoved var
        ["MutBorrowWhileBorrowed", var] -> MutBorrowWhileBorrowed var
        ["BorrowWhileMutBorrowed", var] -> BorrowWhileMutBorrowed var
        ["MultipleMutBorrows", var] -> MultipleMutBorrows var
        ["UseWhileMutBorrowed", var] -> UseWhileMutBorrowed var
        ["OutOfScope", var] -> OutOfScope var
        ["BorrowError", msg] -> BorrowError (unwords msg)
        ["ParseError", msg] -> ParseError (unwords msg)
        ["CrossFunctionMove", var1, var2] -> CrossFunctionMove var1 var2
        ["ParameterMoveMismatch", var] -> ParameterMoveMismatch var
        ["ControlFlowError", msg] -> ControlFlowError (unwords msg)
        ["PathSensitiveError", msg] -> PathSensitiveError (unwords msg)
        ["LoopOwnershipError", msg] -> LoopOwnershipError (unwords msg)
        _ -> undefined
  in parsed === err

-- Property: OwnershipError ordering is consistent
prop_ownership_error_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering err1 err2 =
  let ord1 = compare err1 err2
      ord2 = compare (show err1) (show err2)
  in property $ (ord1 == EQ) === (ord2 == EQ) .&&.
               (ord1 == LT) === (ord2 == LT) .&&.
               (ord1 == GT) === (ord2 == GT)

-- Property: UseAfterMove error contains variable name
prop_use_after_move_contains_var :: String -> Property
prop_use_after_move_contains_var var =
  let err = UseAfterMove var
      shown = show err
  in property $ var `isInfixOf` shown

-- Property: DoubleMove error contains both variable names
prop_double_move_contains_vars :: String -> String -> Property
prop_double_move_contains_vars var1 var2 =
  let err = DoubleMove var1 var2
      shown = show err
  in property $ var1 `isInfixOf` shown .&&. var2 `isInfixOf` shown

-- Property: BorrowWhileMoved error contains variable name
prop_borrow_while_moved_contains_var :: String -> Property
prop_borrow_while_moved_contains_var var =
  let err = BorrowWhileMoved var
      shown = show err
  in property $ var `isInfixOf` shown

-- Property: MutBorrowWhileBorrowed error contains variable name
prop_mut_borrow_while_borrowed_contains_var :: String -> Property
prop_mut_borrow_while_borrowed_contains_var var =
  let err = MutBorrowWhileBorrowed var
      shown = show err
  in property $ var `isInfixOf` shown

-- Property: BorrowWhileMutBorrowed error contains variable name
prop_borrow_while_mut_borrowed_contains_var :: String -> Property
prop_borrow_while_mut_borrowed_contains_var var =
  let err = BorrowWhileMutBorrowed var
      shown = show err
  in property $ var `isInfixOf` shown

-- Property: MultipleMutBorrows error contains variable name
prop_multiple_mut_borrows_contains_var :: String -> Property
prop_multiple_mut_borrows_contains_var var =
  let err = MultipleMutBorrows var
      shown = show err
  in property $ var `isInfixOf` shown

-- Property: UseWhileMutBorrowed error contains variable name
prop_use_while_mut_borrowed_contains_var :: String -> Property
prop_use_while_mut_borrowed_contains_var var =
  let err = UseWhileMutBorrowed var
      shown = show err
  in property $ var `isInfixOf` shown

-- Property: OutOfScope error contains variable name
prop_out_of_scope_contains_var :: String -> Property
prop_out_of_scope_contains_var var =
  let err = OutOfScope var
      shown = show err
  in property $ var `isInfixOf` shown

-- Property: BorrowError error contains message
prop_borrow_error_contains_msg :: String -> Property
prop_borrow_error_contains_msg msg =
  let err = BorrowError msg
      shown = show err
  in property $ msg `isInfixOf` shown

-- Property: ParseError error contains message
prop_parse_error_contains_msg :: String -> Property
prop_parse_error_contains_msg msg =
  let err = ParseError msg
      shown = show err
  in property $ msg `isInfixOf` shown

-- Property: CrossFunctionMove error contains both variable names
prop_cross_function_move_contains_vars :: String -> String -> Property
prop_cross_function_move_contains_vars var1 var2 =
  let err = CrossFunctionMove var1 var2
      shown = show err
  in property $ var1 `isInfixOf` shown .&&. var2 `isInfixOf` shown

-- Property: ParameterMoveMismatch error contains variable name
prop_parameter_move_mismatch_contains_var :: String -> Property
prop_parameter_move_mismatch_contains_var var =
  let err = ParameterMoveMismatch var
      shown = show err
  in property $ var `isInfixOf` shown

-- Property: ControlFlowError error contains message
prop_control_flow_error_contains_msg :: String -> Property
prop_control_flow_error_contains_msg msg =
  let err = ControlFlowError msg
      shown = show err
  in property $ msg `isInfixOf` shown

-- Property: PathSensitiveError error contains message
prop_path_sensitive_error_contains_msg :: String -> Property
prop_path_sensitive_error_contains_msg msg =
  let err = PathSensitiveError msg
      shown = show err
  in property $ msg `isInfixOf` shown

-- Property: LoopOwnershipError error contains message
prop_loop_ownership_error_contains_msg :: String -> Property
prop_loop_ownership_error_contains_msg msg =
  let err = LoopOwnershipError msg
      shown = show err
  in property $ msg `isInfixOf` shown

-- ============================================================================
-- OwnershipTransfer Property Tests
-- ============================================================================

-- Property: OwnershipTransfer preserves from and to variables
prop_ownership_transfer_preserves_vars :: String -> String -> Property
prop_ownership_transfer_preserves_vars fromVar toVar =
  let transfer = OwnershipTransfer fromVar toVar
  in property $ transferFrom transfer === fromVar .&&.
               transferTo transfer === toVar

-- Property: OwnershipTransfer with same variables is valid
prop_ownership_transfer_same_vars :: String -> Property
prop_ownership_transfer_same_vars var =
  let transfer = OwnershipTransfer var var
  in property $ transferFrom transfer === transferTo transfer

-- Property: OwnershipTransfer show includes both variables
prop_ownership_transfer_show_contains_vars :: String -> String -> Property
prop_ownership_transfer_show_contains_vars fromVar toVar =
  let transfer = OwnershipTransfer fromVar toVar
      shown = show transfer
  in property $ fromVar `isInfixOf` shown .&&. toVar `isInfixOf` shown

-- ============================================================================
-- OwnershipAnalyzer Property Tests
-- ============================================================================

-- Property: newOwnershipAnalyzer creates consistent analyzer
prop_new_ownership_analyzer_consistent :: Property
prop_new_ownership_analyzer_consistent =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in property $ analyzer1 === analyzer2

-- Property: OwnershipAnalyzer Show is consistent
prop_ownership_analyzer_show :: Property
prop_ownership_analyzer_show =
  let analyzer = newOwnershipAnalyzer
      shown = show analyzer
  in property $ not (null shown)

-- ============================================================================
-- Advanced Property Tests
-- ============================================================================

-- Property: OwnershipType equality is reflexive
prop_ownership_type_equality_reflexive :: OwnershipType -> Property
prop_ownership_type_equality_reflexive ownType =
  ownType === ownType

-- Property: OwnershipType equality is symmetric
prop_ownership_type_equality_symmetric :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_equality_symmetric own1 own2 =
  (own1 == own2) === (own2 == own1)

-- Property: OwnershipType equality is transitive
prop_ownership_type_equality_transitive :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownership_type_equality_transitive own1 own2 own3 =
  (own1 == own2 && own2 == own3) ==> (own1 == own3)

-- Property: OwnershipError equality is reflexive
prop_ownership_error_equality_reflexive :: OwnershipError -> Property
prop_ownership_error_equality_reflexive err =
  err === err

-- Property: OwnershipError equality is symmetric
prop_ownership_error_equality_symmetric :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_equality_symmetric err1 err2 =
  (err1 == err2) === (err2 == err1)

-- Property: OwnershipError equality is transitive
prop_ownership_error_equality_transitive :: OwnershipError -> OwnershipError -> OwnershipError -> Property
prop_ownership_error_equality_transitive err1 err2 err3 =
  (err1 == err2 && err2 == err3) ==> (err1 == err3)

-- Property: OwnershipTransfer equality is reflexive
prop_ownership_transfer_equality_reflexive :: OwnershipTransfer -> Property
prop_ownership_transfer_equality_reflexive transfer =
  transfer === transfer

-- Property: OwnershipTransfer equality is symmetric
prop_ownership_transfer_equality_symmetric :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownership_transfer_equality_symmetric transfer1 transfer2 =
  (transfer1 == transfer2) === (transfer2 == transfer1)

-- Property: OwnershipTransfer equality is transitive
prop_ownership_transfer_equality_transitive :: OwnershipTransfer -> OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownership_transfer_equality_transitive transfer1 transfer2 transfer3 =
  (transfer1 == transfer2 && transfer2 == transfer3) ==> (transfer1 == transfer3)

-- Property: OwnershipAnalyzer equality is reflexive
prop_ownership_analyzer_equality_reflexive :: Property
prop_ownership_analyzer_equality_reflexive =
  let analyzer = newOwnershipAnalyzer
  in analyzer === analyzer

-- Property: OwnershipAnalyzer equality is symmetric
prop_ownership_analyzer_equality_symmetric :: Property
prop_ownership_analyzer_equality_symmetric =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in (analyzer1 == analyzer2) === (analyzer2 == analyzer1)

-- Property: OwnershipAnalyzer equality is transitive
prop_ownership_analyzer_equality_transitive :: Property
prop_ownership_analyzer_equality_transitive =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
      analyzer3 = newOwnershipAnalyzer
  in (analyzer1 == analyzer2 && analyzer2 == analyzer3) ==> (analyzer1 == analyzer3)

-- Property: OwnershipType with same name but different type are not equal
prop_ownership_type_different_type :: String -> Property
prop_ownership_type_different_type name =
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
  in property $ owned /= borrowed .&&.
               owned /= mutBorrowed .&&.
               borrowed /= mutBorrowed

-- Property: OwnershipError with different constructors are not equal
prop_ownership_error_different_constructors :: String -> Property
prop_ownership_error_different_constructors var =
  let useAfterMove = UseAfterMove var
      doubleMove = DoubleMove var var
      borrowWhileMoved = BorrowWhileMoved var
  in property $ useAfterMove /= doubleMove .&&.
               useAfterMove /= borrowWhileMoved .&&.
               doubleMove /= borrowWhileMoved

-- Property: OwnershipTransfer with different variables are not equal
prop_ownership_transfer_different_vars :: String -> String -> String -> Property
prop_ownership_transfer_different_vars var1 var2 var3 =
  var1 /= var2 && var2 /= var3 && var1 /= var3 ==>
  let transfer1 = OwnershipTransfer var1 var2
      transfer2 = OwnershipTransfer var2 var3
      transfer3 = OwnershipTransfer var1 var3
  in property $ transfer1 /= transfer2 .&&.
               transfer1 /= transfer3 .&&.
               transfer2 /= transfer3

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Ownership QuickCheck Tests"
  [ fastProperty "Show and Read consistency for OwnershipType" prop_ownership_type_show_read
  , fastProperty "OwnershipType ordering is consistent" prop_ownership_type_ordering
  , fastProperty "Owned types are ordered by name" prop_owned_ordering_by_name
  , fastProperty "Borrowed types are ordered by name" prop_borrowed_ordering_by_name
  , fastProperty "MutBorrowed types are ordered by name" prop_mut_borrowed_ordering_by_name
  , fastProperty "Owned < Borrowed < MutBorrowed ordering" prop_ownership_type_hierarchy
  , fastProperty "Show and Read consistency for OwnershipError" prop_ownership_error_show_read
  , fastProperty "OwnershipError ordering is consistent" prop_ownership_error_ordering
  , fastProperty "UseAfterMove error contains variable name" prop_use_after_move_contains_var
  , fastProperty "DoubleMove error contains both variable names" prop_double_move_contains_vars
  , fastProperty "BorrowWhileMoved error contains variable name" prop_borrow_while_moved_contains_var
  , fastProperty "MutBorrowWhileBorrowed error contains variable name" prop_mut_borrow_while_borrowed_contains_var
  , fastProperty "BorrowWhileMutBorrowed error contains variable name" prop_borrow_while_mut_borrowed_contains_var
  , fastProperty "MultipleMutBorrows error contains variable name" prop_multiple_mut_borrows_contains_var
  , fastProperty "UseWhileMutBorrowed error contains variable name" prop_use_while_mut_borrowed_contains_var
  , fastProperty "OutOfScope error contains variable name" prop_out_of_scope_contains_var
  , fastProperty "BorrowError error contains message" prop_borrow_error_contains_msg
  , fastProperty "ParseError error contains message" prop_parse_error_contains_msg
  , fastProperty "CrossFunctionMove error contains both variable names" prop_cross_function_move_contains_vars
  , fastProperty "ParameterMoveMismatch error contains variable name" prop_parameter_move_mismatch_contains_var
  , fastProperty "ControlFlowError error contains message" prop_control_flow_error_contains_msg
  , fastProperty "PathSensitiveError error contains message" prop_path_sensitive_error_contains_msg
  , fastProperty "LoopOwnershipError error contains message" prop_loop_ownership_error_contains_msg
  , fastProperty "OwnershipTransfer preserves from and to variables" prop_ownership_transfer_preserves_vars
  , fastProperty "OwnershipTransfer with same variables is valid" prop_ownership_transfer_same_vars
  , fastProperty "OwnershipTransfer show includes both variables" prop_ownership_transfer_show_contains_vars
  , fastProperty "newOwnershipAnalyzer creates consistent analyzer" prop_new_ownership_analyzer_consistent
  , fastProperty "OwnershipAnalyzer Show is consistent" prop_ownership_analyzer_show
  , fastProperty "OwnershipType equality is reflexive" prop_ownership_type_equality_reflexive
  , fastProperty "OwnershipType equality is symmetric" prop_ownership_type_equality_symmetric
  , fastProperty "OwnershipType equality is transitive" prop_ownership_type_equality_transitive
  , fastProperty "OwnershipError equality is reflexive" prop_ownership_error_equality_reflexive
  , fastProperty "OwnershipError equality is symmetric" prop_ownership_error_equality_symmetric
  , fastProperty "OwnershipError equality is transitive" prop_ownership_error_equality_transitive
  , fastProperty "OwnershipTransfer equality is reflexive" prop_ownership_transfer_equality_reflexive
  , fastProperty "OwnershipTransfer equality is symmetric" prop_ownership_transfer_equality_symmetric
  , fastProperty "OwnershipTransfer equality is transitive" prop_ownership_transfer_equality_transitive
  , fastProperty "OwnershipAnalyzer equality is reflexive" prop_ownership_analyzer_equality_reflexive
  , fastProperty "OwnershipAnalyzer equality is symmetric" prop_ownership_analyzer_equality_symmetric
  , fastProperty "OwnershipAnalyzer equality is transitive" prop_ownership_analyzer_equality_transitive
  , fastProperty "OwnershipType with same name but different type are not equal" prop_ownership_type_different_type
  , fastProperty "OwnershipError with different constructors are not equal" prop_ownership_error_different_constructors
  , fastProperty "OwnershipTransfer with different variables are not equal" prop_ownership_transfer_different_vars
  ]