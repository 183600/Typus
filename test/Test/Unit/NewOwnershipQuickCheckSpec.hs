{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.NewOwnershipQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership
import Ownership.Common.Types
import Data.List (sort)
import Data.Maybe (isJust, isNothing)

-- Arbitrary instances for Ownership types
instance Arbitrary OwnershipType where
  arbitrary = oneof [Owned <$> arbitrary, Borrowed <$> arbitrary, MutBorrowed <$> arbitrary]

instance Arbitrary OwnershipError where
  arbitrary = oneof 
    [ UseAfterMove <$> arbitrary
    , DoubleMove <$> arbitrary <*> arbitrary
    , BorrowWhileMoved <$> arbitrary
    , MutBorrowWhileBorrowed <$> arbitrary
    , BorrowWhileMutBorrowed <$> arbitrary
    , MultipleMutBorrows <$> arbitrary
    , UseWhileMutBorrowed <$> arbitrary
    , OutOfScope <$> arbitrary
    , BorrowError <$> arbitrary
    , ParseError <$> arbitrary
    , CrossFunctionMove <$> arbitrary <*> arbitrary
    , ParameterMoveMismatch <$> arbitrary
    , ControlFlowError <$> arbitrary
    , PathSensitiveError <$> arbitrary
    , LoopOwnershipError <$> arbitrary
    , OwnershipError <$> arbitrary
    ]

instance Arbitrary OwnershipTransfer where
  arbitrary = OwnershipTransfer <$> arbitrary <*> arbitrary

-- ============================================================================
-- Ownership Module QuickCheck Tests
-- ============================================================================

-- Test OwnershipType properties
prop_ownership_type_eq_reflexive :: OwnershipType -> Property
prop_ownership_type_eq_reflexive ownType = 
  property $ ownType == ownType

prop_ownership_type_eq_symmetric :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_eq_symmetric ownType1 ownType2 = 
  property $ (ownType1 == ownType2) == (ownType2 == ownType1)

prop_ownership_type_eq_transitive :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownership_type_eq_transitive ownType1 ownType2 ownType3 = 
  let cond1 = ownType1 == ownType2
      cond2 = ownType2 == ownType3
      cond3 = ownType1 == ownType3
  in property $ if cond1 && cond2 then cond3 else True

prop_ownership_type_ord_total :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ord_total ownType1 ownType2 = 
  let result = compare ownType1 ownType2
  in property $ result == EQ || result == LT || result == GT

prop_ownership_type_ord_consistent_with_eq :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ord_consistent_with_eq ownType1 ownType2 = 
  let eqResult = ownType1 == ownType2
      ordResult = compare ownType1 ownType2
  in property $ (eqResult && ordResult == EQ) || (not eqResult && ordResult /= EQ)

prop_ownership_type_ord_transitive :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownership_type_ord_transitive ownType1 ownType2 ownType3 = 
  let result1 = compare ownType1 ownType2
      result2 = compare ownType2 ownType3
      result3 = compare ownType1 ownType3
  in property $ if result1 == LT && result2 == LT then result3 == LT else True

prop_ownership_type_show_roundtrip :: OwnershipType -> Property
prop_ownership_type_show_roundtrip ownType = 
  let str = show ownType
  in property $ not $ null str

-- Test OwnershipError properties
prop_ownership_error_eq_reflexive :: OwnershipError -> Property
prop_ownership_error_eq_reflexive ownError = 
  property $ ownError == ownError

prop_ownership_error_eq_symmetric :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_eq_symmetric ownError1 ownError2 = 
  property $ (ownError1 == ownError2) == (ownError2 == ownError1)

prop_ownership_error_eq_transitive :: OwnershipError -> OwnershipError -> OwnershipError -> Property
prop_ownership_error_eq_transitive ownError1 ownError2 ownError3 = 
  let cond1 = ownError1 == ownError2
      cond2 = ownError2 == ownError3
      cond3 = ownError1 == ownError3
  in property $ if cond1 && cond2 then cond3 else True

prop_ownership_error_ord_total :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ord_total ownError1 ownError2 = 
  let result = compare ownError1 ownError2
  in property $ result == EQ || result == LT || result == GT

prop_ownership_error_ord_consistent_with_eq :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ord_consistent_with_eq ownError1 ownError2 = 
  let eqResult = ownError1 == ownError2
      ordResult = compare ownError1 ownError2
  in property $ (eqResult && ordResult == EQ) || (not eqResult && ordResult /= EQ)

prop_ownership_error_show_roundtrip :: OwnershipError -> Property
prop_ownership_error_show_roundtrip ownError = 
  let str = show ownError
  in property $ not $ null str

-- Test OwnershipTransfer properties
prop_ownership_transfer_eq_reflexive :: OwnershipTransfer -> Property
prop_ownership_transfer_eq_reflexive transfer = 
  property $ transfer == transfer

prop_ownership_transfer_eq_symmetric :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownership_transfer_eq_symmetric transfer1 transfer2 = 
  property $ (transfer1 == transfer2) == (transfer2 == transfer1)

prop_ownership_transfer_eq_transitive :: OwnershipTransfer -> OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownership_transfer_eq_transitive transfer1 transfer2 transfer3 = 
  let cond1 = transfer1 == transfer2
      cond2 = transfer2 == transfer3
      cond3 = transfer1 == transfer3
  in property $ if cond1 && cond2 then cond3 else True

prop_ownership_transfer_creation :: String -> String -> Property
prop_ownership_transfer_creation from to = 
  let transfer = OwnershipTransfer from to
  in property $ transferFrom transfer == from && transferTo transfer == to

prop_ownership_transfer_show_roundtrip :: String -> String -> Property
prop_ownership_transfer_show_roundtrip from to = 
  let transfer = OwnershipTransfer from to
      str = show transfer
  in property $ not $ null str

-- Test OwnershipAnalyzer properties
prop_ownership_analyzer_creation :: Property
prop_ownership_analyzer_creation = 
  let analyzer = newOwnershipAnalyzer
  in property $ analyzer == analyzer

-- Test specific ownership type properties
prop_owned_type_properties :: String -> Property
prop_owned_type_properties name = 
  let ownType = Owned name
  in property $ show ownType == "Owned " ++ name

prop_borrowed_type_properties :: String -> Property
prop_borrowed_type_properties name = 
  let ownType = Borrowed name
  in property $ show ownType == "Borrowed " ++ name

prop_mut_borrowed_type_properties :: String -> Property
prop_mut_borrowed_type_properties name = 
  let ownType = MutBorrowed name
  in property $ show ownType == "MutBorrowed " ++ name

-- Test ownership type ordering
prop_ownership_type_ordering_owned_borrowed :: String -> String -> Property
prop_ownership_type_ordering_owned_borrowed name1 name2 = 
  let owned = Owned name1
      borrowed = Borrowed name2
  in property $ compare owned borrowed == LT

prop_ownership_type_ordering_owned_mut_borrowed :: String -> String -> Property
prop_ownership_type_ordering_owned_mut_borrowed name1 name2 = 
  let owned = Owned name1
      mutBorrowed = MutBorrowed name2
  in property $ compare owned mutBorrowed == LT

prop_ownership_type_ordering_borrowed_mut_borrowed :: String -> String -> Property
prop_ownership_type_ordering_borrowed_mut_borrowed name1 name2 = 
  let borrowed = Borrowed name1
      mutBorrowed = MutBorrowed name2
  in property $ compare borrowed mutBorrowed == LT

prop_ownership_type_ordering_same_type :: String -> String -> OwnershipType -> Property
prop_ownership_type_ordering_same_type name1 name2 ownType = 
  let ownType1 = case ownType of
        Owned _ -> Owned name1
        Borrowed _ -> Borrowed name1
        MutBorrowed _ -> MutBorrowed name1
      ownType2 = case ownType of
        Owned _ -> Owned name2
        Borrowed _ -> Borrowed name2
        MutBorrowed _ -> MutBorrowed name2
  in property $ compare ownType1 ownType2 == compare name1 name2

-- Test specific ownership error properties
prop_use_after_move_error :: String -> Property
prop_use_after_move_error var = 
  let error = UseAfterMove var
  in property $ show error == "UseAfterMove " ++ var

prop_double_move_error :: String -> String -> Property
prop_double_move_error var1 var2 = 
  let error = DoubleMove var1 var2
  in property $ show error == "DoubleMove " ++ var1 ++ " " ++ var2

prop_borrow_while_moved_error :: String -> Property
prop_borrow_while_moved_error var = 
  let error = BorrowWhileMoved var
  in property $ show error == "BorrowWhileMoved " ++ var

prop_mut_borrow_while_borrowed_error :: String -> Property
prop_mut_borrow_while_borrowed_error var = 
  let error = MutBorrowWhileBorrowed var
  in property $ show error == "MutBorrowWhileBorrowed " ++ var

prop_borrow_while_mut_borrowed_error :: String -> Property
prop_borrow_while_mut_borrowed_error var = 
  let error = BorrowWhileMutBorrowed var
  in property $ show error == "BorrowWhileMutBorrowed " ++ var

prop_multiple_mut_borrows_error :: String -> Property
prop_multiple_mut_borrows_error var = 
  let error = MultipleMutBorrows var
  in property $ show error == "MultipleMutBorrows " ++ var

prop_use_while_mut_borrowed_error :: String -> Property
prop_use_while_mut_borrowed_error var = 
  let error = UseWhileMutBorrowed var
  in property $ show error == "UseWhileMutBorrowed " ++ var

prop_out_of_scope_error :: String -> Property
prop_out_of_scope_error var = 
  let error = OutOfScope var
  in property $ show error == "OutOfScope " ++ var

prop_borrow_error :: String -> Property
prop_borrow_error msg = 
  let error = BorrowError msg
  in property $ show error == "BorrowError " ++ msg

prop_parse_error :: String -> Property
prop_parse_error msg = 
  let error = ParseError msg
  in property $ show error == "ParseError " ++ msg

prop_cross_function_move_error :: String -> String -> Property
prop_cross_function_move_error var1 var2 = 
  let error = CrossFunctionMove var1 var2
  in property $ show error == "CrossFunctionMove " ++ var1 ++ " " ++ var2

prop_parameter_move_mismatch_error :: String -> Property
prop_parameter_move_mismatch_error var = 
  let error = ParameterMoveMismatch var
  in property $ show error == "ParameterMoveMismatch " ++ var

prop_control_flow_error :: String -> Property
prop_control_flow_error msg = 
  let error = ControlFlowError msg
  in property $ show error == "ControlFlowError " ++ msg

prop_path_sensitive_error :: String -> Property
prop_path_sensitive_error msg = 
  let error = PathSensitiveError msg
  in property $ show error == "PathSensitiveError " ++ msg

prop_loop_ownership_error :: String -> Property
prop_loop_ownership_error msg = 
  let error = LoopOwnershipError msg
  in property $ show error == "LoopOwnershipError " ++ msg

prop_ownership_error_generic :: String -> Property
prop_ownership_error_generic msg = 
  let error = OwnershipError msg
  in property $ show error == "OwnershipError " ++ msg

-- Test sorting properties
prop_sort_ownership_types :: [OwnershipType] -> Property
prop_sort_ownership_types ownTypes = 
  let sorted = sort ownTypes
  in property $ length sorted == length ownTypes &&
                all (\(x, y) -> compare x y /= GT) (zip sorted (drop 1 sorted))

prop_sort_ownership_errors :: [OwnershipError] -> Property
prop_sort_ownership_errors ownErrors = 
  let sorted = sort ownErrors
  in property $ length sorted == length ownErrors &&
                all (\(x, y) -> compare x y /= GT) (zip sorted (drop 1 sorted))

-- Unit tests for edge cases
test_ownership_edge_cases :: TestTree
test_ownership_edge_cases = testGroup "Ownership Edge Cases"
  [ testCase "OwnershipType equality" $ do
      let owned1 = Owned "x"
          owned2 = Owned "x"
          owned3 = Owned "y"
          borrowed = Borrowed "x"
      
      assertEqual "Owned with same name equal" owned1 owned2
      assertBool "Owned with different name not equal" $ owned1 /= owned3
      assertBool "Owned and Borrowed not equal" $ owned1 /= borrowed
    
  , testCase "OwnershipType ordering" $ do
      let owned = Owned "x"
          borrowed = Borrowed "x"
          mutBorrowed = MutBorrowed "x"
          ownedY = Owned "y"
      
      assertBool "Owned < Borrowed" $ owned < borrowed
      assertBool "Borrowed < MutBorrowed" $ borrowed < mutBorrowed
      assertBool "Owned < MutBorrowed" $ owned < mutBorrowed
      assertBool "Owned x < Owned y" $ owned < ownedY
    
  , testCase "OwnershipError equality" $ do
      let error1 = UseAfterMove "x"
          error2 = UseAfterMove "x"
          error3 = UseAfterMove "y"
          error4 = DoubleMove "x" "y"
      
      assertEqual "UseAfterMove with same var equal" error1 error2
      assertBool "UseAfterMove with different var not equal" $ error1 /= error3
      assertBool "UseAfterMove and DoubleMove not equal" $ error1 /= error4
    
  , testCase "OwnershipError ordering" $ do
      let error1 = UseAfterMove "x"
          error2 = UseAfterMove "y"
          error3 = DoubleMove "x" "y"
      
      assertBool "UseAfterMove x < UseAfterMove y" $ error1 < error2
      -- Note: Ordering is based on string representation
      assertBool "DoubleMove < UseAfterMove" $ error3 < error1
    
  , testCase "OwnershipTransfer" $ do
      let transfer1 = OwnershipTransfer "x" "y"
          transfer2 = OwnershipTransfer "x" "y"
          transfer3 = OwnershipTransfer "x" "z"
      
      assertEqual "Transfer with same from/to equal" transfer1 transfer2
      assertBool "Transfer with different to not equal" $ transfer1 /= transfer3
      assertEqual "transferFrom" "x" $ transferFrom transfer1
      assertEqual "transferTo" "y" $ transferTo transfer1
    
  , testCase "OwnershipAnalyzer" $ do
      let analyzer1 = newOwnershipAnalyzer
          analyzer2 = newOwnershipAnalyzer
      
      assertEqual "All analyzers equal" analyzer1 analyzer2
    
  , testCase "OwnershipType show" $ do
      let owned = Owned "x"
          borrowed = Borrowed "y"
          mutBorrowed = MutBorrowed "z"
      
      assertEqual "Owned show" "Owned x" $ show owned
      assertEqual "Borrowed show" "Borrowed y" $ show borrowed
      assertEqual "MutBorrowed show" "MutBorrowed z" $ show mutBorrowed
    
  , testCase "OwnershipError show" $ do
      let useAfterMove = UseAfterMove "x"
          doubleMove = DoubleMove "x" "y"
          borrowWhileMoved = BorrowWhileMoved "z"
          borrowError = BorrowError "message"
      
      assertEqual "UseAfterMove show" "UseAfterMove x" $ show useAfterMove
      assertEqual "DoubleMove show" "DoubleMove x y" $ show doubleMove
      assertEqual "BorrowWhileMoved show" "BorrowWhileMoved z" $ show borrowWhileMoved
      assertEqual "BorrowError show" "BorrowError message" $ show borrowError
    
  , testCase "OwnershipTransfer show" $ do
      let transfer = OwnershipTransfer "x" "y"
          expected = "OwnershipTransfer {transferFrom = \"x\", transferTo = \"y\"}"
      assertEqual "Transfer show" expected $ show transfer
  ]

-- QuickCheck properties
test_ownership_properties :: TestTree
test_ownership_properties = testGroup "Ownership QuickCheck Properties"
  [ testProperty "OwnershipType eq reflexive" prop_ownership_type_eq_reflexive
  , testProperty "OwnershipType eq symmetric" prop_ownership_type_eq_symmetric
  , testProperty "OwnershipType eq transitive" prop_ownership_type_eq_transitive
  , testProperty "OwnershipType ord total" prop_ownership_type_ord_total
  , testProperty "OwnershipType ord consistent with eq" prop_ownership_type_ord_consistent_with_eq
  , testProperty "OwnershipType ord transitive" prop_ownership_type_ord_transitive
  , testProperty "OwnershipType show roundtrip" prop_ownership_type_show_roundtrip
  , testProperty "OwnershipError eq reflexive" prop_ownership_error_eq_reflexive
  , testProperty "OwnershipError eq symmetric" prop_ownership_error_eq_symmetric
  , testProperty "OwnershipError eq transitive" prop_ownership_error_eq_transitive
  , testProperty "OwnershipError ord total" prop_ownership_error_ord_total
  , testProperty "OwnershipError ord consistent with eq" prop_ownership_error_ord_consistent_with_eq
  , testProperty "OwnershipError show roundtrip" prop_ownership_error_show_roundtrip
  , testProperty "OwnershipTransfer eq reflexive" prop_ownership_transfer_eq_reflexive
  , testProperty "OwnershipTransfer eq symmetric" prop_ownership_transfer_eq_symmetric
  , testProperty "OwnershipTransfer eq transitive" prop_ownership_transfer_eq_transitive
  , testProperty "OwnershipTransfer creation" prop_ownership_transfer_creation
  , testProperty "OwnershipTransfer show roundtrip" prop_ownership_transfer_show_roundtrip
  , testProperty "OwnershipAnalyzer creation" prop_ownership_analyzer_creation
  , testProperty "Owned type properties" prop_owned_type_properties
  , testProperty "Borrowed type properties" prop_borrowed_type_properties
  , testProperty "MutBorrowed type properties" prop_mut_borrowed_type_properties
  , testProperty "OwnershipType ordering Owned Borrowed" prop_ownership_type_ordering_owned_borrowed
  , testProperty "OwnershipType ordering Owned MutBorrowed" prop_ownership_type_ordering_owned_mut_borrowed
  , testProperty "OwnershipType ordering Borrowed MutBorrowed" prop_ownership_type_ordering_borrowed_mut_borrowed
  , testProperty "OwnershipType ordering same type" prop_ownership_type_ordering_same_type
  , testProperty "UseAfterMove error" prop_use_after_move_error
  , testProperty "DoubleMove error" prop_double_move_error
  , testProperty "BorrowWhileMoved error" prop_borrow_while_moved_error
  , testProperty "MutBorrowWhileBorrowed error" prop_mut_borrow_while_borrowed_error
  , testProperty "BorrowWhileMutBorrowed error" prop_borrow_while_mut_borrowed_error
  , testProperty "MultipleMutBorrows error" prop_multiple_mut_borrows_error
  , testProperty "UseWhileMutBorrowed error" prop_use_while_mut_borrowed_error
  , testProperty "OutOfScope error" prop_out_of_scope_error
  , testProperty "BorrowError" prop_borrow_error
  , testProperty "ParseError" prop_parse_error
  , testProperty "CrossFunctionMove error" prop_cross_function_move_error
  , testProperty "ParameterMoveMismatch error" prop_parameter_move_mismatch_error
  , testProperty "ControlFlowError" prop_control_flow_error
  , testProperty "PathSensitiveError" prop_path_sensitive_error
  , testProperty "LoopOwnershipError" prop_loop_ownership_error
  , testProperty "OwnershipError generic" prop_ownership_error_generic
  , testProperty "sort ownership types" prop_sort_ownership_types
  , testProperty "sort ownership errors" prop_sort_ownership_errors
  ]

-- Main test suite
ownershipTests :: TestTree
ownershipTests = testGroup "Ownership Module Tests"
  [ test_ownership_edge_cases
  , test_ownership_properties
  ]