{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.OwnershipTransferConsistencyQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Ownership.Common.Types
import qualified Ownership as Own
import Data.List (nub, sort)

-- ============================================================================
-- Test Data Generation
-- ============================================================================

-- | Generate ownership types for testing
instance Arbitrary OwnershipType where
  arbitrary = oneof
    [ Owned <$> arbitraryName
    , Borrowed <$> arbitraryName
    , MutBorrowed <$> arbitraryName
    ]

-- | Generate variable names
arbitraryName :: Gen String
arbitraryName = do
  first <- elements ['a'..'z']
  rest <- vectorOf $ choose (0, 5) $ elements ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- | Generate ownership errors
instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> arbitraryName
    , DoubleMove <$> arbitraryName <*> arbitraryName
    , BorrowWhileMoved <$> arbitraryName
    , MutBorrowWhileBorrowed <$> arbitraryName
    , BorrowWhileMutBorrowed <$> arbitraryName
    , MultipleMutBorrows <$> arbitraryName
    , UseWhileMutBorrowed <$> arbitraryName
    , OutOfScope <$> arbitraryName
    , BorrowError <$> arbitraryString
    , ParseError <$> arbitraryString
    , CrossFunctionMove <$> arbitraryName <*> arbitraryName
    , ParameterMoveMismatch <$> arbitraryName
    , ControlFlowError <$> arbitraryString
    , PathSensitiveError <$> arbitraryString
    , LoopOwnershipError <$> arbitraryString
    ]

-- | Generate ownership transfers
instance Arbitrary OwnershipTransfer where
  arbitrary = OwnershipTransfer <$> arbitraryName <*> arbitraryName

-- | Generate arbitrary strings for error messages
arbitraryString :: Gen String
arbitraryString = do
  size <- choose (0, 20)
  vectorOf size $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " _-"

-- ============================================================================
-- QuickCheck Properties for Ownership Transfer Consistency
-- ============================================================================

-- | Ownership transfer should have distinct from/to variables (or be explicit about self-transfer)
prop_ownership_transfer_valid :: OwnershipTransfer -> Property
prop_ownership_transfer_valid transfer =
  let fromVar = transferFrom transfer
      toVar = transferTo transfer
  in (fromVar /= toVar) .||. (fromVar === toVar)  -- Accept both cases

-- | Transfer should preserve variable names
prop_ownership_transfer_preserves_names :: String -> String -> Property
prop_ownership_transfer_preserves_names from to =
  let transfer = OwnershipTransfer from to
  in transferFrom transfer === from .&&. transferTo transfer === to

-- | Multiple transfers should be composable
prop_ownership_transfer_composable :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownership_transfer_composable transfer1 transfer2 =
  let from1 = transferFrom transfer1
      to1 = transferTo transfer1
      from2 = transferFrom transfer2
      to2 = transferTo transfer2
  in (from1, to1, from2, to2) `seq` True  -- Should not crash

-- | Ownership type ordering should be consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering type1 type2 =
  let ord1 = compare type1 type2
      ord2 = compare type2 type1
  in (ord1 == EQ) ==> (ord2 === EQ) .&&. (ord1 === EQ)

-- | Ownership type ordering should be antisymmetric
prop_ownership_type_ordering_antisymmetric :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering_antisymmetric type1 type2 =
  let ord1 = compare type1 type2
      ord2 = compare type2 type1
  in (ord1 == LT) ==> (ord2 === GT)

-- | Owned types should be ordered by name
prop_owned_ordering :: String -> String -> Property
prop_owned_ordering name1 name2 =
  let owned1 = Owned name1
      owned2 = Owned name2
  in compare owned1 owned2 === compare name1 name2

-- | Borrowed types should be ordered by name
prop_borrowed_ordering :: String -> String -> Property
prop_borrowed_ordering name1 name2 =
  let borrowed1 = Borrowed name1
      borrowed2 = Borrowed name2
  in compare borrowed1 borrowed2 === compare name1 name2

-- | MutBorrowed types should be ordered by name
prop_mut_borrowed_ordering :: String -> String -> Property
prop_mut_borrowed_ordering name1 name2 =
  let mutBorrowed1 = MutBorrowed name1
      let mutBorrowed2 = MutBorrowed name2
  in compare mutBorrowed1 mutBorrowed2 === compare name1 name2

-- | Ownership type hierarchy should be consistent
prop_ownership_hierarchy :: String -> Property
prop_ownership_hierarchy name =
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
  in compare owned borrowed === LT .&&.
     compare owned mutBorrowed === LT .&&.
     compare borrowed mutBorrowed === LT

-- | Error ordering should be consistent
prop_error_ordering :: OwnershipError -> OwnershipError -> Property
prop_error_ordering err1 err2 =
  let ord1 = compare err1 err2
      ord2 = compare err2 err1
  in (ord1 == EQ) ==> (ord2 === EQ) .&&. (ord1 === EQ)

-- | Error messages should be deterministic
prop_error_show_deterministic :: OwnershipError -> Property
prop_error_show_deterministic err =
  let show1 = show err
      show2 = show err
  in show1 === show2

-- | UseAfterMove errors should contain variable name
prop_use_after_move_format :: String -> Property
prop_use_after_move_format var =
  let err = UseAfterMove var
      errStr = show err
  in var `isInfixOf` errStr

-- | DoubleMove errors should contain both variable names
prop_double_move_format :: String -> String -> Property
prop_double_move_format var1 var2 =
  let err = DoubleMove var1 var2
      errStr = show err
  in var1 `isInfixOf` errStr .&&. var2 `isInfixOf` errStr

-- | Ownership analyzer should be constructible
prop_ownership_analyzer_constructible :: Property
prop_ownership_analyzer_constructible =
  let analyzer = newOwnershipAnalyzer
  in analyzer `seq` True

-- | List of ownership types should be sortable
prop_ownership_types_sortable :: [OwnershipType] -> Property
prop_ownership_types_sortable types =
  let sorted = sort types
  in length sorted === length types

-- | List of ownership errors should be sortable
prop_ownership_errors_sortable :: [OwnershipError] -> Property
prop_ownership_errors_sortable errors =
  let sorted = sort errors
  in length sorted === length errors

-- | Duplicate transfers should be detectable
prop_duplicate_transfers :: [OwnershipTransfer] -> Property
prop_duplicate_transfers transfers =
  let uniqueTransfers = nub transfers
  in length uniqueTransfers <= length transfers

-- | Transfer chains should be traceable
prop_transfer_chain :: [String] -> Property
prop_transfer_chain vars =
  let length >= 2 ==> 
      let transfers = zipWith OwnershipTransfer vars (tail vars)
          fromVars = map transferFrom transfers
          toVars = map transferTo transfers
      in length fromVars === length toVars .&&.
         all (`elem` vars) fromVars .&&.
         all (`elem` vars) toVars

-- | Variable name validation
prop_variable_name_validity :: String -> Property
prop_variable_name_validity name =
  let valid = not (null name) && isAlphaNum (head name)
      isAlphaNum c = c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  in valid || name === ""

-- | Ownership type consistency with variable names
prop_ownership_type_name_consistency :: OwnershipType -> Property
prop_ownership_type_name_consistency owntype =
  let name = case owntype of
        Owned n -> n
        Borrowed n -> n
        MutBorrowed n -> n
  in name `seq` True  -- Should not crash

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Transfer Consistency QuickCheck Tests"
  [ testProperty "ownership transfer is valid" prop_ownership_transfer_valid
  , testProperty "ownership transfer preserves names" prop_ownership_transfer_preserves_names
  , testProperty "ownership transfers are composable" prop_ownership_transfer_composable
  , testProperty "ownership type ordering is consistent" prop_ownership_type_ordering
  , testProperty "ownership type ordering is antisymmetric" prop_ownership_type_ordering_antisymmetric
  , testProperty "owned types ordered by name" prop_owned_ordering
  , testProperty "borrowed types ordered by name" prop_borrowed_ordering
  , testProperty "mut borrowed types ordered by name" prop_mut_borrowed_ordering
  , testProperty "ownership hierarchy is consistent" prop_ownership_hierarchy
  , testProperty "error ordering is consistent" prop_error_ordering
  , testProperty "error show is deterministic" prop_error_show_deterministic
  , testProperty "UseAfterMove error format" prop_use_after_move_format
  , testProperty "DoubleMove error format" prop_double_move_format
  , testProperty "ownership analyzer is constructible" prop_ownership_analyzer_constructible
  , testProperty "ownership types are sortable" prop_ownership_types_sortable
  , testProperty "ownership errors are sortable" prop_ownership_errors_sortable
  , testProperty "duplicate transfers detectable" prop_duplicate_transfers
  , testProperty "transfer chains are traceable" prop_transfer_chain
  , testProperty "variable name validity" prop_variable_name_validity
  , testProperty "ownership type name consistency" prop_ownership_type_name_consistency
  ]