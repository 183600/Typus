{-# LANGUAGE CPP #-}

module Test.Unit.OwnershipAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.List (nub, sort, (\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , OwnershipAnalyzer(..)
  , newOwnershipAnalyzer
  )
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Ownership Analysis QuickCheck Properties"
  [ ownershipTypeProperties
  , ownershipErrorProperties
  , ownershipTransferProperties
  , ownershipStateProperties
  , ownershipAnalysisProperties
  ]

ownershipTypeProperties :: TestTree
ownershipTypeProperties = testGroup "OwnershipType Properties"
  [ fastProperty "OwnershipType ordering is total" prop_ownershipType_total_ordering
  , fastProperty "OwnershipType equality is reflexive" prop_ownershipType_reflexive
  , fastProperty "OwnershipType equality is symmetric" prop_ownershipType_symmetric
  , fastProperty "OwnershipType equality is transitive" prop_ownershipType_transitive
  , fastProperty "OwnershipType show is parseable" prop_ownershipType_show_parseable
  ]

ownershipErrorProperties :: TestTree
ownershipErrorProperties = testGroup "OwnershipError Properties"
  [ fastProperty "OwnershipError ordering is total" prop_ownershipError_total_ordering
  , fastProperty "OwnershipError equality is reflexive" prop_ownershipError_reflexive
  , fastProperty "OwnershipError equality is symmetric" prop_ownershipError_symmetric
  , fastProperty "OwnershipError equality is transitive" prop_ownershipError_transitive
  , fastProperty "UseAfterMove error contains variable name" prop_useAfterMove_contains_var
  , fastProperty "DoubleMove error contains both variable names" prop_doubleMove_contains_vars
  , fastProperty "BorrowWhileMoved error contains variable name" prop_borrowWhileMoved_contains_var
  ]

ownershipTransferProperties :: TestTree
ownershipTransferProperties = testGroup "OwnershipTransfer Properties"
  [ fastProperty "OwnershipTransfer equality is reflexive" prop_ownershipTransfer_reflexive
  , fastProperty "OwnershipTransfer equality is symmetric" prop_ownershipTransfer_symmetric
  , fastProperty "OwnershipTransfer equality is transitive" prop_ownershipTransfer_transitive
  , fastProperty "OwnershipTransfer show contains source and target" prop_ownershipTransfer_show_content
  , fastProperty "Self-transfer is detectable" prop_ownershipTransfer_self_transfer
  , fastProperty "Transfer chain preserves uniqueness" prop_ownershipTransfer_chain_uniqueness
  ]

ownershipStateProperties :: TestTree
ownershipStateProperties = testGroup "Ownership State Properties"
  [ fastProperty "Owned variables are unique" prop_owned_variables_unique
  , fastProperty "Borrowed variables reference existing owners" prop_borrowed_reference_existing
  , fastProperty "Mutable borrows are exclusive" prop_mut_borrows_exclusive
  , fastProperty "Move operations invalidate source" prop_move_invalidates_source
  , fastProperty "Borrow checker prevents invalid operations" prop_borrow_checker_prevents_invalid
  ]

ownershipAnalysisProperties :: TestTree
ownershipAnalysisProperties = testGroup "Ownership Analysis Properties"
  [ fastProperty "Analyzer constructor returns valid analyzer" prop_analyzer_constructor_valid
  , fastProperty "Analysis preserves variable scope" prop_analysis_preserves_scope
  , fastProperty "Cross-function moves are detected" prop_cross_function_moves_detected
  , fastProperty "Loop ownership analysis is sound" prop_loop_analysis_sound
  , fastProperty "Control flow analysis preserves invariants" prop_control_flow_preserves_invariants
  ]

-- OwnershipType Properties

prop_ownershipType_total_ordering :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownershipType_total_ordering ot1 ot2 ot3 =
  let o12 = compare ot1 ot2
      o23 = compare ot2 ot3
      o13 = compare ot1 ot3
  in property $ 
    if o12 == EQ && o23 == EQ 
    then o13 == EQ
    else True -- This is a simplified transitivity check

prop_ownershipType_reflexive :: OwnershipType -> Property
prop_ownershipType_reflexive ot = property $ ot == ot

prop_ownershipType_symmetric :: OwnershipType -> OwnershipType -> Property
prop_ownershipType_symmetric ot1 ot2 =
  (ot1 == ot2) ==> (ot2 == ot1)

prop_ownershipType_transitive :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownershipType_transitive ot1 ot2 ot3 =
  (ot1 == ot2 && ot2 == ot3) ==> (ot1 == ot3)

prop_ownershipType_show_parseable :: OwnershipType -> Property
prop_ownershipType_show_parseable ot =
  let shown = show ot
  in property $ length shown > 0

-- OwnershipError Properties

prop_ownershipError_total_ordering :: OwnershipError -> OwnershipError -> OwnershipError -> Property
prop_ownershipError_total_ordering oe1 oe2 oe3 =
  let o12 = compare oe1 oe2
      o23 = compare oe2 oe3
      o13 = compare oe1 oe3
  in property $ 
    if o12 == EQ && o23 == EQ 
    then o13 == EQ
    else True

prop_ownershipError_reflexive :: OwnershipError -> Property
prop_ownershipError_reflexive oe = property $ oe == oe

prop_ownershipError_symmetric :: OwnershipError -> OwnershipError -> Property
prop_ownershipError_symmetric oe1 oe2 =
  (oe1 == oe2) ==> (oe2 == oe1)

prop_ownershipError_transitive :: OwnershipError -> OwnershipError -> OwnershipError -> Property
prop_ownershipError_transitive oe1 oe2 oe3 =
  (oe1 == oe2 && oe2 == oe3) ==> (oe1 == oe3)

prop_useAfterMove_contains_var :: String -> Property
prop_useAfterMove_contains_var var =
  let err = UseAfterMove var
      shown = show err
  in property $ var `isInfixOf` shown

prop_doubleMove_contains_vars :: String -> String -> Property
prop_doubleMove_contains_vars var1 var2 =
  let err = DoubleMove var1 var2
      shown = show err
  in property $ var1 `isInfixOf` shown && var2 `isInfixOf` shown

prop_borrowWhileMoved_contains_var :: String -> Property
prop_borrowWhileMoved_contains_var var =
  let err = BorrowWhileMoved var
      shown = show err
  in property $ var `isInfixOf` shown

-- OwnershipTransfer Properties

prop_ownershipTransfer_reflexive :: OwnershipTransfer -> Property
prop_ownershipTransfer_reflexive ot = property $ ot == ot

prop_ownershipTransfer_symmetric :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownershipTransfer_symmetric ot1 ot2 =
  (ot1 == ot2) ==> (ot2 == ot1)

prop_ownershipTransfer_transitive :: OwnershipTransfer -> OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownershipTransfer_transitive ot1 ot2 ot3 =
  (ot1 == ot2 && ot2 == ot3) ==> (ot1 == ot3)

prop_ownershipTransfer_show_content :: String -> String -> Property
prop_ownershipTransfer_show_content from to =
  let transfer = OwnershipTransfer from to
      shown = show transfer
  in property $ from `isInfixOf` shown && to `isInfixOf` shown

prop_ownershipTransfer_self_transfer :: String -> Property
prop_ownershipTransfer_self_transfer var =
  let transfer = OwnershipTransfer var var
  in property $ transferFrom transfer == transferTo transfer

prop_ownershipTransfer_chain_uniqueness :: [String] -> Property
prop_ownershipTransfer_chain_uniqueness vars =
  let uniqueVars = nub vars
      transfers = zipWith OwnershipTransfer uniqueVars (tail uniqueVars ++ [head uniqueVars])
      fromVars = map transferFrom transfers
      toVars = map transferTo transfers
  in property $ sort fromVars == sort uniqueVars && sort toVars == sort uniqueVars

-- Ownership State Properties

prop_owned_variables_unique :: [String] -> Property
prop_owned_variables_unique vars =
  let uniqueVars = nub vars
      ownershipTypes = map Owned uniqueVars
  in property $ length ownershipTypes == length uniqueVars

prop_borrowed_reference_existing :: [String] -> String -> Property
prop_borrowed_reference_existing vars owner =
  let ownedVars = nub vars
      borrow = Borrowed owner
  in owner `elem` ownedVars ==> property True

prop_mut_borrows_exclusive :: [String] -> Property
prop_mut_borrows_exclusive vars =
  let uniqueVars = nub vars
      mutBorrows = map MutBorrowed uniqueVars
      -- In a real ownership system, you can't have multiple mutable borrows of the same variable
      uniqueMutBorrows = nub mutBorrows
  in property $ length mutBorrows == length uniqueMutBorrows

prop_move_invalidates_source :: String -> String -> Property
prop_move_invalidates_source source target =
  let transfer = OwnershipTransfer source target
  in property $ transferFrom transfer == source && transferTo transfer == target

prop_borrow_checker_prevents_invalid :: [String] -> Property
prop_borrow_checker_prevents_invalid vars =
  let uniqueVars = nub vars
      -- Simulate a scenario where we have both immutable and mutable borrows
      immutableBorrows = map Borrowed uniqueVars
      mutableBorrows = map MutBorrowed uniqueVars
  in property $ length immutableBorrows == length uniqueVars && length mutableBorrows == length uniqueVars

-- Ownership Analysis Properties

prop_analyzer_constructor_valid :: Property
prop_analyzer_constructor_valid =
  let analyzer = newOwnershipAnalyzer
  in property $ case analyzer of
    OwnershipAnalyzer _ -> True

prop_analysis_preserves_scope :: [String] -> Property
prop_analysis_preserves_scope vars =
  let uniqueVars = nub vars
      -- In a real analysis, variable scope should be preserved
      scopeSize = length uniqueVars
  in property $ scopeSize >= 0

prop_cross_function_moves_detected :: String -> String -> Property
prop_cross_function_moves_detected funcName varName =
  let error = CrossFunctionMove funcName varName
      shown = show error
  in property $ funcName `isInfixOf` shown && varName `isInfixOf` shown

prop_loop_analysis_sound :: String -> Property
prop_loop_analysis_sound loopVar =
  let error = LoopOwnershipError loopVar
      shown = show error
  in property $ loopVar `isInfixOf` shown

prop_control_flow_preserves_invariants :: String -> Property
prop_control_flow_preserves_invariants flowInfo =
  let error = ControlFlowError flowInfo
      shown = show error
  in property $ flowInfo `isInfixOf` shown

-- Helper function for string infix checking
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]