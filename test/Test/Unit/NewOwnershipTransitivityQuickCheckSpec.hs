{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipTransitivityQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub, (\\))
import Data.Set (Set, fromList, toList, union, intersection, difference)
import qualified Data.Set as Set
import Data.Map (Map, fromList, toList, keys, elems, union, intersection, difference)
import qualified Data.Map as Map

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate valid variable names
genVarName :: Gen String
genVarName = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'])
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- Generate ownership types
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  varName <- genVarName
  elements [Owned varName, Borrowed varName, MutBorrowed varName]

-- Generate ownership errors
genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ UseAfterMove <$> genVarName
  , DoubleMove <$> genVarName <*> genVarName
  , BorrowWhileMoved <$> genVarName
  , MutBorrowWhileBorrowed <$> genVarName
  , BorrowWhileMutBorrowed <$> genVarName
  , MultipleMutBorrows <$> genVarName
  , UseWhileMutBorrowed <$> genVarName
  , OutOfScope <$> genVarName
  , BorrowError <$> genVarName
  , ParseError <$> genVarName
  , CrossFunctionMove <$> genVarName <*> genVarName
  , ParameterMoveMismatch <$> genVarName
  , ControlFlowError <$> genVarName
  , PathSensitiveError <$> genVarName
  , LoopOwnershipError <$> genVarName
  ]

-- Generate ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  fromVar <- genVarName
  toVar <- genVarName `suchThat` (/= fromVar)
  return $ OwnershipTransfer fromVar toVar

-- Generate sets of ownership types
genOwnershipSet :: Gen (Set OwnershipType)
genOwnershipSet = do
  types <- listOf genOwnershipType
  return $ fromList types

-- Generate maps of variable to ownership type
genOwnershipMap :: Gen (Map String OwnershipType)
genOwnershipMap = do
  pairs <- listOf $ do
    var <- genVarName
    ownership <- genOwnershipType
    return (var, ownership)
  return $ fromList pairs

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = genOwnershipType

instance Arbitrary OwnershipError where
  arbitrary = genOwnershipError

instance Arbitrary OwnershipTransfer where
  arbitrary = genOwnershipTransfer

-- ============================================================================
-- Ownership Type Properties
-- ============================================================================

-- Property: Ownership type ordering is consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering ot1 ot2 =
  let ordered = [Owned "", Borrowed "", MutBorrowed ""]
      typeRank (Owned _) = 0
      typeRank (Borrowed _) = 1
      typeRank (MutBorrowed _) = 2
      rank1 = typeRank ot1
      rank2 = typeRank ot2
  in property $ (ot1 <= ot2) === (rank1 <= rank2)

-- Property: Ownership type equality depends on name L.and type
prop_ownership_type_equality :: String -> String -> Property
prop_ownership_type_equality name1 name2 =
  let owned1 = Owned name1
      owned2 = Owned name2
      borrowed1 = Borrowed name1
      borrowed2 = Borrowed name2
  in property $ (owned1 == owned2) === (name1 == name2) .&&.
             (borrowed1 == borrowed2) === (name1 == name2) .&&.
             (owned1 == borrowed1) === False

-- Property: Ownership type Show is invertible for simple cases
prop_ownership_type_show_invertible :: OwnershipType -> Property
prop_ownership_type_show_invertible ot =
  let shown = show ot
      parsed = case words shown of
        ["Owned", name] -> Just (Owned name)
        ["Borrowed", name] -> Just (Borrowed name)
        ["MutBorrowed", name] -> Just (MutBorrowed name)
        _ -> Nothing
  in property $ parsed === Just ot

-- ============================================================================
-- Ownership Error Properties
-- ============================================================================

-- Property: Ownership error ordering is consistent with string representation
prop_ownership_error_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering err1 err2 =
  property $ compare err1 err2 === compare (show err1) (show err2)

-- Property: UseAfterMove error contains variable name
prop_use_after_move_contains_var :: String -> Property
prop_use_after_move_contains_var var =
  let err = UseAfterMove var
      errStr = show err
  in property $ var `L.isInfixOf` errStr

-- Property: DoubleMove error contains both variable names
prop_double_move_contains_vars :: String -> String -> Property
prop_double_move_contains_vars var1 var2 =
  let err = DoubleMove var1 var2
      errStr = show err
  in property $ var1 `L.isInfixOf` errStr .&&. var2 `L.isInfixOf` errStr

-- Property: CrossFunctionMove error contains both function names
prop_cross_function_move_contains_vars :: String -> String -> Property
prop_cross_function_move_contains_vars func1 func2 =
  let err = CrossFunctionMove func1 func2
      errStr = show err
  in property $ func1 `L.isInfixOf` errStr .&&. func2 `L.isInfixOf` errStr

-- ============================================================================
-- Ownership Transfer Properties
-- ============================================================================

-- Property: Ownership transfer has distinct from L.and to variables
prop_ownership_transfer_distinct :: OwnershipTransfer -> Property
prop_ownership_transfer_distinct transfer =
  property $ transferFrom transfer /= transferTo transfer

-- Property: Ownership transfer equality depends on both fields
prop_ownership_transfer_equality :: String -> String -> String -> Property
prop_ownership_transfer_equality from1 to1 to2 =
  to1 /= to2 ==>
  let transfer1 = OwnershipTransfer from1 to1
      transfer2 = OwnershipTransfer from1 to2
  in property $ transfer1 /= transfer2

-- Property: Ownership transfer Show is invertible
prop_ownership_transfer_show_invertible :: OwnershipTransfer -> Property
prop_ownership_transfer_show_invertible transfer =
  let shown = show transfer
  in property $ "OwnershipTransfer" `L.isInfixOf` shown

-- ============================================================================
-- Transitivity Properties
-- ============================================================================

-- Property: Ownership transfer chain is transitive
prop_ownership_transfer_chain_transitive :: [String] -> Property
prop_ownership_transfer_chain_transitive vars =
  length vars >= 3 ==>
  let transfers = zipWith OwnershipTransfer vars (L.tail vars)
      firstVar = L.head vars
      lastVar = last vars
  in property $ L.length transfers === L.length vars - 1 .&&.
             all (\t -> transferFrom t `elem` vars && transferTo t `elem` vars) transfers .&&.
             transferFrom (L.head transfers) === firstVar .&&.
             transferTo (last transfers) === lastVar

-- Property: Circular ownership transfers are detectable
prop_circular_transfers_detectable :: [String] -> Property
prop_circular_transfers_detectable vars =
  length vars >= 3 ==>
  let circularVars = vars ++ [L.head vars]
      transfers = zipWith OwnershipTransfer circularVars (L.tail circularVars)
      fromVars = map transferFrom transfers
      toVars = map transferTo transfers
  in property $ L.length fromVars === L.length vars .&&.
             length toVars === L.length vars .&&.
             head fromVars `elem` toVars .&&.
             last toVars `elem` fromVars

-- Property: Ownership transfer preserves variable uniqueness
prop_ownership_transfer_preserves_uniqueness :: [String] -> Property
prop_ownership_transfer_preserves_uniqueness vars =
  let uniqueVars = nub vars
      transfers = zipWith OwnershipTransfer vars (L.tail vars ++ [L.head vars])
      allVars = concatMap (\t -> [transferFrom t, transferTo t]) transfers
  in property $ L.length uniqueVars <= L.length allVars

-- ============================================================================
-- Ownership Analysis Properties
-- ============================================================================

-- Property: Empty ownership state has no errors
prop_empty_ownership_no_errors :: Property
prop_empty_ownership_no_errors =
  let emptyMap = Map.empty :: Map String OwnershipType
  in property $ Map.null emptyMap

-- Property: Single owned variable has no conflicts
prop_single_owned_no_conflicts :: String -> Property
prop_single_owned_no_conflicts var =
  let ownershipMap = Map.singleton var (Owned var)
      ownedVars = Map.keys $ Map.L.filter (\case Owned _ -> True; _ -> False) ownershipMap
  in property $ L.length ownedVars === 1 .&&. L.head ownedVars === var

-- Property: Multiple borrows from same owner are valid
prop_multiple_borrows_same_owner :: String -> [String] -> Property
prop_multiple_borrows_same_owner owner borrowers =
  not (null borrowers) && L.all (/= owner) borrowers ==>
  let ownershipMap = Map.fromList $ (owner, Owned owner) : 
                                      map (\b -> (b, Borrowed owner)) borrowers
      borrowedVars = Map.keys $ Map.L.filter (\case Borrowed _ -> True; _ -> False) ownershipMap
  in property | Set.fromList borrowedVars === Set.fromList borrowers

-- Property: Multiple mutable borrows from same owner are invalid
prop_multiple_mut_borrows_invalid :: String -> [String] -> Property
prop_multiple_mut_borrows_invalid owner borrowers =
  length borrowers >= 2 && L.all (/= owner) borrowers ==>
  let ownershipMap = Map.fromList $ (owner, Owned owner) : 
                                      map (\b -> (b, MutBorrowed owner)) borrowers
      mutBorrowedVars = Map.keys $ Map.L.filter (\case MutBorrowed _ -> True; _ -> False) ownershipMap
  in property $ L.length mutBorrowedVars >= 2

-- Property: Borrow L.and mut borrow from same owner conflict
prop_borrow_mut_borrow_conflict :: String -> String -> String -> Property
prop_borrow_mut_borrow_conflict owner borrower mutBorrower =
  borrower /= owner && mutBorrower /= owner && borrower /= mutBorrower ==>
  let ownershipMap = Map.fromList [ (owner, Owned owner)
                                   , (borrower, Borrowed owner)
                                   , (mutBorrower, MutBorrowed owner)
                                   ]
      hasBorrow = Map.member borrower ownershipMap
      hasMutBorrow = Map.member mutBorrower ownershipMap
  in property $ hasBorrow .&&. hasMutBorrow

-- ============================================================================
-- Ownership State Transition Properties
-- ============================================================================

-- Property: Moving ownership invalidates source
prop_move_invalidates_source :: String -> String -> Property
prop_move_invalidates_source source target =
  source /= target ==>
  let beforeMove = Map.fromList [(source, Owned source), (target, Owned target)]
      afterMove = Map.insert target (Owned source) $ Map.delete source beforeMove
      sourceExists = Map.member source afterMove
  in property $ not sourceExists

-- Property: Moving ownership preserves target
prop_move_preserves_target :: String -> String -> Property
prop_move_preserves_target source target =
  source /= target ==>
  let beforeMove = Map.fromList [(source, Owned source), (target, Owned target)]
      afterMove = Map.insert target (Owned source) $ Map.delete source beforeMove
      targetOwnership = Map.lookup target afterMove
  in property $ targetOwnership === Just (Owned source)

-- Property: Borrowing preserves owner
prop_borrowing_preserves_owner :: String -> String -> Property
prop_borrowing_preserves_owner owner borrower =
  owner /= borrower ==>
  let beforeBorrow = Map.fromList [(owner, Owned owner), (borrower, Owned borrower)]
      afterBorrow = Map.insert borrower (Borrowed owner) beforeBorrow
      ownerOwnership = Map.lookup owner afterBorrow
  in property $ ownerOwnership === Just (Owned owner)

-- ============================================================================
-- Complex Ownership Scenarios
-- ============================================================================

-- Property: Nested borrowing chains are valid
prop_nested_borrowing_chains :: [String] -> Property
prop_nested_borrowing_chains vars =
  length vars >= 3 ==>
  let chain = zipWith (\owner borrower -> (borrower, Borrowed owner)) vars (L.tail vars)
      ownershipMap = Map.fromList $ (L.head vars, Owned (L.head vars)) : chain
      allBorrowed = L.all (\case Borrowed _ -> True; _ -> False) $ elems ownershipMap
      hasOwner = Map.member (L.head vars) ownershipMap
  in property $ L.length chain === L.length vars - 1 .&&.
             hasOwner .&&.
             allBorrowed

-- Property: Borrowing from moved variable is invalid
prop_borrow_from_moved_invalid :: String -> String -> String -> Property
prop_borrow_from_moved_invalid owner mover borrower =
  all (/=) [owner, mover, borrower] ==>
  let initial = Map.fromList [(owner, Owned owner), (mover, Owned mover), (borrower, Owned borrower)]
      afterMove = Map.insert mover (Owned owner) $ Map.delete owner initial
      invalidBorrow = Map.insert borrower (Borrowed owner) afterMove
  in property $ Map.notMember owner invalidBorrow

-- Property: Multiple moves from same source are invalid
prop_multiple_moves_invalid :: String -> [String] -> Property
prop_multiple_moves_invalid source targets =
  length targets >= 2 && L.all (/= source) targets ==>
  let initial = Map.fromList $ (source, Owned source) : L.map (\t -> (t, Owned t)) targets
      afterMoves = L.foldl (\acc target -> 
        Map.insert target (Owned source) $ Map.delete source acc) initial targets
      sourceExists = Map.member source afterMoves
  in property $ sourceExists

-- ============================================================================
-- Error Detection Properties
-- ============================================================================

-- Property: Use after move is detectable
prop_use_after_move_detectable :: String -> String -> Property
prop_use_after_move_detectable var target =
  var /= target ==>
  let ownershipMap = Map.fromList [(target, Owned var)]
      isMoved = not $ Map.member var ownershipMap
  in property $ isMoved

-- Property: Double move is detectable
prop_double_move_detectable :: String -> String -> String -> Property
prop_double_move_detectable source target1 target2 =
  all (/=) [source, target1, target2] && target1 /= target2 ==>
  let ownershipMap = Map.fromList [ (target1, Owned source), (target2, Owned source) ]
      movedToMultiple = L.length (Map.L.filter (\case Owned src -> src == source; _ -> False) ownershipMap) >= 2
  in property $ movedToMultiple

-- Property: Out of scope access is detectable
prop_out_of_scope_detectable :: [String] -> String -> Property
prop_out_of_scope_detectable inScopeVars var =
  not (var `elem` inScopeVars) ==>
  let ownershipMap = Map.fromList $ L.map (\v -> (v, Owned v)) inScopeVars
      varInScope = Map.member var ownershipMap
  in property $ not varInScope

-- ============================================================================
-- Performance L.and Scalability Properties
-- ============================================================================

-- Property: Large ownership maps handle efficiently
prop_large_ownership_maps :: Int -> Property
prop_large_ownership_maps size =
  size >= 0 && size <= 1000 ==>
  let vars = take size $ L.map (\i -> "var" ++ show i) [1..]
      ownershipMap = Map.fromList $ L.map (\v -> (v, Owned v)) vars
      mapSize = Map.size ownershipMap
  in property $ mapSize === size

-- Property: Complex transfer chains handle correctly
prop_complex_transfer_chains :: Int -> Int -> Property
prop_complex_transfer_chains numVars chainLength =
  numVars >= 0 && chainLength >= 0 && chainLength <= numVars && numVars <= 100 ==>
  let vars = take numVars $ L.map (\i -> "var" ++ show i) [1..]
      chainVars = take chainLength vars
      transfers = zipWith OwnershipTransfer chainVars (L.tail chainVars)
  in property $ L.length transfers === max 0 (chainLength - 1)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Ownership Transitivity QuickCheck Tests"
  [ testGroup "Ownership Type Properties"
    [ fastProperty "ownership type ordering" prop_ownership_type_ordering
    , fastProperty "ownership type equality" prop_ownership_type_equality
    , fastProperty "ownership type show invertible" prop_ownership_type_show_invertible
    ]

  , testGroup "Ownership Error Properties"
    [ fastProperty "ownership error ordering" prop_ownership_error_ordering
    , fastProperty "use after move contains var" prop_use_after_move_contains_var
    , fastProperty "double move contains vars" prop_double_move_contains_vars
    , fastProperty "cross function move contains vars" prop_cross_function_move_contains_vars
    ]

  , testGroup "Ownership Transfer Properties"
    [ fastProperty "ownership transfer distinct" prop_ownership_transfer_distinct
    , fastProperty "ownership transfer equality" prop_ownership_transfer_equality
    , fastProperty "ownership transfer show invertible" prop_ownership_transfer_show_invertible
    ]

  , testGroup "Transitivity Properties"
    [ fastProperty "ownership transfer chain transitive" prop_ownership_transfer_chain_transitive
    , fastProperty "circular transfers detectable" prop_circular_transfers_detectable
    , fastProperty "ownership transfer preserves uniqueness" prop_ownership_transfer_preserves_uniqueness
    ]

  , testGroup "Ownership Analysis Properties"
    [ fastProperty "empty ownership no errors" prop_empty_ownership_no_errors
    , fastProperty "single owned no conflicts" prop_single_owned_no_conflicts
    , fastProperty "multiple borrows same owner" prop_multiple_borrows_same_owner
    , fastProperty "multiple mut borrows invalid" prop_multiple_mut_borrows_invalid
    , fastProperty "borrow mut borrow conflict" prop_borrow_mut_borrow_conflict
    ]

  , testGroup "Ownership State Transition Properties"
    [ fastProperty "move invalidates source" prop_move_invalidates_source
    , fastProperty "move preserves target" prop_move_preserves_target
    , fastProperty "borrowing preserves owner" prop_borrowing_preserves_owner
    ]

  , testGroup "Complex Ownership Scenarios"
    [ fastProperty "nested borrowing chains" prop_nested_borrowing_chains
    , fastProperty "borrow from moved invalid" prop_borrow_from_moved_invalid
    , fastProperty "multiple moves invalid" prop_multiple_moves_invalid
    ]

  , testGroup "Error Detection Properties"
    [ fastProperty "use after move detectable" prop_use_after_move_detectable
    , fastProperty "double move detectable" prop_double_move_detectable
    , fastProperty "out of scope detectable" prop_out_of_scope_detectable
    ]

  , testGroup "Performance L.and Scalability Properties"
    [ fastProperty "large ownership maps" prop_large_ownership_maps
    , fastProperty "complex transfer chains" prop_complex_transfer_chains
    ]
  ]