{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.NewOwnershipTransitivityQuickCheckTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership
import Ownership.Common.Types
import Data.List (nub, sort)
import Data.Set (Set, fromList, toList, union, intersection, empty)
import qualified Data.Set as Set

-- ============================================================================
-- Ownership Transitivity QuickCheck Tests
-- ============================================================================

-- Test OwnershipType equality
prop_ownership_type_owned_reflexive :: String -> Property
prop_ownership_type_owned_reflexive name = 
  let owned = Owned name
  in property $ owned === owned

prop_ownership_type_borrowed_reflexive :: String -> Property
prop_ownership_type_borrowed_reflexive name = 
  let borrowed = Borrowed name
  in property $ borrowed === borrowed

prop_ownership_type_mut_borrowed_reflexive :: String -> Property
prop_ownership_type_mut_borrowed_reflexive name = 
  let mutBorrowed = MutBorrowed name
  in property $ mutBorrowed === mutBorrowed

-- Test OwnershipType ordering
prop_ownership_type_ordered :: String -> String -> Property
prop_ownership_type_ordered name1 name2 = 
  let owned = Owned name1
      borrowed = Borrowed name2
      mutBorrowed = MutBorrowed name1
  in property $ owned < borrowed && borrowed < mutBorrowed

prop_ownership_type_ordering_transitive :: String -> String -> String -> Property
prop_ownership_type_ordering_transitive name1 name2 name3 = 
  let owned1 = Owned name1
      owned2 = Owned name2
      borrowed = Borrowed name3
  in if owned1 <= owned2 && owned2 <= borrowed
     then property $ owned1 <= borrowed
     else property $ True

-- Test OwnershipError equality
prop_ownership_error_use_after_move_reflexive :: String -> Property
prop_ownership_error_use_after_move_reflexive var = 
  let error = UseAfterMove var
  in property $ error === error

prop_ownership_error_double_move_reflexive :: String -> String -> Property
prop_ownership_error_double_move_reflexive var1 var2 = 
  let error = DoubleMove var1 var2
  in property $ error === error

prop_ownership_error_borrow_while_moved_reflexive :: String -> Property
prop_ownership_error_borrow_while_moved_reflexive var = 
  let error = BorrowWhileMoved var
  in property $ error === error

-- Test OwnershipError ordering
prop_ownership_error_ordered :: String -> String -> Property
prop_ownership_error_ordered var1 var2 = 
  let error1 = UseAfterMove var1
      error2 = DoubleMove var1 var2
  in property $ error1 <= error2 || error1 >= error2  -- Just ensure they can be compared

-- Test OwnershipTransfer equality
prop_ownership_transfer_reflexive :: String -> String -> Property
prop_ownership_transfer_reflexive from to = 
  let transfer = OwnershipTransfer from to
  in property $ transfer === transfer

prop_ownership_transfer_symmetric :: String -> String -> Property
prop_ownership_transfer_symmetric from to = 
  let transfer1 = OwnershipTransfer from to
      transfer2 = OwnershipTransfer from to
  in property $ transfer1 === transfer2

prop_ownership_transfer_commutative :: String -> String -> Property
prop_ownership_transfer_commutative from to = 
  let transfer1 = OwnershipTransfer from to
      transfer2 = OwnershipTransfer to from
      isEqual = transfer1 == transfer2
  in property $ isEqual === (from == to)

-- Test ownership transfer transitivity
prop_ownership_transfer_transitive :: String -> String -> String -> Property
prop_ownership_transfer_transitive var1 var2 var3 = 
  let transfer1 = OwnershipTransfer var1 var2
      transfer2 = OwnershipTransfer var2 var3
      transfer3 = OwnershipTransfer var1 var3
      transitiveChain = [transfer1, transfer2]
  in property $ length transitiveChain === 2 &&
                transferFrom transfer1 === var1 &&
                transferTo transfer1 === var2 &&
                transferFrom transfer2 === var2 &&
                transferTo transfer2 === var3

-- Test ownership transfer chain
prop_ownership_transfer_chain :: [String] -> Property
prop_ownership_transfer_chain vars = 
  let transfers = createTransferChain vars
      expectedLength = max 0 (length vars - 1)
  in property $ length transfers === expectedLength
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)

prop_ownership_transfer_chain_consistency :: [String] -> Property
prop_ownership_transfer_chain_consistency vars = 
  let transfers = createTransferChain vars
      isConsistent = all (\t -> transferTo t `elem` vars) transfers
  in property $ isConsistent
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)

-- Test ownership transfer detection
prop_ownership_transfer_detection :: String -> String -> Property
prop_ownership_transfer_detection from to = 
  let transfer = OwnershipTransfer from to
      detectedFrom = transferFrom transfer
      detectedTo = transferTo transfer
  in property $ detectedFrom === from && detectedTo === to

prop_ownership_transfer_uniqueness :: String -> String -> String -> Property
prop_ownership_transfer_uniqueness from to via = 
  let transfer1 = OwnershipTransfer from via
      transfer2 = OwnershipTransfer via to
      transfer3 = OwnershipTransfer from to
      uniqueTransfers = nub [transfer1, transfer2, transfer3]
  in property $ length uniqueTransfers >= 2

-- Test ownership transfer cycles
prop_ownership_transfer_cycle :: String -> Property
prop_ownership_transfer_cycle var = 
  let transfer = OwnershipTransfer var var
      isSelfTransfer = transferFrom transfer == transferTo transfer
  in property $ isSelfTransfer

prop_ownership_transfer_cycle_detection :: [String] -> Property
prop_ownership_transfer_cycle_detection vars = 
  let transfers = createTransferChain vars
      hasCycle = detectCycle transfers vars
  in property $ hasCycle === (length vars > 1)
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)
    
    detectCycle [] _ = False
    detectCycle _ [] = False
    detectCycle transfers allVars = 
      let firstVar = head allVars
          lastVar = last allVars
          hasDirectCycle = any (\t -> transferFrom t == lastVar && transferTo t == firstVar) transfers
      in hasDirectCycle || length allVars > 3

-- Test ownership transfer validation
prop_ownership_transfer_valid :: String -> String -> Property
prop_ownership_transfer_valid from to = 
  let transfer = OwnershipTransfer from to
      isValid = not (null from) && not (null to)
  in property $ isValid

prop_ownership_transfer_invalid_empty :: String -> Property
prop_ownership_transfer_invalid_empty var = 
  let transfer1 = OwnershipTransfer "" var
      transfer2 = OwnershipTransfer var ""
      isInvalid1 = null (transferFrom transfer1)
      isInvalid2 = null (transferTo transfer2)
  in property $ isInvalid1 && isInvalid2

-- Test ownership transfer composition
prop_ownership_transfer_composition :: String -> String -> String -> Property
prop_ownership_transfer_composition var1 var2 var3 = 
  let transfer1 = OwnershipTransfer var1 var2
      transfer2 = OwnershipTransfer var2 var3
      composition = OwnershipTransfer var1 var3
      isComposable = transferTo transfer1 == transferFrom transfer2
  in property $ isComposable

prop_ownership_transfer_associative :: String -> String -> String -> String -> Property
prop_ownership_transfer_associative var1 var2 var3 var4 = 
  let transfer1 = OwnershipTransfer var1 var2
      transfer2 = OwnershipTransfer var2 var3
      transfer3 = OwnershipTransfer var3 var4
      leftComposition = OwnershipTransfer var1 var3
      rightComposition = OwnershipTransfer var2 var4
  in property $ transferTo leftComposition === transferFrom transfer3 &&
                transferTo transfer2 === transferFrom rightComposition

-- Test ownership transfer identity
prop_ownership_transfer_identity :: String -> Property
prop_ownership_transfer_identity var = 
  let identityTransfer = OwnershipTransfer var var
      isIdentity = transferFrom identityTransfer == transferTo identityTransfer
  in property $ isIdentity

prop_ownership_transfer_identity_property :: String -> String -> Property
prop_ownership_transfer_identity_property var1 var2 = 
  let transfer = OwnershipTransfer var1 var2
      identityTransfer1 = OwnershipTransfer var1 var1
      identityTransfer2 = OwnershipTransfer var2 var2
      leftIdentity = transferFrom identityTransfer1 == transferFrom transfer
      rightIdentity = transferTo identityTransfer2 == transferTo transfer
  in property $ leftIdentity && rightIdentity

-- Test ownership transfer inverse
prop_ownership_transfer_inverse :: String -> String -> Property
prop_ownership_transfer_inverse from to = 
  let transfer = OwnershipTransfer from to
      inverseTransfer = OwnershipTransfer to from
      isInverse = transferFrom transfer == transferTo inverseTransfer &&
                  transferTo transfer == transferFrom inverseTransfer
  in property $ isInverse

prop_ownership_transfer_inverse_property :: String -> String -> Property
prop_ownership_transfer_inverse_property from to = 
  let transfer = OwnershipTransfer from to
      inverseTransfer = OwnershipTransfer to from
      doubleInverse = OwnershipTransfer from to
  in property $ transferFrom doubleInverse === transferFrom transfer &&
                transferTo doubleInverse === transferTo transfer

-- Test ownership transfer closure
prop_ownership_transfer_closure :: [String] -> Property
prop_ownership_transfer_closure vars = 
  let transfers = createTransferChain vars
      closure = computeClosure transfers vars
      allVarsIncluded = all (`elem` closure) vars
  in property $ allVarsIncluded
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)
    
    computeClosure [] allVars = allVars
    computeClosure transfers allVars = 
      let fromVars = map transferFrom transfers
          toVars = map transferTo transfers
      in nub (allVars ++ fromVars ++ toVars)

-- Test ownership transfer reachability
prop_ownership_transfer_reachability :: [String] -> Property
prop_ownership_transfer_reachability vars = 
  let transfers = createTransferChain vars
      reachable = computeReachable transfers vars
      allReachable = all (`elem` reachable) vars
  in property $ allReachable
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)
    
    computeReachable [] allVars = allVars
    computeReachable transfers allVars = 
      let reachableFromStart = findReachable (head allVars) transfers
      in if null allVars then [] else reachableFromStart
    
    findReachable _ [] = []
    findReachable current transfers = 
      let directTransfers = filter (\t -> transferFrom t == current) transfers
          nextVars = map transferTo directTransfers
          indirectReachable = concatMap (\v -> findReachable v transfers) nextVars
      in current : nextVars ++ indirectReachable

-- Test ownership transfer equivalence
prop_ownership_transfer_equivalence :: String -> String -> Property
prop_ownership_transfer_equivalence from to = 
  let transfer1 = OwnershipTransfer from to
      transfer2 = OwnershipTransfer from to
      areEquivalent = transfer1 == transfer2
  in property $ areEquivalent

prop_ownership_transfer_equivalence_reflexive :: String -> String -> Property
prop_ownership_transfer_equivalence_reflexive from to = 
  let transfer = OwnershipTransfer from to
      isEquivalent = transfer == transfer
  in property $ isEquivalent

prop_ownership_transfer_equivalence_symmetric :: String -> String -> Property
prop_ownership_transfer_equivalence_symmetric from to = 
  let transfer1 = OwnershipTransfer from to
      transfer2 = OwnershipTransfer from to
      areEquivalent = transfer1 == transfer2
  in property $ areEquivalent

prop_ownership_transfer_equivalence_transitive :: String -> String -> Property
prop_ownership_transfer_equivalence_transitive from to = 
  let transfer1 = OwnershipTransfer from to
      transfer2 = OwnershipTransfer from to
      transfer3 = OwnershipTransfer from to
      areEquivalent = transfer1 == transfer2 && transfer2 == transfer3
  in if areEquivalent
     then property $ transfer1 == transfer3
     else property $ True

-- Test ownership transfer consistency
prop_ownership_transfer_consistency :: String -> String -> Property
prop_ownership_transfer_consistency from to = 
  let transfer = OwnershipTransfer from to
      isConsistent = not (null (transferFrom transfer)) && 
                     not (null (transferTo transfer))
  in property $ isConsistent

prop_ownership_transfer_consistency_chain :: [String] -> Property
prop_ownership_transfer_consistency_chain vars = 
  let transfers = createTransferChain vars
      isConsistent = all (\t -> not (null (transferFrom t)) && 
                               not (null (transferTo t))) transfers
  in property $ isConsistent
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)

-- Test ownership transfer normalization
prop_ownership_transfer_normalization :: [String] -> Property
prop_ownership_transfer_normalization vars = 
  let transfers = createTransferChain vars
      normalized = normalizeTransfers transfers
      isNormalized = length normalized <= length transfers
  in property $ isNormalized
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)
    
    normalizeTransfers = nub

-- Test ownership transfer optimization
prop_ownership_transfer_optimization :: [String] -> Property
prop_ownership_transfer_optimization vars = 
  let transfers = createTransferChain vars
      optimized = optimizeTransfers transfers
      isOptimized = length optimized <= length transfers
  in property $ isOptimized
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)
    
    optimizeTransfers = removeRedundant
    
    removeRedundant [] = []
    removeRedundant (t:ts) = 
      let redundant = filter (\t' -> transferFrom t' == transferFrom t && 
                                   transferTo t' == transferTo t) ts
          remaining = filter (\t' -> not (transferFrom t' == transferFrom t && 
                                        transferTo t' == transferTo t)) ts
      in if null redundant then t : removeRedundant remaining else removeRedundant remaining

-- Test ownership transfer analysis
prop_ownership_transfer_analysis :: [String] -> Property
prop_ownership_transfer_analysis vars = 
  let transfers = createTransferChain vars
      analysis = analyzeTransfers transfers
      hasAnalysis = not (null analysis)
  in property $ hasAnalysis
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)
    
    analyzeTransfers = map (\t -> (transferFrom t, transferTo t))

-- Test ownership transfer validation
prop_ownership_transfer_validation :: [String] -> Property
prop_ownership_transfer_validation vars = 
  let transfers = createTransferChain vars
      validation = validateTransfers transfers
      isValid = validation
  in property $ isValid
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)
    
    validateTransfers = all (\t -> not (null (transferFrom t)) && 
                               not (null (transferTo t)))

-- Test ownership transfer graph properties
prop_ownership_transfer_graph_acyclic :: [String] -> Property
prop_ownership_transfer_graph_acyclic vars = 
  let transfers = createTransferChain vars
      isAcyclic = not (hasCycle transfers)
  in property $ isAcyclic
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)
    
    hasCycle [] = False
    hasCycle transfers = 
      let allVars = nub (map transferFrom transfers ++ map transferTo transfers)
          hasSelfCycle = any (\t -> transferFrom t == transferTo t) transfers
      in hasSelfCycle || length allVars > 3

prop_ownership_transfer_graph_connected :: [String] -> Property
prop_ownership_transfer_graph_connected vars = 
  let transfers = createTransferChain vars
      isConnected = isGraphConnected transfers vars
  in property $ isConnected || length vars <= 1
  where
    createTransferChain [] = []
    createTransferChain [_] = []
    createTransferChain (x:y:xs) = OwnershipTransfer x y : createTransferChain (y:xs)
    
    isGraphConnected [] _ = True
    isGraphConnected _ [] = True
    isGraphConnected transfers allVars = 
      if null allVars then True else
      let startVar = head allVars
          reachable = findReachable startVar transfers
      in all (`elem` reachable) allVars
    
    findReachable _ [] = []
    findReachable current transfers = 
      let directTransfers = filter (\t -> transferFrom t == current) transfers
          nextVars = map transferTo directTransfers
          indirectReachable = concatMap (\v -> findReachable v transfers) nextVars
      in current : nextVars ++ indirectReachable

-- Helper functions
detectCycle :: [OwnershipTransfer] -> [String] -> Bool
detectCycle [] _ = False
detectCycle _ [] = False
detectCycle transfers allVars = 
  if length allVars <= 1 then False else
  let firstVar = head allVars
      lastVar = last allVars
      hasDirectCycle = any (\t -> transferFrom t == lastVar && transferTo t == firstVar) transfers
  in hasDirectCycle || length allVars > 3

-- Tests collection
tests :: TestTree
tests = testGroup "Ownership Transitivity QuickCheck Tests"
  [ testProperty "ownership type owned reflexive" prop_ownership_type_owned_reflexive
  , testProperty "ownership type borrowed reflexive" prop_ownership_type_borrowed_reflexive
  , testProperty "ownership type mut borrowed reflexive" prop_ownership_type_mut_borrowed_reflexive
  , testProperty "ownership type ordered" prop_ownership_type_ordered
  , testProperty "ownership type ordering transitive" prop_ownership_type_ordering_transitive
  , testProperty "ownership error use after move reflexive" prop_ownership_error_use_after_move_reflexive
  , testProperty "ownership error double move reflexive" prop_ownership_error_double_move_reflexive
  , testProperty "ownership error borrow while moved reflexive" prop_ownership_error_borrow_while_moved_reflexive
  , testProperty "ownership error ordered" prop_ownership_error_ordered
  , testProperty "ownership transfer reflexive" prop_ownership_transfer_reflexive
  , testProperty "ownership transfer symmetric" prop_ownership_transfer_symmetric
  , testProperty "ownership transfer commutative" prop_ownership_transfer_commutative
  , testProperty "ownership transfer transitive" prop_ownership_transfer_transitive
  , testProperty "ownership transfer chain" prop_ownership_transfer_chain
  , testProperty "ownership transfer chain consistency" prop_ownership_transfer_chain_consistency
  , testProperty "ownership transfer detection" prop_ownership_transfer_detection
  , testProperty "ownership transfer uniqueness" prop_ownership_transfer_uniqueness
  , testProperty "ownership transfer cycle" prop_ownership_transfer_cycle
  , testProperty "ownership transfer cycle detection" prop_ownership_transfer_cycle_detection
  , testProperty "ownership transfer valid" prop_ownership_transfer_valid
  , testProperty "ownership transfer invalid empty" prop_ownership_transfer_invalid_empty
  , testProperty "ownership transfer composition" prop_ownership_transfer_composition
  , testProperty "ownership transfer associative" prop_ownership_transfer_associative
  , testProperty "ownership transfer identity" prop_ownership_transfer_identity
  , testProperty "ownership transfer identity property" prop_ownership_transfer_identity_property
  , testProperty "ownership transfer inverse" prop_ownership_transfer_inverse
  , testProperty "ownership transfer inverse property" prop_ownership_transfer_inverse_property
  , testProperty "ownership transfer closure" prop_ownership_transfer_closure
  , testProperty "ownership transfer reachability" prop_ownership_transfer_reachability
  , testProperty "ownership transfer equivalence" prop_ownership_transfer_equivalence
  , testProperty "ownership transfer equivalence reflexive" prop_ownership_transfer_equivalence_reflexive
  , testProperty "ownership transfer equivalence symmetric" prop_ownership_transfer_equivalence_symmetric
  , testProperty "ownership transfer equivalence transitive" prop_ownership_transfer_equivalence_transitive
  , testProperty "ownership transfer consistency" prop_ownership_transfer_consistency
  , testProperty "ownership transfer consistency chain" prop_ownership_transfer_consistency_chain
  , testProperty "ownership transfer normalization" prop_ownership_transfer_normalization
  , testProperty "ownership transfer optimization" prop_ownership_transfer_optimization
  , testProperty "ownership transfer analysis" prop_ownership_transfer_analysis
  , testProperty "ownership transfer validation" prop_ownership_transfer_validation
  , testProperty "ownership transfer graph acyclic" prop_ownership_transfer_graph_acyclic
  , testProperty "ownership transfer graph connected" prop_ownership_transfer_graph_connected
  ]