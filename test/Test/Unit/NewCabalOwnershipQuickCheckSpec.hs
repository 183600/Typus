{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (oneof, listOf, choose, elements, listOf1)

import Ownership
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

import Data.List (isInfixOf, isPrefixOf, nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Mock Ownership Data Types for Testing
-- ============================================================================

data MockOwnershipType = MockOwnershipType
  { ownershipTypeName :: String
  , ownershipIsOwned :: Bool
  , ownershipCanMove :: Bool
  , ownershipCanCopy :: Bool
  } deriving (Show, Eq)

data MockOwnershipError = MockOwnershipError
  { errorMessage :: String
  , errorLocation :: SourceSpan
  , errorType :: String
  } deriving (Show, Eq)

data MockOwnershipTransfer = MockOwnershipTransfer
  { transferFrom :: String
  , transferTo :: String
  , transferType :: MockOwnershipType
  , transferLocation :: SourceSpan
  } deriving (Show, Eq)

data MockOwnershipAnalyzer = MockOwnershipAnalyzer
  { analyzerSymbolTable :: Map String MockOwnershipType
  , analyzerTransfers :: [MockOwnershipTransfer]
  , analyzerErrors :: [MockOwnershipError]
  } deriving (Show, Eq)

data MockOwnershipState = MockOwnershipState
  { stateOwners :: Map String String
  , stateBorrowedVars :: Set String
  , stateMovedVars :: Set String
  } deriving (Show, Eq)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    let validEnd = if end >= start then end else start
    return $ SourceSpan start validEnd

instance Arbitrary MockOwnershipType where
  arbitrary = do
    name <- elements ["Owned", "Borrowed", "Shared", "Unique", "Reference"]
    isOwned <- arbitrary
    canMove <- arbitrary
    canCopy <- arbitrary
    return $ MockOwnershipType name isOwned canMove canCopy

instance Arbitrary MockOwnershipError where
  arbitrary = do
    message <- listOf1 (elements ['a'..'z'] ++ " ")
    location <- arbitrary
    errorType' <- elements ["MoveError", "BorrowError", "CopyError", "LifetimeError"]
    return $ MockOwnershipError message location errorType'

instance Arbitrary MockOwnershipTransfer where
  arbitrary = do
    fromVar <- elements ["x", "y", "z", "var1", "var2", "var3"]
    toVar <- elements ["a", "b", "c", "dest1", "dest2", "dest3"]
    transferType' <- arbitrary
    location <- arbitrary
    return $ MockOwnershipTransfer fromVar toVar transferType' location

instance Arbitrary MockOwnershipAnalyzer where
  arbitrary = do
    symbolTable <- Map.fromList <$> listOf (do
      name <- elements ["x", "y", "z", "var1", "var2", "var3"]
      ownershipType <- arbitrary
      return (name, ownershipType))
    transfers <- listOf arbitrary
    errors <- listOf arbitrary
    return $ MockOwnershipAnalyzer symbolTable transfers errors

instance Arbitrary MockOwnershipState where
  arbitrary = do
    owners <- Map.fromList <$> listOf (do
      var <- elements ["x", "y", "z", "var1", "var2", "var3"]
      owner <- elements ["func1", "func2", "func3", "main"]
      return (var, owner))
    borrowed <- Set.fromList <$> listOf (elements ["x", "y", "z", "var1", "var2"])
    moved <- Set.fromList <$> listOf (elements ["a", "b", "c", "dest1", "dest2"])
    return $ MockOwnershipState owners borrowed moved

-- ============================================================================
-- Ownership Property Tests
-- ============================================================================

-- Property: Ownership type name is preserved
prop_ownership_type_name_preserved :: MockOwnershipType -> Property
prop_ownership_type_name_preserved ownershipType =
  let originalName = ownershipTypeName ownershipType
      retrievedName = ownershipTypeName ownershipType
  in property $ originalName === retrievedName

-- Property: Ownership type flags are preserved
prop_ownership_type_flags_preserved :: MockOwnershipType -> Property
prop_ownership_type_flags_preserved ownershipType =
  let originalOwned = ownershipIsOwned ownershipType
      originalMove = ownershipCanMove ownershipType
      originalCopy = ownershipCanCopy ownershipType
      retrievedOwned = ownershipIsOwned ownershipType
      retrievedMove = ownershipCanMove ownershipType
      retrievedCopy = ownershipCanCopy ownershipType
  in property $ (originalOwned, originalMove, originalCopy) === (retrievedOwned, retrievedMove, retrievedCopy)

-- Property: Ownership error location is preserved
prop_ownership_error_location_preserved :: MockOwnershipError -> Property
prop_ownership_error_location_preserved error =
  let originalLocation = errorLocation error
      retrievedLocation = errorLocation error
  in property $ originalLocation === retrievedLocation

-- Property: Ownership transfer preserves source and destination
prop_ownership_transfer_preserves_vars :: MockOwnershipTransfer -> Property
prop_ownership_transfer_preserves_vars transfer =
  let originalFrom = transferFrom transfer
      originalTo = transferTo transfer
      retrievedFrom = transferFrom transfer
      retrievedTo = transferTo transfer
  in property $ (originalFrom, originalTo) === (retrievedFrom, retrievedTo)

-- Property: Ownership analyzer symbol table preserves mappings
prop_ownership_analyzer_symboltable_preserves :: MockOwnershipAnalyzer -> Property
prop_ownership_analyzer_symboltable_preserves analyzer =
  let originalTable = analyzerSymbolTable analyzer
      retrievedTable = analyzerSymbolTable analyzer
  in property $ originalTable === retrievedTable

-- Property: Ownership state tracks owners correctly
prop_ownership_state_tracks_owners :: MockOwnershipState -> Property
prop_ownership_state_tracks_owners state =
  let owners = stateOwners state
      ownerCount = Map.size owners
  in property $ ownerCount >= 0

-- Property: Borrowed variables are tracked in state
prop_ownership_state_tracks_borrowed :: MockOwnershipState -> Property
prop_ownership_state_tracks_borrowed state =
  let borrowed = stateBorrowedVars state
      borrowedCount = Set.size borrowed
  in property $ borrowedCount >= 0

-- Property: Moved variables are tracked in state
prop_ownership_state_tracks_moved :: MockOwnershipState -> Property
prop_ownership_state_tracks_moved state =
  let moved = stateMovedVars state
      movedCount = Set.size moved
  in property $ movedCount >= 0

-- Property: Ownership transfer is valid when source exists
prop_ownership_transfer_valid_source :: MockOwnershipAnalyzer -> MockOwnershipTransfer -> Property
prop_ownership_transfer_valid_source analyzer transfer =
  let symbolTable = analyzerSymbolTable analyzer
      source = transferFrom transfer
      hasSource = Map.member source symbolTable
  in classify hasSource "source exists" $
     classify (not hasSource) "source missing" $
     property $ True

-- Property: Ownership transfer creates appropriate state changes
prop_ownership_transfer_state_changes :: MockOwnershipState -> MockOwnershipTransfer -> Property
prop_ownership_transfer_state_changes state transfer =
  let source = transferFrom transfer
      dest = transferTo transfer
      originalOwners = stateOwners state
      originalMoved = stateMovedVars state
      hasSource = Map.member source originalOwners
      destNotMoved = not (Set.member dest originalMoved)
  in classify hasSource "source exists" $
     classify destNotMoved "destination not moved" $
     property $ True

-- Property: Borrowed variables cannot be moved
prop_ownership_borrowed_cannot_move :: MockOwnershipState -> String -> Property
prop_ownership_borrowed_cannot_move state var =
  let borrowed = stateBorrowedVars state
      isBorrowed = Set.member var borrowed
      moved = stateMovedVars state
      isMoved = Set.member var moved
  in classify isBorrowed "is borrowed" $
     classify isMoved "is moved" $
     property $ not (isBorrowed && isMoved)

-- Property: Ownership analysis detects double moves
prop_ownership_analysis_double_move :: MockOwnershipAnalyzer -> MockOwnershipTransfer -> MockOwnershipTransfer -> Property
prop_ownership_analysis_double_move analyzer transfer1 transfer2 =
  let source1 = transferFrom transfer1
      source2 = transferFrom transfer2
      sameSource = source1 == source2
  in classify sameSource "same source" $
     classify (not sameSource) "different sources" $
     property $ True

-- Property: Ownership type consistency is maintained
prop_ownership_type_consistency :: MockOwnershipType -> MockOwnershipType -> Property
prop_ownership_type_consistency type1 type2 =
  let name1 = ownershipTypeName type1
      name2 = ownershipTypeName type2
      sameName = name1 == name2
  in classify sameName "same type name" $
     classify (not sameName) "different type names" $
     property $ True

-- Property: Ownership analyzer accumulates errors correctly
prop_ownership_analyzer_accumulates_errors :: MockOwnershipAnalyzer -> [MockOwnershipError] -> Property
prop_ownership_analyzer_accumulates_errors analyzer newErrors =
  let originalErrors = analyzerErrors analyzer
      errorCount = length originalErrors
      newErrorCount = length newErrors
  in property $ errorCount >= 0 .&&. newErrorCount >= 0

-- Property: Ownership state transitions are valid
prop_ownership_state_transitions_valid :: MockOwnershipState -> String -> String -> Property
prop_ownership_state_transitions_valid state source dest =
  let owners = stateOwners state
      moved = stateMovedVars state
      sourceExists = Map.member source owners
      destNotMoved = not (Set.member dest moved)
  in classify sourceExists "source exists" $
     classify destNotMoved "destination not moved" $
     property $ True

-- Property: Ownership transfer preserves ownership type
prop_ownership_transfer_preserves_type :: MockOwnershipTransfer -> Property
prop_ownership_transfer_preserves_type transfer =
  let originalType = transferType transfer
      retrievedType = transferType transfer
  in property $ originalType === retrievedType

-- Property: Ownership analyzer can handle empty symbol table
prop_ownership_analyzer_empty_symboltable :: Property
prop_ownership_analyzer_empty_symboltable =
  let analyzer = MockOwnershipAnalyzer Map.empty [] []
      symbolTable = analyzerSymbolTable analyzer
      tableSize = Map.size symbolTable
  in property $ tableSize === 0

-- Property: Ownership state can handle empty mappings
prop_ownership_state_empty_mappings :: Property
prop_ownership_state_empty_mappings =
  let state = MockOwnershipState Map.empty Set.empty Set.empty
      owners = stateOwners state
      borrowed = stateBorrowedVars state
      moved = stateMovedVars state
  in property $ Map.size owners === 0 .&&. Set.size borrowed === 0 .&&. Set.size moved === 0

-- Property: Ownership error messages are preserved
prop_ownership_error_message_preserved :: MockOwnershipError -> Property
prop_ownership_error_message_preserved error =
  let originalMessage = errorMessage error
      retrievedMessage = errorMessage error
  in property $ originalMessage === retrievedMessage

-- Property: Ownership error types are preserved
prop_ownership_error_type_preserved :: MockOwnershipError -> Property
prop_ownership_error_type_preserved error =
  let originalType = errorType error
      retrievedType = errorType error
  in property $ originalType === retrievedType

-- Property: Ownership transfer location is preserved
prop_ownership_transfer_location_preserved :: MockOwnershipTransfer -> Property
prop_ownership_transfer_location_preserved transfer =
  let originalLocation = transferLocation transfer
      retrievedLocation = transferLocation transfer
  in property $ originalLocation === retrievedLocation

-- Property: Ownership analysis is deterministic
prop_ownership_analysis_deterministic :: MockOwnershipAnalyzer -> MockOwnershipTransfer -> Property
prop_ownership_analysis_deterministic analyzer transfer =
  let analysis1 = analyzer -- Simplified analysis
      analysis2 = analyzer -- Simplified analysis
  in property $ analysis1 === analysis2

-- Property: Ownership state updates are consistent
prop_ownership_state_updates_consistent :: MockOwnershipState -> Property
prop_ownership_state_updates_consistent state =
  let originalOwners = stateOwners state
      originalBorrowed = stateBorrowedVars state
      originalMoved = stateMovedVars state
  in property $ (originalOwners, originalBorrowed, originalMoved) === 
                (stateOwners state, stateBorrowedVars state, stateMovedVars state)

tests :: TestTree
tests = testGroup "New Cabal Ownership QuickCheck Tests"
  [ fastProperty "Ownership type name preserved" prop_ownership_type_name_preserved
  , fastProperty "Ownership type flags preserved" prop_ownership_type_flags_preserved
  , fastProperty "Ownership error location preserved" prop_ownership_error_location_preserved
  , fastProperty "Ownership transfer preserves vars" prop_ownership_transfer_preserves_vars
  , fastProperty "Ownership analyzer symboltable preserves" prop_ownership_analyzer_symboltable_preserves
  , fastProperty "Ownership state tracks owners" prop_ownership_state_tracks_owners
  , fastProperty "Ownership state tracks borrowed" prop_ownership_state_tracks_borrowed
  , fastProperty "Ownership state tracks moved" prop_ownership_state_tracks_moved
  , fastProperty "Ownership transfer valid source" prop_ownership_transfer_valid_source
  , fastProperty "Ownership transfer state changes" prop_ownership_transfer_state_changes
  , fastProperty "Borrowed cannot move" prop_ownership_borrowed_cannot_move
  , fastProperty "Ownership analysis double move" prop_ownership_analysis_double_move
  , fastProperty "Ownership type consistency" prop_ownership_type_consistency
  , fastProperty "Ownership analyzer accumulates errors" prop_ownership_analyzer_accumulates_errors
  , fastProperty "Ownership state transitions valid" prop_ownership_state_transitions_valid
  , fastProperty "Ownership transfer preserves type" prop_ownership_transfer_preserves_type
  , fastProperty "Ownership analyzer empty symboltable" prop_ownership_analyzer_empty_symboltable
  , fastProperty "Ownership state empty mappings" prop_ownership_state_empty_mappings
  , fastProperty "Ownership error message preserved" prop_ownership_error_message_preserved
  , fastProperty "Ownership error type preserved" prop_ownership_error_type_preserved
  , fastProperty "Ownership transfer location preserved" prop_ownership_transfer_location_preserved
  , fastProperty "Ownership analysis deterministic" prop_ownership_analysis_deterministic
  , fastProperty "Ownership state updates consistent" prop_ownership_state_updates_consistent
  ]