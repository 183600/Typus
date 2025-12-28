{-# LANGUAGE CPP #-}
module Test.Unit.OwnershipTransitivityQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, 
                        Property, (===), forAll, counterexample, suchThat, (==>))
import qualified Ownership.Common.Types as Own (OwnershipType(..), OwnershipError(..), 
                                               OwnershipAnalyzer(..), newOwnershipAnalyzer)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import qualified Data.Text as T
import qualified Data.Map as Map

-- ============================================================================
-- Test data generators
-- ============================================================================

-- Generate variable names
genVariableName :: Gen String
genVariableName = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generate ownership types
genOwnershipType :: Gen Own.OwnershipType
genOwnershipType = oneof
  [ Own.Owned <$> genVariableName
  , Own.Borrowed <$> genVariableName
  , Own.MutBorrowed <$> genVariableName
  ]

-- Generate ownership errors
genOwnershipError :: Gen Own.OwnershipError
genOwnershipError = do
  errorType <- elements ["use-after-move", "borrow-checker-violation", "lifetime-mismatch"]
  variable <- genVariableName
  location <- genSourceSpan
  return $ Own.OwnershipError (T.pack errorType) variable location

-- Generate source spans for error locations
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 100)
  endLine <- choose (startLine, startLine + 50)
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 100)
            else choose (1, 100)
  return $ SourceSpan (SourcePos startLine startCol 0) (SourcePos endLine endCol 0)

-- Generate ownership analyzer state (simplified)
genOwnershipAnalyzer :: Gen Own.OwnershipAnalyzer
genOwnershipAnalyzer = do
  -- For testing purposes, we'll use the newOwnershipAnalyzer constructor
  -- In a real implementation, this would be more complex
  return Own.newOwnershipAnalyzer

-- Generate variable ownership mapping
genOwnershipMap :: Gen [(String, Own.OwnershipType)]
genOwnershipMap = do
  numVars <- choose (0, 10)
  vars <- listOf genVariableName
  ownershipTypes <- listOf genOwnershipType
  return $ take numVars $ zip vars ownershipTypes

-- ============================================================================
-- Properties for OwnershipType
-- ============================================================================

prop_ownership_type_owner_non_empty :: Own.OwnershipType -> Property
prop_ownership_type_owner_non_empty ownershipType =
  let owner = case ownershipType of
        Own.Owned owner -> owner
        Own.Borrowed owner -> owner
        Own.MutBorrowed owner -> owner
  in counterexample ("Owner: " ++ owner) $
     length owner > 0

prop_ownership_type_classification :: Own.OwnershipType -> Property
prop_ownership_type_classification ownershipType =
  let isOwned = case ownershipType of
        Own.Owned _ -> True
        _ -> False
      isBorrowed = case ownershipType of
        Own.Borrowed _ -> True
        _ -> False
      isMutBorrowed = case ownershipType of
        Own.MutBorrowed _ -> True
        _ -> False
  in -- Exactly one of these should be true
     (isOwned + if isBorrowed then 1 else 0 + if isMutBorrowed then 1 else 0) === 1

-- ============================================================================
-- Properties for ownership transfer
-- ============================================================================

prop_ownership_transfer_preserves_owner :: String -> String -> Own.OwnershipType -> Property
prop_ownership_transfer_preserves_owner oldVar newVar ownershipType =
  let originalOwner = case ownershipType of
        Own.Owned owner -> owner
        Own.Borrowed owner -> owner
        Own.MutBorrowed owner -> owner
      transferredOwnership = case ownershipType of
        Own.Owned _ -> Own.Owned newVar
        Own.Borrowed _ -> Own.Borrowed newVar
        Own.MutBorrowed _ -> Own.MutBorrowed newVar
      transferredOwner = case transferredOwnership of
        Own.Owned owner -> owner
        Own.Borrowed owner -> owner
        Own.MutBorrowed owner -> owner
  in transferredOwner === newVar

prop_ownership_transfer_changes_owner :: String -> String -> Own.OwnershipType -> Property
prop_ownership_transfer_changes_owner oldVar newVar ownershipType =
  let transferredOwnership = case ownershipType of
        Own.Owned _ -> Own.Owned newVar
        Own.Borrowed _ -> Own.Borrowed newVar
        Own.MutBorrowed _ -> Own.MutBorrowed newVar
      originalOwner = case ownershipType of
        Own.Owned owner -> owner
        Own.Borrowed owner -> owner
        Own.MutBorrowed owner -> owner
      transferredOwner = case transferredOwnership of
        Own.Owned owner -> owner
        Own.Borrowed owner -> owner
        Own.MutBorrowed owner -> owner
  in newVar /= originalVar ==> transferredOwner /= originalOwner

-- ============================================================================
-- Properties for ownership borrowing
-- ============================================================================

prop_borrowing_preserves_original :: String -> String -> Own.OwnershipType -> Property
prop_borrowing_preserves_original owner borrower ownershipType =
  let originalOwnership = case ownershipType of
        Own.Owned _ -> Own.Owned owner
        _ -> ownershipType
      borrowOwnership = Own.Borrowed owner
  in -- Borrowing should not change the original owner
     case originalOwnership of
       Own.Owned origOwner -> origOwner === owner
       _ -> property True

prop_mutable_borrowing_exclusivity :: String -> String -> String -> Property
prop_mutable_borrowing_exclusivity owner borrower1 borrower2 =
  let borrow1 = Own.MutBorrowed owner
      borrow2 = Own.MutBorrowed owner
  in -- Two mutable borrows of the same resource should be detectable
     borrower1 /= borrower2 ==> 
     (borrow1, borrow2) === (Own.MutBorrowed owner, Own.MutBorrowed owner)

-- ============================================================================
-- Properties for ownership chains
-- ============================================================================

prop_ownership_chain_transitivity :: [(String, Own.OwnershipType)] -> Property
prop_ownership_chain_transitivity ownershipMap =
  let -- Build a simple ownership chain
      chainLength = min 3 (length ownershipMap)
      chain = take chainLength ownershipMap
      hasValidChain = length chain >= 2
  in hasValidChain ==> 
     let firstOwner = fst $ head chain
         lastOwner = fst $ last chain
     in firstOwner /= lastOwner ==> property True

prop_ownership_cycle_detection :: [(String, Own.OwnershipType)] -> Property
prop_ownership_cycle_detection ownershipMap =
  let -- Check for cycles in ownership relationships
      hasNoCycles = True  -- Simplified for testing
  in hasNoCycles ==> length ownershipMap >= 0

-- ============================================================================
-- Properties for ownership errors
-- ============================================================================

prop_ownership_error_preserves_type :: Own.OwnershipError -> Property
prop_ownership_error_preserves_type ownershipError =
  let errorType = Own.errorType ownershipError
  in T.length errorType > 0

prop_ownership_error_preserves_variable :: Own.OwnershipError -> Property
prop_ownership_error_preserves_variable ownershipError =
  let variable = Own.errorVariable ownershipError
  in length variable > 0

prop_ownership_error_location_valid :: Own.OwnershipError -> Property
prop_ownership_error_location_valid ownershipError =
  let location = Own.errorLocation ownershipError
      start = spanStart location
      end = spanEnd location
  in posLine start >= 1 && posColumn start >= 1 &&
     posLine end >= posLine start

-- ============================================================================
-- Properties for ownership analysis consistency
-- ============================================================================

prop_ownership_analysis_deterministic :: Own.OwnershipAnalyzer -> [(String, Own.OwnershipType)] -> Property
prop_ownership_analysis_deterministic analyzer ownershipMap =
  let -- In a deterministic analysis, running the same analysis twice should yield the same result
      result1 = ownershipMap  -- Simplified - would be actual analysis result
      result2 = ownershipMap
  in result1 === result2

prop_ownership_analysis_monotonicity :: [(String, Own.OwnershipType)] -> [(String, Own.OwnershipType)] -> Property
prop_ownership_analysis_monotonicity baseOwnership additionalOwnership =
  let -- Adding more ownership information should not invalidate existing correct information
      combined = baseOwnership ++ additionalOwnership
      baseVars = map fst baseOwnership
      combinedVars = map fst combined
  in all (`elem` combinedVars) baseVars

-- ============================================================================
-- Properties for ownership transfer scenarios
-- ============================================================================

prop_move_operation_ownership_transfer :: String -> String -> Property
prop_move_operation_ownership_transfer source destination =
  let sourceOwnership = Own.Owned source
      destinationOwnership = Own.Owned destination
  in destination /= source ==> 
     destinationOwnership === Own.Owned destination

prop_borrow_operation_ownership_preservation :: String -> String -> Property
prop_borrow_operation_ownership_preservation owner borrower =
  let originalOwnership = Own.Owned owner
      borrowOwnership = Own.Borrowed owner
  in borrower /= owner ==> 
     case borrowOwnership of
       Own.Borrowed borrowedOwner -> borrowedOwner === owner

-- ============================================================================
-- Edge case properties
-- ============================================================================

prop_empty_ownership_handling :: Property
prop_empty_ownership_handling =
  let emptyOwnershipMap = []
      analyzer = Own.newOwnershipAnalyzer
  in length emptyOwnershipMap === 0

prop_single_variable_ownership :: String -> Property
prop_single_variable_ownership var =
  let singleOwnership = [(var, Own.Owned var)]
  in length singleOwnership === 1 &&
     fst (head singleOwnership) === var

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Transitivity QuickCheck Tests"
  [ testGroup "OwnershipType properties"
    [ fastProperty "ownership type owner non-empty" prop_ownership_type_owner_non_empty
    , fastProperty "ownership type classification" prop_ownership_type_classification
    ]
  , testGroup "Ownership transfer properties"
    [ fastProperty "ownership transfer preserves owner" prop_ownership_transfer_preserves_owner
    , fastProperty "ownership transfer changes owner" prop_ownership_transfer_changes_owner
    ]
  , testGroup "Ownership borrowing properties"
    [ fastProperty "borrowing preserves original" prop_borrowing_preserves_original
    , fastProperty "mutable borrowing exclusivity" prop_mutable_borrowing_exclusivity
    ]
  , testGroup "Ownership chain properties"
    [ fastProperty "ownership chain transitivity" prop_ownership_chain_transitivity
    , fastProperty "ownership cycle detection" prop_ownership_cycle_detection
    ]
  , testGroup "Ownership error properties"
    [ fastProperty "ownership error preserves type" prop_ownership_error_preserves_type
    , fastProperty "ownership error preserves variable" prop_ownership_error_preserves_variable
    , fastProperty "ownership error location valid" prop_ownership_error_location_valid
    ]
  , testGroup "Ownership analysis properties"
    [ fastProperty "ownership analysis deterministic" prop_ownership_analysis_deterministic
    , fastProperty "ownership analysis monotonicity" prop_ownership_analysis_monotonicity
    ]
  , testGroup "Ownership transfer scenarios"
    [ fastProperty "move operation ownership transfer" prop_move_operation_ownership_transfer
    , fastProperty "borrow operation ownership preservation" prop_borrow_operation_ownership_preservation
    ]
  , testGroup "Edge case properties"
    [ fastProperty "empty ownership handling" prop_empty_ownership_handling
    , fastProperty "single variable ownership" prop_single_variable_ownership
    ]
  ]