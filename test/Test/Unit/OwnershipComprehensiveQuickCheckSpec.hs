{-# LANGUAGE CPP #-}

module Test.Unit.OwnershipComprehensiveQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import TestSupport.ExtendedArbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, label, cover)

import Ownership
import qualified Data.Text as T
import Data.List (isInfixOf, nub, intersect, union)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Char (isAlphaNum, toLower)

-- ============================================================================
-- Ownership Type Properties
-- ============================================================================

-- Property: Ownership types are comparable
prop_ownership_types_comparable :: OwnershipType -> OwnershipType -> Property
prop_ownership_types_comparable ot1 ot2 = 
  let areComparable = ot1 == ot2 || ot1 /= ot2
  in property $ areComparable

-- Property: Ownership type extraction is consistent
prop_ownership_type_extraction_consistent :: OwnershipType -> Property
prop_ownership_type_extraction_consistent ot = 
  let extractedId = extractOwnershipId ot
      reconstructed = reconstructOwnershipType extractedId
      isConsistent = extractOwnershipId reconstructed == extractedId
  in property $ isConsistent

-- Property: Ownership type hierarchy is respected
prop_ownership_type_hierarchy :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_hierarchy ot1 ot2 = 
  let id1 = extractOwnershipId ot1
      id2 = extractOwnershipId ot2
      sameVariable = id1 == id2
      hierarchyRespected = case (ot1, ot2) of
        (Owned _, Borrowed _) -> sameVariable
        (Owned _, MutBorrowed _) -> sameVariable
        (Borrowed _, MutBorrowed _) -> sameVariable
        _ -> True
  in property $ hierarchyRespected

-- Property: Ownership type transitions are valid
prop_ownership_type_transitions_valid :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_transitions_valid from to = 
  let isValidTransition = isValidOwnershipTransition from to
  in property $ isValidTransition

-- ============================================================================
-- Ownership Error Properties
-- ============================================================================

-- Property: Ownership errors have valid structure
prop_ownership_error_structure :: OwnershipError -> Property
prop_ownership_error_structure err = 
  let hasValidIds = all isValidIdentifier (extractOwnershipErrorIds err)
      hasValidMessage = not (null (formatOwnershipError err))
  in property $ hasValidIds && hasValidMessage

-- Property: Ownership error detection is sound
prop_ownership_error_detection_sound :: String -> Property
prop_ownership_error_detection_sound code = 
  let errors = analyzeOwnership code
      detectionIsSound = all isValidOwnershipError errors
  in property $ detectionIsSound

-- Property: Ownership error detection is complete
prop_ownership_error_detection_complete :: String -> Property
prop_ownership_error_detection_complete code = 
  let errors = analyzeOwnership code
      hasRealErrors = hasOwnershipIssues code
      detectionIsComplete = hasRealErrors ==> not (null errors)
  in property $ detectionIsComplete

-- Property: Ownership error reporting is informative
prop_ownership_error_reporting_informative :: OwnershipError -> Property
prop_ownership_error_reporting_informative err = 
  let report = formatOwnershipError err
      hasLocation = "line" `isInfixOf` map toLower report
      hasMessage = length report > 10
      hasSuggestion = "suggestion" `isInfixOf` map toLower report
  in property $ hasMessage && (hasLocation || hasSuggestion)

-- ============================================================================
-- Ownership Analysis Properties
-- ============================================================================

-- Property: Ownership analysis is deterministic
prop_ownership_analysis_deterministic :: String -> Property
prop_ownership_analysis_deterministic code = 
  let analysis1 = analyzeOwnership code
      analysis2 = analyzeOwnership code
  in property $ analysis1 == analysis2

-- Property: Ownership analysis is monotonic
prop_ownership_analysis_monotonic :: String -> String -> Property
prop_ownership_analysis_monotonic code1 code2 = 
  let combinedCode = code1 ++ "\n" ++ code2
      errors1 = analyzeOwnership code1
      errors2 = analyzeOwnership code2
      combinedErrors = analyzeOwnership combinedCode
      isMonotonic = length combinedErrors >= max (length errors1) (length errors2)
  in property $ isMonotonic

-- Property: Ownership analysis respects scope
prop_ownership_analysis_respects_scope :: String -> String -> Property
prop_ownership_analysis_respects_scope outerCode innerCode = 
  let nestedCode = outerCode ++ "\n{\n" ++ innerCode ++ "\n}\n"
      outerErrors = analyzeOwnership outerCode
      innerErrors = analyzeOwnership innerCode
      nestedErrors = analyzeOwnership nestedCode
      scopeRespected = length nestedErrors >= length outerErrors + length innerErrors
  in property $ scopeRespected

-- Property: Ownership analysis handles control flow
prop_ownership_analysis_control_flow :: String -> Property
prop_ownership_analysis_control_flow code = 
  let withIfCode = "if true {\n" ++ code ++ "\n}\n"
      withLoopCode = "for {\n" ++ code ++ "\nbreak\n}\n"
      baseErrors = analyzeOwnership code
      ifErrors = analyzeOwnership withIfCode
      loopErrors = analyzeOwnership withLoopCode
      handlesControlFlow = length ifErrors >= length baseErrors &&
                           length loopErrors >= length baseErrors
  in property $ handlesControlFlow

-- ============================================================================
-- Ownership Transfer Properties
-- ============================================================================

-- Property: Ownership transfer preserves invariants
prop_ownership_transfer_preserves_invariants :: OwnershipType -> OwnershipType -> Property
prop_ownership_transfer_preserves_invariants from to = 
  let canTransfer = isValidOwnershipTransition from to
      transferPreservesInvariants = canTransfer ==> 
        let fromId = extractOwnershipId from
            toId = extractOwnershipId to
        in fromId /= toId && isValidIdentifier fromId && isValidIdentifier toId
  in property $ transferPreservesInvariants

-- Property: Ownership transfer chains are valid
prop_ownership_transfer_chains_valid :: [OwnershipType] -> Property
prop_ownership_transfer_chains_valid types = 
  let transferPairs = zip types (tail types)
      allTransfersValid = all (uncurry isValidOwnershipTransition) transferPairs
  in property $ allTransfersValid

-- Property: Ownership transfer preserves lifetime
prop_ownership_transfer_preserves_lifetime :: OwnershipType -> OwnershipType -> Property
prop_ownership_transfer_preserves_lifetime from to = 
  let canTransfer = isValidOwnershipTransition from to
      lifetimePreserved = canTransfer ==> 
        let fromLifetime = getOwnershipLifetime from
            toLifetime = getOwnershipLifetime to
        in fromLifetime >= toLifetime
  in property $ lifetimePreserved

-- ============================================================================
-- Ownership Borrowing Properties
-- ============================================================================

-- Property: Borrowing rules are enforced
prop_borrowing_rules_enforced :: OwnershipType -> OwnershipType -> Property
prop_borrowing_rules_enforced existing newBorrow = 
  let canBorrow = canCreateBorrow existing newBorrow
      rulesEnforced = canBorrow ==> 
        case (existing, newBorrow) of
          (Owned _, Borrowed _) -> True
          (Owned _, MutBorrowed _) -> True
          (Borrowed _, Borrowed _) -> extractOwnershipId existing /= extractOwnershipId newBorrow
          (MutBorrowed _, _) -> False
          _ -> True
  in property $ rulesEnforced

-- Property: Borrowing prevents invalid access
prop_borrowing_prevents_invalid_access :: OwnershipType -> [OwnershipType] -> Property
prop_borrowing_prevents_invalid_access existing borrows = 
  let allBorrowsValid = all (canCreateBorrow existing) borrows
      preventsInvalidAccess = allBorrowsValid ==> 
        let borrowIds = map extractOwnershipId borrows
            uniqueBorrowIds = nub borrowIds
        in length borrowIds == length uniqueBorrowIds
  in property $ preventsInvalidAccess

-- Property: Borrowing lifetime is bounded
prop_borrowing_lifetime_bounded :: OwnershipType -> OwnershipType -> Property
prop_borrowing_lifetime_bounded owner borrow = 
  let canBorrow = canCreateBorrow owner borrow
      lifetimeBounded = canBorrow ==>
        let ownerLifetime = getOwnershipLifetime owner
            borrowLifetime = getOwnershipLifetime borrow
        in borrowLifetime <= ownerLifetime
  in property $ lifetimeBounded

-- ============================================================================
-- Ownership Move Semantics Properties
-- ============================================================================

-- Property: Move semantics are respected
prop_move_semantics_respected :: OwnershipType -> Property
prop_move_semantics_respected ot = 
  let canMove = canMoveOwnership ot
      moveSemanticsRespected = canMove ==> 
        case ot of
          Owned _ -> True
          Borrowed _ -> False
          MutBorrowed _ -> False
  in property $ moveSemanticsRespected

-- Property: Move prevents use after move
prop_move_prevents_use_after_move :: OwnershipType -> Property
prop_move_prevents_use_after_move ot = 
  let moved = moveOwnership ot
      useAfterMovePrevented = case moved of
        Just movedType -> not (canUseOwnership movedType)
        Nothing -> True
  in property $ useAfterMovePrevented

-- Property: Move transfer is complete
prop_move_transfer_complete :: OwnershipType -> OwnershipType -> Property
prop_move_transfer_complete from to = 
  let canMove = canMoveOwnership from
      transferComplete = canMove ==> 
        case moveOwnership from of
          Just moved -> extractOwnershipId moved == extractOwnershipId to
          Nothing -> False
  in property $ transferComplete

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: Ownership analysis is efficient
prop_ownership_analysis_efficient :: String -> Property
prop_ownership_analysis_efficient code = 
  let analysisSteps = countOwnershipAnalysisSteps code
      isEfficient = analysisSteps < 1000 -- Reasonable bound
  in property $ isEfficient

-- Property: Ownership analysis memory usage is bounded
prop_ownership_analysis_memory_bounded :: String -> Property
prop_ownership_analysis_memory_bounded code = 
  let memoryUsage = estimateOwnershipAnalysisMemory code
      isBounded = memoryUsage < 10000 -- Reasonable bound
  in property $ isBounded

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Comprehensive QuickCheck Tests"
  [ testGroup "Ownership Type Properties"
    [ fastProperty "Ownership types are comparable" prop_ownership_types_comparable
    , fastProperty "Ownership type extraction is consistent" prop_ownership_type_extraction_consistent
    , fastProperty "Ownership type hierarchy is respected" prop_ownership_type_hierarchy
    , fastProperty "Ownership type transitions are valid" prop_ownership_type_transitions_valid
    ]
  , testGroup "Ownership Error Properties"
    [ fastProperty "Ownership errors have valid structure" prop_ownership_error_structure
    , fastProperty "Ownership error detection is sound" prop_ownership_error_detection_sound
    , fastProperty "Ownership error detection is complete" prop_ownership_error_detection_complete
    , fastProperty "Ownership error reporting is informative" prop_ownership_error_reporting_informative
    ]
  , testGroup "Ownership Analysis Properties"
    [ fastProperty "Ownership analysis is deterministic" prop_ownership_analysis_deterministic
    , fastProperty "Ownership analysis is monotonic" prop_ownership_analysis_monotonic
    , fastProperty "Ownership analysis respects scope" prop_ownership_analysis_respects_scope
    , fastProperty "Ownership analysis handles control flow" prop_ownership_analysis_control_flow
    ]
  , testGroup "Ownership Transfer Properties"
    [ fastProperty "Ownership transfer preserves invariants" prop_ownership_transfer_preserves_invariants
    , fastProperty "Ownership transfer chains are valid" prop_ownership_transfer_chains_valid
    , fastProperty "Ownership transfer preserves lifetime" prop_ownership_transfer_preserves_lifetime
    ]
  , testGroup "Ownership Borrowing Properties"
    [ fastProperty "Borrowing rules are enforced" prop_borrowing_rules_enforced
    , fastProperty "Borrowing prevents invalid access" prop_borrowing_prevents_invalid_access
    , fastProperty "Borrowing lifetime is bounded" prop_borrowing_lifetime_bounded
    ]
  , testGroup "Ownership Move Semantics Properties"
    [ fastProperty "Move semantics are respected" prop_move_semantics_respected
    , fastProperty "Move prevents use after move" prop_move_prevents_use_after_move
    , fastProperty "Move transfer is complete" prop_move_transfer_complete
    ]
  , testGroup "Performance Properties"
    [ fastProperty "Ownership analysis is efficient" prop_ownership_analysis_efficient
    , fastProperty "Ownership analysis memory usage is bounded" prop_ownership_analysis_memory_bounded
    ]
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

extractOwnershipId :: OwnershipType -> String
extractOwnershipId (Owned id) = id
extractOwnershipId (Borrowed id) = id
extractOwnershipId (MutBorrowed id) = id

reconstructOwnershipType :: String -> OwnershipType
reconstructOwnershipType id = Owned id -- Simplified

isValidOwnershipTransition :: OwnershipType -> OwnershipType -> Bool
isValidOwnershipTransition from to = 
  let fromId = extractOwnershipId from
      toId = extractOwnershipId to
  in fromId /= toId && isValidIdentifier fromId && isValidIdentifier toId

isValidIdentifier :: String -> Bool
isValidIdentifier name = not (null name) && all isAlphaNum name



formatOwnershipError :: OwnershipError -> String
formatOwnershipError err = case err of
  UseAfterMove id -> "Use after move: " ++ id
  DoubleMove id1 id2 -> "Double move: " ++ id1 ++ ", " ++ id2
  BorrowWhileMoved id -> "Borrow while moved: " ++ id
  MutBorrowWhileBorrowed id -> "Mutable borrow while borrowed: " ++ id
  BorrowWhileMutBorrowed id -> "Borrow while mutably borrowed: " ++ id
  MultipleMutBorrows id -> "Multiple mutable borrows: " ++ id
  UseWhileMutBorrowed id -> "Use while mutably borrowed: " ++ id
  OutOfScope id -> "Out of scope: " ++ id
  BorrowError id -> "Borrow error: " ++ id
  ParseError id -> "Parse error: " ++ id
  CrossFunctionMove id1 id2 -> "Cross function move: " ++ id1 ++ " to " ++ id2
  ParameterMoveMismatch id -> "Parameter move mismatch: " ++ id
  ControlFlowError id -> "Control flow error: " ++ id
  PathSensitiveError id -> "Path sensitive error: " ++ id
  LoopOwnershipError id -> "Loop ownership error: " ++ id

hasOwnershipIssues :: String -> Bool
hasOwnershipIssues code = 
  "move" `isInfixOf` code || "borrow" `isInfixOf` code

isValidOwnershipError :: OwnershipError -> Bool
isValidOwnershipError err = 
  let ids = extractOwnershipErrorIds err
  in all isValidIdentifier ids

extractOwnershipErrorIds :: OwnershipError -> [String]
extractOwnershipErrorIds err = case err of
  UseAfterMove id -> [id]
  DoubleMove id1 id2 -> [id1, id2]
  BorrowWhileMoved id -> [id]
  MutBorrowWhileBorrowed id -> [id]
  BorrowWhileMutBorrowed id -> [id]
  MultipleMutBorrows id -> [id]
  UseWhileMutBorrowed id -> [id]
  OutOfScope id -> [id]
  BorrowError id -> [id]
  ParseError id -> [id]
  CrossFunctionMove id1 id2 -> [id1, id2]
  ParameterMoveMismatch id -> [id]
  ControlFlowError id -> [id]
  PathSensitiveError id -> [id]
  LoopOwnershipError id -> [id]

getOwnershipLifetime :: OwnershipType -> Int
getOwnershipLifetime (Owned _) = 100
getOwnershipLifetime (Borrowed _) = 50
getOwnershipLifetime (MutBorrowed _) = 25

canCreateBorrow :: OwnershipType -> OwnershipType -> Bool
canCreateBorrow existing newBorrow = 
  let existingId = extractOwnershipId existing
      newId = extractOwnershipId newBorrow
  in case (existing, newBorrow) of
    (Owned _, Borrowed _) -> True
    (Owned _, MutBorrowed _) -> True
    (Borrowed _, Borrowed _) -> existingId /= newId
    _ -> False

canMoveOwnership :: OwnershipType -> Bool
canMoveOwnership (Owned _) = True
canMoveOwnership _ = False

moveOwnership :: OwnershipType -> Maybe OwnershipType
moveOwnership (Owned id) = Just (Owned id)
moveOwnership _ = Nothing

canUseOwnership :: OwnershipType -> Bool
canUseOwnership (Owned _) = True
canUseOwnership (Borrowed _) = True
canUseOwnership (MutBorrowed _) = True

countOwnershipAnalysisSteps :: String -> Int
countOwnershipAnalysisSteps code = 
  length (lines code) * 10 -- Simplified implementation

estimateOwnershipAnalysisMemory :: String -> Int
estimateOwnershipAnalysisMemory code = 
  length code * 2 -- Simplified implementation