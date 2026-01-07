module Test.Unit.OwnershipRobustnessQuickCheckSpec where


import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List 
         then L.all (\res -> Own.hasOwner newOwners res) transferredResources
         else                               newState === initialSate -- Invalid transfers should not change state

-- | Test borrowing rules enforcement
testBorrowingRulesEnforcement :: Property
                              testBorrowingRulesEnforcement =
  forAll arbitrary $ \state ->
    forAll arbitrary $ \borrowRequest ->
      let canBorrow = Own.canBorrow state borrowRequest
                                        borrowedState = Own.performBorrow state borrowRequest
      in if canBorrow
         then Own.isValidBorrowedState borrowedState
         else                               borrowedState === state -- Invalid borrows should not change state

-- | Test lifetime tracking properties
testLifetimeTrackingProperties :: Property
                              testLifetimeTrackingProperties =
  forAll arbitrary $ \lifetimes ->
    let sortedLifetimes = Own.sortLifetimes lifetimes
                                      overlapping = Own.findOverlappingLifetimes lifetimes
                                      validOrder = Own.isValidLifetimeOrder sortedLifetimes
    in L.length                               sortedLifetimes === L.length lifetimes .&&.
       (if null overlapping then validOrder else property True)

-- | Test resource ownership consistency
testResourceOwnershipConsistency :: Property
                              testResourceOwnershipConsistency =
  forAll arbitrary $ \state ->
    let resources = Own.getAllResources state
                                      owners = Own.getAllOwners state
                                      ownedResources = Own.getOwnedResources owners
                                      unownedResources = Own.getUnownedResources state
    in L.all (`elem` ownedResources) resources .&&.
       L.null (resources `intersect` unownedResources)

-- | Test ownership move semantics
testOwnershipMoveSemantics :: Property
                              testOwnershipMoveSemantics =
  forAll arbitrary $ \state ->
    forAll arbitrary $ \resource ->
      forAll arbitrary $ \newOwner ->
        let movedState = Own.moveResource state resource newOwner
                                          oldOwner = Own.getOwner state resource
                                          newOwnerCheck = Own.getOwner movedState resource
        in case oldOwner of
          Just old ->                               newOwnerCheck === Just newOwner .&&.
                      Own.noLongerOwns movedState old resource
          Nothing ->                               movedState === state -- Moving unowned resource should no-op

-- | Test borrowing conflict detection
testBorrowingConflictDetection :: Property
                              testBorrowingConflictDetection =
  forAll arbitrary $ \state ->
    forAll arbitrary $ \borrow1 ->
      forAll arbitrary $ \borrow2 ->
        let conflicts = Own.borrowingsConflict borrow1 borrow2
                                          sameResource = Own.borrowedResource                               borrow1 == Own.borrowedResource borrow2
                                          bothMutable = Own.isMutableBorrow borrow1 && Own.isMutableBorrow borrow2
        in                               conflicts === (sameResource && bothMutable)

-- | Test ownership checker soundness
testOwnershipCheckerSoundness :: Property
                              testOwnershipCheckerSoundness =
  forAll arbitrary $ \program ->
    let analysis = Own.analyzeOwnership program
                                      errors = Own.getOwnershipErrors analysis
                                      warnings = Own.getOwnershipWarnings analysis
    in if Own.isValidProgram program
       then null errors -- Valid programs should have no errors
       else property True

-- | Test resource deallocation safety
testResourceDeallocationSafety :: Property
                              testResourceDeallocationSafety =
  forAll arbitrary $ \state ->
    forAll arbitrary $ \resource ->
      let deallocatedState = Own.deallocateResource state resource
                                        wasOwned = Own.isOwned state resource
                                        isStillOwned = Own.isOwned deallocatedState resource
      in if wasOwned
         then not isStillOwned -- Should no longer be owned
         else                               deallocatedState === state -- Deallocating unowned resource should no-op

-- | Test ownership annotation consistency
testOwnershipAnnotationConsistency :: Property
                              testOwnershipAnnotationConsistency =
  forAll arbitrary $ \annotations ->
    let normalized = Own.normalizeAnnotations annotations
                                      duplicates = Own.findDuplicateAnnotations annotations
                                      conflicts = Own.findConflictingAnnotations annotations
    in null duplicates .&&.
       (if null conflicts then Own.isConsistent annotations else property True)

-- | Test lifetime parameter inference
testLifetimeParameterInference :: Property
                              testLifetimeParameterInference =
  forAll arbitrary $ \function ->
    let inferredLifetimes = Own.inferLifetimes function
                                      explicitLifetimes = Own.getExplicitLifetimes function
                                      inferredValid = Own.allLifetimesValid inferredLifetimes
    in if null explicitLifetimes
       then inferredValid -- Inferred lifetimes should be valid
       else property True

-- | Test ownership graph properties
testOwnershipGraphProperties :: Property
                              testOwnershipGraphProperties =
  forAll arbitrary $ \state ->
    let graph = Own.buildOwnershipGraph state
                                      cycles = Own.findCycles graph
                                      isAcyclic = Own.isAcyclic graph
    in if isAcyclic
       then null cycles
       else L.length cycles >= 0

-- | Test borrow checker completeness
testBorrowCheckerCompleteness :: Property
                              testBorrowCheckerCompleteness =
  forAll arbitrary $ \program ->
    let borrowAnalysis = Own.checkBorrows program
                                      actualViolations = Own.findActualViolations program
                                      detectedViolations = Own.getDetectedViolations borrowAnalysis
    -- Should detect L.all actual violations (no false negatives)
    in L.all (`elem` detectedViolations) actualViolations

-- | Test ownership transfer transitivity
testOwnershipTransferTransitivity :: Property
                              testOwnershipTransferTransitivity =
  forAll arbitrary $ \state ->
    forAll arbitrary $ \resource ->
      forAll arbitrary $ \owner1 ->
        forAll arbitrary $ \owner2 ->
          let state1 = Own.moveResource state resource owner1
                                            state2 = Own.moveResource state1 resource owner2
                                            finalOwner = Own.getOwner state2 resource
          in                               finalOwner === Just owner2

-- | Test lifetime subtyping properties
testLifetimeSubtypingProperties :: Property
                              testLifetimeSubtypingProperties =
  forAll arbitrary $ \lifetime1 ->
    forAll arbitrary $ \lifetime2 ->
      let isSubtype = Own.isLifetimeSubtype lifetime1 lifetime2
                                        outlives = Own.outlivesRelation lifetime1 lifetime2
      in if isSubtype
         then outlives
         else property True

-- | Test ownership region isolation
testOwnershipRegionIsolation :: Property
                              testOwnershipRegionIsolation =
  forAll arbitrary $ \regions ->
    let isolated = Own.areRegionsIsolated regions
                                      crossReferences = Own.findCrossRegionReferences regions
    in if isolated
       then null crossReferences
       else L.length crossReferences >= 0

tests :: TestTree
tests =   testGroup "Ownership Robustness QuickCheck Tests"
  [             testProperty "Transfer invariants" testOwnershipTransferInvariants
  ,             testProperty "Borrowing rules" testBorrowingRulesEnforcement
  ,             testProperty "Lifetime tracking" testLifetimeTrackingProperties
  ,             testProperty "Resource consistency" testResourceOwnershipConsistency
  ,             testProperty "Move semantics" testOwnershipMoveSemantics
  ,             testProperty "Conflict detection" testBorrowingConflictDetection
  ,             testProperty "Checker soundness" testOwnershipCheckerSoundness
  ,             testProperty "Deallocation safety" testResourceDeallocationSafety
  ,             testProperty "Annotation consistency" testOwnershipAnnotationConsistency
  ,             testProperty "Lifetime inference" testLifetimeParameterInference
  ,             testProperty "Graph properties" testOwnershipGraphProperties
  ,             testProperty "Borrow checker completeness" testBorrowCheckerCompleteness
  ,             testProperty "Transfer transitivity" testOwnershipTransferTransitivity
  ,             testProperty "Lifetime subtyping" testLifetimeSubtypingProperties
  ,             testProperty "Region isolation" testOwnershipRegionIsolation
  ]