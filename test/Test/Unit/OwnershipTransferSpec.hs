module Test.Unit.OwnershipTransferSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt, startPos, posAt)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Test cases for ownership tracking
testOwnershipTracking :: TestTree
testOwnershipTracking = testGroup "Ownership tracking tests"
  [ testCase "create owned resource" $
      let resource = createResource "variable1" (locatedAt startPos "int")
          owner = createOwner "function1" startPos
          transfer = createTransfer owner resource startPos
      in transferSource transfer @?= owner
  , testCase "track multiple owners" $
      let resource = createResource "shared_var" (locatedAt startPos "string")
          owner1 = createOwner "owner1" startPos
          owner2 = createOwner "owner2" startPos
          state = addOwner owner2 resource (addOwner owner1 resource emptyOwnershipState)
      in do hasOwner resource owner1 state @?= True
            hasOwner resource owner2 state @?= True
  , testCase "transfer ownership" $
      let resource = createResource "data" (locatedAt startPos "array")
          originalOwner = createOwner "original" startPos
          newOwner = createOwner "new" startPos
          state = transferOwnership resource originalOwner newOwner emptyOwnershipState
      in do hasOwner resource newOwner state @?= True
            hasOwner resource originalOwner state @?= False
  ]

-- Test cases for ownership constraints
testOwnershipConstraints :: TestTree
testOwnershipConstraints = testGroup "Ownership constraints tests"
  [ testCase "single ownership constraint" $
      let resource = createResource "unique_data" (locatedAt startPos "object")
          owner1 = createOwner "owner1" startPos
          owner2 = createOwner "owner2" startPos
          state = addOwnershipConstraint SingleOwner resource emptyOwnershipState
          result1 = canAddOwner resource owner1 state
          result2 = canAddOwner resource owner2 state
      in do result1 @?= True
            result2 @?= False
  , testCase "shared ownership constraint" $
      let resource = createResource "shared_data" (locatedAt startPos "buffer")
          owner1 = createOwner "owner1" startPos
          owner2 = createOwner "owner2" startPos
          state = addOwnershipConstraint SharedOwner resource emptyOwnershipState
          result1 = canAddOwner resource owner1 state
          result2 = canAddOwner resource owner2 state
      in do result1 @?= True
            result2 @?= True
  , testCase "no ownership constraint" $
      let resource = createResource "unconstrained" (locatedAt startPos "value")
          owner1 = createOwner "owner1" startPos
          owner2 = createOwner "owner2" startPos
          state = emptyOwnershipState
          result1 = canAddOwner resource owner1 state
          result2 = canAddOwner resource owner2 state
      in do result1 @?= True
            result2 @?= True
  ]

-- Test cases for ownership borrowing
testOwnershipBorrowing :: TestTree
testOwnershipBorrowing = testGroup "Ownership borrowing tests"
  [ testCase "create immutable borrow" $
      let resource = createResource "data" (locatedAt startPos "vector")
          owner = createOwner "function" startPos
          borrower = createBorrower "other_function" startPos
          state = addOwner owner resource emptyOwnershipState
          borrow = createImmutableBorrow borrower resource startPos
          newState = addBorrow borrow state
      in hasActiveBorrow resource borrower newState @?= True
  , testCase "prevent mutable borrow when immutable exists" $
      let resource = createResource "data" (locatedAt startPos "vector")
          owner = createOwner "function" startPos
          borrower1 = createBorrower "immutable_user" startPos
          borrower2 = createBorrower "mutable_user" startPos
          state = addOwner owner resource emptyOwnershipState
          borrow1 = createImmutableBorrow borrower1 resource startPos
          stateWithBorrow = addBorrow borrow1 state
          canMutableBorrow = canCreateMutableBorrow resource borrower2 stateWithBorrow
      in canMutableBorrow @?= False
  , testCase "prevent multiple mutable borrows" $
      let resource = createResource "data" (locatedAt startPos "vector")
          owner = createOwner "function" startPos
          borrower1 = createBorrower "mutable_user1" startPos
          borrower2 = createBorrower "mutable_user2" startPos
          state = addOwner owner resource emptyOwnershipState
          borrow1 = createMutableBorrow borrower1 resource startPos
          stateWithBorrow = addBorrow borrow1 state
          canSecondMutableBorrow = canCreateMutableBorrow resource borrower2 stateWithBorrow
      in canSecondMutableBorrow @?= False
  ]

-- Test cases for ownership lifetimes
testOwnershipLifetimes :: TestTree
testOwnershipLifetimes = testGroup "Ownership lifetime tests"
  [ testCase "track resource lifetime" $
      let resource = createResource "temp_data" (locatedAt startPos "temporary")
          owner = createOwner "scope1" startPos
          lifetime = createLifetime "scope1" (posAt 1 1) (posAt 10 1)
          state = setResourceLifetime resource lifetime emptyOwnershipState
      in getResourceLifetime resource state @?= Just lifetime
  , testCase "check lifetime validity" $
      let lifetime = createLifetime "valid_scope" (posAt 1 1) (posAt 10 1)
          validPos = posAt 5 5
          invalidPos = posAt 15 5
      in do isPositionInLifetime validPos lifetime @?= True
            isPositionInLifetime invalidPos lifetime @?= False
  , testCase "prevent use after free" $
      let resource = createResource "freed_data" (locatedAt startPos "pointer")
          owner = createOwner "scope1" startPos
          lifetime = createLifetime "scope1" (posAt 1 1) (posAt 5 1)
          usePos = posAt 10 1
          state = setResourceLifetime resource lifetime emptyOwnershipState
          canUse = canUseResourceAt resource usePos state
      in canUse @?= False
  ]

-- Test cases for ownership transitivity
testOwnershipTransitivity :: TestTree
testOwnershipTransitivity = testGroup "Ownership transitivity tests"
  [ testCase "track nested ownership" $
      let outer = createResource "outer_struct" (locatedAt startPos "struct")
          inner = createResource "inner_field" (locatedAt startPos "field")
          owner = createOwner "function" startPos
          state = addOwner owner outer emptyOwnershipState
          stateWithNested = addNestedOwnership outer inner state
      in hasOwner inner owner stateWithNested @?= True
  , testCase "transfer nested ownership" $
      let outer = createResource "outer_struct" (locatedAt startPos "struct")
          inner = createResource "inner_field" (locatedAt startPos "field")
          originalOwner = createOwner "original" startPos
          newOwner = createOwner "new" startPos
          state = addOwner originalOwner outer emptyOwnershipState
          stateWithNested = addNestedOwnership outer inner state
          finalState = transferOwnership outer originalOwner newOwner stateWithNested
      in hasOwner inner newOwner finalState @?= True
  ]

-- Test cases for ownership analysis
testOwnershipAnalysis :: TestTree
testOwnershipAnalysis = testGroup "Ownership analysis tests"
  [ testCase "detect ownership violations" $
      let resource = createResource "data" (locatedAt startPos "array")
          owner = createOwner "function" startPos
          user = createOwner "other_function" startPos
          use = createUse user resource (posAt 10 5)
          state = addOwner owner resource emptyOwnershipState
          violations = checkOwnershipViolations [use] state
      in length violations @?= 1
  , testCase "detect borrow violations" $
      let resource = createResource "data" (locatedAt startPos "vector")
          owner = createOwner "function" startPos
          borrower = createBorrower "borrower" startPos
          mutableBorrow = createMutableBorrow borrower resource (posAt 5 5)
          secondBorrow = createMutableBorrow borrower resource (posAt 10 5)
          state = addOwner owner resource emptyOwnershipState
          stateWithBorrow = addBorrow mutableBorrow state
          violations = checkBorrowViolations [secondBorrow] stateWithBorrow
      in length violations @?= 1
  ]

-- Mock data types and functions for testing
data Resource = Resource
  { resourceId :: String
  , resourceType :: Located String
  } deriving (Show, Eq)

data Owner = Owner
  { ownerId :: String
  , ownerPosition :: SourcePos
  } deriving (Show, Eq)

data Borrower = Borrower
  { borrowerId :: String
  , borrowerPosition :: SourcePos
  } deriving (Show, Eq)

data OwnershipTransfer = OwnershipTransfer
  { transferSource :: Owner
  , transferResource :: Resource
  , transferPosition :: SourcePos
  } deriving (Show, Eq)

data OwnershipConstraint = SingleOwner | SharedOwner | Unconstrained deriving (Show, Eq)

data Borrow = Borrow
  { borrowResource :: Resource
  , borrowBorrower :: Borrower
  , borrowPosition :: SourcePos
  , borrowType :: BorrowType
  } deriving (Show, Eq)

data BorrowType = ImmutableBorrow | MutableBorrow deriving (Show, Eq)

data Lifetime = Lifetime
  { lifetimeName :: String
  , lifetimeStart :: SourcePos
  , lifetimeEnd :: SourcePos
  } deriving (Show, Eq)

data Use = Use
  { useResource :: Resource
  , useUser :: Owner
  , usePosition :: SourcePos
  } deriving (Show, Eq)

data OwnershipState = OwnershipState
  { resourceOwners :: Map.Map String [Owner]
  , resourceConstraints :: Map.Map String OwnershipConstraint
  , activeBorrows :: Map.Map String [Borrow]
  , resourceLifetimes :: Map.Map String Lifetime
  , nestedOwnership :: Map.Map String [Resource]
  } deriving (Show, Eq)

data OwnershipViolation = OwnershipViolation
  { violationType :: String
  , violationPosition :: SourcePos
  , violationMessage :: String
  } deriving (Show, Eq)

-- Mock implementations
createResource :: String -> Located String -> Resource
createResource name typ = Resource name typ

createOwner :: String -> SourcePos -> Owner
createOwner name pos = Owner name pos

createBorrower :: String -> SourcePos -> Borrower
createBorrower name pos = Borrower name pos

createTransfer :: Owner -> Resource -> SourcePos -> OwnershipTransfer
createTransfer source resource pos = OwnershipTransfer source resource pos

emptyOwnershipState :: OwnershipState
emptyOwnershipState = OwnershipState Map.empty Map.empty Map.empty Map.empty Map.empty

addOwner :: Owner -> Resource -> OwnershipState -> OwnershipState
addOwner owner resource state = 
  let owners = Map.findWithDefault [] (resourceId resource) (resourceOwners state)
      newOwners = owner : owners
  in state { resourceOwners = Map.insert (resourceId resource) newOwners (resourceOwners state) }

hasOwner :: Resource -> Owner -> OwnershipState -> Bool
hasOwner resource owner state = 
  case Map.lookup (resourceId resource) (resourceOwners state) of
    Nothing -> False
    Just owners -> owner `elem` owners

transferOwnership :: Resource -> Owner -> Owner -> OwnershipState -> OwnershipState
transferOwnership resource fromOwner toOwner state = 
  let owners = Map.findWithDefault [] (resourceId resource) (resourceOwners state)
      newOwners = toOwner : filter (/= fromOwner) owners
  in state { resourceOwners = Map.insert (resourceId resource) newOwners (resourceOwners state) }

addOwnershipConstraint :: OwnershipConstraint -> Resource -> OwnershipState -> OwnershipState
addOwnershipConstraint constraint resource state = 
  state { resourceConstraints = Map.insert (resourceId resource) constraint (resourceConstraints state) }

canAddOwner :: Resource -> Owner -> OwnershipState -> Bool
canAddOwner resource owner state = 
  case Map.lookup (resourceId resource) (resourceConstraints state) of
    Nothing -> True
    Just SingleOwner -> null (Map.findWithDefault [] (resourceId resource) (resourceOwners state))
    Just SharedOwner -> True
    Just Unconstrained -> True

createImmutableBorrow :: Borrower -> Resource -> SourcePos -> Borrow
createImmutableBorrow borrower resource pos = Borrow resource borrower pos ImmutableBorrow

createMutableBorrow :: Borrower -> Resource -> SourcePos -> Borrow
createMutableBorrow borrower resource pos = Borrow resource borrower pos MutableBorrow

addBorrow :: Borrow -> OwnershipState -> OwnershipState
addBorrow borrow state = 
  let resource = borrowResource borrow
      borrows = Map.findWithDefault [] (resourceId resource) (activeBorrows state)
      newBorrows = borrow : borrows
  in state { activeBorrows = Map.insert (resourceId resource) newBorrows (activeBorrows state) }

hasActiveBorrow :: Resource -> Borrower -> OwnershipState -> Bool
hasActiveBorrow resource borrower state = 
  case Map.lookup (resourceId resource) (activeBorrows state) of
    Nothing -> False
    Just borrows -> any (\b -> borrowBorrower b == borrower) borrows

canCreateMutableBorrow :: Resource -> Borrower -> OwnershipState -> Bool
canCreateMutableBorrow resource borrower state = 
  case Map.lookup (resourceId resource) (activeBorrows state) of
    Nothing -> True
    Just borrows -> null borrows  -- No existing borrows allowed for mutable

createLifetime :: String -> SourcePos -> SourcePos -> Lifetime
createLifetime name start end = Lifetime name start end

setResourceLifetime :: Resource -> Lifetime -> OwnershipState -> OwnershipState
setResourceLifetime resource lifetime state = 
  state { resourceLifetimes = Map.insert (resourceId resource) lifetime (resourceLifetimes state) }

getResourceLifetime :: Resource -> OwnershipState -> Maybe Lifetime
getResourceLifetime resource state = Map.lookup (resourceId resource) (resourceLifetimes state)

isPositionInLifetime :: SourcePos -> Lifetime -> Bool
isPositionInLifetime pos lifetime = 
  pos >= lifetimeStart lifetime && pos <= lifetimeEnd lifetime

canUseResourceAt :: Resource -> SourcePos -> OwnershipState -> Bool
canUseResourceAt resource pos state = 
  case Map.lookup (resourceId resource) (resourceLifetimes state) of
    Nothing -> True
    Just lifetime -> isPositionInLifetime pos lifetime

addNestedOwnership :: Resource -> Resource -> OwnershipState -> OwnershipState
addNestedOwnership outer inner state = 
  let nested = Map.findWithDefault [] (resourceId outer) (nestedOwnership state)
      newNested = inner : nested
  in state { nestedOwnership = Map.insert (resourceId outer) newNested (nestedOwnership state) }

createUse :: Owner -> Resource -> SourcePos -> Use
createUse user resource pos = Use resource user pos

checkOwnershipViolations :: [Use] -> OwnershipState -> [OwnershipViolation]
checkOwnershipViolations uses state = 
  [ OwnershipViolation "Use without ownership" (usePosition use) "Resource used by non-owner"
  | use <- uses
  , not (hasOwner (useResource use) (useUser use) state)
  ]

checkBorrowViolations :: [Borrow] -> OwnershipState -> [OwnershipViolation]
checkBorrowViolations borrows state = 
  [ OwnershipViolation "Multiple mutable borrows" (borrowPosition borrow) "Cannot have multiple mutable borrows"
  | borrow <- borrows
  , borrowType borrow == MutableBorrow
  , not (canCreateMutableBorrow (borrowResource borrow) (borrowBorrower borrow) state)
  ]

-- QuickCheck properties
prop_transfer_ownership_changes_owner :: Resource -> Owner -> Owner -> OwnershipState -> Property
prop_transfer_ownership_changes_owner resource fromOwner toOwner state = 
  let stateWithOwner = addOwner fromOwner resource state
      finalState = transferOwnership resource fromOwner toOwner stateWithOwner
  in (hasOwner resource toOwner finalState && not (hasOwner resource fromOwner finalState)) === True

prop_single_owner_constraint :: Resource -> Owner -> Owner -> OwnershipState -> Property
prop_single_owner_constraint resource owner1 owner2 state = 
  let constrainedState = addOwnershipConstraint SingleOwner resource state
      canFirst = canAddOwner resource owner1 constrainedState
      stateWithFirst = if canFirst then addOwner owner1 resource constrainedState else constrainedState
      canSecond = canAddOwner resource owner2 stateWithFirst
  in canFirst ==> not canSecond

prop_lifetime_validity :: SourcePos -> SourcePos -> SourcePos -> Property
prop_lifetime_validity start end testPos = 
  let validEnd = if end >= start then end else start
      lifetime = createLifetime "test" start validEnd
      inLifetime = isPositionInLifetime testPos lifetime
  in (testPos >= start && testPos <= validEnd) ==> inLifetime

tests :: TestTree
tests = testGroup "Ownership Transfer Tests"
  [ testOwnershipTracking
  , testOwnershipConstraints
  , testOwnershipBorrowing
  , testOwnershipLifetimes
  , testOwnershipTransitivity
  , testOwnershipAnalysis
  -- , testProperty "transfer ownership changes owner" prop_transfer_ownership_changes_owner
--  , testProperty "single owner constraint" prop_single_owner_constraint
--  , testProperty "lifetime validity" prop_lifetime_validity
  ]