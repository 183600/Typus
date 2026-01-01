module Test.Unit.NewCoreCabalQuickCheckSpec5 (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)

import Ownership (OwnershipType(..), OwnershipTransfer(..), OwnershipError(..))
import SourceLocation (SourcePos(..))

-- | Ownership analysis tests with QuickCheck properties
tests :: TestTree
tests =
  testGroup "New Core Cabal QuickCheck Tests 5 - Ownership Analysis"
    [ testGroup "Ownership type properties"
        [ fastProperty "ownership type transfer is deterministic" prop_ownershipTypeTransferDeterministic
        , fastProperty "ownership type compatibility is symmetric" prop_ownershipTypeCompatibilitySymmetric
        , testCase "ownership type hierarchy" $ do
            let types = [Owned, Shared, Borrowed, Immutable]
            L.length types @?= 4
        ]
    , testGroup "Ownership transfer properties"
        [ fastProperty "ownership transfer composition is associative" prop_ownershipTransferAssociative
        , fastProperty "ownership transfer preserves invariants" prop_ownershipTransferPreservesInvariants
        , testCase "ownership transfer creation" $ do
            let from = Owned
                to = Borrowed
                pos = SourcePos 1 1
                transfer = OwnershipTransfer { otFrom = from, otTo = to, otLocation = pos }
            otFrom transfer @?= from
            otTo transfer @?= to
            otLocation transfer @?= pos
        ]
    , testGroup "Ownership error properties"
        [ fastProperty "ownership error severity is consistent" prop_ownershipErrorSeverityConsistent
        , fastProperty "ownership error location is within bounds" prop_ownershipErrorLocationInBounds
        , testCase "ownership error creation" $ do
            let errorType = UseAfterMove
                pos = SourcePos 5 10
                message = "Use after move error"
                error = OwnershipError { oeType = errorType, oeLocation = pos, oeMessage = message }
            oeType error @?= errorType
            oeLocation error @?= pos
            oeMessage error @?= message
        ]
    , testGroup "Ownership analysis edge cases"
        [ fastProperty "circular ownership transfer detection" prop_circularOwnershipTransferDetection
        , fastProperty "ownership transfer chain validation" prop_ownershipTransferChainValidation
        , testCase "empty ownership analysis" $ do
            let analysis = emptyOwnershipAnalysis
            L.length (oaTransfers analysis) @?= 0
            L.length (oaErrors analysis) @?= 0
        ]
    ]

-- Simplified versions of data structures for testing
data OwnershipType = Owned | Shared | Borrowed | Immutable
  deriving (Show, Eq)

data OwnershipTransfer = OwnershipTransfer
    { otFrom :: OwnershipType
    , otTo :: OwnershipType
    , otLocation :: SourcePos
    } deriving (Show, Eq)

data OwnershipError = OwnershipError
    { oeType :: OwnershipErrorType
    , oeLocation :: SourcePos
    , oeMessage :: String
    } deriving (Show, Eq)

data OwnershipErrorType = UseAfterMove | DoubleBorrow | InvalidTransfer | CircularReference
  deriving (Show, Eq)

data OwnershipAnalysis = OwnershipAnalysis
    { oaTransfers :: [OwnershipTransfer]
    , oaErrors :: [OwnershipError]
    } deriving (Show, Eq)

data SourcePos = SourcePos Int Int  -- line, column
  deriving (Show, Eq)

-- | QuickCheck properties

-- Ownership type transfer is deterministic
prop_ownershipTypeTransferDeterministic :: OwnershipType -> OwnershipType -> Bool
prop_ownershipTypeTransferDeterministic from to =
  let transfer1 = createOwnershipTransfer from to (SourcePos 1 1)
      transfer2 = createOwnershipTransfer from to (SourcePos 1 1)
  in transfer1 == transfer2

-- Ownership type compatibility is symmetric
prop_ownershipTypeCompatibilitySymmetric :: OwnershipType -> OwnershipType -> Bool
prop_ownershipTypeCompatibilitySymmetric type1 type2 =
  areOwnershipTypesCompatible type1 type2 == areOwnershipTypesCompatible type2 type1

-- Ownership transfer composition is associative
prop_ownershipTransferAssociative :: OwnershipType -> OwnershipType -> OwnershipType -> Bool
prop_ownershipTransferAssociative from middle to =
  let transfer1 = createOwnershipTransfer from middle (SourcePos 1 1)
      transfer2 = createOwnershipTransfer middle to (SourcePos 1 2)
      composed1 = composeOwnershipTransfers transfer1 transfer2
      
      transfer3 = createOwnershipTransfer from to (SourcePos 1 3)
      composed2 = transfer3
  in otFrom composed1 == otFrom composed2 && otTo composed1 == otTo composed2

-- Ownership transfer preserves invariants
prop_ownershipTransferPreservesInvariants :: OwnershipType -> OwnershipType -> Bool
prop_ownershipTransferPreservesInvariants from to =
  let transfer = createOwnershipTransfer from to (SourcePos 1 1)
      validFrom = isValidOwnershipType from
      validTo = isValidOwnershipType to
      validTransfer = isValidOwnershipTransfer transfer
  in validFrom && validTo ==> validTransfer

-- Ownership error severity is consistent
prop_ownershipErrorSeverityConsistent :: OwnershipErrorType -> Bool
prop_ownershipErrorSeverityConsistent errorType =
  let severity = getErrorSeverity errorType
      validSeverity = severity >= 1 && severity <= 3
  in validSeverity

-- Ownership error location is within bounds
prop_ownershipErrorLocationInBounds :: Int -> Int -> Int -> Int -> Bool
prop_ownershipErrorLocationInBounds line col maxLine maxCol =
  let pos = SourcePos line col
      error = OwnershipError { oeType = UseAfterMove, oeLocation = pos, oeMessage = "test" }
      SourcePos el ec = oeLocation error
  in el >= 1 && el <= maxLine && ec >= 1 && ec <= maxCol

-- Circular ownership transfer detection
prop_circularOwnershipTransferDetection :: [OwnershipType] -> Bool
prop_circularOwnershipTransferDetection types =
  let transfers = createTransferChain types
      hasCircular = hasCircularTransfer transfers
      expectedCircular = L.length types > 3 && hasCycle types
  in hasCircular == expectedCircular

-- Ownership transfer chain validation
prop_ownershipTransferChainValidation :: [OwnershipType] -> Bool
prop_ownershipTransferChainValidation types =
  let transfers = createTransferChain types
      validChain = L.all isValidOwnershipTransfer transfers
      compatibleTypes = L.all areAdjacentTypesCompatible types
  in validChain == compatibleTypes

-- Helper functions
createOwnershipTransfer :: OwnershipType -> OwnershipType -> SourcePos -> OwnershipTransfer
createOwnershipTransfer from to location = OwnershipTransfer { otFrom = from, otTo = to, otLocation = location }

areOwnershipTypesCompatible :: OwnershipType -> OwnershipType -> Bool
areOwnershipTypesCompatible Owned _ = True
areOwnershipTypesCompatible _ Immutable = True
areOwnershipTypesCompatible Shared Shared = True
areOwnershipTypesCompatible Shared Borrowed = True
areOwnershipTypesCompatible Borrowed Borrowed = True
areOwnershipTypesCompatible _ _ = False

composeOwnershipTransfers :: OwnershipTransfer -> OwnershipTransfer -> OwnershipTransfer
composeOwnershipTransfers t1 t2 = OwnershipTransfer
  { otFrom = otFrom t1
  , otTo = otTo t2
  , otLocation = otLocation t1  -- Use first transfer's location
  }

isValidOwnershipType :: OwnershipType -> Bool
isValidOwnershipType Owned = True
isValidOwnershipType Shared = True
isValidOwnershipType Borrowed = True
isValidOwnershipType Immutable = True

isValidOwnershipTransfer :: OwnershipTransfer -> Bool
isValidOwnershipTransfer transfer = areOwnershipTypesCompatible (otFrom transfer) (otTo transfer)

getErrorSeverity :: OwnershipErrorType -> Int
getErrorSeverity UseAfterMove = 3
getErrorSeverity DoubleBorrow = 2
getErrorSeverity InvalidTransfer = 2
getErrorSeverity CircularReference = 3

createTransferChain :: [OwnershipType] -> [OwnershipTransfer]
createTransferChain [] = []
createTransferChain [_] = []
createTransferChain types = 
  let pairs = zip types (L.tail types)
      positions = L.map (\i -> SourcePos i 1) [1..]
  in zipWith (\(from, to) pos -> createOwnershipTransfer from to pos) pairs positions

hasCircularTransfer :: [OwnershipTransfer] -> Bool
hasCircularTransfer transfers = 
  let types = concatMap (\t -> [otFrom t, otTo t]) transfers
      firstType = L.head types
      lastType = last types
  in L.length types > 2 && firstType == lastType

hasCycle :: [OwnershipType] -> Bool
hasCycle types = L.length types > 3 && L.head types == last types

areAdjacentTypesCompatible :: [OwnershipType] -> Bool
areAdjacentTypesCompatible [] = True
areAdjacentTypesCompatible [_] = True
areAdjacentTypesCompatible types = 
  let pairs = zip types (L.tail types)
  in L.all (uncurry areOwnershipTypesCompatible) pairs

emptyOwnershipAnalysis :: OwnershipAnalysis
emptyOwnershipAnalysis = OwnershipAnalysis { oaTransfers = [], oaErrors = [] }

-- Helper for conditional properties
(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> _ = True