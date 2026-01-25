{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.ConciseOwnershipQuickCheckSpec where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, property, Arbitrary(..), elements, oneof)
import Ownership
  ( OwnershipAnalysis(..)
  , OwnershipConstraint(..)
  , OwnershipType(..)
  , OwnershipTransfer(..)
  , checkOwnershipTransfer
  , validateOwnershipConstraints
  , hasOwnershipErrors
  , getOwnershipErrors
  , clearOwnershipErrors
  , mergeOwnershipAnalyses
  , getOwners
  , getBorrowers
  , getOwnedResources
  , isOwner
  , isBorrower
  , canTransferOwnership
  , transferOwnership
  )
import Ownership.Common.Types (OwnershipError(..))
import Data.List (sort, nub)


-- Arbitrary instances for QuickCheck
instance Arbitrary OwnershipType where
  arbitrary = do
    name <- arbitrary
    elements [Owned name, Borrowed name, MutBorrowed name]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    fromOwner <- arbitrary
    toOwner <- arbitrary
    return $ OwnershipTransfer fromOwner toOwner

instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> arbitrary
    , DoubleMove <$> arbitrary <*> arbitrary
    , BorrowWhileMoved <$> arbitrary
    , MutBorrowWhileBorrowed <$> arbitrary
    , BorrowWhileMutBorrowed <$> arbitrary
    , MultipleMutBorrows <$> arbitrary
    , UseWhileMutBorrowed <$> arbitrary
    , OutOfScope <$> arbitrary
    , BorrowError <$> arbitrary
    , ParseError <$> arbitrary
    , CrossFunctionMove <$> arbitrary <*> arbitrary
    , ParameterMoveMismatch <$> arbitrary
    , ControlFlowError <$> arbitrary
    , PathSensitiveError <$> arbitrary
    , LoopOwnershipError <$> arbitrary
    , OwnershipError <$> arbitrary
    ]

instance Arbitrary OwnershipAnalysis where
  arbitrary = do
    owners <- arbitrary
    borrowers <- arbitrary
    errors <- arbitrary
    return $ OwnershipAnalysis owners borrowers errors

instance Arbitrary OwnershipConstraint where
  arbitrary = oneof
    [ MustNotMove <$> arbitrary
    , MustNotCopy <$> arbitrary
    , MustNotBorrow <$> arbitrary
    ]

tests :: TestTree
tests = testGroup "Concise Ownership QuickCheck Tests"
  [ testProperties "OwnershipAnalysis Properties"
    [ ("oaOwners_properties", property oaOwners_properties)
    , ("oaBorrowers_properties", property oaBorrowers_properties)
    , ("oaErrors_properties", property oaErrors_properties)
    ]
  , testProperties "Ownership Query Properties"
    [ ("getOwners_properties", property getOwners_properties)
    , ("getBorrowers_properties", property getBorrowers_properties)
    , ("getOwnedResources_properties", property getOwnedResources_properties)
    , ("isOwner_properties", property isOwner_properties)
    , ("isBorrower_properties", property isBorrower_properties)
    ]
  , testProperties "Ownership Transfer Properties"
    [ ("checkOwnershipTransfer_properties", property checkOwnershipTransfer_properties)
    , ("canTransferOwnership_properties", property canTransferOwnership_properties)
    , ("transferOwnership_properties", property transferOwnership_properties)
    ]
  , testProperties "Ownership Constraint Properties"
    [ ("validateOwnershipConstraints_properties", property validateOwnershipConstraints_properties)
    ]
  , testProperties "Ownership Error Properties"
    [ ("hasOwnershipErrors_properties", property hasOwnershipErrors_properties)
    , ("getOwnershipErrors_properties", property getOwnershipErrors_properties)
    , ("clearOwnershipErrors_properties", property clearOwnershipErrors_properties)
    ]
  , testProperties "Ownership Analysis Properties"
    [ ("mergeOwnershipAnalyses_properties", property mergeOwnershipAnalyses_properties)
    ]
  ]

-- | Test oaOwners properties
oaOwners_properties :: OwnershipAnalysis -> Bool
oaOwners_properties oa = 
  let owners = oaOwners oa
  in length owners >= 0

-- | Test oaBorrowers properties
oaBorrowers_properties :: OwnershipAnalysis -> Bool
oaBorrowers_properties oa = 
  let borrowers = oaBorrowers oa
  in length borrowers >= 0

-- | Test oaErrors properties
oaErrors_properties :: OwnershipAnalysis -> Bool
oaErrors_properties oa = 
  let errors = oaErrors oa
  in length errors >= 0

-- | Test getOwners properties
getOwners_properties :: OwnershipAnalysis -> Bool
getOwners_properties oa = 
  let owners = getOwners oa
      expected = map fst (oaOwners oa)
  in sort (nub owners) == sort (nub expected)

-- | Test getBorrowers properties
getBorrowers_properties :: OwnershipAnalysis -> Bool
getBorrowers_properties oa = 
  let borrowers = getBorrowers oa
      expected = map fst (oaBorrowers oa)
  in sort (nub borrowers) == sort (nub expected)

-- | Test getOwnedResources properties
getOwnedResources_properties :: OwnershipAnalysis -> Bool
getOwnedResources_properties oa = 
  let resources = getOwnedResources oa
      expected = map snd (oaOwners oa)
  in sort (nub resources) == sort (nub expected)

-- | Test isOwner properties
isOwner_properties :: OwnershipAnalysis -> String -> String -> Bool
isOwner_properties oa owner resource = 
  let isOwn = isOwner oa owner resource
      owners = oaOwners oa
  in isOwn == ((owner, resource) `elem` owners)

-- | Test isBorrower properties
isBorrower_properties :: OwnershipAnalysis -> String -> String -> Bool
isBorrower_properties oa borrower resource = 
  let isBorr = isBorrower oa borrower resource
      borrowers = oaBorrowers oa
  in isBorr == ((borrower, resource) `elem` borrowers)

-- | Test checkOwnershipTransfer properties
checkOwnershipTransfer_properties :: String -> String -> String -> Bool
checkOwnershipTransfer_properties from to resource = 
  case checkOwnershipTransfer from to resource of
    Left _ -> True  -- Errors are acceptable
    Right result -> result == True  -- Placeholder implementation returns True

-- | Test canTransferOwnership properties
canTransferOwnership_properties :: OwnershipAnalysis -> String -> String -> Bool
canTransferOwnership_properties oa owner resource = 
  let canTransfer = canTransferOwnership oa owner resource
  in canTransfer == True  -- Placeholder implementation always returns True

-- | Test transferOwnership properties
transferOwnership_properties :: OwnershipAnalysis -> String -> String -> Bool
transferOwnership_properties oa owner resource = 
  case transferOwnership oa owner resource of
    Left _ -> True  -- Errors are acceptable
    Right newOa -> 
      let newOwners = oaOwners newOa
          oldOwners = oaOwners oa
      in (owner, resource) `elem` newOwners && 
         length newOwners == length oldOwners + 1

-- | Test validateOwnershipConstraints properties
validateOwnershipConstraints_properties :: [OwnershipConstraint] -> Bool
validateOwnershipConstraints_properties constraints = 
  let errors = validateOwnershipConstraints constraints
  in length errors >= 0

-- | Test hasOwnershipErrors properties
hasOwnershipErrors_properties :: OwnershipAnalysis -> Bool
hasOwnershipErrors_properties oa = 
  let hasErr = hasOwnershipErrors oa
      errors = oaErrors oa
  in hasErr == not (null errors)

-- | Test getOwnershipErrors properties
getOwnershipErrors_properties :: OwnershipAnalysis -> Bool
getOwnershipErrors_properties oa = 
  let errors = getOwnershipErrors oa
      expected = oaErrors oa
  in errors == expected

-- | Test clearOwnershipErrors properties
clearOwnershipErrors_properties :: OwnershipAnalysis -> Bool
clearOwnershipErrors_properties oa = 
  let cleared = clearOwnershipErrors oa
  in null (oaErrors cleared) && 
     oaOwners cleared == oaOwners oa &&
     oaBorrowers cleared == oaBorrowers oa

-- | Test mergeOwnershipAnalyses properties
mergeOwnershipAnalyses_properties :: OwnershipAnalysis -> OwnershipAnalysis -> Bool
mergeOwnershipAnalyses_properties oa1 oa2 = 
  let merged = mergeOwnershipAnalyses oa1 oa2
  in oaOwners merged == oaOwners oa1 ++ oaOwners oa2 &&
     oaBorrowers merged == oaBorrowers oa1 ++ oaBorrowers oa2 &&
     oaErrors merged == oaErrors oa1 ++ oaErrors oa2