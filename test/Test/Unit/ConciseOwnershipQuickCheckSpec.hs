{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseOwnershipQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper, isDigit, isLetter)
import Data.Set (Set)
import qualified Data.Set as Set
import Ownership (OwnershipAnalysis, OwnershipError(..), OwnershipType(..), 
                 OwnershipTransfer(..), OwnershipConstraint(..),
                 analyzeOwnership, checkOwnershipTransfer, validateOwnershipConstraints,
                 hasOwnershipErrors, getOwnershipErrors, clearOwnershipErrors,
                 mergeOwnershipAnalyses, getOwners, getBorrowers, getOwnedResources,
                 isOwner, isBorrower, canTransferOwnership, transferOwnership)

-- Helper generators for Ownership tests
genOwnershipType :: Gen OwnershipType
genOwnershipType = elements [Owned, Borrowed, Shared, Unique]

genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  from <- elements ["owner1", "owner2", "owner3"]
  to <- elements ["recipient1", "recipient2", "recipient3"]
  resource <- elements ["resource1", "resource2", "resource3"]
  return $ OwnershipTransfer from to resource

genOwnershipConstraint :: Gen OwnershipConstraint
genOwnershipConstraint = do
  name <- elements ["constraint1", "constraint2", "constraint3"]
  description <- elements ["must be unique", "cannot be shared", "must be owned"]
  return $ OwnershipConstraint name description

genOwnershipError :: Gen OwnershipError
genOwnershipError = do
  msg <- elements ["Ownership violation", "Borrow checker error", "Transfer not allowed", "Constraint violation"]
  line <- choose (1, 100)
  col <- choose (1, 100)
  resource <- elements ["resource1", "resource2", "resource3"]
  return $ OwnershipError msg line col resource

genOwnershipAnalysis :: Gen OwnershipAnalysis
genOwnershipAnalysis = do
  numOwners <- choose (0, 3)
  numBorrowers <- choose (0, 3)
  numResources <- choose (0, 3)
  numErrors <- choose (0, 3)
  
  owners <- vectorOf numOwners $ elements ["owner1", "owner2", "owner3", "owner4"]
  borrowers <- vectorOf numBorrowers $ elements ["borrower1", "borrower2", "borrower3"]
  resources <- vectorOf numResources $ elements ["resource1", "resource2", "resource3", "resource4"]
  errors <- vectorOf numErrors genOwnershipError
  
  return $ OwnershipAnalysis 
    { owners = Set.fromList owners
    , borrowers = Set.fromList borrowers
    , ownedResources = Set.fromList resources
    , ownershipErrors = errors
    }

-- Test properties for Ownership module

-- Basic ownership analysis tests
prop_analyze_ownership_no_crash :: String -> Property
prop_analyze_ownership_no_crash code = 
  not (null code) ==>
  let result = analyzeOwnership code
  in case result of
       Left _ -> property True
       Right _ -> property True

prop_has_ownership_errors_detection :: OwnershipAnalysis -> Property
prop_has_ownership_errors_detection analysis = 
  let hasErrs = hasOwnershipErrors analysis
      hasErrs' = not (null (ownershipErrors analysis))
  in hasErrs === hasErrs'

prop_get_ownership_errors_returns_all :: OwnershipAnalysis -> Property
prop_get_ownership_errors_returns_all analysis = 
  let errs = getOwnershipErrors analysis
  in length errs === length (ownershipErrors analysis)

prop_clear_ownership_errors_removes_all :: OwnershipAnalysis -> Property
prop_clear_ownership_errors_removes_all analysis = 
  let cleared = clearOwnershipErrors analysis
  in null (ownershipErrors cleared)

-- Ownership transfer tests
prop_check_ownership_transfer_no_crash :: OwnershipTransfer -> OwnershipAnalysis -> Property
prop_check_ownership_transfer_no_crash transfer analysis = 
  let result = checkOwnershipTransfer transfer analysis
  in case result of
       Left _ -> property True
       Right _ -> property True

prop_can_transfer_ownership_consistency :: OwnershipTransfer -> OwnershipAnalysis -> Property
prop_can_transfer_ownership_consistency transfer analysis = 
  let canTransfer = canTransferOwnership transfer analysis
      checkResult = checkOwnershipTransfer transfer analysis
  in case checkResult of
       Left _ -> not canTransfer
       Right _ -> canTransfer

prop_transfer_ownership_updates_analysis :: OwnershipTransfer -> OwnershipAnalysis -> Property
prop_transfer_ownership_updates_analysis transfer analysis = 
  case checkOwnershipTransfer transfer analysis of
    Left _ -> property True
    Right _ -> 
      let updated = transferOwnership transfer analysis
          fromOwner = transferFrom transfer
          toOwner = transferTo transfer
          resource = transferResource transfer
      in isOwner toOwner updated && not (isOwner fromOwner updated)

-- Ownership constraint tests
prop_validate_ownership_constraints_no_crash :: [OwnershipConstraint] -> OwnershipAnalysis -> Property
prop_validate_ownership_constraints_no_crash constraints analysis = 
  let result = validateOwnershipConstraints constraints analysis
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Ownership query tests
prop_get_owners_returns_owners :: OwnershipAnalysis -> Property
prop_get_owners_returns_owners analysis = 
  let ownerSet = getOwners analysis
  in all (`Set.member` owners analysis) (Set.toList ownerSet)

prop_get_borrowers_returns_borrowers :: OwnershipAnalysis -> Property
prop_get_borrowers_returns_borrowers analysis = 
  let borrowerSet = getBorrowers analysis
  in all (`Set.member` borrowers analysis) (Set.toList borrowerSet)

prop_get_owned_resources_returns_resources :: OwnershipAnalysis -> Property
prop_get_owned_resources_returns_resources analysis = 
  let resourceSet = getOwnedResources analysis
  in all (`Set.member` ownedResources analysis) (Set.toList resourceSet)

prop_is_owner_detection :: String -> OwnershipAnalysis -> Property
prop_is_owner_detection name analysis = 
  let isOwn = isOwner name analysis
      isOwn' = name `Set.member` owners analysis
  in isOwn === isOwn'

prop_is_borrower_detection :: String -> OwnershipAnalysis -> Property
prop_is_borrower_detection name analysis = 
  let isBorrow = isBorrower name analysis
      isBorrow' = name `Set.member` borrowers analysis
  in isBorrow === isBorrow'

-- Ownership merging tests
prop_merge_ownership_analyses_combines_sets :: OwnershipAnalysis -> OwnershipAnalysis -> Property
prop_merge_ownership_analyses_combines_sets analysis1 analysis2 = 
  let merged = mergeOwnershipAnalyses analysis1 analysis2
  in owners merged === Set.union (owners analysis1) (owners analysis2) &&
     borrowers merged === Set.union (borrowers analysis1) (borrowers analysis2) &&
     ownedResources merged === Set.union (ownedResources analysis1) (ownedResources analysis2)

prop_merge_ownership_analyses_combines_errors :: OwnershipAnalysis -> OwnershipAnalysis -> Property
prop_merge_ownership_analyses_combines_errors analysis1 analysis2 = 
  let merged = mergeOwnershipAnalyses analysis1 analysis2
  in length (ownershipErrors merged) === length (ownershipErrors analysis1) + length (ownershipErrors analysis2)

-- Ownership type tests
prop_ownership_type_properties :: OwnershipType -> Property
prop_ownership_type_properties ownType = 
  case ownType of
    Owned -> property True
    Borrowed -> property True
    Shared -> property True
    Unique -> property True

tests :: TestTree
tests = testGroup "Concise Ownership QuickCheck Tests"
  [ testProperties "Basic Ownership Analysis Tests"
    [ ("analyze ownership no crash", prop_analyze_ownership_no_crash)
    , ("has ownership errors detection", prop_has_ownership_errors_detection)
    , ("get ownership errors returns all", prop_get_ownership_errors_returns_all)
    , ("clear ownership errors removes all", prop_clear_ownership_errors_removes_all)
    ]
  , testProperties "Ownership Transfer Tests"
    [ ("check ownership transfer no crash", prop_check_ownership_transfer_no_crash)
    , ("can transfer ownership consistency", prop_can_transfer_ownership_consistency)
    , ("transfer ownership updates analysis", prop_transfer_ownership_updates_analysis)
    ]
  , testProperties "Ownership Constraint Tests"
    [ ("validate ownership constraints no crash", prop_validate_ownership_constraints_no_crash)
    ]
  , testProperties "Ownership Query Tests"
    [ ("get owners returns owners", prop_get_owners_returns_owners)
    , ("get borrowers returns borrowers", prop_get_borrowers_returns_borrowers)
    , ("get owned resources returns resources", prop_get_owned_resources_returns_resources)
    , ("is owner detection", prop_is_owner_detection)
    , ("is borrower detection", prop_is_borrower_detection)
    ]
  , testProperties "Ownership Merging Tests"
    [ ("merge ownership analyses combines sets", prop_merge_ownership_analyses_combines_sets)
    , ("merge ownership analyses combines errors", prop_merge_ownership_analyses_combines_errors)
    ]
  , testProperties "Ownership Type Tests"
    [ ("ownership type properties", prop_ownership_type_properties)
    ]
  ]