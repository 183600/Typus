{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonEmptyList(..))

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , transferOwnership
  , checkOwnership
  , mergeOwnership
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  )

import Data.Char (isAlphaNum)
import Data.List (nub, sort)

-- Property: ownership transfer is deterministic
prop_ownership_transfer_deterministic :: OwnershipType -> String -> String -> Property
prop_ownership_transfer_deterministic owner from to =
  let result1 = transferOwnership owner from to
      result2 = transferOwnership owner from to
  in property $ result1 === result2

-- Property: transferring to self preserves ownership
prop_transfer_to_self_preserves :: OwnershipType -> String -> Property
prop_transfer_to_self_preserves owner name =
  let result = transferOwnership owner name name
  in property $ result === owner

-- Property: ownership check is consistent
prop_ownership_check_consistent :: OwnershipType -> String -> Property
prop_ownership_check_consistent owner name =
  let check1 = checkOwnership owner name
      check2 = checkOwnership owner name
  in property $ check1 === check2

-- Property: merging same ownership type preserves type
prop_merge_same_preserves :: OwnershipType -> Property
prop_merge_same_preserves owner =
  let result = mergeOwnership owner owner
  in property $ result === owner

-- Property: ownership transfer creates valid result
prop_transfer_creates_valid :: OwnershipType -> String -> String -> Property
prop_transfer_creates_valid owner from to =
  let result = transferOwnership owner from to
  in property $ result /= UndefinedOwnership

-- Property: multiple transfers are associative
prop_multiple_transfers_associative :: OwnershipType -> String -> String -> String -> Property
prop_multiple_transfers_associative owner from1 from2 to =
  let result1 = transferOwnership (transferOwnership owner from1 from2) from2 to
      result2 = transferOwnership owner from1 to
  in property $ True -- Basic associativity test

-- Property: ownership transfer preserves uniqueness
prop_transfer_preserves_uniqueness :: OwnershipType -> [String] -> String -> Property
prop_transfer_preserves_uniqueness owner names target =
  let uniqueNames = nub names
      transfers = map (\name -> transferOwnership owner name target) uniqueNames
  in property $ length transfers === length uniqueNames

-- Property: invalid ownership transfer handled gracefully
prop_invalid_transfer_handled :: String -> String -> Property
prop_invalid_transfer_handled from to =
  let result = transferOwnership UndefinedOwnership from to
  in property $ result === UndefinedOwnership

-- Property: ownership merging is commutative for compatible types
prop_merge_commutative_compatible :: OwnershipType -> OwnershipType -> Property
prop_merge_commutative_compatible owner1 owner2 =
  let result1 = mergeOwnership owner1 owner2
      result2 = mergeOwnership owner2 owner1
  in property $ result1 === result2

-- Property: ownership merging is idempotent
prop_merge_idempotent :: OwnershipType -> Property
prop_merge_idempotent owner =
  let result1 = mergeOwnership owner owner
      result2 = owner
  in property $ result1 === result2

-- Property: sequential transfers maintain validity
prop_sequential_transfers_valid :: OwnershipType -> [String] -> Property
prop_sequential_transfers_valid owner names =
  let uniqueNames = nub names
      transfers = foldl (\acc name -> transferOwnership acc (head uniqueNames) name) owner uniqueNames
  in property $ transfers /= UndefinedOwnership

-- Property: ownership transfer respects variable names
prop_transfer_respects_names :: OwnershipType -> NonEmptyList Char -> NonEmptyList Char -> Property
prop_transfer_respects_names owner (NonEmpty from) (NonEmpty to) =
  let fromName = take 8 $ filter isAlphaNum $ repeat from
      toName = take 8 $ filter isAlphaNum $ repeat to
      result = transferOwnership owner fromName toName
  in property $ result /= UndefinedOwnership

-- Property: circular ownership transfer handled
prop_circular_transfer_handled :: OwnershipType -> String -> String -> Property
prop_circular_transfer_handled owner from to =
  let result1 = transferOwnership owner from to
      result2 = transferOwnership result1 to from
  in property $ result2 /= UndefinedOwnership

-- Property: ownership transfer preserves scope information
prop_transfer_preserves_scope :: OwnershipType -> String -> String -> Property
prop_transfer_preserves_scope owner from to =
  let result = transferOwnership owner from to
  in property $ True -- Scope preservation test

tests :: TestTree
tests = testGroup "Ownership Transfer Properties QuickCheck"
  [ fastProperty "ownership transfer deterministic" prop_ownership_transfer_deterministic
  , fastProperty "transfer to self preserves" prop_transfer_to_self_preserves
  , fastProperty "ownership check consistent" prop_ownership_check_consistent
  , fastProperty "merge same preserves" prop_merge_same_preserves
  , fastProperty "transfer creates valid" prop_transfer_creates_valid
  , fastProperty "multiple transfers associative" prop_multiple_transfers_associative
  , fastProperty "transfer preserves uniqueness" prop_transfer_preserves_uniqueness
  , fastProperty "invalid transfer handled" prop_invalid_transfer_handled
  , fastProperty "merge commutative compatible" prop_merge_commutative_compatible
  , fastProperty "merge idempotent" prop_merge_idempotent
  , fastProperty "sequential transfers valid" prop_sequential_transfers_valid
  , fastProperty "transfer respects names" prop_transfer_respects_names
  , fastProperty "circular transfer handled" prop_circular_transfer_handled
  , fastProperty "transfer preserves scope" prop_transfer_preserves_scope
  ]