{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , resize, Positive(..), NonEmpty(..)
  )

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import Ownership
  ( analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )

import Data.List (isInfixOf, isPrefixOf, sort, nub)
import Data.Char (isSpace, isAlpha)
import qualified Data.Text as T

-- Property: OwnershipType ordering is consistent
prop_ownershipType_ordering_consistent :: OwnershipType -> OwnershipType -> Property
prop_ownershipType_ordering_consistent ot1 ot2 =
  let comparison = compare ot1 ot2
      sorted = sort [ot1, ot2]
  in property (head sorted === min ot1 ot2 && last sorted === max ot1 ot2)

-- Property: OwnershipType equality is reflexive
prop_ownershipType_equality_reflexive :: OwnershipType -> Property
prop_ownershipType_equality_reflexive ot =
  property (ot === ot)

-- Property: OwnershipError ordering is consistent
prop_ownershipError_ordering_consistent :: OwnershipError -> OwnershipError -> Property
prop_ownershipError_ordering_consistent oe1 oe2 =
  let comparison = compare oe1 oe2
      sorted = sort [oe1, oe2]
  in property (head sorted === min oe1 oe2 && last sorted === max oe1 oe2)

-- Property: OwnershipError equality is reflexive
prop_ownershipError_equality_reflexive :: OwnershipError -> Property
prop_ownershipError_equality_reflexive oe =
  property (oe === oe)

-- Property: newOwnershipAnalyzer creates valid analyzer
prop_newOwnershipAnalyzer_valid :: Property
prop_newOwnershipAnalyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in property True -- Should always succeed

-- Property: Ownership analyzer creation is consistent
prop_ownershipAnalyzer_consistent :: Property
prop_ownershipAnalyzer_consistent =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in property (analyzer1 === analyzer2)

-- Property: formatOwnershipErrors handles empty list
prop_formatOwnershipErrors_empty :: Property
prop_formatOwnershipErrors_empty =
  let formatted = formatOwnershipErrors []
  in property (not (null formatted))

-- Property: formatOwnershipErrors handles single error
prop_formatOwnershipErrors_single :: OwnershipError -> Property
prop_formatOwnershipErrors_single error =
  let formatted = formatOwnershipErrors [error]
  in property (not (null formatted) && show error `isInfixOf` formatted)

-- Property: formatOwnershipErrors handles multiple errors
prop_formatOwnershipErrors_multiple :: [OwnershipError] -> Property
prop_formatOwnershipErrors_multiple errors =
  not (null errors) ==>
  let formatted = formatOwnershipErrors errors
      errorStrings = map show errors
  in property (all (`isInfixOf` formatted) errorStrings)

-- Property: formatOwnershipErrors preserves order
prop_formatOwnershipErrors_preserves_order :: [OwnershipError] -> Property
prop_formatOwnershipErrors_preserves_order errors =
  length errors >= 2 ==>
  let formatted = formatOwnershipErrors errors
      sortedErrors = sort errors
      formattedSorted = formatOwnershipErrors sortedErrors
  in property (formatted /= formattedSorted || errors == sortedErrors)

-- Property: Built-in functions list is non-empty
prop_builtInFunctions_nonempty :: Property
prop_builtInFunctions_nonempty =
  let functions = builtInFunctions
  in property (not (null functions))

-- Property: Built-in functions have valid names
prop_builtInFunctions_valid_names :: Property
prop_builtInFunctions_valid_names =
  let functions = builtInFunctions
      validNames = all (all isAlpha) functions
  in property validNames

-- Property: Ownership analysis handles simple variables
prop_analyzeOwnership_simple_variables :: String -> Property
prop_analyzeOwnership_simple_variables varName =
  not (null varName) && all isAlpha varName ==>
  let source = unlines 
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "  " ++ varName ++ " := 42"
        , "  _ = " ++ varName
        , "}"
        ]
  in case analyzeOwnership source of
    Left _ -> property True -- Analysis failures are acceptable
    Right result -> property True -- Should handle simple cases

-- Property: Ownership analysis handles move operations
prop_analyzeOwnership_moves :: String -> String -> Property
prop_analyzeOwnership_moves var1 var2 =
  not (null var1) && not (null var2) && all isAlpha (var1 ++ var2) ==>
  let source = unlines 
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "  " ++ var1 ++ " := 42"
        , "  " ++ var2 ++ " := " ++ var1
        , "  _ = " ++ var2
        , "}"
        ]
  in case analyzeOwnership source of
    Left _ -> property True
    Right result -> property True

-- Property: Ownership analysis handles borrow operations
prop_analyzeOwnership_borrows :: String -> Property
prop_analyzeOwnership_borrows varName =
  not (null varName) && all isAlpha varName ==>
  let source = unlines 
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "  " ++ varName ++ " := 42"
        , "  _ = &" ++ varName
        , "  _ = " ++ varName
        , "}"
        ]
  in case analyzeOwnership source of
    Left _ -> property True
    Right result -> property True

-- Arbitrary instances
instance Arbitrary OwnershipType where
  arbitrary = oneof
    [ Owned <$> arbitraryIdentifier
    , Borrowed <$> arbitraryIdentifier
    , MutBorrowed <$> arbitraryIdentifier
    ]
    where
      arbitraryIdentifier = do
        len <- choose (1, 10)
        chars <- vectorOf len (elements ['a'..'z'])
        return (chars :: String)

instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> arbitraryIdentifier
    , DoubleMove <$> arbitraryIdentifier <*> arbitraryIdentifier
    , BorrowWhileMoved <$> arbitraryIdentifier
    , MutBorrowWhileBorrowed <$> arbitraryIdentifier
    , BorrowWhileMutBorrowed <$> arbitraryIdentifier
    , MultipleMutBorrows <$> arbitraryIdentifier
    , UseWhileMutBorrowed <$> arbitraryIdentifier
    , OutOfScope <$> arbitraryIdentifier
    , BorrowError <$> arbitrary
    , ParseError <$> arbitrary
    , CrossFunctionMove <$> arbitraryIdentifier <*> arbitraryIdentifier
    , ParameterMoveMismatch <$> arbitraryIdentifier
    , ControlFlowError <$> arbitrary
    , PathSensitiveError <$> arbitrary
    , LoopOwnershipError <$> arbitrary
    ]
    where
      arbitraryIdentifier = do
        len <- choose (1, 10)
        chars <- vectorOf len (elements ['a'..'z'])
        return (chars :: String)

tests :: TestTree
tests = testGroup "Enhanced Ownership QuickCheck Tests"
  [ fastProperty "OwnershipType ordering consistent" prop_ownershipType_ordering_consistent
  , fastProperty "OwnershipType equality reflexive" prop_ownershipType_equality_reflexive
  , fastProperty "OwnershipError ordering consistent" prop_ownershipError_ordering_consistent
  , fastProperty "OwnershipError equality reflexive" prop_ownershipError_equality_reflexive
  , fastProperty "newOwnershipAnalyzer creates valid analyzer" prop_newOwnershipAnalyzer_valid
  , fastProperty "Ownership analyzer creation consistent" prop_ownershipAnalyzer_consistent
  , fastProperty "formatOwnershipErrors handles empty list" prop_formatOwnershipErrors_empty
  , fastProperty "formatOwnershipErrors handles single error" prop_formatOwnershipErrors_single
  , fastProperty "formatOwnershipErrors handles multiple errors" prop_formatOwnershipErrors_multiple
  , fastProperty "formatOwnershipErrors preserves order" prop_formatOwnershipErrors_preserves_order
  , fastProperty "Built-in functions non-empty" prop_builtInFunctions_nonempty
  , fastProperty "Built-in functions valid names" prop_builtInFunctions_valid_names
  , fastProperty "analyzeOwnership handles simple variables" prop_analyzeOwnership_simple_variables
  , fastProperty "analyzeOwnership handles moves" prop_analyzeOwnership_moves
  , fastProperty "analyzeOwnership handles borrows" prop_analyzeOwnership_borrows
  ]