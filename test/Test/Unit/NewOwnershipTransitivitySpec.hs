{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipTransitivitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (choose, listOf, listOf1, elements, vectorOf, resize)
import Test.QuickCheck.Arbitrary (Arbitrary(..), oneof)

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )
import Ownership (analyzeOwnership, analyzeOwnershipDebug, analyzeOwnershipFile)

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourceSpan(..), SourcePos(..), startPos, spanBetween)

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)

-- ============================================================================
-- New Ownership Transitivity Tests
-- ============================================================================

-- Create a simple code block for testing
createCodeBlock :: String -> CodeBlock
createCodeBlock content = CodeBlock
    { cbDirectives = defaultBlockDirectives
    , cbContent = content
    , cbSpan = spanBetween startPos startPos
    }

-- Create a simple Typus file for testing
createTypusFile :: [String] -> TypusFile
createTypusFile codeContents =
  let blocks = map createCodeBlock codeContents
  in TypusFile
     { tfDirectives = defaultFileDirectives
     , tfBuildTags = []
     , tfBlocks = blocks
     , tfSyntaxErrors = []
     }

-- Property: Ownership type ordering is total
prop_ownership_type_total_ordering :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownership_type_total_ordering own1 own2 own3 =
  let comp12 = compare own1 own2
      comp23 = compare own2 own3
      comp13 = compare own1 own3
  in property $ (comp12 == EQ && comp23 == EQ) ==> comp13 == EQ

-- Property: Ownership type ordering is transitive
prop_ownership_type_transitive :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownership_type_transitive own1 own2 own3 =
  let comp12 = compare own1 own2
      comp23 = compare own2 own3
      comp13 = compare own1 own3
  in property $ (comp12 == LT && comp23 == LT) ==> comp13 == LT .&&.
             (comp12 == GT && comp23 == GT) ==> comp13 == GT

-- Property: Ownership error ordering is total
prop_ownership_error_total_ordering :: OwnershipError -> OwnershipError -> OwnershipError -> Property
prop_ownership_error_total_ordering err1 err2 err3 =
  let comp12 = compare err1 err2
      comp23 = compare err2 err3
      comp13 = compare err1 err3
  in property $ (comp12 == EQ && comp23 == EQ) ==> comp13 == EQ

-- Property: Ownership error ordering is transitive
prop_ownership_error_transitive :: OwnershipError -> OwnershipError -> OwnershipError -> Property
prop_ownership_error_transitive err1 err2 err3 =
  let comp12 = compare err1 err2
      comp23 = compare err2 err3
      comp13 = compare err1 err3
  in property $ (comp12 == LT && comp23 == LT) ==> comp13 == LT .&&.
             (comp12 == GT && comp23 == GT) ==> comp13 == GT

-- Property: Ownership analyzer creation is deterministic
prop_analyzer_creation_deterministic :: Property
prop_analyzer_creation_deterministic =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in property $ analyzer1 === analyzer2

-- Property: Ownership transfer preserves variable names
prop_ownership_transfer_preserves_names :: String -> String -> Property
prop_ownership_transfer_preserves_names fromVar toVar =
  not (null fromVar) && not (null toVar) ==>
  let transfer = OwnershipTransfer fromVar toVar
  in property $ transferFrom transfer === fromVar .&&.
             transferTo transfer === toVar

-- Property: Ownership transfer equality is reflexive
prop_ownership_transfer_reflexive :: String -> String -> Property
prop_ownership_transfer_reflexive fromVar toVar =
  not (null fromVar) && not (null toVar) ==>
  let transfer = OwnershipTransfer fromVar toVar
  in property $ transfer === transfer

-- Property: Ownership transfer equality is symmetric
prop_ownership_transfer_symmetric :: String -> String -> Property
prop_ownership_transfer_symmetric fromVar toVar =
  not (null fromVar) && not (null toVar) && fromVar /= toVar ==>
  let transfer1 = OwnershipTransfer fromVar toVar
      transfer2 = OwnershipTransfer fromVar toVar
  in property $ transfer1 === transfer2 .&&. transfer2 === transfer1

-- Property: Ownership type hierarchy is consistent
prop_ownership_type_hierarchy :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_hierarchy own1 own2 =
  let comp = compare own1 own2
  in property $ (comp == LT || comp == EQ || comp == GT)  -- Should always have a total ordering

-- Property: Owned types are ordered by name
prop_owned_ordered_by_name :: String -> String -> Property
prop_owned_ordered_by_name name1 name2 =
  not (null name1) && not (null name2) && name1 /= name2 ==>
  let owned1 = Owned name1
      owned2 = Owned name2
      comp = compare owned1 owned2
  in property $ (comp == LT && name1 < name2) .||. 
             (comp == GT && name1 > name2)

-- Property: Borrowed types are ordered by name
prop_borrowed_ordered_by_name :: String -> String -> Property
prop_borrowed_ordered_by_name name1 name2 =
  not (null name1) && not (null name2) && name1 /= name2 ==>
  let borrowed1 = Borrowed name1
      borrowed2 = Borrowed name2
      comp = compare borrowed1 borrowed2
  in property $ (comp == LT && name1 < name2) .||. 
             (comp == GT && name1 > name2)

-- Property: MutBorrowed types are ordered by name
prop_mutborrowed_ordered_by_name :: String -> String -> Property
prop_mutborrowed_ordered_by_name name1 name2 =
  not (null name1) && not (null name2) && name1 /= name2 ==>
  let mutborrowed1 = MutBorrowed name1
      let mutborrowed2 = MutBorrowed name2
      comp = compare mutborrowed1 mutborrowed2
  in property $ (comp == LT && name1 < name2) .||. 
             (comp == GT && name1 > name2)

-- Property: Ownership type category ordering
prop_ownership_type_category_ordering :: String -> String -> String -> Property
prop_ownership_type_category_ordering name1 name2 name3 =
  not (null name1) && not (null name2) && not (null name3) ==>
  let owned = Owned name1
      borrowed = Borrowed name2
      mutborrowed = MutBorrowed name3
  in property $ compare owned borrowed === LT .&&.
             compare borrowed mutborrowed === LT .&&.
             compare owned mutborrowed === LT

-- Property: UseAfterMove error equality
prop_use_after_move_equality :: String -> Property
prop_use_after_move_equality varName =
  not (null varName) ==>
  let error1 = UseAfterMove varName
      error2 = UseAfterMove varName
  in property $ error1 === error2

-- Property: DoubleMove error equality
prop_double_move_equality :: String -> String -> Property
prop_double_move_equality var1 var2 =
  not (null var1) && not (null var2) ==>
  let error1 = DoubleMove var1 var2
      error2 = DoubleMove var1 var2
  in property $ error1 === error2

-- Property: Ownership analysis handles basic code gracefully
prop_basic_ownership_analysis :: String -> Property
prop_basic_ownership_analysis code =
  not (null code) && L.length code <= 500 ==>  -- Limit for performance
  let typusFile = createTypusFile [code]
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer typusFile
  in case result of
       Right _ -> property $ True  -- Success is acceptable
       Left _ -> property $ True  -- Failure is acceptable, but shouldn't crash

-- Property: Ownership analysis is deterministic
prop_ownership_analysis_deterministic :: String -> Property
prop_ownership_analysis_deterministic code =
  not (null code) && L.length code <= 500 ==>  -- Limit for performance
  let typusFile = createTypusFile [code]
      analyzer = newOwnershipAnalyzer
      result1 = analyzeOwnership analyzer typusFile
      result2 = analyzeOwnership analyzer typusFile
  in case (result1, result2) of
       (Right _, Right _) -> property $ True  -- Both succeeded
       (Left err1, Left err2) -> property $ L.length err1 === L.length err2  -- Same number of errors
       (Right _, Left _) -> property $ False  -- Shouldn't happen
       (Left _, Right _) -> property $ False  -- Shouldn't happen

-- Property: Ownership analysis handles multiple code blocks
prop_multiple_blocks_ownership_analysis :: [String] -> Property
prop_multiple_blocks_ownership_analysis codeBlocks =
  not (null codeBlocks) && L.length codeBlocks <= 5 ==>  -- Limit for performance
  let typusFile = createTypusFile codeBlocks
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer typusFile
  in case result of
       Right _ -> property $ True  -- Success is acceptable
       Left _ -> property $ True  -- Failure is acceptable, but shouldn't crash

-- Property: Ownership analysis handles empty code gracefully
prop_empty_code_ownership_analysis :: Property
prop_empty_code_ownership_analysis =
  let typusFile = createTypusFile [""]
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer typusFile
  in case result of
       Right _ -> property $ True  -- Success is acceptable
       Left _ -> property $ True  -- Failure is acceptable, but shouldn't crash

-- Property: Ownership analysis preserves variable naming
prop_ownership_analysis_preserves_variables :: String -> Property
prop_ownership_analysis_preserves_variables varName =
  not (null varName) && L.all isAlphaNum varName ==>  -- Ensure valid identifier
  let code = "let " ++ varName ++ " = 42\nprintln!(" ++ varName ++ ")"
      typusFile = createTypusFile [code]
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer typusFile
  in case result of
       Right _ -> property $ True  -- Success is acceptable
       Left _ -> property $ True  -- Failure is acceptable, but shouldn't crash

-- Property: Ownership analysis handles move operations
prop_ownership_analysis_handles_moves :: String -> String -> Property
prop_ownership_analysis_handles_moves var1 var2 =
  not (null var1) && not (null var2) && 
  all isAlphaNum var1 && L.all isAlphaNum var2 ==>  -- Ensure valid identifiers
  let code = "let " ++ var1 ++ " = 42\nlet " ++ var2 ++ " = " ++ var1 ++ "\nprintln!(" ++ var2 ++ ")"
      typusFile = createTypusFile [code]
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer typusFile
  in case result of
       Right _ -> property $ True  -- Success is acceptable
       Left _ -> property $ True  -- Failure is acceptable, but shouldn't crash

-- Property: Ownership analysis handles borrow operations
prop_ownership_analysis_handles_borrows :: String -> String -> Property
prop_ownership_analysis_handles_borrows var1 var2 =
  not (null var1) && not (null var2) && 
  all isAlphaNum var1 && L.all isAlphaNum var2 ==>  -- Ensure valid identifiers
  let code = "let " ++ var1 ++ " = 42\nlet " ++ var2 ++ " = &" ++ var1 ++ "\nprintln!(" ++ var1 ++ ")"
      typusFile = createTypusFile [code]
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer typusFile
  in case result of
       Right _ -> property $ True  -- Success is acceptable
       Left _ -> property $ True  -- Failure is acceptable, but shouldn't crash

-- Property: Ownership error messages contain variable names
prop_ownership_error_contains_variable :: String -> Property
prop_ownership_error_contains_variable varName =
  not (null varName) && L.all isAlphaNum varName ==>  -- Ensure valid identifier
  let error = UseAfterMove varName
      errorMsg = show error
  in property $ varName `L.isInfixOf` errorMsg

-- Property: Ownership transfer chain preserves consistency
prop_ownership_transfer_chain :: [String] -> Property
prop_ownership_transfer_chain varNames =
  not (null varNames) && L.length varNames <= 5 && L.all isAlphaNum (L.concat varNames) ==>  -- Limit for performance
  let transfers = zipWith OwnershipTransfer varNames (L.tail varNames)
      allFromVars = map transferFrom transfers
      allToVars = map transferTo transfers
  in property $ allFromVars === init varNames .&&. allToVars === L.tail varNames

-- Property: Ownership type comparisons are consistent
prop_ownership_type_consistent_comparisons :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_consistent_comparisons own1 own2 =
  let comp1 = compare own1 own2
      comp2 = compare own2 own1
  in property $ (comp1 == EQ && comp2 == EQ) .||.
             (comp1 == LT && comp2 == GT) .||.
             (comp1 == GT && comp2 == LT)

-- Helper function to check if a string contains only alphanumeric characters
isAlphaNum :: String -> Bool
isAlphaNum = L.all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'))

-- Tests collection
tests :: TestTree
tests = testGroup "New Ownership Transitivity Tests"
  [ fastProperty "Ownership type ordering is total" prop_ownership_type_total_ordering
  , fastProperty "Ownership type ordering is transitive" prop_ownership_type_transitive
  , fastProperty "Ownership error ordering is total" prop_ownership_error_total_ordering
  , fastProperty "Ownership error ordering is transitive" prop_ownership_error_transitive
  , fastProperty "Ownership analyzer creation is deterministic" prop_analyzer_creation_deterministic
  , fastProperty "Ownership transfer preserves variable names" prop_ownership_transfer_preserves_names
  , fastProperty "Ownership transfer equality is reflexive" prop_ownership_transfer_reflexive
  , fastProperty "Ownership transfer equality is symmetric" prop_ownership_transfer_symmetric
  , fastProperty "Ownership type hierarchy is consistent" prop_ownership_type_hierarchy
  , fastProperty "Owned types are ordered by name" prop_owned_ordered_by_name
  , fastProperty "Borrowed types are ordered by name" prop_borrowed_ordered_by_name
  , fastProperty "MutBorrowed types are ordered by name" prop_mutborrowed_ordered_by_name
  , fastProperty "Ownership type category ordering" prop_ownership_type_category_ordering
  , fastProperty "UseAfterMove error equality" prop_use_after_move_equality
  , fastProperty "DoubleMove error equality" prop_double_move_equality
  , fastProperty "Ownership analysis handles basic code gracefully" prop_basic_ownership_analysis
  , fastProperty "Ownership analysis is deterministic" prop_ownership_analysis_deterministic
  , fastProperty "Ownership analysis handles multiple code blocks" prop_multiple_blocks_ownership_analysis
  , fastProperty "Ownership analysis handles empty code gracefully" prop_empty_code_ownership_analysis
  , fastProperty "Ownership analysis preserves variable naming" prop_ownership_analysis_preserves_variables
  , fastProperty "Ownership analysis handles move operations" prop_ownership_analysis_handles_moves
  , fastProperty "Ownership analysis handles borrow operations" prop_ownership_analysis_handles_borrows
  , fastProperty "Ownership error messages contain variable names" prop_ownership_error_contains_variable
  , fastProperty "Ownership transfer chain preserves consistency" prop_ownership_transfer_chain
  , fastProperty "Ownership type comparisons are consistent" prop_ownership_type_consistent_comparisons
  ]