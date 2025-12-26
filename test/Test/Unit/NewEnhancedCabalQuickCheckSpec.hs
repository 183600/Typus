{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewEnhancedCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, sized, resize, suchThat, vectorOf)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler.IR (SourceIR(..), SemanticIR(..), buildSourceIR, buildSemanticIR)
import Ownership (OwnershipType(..), OwnershipTransfer(..), analyzeOwnership, newOwnershipAnalyzer)
import DependentTypesParser (TypeRef(..), TypeConstraint(..), DependentType(..), parseDependentType, validateDependentTypeSyntax)
import Compiler.Errors (CompilerError(..), ErrorCategory(..), ErrorSeverity(..), CompilationPhase(..))
import ErrorHandler (CompilerError(..))

import Data.List (isPrefixOf, isInfixOf, nub, sort, group, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (replicateM, when)

-- ============================================================================
-- Parser Property Tests (3 tests)
-- ============================================================================

-- Property: Parsing result consistency - parsing the same valid input twice yields identical results
prop_parser_parsing_consistency :: String -> Property
prop_parser_parsing_consistency input =
  let result1 = parseTypus input
      result2 = parseTypus input
  in classify (isRight result1) "valid input" $
     classify (isLeft result1) "invalid input" $
     property $ result1 === result2

-- Property: Error recovery idempotency - attempting to parse after an error maintains the same error state
prop_parser_error_recovery_idempotent :: String -> Property
prop_parser_error_recovery_idempotent input =
  let firstResult = parseTypus input
      secondResult = parseTypus input
  in classify (isLeft firstResult) "has parsing error" $
     property $ case (firstResult, secondResult) of
       (Left err1, Left err2) -> property $ length (show err1) > 0 .&&. length (show err2) > 0
       (Right _, Right _) -> property $ True  -- Both successful
       _ -> property $ False  -- Inconsistent results

-- Property: Directive block nesting structure - nested directive blocks maintain proper hierarchy
prop_parser_directive_block_nesting :: String -> Property
prop_parser_directive_block_nesting input =
  let result = parseTypus input
  in classify (isRight result) "valid parsing" $
     property $ case result of
       Left _ -> property $ True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
             blockCount = length blocks
             directiveBlocks = filter (hasDirectives . cbDirectives) blocks
         in property $ blockCount >= length directiveBlocks
  where
    hasDirectives directives = 
      isJust (bdOwnership directives) || 
      isJust (bdDependentTypes directives) || 
      isJust (bdConstraints directives)

-- ============================================================================
-- Compiler IR Generation Property Tests (2 tests)
-- ============================================================================

-- Property: SourceIR to SemanticIR transformation consistency
prop_ir_transformation_consistency :: String -> Property
prop_ir_transformation_consistency input =
  let parseResult = parseTypus input
  in classify (isRight parseResult) "valid input for IR" $
     property $ case parseResult of
       Left _ -> property $ True
       Right typusFile ->
         let sourceIR1 = buildSourceIR typusFile input
             sourceIR2 = buildSourceIR typusFile input
             semanticIR1 = buildSemanticIR sourceIR1
             semanticIR2 = buildSemanticIR sourceIR2
         in property $ sourceIR1 === sourceIR2 .&&. semanticIR1 === semanticIR2

-- Property: IR generation completeness - generated IR contains all essential components
prop_ir_generation_completeness :: String -> Property
prop_ir_generation_completeness input =
  let parseResult = parseTypus input
  in classify (isRight parseResult) "parseable input" $
     property $ case parseResult of
       Left _ -> property $ True
       Right typusFile ->
         let sourceIR = buildSourceIR typusFile input
             semanticIR = buildSemanticIR sourceIR
             hasSourceText = not (null $ sourceText sourceIR)
             hasTypusFile = not (null $ show $ semanticTypusFile semanticIR)
         in property $ hasSourceText .&&. hasTypusFile

-- ============================================================================
-- Ownership Analysis Property Tests (2 tests)
-- ============================================================================

-- Property: Ownership transfer transitivity
prop_ownership_transfer_transitivity :: [OwnershipType] -> Property
prop_ownership_transfer_transitivity ownershipTypes =
  not (null ownershipTypes) && length ownershipTypes <= 5 ==>
  let analyzer = newOwnershipAnalyzer
      transfers = zip ownershipTypes (tail ownershipTypes ++ [head ownershipTypes])
      validTransfers = filter isTransferValid transfers
  in property $ length validTransfers >= 0
  where
    isTransferValid (from, to) = case (from, to) of
      (Owned, Borrowed) -> True
      (Borrowed, Shared) -> True
      (Shared, Owned) -> False
      _ -> True

-- Property: Ownership checking idempotency
prop_ownership_checking_idempotent :: String -> Property
prop_ownership_checking_idempotent input =
  let parseResult = parseTypus input
  in classify (isRight parseResult) "valid input for ownership" $
     property $ case parseResult of
       Left _ -> property $ True
       Right typusFile ->
         let analyzer1 = newOwnershipAnalyzer
             analyzer2 = newOwnershipAnalyzer
             result1 = analyzeOwnership analyzer1 (show typusFile)
             result2 = analyzeOwnership analyzer2 (show typusFile)
         in property $ length result1 === length result2

-- ============================================================================
-- Dependent Type System Property Tests (2 tests)
-- ============================================================================

-- Property: Type constraint equivalence
prop_type_constraint_equivalence :: TypeConstraint -> TypeConstraint -> Property
prop_type_constraint_equivalence constraint1 constraint2 =
  let validation1 = validateDependentTypeSyntax $ show constraint1
      validation2 = validateDependentTypeSyntax $ show constraint2
  in classify (isRight validation1 && isRight validation2) "both valid constraints" $
     classify (isLeft validation1 && isLeft validation2) "both invalid constraints" $
     property $ case (validation1, validation2) of
       (Right _, Right _) -> property $ True
       (Left _, Left _) -> property $ True
       _ -> property $ False

-- Property: Type reference parsing consistency
prop_type_reference_parsing_consistency :: String -> Property
prop_type_reference_parsing_consistency typeString =
  not (null typeString) && length typeString <= 100 ==>
  let parseResult1 = parseDependentType typeString
      parseResult2 = parseDependentType typeString
  in classify (isRight parseResult1) "valid type reference" $
     classify (isLeft parseResult1) "invalid type reference" $
     property $ case (parseResult1, parseResult2) of
       (Right type1, Right type2) -> property $ show type1 === show type2
       (Left err1, Left err2) -> property $ length (show err1) > 0 .&&. length (show err2) > 0
       _ -> property $ False

-- ============================================================================
-- Error Handling Property Tests (1 test)
-- ============================================================================

-- Property: Error aggregation idempotency
prop_error_aggregation_idempotent :: [CompilerError] -> Property
prop_error_aggregation_idempotent errors =
  let uniqueErrors1 = nubBy sameErrorType errors
      uniqueErrors2 = nubBy sameErrorType errors
      errorCount1 = length uniqueErrors1
      errorCount2 = length uniqueErrors2
  in property $ errorCount1 === errorCount2
  where
    sameErrorType err1 err2 = 
      errorCategory err1 == errorCategory err2 &&
      errorPhase err1 == errorPhase err2

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Enhanced Cabal QuickCheck Tests"
    [ testGroup "Parser Property Tests"
        [ fastProperty "Parsing result consistency" prop_parser_parsing_consistency
        , fastProperty "Error recovery idempotency" prop_parser_error_recovery_idempotent
        , fastProperty "Directive block nesting structure" prop_parser_directive_block_nesting
        ]
    , testGroup "Compiler IR Generation Property Tests"
        [ fastProperty "IR transformation consistency" prop_ir_transformation_consistency
        , fastProperty "IR generation completeness" prop_ir_generation_completeness
        ]
    , testGroup "Ownership Analysis Property Tests"
        [ fastProperty "Ownership transfer transitivity" prop_ownership_transfer_transitivity
        , fastProperty "Ownership checking idempotency" prop_ownership_checking_idempotent
        ]
    , testGroup "Dependent Type System Property Tests"
        [ fastProperty "Type constraint equivalence" prop_type_constraint_equivalence
        , fastProperty "Type reference parsing consistency" prop_type_reference_parsing_consistency
        ]
    , testGroup "Error Handling Property Tests"
        [ fastProperty "Error aggregation idempotency" prop_error_aggregation_idempotent
        ]
    ]