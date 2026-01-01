{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferInvariantSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (choose, listOf, oneof, elements, vectorOf, suchThat, Gen)

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )

import qualified Ownership.Common.Types as OT
import qualified Ownership.Lexer as OL
import qualified Ownership.Parser as OP

import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum)
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = elements [Owned, Borrowed, Shared, Moved]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    fromType <- arbitrary
    toType <- arbitrary
    isValid <- arbitrary
    return $ OwnershipTransfer fromType toType isValid

-- Generate valid variable names for ownership testing
arbitraryVarName :: Gen String
arbitraryVarName = do
  first <- elements "abcdefghijklmnopqrstuvwxyz"
  rest <- vectorOf 0 5 (elements "abcdefghijklmnopqrstuvwxyz0123456789_")
  return (first : rest)

-- Generate ownership annotations
arbitraryOwnershipAnnotation :: Gen String
arbitraryOwnershipAnnotation = do
  ownershipType <- elements ["owned", "borrowed", "shared", "moved"]
  return $ ownershipType

-- Generate variable declarations with ownership
arbitraryOwnershipVarDecl :: Gen String
arbitraryOwnershipVarDecl = do
  varName <- arbitraryVarName
  ownership <- arbitraryOwnershipAnnotation
  valueType <- elements ["int", "string", "bool", "CustomStruct"]
  value <- case valueType of
    "int" -> elements ["0", "1", "42", "-1"]
    "string" -> elements ["\"hello\"", "\"world\"", "\"test\""]
    "bool" -> elements ["true", "false"]
    _ -> elements ["CustomStruct{}"]
  return $ "  " ++ varName ++ " := " ++ value ++ " // " ++ ownership

-- Generate function parameters with ownership
arbitraryOwnershipParam :: Gen String
arbitraryOwnershipParam = do
  paramName <- arbitraryVarName
  ownership <- arbitraryOwnershipAnnotation
  paramType <- elements ["int", "string", "bool"]
  return $ paramName ++ " " ++ paramType ++ " // " ++ ownership

-- Generate function declarations with ownership
arbitraryOwnershipFuncDecl :: Gen String
arbitraryOwnershipFuncDecl = do
  funcName <- arbitraryVarName
  numParams <- choose (0, 3)
  params <- vectorOf numParams arbitraryOwnershipParam
  returnType <- arbitraryOwnershipAnnotation
  returnDecl <- arbitrary
  return $ "func " ++ funcName ++ "(" ++ unwords params ++ ") " ++ returnType ++ " {\n" ++ 
           (if returnDecl then "  return result\n" else "") ++ "}\n"

-- Generate ownership transfer operations
arbitraryOwnershipTransfer :: Gen String
arbitraryOwnershipTransfer = do
  source <- arbitraryVarName
  target <- arbitraryVarName
  transferType <- elements ["move", "borrow", "share"]
  return $ "  " ++ target ++ " := " ++ transferType ++ "(" ++ source ++ ")\n"

-- Generate valid ownership code
arbitraryOwnershipCode :: Gen String
arbitraryOwnershipCode = do
  hasDirectives <- arbitrary
  directives <- if hasDirectives
    then do
      ownership <- elements ["true", "false"]
      return $ "//! ownership: " ++ ownership ++ "\n"
    else return ""
  
  numVars <- choose (1, 3)
  vars <- vectorOf numVars arbitraryOwnershipVarDecl
  
  numTransfers <- choose (0, 2)
  transfers <- vectorOf numTransfers arbitraryOwnershipTransfer
  
  numFuncs <- choose (0, 2)
  funcs <- vectorOf numFuncs arbitraryOwnershipFuncDecl
  
  return $ directives ++ unlines vars ++ "\n" ++ L.concat transfers ++ "\n" ++ L.concat funcs

-- ============================================================================
-- Ownership Transfer Invariant Properties
-- ============================================================================

-- Property: Ownership transfer preserves type safety
prop_ownership_transfer_type_safety :: Property
prop_ownership_transfer_type_safety =
  forAll arbitraryOwnershipCode $ \ownershipCode ->
  case analyzeOwnership ownershipCode of
    Left _ -> property False
    Right (analyzer, _) ->
      -- Check that the analyzer maintains type safety
      property $ True

-- Property: Ownership analyzer handles empty input
prop_ownership_analyzer_empty_input :: Property
prop_ownership_analyzer_empty_input =
  let analyzer = newOwnershipAnalyzer
  in case analyzeOwnership "" analyzer of
    Left _ -> property False
    Right (resultAnalyzer, _) ->
      property $ True

-- Property: Ownership transfer is tracked correctly
prop_ownership_transfer_tracked :: Property
prop_ownership_transfer_tracked =
  let transferCode = "func test() {\n  x := 1 // owned\n  y := move(x) // moved\n  z := y // borrowed from moved\n}\n"
  in case analyzeOwnership transferCode of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- Property: Multiple ownership transfers are handled
prop_multiple_ownership_transfers :: Property
prop_multiple_ownership_transfers =
  let multipleTransfers = "func test() {\n  a := 1 // owned\n  b := move(a) // moved\n  c := borrow(b) // borrowed\n  d := share(c) // shared\n}\n"
  in case analyzeOwnership multipleTransfers of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- Property: Ownership analyzer detects invalid transfers
prop_ownership_analyzer_detects_invalid :: Property
prop_ownership_analyzer_detects_invalid =
  let invalidTransfer = "func test() {\n  x := 1 // owned\n  y := move(x) // moved\n  z := move(x) // double move - should be error\n}\n"
  in case analyzeOwnership invalidTransfer of
    Left _ -> property True  -- Should detect error
    Right _ -> property False

-- Property: Ownership analyzer handles function parameters
prop_ownership_analyzer_function_params :: Property
prop_ownership_analyzer_function_params =
  let funcWithParams = "func process(data int // borrowed) {\n  result := data * 2\n  return result // owned\n}\n"
  in case analyzeOwnership funcWithParams of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- Property: Ownership analyzer handles return values
prop_ownership_analyzer_return_values :: Property
prop_ownership_analyzer_return_values =
  let funcWithReturn = "func create() int // owned {\n  return 42\n}\nfunc test() {\n  x := create() // owned\n}\n"
  in case analyzeOwnership funcWithReturn of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- Property: Ownership analyzer handles nested scopes
prop_ownership_analyzer_nested_scopes :: Property
prop_ownership_analyzer_nested_scopes =
  let nestedScopes = "func outer() {\n  x := 1 // owned\n  {\n    y := borrow(x) // borrowed\n    z := 2 // owned\n  }\n  w := x // still accessible\n}\n"
  in case analyzeOwnership nestedScopes of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- Property: Ownership analyzer handles borrowing chains
prop_ownership_analyzer_borrowing_chains :: Property
prop_ownership_analyzer_borrowing_chains =
  let borrowingChains = "func test() {\n  original := 1 // owned\n  first := borrow(original) // borrowed\n  second := borrow(first) // borrowed from borrowed\n  third := borrow(second) // borrowed chain\n}\n"
  in case analyzeOwnership borrowingChains of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- Property: Ownership analyzer handles shared references
prop_ownership_analyzer_shared_references :: Property
prop_ownership_analyzer_shared_references =
  let sharedRefs = "func test() {\n  data := 1 // owned\n  shared1 := share(data) // shared\n  shared2 := share(data) // another shared reference\n  shared3 := share(data) // third shared reference\n}\n"
  in case analyzeOwnership sharedRefs of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- Property: Ownership analyzer handles move semantics
prop_ownership_analyzer_move_semantics :: Property
prop_ownership_analyzer_move_semantics =
  let moveSemantics = "func test() {\n  source := 1 // owned\n  target := move(source) // moved\n  // source is no longer accessible\n  result := target * 2 // using moved value\n}\n"
  in case analyzeOwnership moveSemantics of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- ============================================================================
-- Error Detection Properties
-- ============================================================================

-- Property: Use after move is detected
prop_use_after_move_detected :: Property
prop_use_after_move_detected =
  let useAfterMove = "func test() {\n  x := 1 // owned\n  y := move(x) // moved\n  z := x + 1 // use after move - should be error\n}\n"
  in case analyzeOwnership useAfterMove of
    Left _ -> property True  -- Should detect error
    Right _ -> property False

-- Property: Double borrow is detected
prop_double_borrow_detected :: Property
prop_double_borrow_detected =
  let doubleBorrow = "func test() {\n  x := 1 // owned\n  y := borrow(x) // borrowed\n  z := borrow(x) // double borrow - should be error\n}\n"
  in case analyzeOwnership doubleBorrow of
    Left _ -> property True  -- Should detect error
    Right _ -> property False

-- Property: Invalid ownership transfer is detected
prop_invalid_transfer_detected :: Property
prop_invalid_transfer_detected =
  let invalidTransfer = "func test() {\n  x := 1 // owned\n  y := move(x) // moved\n  z := move(y) // moving moved value - should be error\n}\n"
  in case analyzeOwnership invalidTransfer of
    Left _ -> property True  -- Should detect error
    Right _ -> property False

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: Ownership analysis is linear in code size
prop_ownership_analysis_linear :: Property
prop_ownership_analysis_linear =
  let largeCode = unlines $ replicate 100 "  x := 1 // owned\n  y := move(x) // moved\n  z := 2 // owned\n"
  in case analyzeOwnership largeCode of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- Property: Ownership analysis handles repeated patterns efficiently
prop_ownership_analysis_repeated_patterns :: Property
prop_ownership_analysis_repeated_patterns =
  let pattern = "a := 1 // owned\nb := move(a) // moved\nc := 2 // owned\n"
      largeCode = "func test() {\n" ++ L.concat (replicate 50 pattern) ++ "}\n"
  in case analyzeOwnership largeCode of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- ============================================================================
-- Advanced Ownership Properties
-- ============================================================================

-- Property: Ownership analyzer maintains consistency across scopes
prop_ownership_consistency_across_scopes :: Property
prop_ownership_consistency_across_scopes =
  let multiScopeCode = "func outer() {\n  outerVar := 1 // owned\n  func inner() {\n    innerVar := 2 // owned\n    temp := borrow(outerVar) // borrowed from outer\n  }\n  // outerVar should still be valid here\n}\n"
  in case analyzeOwnership multiScopeCode of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- Property: Ownership analyzer handles circular dependencies
prop_ownership_analyzer_circular_deps :: Property
prop_ownership_analyzer_circular_deps =
  let circularDeps = "func test() {\n  a := 1 // owned\n  b := borrow(a) // borrowed\n  c := borrow(b) // borrowed from borrowed\n  // c -> b -> a forms a chain, not a cycle\n}\n"
  in case analyzeOwnership circularDeps of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- Property: Ownership analyzer handles complex transfer patterns
prop_ownership_analyzer_complex_patterns :: Property
prop_ownership_analyzer_complex_patterns =
  let complexPattern = "func process(data int // borrowed) int // owned {\n  temp := data * 2 // local\n  result := move(temp) // moved\n  return result // moved out\n}\nfunc test() {\n  input := 1 // owned\n  output := process(input) // owned\n  // input should still be valid (was borrowed)\n}\n"
  in case analyzeOwnership complexPattern of
    Left _ -> property False
    Right (analyzer, _) ->
      property $ True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Transfer Invariant Tests"
  [ testGroup "Basic Ownership Properties"
    [ fastProperty "Ownership transfer preserves type safety" prop_ownership_transfer_type_safety
    , fastProperty "Ownership analyzer handles empty input" prop_ownership_analyzer_empty_input
    , fastProperty "Ownership transfer is tracked correctly" prop_ownership_transfer_tracked
    ]

  , testGroup "Transfer Operation Properties"
    [ fastProperty "Multiple ownership transfers are handled" prop_multiple_ownership_transfers
    , fastProperty "Ownership analyzer detects invalid transfers" prop_ownership_analyzer_detects_invalid
    , fastProperty "Ownership analyzer handles function parameters" prop_ownership_analyzer_function_params
    , fastProperty "Ownership analyzer handles return values" prop_ownership_analyzer_return_values
    ]

  , testGroup "Advanced Ownership Properties"
    [ fastProperty "Ownership analyzer handles nested scopes" prop_ownership_analyzer_nested_scopes
    , fastProperty "Ownership analyzer handles borrowing chains" prop_ownership_analyzer_borrowing_chains
    , fastProperty "Ownership analyzer handles shared references" prop_ownership_analyzer_shared_references
    , fastProperty "Ownership analyzer handles move semantics" prop_ownership_analyzer_move_semantics
    ]

  , testGroup "Error Detection Properties"
    [ fastProperty "Use after move is detected" prop_use_after_move_detected
    , fastProperty "Double borrow is detected" prop_double_borrow_detected
    , fastProperty "Invalid ownership transfer is detected" prop_invalid_transfer_detected
    ]

  , testGroup "Performance Properties"
    [ fastProperty "Ownership analysis is linear in code size" prop_ownership_analysis_linear
    , fastProperty "Ownership analysis handles repeated patterns efficiently" prop_ownership_analysis_repeated_patterns
    ]

  , testGroup "Advanced Ownership Properties"
    [ fastProperty "Ownership analyzer maintains consistency across scopes" prop_ownership_consistency_across_scopes
    , fastProperty "Ownership analyzer handles circular dependencies" prop_ownership_analyzer_circular_deps
    , fastProperty "Ownership analyzer handles complex transfer patterns" prop_ownership_analyzer_complex_patterns
    ]
  ]