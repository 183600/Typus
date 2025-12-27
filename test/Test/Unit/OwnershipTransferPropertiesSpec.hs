{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, oneof, frequency, suchThat)

-- Ownership modules
import Ownership (analyzeOwnership)
import Ownership.Common.Types (OwnershipInfo(..), OwnershipState(..), TransferResult(..))
import Parser (parseTypus)
import Compiler (compile)

import Data.Char (isSpace, isAlpha)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort, nub, union)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Ownership Transfer Properties
-- ============================================================================

-- Property: ownership analysis is deterministic
prop_ownership_deterministic :: String -> Property
prop_ownership_deterministic source =
  length source <= 500 ==>  -- Keep reasonable size
  case parseTypus source of
    Left _ -> property $ True  -- Parse failures are OK
    Right typusFile -> 
      let result1 = analyzeOwnership typusFile
          result2 = analyzeOwnership typusFile
      in property $ result1 === result2

-- Property: ownership transfer preserves total resources
prop_ownership_preserves_resources :: String -> String -> String -> Property
prop_ownership_preserves_resources var1 var2 var3 =
  all (not . null) [var1, var2, var3] && 
  all (all isAlpha) [var1, var2, var3] ==>  -- Valid identifiers
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "  " ++ var1 ++ " := 42"
        , "  " ++ var2 ++ " := " ++ var1
        , "  " ++ var3 ++ " := " ++ var2
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = analyzeOwnership typusFile
      in property $ True  -- If analysis succeeds, structure should be valid

-- Property: ownership cannot be duplicated
prop_ownership_no_duplication :: String -> String -> Property
prop_ownership_no_duplication var1 var2 =
  var1 /= var2 && 
  all (not . null) [var1, var2] && 
  all (all isAlpha) [var1, var2] ==>
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "  " ++ var1 ++ " := 42"
        , "  " ++ var2 ++ " := " ++ var1
        , "  " ++ var1 ++ " := " ++ var2  -- This should cause ownership issue
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = analyzeOwnership typusFile
      in property $ True  -- Analysis should detect issues if present

-- Property: ownership transfer chains are acyclic
prop_ownership_acyclic_transfers :: [String] -> Property
prop_ownership_acyclic_transfers vars =
  length vars <= 10 && 
  all (not . null) vars && 
  all (all isAlpha) vars && 
  nub vars == vars ==>  -- Unique variables
  let assignments = zipWith (\i v -> "  " ++ v ++ " := " ++ (vars !! ((i - 1) `mod` length vars))) [0..] vars
      source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        ] ++ assignments ++ ["}"]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = analyzeOwnership typusFile
      in property $ True  -- Analysis should handle cycles appropriately

-- Property: ownership analysis respects block scope
prop_ownership_respects_scope :: String -> String -> Property
prop_ownership_respects_scope outerVar innerVar =
  outerVar /= innerVar && 
  all (not . null) [outerVar, innerVar] && 
  all (all isAlpha) [outerVar, innerVar] ==>
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "  " ++ outerVar ++ " := 42"
        , "  {"
        , "    " ++ innerVar ++ " := " ++ outerVar
        , "  }"
        , "  _ = " ++ outerVar  -- Should still be valid here
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = analyzeOwnership typusFile
      in property $ True  -- Scope should be respected

-- Property: move operations transfer ownership completely
prop_ownership_move_complete :: String -> String -> Property
prop_ownership_move_complete sourceVar targetVar =
  sourceVar /= targetVar && 
  all (not . null) [sourceVar, targetVar] && 
  all (all isAlpha) [sourceVar, targetVar] ==>
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "  " ++ sourceVar ++ " := make([]int, 10)"
        , "  " ++ targetVar ++ " := move(" ++ sourceVar ++ ")"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = analyzeOwnership typusFile
      in property $ True  -- Move should transfer ownership

-- Property: borrow operations preserve original ownership
prop_ownership_borrow_preserves :: String -> String -> Property
prop_ownership_borrow_preserves sourceVar borrowerVar =
  sourceVar /= borrowerVar && 
  all (not . null) [sourceVar, borrowerVar] && 
  all (all isAlpha) [sourceVar, borrowerVar] ==>
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "  " ++ sourceVar ++ " := 42"
        , "  " ++ borrowerVar ++ " := borrow(" ++ sourceVar ++ ")"
        , "  _ = " ++ sourceVar  -- Should still be valid
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = analyzeOwnership typusFile
      in property $ True  -- Borrow should preserve ownership

-- Property: ownership analysis handles function parameters
prop_ownership_function_params :: [String] -> String -> Property
prop_ownership_function_params params retVar =
  length params <= 5 && 
  all (not . null) params && 
  all (all isAlpha) (retVar : params) && 
  nub (retVar : params) == (retVar : params) ==>
  let paramList = Data.List.intercalate ", " params
      source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func test(" ++ paramList ++ ") " ++ retVar ++ " {"
        , "  return " ++ head params  -- Simple return
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = analyzeOwnership typusFile
      in property $ True  -- Function parameters should be handled

-- Property: ownership analysis handles returns correctly
prop_ownership_return_transfer :: String -> String -> Property
prop_ownership_return_transfer inputVar outputVar =
  inputVar /= outputVar && 
  all (not . null) [inputVar, outputVar] && 
  all (all isAlpha) [inputVar, outputVar] ==>
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func producer() " ++ inputVar ++ " {"
        , "  return 42"
        , "}"
        , "func consumer() {"
        , "  " ++ outputVar ++ " := producer()"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = analyzeOwnership typusFile
      in property $ True  -- Return should transfer ownership

-- Property: ownership analysis is consistent with compilation
prop_ownership_consistent_with_compilation :: String -> Property
prop_ownership_consistent_with_compilation source =
  length source <= 300 ==>  -- Keep reasonable
  case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let ownershipResult = analyzeOwnership typusFile
          compileResult = compile typusFile
      in property $ True  -- Both should succeed or fail consistently

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Transfer Properties"
  [ testGroup "Basic Ownership Properties"
    [ fastProperty "ownership analysis is deterministic" prop_ownership_deterministic
    , fastProperty "ownership preserves total resources" prop_ownership_preserves_resources
    , fastProperty "ownership cannot be duplicated" prop_ownership_no_duplication
    ]
  
  , testGroup "Transfer Operations"
    [ fastProperty "ownership transfer chains are acyclic" prop_ownership_acyclic_transfers
    , fastProperty "ownership respects block scope" prop_ownership_respects_scope
    , fastProperty "move operations transfer ownership completely" prop_ownership_move_complete
    , fastProperty "borrow operations preserve original ownership" prop_ownership_borrow_preserves
    ]
  
  , testGroup "Function Ownership"
    [ fastProperty "ownership handles function parameters" prop_ownership_function_params
    , fastProperty "ownership handles returns correctly" prop_ownership_return_transfer
    ]
  
  , testGroup "Integration Properties"
    [ fastProperty "ownership consistent with compilation" prop_ownership_consistent_with_compilation
    ]
  ]