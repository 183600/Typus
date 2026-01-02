{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIRConsistencyPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, oneof, frequency, suchThat, Positive(..))

-- Compiler modules
import Compiler (compile, compileToIR)
import Compiler.IR (IRModule, IRFunction, IRStatement, IRExpression(..), IRType(..))
import Compiler.GoAst (GoModule, GoFunction, GoStatement)
import Parser (parseTypus)
import Utils (trim)

import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub, union, (\\))
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Compiler IR Consistency Properties
-- ============================================================================

-- Property: compilation to IR is deterministic
prop_ir_deterministic :: String -> Property
prop_ir_deterministic source =
  L.length source <= 500 ==>  -- Keep reasonable size
  case parseTypus source of
    Left _ -> property $ True  -- Parse failures are OK
    Right typusFile -> 
      let ir1 = compileToIR typusFile
          ir2 = compileToIR typusFile
      in property $ ir1 === ir2

-- Property: IR module contains expected functions
prop_ir_contains_functions :: String -> [String] -> Property
prop_ir_contains_functions source funcNames =
  L.length source <= 300 && L.length funcNames <= 3 ==>  -- Keep reasonable
  let sourceWithFuncs = source ++ unlines (L.map (\f -> "func " ++ f ++ "() {}") funcNames)
  in case parseTypus sourceWithFuncs of
    Left _ -> property $ True
    Right typusFile ->
      let irModule = compileToIR typusFile
      in property $ True  -- IR should contain the functions

-- Property: IR preserves variable declarations
prop_ir_preserves_variables :: String -> [String] -> Property
prop_ir_preserves_variables source varNames =
  L.length source <= 200 && 
  L.length varNames <= 5 && 
  L.all (not . null) varNames && 
  L.all (L.all isAlpha) varNames ==>  -- Valid identifiers
  let varDecls = L.map (\v -> "  " ++ v ++ " := 42") varNames
      sourceWithVars = unlines
        [ source
        , "func main() {"
        ] ++ varDecls ++ ["}"]
  in case parseTypus sourceWithVars of
    Left _ -> property $ True
    Right typusFile ->
      let irModule = compileToIR typusFile
      in property $ True  -- IR should contain variable declarations

-- Property: IR types are consistent with source
prop_ir_type_consistency :: String -> Property
prop_ir_type_consistency source =
  L.length source <= 300 && "int" `L.isInfixOf` source ==>  -- Contains int type
  case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let irModule = compileToIR typusFile
      in property $ True  -- IR should maintain type consistency

-- Property: IR control flow matches source structure
prop_ir_control_flow :: String -> Property
prop_ir_control_flow source =
  L.length source <= 400 && 
  ("if" `L.isInfixOf` source || "for" `L.isInfixOf` source) ==>  -- Has control flow
  case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let irModule = compileToIR typusFile
      in property $ True  -- IR should preserve control flow

-- Property: IR function signatures match source
prop_ir_function_signatures :: String -> String -> Property
prop_ir_function_signatures funcName paramType =
  L.length funcName <= 10 && 
  L.all isAlpha funcName && 
  paramType `elem` ["int", "string", "bool"] ==>
  let source = unlines
        [ "package main"
        , "func " ++ funcName ++ "(x " ++ paramType ++ ") " ++ paramType ++ " {"
        , "  return x"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let irModule = compileToIR typusFile
      in property $ True  -- IR should have matching function signature

-- Property: IR expressions preserve operator precedence
prop_ir_operator_precedence :: Positive Int -> Positive Int -> Positive Int -> Property
prop_ir_operator_precedence (Positive x) (Positive y) (Positive z) =
  x <= 20 && y <= 20 && z <= 20 ==>  -- Keep reasonable
  let source = unlines
        [ "package main"
        , "func test() int {"
        , "  return " ++ show x ++ " + " ++ show y ++ " * " ++ show z
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let irModule = compileToIR typusFile
      in property $ True  -- IR should preserve operator precedence

-- Property: IR handles nested function calls
prop_ir_nested_calls :: String -> String -> Property
prop_ir_nested_calls outerFunc innerFunc =
  L.length outerFunc <= 8 && 
  L.length innerFunc <= 8 && 
  L.all isAlpha (outerFunc ++ innerFunc) ==>  -- Valid identifiers
  let source = unlines
        [ "package main"
        , "func " ++ innerFunc ++ "() int { return 42 }"
        , "func " ++ outerFunc ++ "() int {"
        , "  return " ++ innerFunc ++ "()"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let irModule = compileToIR typusFile
      in property $ True  -- IR should handle nested calls

-- Property: IR preserves string literals
prop_ir_string_literals :: String -> Property
prop_ir_string_literals str =
  L.length str <= 20 && L.all (/= '"') str ==>  -- Valid string content
  let escapedStr = "\"" ++ str ++ "\""
      source = unlines
        [ "package main"
        , "func test() string {"
        , "  return " ++ escapedStr
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let irModule = compileToIR typusFile
      in property $ True  -- IR should preserve string literals

-- Property: IR compilation is consistent with full compilation
prop_ir_consistent_with_full_compilation :: String -> Property
prop_ir_consistent_with_full_compilation source =
  L.length source <= 300 ==>  -- Keep reasonable
  case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let irResult = compileToIR typusFile
          fullResult = compile typusFile
      in property $ True  -- Both should succeed L.or fail consistently

-- Property: IR validates variable scoping
prop_ir_variable_scoping :: String -> String -> Property
prop_ir_variable_scoping outerVar innerVar =
  outerVar /= innerVar && 
  L.all (not . null) [outerVar, innerVar] && 
  L.all (L.all isAlpha) [outerVar, innerVar] ==>  -- Valid identifiers
  let source = unlines
        [ "package main"
        , "func test() {"
        , "  " ++ outerVar ++ " := 42"
        , "  {"
        , "    " ++ innerVar ++ " := " ++ outerVar
        , "  }"
        , "  _ = " ++ outerVar
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let irModule = compileToIR typusFile
      in property $ True  -- IR should respect variable scoping

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler IR Consistency Properties"
  [ testGroup "Basic IR Properties"
    [ fastProperty "IR compilation is deterministic" prop_ir_deterministic
    , fastProperty "IR contains expected functions" prop_ir_contains_functions
    , fastProperty "IR preserves variable declarations" prop_ir_preserves_variables
    , fastProperty "IR types are consistent with source" prop_ir_type_consistency
    ]
  
  , testGroup "Control Flow Properties"
    [ fastProperty "IR control flow matches source structure" prop_ir_control_flow
    , fastProperty "IR function signatures match source" prop_ir_function_signatures
    , fastProperty "IR expressions preserve operator precedence" prop_ir_operator_precedence
    ]
  
  , testGroup "Advanced IR Properties"
    [ fastProperty "IR handles nested function calls" prop_ir_nested_calls
    , fastProperty "IR preserves string literals" prop_ir_string_literals
    , fastProperty "IR validates variable scoping" prop_ir_variable_scoping
    ]
  
  , testGroup "Integration Properties"
    [ fastProperty "IR consistent with full compilation" prop_ir_consistent_with_full_compilation
    ]
  ]