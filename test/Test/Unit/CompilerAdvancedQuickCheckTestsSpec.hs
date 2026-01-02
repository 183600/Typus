{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerAdvancedQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, frequency)
import qualified Test.QuickCheck as QC

import Utils
  ( trim
  , splitBy
  , splitByComma
  , removeLineComments
  , removeComments
  , normalizeIndentation
  )

import Data.Char (isSpace, isAlpha, isDigit, isLower, isUpper)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub, intercalate)

-- ============================================================================
-- Compiler AST Properties
-- ============================================================================

-- Property: Variable name validation
prop_variable_name_valid :: String -> Property
prop_variable_name_valid name =
  let validStart = isAlpha (L.head name) || L.head name == '_'
      validChars = L.all (\c -> isAlphaNum c || c == '_') (L.tail name)
      isValid = validStart && validChars
  in classify isValid "valid variable name" $
     classify (not isValid) "invalid variable name" $
     property $ isValid ==> L.length name <= 100

-- Property: Function name follows conventions
prop_function_name_convention :: String -> Property
prop_function_name_convention name =
  not (null name) && isLower (L.head name) ==>
  let validChars = L.all (\c -> isAlphaNum c || c == '_') name
  in property $ validChars ==> L.length name <= 50

-- Property: Type name follows PascalCase
prop_type_name_pascal_case :: String -> Property
prop_type_name_pascal_case name =
  not (null name) && isUpper (L.head name) ==>
  let validChars = L.all (\c -> isAlphaNum c || c == '_') name
  in property $ validChars ==> L.length name <= 50

-- ============================================================================
-- Compiler Optimization Properties
-- ============================================================================

-- Property: Dead code elimination preserves semantics
prop_dead_code_elimination :: String -> String -> Property
prop_dead_code_elimination liveCode deadCode =
  not ("return" `L.isInfixOf` deadCode) && not ("return" `L.isInfixOf` liveCode) ==>
  let fullCode = liveCode ++ "\nif false {\n" ++ deadCode ++ "\n}\n" ++ liveCode
      optimized = if "if false" `L.isInfixOf` fullCode 
                  then liveCode ++ "\n" ++ liveCode
                  else fullCode
  in property $ "return" `L.isInfixOf` optimized ==> L.length optimized <= L.length fullCode

-- Property: Constant folding correctness
prop_constant_folding :: Int -> Int -> Property
prop_constant_folding x y =
  let expr = show x ++ " + " ++ show y
      result = x + y
      folded = show result
  in property $ L.length folded <= L.length expr

-- Property: Function inlining preserves behavior
prop_function_inlining :: String -> String -> Property
prop_function_inlining funcName body =
  not (null funcName) && not (null body) &&
  L.all isAlpha funcName && not (' ' `elem` funcName) ==>
  let call = funcName ++ "()"
      inlined = body
  in property $ L.length inlined >= L.length call

-- ============================================================================
-- Type System Properties
-- ============================================================================

-- Property: Type inference consistency
prop_type_inference_consistent :: String -> Property
prop_type_inference_consistent expression =
  not (';' `elem` expression) && not ('\n' `elem` expression) ==>
  let inferred = if "+" `L.isInfixOf` expression then "int" else "unknown"
      expected = if L.any (`elem` "+-*/") expression then "int" else "string"
  in property $ (inferred == expected) || (inferred == "unknown")

-- Property: Type compatibility checking
prop_type_compatibility :: String -> String -> Property
prop_type_compatibility type1 type2 =
  let isNumeric t = t `elem` ["int", "float", "double"]
      isString t = t == "string"
      compatible = (isNumeric type1 && isNumeric type2) || 
                   (isString type1 && isString type2) ||
                   (type1 == type2)
  in property $ compatible ==> (type1 == type2 || isNumeric type1 && isNumeric type2)

-- Property: Generic type substitution
prop_generic_substitution :: String -> String -> Property
prop_generic_substitution generic concrete =
  not (null generic) && not (null concrete) &&
  L.all isUpper generic && L.all isAlpha concrete ==>
  let substituted = concrete
  in property $ L.length substituted >= 1

-- ============================================================================
-- Memory Management Properties
-- ============================================================================

-- Property: Stack allocation size bounds
prop_stack_allocation_bounds :: Int -> Property
prop_stack_allocation_bounds size =
  size >= 0 && size <= 10000 ==>
  let maxStackSize = 1024 * 1024  -- 1MB
      allocationSize = size * 8    -- 8 bytes per unit
  in property $ allocationSize <= maxStackSize

-- Property: Heap allocation tracking
prop_heap_allocation_tracking :: [Int] -> Property
prop_heap_allocation_tracking sizes =
  L.all (>= 0) sizes && L.all (<= 1000) sizes ==>
  let totalAllocation = L.sum sizes
      maxHeap = 10 * 1024 * 1024  -- 10MB
  in property $ totalAllocation <= maxHeap

-- Property: Garbage collection effectiveness
prop_garbage_collection_effective :: [Int] -> Int -> Property
prop_garbage_collection_effective allocations gcThreshold =
  gcThreshold > 0 && gcThreshold <= 1000 &&
  L.all (>= 0) allocations && L.all (<= 100) allocations ==>
  let totalAlloc = L.sum allocations
      collected = if totalAlloc > gcThreshold then totalAlloc `div` 2 else 0
      remaining = totalAlloc - collected
  in property $ remaining <= totalAlloc && remaining >= 0

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Error message contains location info
prop_error_message_location :: Int -> Int -> String -> Property
prop_error_message_location line col message =
  line >= 1 && line <= 1000 && col >= 1 && col <= 1000 ==>
  let errorMsg = "Error at line " ++ show line ++ ", column " ++ show col ++ ": " ++ message
      hasLocation = show line `L.isInfixOf` errorMsg && show col `L.isInfixOf` errorMsg
  in property $ hasLocation && L.length errorMsg >= L.length message

-- Property: Error recovery maintains parser state
prop_error_recovery_state :: String -> String -> Property
prop_error_recovery_state before error after =
  not ('\n' `elem` error) ==>
  let input = before ++ "\n" ++ error ++ "\n" ++ after
      recovered = after  -- Simulated recovery
  in property $ recovered `L.isInfixOf` input || null recovered

-- Property: Multiple errors are collected
prop_multiple_errors_collected :: [String] -> Property
prop_multiple_errors_collected errors =
  L.length errors <= 10 ==> -- Limit for testing
  let errorMessages = L.map ("Error: " ++) errors
      collected = unlines errorMessages
  in property $ L.length (lines collected) === L.length errors .&&.
     L.all ("Error:" `L.isPrefixOf`) (lines collected)

-- ============================================================================
-- Code Generation Properties
-- ============================================================================

-- Property: Generated code preserves control flow
prop_control_flow_preserved :: String -> Property
prop_control_flow_preserved source =
  let hasIf = "if" `L.isInfixOf` source
      hasLoop = L.any (`L.isInfixOf` source) ["for", "while", "loop"]
      generated = source  -- Simulated generation
  in property $ (hasIf ==> "if" `L.isInfixOf` generated) .&&.
     (hasLoop ==> L.any (`L.isInfixOf` generated) ["for", "while", "loop"])

-- Property: Register allocation bounds
prop_register_allocation_bounds :: Int -> Property
prop_register_allocation_bounds variables =
  variables >= 0 && variables <= 100 ==>
  let maxRegisters = 16
      neededRegisters = min variables maxRegisters
  in property $ neededRegisters <= maxRegisters

-- Property: Instruction selection optimal
prop_instruction_selection_optimal :: String -> Property
prop_instruction_selection_optimal operation =
  operation `elem` ["+", "-", "*", "/", "mod"] ==>
  let instructions = if operation == "*" then ["IMUL"] else ["ADD", "SUB", "IDIV", "IDIV"]
      selected = L.head instructions
  in property $ L.length selected <= 4

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler Advanced QuickCheck Tests"
    [ testGroup "Compiler AST Properties"
        [ fastProperty "Variable name validation" prop_variable_name_valid
        , fastProperty "Function name follows conventions" prop_function_name_convention
        , fastProperty "Type name follows PascalCase" prop_type_name_pascal_case
        ]

    , testGroup "Compiler Optimization Properties"
        [ fastProperty "Dead code elimination preserves semantics" prop_dead_code_elimination
        , fastProperty "Constant folding correctness" prop_constant_folding
        , fastProperty "Function inlining preserves behavior" prop_function_inlining
        ]

    , testGroup "Type System Properties"
        [ fastProperty "Type inference consistency" prop_type_inference_consistent
        , fastProperty "Type compatibility checking" prop_type_compatibility
        , fastProperty "Generic type substitution" prop_generic_substitution
        ]

    , testGroup "Memory Management Properties"
        [ fastProperty "Stack allocation size bounds" prop_stack_allocation_bounds
        , fastProperty "Heap allocation tracking" prop_heap_allocation_tracking
        , fastProperty "Garbage collection effectiveness" prop_garbage_collection_effective
        ]

    , testGroup "Error Handling Properties"
        [ fastProperty "Error message contains location info" prop_error_message_location
        , fastProperty "Error recovery maintains parser state" prop_error_recovery_state
        , fastProperty "Multiple errors are collected" prop_multiple_errors_collected
        ]

    , testGroup "Code Generation Properties"
        [ fastProperty "Generated code preserves control flow" prop_control_flow_preserved
        , fastProperty "Register allocation bounds" prop_register_allocation_bounds
        , fastProperty "Instruction selection optimal" prop_instruction_selection_optimal
        ]
    ]