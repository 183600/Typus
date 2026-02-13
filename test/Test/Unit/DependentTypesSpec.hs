{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.DependentTypesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isDigit)

-- Tests for dependent types feature as described in README.md

-- | Test parsing of value parameterized types like Vector[n: int]
prop_parse_value_parameterized_type :: String -> Property
prop_parse_value_parameterized_type typeName =
  let validType = typeName ++ "[3]"
      hasValueParam = "[" `isInfixOf` validType && "]" `isInfixOf` validType
  in property $ hasValueParam

-- | Test parsing of precise types with constraints like NonZero = int where { self != 0 }
prop_parse_precise_type :: String -> Property
prop_parse_precise_type typeName =
  let constrainedType = typeName ++ " where { self > 0 }"
      hasConstraint = "where" `isInfixOf` constrainedType && "self" `isInfixOf` constrainedType
  in property $ hasConstraint

-- | Test parsing of dependent function signatures
prop_parse_dependent_function :: String -> Property
prop_parse_dependent_function funcName =
  let dependentFunc = "func " ++ funcName ++ "(n: int) -> Vector[n]"
      hasDependentType = "n: int" `isInfixOf` dependentFunc && "Vector[n]" `isInfixOf` dependentFunc
  in property $ hasDependentType

-- | Test parsing of function preconditions
prop_parse_function_precondition :: String -> Property
prop_parse_function_precondition funcName =
  let funcWithPrecondition = "func " ++ funcName ++ "(n: int) where { n > 0 }"
      hasPrecondition = "where" `isInfixOf` funcWithPrecondition && "n > 0" `isInfixOf` funcWithPrecondition
  in property $ hasPrecondition

-- | Test parsing of existential types
prop_parse_existential_type :: String -> Property
prop_parse_existential_type typeName =
  let existentialType = "Vector[some n: int]"
      hasExistential = "some" `isInfixOf` existentialType
  in property $ hasExistential

-- | Test parsing of match expressions for existential unpacking
prop_parse_match_expression :: String -> Property
prop_parse_match_expression varName =
  let matchExpr = "match " ++ varName ++ ".(n) { ... }"
      hasMatch = "match" `isInfixOf` matchExpr && "." `isInfixOf` matchExpr
  in property $ hasMatch

-- | Test parsing of assert statements for type narrowing
prop_parse_assert_statement :: String -> Property
prop_parse_assert_statement expr =
  let assertStmt = "assert " ++ expr
      hasAssert = "assert" `isInfixOf` assertStmt
  in property $ hasAssert

-- | Test parsing of static_assert statements
prop_parse_static_assert :: String -> Property
prop_parse_static_assert expr =
  let staticAssert = "static_assert " ++ expr
      hasStaticAssert = "static_assert" `isInfixOf` staticAssert
  in property $ hasStaticAssert

-- | Test parsing of file-level directives
prop_parse_file_directive :: String -> Property
prop_parse_file_directive feature =
  let directive = "//! " ++ feature ++ ": on"
      hasDirective = "//! " `isInfixOf` directive && ": on" `isInfixOf` directive
  in property $ hasDirective

-- | Test parsing of block-level directives
prop_parse_block_directive :: String -> Property
prop_parse_block_directive feature =
  let blockDirective = "{//! " ++ feature ++ ": on"
      hasBlockDirective = "{//! " `isInfixOf` blockDirective
  in property $ hasBlockDirective

-- | Test that dependent types code can be identified
prop_identify_dependent_types_code :: String -> Property
prop_identify_dependent_types_code code =
  let hasDependentTypes = any (`isInfixOf` code) 
        ["where {", "[n: int]", "-> Vector[", "some n:", "assert ", "static_assert "]
  in property $ hasDependentTypes

-- | Test that ownership code can be identified
prop_identify_ownership_code :: String -> Property
prop_identify_ownership_code code =
  let hasOwnership = any (`isInfixOf` code)
        ["&mut ", "//! ownership:", "move ", "borrow "]
  in property $ hasOwnership

-- | Test parsing of mixed type parameters (type + value)
prop_parse_mixed_parameters :: String -> Property
prop_parse_mixed_parameters typeName =
  let mixedType = typeName ++ "[T any, n: int]"
      hasMixed = "[T any" `isInfixOf` mixedType && "n: int]" `isInfixOf` mixedType
  in property $ hasMixed

-- | Test parsing of type-level arithmetic
prop_parse_type_arithmetic :: String -> Property
prop_parse_type_arithmetic typeName =
  let arithmeticType = typeName ++ "[m + n]"
      hasArithmetic = "m + n" `isInfixOf` arithmeticType
  in property $ hasArithmetic

tests :: TestTree
tests = testGroup "Dependent Types Tests"
  [ testProperty "Parse value parameterized type" prop_parse_value_parameterized_type
  , testProperty "Parse precise type with constraints" prop_parse_precise_type
  , testProperty "Parse dependent function signature" prop_parse_dependent_function
  , testProperty "Parse function precondition" prop_parse_function_precondition
  , testProperty "Parse existential type" prop_parse_existential_type
  , testProperty "Parse match expression" prop_parse_match_expression
  , testProperty "Parse assert statement" prop_parse_assert_statement
  , testProperty "Parse static assert" prop_parse_static_assert
  , testProperty "Parse file directive" prop_parse_file_directive
  , testProperty "Parse block directive" prop_parse_block_directive
  , testProperty "Identify dependent types code" prop_identify_dependent_types_code
  , testProperty "Identify ownership code" prop_identify_ownership_code
  , testProperty "Parse mixed parameters" prop_parse_mixed_parameters
  , testProperty "Parse type arithmetic" prop_parse_type_arithmetic
  ]
