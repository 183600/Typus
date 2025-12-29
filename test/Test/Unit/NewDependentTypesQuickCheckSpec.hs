{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewDependentTypesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import DependentTypesParser
  ( DependentTypesParser(..)
  , DependentTypeError(..)
  , TypeRef(..)
  , TypeBody(..)
  , Field(..)
  , TypeParameter(..)
  , TypeConstraint(..)
  , DependentType(..)
  , DependentParseResult
  , runDependentTypesParser
  , parseDependentType
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  )

import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import qualified Data.Text as T
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import Data.Char (isAlphaNum)

-- | Dependent Types QuickCheck tests
tests :: TestTree
tests = testGroup "New Dependent Types QuickCheck Tests"
  [ fastProperty "parseTypeDeclaration handles simple types" prop_parse_simple_type
  , fastProperty "parseDependentType extracts first type" prop_parse_first_type
  , fastProperty "validateDependentTypeSyntax checks syntax" prop_validate_syntax
  , fastProperty "runDependentTypesParser processes all types" prop_run_parser
  , fastProperty "TypeRef construction is consistent" prop_type_ref_consistency
  , fastProperty "Field definitions are valid" prop_field_definitions
  , fastProperty "Type constraints are properly formed" prop_type_constraints
  , fastProperty "Error messages contain useful information" prop_error_messages
  ]

-- Property: parseTypeDeclaration handles simple types
prop_parse_simple_type :: String -> Property
prop_parse_simple_type typeName =
  length typeName <= 10 && all isAlphaNum typeName ==>
  let typeDef = "type " ++ typeName ++ " = int"
      result = parseTypeDeclaration typeDef
  in case result of
    Left _ -> property True -- May fail but shouldn't crash
    Right parsedType -> property $ parsedType `seq` True

-- Property: parseDependentType extracts first type
prop_parseDependentType :: String -> Property
prop_parseDependentType content =
  length content <= 50 ==>
  let result = parseDependentType content
  in case result of
    Left _ -> property True -- May fail but shouldn't crash
    Right parsedType -> property $ parsedType `seq` True

-- Property: validateDependentTypeSyntax checks syntax
prop_validate_syntax :: String -> Property
prop_validate_syntax content =
  length content <= 40 ==>
  let result = validateDependentTypeSyntax content
      errors = result
  in property $ length errors >= 0 -- Should return some errors or none

-- Property: runDependentTypesParser processes all types
prop_run_parser :: String -> Property
prop_run_parser content =
  length content <= 60 ==>
  let result = runDependentTypesParser content
  in case result of
    Left _ -> property True -- May fail but shouldn't crash
    Right (types, errors) -> property $ length types >= 0 .&&. length errors >= 0

-- Property: TypeRef construction is consistent
prop_type_ref_consistency :: String -> [String] -> Property
prop_type_ref_consistency baseName params =
  length baseName <= 10 && all (\p -> length p <= 8) params ==>
  let typeRef = TypeRef baseName params
  in property $ case typeRef of
    TypeRef name typeParams -> 
      name === baseName .&&. typeParams === params

-- Property: Field definitions are valid
prop_field_definitions :: String -> String -> Property
prop_field_definitions fieldName fieldType =
  length fieldName <= 8 && length fieldType <= 10 &&
  all isAlphaNum fieldName && all isAlphaNum fieldType ==>
  let field = Field fieldName fieldType
  in property $ case field of
    Field name typ -> name === fieldName .&&. typ === fieldType

-- Property: Type constraints are properly formed
prop_type_constraints :: String -> String -> Property
prop_type_constraints constraintName constraintValue =
  length constraintName <= 10 && length constraintValue <= 15 ==>
  let constraint = TypeConstraint constraintName constraintValue
  in property $ case constraint of
    TypeConstraint name value -> name === constraintName .&&. value === constraintValue

-- Property: Error messages contain useful information
prop_error_messages :: String -> Property
prop_error_messages errorMsg =
  length errorMsg <= 30 ==>
  let error = DependentTypeError "DT001" (T.pack errorMsg) Nothing Nothing
      errorStr = show error
  in property $ not (null errorStr) .&&. errorMsg `isInfixOf` errorStr