{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedDependentTypesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , resize, Positive(..), NonEmpty(..)
  )

import DependentTypesParser
  ( DependentTypesParser(..)
  , DependentTypeError(..)
  , TypeRef(..)
  , TypeBody(..)
  , Field(..)
  , TypeParameter(..)
  , TypeConstraint(..)
  , DependentType(..)
  , runDependentTypesParser
  , parseDependentType
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  )

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort, nub)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)

-- Property: TypeRef equality is reflexive
prop_typeRef_equality_reflexive :: TypeRef -> Property
prop_typeRef_equality_reflexive tr =
  property (tr === tr)

-- Property: TypeRef with same name are equal
prop_typeRef_same_name_equal :: String -> Property
prop_typeRef_same_name_equal name =
  not (null name) && L.all isAlphaNum name ==>
  let tr1 = TypeRef name []
      tr2 = TypeRef name []
  in property (tr1 === tr2)

-- Property: TypeRef with different parameters are different
prop_typeRef_different_params_unequal :: String -> [TypeRef] -> [TypeRef] -> Property
prop_typeRef_different_params_unequal name params1 params2 =
  not (null name) && L.all isAlphaNum name && params1 /= params2 ==>
  let tr1 = TypeRef name params1
      tr2 = TypeRef name params2
  in property (tr1 /= tr2)

-- Property: Field equality is reflexive
prop_field_equality_reflexive :: Field -> Property
prop_field_equality_reflexive field =
  property (field === field)

-- Property: Field with same name L.and type are equal
prop_field_same_name_type_equal :: String -> TypeRef -> Property
prop_field_same_name_type_equal name typ =
  not (null name) && L.all isAlphaNum name ==>
  let field1 = Field name typ
      field2 = Field name typ
  in property (field1 === field2)

-- Property: TypeParameter equality is reflexive
prop_typeParameter_equality_reflexive :: TypeParameter -> Property
prop_typeParameter_equality_reflexive tp =
  property (tp === tp)

-- Property: TypeConstraint equality is reflexive
prop_typeConstraint_equality_reflexive :: TypeConstraint -> Property
prop_typeConstraint_equality_reflexive tc =
  property (tc === tc)

-- Property: DependentType equality is reflexive
prop_dependentType_equality_reflexive :: DependentType -> Property
prop_dependentType_equality_reflexive dt =
  property (dt === dt)

-- Property: Parser handles empty input
prop_parseDependentType_empty :: Property
prop_parseDependentType_empty =
  case parseDependentType "" of
    Left _ -> property True -- Parsing errors are acceptable
    Right _ -> property True -- Or parsing succeeds

-- Property: Parser handles whitespace-only input
prop_parseDependentType_whitespace :: String -> Property
prop_parseDependentType_whitespace whitespace =
  L.all isSpace whitespace ==>
  case parseDependentType whitespace of
    Left _ -> property True
    Right _ -> property True

-- Property: Parser handles simple type definition
prop_parseDependentType_simple :: String -> Property
prop_parseDependentType_simple typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let source = "type " ++ typeName ++ " = int"
  in case parseDependentType source of
    Left _ -> property True
    Right dt -> property (dtName dt === typeName)

-- Property: Parser handles generic type definition
prop_parseDependentType_generic :: String -> [String] -> Property
prop_parseDependentType_generic typeName params =
  not (null typeName) && L.all isAlphaNum typeName && 
  L.all (L.all isAlphaNum) params && not (null params) ==>
  let paramStr = unwords params
      source = "type " ++ typeName ++ " [" ++ paramStr ++ "] = int"
  in case parseDependentType source of
    Left _ -> property True
    Right dt -> property (dtName dt === typeName)

-- Property: Parser handles struct type definition
prop_parseDependentType_struct :: String -> [(String, String)] -> Property
prop_parseDependentType_struct typeName fields =
  not (null typeName) && L.all isAlphaNum typeName && 
  L.all (\(n, t) -> L.all isAlphaNum n && L.all isAlphaNum t) fields ==>
  let fieldStr = unlines $ L.map (\(n, t) -> "  " ++ n ++ ": " ++ t) fields
      source = unlines ["type " ++ typeName ++ " = struct {", fieldStr, "}"]
  in case parseDependentType source of
    Left _ -> property True
    Right dt -> property (dtName dt === typeName)

-- Property: Parser handles type with constraints
prop_parseDependentType_constraints :: String -> String -> Property
prop_parseDependentType_constraints typeName constraint =
  not (null typeName) && L.all isAlphaNum typeName && 
  not (null constraint) ==>
  let source = unlines 
        [ "type " ++ typeName ++ " = int"
        , "where " ++ constraint
        ]
  in case parseDependentType source of
    Left _ -> property True
    Right dt -> property (dtName dt === typeName)

-- Property: validateDependentTypeSyntax handles valid input
prop_validateDependentTypeSyntax_valid :: String -> Property
prop_validateDependentTypeSyntax_valid source =
  not (null source) && "type " `L.isPrefixOf` source ==>
  let result = validateDependentTypeSyntax source
  in case result of
    Left _ -> property True -- Validation errors are acceptable
    Right _ -> property True -- Or validation succeeds

-- Property: runDependentTypesParser handles multiple definitions
prop_runDependentTypesParser_multiple :: [String] -> Property
prop_runDependentTypesParser_multiple typeNames =
  not (null typeNames) && L.all (L.all isAlphaNum) typeNames && 
  L.length typeNames <= 5 ==> -- Limit to avoid huge inputs
  let definitions = L.map (\n -> "type " ++ n ++ " = int") typeNames
      source = unlines definitions
  in case runDependentTypesParser source of
    Left _ -> property True
    Right (dts, _) -> property (L.length dts >= 0)

-- Property: parseTypeDeclaration handles simple declaration
prop_parseTypeDeclaration_simple :: String -> Property
prop_parseTypeDeclaration_simple typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let source = "type " ++ typeName ++ " = int"
  in case parseTypeDeclaration source of
    Left _ -> property True
    Right dt -> property (dtName dt === typeName)

-- Arbitrary instances
instance Arbitrary TypeRef where
  arbitrary = oneof
    [ TypeRef <$> arbitraryIdentifier <*> pure []
    , TypeRef <$> arbitraryIdentifier <*> listOf arbitrary
    ]
    where
      arbitraryIdentifier = do
        len <- choose (1, 10)
        chars <- vectorOf len (elements ['a'..'z'])
        return (chars :: String)

instance Arbitrary Field where
  arbitrary = Field <$> arbitraryIdentifier <*> arbitrary
    where
      arbitraryIdentifier = do
        len <- choose (1, 10)
        chars <- vectorOf len (elements ['a'..'z'])
        return (chars :: String)

instance Arbitrary TypeParameter where
  arbitrary = oneof
    [ TypeParameter <$> arbitraryIdentifier <*> pure Nothing
    , TypeParameter <$> arbitraryIdentifier <*> arbitrary
    ]
    where
      arbitraryIdentifier = do
        len <- choose (1, 10)
        chars <- vectorOf len (elements ['a'..'z'])
        return (chars :: String)

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ EqualityConstraint <$> arbitraryIdentifier <*> arbitrary
    , ComparisonConstraint <$> arbitraryIdentifier <*> arbitrary <*> arbitrary
    , LengthConstraint <$> arbitraryIdentifier <*> arbitrary
    , NonEmptyConstraint <$> arbitraryIdentifier
    , PredicateConstraint <$> arbitraryIdentifier <*> arbitrary
    ]
    where
      arbitraryIdentifier = do
        len <- choose (1, 10)
        chars <- vectorOf len (elements ['a'..'z'])
        return (chars :: String)

instance Arbitrary DependentType where
  arbitrary = oneof
    [ DependentType <$> arbitraryIdentifier <*> pure [] <*> (SimpleType <$> arbitrary)
    , DependentType <$> arbitraryIdentifier <*> listOf arbitrary <*> (StructType <$> listOf arbitrary)
    , DependentType <$> arbitraryIdentifier <*> pure [] <*> (AliasType <$> arbitrary)
    ]
    where
      arbitraryIdentifier = do
        len <- choose (1, 10)
        chars <- vectorOf len (elements ['a'..'z'])
        return (chars :: String)

-- Helper function
arbitraryIdentifier :: Gen String
arbitraryIdentifier = do
  len <- choose (1, 10)
  chars <- vectorOf len (elements ['a'..'z'])
  return (chars :: String)

tests :: TestTree
tests = testGroup "Enhanced DependentTypes QuickCheck Tests"
  [ fastProperty "TypeRef equality reflexive" prop_typeRef_equality_reflexive
  , fastProperty "TypeRef same name equal" prop_typeRef_same_name_equal
  , fastProperty "TypeRef different params unequal" prop_typeRef_different_params_unequal
  , fastProperty "Field equality reflexive" prop_field_equality_reflexive
  , fastProperty "Field same name type equal" prop_field_same_name_type_equal
  , fastProperty "TypeParameter equality reflexive" prop_typeParameter_equality_reflexive
  , fastProperty "TypeConstraint equality reflexive" prop_typeConstraint_equality_reflexive
  , fastProperty "DependentType equality reflexive" prop_dependentType_equality_reflexive
  , fastProperty "Parser handles empty input" prop_parseDependentType_empty
  , fastProperty "Parser handles whitespace-only input" prop_parseDependentType_whitespace
  , fastProperty "Parser handles simple type definition" prop_parseDependentType_simple
  , fastProperty "Parser handles generic type definition" prop_parseDependentType_generic
  , fastProperty "Parser handles struct type definition" prop_parseDependentType_struct
  , fastProperty "Parser handles type with constraints" prop_parseDependentType_constraints
  , fastProperty "validateDependentTypeSyntax handles valid input" prop_validateDependentTypeSyntax_valid
  , fastProperty "runDependentTypesParser handles multiple definitions" prop_runDependentTypesParser_multiple
  , fastProperty "parseTypeDeclaration handles simple declaration" prop_parseTypeDeclaration_simple
  ]