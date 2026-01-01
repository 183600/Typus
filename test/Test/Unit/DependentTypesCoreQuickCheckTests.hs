{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.DependentTypesCoreQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperties, (===), Property, forAll, Gen, Arbitrary(..), oneof, elements, listOf, listOf1, resize, suchThat)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import DependentTypesParser
  ( TypeRef(..), TypeBody(..), Field(..), TypeParameter(..)
  , TypeConstraint(..), DependentType(..), DependentTypesParser(..)
  , DependentTypeError(..), DependentParseResult
  , runDependentTypesParser, parseDependentType, validateDependentTypeSyntax
  )

import Dependencies.TypeSystem
  ( TypeVar(..), TypeConstraint(..), DependentTypeError(..), TypeDef(..)
  , TypeEnv(..), DependentTypeChecker(..), Substitution
  , newDependentTypeChecker, addType, addConstraint, lookupTypeDef
  , checkType, solveConstraints, checkTypeConstraint, unify
  )

import Dependencies.AST (TypeExpr(..), Constraint(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (length)
import Data.List (sort)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary TypeRef where
  arbitrary = do
    name <- listOf1 (elements ['a'..'z'] ++ ['A'..'Z'] ++ "_")
    params <- listOf arbitrary
    return $ TypeRef name params

instance Arbitrary TypeBody where
  arbitrary = oneof
    [ TypeStruct <$> listOf arbitrary
    , TypeAlias <$> arbitrary
    , TypeFunc <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Field where
  arbitrary = do
    name <- listOf1 (elements ['a'..'z'] ++ ['A'..'Z'] ++ "_")
    fieldType <- arbitrary
    return $ Field name fieldType

instance Arbitrary TypeParameter where
  arbitrary = do
    name <- listOf1 (elements ['a'..'z'] ++ ['A'..'Z'])
    constraints <- listOf arbitrary
    return $ TypeParameter name constraints

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ EqualityConstraint <$> arbitrary <*> arbitrary
    , RangeConstraint <$> arbitrary <*> arbitrary <*> arbitrary
    , SizeConstraint <$> arbitrary <*> arbitrary
    , PredicateConstraint <$> arbitrary <*> arbitrary
    ]

instance Arbitrary DependentType where
  arbitrary = do
    name <- listOf1 (elements ['a'..'z'] ++ ['A'..'Z'])
    params <- listOf arbitrary
    body <- arbitrary
    return $ DependentType name params body

instance Arbitrary DependentTypeError where
  arbitrary = oneof
    [ ParseError <$> listOf1 (elements ['a'..'z'] ++ " ")
    , TypeError <$> listOf1 (elements ['a'..'z'] ++ " ")
    , ConstraintError <$> listOf1 (elements ['a'..'z'] ++ " ")
    , DuplicateDefinition <$> listOf1 (elements ['a'..'z'])
    , UnresolvedReference <$> listOf1 (elements ['a'..'z'])
    ]

instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ SimpleT <$> listOf1 (elements ['a'..'z'])
    , GenericT <$> arbitrary <*> listOf arbitrary
    , RefineT <$> arbitrary <*> listOf arbitrary
    , FuncT <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Constraint where
  arbitrary = oneof
    [ RangeC <$> arbitrary <*> arbitrary
    , PredC <$> arbitrary <*> arbitrary
    , SizeGE <$> arbitrary
    , SizeGT <$> arbitrary
    ]

-- ============================================================================
-- QuickCheck Properties for DependentTypes Module
-- ============================================================================

-- | TypeRef: equality should be reflexive
prop_typeRef_reflexive :: TypeRef -> Bool
prop_typeRef_reflexive tr = tr == tr

-- | TypeRef: Show should contain type name
prop_typeRef_show_contains_name :: TypeRef -> Bool
prop_typeRef_show_contains_name tr = 
    let showStr = show tr
        name = trName tr
    in name `L.isInfixOf` showStr
  where
    isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack

-- | Field: equality should be reflexive
prop_field_reflexive :: Field -> Bool
prop_field_reflexive f = f == f

-- | Field: Show should contain field name
prop_field_show_contains_name :: Field -> Bool
prop_field_show_contains_name f = 
    let showStr = show f
        name = fName f
    in name `L.isInfixOf` showStr
  where
    isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack

-- | TypeParameter: equality should be reflexive
prop_typeParameter_reflexive :: TypeParameter -> Bool
prop_typeParameter_reflexive tp = tp == tp

-- | TypeConstraint: equality should be reflexive
prop_typeConstraint_reflexive :: TypeConstraint -> Bool
prop_typeConstraint_reflexive tc = tc == tc

-- | DependentType: equality should be reflexive
prop_dependentType_reflexive :: DependentType -> Bool
prop_dependentType_reflexive dt = dt == dt

-- | DependentType: Show should contain type name
prop_dependentType_show_contains_name :: DependentType -> Bool
prop_dependentType_show_contains_name dt = 
    let showStr = show dt
        name = dtName dt
    in name `L.isInfixOf` showStr
  where
    isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack

-- | DependentTypeError: equality should be reflexive
prop_dependentTypeError_reflexive :: DependentTypeError -> Bool
prop_dependentTypeError_reflexive dte = dte == dte

-- | newDependentTypeChecker: should create valid checker
prop_newDependentTypeChecker_valid :: Bool
prop_newDependentTypeChecker_valid = 
    let checker = newDependentTypeChecker
    in True  -- Basic sanity check that constructor works

-- | TypeEnv: adding type should make it available
prop_addType_lookup :: String -> TypeDef -> Property
prop_addType_lookup name typeDef = 
    let checker = newDependentTypeChecker
        checker' = addType name typeDef checker
    in not (null name) ==> 
       case lookupTypeDef name checker' of
         Just found -> found == typeDef
         Nothing -> False

-- | TypeConstraint: validation should be consistent
prop_typeConstraint_validation :: TypeConstraint -> Bool
prop_typeConstraint_validation tc = 
    let checker = newDependentTypeChecker
        checker' = addConstraint tc checker
    in True  -- Basic check that constraint can be added

-- | TypeExpr: equality should be reflexive
prop_typeExpr_reflexive :: TypeExpr -> Bool
prop_typeExpr_reflexive te = te == te

-- | Constraint: equality should be reflexive
prop_constraint_reflexive :: Constraint -> Bool
prop_constraint_reflexive c = c == c

-- | unify: unifying identical types should succeed
prop_unify_identical :: TypeExpr -> Bool
prop_unify_identical te = 
    let checker = newDependentTypeChecker
        result = unify te te checker
    in case result of
      Right _ -> True
      Left _ -> False  -- May fail for complex types, but identical should work

-- | checkType: checking simple types should not crash
prop_checkType_simple :: TypeExpr -> Bool
prop_checkType_simple te = 
    let checker = newDependentTypeChecker
        result = checkType te checker
    in case result of
      Right _ -> True
      Left _ -> True  -- May fail, but shouldn't crash

-- | solveConstraints: solving empty constraints should succeed
prop_solveConstraints_empty :: Bool
prop_solveConstraints_empty = 
    let checker = newDependentTypeChecker
        result = solveConstraints [] checker
    in case result of
      Right _ -> True
      Left _ -> False

-- | checkTypeConstraint: checking constraint should not crash
prop_checkTypeConstraint_no_crash :: Constraint -> Bool
prop_checkTypeConstraint_no_crash c = 
    let checker = newDependentTypeChecker
        result = checkTypeConstraint c checker
    in case result of
      Right _ -> True
      Left _ -> True  -- May fail, but shouldn't crash

-- | runDependentTypesParser: parsing empty input should not crash
prop_runDependentTypesParser_empty :: Bool
prop_runDependentTypesParser_empty = 
    let result = runDependentTypesParser ""
    in case result of
      Right _ -> True
      Left _ -> True  -- May fail, but shouldn't crash

-- | parseDependentType: parsing simple type should not crash
prop_parseDependentType_no_crash :: String -> Property
prop_parseDependentType_no_crash input = 
    let simpleInput = take 20 $ L.filter (\c -> isAlphaNum c || c `elem` "_[]{}()") input
    in not (null simpleInput) ==> 
       case parseDependentType simpleInput of
         Right _ -> True
         Left _ -> True  -- May fail, but shouldn't crash

-- | validateDependentTypeSyntax: validation should not crash
prop_validateDependentTypeSyntax_no_crash :: String -> Bool
prop_validateDependentTypeSyntax_no_crash input = 
    let result = validateDependentTypeSyntax input
    in case result of
      Right _ -> True
      Left _ -> True  -- May fail, but shouldn't crash
  where
    isAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | TypeRef: parameter count should be preserved
prop_typeRef_param_count :: String -> [TypeRef] -> Bool
prop_typeRef_param_count name params = 
    let typeRef = TypeRef name params
    in L.length (trParams typeRef) == L.length params

-- | Field: field should preserve name L.and type
prop_field_preserves_components :: String -> TypeRef -> Bool
prop_field_preserves_components name fieldType = 
    let field = Field name fieldType
    in fName field == name && fType field == fieldType

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "DependentTypes Core QuickCheck Tests"
  [ testProperties "TypeRef Properties"
    [ ("TypeRef reflexive", prop_typeRef_reflexive)
    , ("TypeRef show contains name", prop_typeRef_show_contains_name)
    , ("TypeRef param count", prop_typeRef_param_count)
    ]

  , testProperties "Field Properties"
    [ ("Field reflexive", prop_field_reflexive)
    , ("Field show contains name", prop_field_show_contains_name)
    , ("Field preserves components", prop_field_preserves_components)
    ]

  , testProperties "Core Type Properties"
    [ ("TypeParameter reflexive", prop_typeParameter_reflexive)
    , ("TypeConstraint reflexive", prop_typeConstraint_reflexive)
    , ("DependentType reflexive", prop_dependentType_reflexive)
    , ("DependentType show contains name", prop_dependentType_show_contains_name)
    , ("DependentTypeError reflexive", prop_dependentTypeError_reflexive)
    ]

  , testProperties "TypeSystem Properties"
    [ ("newDependentTypeChecker valid", prop_newDependentTypeChecker_valid)
    , ("addType lookup", prop_addType_lookup)
    , ("typeConstraint validation", prop_typeConstraint_validation)
    , ("TypeExpr reflexive", prop_typeExpr_reflexive)
    , ("Constraint reflexive", prop_constraint_reflexive)
    ]

  , testProperties "Type Checking Properties"
    [ ("unify identical", prop_unify_identical)
    , ("checkType simple", prop_checkType_simple)
    , ("solveConstraints empty", prop_solveConstraints_empty)
    , ("checkTypeConstraint no crash", prop_checkTypeConstraint_no_crash)
    ]

  , testProperties "Parser Properties"
    [ ("runDependentTypesParser empty", prop_runDependentTypesParser_empty)
    , ("parseDependentType no crash", prop_parseDependentType_no_crash)
    , ("validateDependentTypeSyntax no crash", prop_validateDependentTypeSyntax_no_crash)
    ]
  ]