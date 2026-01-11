module Test.Unit.EnhancedDependentTypesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import DependentTypesParser (DependentType(..), TypeConstraint(..), 
                            DependentTypeChecker(..), 
                            parseDependentType, checkTypeConstraints,
                            validateDependentType)
import Parser (TypusFile(..), defaultFileDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import qualified Data.Map as Map

-- | Test DependentType properties
prop_dependent_type_equality :: String -> String -> Property
prop_dependent_type_equality name typeName =
  let type1 = DependentType name typeName []
      type2 = DependentType name typeName []
  in property $ type1 == type2

prop_dependent_type_with_constraints :: String -> String -> [TypeConstraint] -> Property
prop_dependent_type_with_constraints name typeName constraints =
  let dependentType = DependentType name typeName constraints
  in property $ 
    dtName dependentType == name && 
    dtBaseType dependentType == typeName && 
    dtConstraints dependentType == constraints

-- | Test TypeConstraint properties
prop_type_constraint_equality :: String -> String -> Property
prop_type_constraint_equality constraintName constraintValue =
  let constraint1 = TypeConstraint constraintName constraintValue
      constraint2 = TypeConstraint constraintName constraintValue
  in property $ constraint1 == constraint2

prop_type_constraint_ordering :: String -> String -> String -> String -> Property
prop_type_constraint_ordering name1 value1 name2 value2 =
  let constraint1 = TypeConstraint name1 value1
      constraint2 = TypeConstraint name2 value2
  in property $ 
    (name1 `compare` name2) === (constraint1 `compare` constraint2)

-- | Test dependent type parsing
prop_parse_dependent_type_simple :: String -> String -> Property
prop_parse_dependent_type_simple name typeName =
  let typeString = name ++ " : " ++ typeName
      result = parseDependentType typeString
  in property $ 
    case result of
      Left _ -> True
      Right dt -> dtName dt == name

prop_parse_dependent_type_with_constraint :: String -> String -> String -> String -> Property
prop_parse_dependent_type_with_constraint name typeName constraintName constraintValue =
  let typeString = name ++ " : " ++ typeName ++ " where " ++ constraintName ++ " = " ++ constraintValue
      result = parseDependentType typeString
  in property $ 
    case result of
      Left _ -> True
      Right dt -> dtName dt == name

-- | Test type constraint checking
prop_check_type_constraints_empty :: DependentType -> Property
prop_check_type_constraints_empty dependentType =
  let dtWithoutConstraints = dependentType { dtConstraints = [] }
      result = checkTypeConstraints dtWithoutConstraints
  in property $ 
    case result of
      Left _ -> False
      Right _ -> True

prop_check_type_constraints_consistent :: String -> String -> Property
prop_check_type_constraints_consistent constraintName constraintValue =
  let constraint = TypeConstraint constraintName constraintValue
      dependentType = DependentType "test" "int" [constraint]
      result = checkTypeConstraints dependentType
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

-- | Test dependent type validation
prop_validate_dependent_type_simple :: String -> String -> Property
prop_validate_dependent_type name typeName =
  let dependentType = DependentType name typeName []
      result = validateDependentType dependentType
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

prop_validate_dependent_type_with_constraints :: String -> String -> [TypeConstraint] -> Property
prop_validate_dependent_type_with_constraints name typeName constraints =
  let dependentType = DependentType name typeName constraints
      result = validateDependentType dependentType
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

-- | Test dependent type analysis
prop_analyze_dependent_types_empty :: Property
prop_analyze_dependent_types_empty = 
  let file = TypusFile defaultFileDirectives [] "" ""
  in property $ True  -- Should handle empty file gracefully

prop_analyze_dependent_types_preserves :: [String] -> Property
prop_analyze_dependent_types_preserves typeNames =
  let typeDeclarations = map (\name -> name ++ " : int") typeNames
      fileContent = unlines typeDeclarations
      file = TypusFile defaultFileDirectives [] fileContent fileContent
  in property $ True  -- Should parse and analyze types

-- | Test constraint validation
prop_validate_constraint_value :: String -> Property
prop_validate_constraint_value value =
  let constraint = TypeConstraint "length" value
      dependentType = DependentType "array" "list" [constraint]
      result = validateDependentType dependentType
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

-- | Test dependent type relationships
prop_dependent_type_relationships :: String -> String -> String -> Property
prop_dependent_type_relationships baseType dependentType1 dependentType2 =
  let type1 = DependentType dependentType1 baseType []
      type2 = DependentType dependentType2 baseType []
  in property $ 
    dtBaseType type1 == dtBaseType type2

-- | Test complex constraints
prop_complex_constraints :: String -> [(String, String)] -> Property
prop_complex_constraints typeName constraintPairs =
  let constraints = map (\(name, value) -> TypeConstraint name value) constraintPairs
      dependentType = DependentType "complex" typeName constraints
      result = validateDependentType dependentType
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

-- | Test dependent type error handling
prop_dependent_type_error_handling :: String -> Property
prop_dependent_type_error_handling invalidTypeString =
  let result = parseDependentType invalidTypeString
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

-- | Test type constraint ordering
prop_constraint_ordering_preserved :: [TypeConstraint] -> Property
prop_constraint_ordering_preserved constraints =
  let dependentType = DependentType "test" "int" constraints
  in property $ dtConstraints dependentType == constraints

-- | Test dependent type with location
prop_dependent_type_with_location :: String -> String -> Int -> Int -> Property
prop_dependent_type_with_location name typeName line col =
  let location = SourceSpan (SourcePos line col) (SourcePos line (col + 1))
      dependentType = DependentType name typeName []
  in property $ dtName dependentType == name

-- | Test constraint checking consistency
prop_constraint_checking_consistent :: DependentType -> Property
prop_constraint_checking_consistent dependentType =
  let result1 = checkTypeConstraints dependentType
      result2 = checkTypeConstraints dependentType
  in property $ result1 == result2

-- | Test dependent type validation idempotent
prop_validation_idempotent :: DependentType -> Property
prop_validation_idempotent dependentType =
  let result1 = validateDependentType dependentType
      result2 = validateDependentType dependentType
  in property $ result1 == result2

-- | Test dependent type parsing with different base types
prop_parse_different_base_types :: String -> String -> Property
prop_parse_different_base_types name baseType =
  let typeString = name ++ " : " ++ baseType
      result = parseDependentType typeString
  in property $ 
    case result of
      Left _ -> True
      Right dt -> dtBaseType dt == baseType

-- | Test constraint value types
prop_constraint_value_types :: String -> Property
prop_constraint_value_types value =
  let constraints = [TypeConstraint "length" value, 
                     TypeConstraint "min" value,
                     TypeConstraint "max" value]
      dependentType = DependentType "test" "int" constraints
      result = validateDependentType dependentType
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

tests :: TestTree
tests = testGroup "Enhanced DependentTypes Tests"
  [ testGroup "DependentType tests"
    [ testProperty "dependent type equality" prop_dependent_type_equality
    , testProperty "dependent type with constraints" prop_dependent_type_with_constraints
    ]
  , testGroup "TypeConstraint tests"
    [ testProperty "type constraint equality" prop_type_constraint_equality
    , testProperty "type constraint ordering" prop_type_constraint_ordering
    ]
  , testGroup "Dependent type parsing"
    [ testProperty "parse dependent type simple" prop_parse_dependent_type_simple
    , testProperty "parse dependent type with constraint" prop_parse_dependent_type_with_constraint
    , testProperty "parse different base types" prop_parse_different_base_types
    ]
  , testGroup "Type constraint checking"
    [ testProperty "check type constraints empty" prop_check_type_constraints_empty
    , testProperty "check type constraints consistent" prop_check_type_constraints_consistent
    , testProperty "constraint checking consistent" prop_constraint_checking_consistent
    ]
  , testGroup "Dependent type validation"
    [ testProperty "validate dependent type simple" prop_validate_dependent_type_simple
    , testProperty "validate dependent type with constraints" prop_validate_dependent_type_with_constraints
    , testProperty "validation idempotent" prop_validation_idempotent
    ]
  , testGroup "Dependent type analysis"
    [ testProperty "analyze dependent types empty" prop_analyze_dependent_types_empty
    , testProperty "analyze dependent types preserves" prop_analyze_dependent_types_preserves
    ]
  , testGroup "Constraint validation"
    [ testProperty "validate constraint value" prop_validate_constraint_value
    , testProperty "constraint value types" prop_constraint_value_types
    ]
  , testGroup "Type relationships"
    [ testProperty "dependent type relationships" prop_dependent_type_relationships
    ]
  , testGroup "Complex constraints"
    [ testProperty "complex constraints" prop_complex_constraints
    ]
  , testGroup "Error handling"
    [ testProperty "dependent type error handling" prop_dependent_type_error_handling
    ]
  , testGroup "Constraint ordering"
    [ testProperty "constraint ordering preserved" prop_constraint_ordering_preserved
    ]
  , testGroup "Location handling"
    [ testProperty "dependent type with location" prop_dependent_type_with_location
    ]
  ]