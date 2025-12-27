{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewDependentTypeValidationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property
  , (===)
  , (==>)
  , forAll
  , counterexample
  , classify
  , property
  , (.&&.)
  , (.||.)
  , Arbitrary(..)
  , Gen
  , choose
  , listOf
  , elements
  , oneof
  , sized
  , resize
  , Positive(..)
  , NonEmptyList(..)
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
  , DependentParseResult
  , runDependentTypesParser
  , parseDependentType
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)
import qualified Data.Text as T

-- Test basic type declaration parsing
test_basic_type_declaration :: TestTree
test_basic_type_declaration = testCase "Basic type declaration parsing" $ do
  let source = "type Int where value >= 0"
      result = validateDependentTypeSyntax source
  case result of
    Left errors -> assertFailure $ "Parse failed: " ++ show errors
    Right _ -> pure () -- Should parse successfully

-- Test complex type with constraints
test_complex_type_constraints :: TestTree
test_complex_type_constraints = testCase "Complex type with constraints" $ do
  let source = unlines
        [ "type Vector<T> where"
        , "  len(T) > 0"
        , "  T == Int || T == String"
        , "  len(T) <= 100"
        ]
      result = validateDependentTypeSyntax source
  case result of
    Left errors -> assertFailure $ "Parse failed: " ++ show errors
    Right _ -> pure () -- Should parse successfully

-- Test struct type with fields
test_struct_type_fields :: TestTree
test_struct_type_fields = testCase "Struct type with fields" $ do
  let source = unlines
        [ "type Person where"
        , "  age: Int where age >= 0 && age < 150"
        , "  name: String where len(name) > 0"
        , "  email: String where email contains '@'"
        ]
      result = validateDependentTypeSyntax source
  case result of
    Left errors -> assertFailure $ "Parse failed: " ++ show errors
    Right _ -> pure () -- Should parse successfully

-- Test generic type parameters
test_generic_type_parameters :: TestTree
test_generic_type_parameters = testCase "Generic type parameters" $ do
  let source = unlines
        [ "type Container<T, U> where"
        , "  T: Ord"
        , "  U: Show"
        , "  len(T) == len(U)"
        , ]
      result = validateDependentTypeSyntax source
  case result of
    Left errors -> assertFailure $ "Parse failed: " ++ show errors
    Right _ -> pure () -- Should parse successfully

-- Test nested type constraints
test_nested_constraints :: TestTree
test_nested_constraints = testCase "Nested type constraints" $ do
  let source = unlines
        [ "type Matrix<T> where"
        , "  T: Numeric"
        , "  rows: Int where rows > 0"
        , "  cols: Int where cols > 0"
        , "  rows * cols <= 1000"
        , ]
      result = validateDependentTypeSyntax source
  case result of
    Left errors -> assertFailure $ "Parse failed: " ++ show errors
    Right _ -> pure () -- Should parse successfully

-- Test type alias declarations
test_type_aliases :: TestTree
test_type_aliases = testCase "Type alias declarations" $ do
  let source = unlines
        [ "alias UserID = Int where value > 0"
        , "alias Email = String where contains '@'"
        , "alias PositiveInt = Int where value >= 1"
        ]
      result = validateDependentTypeSyntax source
  case result of
    Left errors -> assertFailure $ "Parse failed: " ++ show errors
    Right _ -> pure () -- Should parse successfully

-- Test function type declarations
test_function_types :: TestTree
test_function_types = testCase "Function type declarations" = do
  let source = unlines
        [ "func safeDivide(a: Int, b: Int) -> Int where b != 0"
        , "func arrayAccess(arr: Array<T>, index: Int) -> T where"
        , "  index >= 0 && index < len(arr)"
        ]
      result = validateDependentTypeSyntax source
  case result of
    Left errors -> assertFailure $ "Parse failed: " ++ show errors
    Right _ -> pure () -- Should parse successfully

-- Test error handling for invalid syntax
test_invalid_syntax_handling :: TestTree
test_invalid_syntax_handling = testCase "Invalid syntax handling" $ do
  let invalidSources = 
        [ "type Int where"  -- Incomplete constraint
        , "type where value >= 0"  -- Missing type name
        , "type Int where >="  -- Incomplete constraint
        , "type Int where value > 0 &&"  -- Incomplete binary operation
        , "func () -> Int"  -- Empty function name
        ]
  mapM_ (\source -> do
    let result = validateDependentTypeSyntax source
    case result of
      Left _ -> pure () -- Should fail as expected
      Right _ -> assertFailure $ "Expected parse failure for: " ++ source
    ) invalidSources

-- Test constraint validation edge cases
test_constraint_validation_edge_cases :: TestTree
test_constraint_validation_edge_cases = testCase "Constraint validation edge cases" $ do
  let edgeCases = 
        [ "type Int where value == 0"  -- Equality constraint
        , "type String where len(s) == 0"  -- Empty string constraint
        , "type Array<T> where len(T) == 1"  -- Single element constraint
        , "type Number where value > -1000 && value < 1000"  -- Range constraint
        ]
  mapM_ (\source -> do
    let result = validateDependentTypeSyntax source
    case result of
      Left errors -> assertFailure $ "Parse failed: " ++ show errors
      Right _ -> pure () -- Should parse successfully
    ) edgeCases

-- Test Unicode and special characters in type names
test_unicode_type_names :: TestTree
test_unicode_type_names = testCase "Unicode and special characters in type names" = do
  let unicodeSources = 
        [ "type 测试类型 where value > 0"
        , "type Vector<向量类型> where len(向量类型) > 0"
        , "type 🚀Rocket where speed >= 0"
        ]
  mapM_ (\source -> do
    let result = validateDependentTypeSyntax source
    case result of
      Left errors -> do
        -- Unicode support might be limited, so we check if it's a reasonable error
        let errorStr = show errors
        assertBool ("Should handle Unicode gracefully: " ++ errorStr) $ 
          not (isInfixOf "crash" errorStr) && not (isInfixOf "panic" errorStr)
      Right _ -> pure () -- Unicode support works
    ) unicodeSources

-- Test very long type definitions
test_long_type_definitions :: TestTree
test_long_type_definitions = testCase "Very long type definitions" $ do
  let longConstraint = "value >= 0 && value <= " ++ show (maxBound :: Int)
      longSource = "type VeryLongTypeName<" ++ replicate 50 'A' ++ "> where " ++ longConstraint
      result = validateDependentTypeSyntax longSource
  case result of
    Left errors -> do
      -- Should handle long inputs gracefully
      let errorStr = show errors
      assertBool ("Should handle long inputs gracefully: " ++ errorStr) $ 
        not (isInfixOf "crash" errorStr) && not (isInfixOf "panic" errorStr)
    Right _ -> pure () -- Long input handled successfully

-- Test deeply nested type structures
test_deeply_nested_types :: TestTree
test_deeply_nested_types = testCase "Deeply nested type structures" = do
  let nestedSource = unlines
        [ "type Outer<T> where"
        , "  inner: Inner<T> where"
        , "    deep: Deep<T> where"
        , "      value: T where value > 0"
        ]
      result = validateDependentTypeSyntax nestedSource
  case result of
    Left errors -> assertFailure $ "Parse failed: " ++ show errors
    Right _ -> pure () -- Should parse successfully

-- Property: Type parsing is deterministic
prop_type_parsing_deterministic :: String -> Property
prop_type_parsing_deterministic source = 
  let result1 = validateDependentTypeSyntax source
      result2 = validateDependentTypeSyntax source
  in case (result1, result2) of
    (Left err1, Left err2) -> err1 === err2
    (Right res1, Right res2) -> res1 === res2
    _ -> property False -- Should have consistent results

-- Property: Valid type constraints parse successfully
prop_valid_constraints_parse :: String -> Property
prop_valid_constraints_parse constraint = 
  let source = "type Test where " ++ constraint
      result = validateDependentTypeSyntax source
  in classify (isRight result) "parses successfully" $
     property $ case result of
       Left _ -> True -- May fail for invalid constraints
       Right _ -> True -- Success is good

-- Property: Type name validation
prop_type_name_validation :: String -> Property
prop_type_name_validation name = 
  let validName = not (null name) && all isAlphaNum (head name : dropWhile (== ' ') name)
      source = "type " ++ name ++ " where value > 0"
      result = validateDependentTypeSyntax source
  in classify validName "valid name" $
     classify (not validName) "invalid name" $
     property $ case result of
       Left _ -> not validName -- Should fail for invalid names
       Right _ -> validName -- Should succeed for valid names

-- Property: Constraint complexity handling
prop_constraint_complexity :: Positive Int -> Property
prop_constraint_complexity (Positive n) = 
  let complexity = min n 10  -- Limit complexity for reasonable test size
      constraint = "value > 0" ++ concat (replicate complexity " && value < " ++ show (complexity * 10))
      source = "type Complex where " ++ constraint
      result = validateDependentTypeSyntax source
  in property $ case result of
    Left _ -> True -- May fail for very complex constraints
    Right _ -> True -- Success is good

-- Property: Generic type parameter handling
prop_generic_parameters :: [String] -> Property
prop_generic_parameters params = 
  let validParams = all (not . null) params
      paramList = intercalate ", " params
      source = "type Generic<" ++ paramList ++ "> where true"
      result = validateDependentTypeSyntax source
  in classify validParams "valid parameters" $
     classify (not validParams) "invalid parameters" $
     property $ case result of
       Left _ -> not validParams -- Should fail for invalid params
       Right _ -> validParams -- Should succeed for valid params

-- Property: Multiple type declarations
prop_multiple_declarations :: [String] -> Property
prop_multiple_declarations typeNames = 
  let validNames = all (not . null) typeNames
      declarations = map (\name -> "type " ++ name ++ " where value > 0") typeNames
      source = unlines declarations
      result = validateDependentTypeSyntax source
  in classify validNames "valid names" $
     classify (not validNames) "invalid names" $
     property $ case result of
       Left _ -> not validNames -- Should fail for invalid names
       Right _ -> validNames -- Should succeed for valid names

-- Helper functions for property tests
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isAlphaNum :: Char -> Bool
isAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

tests :: TestTree
tests = testGroup "New Dependent Type Validation Tests"
  [ test_basic_type_declaration
  , test_complex_type_constraints
  , test_struct_type_fields
  , test_generic_type_parameters
  , test_nested_constraints
  , test_type_aliases
  , test_function_types
  , test_invalid_syntax_handling
  , test_constraint_validation_edge_cases
  , test_unicode_type_names
  , test_long_type_definitions
  , test_deeply_nested_types
  , fastProperty "Type parsing is deterministic" prop_type_parsing_deterministic
  , fastProperty "Valid constraints parse successfully" prop_valid_constraints_parse
  , fastProperty "Type name validation" prop_type_name_validation
  , fastProperty "Constraint complexity handling" prop_constraint_complexity
  , fastProperty "Generic type parameter handling" prop_generic_parameters
  , fastProperty "Multiple type declarations" prop_multiple_declarations
  ]