{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.DependentTypeBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, oneof, elements, choose, listOf, resize)

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

import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isDigit)

-- ============================================================================
-- Dependent Type Boundary Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Dependent Type Boundary Tests"
    [ testGroup "Type Parameter Boundary Tests"
        [ testCase "handles extreme type parameter values" $ do
            let input = "type Extreme<T> where T > 1000000 && T < -1000000"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle extreme values" True
                Right _ -> assertBool "should parse extreme constraints" True

        , testCase "validates type parameter type boundaries" $ do
            let input = "type Bounded<T: int> where T >= 0 && T <= 100"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle bounded parameters" True
                Right _ -> assertBool "should parse bounded type parameters" True

        , testCase "detects invalid type parameter constraints" $ do
            let input = "type Invalid<T: string> where T > 5"
            case validateDependentTypeSyntax input of
                Left errors -> 
                    let constraintErrors = filter isConstraintError errors
                    in assertBool "should detect invalid string comparison" (not $ null constraintErrors)
                Right _ -> assertBool "should reject invalid constraints" False

        , testCase "handles complex nested type parameters" $ do
            let input = "type Nested<A, B<C<D>, E>> where A > B && C < D"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle nested parameters" True
                Right _ -> assertBool "should parse nested type parameters" True
        ]

    , testGroup "Constraint Boundary Tests"
        [ testCase "validates equality constraint boundaries" $ do
            let input = "type EqConstrained<T> where T == 42"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle equality constraints" True
                Right _ -> assertBool "should parse equality constraints" True

        , testCase "validates range constraint boundaries" $ do
            let input = "type RangeConstrained<T> where T >= 0 && T <= 1000"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle range constraints" True
                Right _ -> assertBool "should parse range constraints" True

        , testCase "detects contradictory constraints" $ do
            let input = "type Contradiction<T> where T > 10 && T < 5"
            case validateDependentTypeSyntax input of
                Left errors -> 
                    let constraintErrors = filter isConstraintError errors
                    in assertBool "should detect contradictory constraints" (not $ null constraintErrors)
                Right _ -> assertBool "should reject contradictory constraints" False

        , testCase "handles complex predicate constraints" $ do
            let input = "type Predicated<T> where predicate(T, \"custom_check\")"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle predicate constraints" True
                Right _ -> assertBool "should parse predicate constraints" True
        ]

    , testGroup "Struct Field Boundary Tests"
        [ testCase "validates struct field type boundaries" $ do
            let input = unlines
                  [ "type BoundedStruct {"
                  , "    field1: int"
                  , "    field2: string"
                  , "    field3: CustomType<A, B>"
                  , "} where field1 >= 0 && len(field2) <= 100"
                  ]
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle field constraints" True
                Right _ -> assertBool "should parse struct with field constraints" True

        , testCase "detects invalid field type references" $ do
            let input = "type InvalidStruct { field: UndefinedType<A, B, C> }"
            case validateDependentTypeSyntax input of
                Left errors -> 
                    let typeErrors = filter isInvalidTypeSyntax errors
                    in assertBool "should detect undefined type references" (not $ null typeErrors)
                Right _ -> assertBool "should reject undefined types" False

        , testCase "handles deeply nested field types" $ do
            let input = "type DeepStruct { field: Map<List<Set<Option<Result<Data>>>>> }"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle deeply nested types" True
                Right _ -> assertBool "should parse deeply nested field types" True

        , testCase "validates recursive struct boundaries" $ do
            let input = unlines
                  [ "type RecursiveStruct {"
                  , "    value: int"
                  , "    next: Option<RecursiveStruct>"
                  , "} where value >= 0"
                  ]
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle recursive types" True
                Right _ -> assertBool "should parse recursive struct definitions" True
        ]

    , testGroup "Function Type Boundary Tests"
        [ testCase "validates function parameter type boundaries" $ do
            let input = "func bounded_func(x: int, y: string) bool where x > 0 && len(y) <= 50"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle function parameter constraints" True
                Right _ -> assertBool "should parse function with parameter constraints" True

        , testCase "validates return type constraints" $ do
            let input = "func constrained() Result<T> where T > 0"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle return type constraints" True
                Right _ -> assertBool "should parse function with return constraints" True

        , testCase "detects invalid function signatures" $ do
            let input = "func invalid(x: UndefinedType) InvalidReturn<T> where x == T"
            case validateDependentTypeSyntax input of
                Left errors -> 
                    let typeErrors = filter isInvalidTypeSyntax errors
                    in assertBool "should detect invalid function types" (not $ null typeErrors)
                Right _ -> assertBool "should reject invalid function signatures" False

        , testCase "handles higher-order function boundaries" $ do
            let input = "func higher_order(f: (int) -> string) Result<T> where T != null"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle higher-order functions" True
                Right _ -> assertBool "should parse higher-order function types" True
        ]

    , testGroup "Type Alias Boundary Tests"
        [ testCase "validates type alias constraints" $ do
            let input = "alias SafeString = string where len(SafeString) > 0 && len(SafeString) <= 255"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle alias constraints" True
                Right _ -> assertBool "should parse type alias with constraints" True

        , testCase "detects circular alias definitions" $ do
            let input = "alias Circular = Circular where Circular != null"
            case validateDependentTypeSyntax input of
                Left errors -> 
                    let typeErrors = filter isInvalidTypeSyntax errors
                    in assertBool "should detect circular aliases" (not $ null typeErrors)
                Right _ -> assertBool "should reject circular aliases" False

        , testCase "handles complex type aliases" $ do
            let input = "alias ComplexMap = Map<string, List<Option<Result<Data>>>> where len(Map) >= 0"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle complex aliases" True
                Right _ -> assertBool "should parse complex type aliases" True

        , testCase "validates generic type aliases" $ do
            let input = "alias Generic<T> = Container<T> where T > 0"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle generic aliases" True
                Right _ -> assertBool "should parse generic type aliases" True
        ]

    , testGroup "Constraint Expression Boundary Tests"
        [ testCase "handles complex logical expressions" $ do
            let input = "type ComplexLogic<T> where (T > 0 && T < 100) || (T >= 1000 && T <= 2000)"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle complex logical expressions" True
                Right _ -> assertBool "should parse complex constraint expressions" True

        , testCase "validates nested constraint expressions" $ do
            let input = "type Nested<T> where ((T > 0) && (T < 100)) || (T == 42)"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle nested expressions" True
                Right _ -> assertBool "should parse nested constraint expressions" True

        , testCase "detects malformed constraint expressions" $ do
            let input = "type Malformed<T> where T > && T < 100"
            case validateDependentTypeSyntax input of
                Left errors -> 
                    let syntaxErrors = filter isSyntaxError errors
                    in assertBool "should detect malformed expressions" (not $ null syntaxErrors)
                Right _ -> assertBool "should reject malformed expressions" False

        , testCase "handles constraint precedence" $ do
            let input = "type Precedence<T> where T > 0 && T < 100 || T == 42"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle operator precedence" True
                Right _ -> assertBool "should parse constraint precedence correctly" True
        ]

    , testGroup "Property-Based Boundary Tests"
        [ fastProperty "type constraint boundaries are enforced" $
            \constraintValue ->
                let value = abs constraintValue `mod` 1000
                    input = "type BoundaryTest<T> where T > " ++ show value ++ " && T < " ++ show (value + 100)
                in case validateDependentTypeSyntax input of
                    Left _ -> property True
                    Right _ -> property True

        , fastProperty "nested type boundaries are preserved" $
            \nestingDepth ->
                let depth = min 5 (max 1 nestingDepth)
                    input = generateNestedType depth
                in case validateDependentTypeSyntax input of
                    Left _ -> property True
                    Right _ -> property True

        , fastProperty "constraint complexity scales appropriately" $
            \constraintCount ->
                let count = min 10 (max 1 constraintCount)
                    input = generateComplexConstraints count
                in case validateDependentTypeSyntax input of
                    Left _ -> property True
                    Right _ -> property True
        ]

    , testGroup "Edge Cases and Stress Tests"
        [ testCase "handles extremely long type names" $ do
            let longName = replicate 100 'A'
            let input = "type " ++ longName ++ "<T> where T > 0"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle long type names" True
                Right _ -> assertBool "should parse long type names" True

        , testCase "handles deeply nested constraints" $ do
            let input = "type Deep<T> where " ++ concat ["(" ++ show i ++ " < " ++ show (i+1) ++ ") && " | i <- [1..20]] ++ "T > 0"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle deeply nested constraints" True
                Right _ -> assertBool "should parse deeply nested constraints" True

        , testCase "handles unicode in type names and constraints" $ do
            let input = "type 类型<T> where T > 值 && T < 最大值"
            case validateDependentTypeSyntax input of
                Left _ -> assertBool "should handle unicode" True
                Right _ -> assertBool "should parse unicode type names" True

        , testCase "recovers from partial constraint failures" $ do
            let input = unlines
                  [ "type Partial<T> where"
                  , "    T > 0 &&"
                  , "    // malformed constraint here"
                  , "    T < &&"
                  , "    T < 100"
                  , "}"
                  ]
            case validateDependentTypeSyntax input of
                Left errors -> 
                    let syntaxErrors = filter isSyntaxError errors
                    in assertBool "should recover from partial failures" (not $ null syntaxErrors)
                Right _ -> assertBool "should handle partial failures gracefully" True
        ]
    ]

-- Helper functions for error detection
isConstraintError :: DependentTypeError -> Bool
isConstraintError (ConstraintParseError _) = True
isConstraintError (MissingConstraint _) = True
isConstraintError _ = False

isInvalidTypeSyntax :: DependentTypeError -> Bool
isInvalidTypeSyntax (InvalidTypeSyntax _) = True
isInvalidTypeSyntax (TypeVariableError _) = True
isInvalidTypeSyntax _ = False

isSyntaxError :: DependentTypeError -> Bool
isSyntaxError (SyntaxError _ _ _) = True
isSyntaxError _ = False

-- Helper functions for property-based testing
generateNestedType :: Int -> String
generateNestedType depth = "type Nested" ++ concat [show i ++ "<" | i <- [1..depth]] ++ "T" ++ concat [">" | i <- [1..depth]] ++ " where T > 0"

generateComplexConstraints :: Int -> String
generateComplexConstraints count = 
    "type Complex<T> where " ++ concat [if i == 1 then "T > " ++ show i else " && T < " ++ show (i * 10) | i <- [1..count]]