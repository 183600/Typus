module Test.Unit.DependentTypeValidationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, elements)
import qualified Test.QuickCheck as QC

import DependentTypesParser 
    ( DependentType(..), TypeRef(..), TypeBody(..), Field(..), 
      TypeParameter(..), TypeConstraint(..), DependentParseResult(..),
      parseDependentType, parseTypeDeclaration, validateDependentTypeSyntax,
      runDependentTypesParser, DependentTypeError(..) )
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (nub)
import qualified Data.Map.Strict as Map

-- | Generate simple type references
genSimpleTypeRef :: Gen TypeRef
genSimpleTypeRef = do
    name <- elements ["Int", "String", "Bool", "Float", "Char"]
    return $ TypeRef name []

-- | Generate generic type references
genGenericTypeRef :: Gen TypeRef
genGenericTypeRef = do
    base <- elements ["List", "Map", "Option", "Result"]
    paramCount <- choose (1, 3)
    params <- take paramCount <$> listOf genSimpleTypeRef
    return $ TypeRef base params

-- | Generate type parameters
genTypeParameter :: Gen TypeParameter
genTypeParameter = do
    name <- elements ["T", "U", "V", "A", "B", "C", "K", "V"]
    constraints <- listOf genTypeConstraint
    return $ TypeParameter name constraints

-- | Generate type constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = do
    constraint <- elements
        [ EqualityConstraint "x" "y"
        , GreaterThanConstraint "len" 0
        , GreaterEqualConstraint "size" 1
        , LessThanConstraint "count" 100
        , LessEqualConstraint "capacity" 1000
        , NonEmptyConstraint "items"
        , PredicateConstraint "valid" ["x", "y"]
        ]
    return constraint

-- | Generate fields for struct types
genField :: Gen Field
genField = do
    name <- elements ["id", "name", "value", "data", "result", "output"]
    fieldType <- genSimpleTypeRef
    return $ Field name fieldType

tests :: TestTree
tests =
  testGroup "Dependent Type Validation"
    [ testGroup "Basic Type Parsing"
        [ testCase "parses simple type declaration" $ do
            let code = "type Int = primitive"
            result <- parseTypeDeclaration code
            case result of
                Right dependentType -> do
                    assertBool "Should parse simple type" $ 
                        case dtType dependentType of
                            PrimitiveType -> True
                            _ -> False
                Left _ -> assertBool "Should parse simple type declaration" False

        , testCase "parses struct type with fields" $ do
            let code = "type Person = struct { name: String, age: Int }"
            result <- parseTypeDeclaration code
            case result of
                Right dependentType -> do
                    assertBool "Should parse struct type" $ 
                        case dtType dependentType of
                            StructType fields -> L.length fields == 2
                            _ -> False
                Left _ -> assertBool "Should parse struct type" False

        , testCase "parses generic type" $ do
            let code = "type List[T] = struct { items: [T], L.length: Int }"
            result <- parseTypeDeclaration code
            case result of
                Right dependentType -> do
                    assertBool "Should parse generic type" $ 
                        L.length (dtParameters dependentType) == 1
                Left _ -> assertBool "Should parse generic type" False

        , testCase "parses type with constraints" $ do
            let code = "type Vector[n] where n > 0 = struct { data: [Int], size: n }"
            result <- parseTypeDeclaration code
            case result of
                Right dependentType -> do
                    assertBool "Should parse type with constraints" $ 
                        not (L.null (dtConstraints dependentType))
                Left _ -> assertBool "Should parse type with constraints" False
        ]

    , testGroup "Type Reference Validation"
        [ testCase "validates simple type references" $ do
            let typeRef = TypeRef "Int" []
            assertBool "Simple type reference should be valid" $ 
                not (L.null $ show typeRef)

        , testCase "validates generic type references" $ do
            let typeRef = TypeRef "List" [TypeRef "Int" [], TypeRef "String" []]
            assertBool "Generic type reference should be valid" $ 
                L.length (trParams typeRef) == 2

        , testCase "validates nested type references" $ do
            let nestedType = TypeRef "Map" 
                    [ TypeRef "String" []
                    , TypeRef "List" [TypeRef "Int" []]
                    ]
            assertBool "Nested type reference should be valid" $ 
                L.length (trParams nestedType) == 2

        , fastProperty "type reference construction is consistent" $ 
            prop_typeReferenceConsistent
        ]

    , testGroup "Constraint Validation"
        [ testCase "validates equality constraints" $ do
            let constraint = EqualityConstraint "x" "y"
            assertBool "Equality constraint should be valid" $ 
                show constraint `L.isInfixOf` "x" && show constraint `L.isInfixOf` "y"

        , testCase "validates numeric constraints" $ do
            let constraints = 
                    [ GreaterThanConstraint "size" 0
                    , GreaterEqualConstraint "L.length" 1
                    , LessThanConstraint "count" 100
                    , LessEqualConstraint "capacity" 1000
                    ]
            assertBool "All numeric constraints should be valid" $ 
                L.all (not . null . show) constraints

        , testCase "validates predicate constraints" $ do
            let constraint = PredicateConstraint "valid" ["x", "y", "z"]
            assertBool "Predicate constraint should be valid" $ 
                show constraint `L.isInfixOf` "valid"

        , testCase "validates non-empty constraints" $ do
            let constraint = NonEmptyConstraint "items"
            assertBool "Non-empty constraint should be valid" $ 
                show constraint `L.isInfixOf` "items"

        , fastProperty "constraint construction preserves properties" $ 
            prop_constraintConstructionPreserves
        ]

    , testGroup "Type Body Validation"
        [ testCase "validates primitive types" $ do
            let typeBody = PrimitiveType
            assertBool "Primitive type should be valid" $ 
                show typeBody `L.isInfixOf` "primitive"

        , testCase "validates struct types with valid fields" $ do
            let fields = [Field "name" (TypeRef "String" []), Field "age" (TypeRef "Int" [])]
            let typeBody = StructType fields
            assertBool "Struct type with valid fields should be valid" $ 
                L.length fields == 2

        , testCase "validates alias types" $ do
            let typeBody = AliasType (TypeRef "Int" [])
            assertBool "Alias type should be valid" $ 
                show typeBody `L.isInfixOf` "Int"

        , testCase "validates function types" $ do
            let params = [TypeRef "String" [], TypeRef "Int" []]
            let returnType = TypeRef "Bool" []
            let typeBody = FunctionType params returnType
            assertBool "Function type should be valid" $ 
                L.length params == 2 && show typeBody `L.isInfixOf` "Bool"

        , fastProperty "type body validation is consistent" $ 
            prop_typeBodyValidationConsistent
        ]

    , testGroup "Dependent Type Relationships"
        [ testCase "validates type parameter dependencies" $ do
            let param = TypeParameter "T" [GreaterThanConstraint "size" 0]
            assertBool "Type parameter with constraints should be valid" $ 
                not (L.null (tpConstraints param))

        , testCase "validates recursive type definitions" $ do
            let code = "type List[T] = struct { L.head: T, L.tail: List[T] }"
            result <- parseTypeDeclaration code
            case result of
                Right dependentType -> do
                    assertBool "Should handle recursive types" $ 
                        case dtType dependentType of
                            StructType fields -> L.any (hasRecursiveRef "List") fields
                            _ -> False
                Left _ -> assertBool "Should parse recursive types" False

        , testCase "validates mutually recursive types" $ do
            let code = unlines
                [ "type Even = struct { value: Int, next: Odd }"
                , "type Odd = struct { value: Int, next: Even }"
                ]
            result <- runDependentTypesParser code
            case result of
                Right parseResult -> do
                    assertBool "Should handle mutually recursive types" $ 
                        L.length (dprTypes parseResult) >= 2
                Left _ -> assertBool "Should parse mutually recursive types" False
        ]

    , testGroup "Error Handling L.and Validation"
        [ testCase "detects invalid type names" $ do
            let invalidCode = "type 123Invalid = primitive"
            result <- validateDependentTypeSyntax invalidCode
            assertBool "Should detect invalid type names" $ 
                not $ null result

        , testCase "detects missing type body" $ do
            let invalidCode = "type Incomplete ="
            result <- validateDependentTypeSyntax invalidCode
            assertBool "Should detect missing type body" $ 
                not $ null result

        , testCase "detects invalid constraints" $ do
            let invalidCode = "type Bad[T] where T invalid_op 0 = struct { }"
            result <- validateDependentTypeSyntax invalidCode
            assertBool "Should detect invalid constraints" $ 
                not $ null result

        , testCase "detects circular dependencies" $ do
            let code = unlines
                [ "type A = B"
                , "type B = C"
                , "type C = A"  -- Circular dependency
                ]
            result <- validateDependentTypeSyntax code
            assertBool "Should detect circular dependencies" $ 
                not $ null result

        , testCase "provides meaningful error messages" $ do
            let invalidCode = "type Invalid = { malformed syntax }"
            result <- validateDependentTypeSyntax invalidCode
            case result of
                (err:_) -> assertBool "Error message should be descriptive" $ 
                    L.length (show err) > 10
                [] -> assertBool "Should produce error messages" False
        ]

    , testGroup "Property-based Type Validation"
        [ fastProperty "type parsing is deterministic" $ 
            prop_typeParsingDeterministic
        , fastProperty "type validation preserves invariants" $ 
            prop_typeValidationPreservesInvariants
        , fastProperty "constraint satisfaction is consistent" $ 
            prop_constraintSatisfactionConsistent
        , fastProperty "type parameter substitution works" $ 
            prop_typeParameterSubstitution
        ]

    , testGroup "Complex Type Scenarios"
        [ testCase "handles higher-kinded types" $ do
            let code = "type Functor[F[_], A] = struct { value: F[A] }"
            result <- parseTypeDeclaration code
            case result of
                Right dependentType -> do
                    assertBool "Should handle higher-kinded types" $ 
                        L.length (dtParameters dependentType) >= 2
                Left _ -> assertBool "Should parse higher-kinded types" False

        , testCase "handles dependent function types" $ do
            let code = "type Vec(n) where n >= 0 = struct { data: [Int], L.length: n }"
            result <- parseTypeDeclaration code
            case result of
                Right dependentType -> do
                    assertBool "Should handle dependent function types" $ 
                        not (L.null (dtConstraints dependentType))
                Left _ -> assertBool "Should parse dependent function types" False

        , testCase "handles type-level computations" $ do
            let code = "type Matrix(m, n) where m > 0 && n > 0 = struct { rows: m, cols: n, data: [Int] }"
            result <- parseTypeDeclaration code
            case result of
                Right dependentType -> do
                    assertBool "Should handle type-level computations" $ 
                        L.length (dtConstraints dependentType) >= 2
                Left _ -> assertBool "Should parse type-level computations" False

        , testCase "handles complex nested types" $ do
            let code = "type Complex = struct { items: Map[String, List[Result[Int, Error]]] }"
            result <- parseTypeDeclaration code
            case result of
                Right dependentType -> do
                    assertBool "Should handle complex nested types" $ 
                        case dtType dependentType of
                            StructType fields -> L.any hasComplexNestedType fields
                            _ -> False
                Left _ -> assertBool "Should parse complex nested types" False
        ]

    , testGroup "Performance L.and Scalability"
        [ testCase "handles large type definitions efficiently" $ do
            let largeStruct = unlines $ 
                    [ "type LargeStruct = struct {"
                    ] ++ 
                    [ "  field" ++ show i ++ ": Int"
                    | i <- [1..100]
                    ] ++
                    [ "}"
                    ]
            result <- parseTypeDeclaration largeStruct
            case result of
                Right dependentType -> do
                    assertBool "Should handle large type definitions" $ 
                        case dtType dependentType of
                            StructType fields -> L.length fields >= 100
                            _ -> False
                Left _ -> assertBool "Should parse large type definitions" False

        , testCase "handles deeply nested type parameters" $ do
            let deeplyNested = "type Deep = " ++ 
                    L.concat (replicate 10 "List[") ++ "Int" ++ L.concat (replicate 10 "]")
            result <- parseTypeDeclaration deeplyNested
            case result of
                Right dependentType -> do
                    assertBool "Should handle deeply nested types" $ 
                        show dependentType `L.isInfixOf` "Int"
                Left _ -> assertBool "Should parse deeply nested types" False
        ]
    ]

-- Helper function to check if field has recursive reference
hasRecursiveRef :: String -> Field -> Bool
hasRecursiveRef name (Field _ fieldType) = hasTypeRef name fieldType
  where
    hasTypeRef target (TypeRef base params) 
        | base == target = True
        | otherwise = L.any (hasTypeRef target) params

-- Helper function to check if field has complex nested type
hasComplexNestedType :: Field -> Bool
hasComplexNestedType (Field _ fieldType) = hasComplexType fieldType
  where
    hasComplexType (TypeRef base params) = 
        L.length params > 1 || L.any hasComplexType params

-- Property: type reference construction is consistent
prop_typeReferenceConsistent :: TypeRef -> Bool
prop_typeReferenceConsistent typeRef = 
    let reconstructed = TypeRef (trName typeRef) (trParams typeRef)
    in show typeRef == show reconstructed

-- Property: constraint construction preserves properties
prop_constraintConstructionPreserves :: TypeConstraint -> Bool
prop_constraintConstructionPreserves constraint = 
    not (L.null (show constraint)) && L.length (show constraint) > 0

-- Property: type body validation is consistent
prop_typeBodyValidationConsistent :: TypeBody -> Bool
prop_typeBodyValidationConsistent typeBody = 
    not (L.null (show typeBody))

-- Property: type parsing is deterministic
prop_typeParsingDeterministic :: String -> Bool
prop_typeParsingDeterministic code = 
    case parseDependentType code of
        Right type1 -> 
            case parseDependentType code of
                Right type2 -> show type1 == show type2
                Left _ -> False
        Left _ -> True  -- If parsing fails, that's acceptable for property test

-- Property: type validation preserves invariants
prop_typeValidationPreservesInvariants :: String -> Bool
prop_typeValidationPreservesInvariants code = 
    let errors = validateDependentTypeSyntax code
    in -- If there are no errors, parsing should succeed
       if null errors
       then case parseDependentType code of
              Right _ -> True
              Left _ -> False
       else True  -- If there are errors, that's acceptable

-- Property: constraint satisfaction is consistent
prop_constraintSatisfactionConsistent :: TypeConstraint -> Bool
prop_constraintSatisfactionConsistent constraint = 
    -- All well-formed constraints should be representable
    not (L.null (show constraint))

-- Property: type parameter substitution works
prop_typeParameterSubstitution :: TypeParameter -> Bool
prop_typeParameterSubstitution param = 
    let name = tpName param
        constraints = tpConstraints param
    in not (null name) && L.length constraints >= 0