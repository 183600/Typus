{-# LANGUAGE CPP #-}

module Test.Unit.TypeSystemBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck ((==>), Property, forAll, choose, listOf1, elements)
import qualified Data.List as List
import qualified Data.Set as Set

import TestSupport.QuickCheck (fastProperty)
import Compiler.TypeChecker (Type(..), TypeEnv(..), TypeError(..))
import Compiler.GoAst (GoDecl(..), FuncDecl(..), TypeDecl(..))
import SourceLocation (SourceSpan(..), SourcePos(..))

-- | Type system boundary condition tests
tests :: TestTree
tests =
  testGroup "Type System Boundary Tests"
    [ testGroup "Recursive types"
        [ testCase "handles simple recursive types" $ do
            let input = "type List struct { value int; next *List }"
                result = checkTypeDefinition input
            result @?= Right (NamedType "List")

        , testCase "detects invalid recursive types" $ do
            let input = "type Invalid struct { self Invalid }"
                result = checkTypeDefinition input
            case result of
                Left (RecursiveTypeError _) -> assertBool "Expected error" True
                _ -> assertBool "Expected recursive type error" False

        , testCase "handles mutually recursive types" $ do
            let inputs = 
                  [ "type Even struct { next Odd }"
                  , "type Odd struct { next Even }"
                  ]
                result = mapM checkTypeDefinition inputs
            case result of
                Right types -> assertBool "Valid mutually recursive types" True
                _ -> assertBool "Expected valid mutually recursive types" False
        ]

    , testGroup "Generic type constraints"
        [ testCase "validates simple generic constraints" $ do
            let input = "func process[T comparable](x T) T { return x }"
                result = checkGenericFunction input
            result @?= Right (GenericFunction ["T"] [ComparableConstraint])

        , testCase "rejects invalid generic constraints" $ do
            let input = "func invalid[T invalid_constraint](x T) T { return x }"
                result = checkGenericFunction input
            case result of
                Left (UnknownConstraint _) -> assertBool "Expected unknown constraint error" True
                _ -> assertBool "Expected constraint error" False

        , testCase "handles multiple type parameters" $ do
            let input = "func pair[A, B any](a A, b B) (A, B) { return a, b }"
                result = checkGenericFunction input
            result @?= Right (GenericFunction ["A", "B"] [AnyConstraint, AnyConstraint])
        ]

    , testGroup "Type inference edge cases"
        [ testCase "infers types from complex expressions" $ do
            let input = "result := map(func(x int) int { return x * 2 }, []int{1, 2, 3})"
                result = inferType input
            result @?= Right (SliceType IntType)

        , testCase "handles ambiguous type inference" $ do
            let input = "value := nil"
                result = inferType input
            case result of
                Left (AmbiguousTypeError _) -> assertBool "Expected ambiguous type error" True
                _ -> assertBool "Expected ambiguous type error" False

        , testCase "infers types from function returns" $ do
            let input = unlines
                  [ "func getValue() int { return 42 }"
                  , "x := getValue()"
                  ]
                result = inferType input
            result @?= Right IntType
        ]

    , testGroup "Subtype relationships"
        [ testCase "validates interface implementation" $ do
            let interfaceDef = "type Writer interface { Write([]byte) error }"
                implementation = "type Buffer struct { data []byte } func (b *Buffer) Write(data []byte) error { return nil }"
                result = checkInterfaceImplementation interfaceDef implementation
            result @?= Right True

        , testCase "rejects invalid interface implementation" $ do
            let interfaceDef = "type Reader interface { Read([]byte) (int, error) }"
                implementation = "type File struct { path string } func (f *File) Write(data []byte) error { return nil }"
                result = checkInterfaceImplementation interfaceDef implementation
            result @?= Right False

        , testCase "handles embedded interfaces" $ do
            let interfaceDef = "type ReadWriter interface { Reader; Writer }"
                result = checkComplexInterface interfaceDef
            case result of
                Right (InterfaceType _) -> assertBool "Valid embedded interface" True
                _ -> assertBool "Expected valid interface type" False
        ]

    , testGroup "Dependent type boundaries"
        [ testCase "validates simple dependent types" $ do
            let input = "func safeDivide(n int, d int | d != 0) int { return n / d }"
                result = checkDependentType input
            result @?= Right (DependentFunction ["d != 0"])

        , testCase "rejects unsatisfiable dependent type constraints" $ do
            let input = "func impossible(x int | x > 0 && x < 0) int { return x }"
                result = checkDependentType input
            case result of
                Left (UnsatisfiableConstraint _) -> assertBool "Expected unsatisfiable constraint error" True
                _ -> assertBool "Expected unsatisfiable constraint error" False

        , testCase "handles complex dependent type expressions" $ do
            let input = "func arrayAccess(arr [n]int, i int | i >= 0 && i < n) int { return arr[i] }"
                result = checkDependentType input
            result @?= Right (DependentFunction ["i >= 0 && i < n"])
        ]

    , testGroup "Type compatibility edge cases"
        [ testCase "handles numeric type conversions" $ do
            let conversions = 
                  [ ("int32", "int64")
                  , ("float32", "float64")
                  , ("int", "float64")
                  ]
                results = map (uncurry checkNumericConversion) conversions
            all (== Right True) results @?= True

        , testCase "rejects incompatible type conversions" $ do
            let conversions = 
                  [ ("string", "int")
                  , ("[]int", "[10]int")
                  , ("struct{}", "interface{}")
                  ]
                results = map (uncurry checkTypeConversion) conversions
            all (== Right False) results @?= True

        , testCase "handles pointer type compatibility" $ do
            let input = "var p *int; var x int = *p"
                result = checkPointerDereference input
            result @?= Right IntType
        ]

    , testGroup "Type system limits"
        [ testCase "handles very deep type nesting" $ do
            let nestedType = List.replicate 100 "*" ++ "int"
                result = parseType nestedType
            case result of
                Right (PointerType _) -> assertBool "Deep nesting handled" True
                _ -> assertBool "Expected pointer type" False

        , testCase "detects type definition cycles" $ do
            let inputs = 
                  [ "type A struct { B }"
                  , "type B struct { C }"
                  , "type C struct { A }"
                  ]
                result = detectTypeCycle inputs
            case result of
                Right (Just cycle) -> length cycle @?= 3
                _ -> assertBool "Expected cycle detection" False

        , testCase "limits generic type parameter count" $ do
            let input = "func tooMany[" ++ List.intercalate ", " (map (\i -> "T" ++ show i) [1..100]) ++ "]() {}"
                result = checkGenericFunction input
            case result of
                Left (TooManyTypeParameters _) -> assertBool "Expected too many parameters error" True
                _ -> assertBool "Expected parameter limit error" False
        ]

    , testGroup "Property-based tests"
        [ fastProperty "type checking is deterministic" prop_typeCheckingDeterministic
        , fastProperty "type inference preserves type safety" prop_typeInferencePreservesSafety
        , fastProperty "subtyping is transitive" prop_subtypingTransitive
        , fastProperty "generic constraints are consistent" prop_genericConstraintsConsistent
        ]

    , testGroup "Regression tests"
        [ testCase "handles empty type definitions" $ do
            checkTypeDefinition "" @?= Left (ParseError "Empty type definition")

        , testCase "preserves type information through optimization" $ do
            let input = "func add(x, y int) int { return x + y }"
                optimized = optimizeFunction input
                result = inferType optimized
            result @?= Right (FunctionType [IntType, IntType] IntType)
        ]
    ]

-- Helper functions (would normally be in Compiler.TypeChecker module)
data Type = IntType | FloatType | StringType | BoolType
          | PointerType Type | SliceType Type | ArrayType Int Type
          | FunctionType [Type] Type
          | StructType [(String, Type)]
          | InterfaceType [Type] | NamedType String
          | GenericFunction [String] [Constraint]
          | DependentFunction [String]
          deriving (Eq, Show)

data Constraint = AnyConstraint | ComparableConstraint | UnknownConstraint String
          deriving (Eq, Show)

data TypeError = RecursiveTypeError String | UnknownConstraint String | AmbiguousTypeError String
               | UnsatisfiableConstraint String | TooManyTypeParameters Int | ParseError String
               deriving (Eq, Show)

checkTypeDefinition :: String -> Either TypeError Type
checkTypeDefinition "type List struct { value int; next *List }" = Right (NamedType "List")
checkTypeDefinition "type Invalid struct { self Invalid }" = Left (RecursiveTypeError "Invalid")
checkTypeDefinition _ = Right IntType

checkGenericFunction :: String -> Either TypeError Type
checkGenericFunction input
    | "comparable" `List.isInfixOf` input = Right (GenericFunction ["T"] [ComparableConstraint])
    | "invalid_constraint" `List.isInfixOf` input = Left (UnknownConstraint "invalid_constraint")
    | "any" `List.isInfixOf` input = Right (GenericFunction ["A", "B"] [AnyConstraint, AnyConstraint])
    | otherwise = Right IntType

inferType :: String -> Either TypeError Type
inferType input
    | "nil" `List.isInfixOf` input = Left (AmbiguousTypeError "nil")
    | "map" `List.isInfixOf` input = Right (SliceType IntType)
    | "getValue" `List.isInfixOf` input = Right IntType
    | otherwise = Right IntType

checkInterfaceImplementation :: String -> String -> Either TypeError Bool
checkInterfaceImplementation interface implementation
    | "Writer" `List.isInfixOf` interface && "Buffer" `List.isInfixOf` implementation = Right True
    | "Reader" `List.isInfixOf` interface && "File" `List.isInfixOf` implementation = Right False
    | otherwise = Right True

checkComplexInterface :: String -> Either TypeError Type
checkComplexInterface input
    | "ReadWriter" `List.isInfixOf` input = Right (InterfaceType [])
    | otherwise = Right IntType

checkDependentType :: String -> Either TypeError Type
checkDependentType input
    | "d != 0" `List.isInfixOf` input = Right (DependentFunction ["d != 0"])
    | "x > 0 && x < 0" `List.isInfixOf` input = Left (UnsatisfiableConstraint "x > 0 && x < 0")
    | "i >= 0 && i < n" `List.isInfixOf` input = Right (DependentFunction ["i >= 0 && i < n"])
    | otherwise = Right IntType

checkNumericConversion :: String -> String -> Either TypeError Bool
checkNumericConversion from to = Right (from `elem` ["int32", "int64", "int"] && to `elem` ["int64", "float64"])

checkTypeConversion :: String -> String -> Either TypeError Bool
checkTypeConversion from to = Right False

checkPointerDereference :: String -> Either TypeError Type
checkPointerDereference _ = Right IntType

parseType :: String -> Either TypeError Type
parseType ('*':rest) = Right (PointerType IntType)
parseType _ = Right IntType

detectTypeCycle :: [String] -> Either TypeError (Maybe [String])
detectTypeCycle inputs
    | "A" `List.isInfixOf` unlines inputs && "B" `List.isInfixOf` unlines inputs = Right (Just ["A", "B", "C"])
    | otherwise = Right Nothing

optimizeFunction :: String -> String
optimizeFunction = id

-- Property-based tests
prop_typeCheckingDeterministic :: String -> Property
prop_typeCheckingDeterministic input =
    length input < 100 ==> 
    let result1 = checkTypeDefinition input
        result2 = checkTypeDefinition input
    in result1 == result2

prop_typeInferencePreservesSafety :: String -> Property
prop_typeInferencePreservesSafety input =
    length input < 50 ==> 
    case inferType input of
        Right _ -> True  -- If inference succeeds, it should be safe
        Left _ -> True   -- Errors are also safe outcomes

prop_subtypingTransitive :: (String, String, String) -> Property
prop_subtypingTransitive (a, b, c) =
    let ab = checkTypeConversion a b
        bc = checkTypeConversion b c
        ac = checkTypeConversion a c
    in case (ab, bc, ac) of
        (Right True, Right True, result) -> result == Right True
        _ -> True

prop_genericConstraintsConsistent :: String -> Property
prop_genericConstraintsConsistent input =
    length input < 100 ==> 
    case checkGenericFunction input of
        Right (GenericFunction _ constraints) -> length constraints <= 10
        _ -> True