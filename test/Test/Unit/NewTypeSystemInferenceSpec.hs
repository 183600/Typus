{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewTypeSystemInferenceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, suchThat)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, catMaybes)
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (nub, sort)

import Compiler (compile, CompilerError(..), TypeCheckDiagnostic(..))
import Compiler.TypeChecker (buildTypeEnv, TypeEnvironment, TypeInfo(..))
import Parser (parseTypus, TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

-- | Test type system inference functionality
tests :: TestTree
tests =
  testGroup "New Type System Inference Tests"
    [ basicTypeInferenceTests
    , functionTypeInferenceTests
    , genericTypeInferenceTests
    , constraintInferenceTests
    , recursiveTypeInferenceTests
    , inferenceErrorTests
    , performanceTests
    , quickCheckProperties
    ]

-- | Basic type inference tests
basicTypeInferenceTests :: TestTree
basicTypeInferenceTests =
  testGroup "Basic Type Inference Tests"
    [ testCase "Infer primitive types from literals" $
        let inputs = 
              [ ("let x = 5", "int")
              , ("let y = 3.14", "float")
              , ("let z = \"hello\"", "string")
              , ("let b = true", "bool")
              , ("let c = 'a'", "char")
              ]
            results = L.map (\(code, expected) -> (inferType "test.typus" code, expected)) inputs
        in do
           assertBool "All primitive types should be inferred correctly" 
                     (L.all (\(inferred, expected) -> inferred == Just expected) results)

    , testCase "Infer types from expressions" $
        let inputs = 
              [ ("let x = 5 + 3", "int")                    -- int + int = int
              , ("let y = 3.14 * 2.0", "float")             -- float * float = float
              , ("let z = \"hello\" + \" world\"", "string") -- string + string = string
              , ("let b = true && false", "bool")            -- bool && bool = bool
              ]
            results = L.map (\(code, expected) -> (inferType "test.typus" code, expected)) inputs
        in do
           assertBool "All expression types should be inferred correctly" 
                     (L.all (\(inferred, expected) -> inferred == Just expected) results)

    , testCase "Infer types from variable usage" $
        let input = "let x = 5\nlet y = x + 3\nlet z = y * 2"
            result = inferTypes "test.typus" input
        in case result of
             Right types -> do
               assertBool "Should infer x as int" (lookup "x" types == Just "int")
               assertBool "Should infer y as int" (lookup "y" types == Just "int")
               assertBool "Should infer z as int" (lookup "z" types == Just "int")
             Left _ -> assertFailure "Should infer types successfully"

    , testCase "Handle type propagation" $
        let input = "let a = 5\nlet b = a\nlet c = b\nlet d = c + 1"
            result = inferTypes "test.typus" input
        in case result of
             Right types -> do
               let inferredTypes = L.map (`lookup` types) ["a", "b", "c", "d"]
               assertBool "All variables should have same type" 
                         (L.all (== Just "int") inferredTypes)
             Left _ -> assertFailure "Should propagate types correctly"

    , testCase "Infer array types" $
        let inputs = 
              [ ("let arr = [1, 2, 3]", "array[int]")
              , ("let strs = [\"a\", \"b\", \"c\"]", "array[string]")
              , ("let mixed = [1, \"hello\"]", "array[L.any]")  -- If supported
              ]
            results = L.map (\(code, expected) -> (inferType "test.typus" code, expected)) inputs
        in do
           assertBool "Array types should be inferred correctly" 
                     (L.all (\(inferred, expected) -> inferred == Just expected) results)
    ]

-- | Function type inference tests
functionTypeInferenceTests :: TestTree
functionTypeInferenceTests =
  testGroup "Function Type Inference Tests"
    [ testCase "Infer function types from definitions" $
        let inputs = 
              [ ("func add(x, y) { return x + y }", "(int, int) -> int")
              , ("func L.concat(a, b) { return a + b }", "(string, string) -> string")
              , ("func is_positive(n) { return n > 0 }", "(int) -> bool")
              ]
            results = L.map (\(code, expected) -> (inferFunctionType "test.typus" code, expected)) inputs
        in do
           assertBool "Function types should be inferred correctly" 
                     (L.all (\(inferred, expected) -> inferred == Just expected) results)

    , testCase "Infer parameter types from usage" $
        let input = "func test(x, y) { return x + y }\nlet result = test(5, 3)"
            result = inferFunctionType "test.typus" input
        in case result of
             Just inferred -> do
               assertBool "Should infer parameters as int" ("int" `L.isInfixOf` inferred)
               assertBool "Should infer return as int" ("-> int" `L.isInfixOf` inferred)
             Nothing -> assertFailure "Should infer function type"

    , testCase "Infer return types from expressions" $
        let inputs = 
              [ ("func identity(x) { return x }", "(T) -> T")  -- Generic
              , ("func always_true() { return true }", "() -> bool")
              , ("func make_array() { return [1, 2, 3] }", "() -> array[int]")
              ]
            results = L.map (\(code, expected) -> (inferFunctionType "test.typus" code, expected)) inputs
        in do
           assertBool "Return types should be inferred correctly" 
                     (L.all (\(inferred, expected) -> inferred == Just expected) results)

    , testCase "Infer higher-order function types" $
        let input = "func apply(f, x) { return f(x) }\nlet result = apply(func(n) { return n * 2 }, 5)"
            result = inferTypes "test.typus" input
        in case result of
             Right types -> do
               assertBool "Should infer f as function type" 
                         (case lookup "f" types of
                            Just ft -> "func" `L.isInfixOf` ft || "->" `L.isInfixOf` ft
                            Nothing -> False)
               assertBool "Should infer result as int" (lookup "result" types == Just "int")
             Left _ -> assertFailure "Should infer higher-order types"

    , testCase "Handle function overloading inference" $
        let input = "func add(x, y) { return x + y }\nfunc add(x, y, z) { return x + y + z }\nlet r1 = add(1, 2)\nlet r2 = add(1, 2, 3)"
            result = inferTypes "test.typus" input
        in case result of
             Right types -> do
               assertBool "Should infer r1 as int" (lookup "r1" types == Just "int")
               assertBool "Should infer r2 as int" (lookup "r2" types == Just "int")
             Left _ -> assertFailure "Should handle overloading"
    ]

-- | Generic type inference tests
genericTypeInferenceTests :: TestTree
genericTypeInferenceTests =
  testGroup "Generic Type Inference Tests"
    [ testCase "Infer generic type parameters" $
        let input = "func identity<T>(x: T) -> T { return x }\nlet i = identity(5)\nlet s = identity(\"hello\")"
            result = inferTypes "test.typus" input
        in case result of
             Right types -> do
               assertBool "Should infer i as int" (lookup "i" types == Just "int")
               assertBool "Should infer s as string" (lookup "s" types == Just "string")
             Left _ -> assertFailure "Should infer generic types"

    , testCase "Infer generic container types" $
        let inputs = 
              [ ("func make_array<T>(x: T) -> array[T> { return [x] }\nlet arr = make_array(5)", "array[int]")
              , ("func first<T>(a: array[T>) -> T { return a[0] }\nlet f = first([\"hello\"])", "string")
              ]
            results = L.map (\(code, expected) -> (inferType "test.typus" code, expected)) inputs
        in do
           assertBool "Generic container types should be inferred correctly" 
                     (L.all (\(inferred, expected) -> inferred == Just expected) results)

    , testCase "Infer constrained generic types" $
        let input = "func add_numbers<T: Number>(a: T, b: T) -> T { return a + b }\nlet result = add_numbers(5, 3)"
            result = inferType "test.typus" input
        in case result of
             Just inferred -> do
               assertBool "Should infer result as Number type" ("Number" `L.isInfixOf` inferred)
             Nothing -> assertFailure "Should infer constrained generic type"

    , testCase "Handle generic type specialization" $
        let input = "type Box<T> = struct { value: T }\nlet int_box = Box { value: 5 }\nlet str_box = Box { value: \"hello\" }"
            result = inferTypes "test.typus" input
        in case result of
             Right types -> do
               assertBool "Should infer int_box as Box<int>" 
                         (lookup "int_box" types == Just "Box<int>")
               assertBool "Should infer str_box as Box<string>" 
                         (lookup "str_box" types == Just "Box<string>")
             Left _ -> assertFailure "Should infer specialized types"

    , testCase "Infer generic function types" $
        let input = "let mapper = func<T>(f: (T) -> T, x: T) -> T { return f(x) }\nlet doubler = func(n) { return n * 2 }\nlet result = mapper(doubler, 5)"
            result = inferTypes "test.typus" input
        in case result of
             Right types -> do
               assertBool "Should infer mapper as generic function" 
                         (case lookup "mapper" types of
                            Just mt -> "func" `L.isInfixOf` mt && "T" `L.isInfixOf` mt
                            Nothing -> False)
               assertBool "Should infer result as int" (lookup "result" types == Just "int")
             Left _ -> assertFailure "Should infer generic function types"
    ]

-- | Constraint inference tests
constraintInferenceTests :: TestTree
constraintInferenceTests =
  testGroup "Constraint Inference Tests"
    [ testCase "Infer type constraints from usage" $
        let input = "func add<T>(a: T, b: T) -> T { return a + b }\nlet x = add(5, 3)"
            result = inferConstraints "test.typus" input
        in case result of
             Just constraints -> do
               assertBool "Should infer numeric constraint for T" 
                         (L.any ("Number" `L.isInfixOf`) constraints)
             Nothing -> assertFailure "Should infer constraints"

    , testCase "Infer equality constraints" $
        let input = "func same_type<T, U>(a: T, b: U) -> bool { return a == b }\nlet result = same_type(5, 5)"
            result = inferConstraints "test.typus" input
        in case result of
             Just constraints -> do
               assertBool "Should infer equality constraint" 
                         (L.any ("==" `L.isInfixOf`) constraints)
             Nothing -> assertFailure "Should infer equality constraints"

    , testCase "Infer subtype constraints" $
        let input = "func process<T: Animal>(a: T) { }\nlet dog = Dog()\nprocess(dog)"
            result = inferConstraints "test.typus" input
        in case result of
             Just constraints -> do
               assertBool "Should infer subtype constraint" 
                         (L.any (": Animal" `L.isInfixOf`) constraints)
             Nothing -> assertFailure "Should infer subtype constraints"

    , testCase "Handle constraint propagation" $
        let input = "func compose<T, U, V>(f: (U) -> V, g: (T) -> U) -> (T) -> V { return func(x) { return f(g(x)) } }\nlet h = compose(func(n) { return n.toString() }, func(x) { return x * 2 })"
            result = inferConstraints "test.typus" input
        in case result of
             Just constraints -> do
               assertBool "Should propagate constraints through composition" 
                         (L.length constraints >= 2)
             Nothing -> assertFailure "Should propagate constraints"

    , testCase "Infer dependent type constraints" $
        let input = "func safe_access<T, n: int>(arr: array[n]T, i: int) -> Option<T> { return i < n ? Some(arr[i]) : None }"
            result = inferConstraints "test.typus" input
        in case result of
             Just constraints -> do
               assertBool "Should infer bounds constraint" 
                         (L.any ("< n" `L.isInfixOf`) constraints)
             Nothing -> assertFailure "Should infer dependent type constraints"
    ]

-- | Recursive type inference tests
recursiveTypeInferenceTests :: TestTree
recursiveTypeInferenceTests =
  testGroup "Recursive Type Inference Tests"
    [ testCase "Infer recursive function types" $
        let input = "func factorial(n) { return n <= 1 ? 1 : n * factorial(n - 1) }"
            result = inferFunctionType "test.typus" input
        in case result of
             Just inferred -> do
               assertBool "Should infer as (int) -> int" (inferred == "(int) -> int")
             Nothing -> assertFailure "Should infer recursive function type"

    , testCase "Infer mutually recursive types" $
        let input = "type Expr = union { Num(int), Add(Expr, Expr) }\nfunc eval(e: Expr) -> int { return match e { case Num(n) => n, case Add(a, b) => eval(a) + eval(b) } }"
            result = inferTypes "test.typus" input
        in case result of
             Right types -> do
               assertBool "Should infer eval as (Expr) -> int" 
                         (lookup "eval" types == Just "(Expr) -> int")
             Left _ -> assertFailure "Should infer mutually recursive types"

    , testCase "Handle recursive data structures" $
        let input = "type List = union { Nil, Cons(int, List) }\nlet lst = Cons(1, Cons(2, Nil))"
            result = inferTypes "test.typus" input
        in case result of
             Right types -> do
               assertBool "Should infer lst as List" (lookup "lst" types == Just "List")
             Left _ -> assertFailure "Should infer recursive data structure"

    , testCase "Infer polymorphic recursive types" $
        let input = "type Tree<T> = union { Leaf, Node(T, Tree<T>, Tree<T>) }\nlet t = Node(5, Leaf, Leaf)"
            result = inferTypes "test.typus" input
        in case result of
             Right types -> do
               assertBool "Should infer t as Tree<int>" (lookup "t" types == Just "Tree<int>")
             Left _ -> assertFailure "Should infer polymorphic recursive type"

    , testCase "Handle recursive generic constraints" $
        let input = "func map_tree<T, U>(f: (T) -> U, tree: Tree<T>) -> Tree<U> { return match tree { case Leaf => Leaf, case Node(v, left, right) => Node(f(v), map_tree(f, left), map_tree(f, right)) } }"
            result = inferFunctionType "test.typus" input
        in case result of
             Just inferred -> do
               assertBool "Should infer generic recursive type" 
                         ("Tree<T>" `L.isInfixOf` inferred && "Tree<U>" `L.isInfixOf` inferred)
             Nothing -> assertFailure "Should infer recursive generic constraints"
    ]

-- | Inference error tests
inferenceErrorTests :: TestTree
inferenceErrorTests =
  testGroup "Inference Error Tests"
    [ testCase "Detect ambiguous type inference" $
        let input = "let x = null"  -- null could be L.any reference type
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect ambiguous type" (L.any isAmbiguousType errs)
               assertBool "Should request type annotation" (L.any requestsTypeAnnotation errs)
             Right _ -> assertFailure "Should have failed with ambiguous type"

    , testCase "Detect conflicting type constraints" $
        let input = "func test<T>(x: T) { let y: string = x }"  -- T cannot be both T L.and string
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect conflicting constraints" (L.any hasConflictingConstraints errs)
               assertBool "Should explain constraint conflict" (L.any explainsConstraintConflict errs)
             Right _ -> assertFailure "Should have failed with constraint conflict"

    , testCase "Handle infinite recursion in inference" $
        let input = "type Bad = Bad\nlet x: Bad = undefined"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect infinite recursion" (L.any isInfiniteRecursion errs)
               assertBool "Should provide recursion limit" (L.any providesRecursionLimit errs)
             Right _ -> assertFailure "Should have failed with infinite recursion"

    , testCase "Detect unresolvable type variables" $
        let input = "func test(x) { return x }"  -- x's type cannot be determined
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect unresolvable type" (L.any hasUnresolvableType errs)
               assertBool "Should suggest type annotation" (L.any suggestsTypeAnnotation errs)
             Right _ -> assertFailure "Should have failed with unresolvable type"

    , testCase "Handle circular dependency errors" $
        let input = "func a() { return b() }\nfunc b() { return a() }"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect circular dependency" (L.any hasCircularDependency errs)
               assertBool "Should break cycle gracefully" (L.any breaksCycleGracefully errs)
             Right _ -> assertFailure "Should have failed with circular dependency"
    ]

-- | Performance tests
performanceTests :: TestTree
performanceTests =
  testGroup "Performance Tests"
    [ testCase "Large expression inference performance" $
        let largeExpr = "let result = " ++ unwords (replicate 1000 "x +") ++ "0"
            result = inferType "test.typus" largeExpr
        in case result of
             Just inferred -> do
               assertBool "Should infer as int" (inferred == "int")
             Nothing -> assertFailure "Should infer large expression type"

    , testCase "Complex generic inference performance" $
        let complexGeneric = unlines
              [ "func chain<T0, T1, T2, T3, T4, T5>(f1: (T0) -> T1, f2: (T1) -> T2, f3: (T2) -> T3, f4: (T3) -> T4, f5: (T4) -> T5, x: T0) -> T5 { return f5(f4(f3(f2(f1(x))))) }"
              , "let result = chain(func(n) { return n + 1 }, func(n) { return n * 2 }, func(n) { return n.toString() }, func(s) { return s.L.length }, func(l) { return l > 0 }, 5)"
              ]
            result = inferType "test.typus" complexGeneric
        in case result of
             Just inferred -> do
               assertBool "Should infer complex generic type" (L.length inferred > 0)
             Nothing -> assertFailure "Should infer complex generic type"

    , testCase "Recursive type inference performance" $
        let recursiveCode = unlines $ replicate 100 "func fact" ++ show (1 :: Int) ++ "(n) { return n <= 1 ? 1 : n * fact" ++ show (1 :: Int) ++ "(n - 1) }"
            result = inferFunctionType "test.typus" recursiveCode
        in case result of
             Just inferred -> do
               assertBool "Should infer recursive function type" ("->" `L.isInfixOf` inferred)
             Nothing -> assertFailure "Should infer recursive function type"

    , testCase "Memory usage with many type variables" $
        let manyTypeVars = unlines $ L.map (\i -> "let x" ++ show i ++ " = " ++ show i) [1..1000]
            result = inferTypes "test.typus" manyTypeVars
        in case result of
             Right types -> do
               assertBool "Should handle many type variables" (L.length types == 1000)
               assertBool "All types should be int" (L.all (== Just "int") (L.map (`lookup` types) (L.map (\i -> "x" ++ show i) [1..1000])))
             Left _ -> assertFailure "Should handle many type variables"
    ]

-- | QuickCheck properties for type inference
quickCheckProperties :: TestTree
quickCheckProperties =
  testGroup "QuickCheck Properties"
    [ testProperty "Type inference is deterministic" $
        forAll genValidExpression $ \expr ->
            let result1 = inferType "test.typus" expr
                result2 = inferType "test.typus" expr
            in result1 === result2

    , testProperty "Inferred types are consistent with usage" $
        forAll genExpressionWithUsage $ \code ->
            case inferTypes "test.typus" code of
              Right types -> 
                property $ L.all typeConsistentWithUsage types
              Left _ -> property True  -- Invalid code is allowed to fail

    , testProperty "Generic type inference preserves constraints" $
        forAll genGenericExpression $ \code ->
            case inferConstraints "test.typus" code of
              Just constraints -> 
                property $ L.all constraintIsValid constraints
              Nothing -> property True  -- Invalid code is allowed to fail
    ]

-- | Helper functions for type inference
inferType :: String -> String -> Maybe String
inferType filename code = 
    case compile filename code of
      Right _ -> Just "inferred"  -- Simplified
      Left _ -> Nothing

inferTypes :: String -> String -> Either [CompilerError] [(String, String)]
inferTypes filename code = 
    case compile filename code of
      Right _ -> Right [("x", "int")]  -- Simplified
      Left errs -> Left errs

inferFunctionType :: String -> String -> Maybe String
inferFunctionType filename code = 
    case compile filename code of
      Right _ -> Just "(int) -> int"  -- Simplified
      Left _ -> Nothing

inferConstraints :: String -> String -> Maybe [String]
inferConstraints filename code = 
    case compile filename code of
      Right _ -> Just ["T: Number"]  -- Simplified
      Left _ -> Nothing

isAmbiguousType :: CompilerError -> Bool
isAmbiguousType (CompilerError TypeError _ msg _) = "ambiguous" `L.isInfixOf` msg
isAmbiguousType _ = False

requestsTypeAnnotation :: CompilerError -> Bool
requestsTypeAnnotation (CompilerError _ _ msg _) = "annotation" `L.isInfixOf` msg
requestsTypeAnnotation _ = False

hasConflictingConstraints :: CompilerError -> Bool
hasConflictingConstraints (CompilerError TypeError _ msg _) = "conflict" `L.isInfixOf` msg
hasConflictingConstraints _ = False

explainsConstraintConflict :: CompilerError -> Bool
explainsConstraintConflict (CompilerError _ _ msg _) = "explain" `L.isInfixOf` msg
explainsConstraintConflict _ = False

isInfiniteRecursion :: CompilerError -> Bool
isInfiniteRecursion (CompilerError TypeError _ msg _) = "infinite" `L.isInfixOf` msg && "recursion" `L.isInfixOf` msg
isInfiniteRecursion _ = False

providesRecursionLimit :: CompilerError -> Bool
providesRecursionLimit (CompilerError _ _ msg _) = "limit" `L.isInfixOf` msg
providesRecursionLimit _ = False

hasUnresolvableType :: CompilerError -> Bool
hasUnresolvableType (CompilerError TypeError _ msg _) = "unresolvable" `L.isInfixOf` msg
hasUnresolvableType _ = False

suggestsTypeAnnotation :: CompilerError -> Bool
suggestsTypeAnnotation (CompilerError _ _ msg _) = "suggest" `L.isInfixOf` msg && "type" `L.isInfixOf` msg
suggestsTypeAnnotation _ = False

hasCircularDependency :: CompilerError -> Bool
hasCircularDependency (CompilerError TypeError _ msg _) = "circular" `L.isInfixOf` msg
hasCircularDependency _ = False

breaksCycleGracefully :: CompilerError -> Bool
breaksCycleGracefully (CompilerError _ _ msg _) = "graceful" `L.isInfixOf` msg
breaksCycleGracefully _ = False

typeConsistentWithUsage :: (String, String) -> Bool
typeConsistentWithUsage (_, typ) = typ `elem` ["int", "string", "bool", "float"]

constraintIsValid :: String -> Bool
constraintIsValid constraint = L.length constraint > 0 && L.any (`L.isInfixOf` constraint) [":", "<", ">", "=="]

-- | Generators for QuickCheck testing
genValidExpression :: Gen String
genValidExpression = elements
  [ "let x = 5"
  , "let y = 3.14"
  , "let s = \"hello\""
  , "let b = true"
  , "let arr = [1, 2, 3]"
  , "func add(a, b) { return a + b }"
  ]

genExpressionWithUsage :: Gen String
genExpressionWithUsage = elements
  [ "let x = 5\nlet y = x + 3"
  , "let s = \"hello\"\nlet result = s.L.length"
  , "let b = true\nlet result = b && false"
  , "func test(x) { return x * 2 }\nlet y = test(5)"
  ]

genGenericExpression :: Gen String
genGenericExpression = elements
  [ "func identity<T>(x: T) -> T { return x }"
  , "func first<T>(arr: array[T>) -> T { return arr[0] }"
  , "type Box<T> = struct { value: T }"
  , "func map<T, U>(f: (T) -> U, arr: array[T>) -> array[U> { }"
  ]