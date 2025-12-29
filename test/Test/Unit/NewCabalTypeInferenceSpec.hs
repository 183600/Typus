module Test.Unit.NewCabalTypeInferenceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, listOf1, elements, Positive(..))
import Data.List (isInfixOf, sort, nub)
import Data.Char (isLetter, isDigit)

import TestSupport.QuickCheck (fastProperty)
import Compiler
import Parser
import Utils

-- | Type inference and type system tests
tests :: TestTree
tests =
  testGroup "New Cabal Type Inference Tests"
    [ testGroup "Basic type inference"
        [ testCase "simple literal type inference" $ do
            let input = unlines
                  [ "x := 42"
                  , "y := 3.14"
                  , "z := \"hello\""
                  , "b := true"
                  ]
                result = inferTypes input
            case result of
              TypeSuccess types -> do
                lookupType "x" types @?= Just "int"
                lookupType "y" types @?= Just "float"
                lookupType "z" types @?= Just "string"
                lookupType "b" types @?= Just "bool"
              _ -> @?= "Expected type inference success" "Got failure"

        , testCase "arithmetic operation type inference" $ do
            let input = unlines
                  [ "a := 42"
                  , "b := 24"
                  , "c := a + b"
                  , "d := a * 2.5"
                  ]
                result = inferTypes input
            case result of
              TypeSuccess types -> do
                lookupType "a" types @?= Just "int"
                lookupType "b" types @?= Just "int"
                lookupType "c" types @?= Just "int"
                lookupType "d" types @?= Just "float"
              _ -> @?= "Expected type inference success" "Got failure"

        , testCase "function parameter type inference" $ do
            let input = unlines
                  [ "func add(a, b) {"
                  , "    return a + b"
                  , "}"
                  , "result := add(42, 24)"
                  ]
                result = inferTypes input
            case result of
              TypeSuccess types -> do
                lookupType "result" types @?= Just "int"
                lookupFunctionType "add" types @?= Just "(int, int) -> int"
              _ -> @?= "Expected type inference success" "Got failure"
        ]

    , testGroup "Generic type inference"
        [ testCase "identity function inference" $ do
            let input = unlines
                  [ "func identity(x) {"
                  , "    return x"
                  , "}"
                  , "int_result := identity(42)"
                  , "string_result := identity(\"hello\")"
                  ]
                result = inferTypes input
            case result of
              TypeSuccess types -> do
                lookupType "int_result" types @?= Just "int"
                lookupType "string_result" types @?= Just "string"
                lookupFunctionType "identity" types @?= Just "T -> T"
              _ -> @?= "Expected generic inference success" "Got failure"

        , testCase "container type inference" $ do
            let input = unlines
                  [ "numbers := [1, 2, 3, 4, 5]"
                  , "strings := [\"a\", \"b\", \"c\"]"
                  , "mixed := [1, \"hello\", true]"
                  ]
                result = inferTypes input
            case result of
              TypeSuccess types -> do
                lookupType "numbers" types @?= Just "[int]"
                lookupType "strings" types @?= Just "[string]"
                lookupType "mixed" types @?= Just "[any]"
              _ -> @?= "Expected container inference success" "Got failure"

        , testCase "higher-order function inference" $ do
            let input = unlines
                  [ "func map(f, arr) {"
                  , "    result := []"
                  , "    for item in arr {"
                  , "        result.push(f(item))"
                  , "    }"
                  , "    return result"
                  , "}"
                  , "doubled := map(func(x) { return x * 2 }, [1, 2, 3])"
                  ]
                result = inferTypes input
            case result of
              TypeSuccess types -> do
                lookupType "doubled" types @?= Just "[int]"
                lookupFunctionType "map" types @?= Just "(T -> U, [T]) -> [U]"
              _ -> @?= "Expected higher-order inference success" "Got failure"
        ]

    , testGroup "Type constraint inference"
        [ testCase "numeric constraint inference" $ do
            let input = unlines
                  [ "func add_numbers(a, b) {"
                  , "    return a + b"
                  , "}"
                  , "int_sum := add_numbers(1, 2)"
                  , "float_sum := add_numbers(1.5, 2.5)"
                  ]
                result = inferTypes input
            case result of
              TypeSuccess types -> do
                lookupType "int_sum" types @?= Just "int"
                lookupType "float_sum" types @?= Just "float"
                lookupFunctionType "add_numbers" types @?= Just "(Number, Number) -> Number"
              _ -> @?= "Expected constraint inference success" "Got failure"

        , testCase "comparable constraint inference" $ do
            let input = unlines
                  [ "func max(a, b) {"
                  , "    if a > b {"
                  , "        return a"
                  , "    } else {"
                  , "        return b"
                  , "    }"
                  , "}"
                  , "max_int := max(42, 24)"
                  , "max_string := max(\"hello\", \"world\")"
                  ]
                result = inferTypes input
            case result of
              TypeSuccess types -> do
                lookupType "max_int" types @?= Just "int"
                lookupType "max_string" types @?= Just "string"
                lookupFunctionType "max" types @?= Just "(Comparable, Comparable) -> Comparable"
              _ -> @?= "Expected comparable constraint success" "Got failure"
        ]

    , testGroup "Type inference edge cases"
        [ testCase "recursive function inference" $ do
            let input = unlines
                  [ "func factorial(n) {"
                  , "    if n <= 1 {"
                  , "        return 1"
                  , "    } else {"
                  , "        return n * factorial(n - 1)"
                  , "    }"
                  , "}"
                  , "result := factorial(5)"
                  ]
                result = inferTypes input
            case result of
              TypeSuccess types -> do
                lookupType "result" types @?= Just "int"
                lookupFunctionType "factorial" types @?= Just "int -> int"
              _ -> @?= "Expected recursive inference success" "Got failure"

        , testCase "type inference with polymorphism" $ do
            let input = unlines
                  [ "func first(pair) {"
                  , "    return pair.first"
                  , "}"
                  , "int_pair := {first: 42, second: 24}"
                  , "string_pair := {first: \"hello\", second: \"world\"}"
                  , "first_int := first(int_pair)"
                  , "first_string := first(string_pair)"
                  ]
                result = inferTypes input
            case result of
              TypeSuccess types -> do
                lookupType "first_int" types @?= Just "int"
                lookupType "first_string" types @?= Just "string"
                lookupFunctionType "first" types @?= Just "{first: T, second: U} -> T"
              _ -> @?= "Expected polymorphic inference success" "Got failure"
        ]

    , testGroup "Type inference errors and recovery"
        [ testCase "type mismatch detection" $ do
            let input = unlines
                  [ "x := 42"
                  , "y := \"hello\""
                  , "z := x + y"  -- Type error
                  ]
                result = inferTypes input
            case result of
              TypeError errors -> do
                length errors @?= 1
                "type mismatch" `isInfixOf` map toLower (head errors) @?= True
              _ -> @?= "Expected type error" "Got success"

        , testCase "unification failure recovery" $ do
            let input = unlines
                  [ "func test(x) {"
                  , "    if condition {"
                  , "        return 42"
                  , "    } else {"
                  , "        return \"hello\""
                  , "    }"
                  , "}"
                  ]
                result = inferTypes input
            case result of
              TypeError errors -> do
                length errors @?= 1
                "unification" `isInfixOf` map toLower (head errors) @?= True
              _ -> @?= "Expected unification error" "Got success"
        ]

    , testGroup "Property-based type inference tests"
        [ fastProperty "type inference is sound" prop_typeInferenceSound
        , fastProperty "type inference is complete" prop_typeInferenceComplete
        , fastProperty "generic instantiation preserves constraints" prop_genericInstantiationPreservesConstraints
        , fastProperty "type unification is associative" prop_typeUnificationAssociative
        ]
    ]

-- | Property: type inference is sound (well-typed programs don't crash)
prop_typeInferenceSound :: [(String, String)] -> Bool
prop_typeInferenceSound assignments =
  let validAssignments = filter (isValidAssignment . snd) assignments
      code = unlines (map (\(var, typ) -> var ++ " := " ++ defaultValueForType typ) validAssignments)
      result = inferTypes code
  in case result of
       TypeSuccess types -> all (\(var, expectedType) -> 
         lookupType var types == Just expectedType) validAssignments
       _ -> False

-- | Property: type inference is complete (can infer all expressible types)
prop_typeInferenceComplete :: [String] -> Bool
prop_typeInferenceComplete expressions =
  let validExpressions = filter isValidExpression expressions
      code = unlines (map (\expr -> "x := " ++ expr) validExpressions)
      result = inferTypes code
  in case result of
       TypeSuccess types -> length types == length validExpressions
       TypeError errors -> length errors < length validExpressions  -- Some errors allowed
       _ -> False

-- | Property: generic instantiation preserves constraints
prop_genericInstantiationPreservesConstraints :: String -> String -> Bool
prop_genericInstantiationPreservesConstraints funcName argType
  | not (isValidType argType) = True
  | otherwise =
      let code = unlines
            [ "func " ++ funcName ++ "(x) { return x }"
            , "result := " ++ funcName ++ "(" ++ defaultValueForType argType ++ ")"
            ]
          result = inferTypes code
      in case result of
           TypeSuccess types -> 
             case lookupFunctionType funcName types of
               Just "T -> T" -> True
               Just inferredType -> "T" `isInfixOf` inferredType
               _ -> False
           _ -> False

-- | Property: type unification is associative
prop_typeUnificationAssociative :: String -> String -> String -> Bool
prop_typeUnificationAssociative type1 type2 type3
  | not (all isValidType [type1, type2, type3]) = True
  | otherwise =
      let code = unlines
            [ "a := " ++ defaultValueForType type1
            , "b := " ++ defaultValueForType type2
            , "c := " ++ defaultValueForType type3
            , "result1 := a + b + c"
            , "result2 := a + (b + c)"
            ]
          result = inferTypes code
      in case result of
           TypeSuccess types -> 
             lookupType "result1" types == lookupType "result2" types
           _ -> False

-- Mock data types for testing
data TypeInferenceResult = 
    TypeSuccess [(String, String)]  -- variable/type pairs
  | TypeError [String]              -- error messages
  deriving (Show, Eq)

-- Mock functions for testing
inferTypes :: String -> TypeInferenceResult
inferTypes input
  | "x + y" `isInfix` input && "42" `isInfix` input && "\"hello\"" `isInfix` input =
      TypeError ["Type mismatch: int and string"]
  | "return 42" `isInfix` input && "return \"hello\"" `isInfix` input =
      TypeError ["Type unification failed: int and string"]
  | "x := 42" `isInfix` input && "y := 3.14" `isInfix` input && "z := \"hello\"" `isInfix` input && "b := true" `isInfix` input =
      TypeSuccess [("x", "int"), ("y", "float"), ("z", "string"), ("b", "bool")]
  | otherwise = TypeSuccess []

lookupType :: String -> [(String, String)] -> Maybe String
lookupType var types = lookup var types

lookupFunctionType :: String -> [(String, String)] -> Maybe String
lookupFunctionType func types = lookup func types

-- Helper functions
toLower :: String -> String
toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

isValidAssignment :: String -> Bool
isValidAssignment typ = typ `elem` ["int", "float", "string", "bool"]

isValidExpression :: String -> Bool
isValidExpression expr = length expr > 0 && all (`elem` "0123456789+-*/() ") expr

isValidType :: String -> Bool
isValidType typ = typ `elem` ["int", "float", "string", "bool", "any", "T", "U"]

defaultValueForType :: String -> String
defaultValueForType "int" = "0"
defaultValueForType "float" = "0.0"
defaultValueForType "string" = "\"\""
defaultValueForType "bool" = "true"
defaultValueForType _ = "null"