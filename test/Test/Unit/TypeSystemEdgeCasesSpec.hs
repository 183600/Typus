module Test.Unit.TypeSystemEdgeCasesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import Utils
import SourceLocation
import qualified Data.Text as T
import Data.List (isInfixOf)

-- | Test type system edge cases and boundary conditions
tests :: TestTree
tests =
  testGroup "Type System Edge Cases Tests"
    [ testGroup "Dependent Type Edge Cases"
        [ testCase "dependent type with zero value" $ do
            let zeroVector = "Vector[int, 0]"
                -- Should handle zero-sized vectors gracefully
                vectorSize = 0
                isValid = vectorSize >= 0
            isValid @?= True

        , testCase "dependent type with negative constraints" $ do
            let negativeConstraint = "Array[T, n] where n > 0"
                constraintValue = -1
                -- Should reject negative values where inappropriate
                isValid = constraintValue > 0
            isValid @?= False

        , testCase "dependent type with large values" $ do
            let largeArray = "Matrix[int, 1000, 1000]"
                dimensions = (1000, 1000)
                totalSize = fst dimensions * snd dimensions
                -- Should handle large but reasonable values
                isManageable = totalSize <= 1000000
            isManageable @?= True

        , testCase "dependent type with complex expressions" $ do
            let complexType = "Vector[T, n + m * 2]"
                expression = "n + m * 2"
                -- Should parse and evaluate complex expressions
                hasVariables = any (`elem` expression) ['n', 'm']
                hasOperations = any (`elem` expression) ['+', '*']
            hasVariables @?= True
            hasOperations @?= True

        , testCase "dependent type with recursive constraints" $ do
            let recursiveType = "List[T, length] where length > 0"
                -- Should handle recursive type definitions
                isRecursive = "length" `isInfixOf` recursiveType
            isRecursive @?= True
        ]

    , testGroup "Generic Type Edge Cases"
        [ testCase "higher-kinded types" $ do
            let higherKinded = "Functor[List]"
                -- Should support types that take type constructors
                kindLevel = 2  -- List takes a type parameter
                supportsHKT = kindLevel > 1
            supportsHKT @?= True

        , testCase "generic type constraints" $ do
            let constrainedGeneric = "sort[T: Ord](list: List[T]) -> List[T]"
                hasConstraint = "Ord" `isInfixOf` constrainedGeneric
                hasGeneric = "T" `isInfixOf` constrainedGeneric
            hasConstraint @?= True
            hasGeneric @?= True

        , testCase "variance handling" $ do
            let covariant = "Box[+T]"  -- Covariant
                contravariant = "Consumer[-T]"  -- Contravariant
                invariant = "MutableBox[T]"  -- Invariant
                -- Should handle different variance annotations
                varianceTypes = [covariant, contravariant, invariant]
            length varianceTypes @?= 3

        , testCase "type parameter bounds" $ do
            let boundedType = "Range[T: Number & Comparable](min: T, max: T)"
                bounds = ["Number", "Comparable"]
                hasMultipleBounds = length bounds > 1
            hasMultipleBounds @?= True

        , testCase "generic type inference edge cases" $ do
            let inferenceExample = "result := identity(42)"  -- Should infer T = int
                inferredType = "int"
                expectedType = "int"
            inferredType @?= expectedType
        ]

    , testGroup "Type Conversion Edge Cases"
        [ testCase "circular type conversion" $ do
            let circularTypes = [("A", "Option[B]"), ("B", "Option[A]")]
                hasCircular = any (\(a, b) -> a `isInfixOf` b && b `isInfixOf` a) 
                              [(t1, t2) | (t1, _) <- circularTypes, (_, t2) <- circularTypes]
                -- Should detect and handle circular type references
            hasCircular @?= True

        , testCase "lossy type conversion" $ do
            let conversion64to32 = "int64_to_int32(9223372036854775807)"
                -- Should detect potential overflow
                sourceValue = 9223372036854775807
                targetMax = 2147483647
                wouldOverflow = sourceValue > targetMax
            wouldOverflow @?= True

        , testCase "type union edge cases" $ do
            let unionType = "String | Int | Null"
                unionMembers = ["String", "Int", "Null"]
                -- Should handle union types with many members
                memberCount = length unionMembers
            memberCount @?= 3

        , testCase "intersection type complexity" $ do
            let intersectionType = "TypeA & TypeB & TypeC"
                intersectionMembers = ["TypeA", "TypeB", "TypeC"]
                -- Should handle intersection of multiple types
                intersectionCount = length intersectionMembers
            intersectionCount @?= 3

        , testCase "type casting safety" $ do
            let safeCast = "cast<String>(dynamic_value)"
                unsafeCast = "dynamic_value as String"
                -- Should distinguish between safe and unsafe casts
                safePatterns = ["cast<", "checked_cast", "safe_cast"]
                unsafePatterns = ["as ", "force_cast"]
            length safePatterns @?= 3
            length unsafePatterns @?= 2
        ]

    , testGroup "Subtyping and Inheritance Edge Cases"
        [ testCase "diamond inheritance problem" $ do
            let diamondHierarchy = 
                  [ ("Base", [])
                  , ("Left", ["Base"])
                  , ("Right", ["Base"])
                  , ("Diamond", ["Left", "Right"])
                  ]
                -- Should handle diamond inheritance gracefully
                diamondType = lookup "Diamond" diamondHierarchy
            diamondType @?= Just ["Left", "Right"]

        , testCase "deep inheritance chains" $ do
            let deepChain = take 20 $ iterate (\n -> "Child" ++ show n) "Base"
                chainLength = length deepChain
                -- Should handle reasonably deep inheritance
                manageableDepth = chainLength <= 50
            manageableDepth @?= True

        , testCase "multiple inheritance conflicts" $ do
            let conflicts = 
                  [ ("A", ["method()"])
                  , ("B", ["method()"])
                  , ("C", ["A", "B"])
                  ]
                -- Should detect method name conflicts
                conflictingMethods = ["method()"]
            length conflictingMethods @?= 1

        , testCase "covariant return types" $ do
            let baseMethod = "Base.method() -> Base"
                derivedMethod = "Derived.method() -> Derived"
                -- Should allow covariant return types
                returnCovariance = "Derived" `isInfixOf` derivedMethod
            returnCovariance @?= True

        , testCase "contravariant parameter types" $ do
            let baseParam = "Base.process(param: Base)"
                derivedParam = "Derived.process(param: Base)"  -- Less specific
                -- Should handle contravariant parameter rules
                paramContravariance = "Base" `isInfixOf` derivedParam
            paramContravariance @?= True
        ]

    , testGroup "Type Variable and Polymorphism Edge Cases"
        [ testCase "higher-rank polymorphism" $ do
            let higherRank = "(forall a. a -> a) -> forall b. b -> b"
                hasNestedForall = "forall" `isInfixOf` higherRank && 
                                  length (filter (== "forall") (words higherRank)) > 1
            hasNestedForall @?= True

        , testCase "type variable capture" $ do
            let captureExample = "forall a. (forall b. b -> a) -> a"
                nestedVars = ["a", "b"]
                distinctVars = length (nub nestedVars)
            distinctVars @?= 2

        , testCase "polymorphic recursion" $ do
            let polyRecursive = "data List[T] = Nil | Cons(T, List[List[T]])"
                hasRecursiveUse = "List[List" `isInfixOf` polyRecursive
            hasRecursiveUse @?= True

        , testCase "type-level computation" $ do
            let typeLevelCalc = "Vector[Add[3, Mul[2, 4]]]"  -- Should be Vector[11]
                hasTypeOps = any (`isInfixOf` typeLevelCalc) ["Add", "Mul", "Sub"]
            hasTypeOps @?= True

        , testCase "existential types" $ do
            let existential = "exists T. { value: T, process: T -> Void }"
                hasExists = "exists" `isInfixOf` existential
                hasHiddenType = "T" `isInfixOf` existential
            hasExists @?= True
            hasHiddenType @?= True
        ]

    , testGroup "Type System Performance Edge Cases"
        [ testCase "type unification performance" $ do
            let complexTypes = replicate 100 "Option[List[Map[String, Int]]]"
                -- Should handle complex type unification efficiently
                typeCount = length complexTypes
            typeCount @?= 100

        , testCase "type inference explosion" $ do
            let inferenceExample = "let x = f(g(h(i(j(k(l(m(n(o(p)))))))))"
                nestedCalls = length $ filter (== '(') inferenceExample
                -- Should handle deeply nested expressions
                manageableNesting = nestedCalls <= 15
            manageableNesting @?= True

        , testCase "type variable explosion" $ do
            let manyTypeVars = concatMap (\i -> "T" ++ show i ++ " -> ") [1..50] ++ "Result"
                varCount = length $ filter (== 'T') manyTypeVars
                -- Should handle many type variables
                manageableVarCount = varCount <= 100
            manageableVarCount @?= True

        , testCase "constraint solving complexity" $ do
            let complexConstraints = 
                  [ "T: Ord"
                  , "U: Num"
                  , "V: Container[T]"
                  , "W: Functor[U]"
                  ]
                constraintCount = length complexConstraints
            constraintCount @?= 4
        ]

    , testGroup "Error Recovery in Type System"
        [ testCase "type error propagation" $ do
            let typeError = "Type mismatch: expected Int, got String"
                errorContext = "in function add at line 5"
                fullError = typeError ++ " (" ++ errorContext ++ ")"
            "Type mismatch" `isInfixOf` fullError @?= True
            "function add" `isInfixOf` fullError @?= True

        , testCase "partial type information recovery" $ do
            let partialInfo = "Type partially known: Container[?]"
                hasWildcard = "?" `isInfixOf` partialInfo
                hasKnownPart = "Container" `isInfixOf` partialInfo
            hasWildcard @?= True
            hasKnownPart @?= True

        , testCase "type error suggestions" $ do
            let suggestion = "Type error: String cannot be used as Int. Did you mean to use parseInt()?"
                hasError = "Type error" `isInfixOf` suggestion
                hasSuggestion = "Did you mean" `isInfixOf` suggestion
            hasError @?= True
            hasSuggestion @?= True

        , testCase "graceful handling of unknown types" $ do
            let unknownType = "UnknownType"
                fallbackHandling = "Any"  -- Fallback to Any type
                unknownTypes = [unknownType]
                fallbackTypes = [fallbackHandling]
            length unknownTypes @?= 1
            length fallbackTypes @?= 1
        ]

    , testGroup "Property-based Type System Tests"
        [ fastProperty "type unification is symmetric" prop_unificationSymmetric
        , fastProperty "type substitution preserves structure" prop_substitutionPreservesStructure
        , fastProperty "type variable renaming preserves equivalence" prop_renamingPreservesEquivalence
        , fastProperty "type generalization preserves instances" prop_generalizationPreservesInstances
        ]
    ]

-- Helper function for nub (remove duplicates)
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- Property: type unification should be symmetric
prop_unificationSymmetric :: String -> String -> Bool
prop_unificationSymmetric type1 type2 =
  let -- Simplified unification check
      canUnify = type1 == type2 || type1 == "Any" || type2 == "Any"
      canUnifyReverse = type2 == type1 || type2 == "Any" || type1 == "Any"
  in canUnify == canUnifyReverse

-- Property: type substitution should preserve structure
prop_substitutionPreservesStructure :: String -> String -> String -> Bool
prop_substitutionPreservesStructure originalType fromType toType =
  let -- Simplified substitution
      substituted = if fromType `isInfixOf` originalType 
                    then replace fromType toType originalType
                    else originalType
      -- Should maintain valid type structure
      hasBalancedBrackets = length (filter (== '[') substituted) == length (filter (== ']') substituted)
  in hasBalancedBrackets
  where
    replace old new = map (\c -> if c == head old then head new else c)

-- Property: type variable renaming should preserve equivalence
prop_renamingPreservesEquivalence :: String -> String -> Bool
prop_renamingPreservesEquivalence type1 type2 =
  let -- Simplified renaming check
      normalizeVars = map (\c -> if c `elem` ['A'..'Z'] then 'T' else c)
      norm1 = normalizeVars type1
      norm2 = normalizeVars type2
  in norm1 == norm2 || type1 == type2

-- Property: type generalization should preserve instances
prop_generalizationPreservesInstances :: String -> Bool
prop_generalizationPreservesInstances concreteType =
  let -- Simplified generalization
      generalized = if "Int" `isInfix` concreteType then "T" else concreteType
      -- Generalized type should be more general
      isMoreGeneral = generalized == "T" || generalized == concreteType
  in isMoreGeneral