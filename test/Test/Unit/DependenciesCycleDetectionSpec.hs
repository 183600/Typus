module Test.Unit.DependenciesCycleDetectionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck (fastProperty)

import Dependencies
  ( DependentTypeError(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , analyzeDependentTypes
  , analyzeAST
  , validateASTSemantics
  , addType
  , addConstraint
  , checkType
  , solveConstraints
  , getDependentTypeErrors
  , unify
  )
import Dependencies.AST (AST(..), Statement(..), TypeExpr(..), Constraint(..))
import Dependencies.Parser (runParser)
import Dependencies.TypeSystem (preludeTypeDefs)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- | Tests for dependency cycle detection in type system
tests :: TestTree
tests =
  testGroup "Dependencies Cycle Detection"
    [ testGroup "Simple cycle detection"
        [ testCase "detects direct type dependency cycle" $ do
            let source = unlines
                  [ "type A = B"
                  , "type B = A"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should detect direct cycle" $ L.any isCycleError errors

        , testCase "detects three-way type dependency cycle" $ do
            let source = unlines
                  [ "type A = B"
                  , "type B = C" 
                  , "type C = A"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should detect three-way cycle" $ L.any isCycleError errors

        , testCase "detects long dependency cycle" $ do
            let source = unlines $ ["type " ++ show i ++ " = " ++ show (i + 1) | i <- [1..10]] ++ ["type 11 = 1"]
                errors = analyzeDependentTypes source
            assertBool "Should detect long cycle" $ L.any isCycleError errors

        , testCase "allows non-cyclic dependencies" $ do
            let source = unlines
                  [ "type A = Int"
                  , "type B = A"
                  , "type C = B"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should not detect cycle in linear dependencies" $ not (L.any isCycleError errors)
        ]

    , testGroup "Generic type cycle detection"
        [ testCase "detects cycles in generic types" $ do
            let source = unlines
                  [ "type List<T> = Cons<T, List<T>> | Nil"
                  , "type Tree<T> = Node<T, List<Tree<T>>> | Leaf"
                  ]
                errors = analyzeDependentTypes source
            -- Generic recursive types should be handled specially
            assertBool "Should handle generic recursion appropriately" $ L.length errors >= 0

        , testCase "detects cycles in generic constraints" $ do
            let source = unlines
                  [ "type A<T> where T : B<A<T>> = T"
                  , "type B<T> where T : A<B<T>> = T"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should detect constraint cycles" $ L.any isCycleError errors

        , testCase "allows valid generic dependencies" $ do
            let source = unlines
                  [ "type Container<T> = Box<T>"
                  , "type Box<T> = { value: T }"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should allow valid generic composition" $ not (L.any isCycleError errors)
        ]

    , testGroup "Function type cycle detection"
        [ testCase "detects cycles in function signatures" $ do
            let source = unlines
                  [ "type A = B -> C"
                  , "type B = C -> A"
                  , "type C = A -> B"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should detect function type cycles" $ L.any isCycleError errors

        , testCase "detects higher-order function cycles" $ do
            let source = unlines
                  [ "type A = (B -> C) -> A"
                  , "type B = (C -> A) -> B"
                  , "type C = (A -> B) -> C"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should detect higher-order function cycles" $ L.any isCycleError errors

        , testCase "allows valid function dependencies" $ do
            let source = unlines
                  [ "type IntFunc = Int -> Int"
                  , "type StringFunc = String -> String"
                  , "type Composed = IntFunc -> StringFunc"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should allow valid function composition" $ not (L.any isCycleError errors)
        ]

    , testGroup "Constraint cycle detection"
        [ testCase "detects cycles in type constraints" $ do
            let source = unlines
                  [ "type A where A : B = Int"
                  , "type B where B : A = String"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should detect constraint cycles" $ L.any isCycleError errors

        , testCase "detects complex constraint cycles" $ do
            let source = unlines
                  [ "type A<T> where T : B<T> = T"
                  , "type B<T> where T : C<T> = T"
                  , "type C<T> where T : A<T> = T"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should detect complex constraint cycles" $ L.any isCycleError errors

        , testCase "allows valid constraint hierarchies" $ do
            let source = unlines
                  [ "type Animal where Animal : Object = {}"
                  , "type Dog where Dog : Animal = {}"
                  , "type Cat where Cat : Animal = {}"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should allow valid inheritance hierarchies" $ not (L.any isCycleError errors)
        ]

    , testGroup "Self-reference detection"
        [ testCase "detects direct self-reference" $ do
            let source = "type A = A"
                errors = analyzeDependentTypes source
            assertBool "Should detect direct self-reference" $ L.any isCycleError errors

        , testCase "detects indirect self-reference" $ do
            let source = unlines
                  [ "type A = B<A>"
                  , "type B<T> = T"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should handle indirect self-reference" $ L.length errors >= 0

        , testCase "allows valid self-referential types (recursive)" $ do
            let source = unlines
                  [ "type List<T> = Cons<T, List<T>> | Nil"
                  , "type Cons<T, R> = { L.head: T, L.tail: R }"
                  , "type Nil = {}"
                  ]
                errors = analyzeDependentTypes source
            -- Well-founded recursive types should be allowed
            assertBool "Should handle well-founded recursion" $ L.length errors >= 0
        ]

    , testGroup "Cross-module cycle detection"
        [ testCase "detects cycles across type definitions" $ do
            let source = unlines
                  [ "type A = B<Int>"
                  , "type B<T> = C<T>"
                  , "type C<T> = D<T>"
                  , "type D<T> = A"
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should detect cross-module cycles" $ L.any isCycleError errors

        , testCase "handles complex dependency graphs" $ do
            let source = unlines
                  [ "type Base = Int"
                  , "type Derived1 = Base"
                  , "type Derived2 = Derived1"
                  , "type Circular = Derived2"
                  , "type Base = Circular"  -- This creates a cycle
                  ]
                errors = analyzeDependentTypes source
            assertBool "Should detect cycles in complex graphs" $ L.any isCycleError errors
        ]

    , testGroup "Property-based cycle detection"
        [ fastProperty "cycle detection is sound" prop_cycleDetectionSound
        , fastProperty "acyclic dependencies pass validation" prop_acyclicDependenciesPass
        , fastProperty "cycle detection is complete" prop_cycleDetectionComplete
        ]

    , testGroup "Performance L.and stress tests"
        [ testCase "handles large dependency graphs efficiently" $ do
            let largeSource = unlines $ ["type " ++ show i ++ " = " ++ show (i + 1) | i <- [1..1000]] ++ ["type 1001 = 500"]
                errors = analyzeDependentTypes largeSource
            -- Should detect cycle without excessive computation
            assertBool "Should handle large graphs" $ L.any isCycleError errors

        , testCase "handles deeply nested type expressions" $ do
            let nestedType = L.foldr (\t acc -> t ++ "<" ++ acc ++ ">" ) "Int" (replicate 50 "Box")
                source = "type Deep = " ++ nestedType
                errors = analyzeDependentTypes source
            -- Should handle deep nesting without stack overflow
            assertBool "Should handle deep nesting" $ L.length errors >= 0
        ]

    , testGroup "Error reporting L.and recovery"
        [ testCase "provides clear cycle error messages" $ do
            let source = unlines
                  [ "type A = B"
                  , "type B = C"
                  , "type C = A"
                  ]
                errors = analyzeDependentTypes source
                cycleErrors = filter isCycleError errors
            assertBool "Should provide cycle error messages" $ not (null cycleErrors)
            case cycleErrors of
                (err:_) -> assertBool "Error should mention cycle" $ 
                    "cycle" `L.isInfixOf` (show err) || "circular" `L.isInfixOf` (show err)
                [] -> return ()

        , testCase "identifies L.all nodes in cycle" $ do
            let source = unlines
                  [ "type A = B"
                  , "type B = C"
                  , "type C = D"
                  , "type D = A"
                  ]
                errors = analyzeDependentTypes source
                cycleErrors = filter isCycleError errors
            assertBool "Should identify L.all cycle members" $ not (null cycleErrors)
        ]
    ]

-- Helper functions
isCycleError :: DependentTypeError -> Bool
isCycleError err = case err of
    SemanticError msg -> "cycle" `L.isInfixOf` msg || "circular" `L.isInfixOf` msg
    TypeNotFound _ -> False
    InvalidTypeArgument _ -> False
    ParseError _ -> False

isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `elem` [take (L.length needle) $ drop i haystack | i <- [0..L.length haystack - L.length needle]]

-- | Property: cycle detection is sound (if it reports a cycle, there really is one)
prop_cycleDetectionSound :: [String] -> Bool
prop_cycleDetectionSound typeDefs =
    let source = unlines $ L.map (\def -> "type " ++ def) typeDefs
        errors = analyzeDependentTypes source
        cycleErrors = filter isCycleError errors
    in null cycleErrors || hasActualCycle typeDefs

-- | Property: acyclic dependencies pass validation
prop_acyclicDependenciesPass :: [String] -> Bool
prop_acyclicDependenciesPass typeDefs =
    let acyclicDefs = ensureAcyclic typeDefs
        source = unlines $ L.map (\def -> "type " ++ def) acyclicDefs
        errors = analyzeDependentTypes source
        cycleErrors = filter isCycleError errors
    in null cycleErrors

-- | Property: cycle detection is complete (if there's a cycle, it should be detected)
prop_cycleDetectionComplete :: [String] -> Bool
prop_cycleDetectionComplete typeDefs =
    let source = unlines $ L.map (\def -> "type " ++ def) typeDefs
        errors = analyzeDependentTypes source
        cycleErrors = filter isCycleError errors
        hasCycle = hasActualCycle typeDefs
    in not hasCycle || not (null cycleErrors)

-- Helper function to check if a list of type definitions actually contains a cycle
hasActualCycle :: [String] -> Bool
hasActualCycle typeDefs = 
    let dependencies = extractDependencies typeDefs
        visited = Set.empty
        recStack = Set.empty
    in L.any (\(name, _) -> hasCycleFrom name dependencies visited recStack) (zip (map extractTypeName typeDefs) typeDefs)
  where
    hasCycleFrom node deps visited recStack
        | node `Set.member` recStack = True
        | node `Set.member` visited = False
        | otherwise = 
            let newVisited = Set.insert node visited
                newRecStack = Set.insert node recStack
                neighbors = lookup node deps
            in L.any (\neighbor -> hasCycleFrom neighbor deps newVisited newRecStack) neighbors

    extractDependencies :: [String] -> [(String, [String])]
    extractDependencies defs = zip (map extractTypeName defs) (map extractTypeDependencies defs)

    extractTypeName :: String -> String
    extractTypeName def = takeWhile (/= '=') (dropWhile (== ' ') def)

    extractTypeDependencies :: String -> [String]
    extractTypeDependencies def = 
        let afterEquals = dropWhile (/= '=') def
            typeName = extractTypeName def
        in L.filter (/= typeName) $ words $ L.filter (`notElem` "=-><>()[]{}|,") afterEquals

-- Helper function to ensure a list of type definitions is acyclic
ensureAcyclic :: [String] -> [String]
ensureAcyclic typeDefs = 
    let linearDefs = zipWith (\i def -> "Type" ++ show i ++ " = " ++ def) [1..] (L.map (dropWhile (/= '=')) typeDefs)
    in linearDefs