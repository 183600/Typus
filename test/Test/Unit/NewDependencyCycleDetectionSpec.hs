{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewDependencyCycleDetectionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Dependencies
  ( DependentTypeChecker
  , DependentTypeError(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , newDependentTypeChecker
  , analyzeDependentTypes
  , analyzeAST
  , solveConstraints
  , addType
  , addConstraint
  , unify
  , checkType
  , checkTypeInstantiation
  , getDependentTypeErrors
  )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort, nub, find)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State (runState, get, put)

-- | Dependency cycle detection tests
tests :: TestTree
tests =
  testGroup "New Dependency Cycle Detection Tests"
    [ testGroup "Simple type dependency cycles"
        [ testCase "detects direct type cycle" $ do
            let src = unlines
                  [ "type A = B"
                  , "type B = A"
                  ]
                errors = analyzeDependentTypes src
            case find isCycleError errors of
              Nothing -> assertFailure "Expected to detect direct type cycle"
              Just err -> return ()
                
        , testCase "detects three-way type cycle" $ do
            let src = unlines
                  [ "type A = B"
                  , "type B = C" 
                  , "type C = A"
                  ]
                errors = analyzeDependentTypes src
            case find isCycleError errors of
              Nothing -> assertFailure "Expected to detect three-way type cycle"
              Just err -> return ()
        ]
        
    , testGroup "Complex dependency cycles"
        [ testCase "detects cycle with generic parameters" $ do
            let src = unlines
                  [ "type List<T> = Node<T> | Nil"
                  , "type Node<T> = { value: T, next: List<T> }"
                  ]
                errors = analyzeDependentTypes src
            -- This should be allowed (recursive types are OK)
            let cycleErrors = filter isCycleError errors
            L.length cycleErrors @?= 0
                
        , testCase "detects constraint dependency cycles" $ do
            let src = unlines
                  [ "type A = B where Size(A) >= Size(B)"
                  , "type B = A where Size(B) >= Size(A)"
                  ]
                errors = analyzeDependentTypes src
            case find isConstraintCycleError errors of
              Nothing -> assertFailure "Expected to detect constraint cycle"
              Just err -> return ()
        ]
        
    , testGroup "Function dependency cycles"
        [ testCase "detects recursive function type cycles" $ do
            let src = unlines
                  [ "func factorial(n: Int): Int ="
                  , "    if n <= 1 then 1 else n * factorial(n-1)"
                  ]
                errors = analyzeDependentTypes src
            -- Recursive functions should be allowed
            let cycleErrors = filter isCycleError errors
            L.length cycleErrors @?= 0
                
        , testCase "detects mutually recursive function cycles" $ do
            let src = unlines
                  [ "func even(n: Int): Bool ="
                  , "    if n == 0 then true else odd(n-1)"
                  , "func odd(n: Int): Bool ="
                  , "    if n == 0 then false else even(n-1)"
                  ]
                errors = analyzeDependentTypes src
            -- Mutually recursive functions should be allowed
            let cycleErrors = filter isCycleError errors
            L.length cycleErrors @?= 0
        ]
        
    , testGroup "Type constraint cycles"
        [ testCase "detects size constraint cycles" $ do
            let src = unlines
                  [ "type A = B where Size(A) > Size(B)"
                  , "type B = C where Size(B) > Size(C)"
                  , "type C = A where Size(C) > Size(A)"
                  ]
                errors = analyzeDependentTypes src
            case find isConstraintCycleError errors of
              Nothing -> assertFailure "Expected to detect size constraint cycle"
              Just err -> return ()
                
        , testCase "detects predicate constraint cycles" $ do
            let src = unlines
                  [ "type A = B where Valid(A) => Valid(B)"
                  , "type B = A where Valid(B) => Valid(A)"
                  ]
                errors = analyzeDependentTypes src
            case find isConstraintCycleError errors of
              Nothing -> assertFailure "Expected to detect predicate constraint cycle"
              Just err -> return ()
        ]
        
    , testGroup "Cross-module dependency cycles"
        [ testCase "detects indirect cross-module cycles" $ do
            let src = unlines
                  [ "import module1"
                  , "import module2"
                  , "type Local = Module1.Type"
                  , "-- module1 defines Module1.Type = Module2.Type"
                  , "-- module2 defines Module2.Type = Local"
                  ]
                errors = analyzeDependentTypes src
            -- Should detect potential cross-module cycle
            let crossModuleErrors = filter isCrossModuleCycleError errors
            L.length crossModuleErrors @>= 1
        ]
        
    , testGroup "Advanced cycle detection"
        [ testCase "detects cycles in complex type hierarchies" $ do
            let src = unlines
                  [ "type Container<T> = Box<T> | Array<T>"
                  , "type Box<T> = { item: T, container: Container<T> }"
                  , "type Array<T> = { elements: List<T>, container: Container<T> }"
                  , "type List<T> = { L.head: T, L.tail: List<T> } | Nil"
                  ]
                errors = analyzeDependentTypes src
            -- Should handle complex recursive structures
            let cycleErrors = filter isInvalidCycleError errors
            L.length cycleErrors @?= 0
                
        , testCase "detects cycles with dependent types" $ do
            let src = unlines
                  [ "type Vector<n: Nat> = Array<n, Int>"
                  , "type Array<m: Nat, T> = { size: m, data: Vector<m> }"
                  ]
                errors = analyzeDependentTypes src
            case find isDependentTypeCycleError errors of
              Nothing -> assertFailure "Expected to detect dependent type cycle"
              Just err -> return ()
        ]
        
    , testGroup "Cycle resolution strategies"
        [ testCase "resolves valid recursive types" $ do
            let src = unlines
                  [ "type List<T> = { L.head: T, L.tail: List<T> } | Nil"
                  , "type Tree<T> = { value: T, left: Tree<T>, right: Tree<T> } | Empty"
                  ]
                errors = analyzeDependentTypes src
            -- Should resolve valid recursive types without errors
            let invalidErrors = filter isInvalidCycleError errors
            L.length invalidErrors @?= 0
                
        , testCase "rejects paradoxical constraints" $ do
            let src = unlines
                  [ "type Paradox = Int where Size(Paradox) > Size(Paradox)"
                  ]
                errors = analyzeDependentTypes src
            case find isParadoxicalConstraintError errors of
              Nothing -> assertFailure "Expected to detect paradoxical constraint"
              Just err -> return ()
        ]
        
    , testGroup "Performance with large dependency graphs"
        [ testCase "handles large acyclic dependency graphs efficiently" $ do
            let src = unlines $ L.concat
                  [ ["type A" ++ show i ++ " = B" ++ show (i+1) | i <- [1..99]]
                  , ["type A100 = Int"]
                  ]
                errors = analyzeDependentTypes src
            -- Should handle large linear chains without performance issues
            L.length errors @?= 0
                
        , testCase "detects cycles in large dependency graphs" $ do
            let src = unlines $ L.concat
                  [ ["type B" ++ show i ++ " = B" ++ show (i+1) | i <- [1..99]]
                  , ["type B100 = B1"]  -- Creates a cycle
                  ]
                errors = analyzeDependentTypes src
            case find isCycleError errors of
              Nothing -> assertFailure "Expected to detect cycle in large graph"
              Just err -> return ()
        ]
        
    , testGroup "Edge case cycle detection"
        [ testCase "handles self-referencing types" $ do
            let src = "type SelfRef = SelfRef"
                errors = analyzeDependentTypes src
            case find isSelfReferenceError errors of
              Nothing -> assertFailure "Expected to detect self-reference"
              Just err -> return ()
                
        , testCase "detects cycles through type aliases" $ do
            let src = unlines
                  [ "type AliasA = TypeB"
                  , "type TypeB = AliasC"
                  , "type AliasC = AliasA"
                  ]
                errors = analyzeDependentTypes src
            case find isCycleError errors of
              Nothing -> assertFailure "Expected to detect cycle through aliases"
              Just err -> return ()
        ]
    ]

-- Helper functions to identify different types of cycle errors
isCycleError :: DependentTypeError -> Bool
isCycleError (DependentInfiniteType _ _) = True
isCycleError (UnsolvableConstraint _) = True
isCycleError _ = False

isConstraintCycleError :: DependentTypeError -> Bool
isConstraintCycleError (UnsolvableConstraint (Equal _ _)) = True
isConstraintCycleError (UnsolvableConstraint (Subtype _ _)) = True
isConstraintCycleError (ConstraintViolation _ _) = True
isConstraintCycleError _ = False

isCrossModuleCycleError :: DependentTypeError -> Bool
isCrossModuleCycleError (SemanticError msg) = "cross-module" `L.isInfixOf` msg
isCrossModuleCycleError _ = False

isInvalidCycleError :: DependentTypeError -> Bool
isInvalidCycleError (DependentInfiniteType _ _) = True
isInvalidCycleError _ = False

isDependentTypeCycleError :: DependentTypeError -> Bool
isDependentTypeCycleError (UnsolvableConstraint (TypeSizeGE _ _)) = True
isDependentTypeCycleError (UnsolvableConstraint (TypeSizeGT _ _)) = True
isDependentTypeCycleError (UnsolvableConstraint (TypeRange _ _ _)) = True
isDependentTypeCycleError _ = False

isParadoxicalConstraintError :: DependentTypeError -> Bool
isParadoxicalConstraintError (ConstraintViolation _ _) = True
isParadoxicalConstraintError (UnsolvableConstraint _) = True
isParadoxicalConstraintError _ = False

isSelfReferenceError :: DependentTypeError -> Bool
isSelfReferenceError (DependentInfiniteType name _) = name == name
isSelfReferenceError _ = False

isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack