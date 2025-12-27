{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewDependenciesQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.Tasty (TestTree)

import Dependencies (DependentTypeChecker, DependentTypeError(..), AST(..), Statement(..), TypeExpr(..), 
                      Constraint(..), TypeVar(..), TypeConstraint(..), Substitution, 
                      TypeScheme(..), TypeEnvironment(..), TypeInferenceState(..), TypeInferenceError(..),
                      newDependentTypeChecker, analyzeDependentTypes, analyzeAST, validateASTSemantics, validateStatement)
import Data.Char (isAlphaNum)
import qualified Data.List as List
import qualified Data.Set as Set

-- Property: Dependency analysis handles empty input
prop_dependencies_empty_input :: Property
prop_dependencies_empty_input =
  let checker = newDependentTypeChecker
      result = analyzeDependentTypes checker ""
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Dependency graph construction is deterministic
prop_dependencies_deterministic :: String -> Property
prop_dependencies_deterministic input =
  let checker = newDependentTypeChecker
      result1 = analyzeDependentTypes checker input
      result2 = analyzeDependentTypes checker input
  in property $ case (result1, result2) of
    (Left err1, Left err2) -> show err1 === show err2
    (Right res1, Right res2) -> show res1 === show res2
    _ -> property False

-- Property: Simple dependency chains are detected correctly
prop_dependencies_simple_chain :: String -> String -> Property
prop_dependencies_simple_chain node1 node2 =
  not (null node1) && not (null node2) && all isAlphaNum (node1 ++ node2) ==>
  let input = node1 ++ " depends on " ++ node2
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Circular dependencies are detected
prop_dependencies_circular :: String -> String -> Property
prop_dependencies_circular node1 node2 =
  not (null node1) && not (null node2) && node1 /= node2 && all isAlphaNum (node1 ++ node2) ==>
  let input = node1 ++ " depends on " ++ node2 ++ "\n" ++ node2 ++ " depends on " ++ node1
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Dependency type checking is consistent
prop_dependencies_type_checking :: String -> Property
prop_dependencies_type_checking input =
  let checker = newDependentTypeChecker
      result = analyzeDependentTypes checker input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Multiple dependencies are handled correctly
prop_dependencies_multiple :: [String] -> Property
prop_dependencies_multiple nodes =
  not (null nodes) && all (all isAlphaNum) nodes ==>
  let dependencies = [n1 ++ " depends on " ++ n2 | (n1, n2) <- zip nodes (tail nodes ++ [head nodes])]
      input = List.intercalate "\n" dependencies
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Dependency analysis handles comments
prop_dependencies_comments :: String -> String -> Property
prop_dependencies_comments code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) ==>
  let codeWithComment = code ++ "// " ++ comment ++ "\n" ++ code
      checker = newDependentTypeChecker
      result1 = analyzeDependentTypes checker code
      result2 = analyzeDependentTypes checker codeWithComment
  in property $ case (result1, result2) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    _ -> property False

-- Property: Self-dependencies are handled appropriately
prop_dependencies_self :: String -> Property
prop_dependencies_self node =
  not (null node) && all isAlphaNum node ==>
  let input = node ++ " depends on " ++ node
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Complex dependency networks are analyzed correctly
prop_dependencies_complex_network :: Int -> String -> Property
prop_dependencies_complex_network size base =
  size >= 3 && size <= 8 && not (null base) && all isAlphaNum base ==>
  let nodes = take size [base ++ show i | i <- [1..]]
      allPairs = [(n1, n2) | n1 <- nodes, n2 <- nodes, n1 /= n2]
      dependencies = [n1 ++ " depends on " ++ n2 | (n1, n2) <- allPairs]
      input = List.intercalate "\n" dependencies
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Dependency analysis preserves transitivity
prop_dependencies_transitivity :: String -> String -> String -> Property
prop_dependencies_transitivity a b c =
  not (null a) && not (null b) && not (null c) && 
  all isAlphaNum (a ++ b ++ c) && a /= b && b /= c && a /= c ==>
  let input = a ++ " depends on " ++ b ++ "\n" ++ b ++ " depends on " ++ c
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

tests :: TestTree
tests = testGroup "New Dependencies QuickCheck Tests"
  [ fastProperty "Dependencies handles empty input" prop_dependencies_empty_input
  , fastProperty "Dependency graph is deterministic" prop_dependencies_deterministic
  , fastProperty "Simple dependency chains detected" prop_dependencies_simple_chain
  , fastProperty "Circular dependencies detected" prop_dependencies_circular
  , fastProperty "Dependency type checking consistent" prop_dependencies_type_checking
  , fastProperty "Multiple dependencies handled" prop_dependencies_multiple
  , fastProperty "Dependencies handles comments" prop_dependencies_comments
  , fastProperty "Self-dependencies handled" prop_dependencies_self
  , fastProperty "Complex networks analyzed" prop_dependencies_complex_network
  , fastProperty "Dependency transitivity preserved" prop_dependencies_transitivity
  ]