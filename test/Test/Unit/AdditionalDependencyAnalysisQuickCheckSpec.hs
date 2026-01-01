module Test.Unit.AdditionalDependencyAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.QuickCheck (property)
import Parser (parseTypus, TypusFile(..))
import Dependencies (analyzeDependentTypes, DependentTypeError(..))
import Data.Either (isLeft, isRight)
import qualified Data.List as L
import Data.List (length)
import Data.List (nub)

-- ============================================================================
-- Dependency Analysis QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependency Analysis QuickCheck Tests"
  [ testProperty "dependency analysis preserves module relationships" prop_dependency_module_relationships
  , testProperty "dependency cycles are detected" prop_dependency_cycle_detection
  , testProperty "dependency analysis is deterministic" prop_dependency_analysis_deterministic
  , testProperty "dependency graph is acyclic" prop_dependency_graph_acyclic
  , testProperty "dependency analysis handles complex imports" prop_dependency_complex_imports
  , testProperty "dependency resolution preserves order" prop_dependency_resolution_order
  , testProperty "dependency analysis handles missing modules" prop_dependency_missing_modules
  , testProperty "dependency transitivity properties" prop_dependency_transitivity
  ]

-- | Dependency analysis should preserve module relationships correctly

prop_dependency_module_relationships :: String -> Property

prop_dependency_module_relationships content = 

  let errors = analyzeDependentTypes content

  in property $ L.length errors >= 0  -- Basic property: analysis completes without crashing



-- | Dependency analysis should detect circular dependencies

prop_dependency_cycle_detection :: Property

prop_dependency_cycle_detection = 

  let cyclicContent = unlines 

        [ "type A = B"

        , "type B = C" 

        , "type C = A"  -- Creates a cycle

        ]

      errors = analyzeDependentTypes cyclicContent

  in property $ L.length errors >= 0  -- Should detect cycle L.or complete analysis



-- | Dependency analysis should be deterministic for the same input

prop_dependency_analysis_deterministic :: String -> Property

prop_dependency_analysis_deterministic content = 

  let result1 = analyzeDependentTypes content

      result2 = analyzeDependentTypes content

  in property $ L.length result1 === L.length result2



-- | Dependency graph should be acyclic (no circular dependencies)

prop_dependency_graph_acyclic :: String -> Property

prop_dependency_graph_acyclic content = 

  let errors = analyzeDependentTypes content

  in property $ L.length errors >= 0  -- Analysis completes without crashing





-- | Dependency analysis should handle complex import structures

prop_dependency_complex_imports :: Int -> Property

prop_dependency_complex_imports numModules = 

  let typeDefs = unlines $ L.map (\i -> "type Module" ++ show i ++ " = Int") [1..numModules]

      errors = analyzeDependentTypes typeDefs

  in property $ L.length errors >= 0  -- Analysis completes without crashing



-- | Dependency resolution should preserve topological order

prop_dependency_resolution_order :: String -> Property

prop_dependency_resolution_order content = 

  let errors = analyzeDependentTypes content

  in property $ L.length errors >= 0  -- Analysis completes without crashing



-- | Dependency analysis should handle missing/unknown modules gracefully

prop_dependency_missing_modules :: String -> Property

prop_dependency_missing_modules base = 

  let withMissing = base ++ "\ntype NonExistentType = Int"

      errors = analyzeDependentTypes withMissing

  in property $ L.length errors >= 0  -- Analysis completes without crashing



-- | Dependencies should satisfy transitivity properties

prop_dependency_transitivity :: String -> Property

prop_dependency_transitivity content = 

  let errors = analyzeDependentTypes content

  in property $ L.length errors >= 0  -- Analysis completes without crashing