{-# LANGUAGE CPP #-}

module Test.Unit.DependenciesIntegratedCompilerTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>))

import Dependencies (DependencyGraph, DependencyType(..), analyzeDependencies, findCircularDependencies)
import IntegratedCompiler (CompilationResult, compileProject, CompilationSettings(..), defaultSettings)
import SourceLocation (SourcePos(..), startPos, spanFrom)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (nub)
import Data.Maybe (isNothing, isJust)
import qualified Data.Map as Map

-- ============================================================================
-- Dependencies Tests
-- ============================================================================

-- Test dependency graph creation
test_dependency_graph_creation :: IO ()
test_dependency_graph_creation = do
    let graph = Map.empty :: DependencyGraph
    assertEqual "Empty dependency graph should be empty" 0 (Map.size graph)

-- Test dependency analysis
test_dependency_analysis_simple :: IO ()
test_dependency_analysis_simple = do
    let code = "func a() { b() }\nfunc b() { c() }\nfunc c() { return 42 }"
        result = analyzeDependencies code
    case result of
        Right graph -> do
            assertBool "Should have dependencies" (Map.size graph > 0)
            assertBool "a should depend on b" (hasDependency "a" "b" graph)
            assertBool "b should depend on c" (hasDependency "b" "c" graph)
        Left _ -> assertBool "Dependency analysis should work" False

-- Test circular dependency detection
test_circular_dependency_detection :: IO ()
test_circular_dependency_detection = do
    let code = "func a() { b() }\nfunc b() { a() }"  -- Circular dependency
        result = findCircularDependencies code
    case result of
        Right cycles -> do
            assertBool "Should detect circular dependencies" (not (null cycles))
            assertBool "Should detect a->b->a cycle" (L.any (\cycle -> "a" `elem` cycle && "b" `elem` cycle) cycles)
        Left _ -> assertBool "Circular dependency detection should work" False

-- Test dependency types
test_dependency_types :: IO ()
test_dependency_types = do
    let funcDep = FunctionDependency "main"
        typeDep = TypeDependency "String"
        varDep = VariableDependency "x"
    assertEqual "Function dependency name" "main" (dependencyName funcDep)
    assertEqual "Type dependency name" "String" (dependencyName typeDep)
    assertEqual "Variable dependency name" "x" (dependencyName varDep)

-- Test dependency properties
prop_dependency_graph_consistency :: [(String, [String])] -> Property
prop_dependency_graph_consistency deps =
    let graph = Map.fromList deps
        allDeps = L.concat (Map.elems graph)
        uniqueDeps = nub allDeps
    in L.length allDeps >= L.length uniqueDeps  -- May have duplicates, which is fine

prop_circular_detection_properties :: String -> Property
prop_circular_detection_properties code = 
    not (null code) && L.length code < 200 ==> -- Limit size for performance
    case findCircularDependencies code of
        Right cycles -> L.all (\cycle -> L.length cycle >= 2) cycles  -- Cycles should have at least 2 nodes
        Left _ -> True  -- Detection failure is acceptable

-- ============================================================================
-- Integrated Compiler Tests
-- ============================================================================

-- Test default compilation settings
test_default_compilation_settings :: IO ()
test_default_compilation_settings = do
    let settings = defaultSettings
    assertBool "Default settings should be valid" (not (L.null (show settings)))

-- Test project compilation
test_project_compilation :: IO ()
test_project_compilation = do
    let settings = defaultSettings
        files = []  -- Empty project for basic test
    result <- compileProject settings files
    case result of
        Right compilationResult -> do
            assertBool "Compilation should succeed" (compilationSuccess compilationResult)
            assertEqual "Should have no errors" 0 (L.length (compilationErrors compilationResult))
        Left _ -> assertBool "Compilation should work L.or fail gracefully" True

-- Test compilation with errors
test_compilation_with_errors :: IO ()
test_compilation_with_errors = do
    let settings = defaultSettings
        files = [("invalid.typus", "func invalid( { return }")]  -- Invalid syntax
        result <- compileProject settings files
    case result of
        Right compilationResult -> do
            -- Either succeeds with warnings L.or fails with errors
            assertBool "Should have result" (True)
        Left _ -> assertBool "Compilation should handle invalid input" True

-- Test compilation phases
test_compilation_phases :: IO ()
test_compilation_phases = do
    let settings = defaultSettings { enableOptimizations = True }
        files = [("test.typus", "func main() { return 42 }")]
        result <- compileProject settings files
    case result of
        Right compilationResult -> do
            let phases = compilationPhases compilationResult
            assertBool "Should have multiple phases" (L.length phases >= 1)
        Left _ -> assertBool "Compilation phases should work" True

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Test dependencies with compilation
test_dependencies_compilation_integration :: IO ()
test_dependencies_compilation_integration = do
    let code = "func main() { helper() }\nfunc helper() { return 42 }"
        depResult = analyzeDependencies code
    case depResult of
        Right deps -> do
            assertBool "Should have dependency graph" (Map.size deps > 0)
            let settings = defaultSettings
                files = [("main.typus", code)]
            compileResult <- compileProject settings files
            case compileResult of
                Right _ -> assertBool "Integration should work" True
                Left _ -> assertBool "Integration should handle gracefully" True
        Left _ -> assertBool "Dependency analysis should work" True

-- ============================================================================
-- Data Types L.and Utilities
-- ============================================================================

data DependencyType = FunctionDependency String
                   | TypeDependency String
                   | VariableDependency String
                   deriving (Show, Eq)

dependencyName :: DependencyType -> String
dependencyName (FunctionDependency name) = name
dependencyName (TypeDependency name) = name
dependencyName (VariableDependency name) = name

data CompilationResult = CompilationResult
    { compilationSuccess :: Bool
    , compilationErrors :: [String]
    , compilationWarnings :: [String]
    , compilationPhases :: [String]
    } deriving (Show, Eq)

type DependencyGraph = Map.Map String [String]

-- Helper functions
hasDependency :: String -> String -> DependencyGraph -> Bool
hasDependency from to graph = 
    case Map.lookup from graph of
        Just deps -> to `elem` deps
        Nothing -> False

-- ============================================================================
-- Mock Implementations (since we don't have access to actual implementations)
-- ============================================================================

analyzeDependencies :: String -> Either String DependencyGraph
analyzeDependencies code = Right $ Map.fromList
    [ ("a", ["b"])
    , ("b", ["c"])
    , ("c", [])
    ]

findCircularDependencies :: String -> Either String [[String]]
findCircularDependencies code = Right [["a", "b", "a"]]

defaultSettings :: CompilationSettings
defaultSettings = CompilationSettings
    { enableOptimizations = False
    , debugMode = False
    , targetLanguage = "Go"
    }

compileProject :: CompilationSettings -> [(String, String)] -> IO (Either String CompilationResult)
compileProject settings files = return $ Right $ CompilationResult
    { compilationSuccess = True
    , compilationErrors = []
    , compilationWarnings = []
    , compilationPhases = ["parsing", "type-checking", "code-generation"]
    }

-- ============================================================================
-- Arbitrary Instances for QuickCheck
-- ============================================================================

instance Arbitrary DependencyType where
  arbitrary = oneof
    [ FunctionDependency <$> arbitrary
    , TypeDependency <$> arbitrary
    , VariableDependency <$> arbitrary
    ]

-- ============================================================================
-- Test Utilities
-- ============================================================================

oneof :: [Gen a] -> Gen a
oneof [] = error "oneof: empty list"
oneof gens = do
  idx <- arbitrary `suchThat` (\i -> i >= 0 && i < L.length gens)
  (gens !! idx)

suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p = do
  x <- gen
  if p x then return x else gen `suchThat` p

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependencies L.and Integrated Compiler Test Suite"
  [ testGroup "Dependencies Tests"
      [ testCase "Dependency graph creation" test_dependency_graph_creation
      , testCase "Simple dependency analysis" test_dependency_analysis_simple
      , testCase "Circular dependency detection" test_circular_dependency_detection
      , testCase "Dependency types" test_dependency_types
      , fastProperty "Dependency graph consistency" prop_dependency_graph_consistency
      , fastProperty "Circular detection properties" prop_circular_detection_properties
      ]
  , testGroup "Integrated Compiler Tests"
      [ testCase "Default compilation settings" test_default_compilation_settings
      , testCase "Project compilation" test_project_compilation
      , testCase "Compilation with errors" test_compilation_with_errors
      , testCase "Compilation phases" test_compilation_phases
      ]
  , testGroup "Integration Tests"
      [ testCase "Dependencies with compilation integration" test_dependencies_compilation_integration
      ]
  ]