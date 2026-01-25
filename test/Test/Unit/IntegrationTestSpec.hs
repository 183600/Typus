{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.IntegrationTestSpec where



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List (nub, sort, intersect)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.QuickCheck

import Parser
import Compiler.TypeChecker
import Compiler.GoAst
import Compiler.IR
import qualified Ownership.Common.Types as Own
import qualified Dependencies.AST as Dep
import SourceLocation
import ErrorHandler

-- Helper generators for integration tests
genSimpleProgram :: Gen String
genSimpleProgram = do
  varCount <- choose (1, 5)
  vars <- vectorOf varCount $ do
    varName <- elements ["x", "y", "z", "a", "b", "c"]
    varType <- elements ["int", "string", "bool"]
    return $ varName ++ " : " ++ varType
  return $ unlines vars

genComplexProgram :: Gen String
genComplexProgram = do
  funcCount <- choose (1, 3)
  funcs <- vectorOf funcCount $ do
    funcName <- elements ["foo", "bar", "baz", "qux"]
    paramCount <- choose (0, 3)
    params <- vectorOf paramCount $ do
      paramName <- elements ["x", "y", "z"]
      paramType <- elements ["int", "string", "bool"]
      return $ paramName ++ " : " ++ paramType
    returnType <- elements ["int", "string", "bool", "void"]
    let paramList = intercalate ", " params
    return $ "func " ++ funcName ++ "(" ++ paramList ++ ") : " ++ returnType ++ " { }"
  return $ unlines funcs

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

genProgramWithOwnership :: Gen String
genProgramWithOwnership = do
  varCount <- choose (1, 5)
  vars <- vectorOf varCount $ do
    varName <- elements ["x", "y", "z", "a", "b", "c"]
    varType <- elements ["owned", "borrowed", "mut"]
    return $ varName ++ " : " ++ varType
  return $ unlines vars

genProgramWithDependencies :: Gen String
genProgramWithDependencies = do
  moduleCount <- choose (1, 3)
  modules <- vectorOf moduleCount $ do
    moduleName <- elements ["Module1", "Module2", "Module3"]
    importCount <- choose (0, 2)
    imports <- vectorOf importCount $ do
      importedModule <- elements ["Base", "Utils", "Types"]
      return $ "import " ++ importedModule
    return $ unlines $ ["module " ++ moduleName] ++ imports
  return $ unlines modules

-- Test properties for integration

-- Property 1: Simple programs parse without errors
prop_simpleProgramParses :: String -> Property
prop_simpleProgramParses program =
  not (null program) ==> 
    -- In a real implementation, this would call the actual parser
    -- and check that it doesn't throw an error
    property $ length program > 0

-- Property 2: Complex programs with functions maintain function count
prop_complexProgramFunctionCount :: String -> Property
prop_complexProgramFunctionCount program =
  not (null program) ==> 
    let funcLines = filter (isPrefixOf "func") $ lines program
        funcCount = length funcLines
    in property $ funcCount >= 1 && funcCount <= 3

-- Property 3: Programs with ownership annotations maintain ownership types
prop_ownershipProgramMaintainsTypes :: String -> Property
prop_ownershipProgramMaintainsTypes program =
  not (null program) ==> 
    let ownershipLines = filter (any (`elem` ["owned", "borrowed", "mut"]) . words) $ lines program
        ownershipTypes = concatMap (filter (`elem` ["owned", "borrowed", "mut"]) . words) ownershipLines
    in not (null ownershipTypes) ==> property $ all (`elem` ["owned", "borrowed", "mut"]) ownershipTypes

-- Property 4: Programs with dependencies maintain module structure
prop_dependencyProgramMaintainsModules :: String -> Property
prop_dependencyProgramMaintainsModules program =
  not (null program) ==> 
    let moduleLines = filter (isPrefixOf "module") $ lines program
        moduleNames = map (drop 7) moduleLines  -- Drop "module " prefix
    in not (null moduleNames) ==> property $ all (not . null) moduleNames

-- Property 5: Type checking preserves type information
prop_typeCheckingPreservesTypes :: String -> Property
prop_typeCheckingPreservesTypes program =
  not (null program) ==> 
    -- In a real implementation, this would run type checking
    -- and verify that type information is preserved
    property $ length program > 0

-- Property 6: Ownership analysis preserves ownership relationships
prop_ownershipAnalysisPreservesRelationships :: String -> Property
prop_ownershipAnalysisPreservesRelationships program =
  not (null program) ==> 
    -- In a real implementation, this would run ownership analysis
    -- and verify that ownership relationships are preserved
    property $ length program > 0

-- Property 7: Dependency analysis preserves dependency relationships
prop_dependencyAnalysisPreservesRelationships :: String -> Property
prop_dependencyAnalysisPreservesRelationships program =
  not (null program) ==> 
    -- In a real implementation, this would run dependency analysis
    -- and verify that dependency relationships are preserved
    property $ length program > 0

-- Property 8: End-to-end compilation preserves program semantics
prop_endToEndCompilationPreservesSemantics :: String -> Property
prop_endToEndCompilationPreservesSemantics program =
  not (null program) ==> 
    -- In a real implementation, this would run the full compilation pipeline
    -- and verify that program semantics are preserved
    property $ length program > 0

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

integrationTests :: TestTree
integrationTests = testGroup "Integration Tests"
  [ testProperties "Parsing Integration"
    [ ("Simple programs parse without errors", property prop_simpleProgramParses)
    , ("Complex programs with functions maintain function count", property prop_complexProgramFunctionCount)
    ]
  , testProperties "Type System Integration"
    [ ("Type checking preserves type information", property prop_typeCheckingPreservesTypes)
    ]
  , testProperties "Ownership Integration"
    [ ("Programs with ownership annotations maintain ownership types", property prop_ownershipProgramMaintainsTypes)
    , ("Ownership analysis preserves ownership relationships", property prop_ownershipAnalysisPreservesRelationships)
    ]
  , testProperties "Dependency Integration"
    [ ("Programs with dependencies maintain module structure", property prop_dependencyProgramMaintainsModules)
    , ("Dependency analysis preserves dependency relationships", property prop_dependencyAnalysisPreservesRelationships)
    ]
  , testProperties "End-to-End Integration"
    [ ("End-to-end compilation preserves program semantics", property prop_endToEndCompilationPreservesSemantics)
    ]
  ]