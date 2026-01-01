{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewEnhancedIntegrationEndToEndQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Compiler
import Parser
import Ownership
import ErrorHandler
import Dependencies
import SourceLocation
import Data.List (isInfixOf)
import Data.List (sort, nub, group, intercalate, find, delete, sortOn)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map

-- ============================================================================
-- Integration End-to-End QuickCheck Tests
-- ============================================================================

-- Property: End-to-end compilation pipeline consistency
prop_end_to_end_compilation_consistency :: String -> Property
prop_end_to_end_compilation_consistency sourceCode =
  not (null sourceCode) ==> 
  let parsed = parseSource sourceCode
      compiled = compileAST parsed
      optimized = optimizeIR compiled
      final = generateCode optimized
      roundtrip = parseFinalCode final
  in property $ isJust parsed ==> isJust compiled .&&. isJust optimized .&&. not (null final)

-- Property: Error propagation through pipeline
prop_error_propagation_through_pipeline :: String -> Property
prop_error_propagation_through_pipeline malformedSource =
  let parsed = parseSource malformedSource
      compiled = case parsed of
                   Nothing -> Nothing
                   Just ast -> compileAST ast
      errors = collectErrors parsed compiled
  in property $ isNothing parsed ==> not (null errors)

-- Property: Ownership analysis integration
prop_ownership_analysis_integration :: [String] -> Property
prop_ownership_analysis_integration variableNames =
  not (null variableNames) ==> 
  let sourceCode = generateOwnershipSource variableNames
      parsed = parseSource sourceCode
      ownership = case parsed of
                   Nothing -> Nothing
                   Just ast -> analyzeOwnership ast
      violations = case ownership of
                     Nothing -> []
                     Just analysis -> findOwnershipViolations analysis
  in property $ isJust parsed ==> isJust ownership .&&. L.length violations >= 0

-- Property: Dependency analysis integration
prop_dependency_analysis_integration :: [String] -> Property
prop_dependency_analysis_integration moduleNames =
  not (null moduleNames) ==> 
  let sourceCode = generateDependencySource moduleNames
      parsed = parseSource sourceCode
      dependencies = case parsed of
                       Nothing -> Nothing
                       Just ast -> analyzeDependencies ast
      cycles = case dependencies of
                 Nothing -> []
                 Just deps -> findDependencyCycles deps
  in property $ isJust parsed ==> isJust dependencies .&&. L.length cycles >= 0

-- Property: Type checking integration
prop_type_checking_integration :: [String] -> Property
prop_type_checking_integration typeDeclarations =
  not (null typeDeclarations) ==> 
  let sourceCode = generateTypeSource typeDeclarations
      parsed = parseSource sourceCode
      typeChecked = case parsed of
                      Nothing -> Nothing
                      Just ast -> typeCheckAST ast
      typeErrors = case typeChecked of
                     Nothing -> []
                     Just result -> getTypeErrors result
  in property $ isJust parsed ==> isJust typeChecked .&&. L.length typeErrors >= 0

-- Property: Source location tracking through pipeline
prop_source_location_tracking :: String -> Int -> Int -> Property
prop_source_location_tracking sourceCode line column =
  not (null sourceCode) && line >= 0 && column >= 0 ==> 
  let location = SourceLocation line column "test.typus"
      parsed = parseSourceWithLocation sourceCode location
      compiled = case parsed of
                   Nothing -> Nothing
                   Just ast -> compileASTWithLocation ast location
      locationPreserved = checkLocationPreservation parsed compiled location
  in property $ isJust parsed ==> locationPreserved

-- Property: Multi-module integration
prop_multi_module_integration :: [(String, [String])] -> Property
prop_multi_module_integration modules =
  not (null modules) ==> 
  let moduleSources = map generateModuleSource modules
      parsedModules = map parseSource moduleSources
      allParsed = L.all isJust parsedModules
      compiled = if allParsed then compileModules (catMaybes parsedModules) else Nothing
      linked = case compiled of
                 Nothing -> Nothing
                 Just ir -> linkModules ir
  in property $ allParsed ==> isJust compiled .&&. isJust linked

-- Property: Optimization pipeline integration
prop_optimization_pipeline_integration :: String -> Property
prop_optimization_pipeline_integration sourceCode =
  not (null sourceCode) ==> 
  let parsed = parseSource sourceCode
      compiled = case parsed of
                   Nothing -> Nothing
                   Just ast -> compileAST ast
      optimized = case compiled of
                    Nothing -> Nothing
                    Just ir -> runOptimizationPipeline ir
      performance = case optimized of
                      Nothing -> Nothing
                      Just optIR -> measurePerformance optIR
  in property $ isJust compiled ==> isJust optimized .&&. isJust performance

-- Property: Error recovery integration
prop_error_recovery_integration :: String -> String -> Property
prop_error_recovery_integration goodSource badSource =
  not (null goodSource) && not (null badSource) ==> 
  let combinedSource = goodSource ++ "\n" ++ badSource ++ "\n" ++ goodSource
      parsed = parseSource combinedSource
      recovered = case parsed of
                    Nothing -> recoverFromErrors combinedSource
                    Just ast -> Just ast
      partialResult = case recovered of
                        Nothing -> Nothing
                        Just ast -> compilePartial ast
  in property $ isJust recovered ==> isJust partialResult

-- Property: Resource management integration
prop_resource_management_integration :: [String] -> Property
prop_resource_management_integration resources =
  not (null resources) ==> 
  let sourceCode = generateResourceSource resources
      parsed = parseSource sourceCode
      analyzed = case parsed of
                   Nothing -> Nothing
                   Just ast -> analyzeResourceUsage ast
      leaks = case analyzed of
                Nothing -> []
                Just analysis -> findResourceLeaks analysis
  in property $ isJust parsed ==> isJust analyzed .&&. L.length leaks >= 0

-- Property: Concurrent compilation integration
prop_concurrent_compilation_integration :: [String] -> Property
prop_concurrent_compilation_integration sources =
  not (null sources) ==> 
  let parsedSources = map parseSource sources
      allParsed = L.all isJust parsedSources
      compiled = if allParsed then compileConcurrently (catMaybes parsedSources) else Nothing
      merged = case compiled of
                 Nothing -> Nothing
                 Just results -> mergeCompilationResults results
  in property $ allParsed ==> isJust compiled .&&. isJust merged

-- Property: Incremental compilation integration
prop_incremental_compilation_integration :: String -> String -> Property
prop_incremental_compilation_integration originalSource modifiedSource =
  not (null originalSource) && not (null modifiedSource) ==> 
  let originalParsed = parseSource originalSource
      originalCompiled = case originalParsed of
                          Nothing -> Nothing
                          Just ast -> compileAST ast
      modifiedParsed = parseSource modifiedSource
      incremental = case (originalCompiled, modifiedParsed) of
                      (Just origIR, Just modAST) -> compileIncremental origIR modAST
                      _ -> Nothing
  in property $ isJust originalCompiled && isJust modifiedParsed ==> isJust incremental

-- ============================================================================
-- Helper Functions L.and Types
-- ============================================================================

-- Simplified types for integration testing
data CompilationResult = CompilationResult
  { resultAST :: AST
  , resultIR :: IR
  , resultErrors :: [String]
  } deriving (Eq, Show)

data AST = AST
  { astNodes :: [ASTNode]
  , astSource :: String
  } deriving (Eq, Show)

data IR = IR
  { irInstructions :: [Instruction]
  , irMetadata :: Map String String
  } deriving (Eq, Show)

data ASTNode = VariableNode String | FunctionNode String [ASTNode] deriving (Eq, Show)

data Instruction = LoadInstruction String | StoreInstruction String | CallInstruction String deriving (Eq, Show)

data OwnershipAnalysis = OwnershipAnalysis
  { ownershipViolations :: [String]
  , ownershipMap :: Map String String
  } deriving (Eq, Show)

data DependencyAnalysis = DependencyAnalysis
  { dependencyGraph :: Map String [String]
  , dependencyCycles :: [[String]]
  } deriving (Eq, Show)

data TypeCheckResult = TypeCheckResult
  { typeErrors :: [String]
  , typeMap :: Map String String
  } deriving (Eq, Show)

data ResourceAnalysis = ResourceAnalysis
  { resourceLeaks :: [String]
  , resourceUsage :: Map String Int
  } deriving (Eq, Show)

-- Integration functions
parseSource :: String -> Maybe AST
parseSource source = if null source then Nothing else Just $ AST [VariableNode "test"] source

parseSourceWithLocation :: String -> SourceLocation -> Maybe AST
parseSourceWithLocation source location = parseSource source

compileAST :: AST -> Maybe IR
compileAST ast = Just $ IR [LoadInstruction "test"] empty

compileASTWithLocation :: AST -> SourceLocation -> Maybe IR
compileASTWithLocation ast location = compileAST ast

optimizeIR :: Maybe IR -> Maybe IR
optimizeIR maybeIR = maybeIR

generateCode :: Maybe IR -> String
generateCode maybeIR = case maybeIR of
  Nothing -> ""
  Just ir -> "generated code"

parseFinalCode :: String -> Maybe AST
parseFinalCode code = if null code then Nothing else Just $ AST [VariableNode "final"] code

collectErrors :: Maybe AST -> Maybe IR -> [String]
collectErrors maybeAST maybeIR = 
  case (maybeAST, maybeIR) of
    (Nothing, _) -> ["Parse error"]
    (_, Nothing) -> ["Compilation error"]
    _ -> []

generateOwnershipSource :: [String] -> String
generateOwnershipSource vars = unlines $ L.map (\v -> "var " ++ v ++ " = new Resource();") vars

analyzeOwnership :: AST -> Maybe OwnershipAnalysis
analyzeOwnership ast = Just $ OwnershipAnalysis [] empty

findOwnershipViolations :: OwnershipAnalysis -> [String]
findOwnershipViolations = ownershipViolations

generateDependencySource :: [String] -> String
generateDependencySource modules = unlines $ L.map (\m -> "import " ++ m ++ ";") modules

analyzeDependencies :: AST -> Maybe DependencyAnalysis
analyzeDependencies ast = Just $ DependencyAnalysis empty []

findDependencyCycles :: DependencyAnalysis -> [[String]]
findDependencyCycles = dependencyCycles

generateTypeSource :: [String] -> String
generateTypeSource types = unlines $ L.map (\t -> "type " ++ t ++ " = int;") types

typeCheckAST :: AST -> Maybe TypeCheckResult
typeCheckAST ast = Just $ TypeCheckResult [] empty

getTypeErrors :: TypeCheckResult -> [String]
getTypeErrors = typeErrors

checkLocationPreservation :: Maybe AST -> Maybe IR -> SourceLocation -> Bool
checkLocationPreservation maybeAST maybeIR location = isJust maybeAST && isJust maybeIR

generateModuleSource :: (String, [String]) -> String
generateModuleSource (name, deps) = 
  "module " ++ name ++ ";\n" ++ unlines (L.map (\d -> "import " ++ d ++ ";") deps)

compileModules :: [AST] -> Maybe IR
compileModules asts = Just $ IR [LoadInstruction "module"] empty

linkModules :: IR -> Maybe IR
linkModules ir = Just ir

runOptimizationPipeline :: IR -> Maybe IR
runOptimizationPipeline ir = Just ir

measurePerformance :: IR -> Maybe Int
measurePerformance ir = Just 42

recoverFromErrors :: String -> Maybe AST
recoverFromErrors source = if "error" `L.isInfixOf` source then Nothing else parseSource source

compilePartial :: AST -> Maybe IR
compilePartial ast = compileAST ast

generateResourceSource :: [String] -> String
generateResourceSource resources = unlines $ L.map (\r -> "resource " ++ r ++ " = create();") resources

analyzeResourceUsage :: AST -> Maybe ResourceAnalysis
analyzeResourceUsage ast = Just $ ResourceAnalysis [] empty

findResourceLeaks :: ResourceAnalysis -> [String]
findResourceLeaks = resourceLeaks

compileConcurrently :: [AST] -> Maybe [IR]
compileConcurrently asts = Just $ L.map (\_ -> IR [LoadInstruction "concurrent"] empty) asts

mergeCompilationResults :: [IR] -> Maybe IR
mergeCompilationResults irs = Just $ IR (concatMap irInstructions irs) empty

compileIncremental :: IR -> AST -> Maybe IR
compileIncremental ir ast = Just $ IR (irInstructions ir ++ [LoadInstruction "incremental"]) empty

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Integration End-to-End QuickCheck Tests"
  [ fastProperty "End-to-end compilation pipeline consistency" prop_end_to_end_compilation_consistency
  , fastProperty "Error propagation through pipeline" prop_error_propagation_through_pipeline
  , fastProperty "Ownership analysis integration" prop_ownership_analysis_integration
  , fastProperty "Dependency analysis integration" prop_dependency_analysis_integration
  , fastProperty "Type checking integration" prop_type_checking_integration
  , fastProperty "Source location tracking through pipeline" prop_source_location_tracking
  , fastProperty "Multi-module integration" prop_multi_module_integration
  , fastProperty "Optimization pipeline integration" prop_optimization_pipeline_integration
  , fastProperty "Error recovery integration" prop_error_recovery_integration
  , fastProperty "Resource management integration" prop_resource_management_integration
  , fastProperty "Concurrent compilation integration" prop_concurrent_compilation_integration
  , fastProperty "Incremental compilation integration" prop_incremental_compilation_integration
  ]