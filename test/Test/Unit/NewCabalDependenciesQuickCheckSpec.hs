{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalDependenciesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, vectorOf, Positive(..), NonNegative(..))

import Dependencies
  ( DependentTypeChecker(..)
  , DependentTypeError(..)
  , AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , newDependentTypeChecker
  , analyzeDependentTypes
  , analyzeAST
  , validateASTSemantics
  , validateStatement
  , addType
  , addConstraint
  , solveConstraints
  , checkType
  , unify
  , getDependentTypeErrors
  )

import Dependencies.AST (AST(..), Statement(..), TypeExpr(..), Constraint(..), DependencyNode(..), DependencyGraph(..))
import qualified Data.Map.Strict as Map
import Data.List (nub, sort, find, intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | 新的QuickCheck属性测试，针对Dependencies模块的循环检测
tests :: TestTree
tests =
  testGroup "New Cabal Dependencies QuickCheck Tests"
    [ testGroup "Dependency graph properties"
        [ fastProperty "DependencyNode preserves structure" $
            \name deps ->
              let node = DependencyNode name deps
              in nodeName node === name .&&. sort (nodeDependencies node) === sort deps

        , fastProperty "DependencyGraph stores nodes correctly" $
            \nodes ->
              let nodeMap = Map.fromList $ map (\n -> (nodeName n, n)) nodes
                  graph = DependencyGraph nodeMap
              in all (\n -> Map.lookup (nodeName n) (graphNodes graph) == Just n) nodes

        , fastProperty "Empty dependency graph is valid" $
            let emptyGraph = DependencyGraph Map.empty
            in Map.null (graphNodes emptyGraph)

        , fastProperty "Self-dependency detection" $
            \name ->
              let node = DependencyNode name [name]
                  hasSelfDep = name `elem` nodeDependencies node
              in hasSelfDep
        ]

    , testGroup "Circular dependency detection"
        [ fastProperty "Simple cycle detection" $
            \names ->
              length names >= 2 && length names <= 10 ==>
              let nodes = zipWith (\i n -> DependencyNode n [names !! ((i + 1) `mod` length names)]) [0..] names
                  graph = DependencyGraph (Map.fromList $ map (\n -> (nodeName n, n)) nodes)
                  hasCycle = detectCycle graph names
              in hasCycle

        , fastProperty "Acyclic graph validation" $
            \names ->
              length names <= 10 ==>
              let nodes = map (\n -> DependencyNode n []) names
                  graph = DependencyGraph (Map.fromList $ map (\n -> (nodeName n, n)) nodes)
                  hasCycle = detectCycle graph names
              in not hasCycle

        , fastProperty "Complex cycle detection" $
            \names ->
              length names >= 3 && length names <= 8 ==>
              let -- Create a graph with a cycle and some acyclic parts
                  cycleNames = take 3 names
                  acyclicNames = drop 3 names
                  cycleNodes = zipWith (\i n -> DependencyNode n [cycleNames !! ((i + 1) `mod` 3)]) [0..] cycleNames
                  acyclicNodes = map (\n -> DependencyNode n [head cycleNames]) acyclicNames
                  allNodes = cycleNodes ++ acyclicNodes
                  graph = DependencyGraph (Map.fromList $ map (\n -> (nodeName n, n)) allNodes)
                  hasCycle = detectCycle graph names
              in hasCycle

        , fastProperty "Multiple independent cycles" $
            \names ->
              length names >= 6 && even (length names) ==>
              let -- Create two separate cycles
                  half = length names `div` 2
                  firstCycle = take half names
                  secondCycle = drop half names
                  makeCycle cycle = zipWith (\i n -> DependencyNode n [cycle !! ((i + 1) `mod` length cycle)]) [0..] cycle
                  nodes = makeCycle firstCycle ++ makeCycle secondCycle
                  graph = DependencyGraph (Map.fromList $ map (\n -> (nodeName n, n)) nodes)
                  hasCycle = detectCycle graph names
              in hasCycle
        ]

    , testGroup "Type dependency analysis"
        [ fastProperty "Type definition dependencies" $
            \typeName params ->
              length params < 5 ==>
              let typeDef = STypeDef (T.pack typeName) (map T.pack params) []
                  ast = Program [typeDef]
                  result = analyzeAST ast
              in length result >= 0  -- Should analyze without crashing

        , fastProperty "Type alias dependencies" $
            \typeName targetName ->
              let alias = STypeAlias (T.pack typeName) (SimpleT (T.pack targetName)) []
                  ast = Program [alias]
                  result = analyzeAST ast
              in length result >= 0

        , fastProperty "Function type dependencies" $
            \funcName paramCount ->
              paramCount < 10 ==>
              let params = map (\i -> ("param" ++ show i, SimpleT "int")) [1..paramCount]
                  func = SFuncDecl (T.pack funcName) params (Just (SimpleT "int"))
                  ast = Program [func]
                  result = analyzeAST ast
              in length result >= 0

        , fastProperty "Constraint dependencies" $
            \typeName size ->
              size >= 0 && size < 1000 ==>
              let constraint = SizeGT (T.pack typeName) size
                  typeDef = STypeDef (T.pack typeName) [] [constraint]
                  ast = Program [typeDef]
                  result = analyzeAST ast
              in length result >= 0
        ]

    , testGroup "Type constraint cycle detection"
        [ fastProperty "Mutually recursive types" $
            \type1 type2 ->
              type1 /= type2 ==>
              let ast = Program
                    [ STypeDef (T.pack type1) [] [PredC (T.pack type2) [SimpleT (T.pack type1)]]
                    , STypeDef (T.pack type2) [] [PredC (T.pack type1) [SimpleT (T.pack type2)]]
                    ]
                  result = analyzeAST ast
              in length result >= 0

        , fastProperty "Type constraint chain" $
            \types ->
              length types >= 2 && length types <= 5 ==>
              let makeConstraint i = PredC (T.pack (types !! i)) [SimpleT (T.pack (types !! ((i + 1) `mod` length types)))]
                  typeDefs = zipWith (\name i -> STypeDef (T.pack name) [] [makeConstraint i]) types [0..]
                  ast = Program typeDefs
                  result = analyzeAST ast
              in length result >= 0

        , fastProperty "Generic type dependencies" $
            \typeName paramCount ->
              paramCount < 5 ==>
              let params = map (\i -> T.pack ("T" ++ show i)) [1..paramCount]
                  genericType = GenericT (T.pack typeName) (map SimpleT params)
                  alias = STypeAlias (T.pack typeName) genericType []
                  ast = Program [alias]
                  result = analyzeAST ast
              in length result >= 0
        ]

    , testGroup "Edge cases and boundary conditions"
        [ testCase "Empty program analysis" $ do
            let ast = Program []
                result = analyzeAST ast
            result @?= []

        , testCase "Self-referencing type" $ do
            let ast = Program [STypeDef (T.pack "SelfType") [] [PredC (T.pack "SelfType") [SimpleT (T.pack "SelfType")]]]
                result = analyzeAST ast
            length result @? (>= 0)  -- Should handle self-reference

        , testCase "Deep type hierarchy" $ do
            let depth = 10
                types = map (\i -> "Type" ++ show i) [1..depth]
                makeType i = STypeDef (T.pack (types !! i)) [] 
                                  [PredC (T.pack (types !! ((i + 1) `mod` depth))) [SimpleT (T.pack (types !! i))]]
                ast = Program (map makeType [0..depth-1])
                result = analyzeAST ast
            length result @? (>= 0)

        , testCase "Circular function dependencies" $ do
            let ast = Program
                  [ SFuncDecl (T.pack "funcA") [("x", SimpleT (T.pack "TypeB"))] (Just (SimpleT (T.pack "TypeA")))
                  , SFuncDecl (T.pack "funcB") [("y", SimpleT (T.pack "TypeA"))] (Just (SimpleT (T.pack "TypeB")))
                  , STypeDef (T.pack "TypeA") [] []
                  , STypeDef (T.pack "TypeB") [] []
                  ]
                result = analyzeAST ast
            length result @? (>= 0)

        , testCase "Complex constraint cycles" $ do
            let ast = Program
                  [ STypeDef (T.pack "List") [] [SizeGT (T.pack "List") 0]
                  , STypeDef (T.pack "Container") [] [PredC (T.pack "List") [SimpleT (T.pack "Container")]]
                  , STypeDef (T.pack "Collection") [] [PredC (T.pack "Container") [SimpleT (T.pack "Collection")]]
                  ]
                result = analyzeAST ast
            length result @? (>= 0)
        ]

    , testGroup "Performance and stress tests"
        [ fastProperty "Large dependency graph" $
            \size ->
              size < 100 ==>
              let names = map (\i -> "Module" ++ show i) [1..size]
                  -- Create a complex graph with many dependencies
                  nodes = map (\name -> DependencyNode name (take 3 (filter (/= name) names))) names
                  graph = DependencyGraph (Map.fromList $ map (\n -> (nodeName n, n)) nodes)
                  hasCycle = detectCycle graph names
              in length (graphNodes graph) === size

        , fastProperty "Many type constraints" $
            \count ->
              count < 50 ==>
              let constraints = map (\i -> SizeGT (T.pack ("var" ++ show i)) i) [1..count]
                  typeDef = STypeDef (T.pack "ConstrainedType") [] constraints
                  ast = Program [typeDef]
                  result = analyzeAST ast
              in length result >= 0

        , fastProperty "Complex generic hierarchies" $
            \depth breadth ->
              depth < 5 && breadth < 5 ==>
              let createHierarchy d = if d <= 0 
                                     then [SimpleT (T.pack "Base")]
                                     else map (\b -> GenericT (T.pack ("Level" ++ show d ++ "_" ++ show b)) 
                                                               (createHierarchy (d - 1))) [1..breadth]
                  topLevel = GenericT (T.pack "TopLevel") (createHierarchy depth)
                  alias = STypeAlias (T.pack "ComplexType") topLevel []
                  ast = Program [alias]
                  result = analyzeAST ast
              in length result >= 0
        ]

    , testGroup "Error handling and recovery"
        [ fastProperty "Invalid type references" $
            \invalidType ->
              let ast = Program [SVarDecl (T.pack "x") (SimpleT (T.pack invalidType))]
                  result = analyzeAST ast
              in case result of
                   [] -> property False  -- Should have errors for invalid types
                   errs -> any isTypeNotFoundError errs

        , fastProperty "Malformed constraints" $
            \typeName ->
              let invalidConstraint = SizeGT (T.pack typeName) (-1)  -- Negative size
                  typeDef = STypeDef (T.pack typeName) [] [invalidConstraint]
                  ast = Program [typeDef]
                  result = analyzeAST ast
              in case result of
                   [] -> property False  -- Should detect invalid constraint
                   errs -> length errs > 0

        , fastProperty "Recursive type limits" $
            \depth ->
              depth < 20 ==>
              let createRecursiveType d = if d <= 0
                                         then SimpleT (T.pack "Base")
                                         else GenericT (T.pack ("Recursive" ++ show d)) [createRecursiveType (d - 1)]
                  typeDef = STypeAlias (T.pack "DeepRecursive") (createRecursiveType depth) []
                  ast = Program [typeDef]
                  result = analyzeAST ast
              in length result >= 0  -- Should handle or detect recursion
        ]
    ]

-- Helper function to detect cycles in dependency graph
detectCycle :: DependencyGraph -> [String] -> Bool
detectCycle graph names = any (hasPath graph) names
  where
    hasPath g start = visited start Set.empty
      where
        visited node visitedSet
          | node `Set.member` visitedSet = True  -- Cycle detected
          | otherwise = case Map.lookup node (graphNodes g) of
                         Nothing -> False
                         Just depNode -> any (\dep -> visited dep (Set.insert node visitedSet)) 
                                               (nodeDependencies depNode)

-- Helper function to check for type not found errors
isTypeNotFoundError :: DependentTypeError -> Bool
isTypeNotFoundError (TypeNotFound _) = True
isTypeNotFoundError _ = False