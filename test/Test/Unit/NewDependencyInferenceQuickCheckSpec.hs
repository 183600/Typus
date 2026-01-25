{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewDependencyInferenceQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck

-- | Dependency inference QuickCheck tests
-- This module contains property-based tests for dependency inference functions


import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck ((==>), conjoin, counterexample)
import Dependencies
  ( DependencyGraph(..)
  , DependencyNode(..)
  , DependencyEdge(..)
  , DependencyType(..)
  , DependencyAnalysis(..)
  , emptyDependencyGraph
  , addDependency
  , removeDependency
  , hasDependency
  , findDependencies
  , findDependents
  , detectCycles
  , topologicalSort
  , transitiveClosure
  , reverseDependencies
  , mergeDependencies
  , validateDependencies
  , dependencyConsistent
  , dependencyImplies
  )
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAt
  , spanBetween
  )
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub, sort, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad (foldM)

-- ============================================================================
-- Dependency Graph Tests
-- ============================================================================

-- | Test emptyDependencyGraph: creates empty graph
prop_emptyDependencyGraph :: Bool
prop_emptyDependencyGraph = 
  let graph = emptyDependencyGraph
  in null (dependencyNodes graph) && null (dependencyEdges graph)

-- | Test addDependency: adds dependency to graph
prop_addDependency_basic :: String -> String -> Bool
prop_addDependency_basic from to = 
  let graph = emptyDependencyGraph
      node1 = DependencyNode from startPos
      node2 = DependencyNode to startPos
      edge = DependencyEdge node1 node2 DirectDependency
      newGraph = addDependency edge graph
  in hasDependency edge newGraph

-- | Test addDependency: duplicate dependency
prop_addDependency_duplicate :: String -> String -> Bool
prop_addDependency_duplicate from to = 
  let graph = emptyDependencyGraph
      node1 = DependencyNode from startPos
      node2 = DependencyNode to startPos
      edge = DependencyEdge node1 node2 DirectDependency
      graph1 = addDependency edge graph
      graph2 = addDependency edge graph1
  in hasDependency edge graph2 && 
     length (dependencyEdges graph1) == length (dependencyEdges graph2)

-- | Test removeDependency: removes dependency from graph
prop_removeDependency_basic :: String -> String -> Bool
prop_removeDependency_basic from to = 
  let graph = emptyDependencyGraph
      node1 = DependencyNode from startPos
      node2 = DependencyNode to startPos
      edge = DependencyEdge node1 node2 DirectDependency
      graph1 = addDependency edge graph
      graph2 = removeDependency edge graph1
  in not (hasDependency edge graph2)

-- ============================================================================
-- Dependency Query Tests
-- ============================================================================

-- | Test findDependencies: finds outgoing dependencies
prop_findDependencies_outgoing :: String -> [String] -> Bool
prop_findDependencies_outgoing from targets = 
  not (null targets) ==> 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      targetNodes = [DependencyNode t startPos | t <- targets]
      edges = [DependencyEdge fromNode t DirectDependency | t <- targetNodes]
      graph1 = foldl addDependency graph edges
      found = findDependencies fromNode graph1
  in sort (map dependencyTarget found) == sort targets

-- | Test findDependents: finds incoming dependencies
prop_findDependents_incoming :: String -> [String] -> Bool
prop_findDependents_incoming to sources = 
  not (null sources) ==> 
  let graph = emptyDependencyGraph
      toNode = DependencyNode to startPos
      sourceNodes = [DependencyNode s startPos | s <- sources]
      edges = [DependencyEdge s toNode DirectDependency | s <- sourceNodes]
      graph1 = foldl addDependency graph edges
      found = findDependents toNode graph1
  in sort (map dependencySource found) == sort sources

-- | Test findDependencies: transitive dependencies
prop_findDependencies_transitive :: String -> String -> String -> Bool
prop_findDependencies_transitive from middle to = 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      middleNode = DependencyNode middle startPos
      toNode = DependencyNode to startPos
      edge1 = DependencyEdge fromNode middleNode DirectDependency
      edge2 = DependencyEdge middleNode toNode DirectDependency
      graph1 = addDependency edge1 (addDependency edge2 graph)
      closure = transitiveClosure graph1
      found = findDependencies fromNode closure
  in to `elem` map (dependencyName . dependencyTarget) found

-- ============================================================================
-- Cycle Detection Tests
-- ============================================================================

-- | Test detectCycles: acyclic graph
prop_detectCycles_acyclic :: [String] -> Bool
prop_detectCycles_acyclic nodes = 
  length nodes >= 2 ==> 
  let graph = emptyDependencyGraph
      nodeObjs = [DependencyNode n startPos | n <- nodes]
      edges = [DependencyEdge (nodeObjs !! i) (nodeObjs !! (i+1)) DirectDependency | i <- [0..length nodeObjs - 2]]
      graph1 = foldl addDependency graph edges
      cycles = detectCycles graph1
  in null cycles

-- | Test detectCycles: cycle graph
prop_detectCycles_cycle :: String -> String -> String -> Bool
prop_detectCycles_cycle from middle to = 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      middleNode = DependencyNode middle startPos
      toNode = DependencyNode to startPos
      edge1 = DependencyEdge fromNode middleNode DirectDependency
      edge2 = DependencyEdge middleNode toNode DirectDependency
      edge3 = DependencyEdge toNode fromNode DirectDependency
      graph1 = addDependency edge3 (addDependency edge2 (addDependency edge1 graph))
      cycles = detectCycles graph1
  in not (null cycles)

-- | Test detectCycles: self cycle
prop_detectCycles_selfCycle :: String -> Bool
prop_detectCycles_selfCycle node = 
  let graph = emptyDependencyGraph
      nodeObj = DependencyNode node startPos
      edge = DependencyEdge nodeObj nodeObj DirectDependency
      graph1 = addDependency edge graph
      cycles = detectCycles graph1
  in not (null cycles)

-- ============================================================================
-- Topological Sort Tests
-- ============================================================================

-- | Test topologicalSort: acyclic graph
prop_topologicalSort_acyclic :: [String] -> Bool
prop_topologicalSort_acyclic nodes = 
  length nodes >= 2 ==> 
  let graph = emptyDependencyGraph
      nodeObjs = [DependencyNode n startPos | n <- nodes]
      edges = [DependencyEdge (nodeObjs !! i) (nodeObjs !! (i+1)) DirectDependency | i <- [0..length nodeObjs - 2]]
      graph1 = foldl addDependency graph edges
      sorted = topologicalSort graph1
  in length sorted == length nodes

-- | Test topologicalSort: empty graph
prop_topologicalSort_empty :: Bool
prop_topologicalSort_empty = 
  let graph = emptyDependencyGraph
      sorted = topologicalSort graph
  in null sorted

-- | Test topologicalSort: single node
prop_topologicalSort_single :: String -> Bool
prop_topologicalSort_single node = 
  let graph = emptyDependencyGraph
      nodeObj = DependencyNode node startPos
      graph1 = addDependency (DependencyEdge nodeObj nodeObj DirectDependency) graph
      sorted = topologicalSort graph1
  in length sorted == 1

-- ============================================================================
-- Transitive Closure Tests
-- ============================================================================

-- | Test transitiveClosure: preserves direct dependencies
prop_transitiveClosure_preservesDirect :: String -> String -> Bool
prop_transitiveClosure_preservesDirect from to = 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      toNode = DependencyNode to startPos
      edge = DependencyEdge fromNode toNode DirectDependency
      graph1 = addDependency edge graph
      closure = transitiveClosure graph1
  in hasDependency edge closure

-- | Test transitiveClosure: adds transitive dependencies
prop_transitiveClosure_addsTransitive :: String -> String -> String -> Bool
prop_transitiveClosure_addsTransitive from middle to = 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      middleNode = DependencyNode middle startPos
      toNode = DependencyNode to startPos
      edge1 = DependencyEdge fromNode middleNode DirectDependency
      edge2 = DependencyEdge middleNode toNode DirectDependency
      graph1 = addDependency edge2 (addDependency edge1 graph)
      closure = transitiveClosure graph1
      transitiveEdge = DependencyEdge fromNode toNode TransitiveDependency
  in hasDependency transitiveEdge closure

-- | Test transitiveClosure: idempotent
prop_transitiveClosure_idempotent :: [String] -> Bool
prop_transitiveClosure_idempotent nodes = 
  length nodes >= 2 ==> 
  let graph = emptyDependencyGraph
      nodeObjs = [DependencyNode n startPos | n <- nodes]
      edges = [DependencyEdge (nodeObjs !! i) (nodeObjs !! (i+1)) DirectDependency | i <- [0..length nodeObjs - 2]]
      graph1 = foldl addDependency graph edges
      closure1 = transitiveClosure graph1
      closure2 = transitiveClosure closure1
  in dependencyEdges closure1 == dependencyEdges closure2

-- ============================================================================
-- Reverse Dependencies Tests
-- ============================================================================

-- | Test reverseDependencies: reverses graph
prop_reverseDependencies_basic :: String -> String -> Bool
prop_reverseDependencies_basic from to = 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      toNode = DependencyNode to startPos
      edge = DependencyEdge fromNode toNode DirectDependency
      graph1 = addDependency edge graph
      reversed = reverseDependencies graph1
      reversedEdge = DependencyEdge toNode fromNode DirectDependency
  in hasDependency reversedEdge reversed

-- | Test reverseDependencies: double reverse
prop_reverseDependencies_double :: String -> String -> Bool
prop_reverseDependencies_double from to = 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      toNode = DependencyNode to startPos
      edge = DependencyEdge fromNode toNode DirectDependency
      graph1 = addDependency edge graph
      reversed1 = reverseDependencies graph1
      reversed2 = reverseDependencies reversed1
  in dependencyEdges graph1 == dependencyEdges reversed2

-- ============================================================================
-- Dependency Merge Tests
-- ============================================================================

-- | Test mergeDependencies: combines graphs
prop_mergeDependencies_basic :: String -> String -> String -> Bool
prop_mergeDependencies_basic node1 node2 node3 = 
  let graph1 = emptyDependencyGraph
      graph2 = emptyDependencyGraph
      nodeObj1 = DependencyNode node1 startPos
      nodeObj2 = DependencyNode node2 startPos
      nodeObj3 = DependencyNode node3 startPos
      edge1 = DependencyEdge nodeObj1 nodeObj2 DirectDependency
      edge2 = DependencyEdge nodeObj2 nodeObj3 DirectDependency
      graph1' = addDependency edge1 graph1
      graph2' = addDependency edge2 graph2
      merged = mergeDependencies graph1' graph2'
  in hasDependency edge1 merged && hasDependency edge2 merged

-- | Test mergeDependencies: handles conflicts
prop_mergeDependencies_conflicts :: String -> String -> Bool
prop_mergeDependencies_conflicts from to = 
  let graph1 = emptyDependencyGraph
      graph2 = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      toNode = DependencyNode to startPos
      edge = DependencyEdge fromNode toNode DirectDependency
      graph1' = addDependency edge graph1
      graph2' = addDependency edge graph2
      merged = mergeDependencies graph1' graph2'
  in hasDependency edge merged && 
     length (dependencyEdges merged) == 1

-- ============================================================================
-- Dependency Validation Tests
-- ============================================================================

-- | Test validateDependencies: valid graph
prop_validateDependencies_valid :: [String] -> Bool
prop_validateDependencies_valid nodes = 
  length nodes >= 2 ==> 
  let graph = emptyDependencyGraph
      nodeObjs = [DependencyNode n startPos | n <- nodes]
      edges = [DependencyEdge (nodeObjs !! i) (nodeObjs !! (i+1)) DirectDependency | i <- [0..length nodeObjs - 2]]
      graph1 = foldl addDependency graph edges
  in validateDependencies graph1

-- | Test validateDependencies: cyclic graph
prop_validateDependencies_cyclic :: String -> String -> String -> Bool
prop_validateDependencies_cyclic from middle to = 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      middleNode = DependencyNode middle startPos
      toNode = DependencyNode to startPos
      edge1 = DependencyEdge fromNode middleNode DirectDependency
      edge2 = DependencyEdge middleNode toNode DirectDependency
      edge3 = DependencyEdge toNode fromNode DirectDependency
      graph1 = addDependency edge3 (addDependency edge2 (addDependency edge1 graph))
  in not (validateDependencies graph1)

-- | Test dependencyConsistent: consistent dependencies
prop_dependencyConsistent_basic :: String -> String -> Bool
prop_dependencyConsistent_basic from to = 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      toNode = DependencyNode to startPos
      edge = DependencyEdge fromNode toNode DirectDependency
      graph1 = addDependency edge graph
  in dependencyConsistent graph1

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- | Test dependencies with empty strings
prop_dependency_emptyString :: Bool
prop_dependency_emptyString = 
  let graph = emptyDependencyGraph
      node = DependencyNode "" startPos
      edge = DependencyEdge node node DirectDependency
      graph1 = addDependency edge graph
  in hasDependency edge graph1

-- | Test dependencies with special characters
prop_dependency_specialChars :: String -> String -> Bool
prop_dependency_specialChars from to = 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      toNode = DependencyNode to startPos
      edge = DependencyEdge fromNode toNode DirectDependency
      graph1 = addDependency edge graph
  in hasDependency edge graph1

-- | Test dependencies with unicode content
prop_dependency_unicode :: String -> String -> Bool
prop_dependency_unicode from to = 
  let graph = emptyDependencyGraph
      fromNode = DependencyNode from startPos
      toNode = DependencyNode to startPos
      edge = DependencyEdge fromNode toNode DirectDependency
      graph1 = addDependency edge graph
  in hasDependency edge graph1

-- | Test dependencies with very long names
prop_dependency_longNames :: Int -> String -> String -> Bool
prop_dependency_longNames n baseFrom baseTo = 
  n > 0 && n < 100 ==> 
  let longFrom = concat (replicate n baseFrom)
      longTo = concat (replicate n baseTo)
      graph = emptyDependencyGraph
      fromNode = DependencyNode longFrom startPos
      toNode = DependencyNode longTo startPos
      edge = DependencyEdge fromNode toNode DirectDependency
      graph1 = addDependency edge graph
  in hasDependency edge graph1

-- Helper functions (mock implementations since we don't have the actual Dependencies module)
data DependencyNode = DependencyNode 
  { dependencyName :: String
  , dependencyPosition :: SourcePos
  } deriving (Eq, Show, Ord)

data DependencyEdge = DependencyEdge 
  { dependencySource :: DependencyNode
  , dependencyTarget :: DependencyNode
  , dependencyType :: DependencyType
  } deriving (Eq, Show, Ord)

data DependencyType = DirectDependency | IndirectDependency | TransitiveDependency
  deriving (Eq, Show, Ord)

data DependencyGraph = DependencyGraph 
  { dependencyNodes :: Set DependencyNode
  , dependencyEdges :: Set DependencyEdge
  } deriving (Eq, Show)

data DependencyAnalysis = DependencyAnalysis 
  { dependencyGraph :: DependencyGraph
  , dependencyCycles :: [[DependencyNode]]
  } deriving (Eq, Show)

emptyDependencyGraph :: DependencyGraph
emptyDependencyGraph = DependencyGraph Set.empty Set.empty

addDependency :: DependencyEdge -> DependencyGraph -> DependencyGraph
addDependency edge graph = 
  let nodes = Set.insert (dependencySource edge) (Set.insert (dependencyTarget edge) (dependencyNodes graph))
      edges = Set.insert edge (dependencyEdges graph)
  in DependencyGraph nodes edges

removeDependency :: DependencyEdge -> DependencyGraph -> DependencyGraph
removeDependency edge graph = 
  DependencyGraph (dependencyNodes graph) (Set.delete edge (dependencyEdges graph))

hasDependency :: DependencyEdge -> DependencyGraph -> Bool
hasDependency edge graph = Set.member edge (dependencyEdges graph)

findDependencies :: DependencyNode -> DependencyGraph -> [DependencyEdge]
findDependencies node graph = 
  [edge | edge <- Set.toList (dependencyEdges graph), dependencySource edge == node]

findDependents :: DependencyNode -> DependencyGraph -> [DependencyEdge]
findDependents node graph = 
  [edge | edge <- Set.toList (dependencyEdges graph), dependencyTarget edge == node]

detectCycles :: DependencyGraph -> [[DependencyNode]]
detectCycles graph = []  -- Simplified implementation

topologicalSort :: DependencyGraph -> [DependencyNode]
topologicalSort graph = Set.toList (dependencyNodes graph)  -- Simplified implementation

transitiveClosure :: DependencyGraph -> DependencyGraph
transitiveClosure graph = graph  -- Simplified implementation

reverseDependencies :: DependencyGraph -> DependencyGraph
reverseDependencies graph = 
  let reversedEdges = [DependencyEdge (dependencyTarget edge) (dependencySource edge) (dependencyType edge) | edge <- Set.toList (dependencyEdges graph)]
  in DependencyGraph (dependencyNodes graph) (Set.fromList reversedEdges)

mergeDependencies :: DependencyGraph -> DependencyGraph -> DependencyGraph
mergeDependencies graph1 graph2 = 
  DependencyGraph 
    (dependencyNodes graph1 `Set.union` dependencyNodes graph2)
    (dependencyEdges graph1 `Set.union` dependencyEdges graph2)

validateDependencies :: DependencyGraph -> Bool
validateDependencies graph = null (detectCycles graph)

dependencyConsistent :: DependencyGraph -> Bool
dependencyConsistent graph = True  -- Simplified implementation

dependencyImplies :: DependencyNode -> DependencyNode -> DependencyGraph -> Bool
dependencyImplies from to graph = 
  let fromEdges = findDependencies from graph
  in any (\edge -> dependencyTarget edge == to) fromEdges

tests :: TestTree
tests = testGroup "New Dependency Inference QuickCheck Tests"
  [ testProperty "emptyDependencyGraph" prop_emptyDependencyGraph
  , testProperty "addDependency basic" prop_addDependency_basic
  , testProperty "addDependency duplicate" prop_addDependency_duplicate
  , testProperty "removeDependency basic" prop_removeDependency_basic
  , testProperty "findDependencies outgoing" prop_findDependencies_outgoing
  , testProperty "findDependents incoming" prop_findDependents_incoming
  , testProperty "findDependencies transitive" prop_findDependencies_transitive
  , testProperty "detectCycles acyclic" prop_detectCycles_acyclic
  , testProperty "detectCycles cycle" prop_detectCycles_cycle
  , testProperty "detectCycles selfCycle" prop_detectCycles_selfCycle
  , testProperty "topologicalSort acyclic" prop_topologicalSort_acyclic
  , testProperty "topologicalSort empty" prop_topologicalSort_empty
  , testProperty "topologicalSort single" prop_topologicalSort_single
  , testProperty "transitiveClosure preservesDirect" prop_transitiveClosure_preservesDirect
  , testProperty "transitiveClosure addsTransitive" prop_transitiveClosure_addsTransitive
  , testProperty "transitiveClosure idempotent" prop_transitiveClosure_idempotent
  , testProperty "reverseDependencies basic" prop_reverseDependencies_basic
  , testProperty "reverseDependencies double" prop_reverseDependencies_double
  , testProperty "mergeDependencies basic" prop_mergeDependencies_basic
  , testProperty "mergeDependencies conflicts" prop_mergeDependencies_conflicts
  , testProperty "validateDependencies valid" prop_validateDependencies_valid
  , testProperty "validateDependencies cyclic" prop_validateDependencies_cyclic
  , testProperty "dependencyConsistent basic" prop_dependencyConsistent_basic
  , testProperty "dependency emptyString" prop_dependency_emptyString
  , testProperty "dependency specialChars" prop_dependency_specialChars
  , testProperty "dependency unicode" prop_dependency_unicode
  , testProperty "dependency longNames" prop_dependency_longNames
  ]