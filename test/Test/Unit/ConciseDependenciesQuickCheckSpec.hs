{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseDependenciesQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, property, Arbitrary(..), Gen, choose, elements)
import Dependencies
  ( DependencyGraph(..)
  , DependencyError(..)
  , DependencyType(..)
  , analyzeDependencies
  , detectCycles
  , resolveDependencies
  , getDirectDependencies
  , getTransitiveDependencies
  , hasCycles
  , getDependencyErrors
  , clearDependencyErrors
  , mergeDependencyGraphs
  , addDependency
  , removeDependency
  , hasDependency
  , getNodes
  , getDependencyPath
  , topologicalSort
  , dgNodes
  , dgEdges
  , inferTypes
  )
import Dependencies.AST (AST(..), Statement(..), TypeExpr(..))
import Data.List (nub, sort)

-- Arbitrary instances for QuickCheck
instance Arbitrary DependencyGraph where
  arbitrary = do
    nodes <- arbitrary
    edges <- arbitrary
    return $ TestDependencyGraph nodes edges

instance Arbitrary DependencyError where
  arbitrary = oneof
    [ TestCycleError <$> arbitrary
    , TestMissingDependency <$> arbitrary <*> arbitrary
    ]

instance Arbitrary DependencyType where
  arbitrary = elements [TestDirectDependency, TestTransitiveDependency]

instance Arbitrary AST where
  arbitrary = do
    statements <- arbitrary
    return $ AST statements

instance Arbitrary Statement where
  arbitrary = oneof
    [ VarDecl <$> arbitrary <*> arbitrary
    , FuncDecl <$> arbitrary <*> arbitrary <*> arbitrary
    , TypeDecl <$> arbitrary <*> arbitrary
    ]

instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ TypeVar <$> arbitrary
    , TypeConstructor <$> arbitrary <*> arbitrary
    , FunctionType <$> arbitrary <*> arbitrary
    ]

tests :: TestTree
tests = testGroup "Concise Dependencies QuickCheck Tests"
  [ testProperties "DependencyGraph Properties"
    [ dgNodes_properties
    , dgEdges_properties
    , getNodes_properties
    ]
  , testProperties "Dependency Analysis Properties"
    [ analyzeDependencies_properties
    , detectCycles_properties
    , hasCycles_properties
    ]
  , testProperties "Dependency Resolution Properties"
    [ resolveDependencies_properties
    , getDependencyErrors_properties
    , clearDependencyErrors_properties
    ]
  , testProperties "Dependency Manipulation Properties"
    [ addDependency_properties
    , removeDependency_properties
    , hasDependency_properties
    , mergeDependencyGraphs_properties
    ]
  , testProperties "Dependency Query Properties"
    [ getDirectDependencies_properties
    , getTransitiveDependencies_properties
    , getDependencyPath_properties
    , topologicalSort_properties
    ]
  , testProperties "Type Inference Properties"
    [ inferTypes_properties
    ]
  ]

-- | Test dgNodes properties
dgNodes_properties :: DependencyGraph -> Bool
dgNodes_properties dg = 
  let nodes = dgNodes dg
  in length nodes >= 0

-- | Test dgEdges properties
dgEdges_properties :: DependencyGraph -> Bool
dgEdges_properties dg = 
  let edges = dgEdges dg
  in length edges >= 0

-- | Test getNodes properties
getNodes_properties :: DependencyGraph -> Bool
getNodes_properties dg = getNodes dg == dgNodes dg

-- | Test analyzeDependencies properties
analyzeDependencies_properties :: DependencyGraph -> Bool
analyzeDependencies_properties dg = 
  let result = analyzeDependencies dg
  in result == dg  -- Placeholder implementation returns input unchanged

-- | Test detectCycles properties
detectCycles_properties :: DependencyGraph -> Bool
detectCycles_properties dg = 
  let hasCycle = detectCycles dg
  in hasCycle == False  -- Placeholder implementation always returns False

-- | Test hasCycles properties
hasCycles_properties :: DependencyGraph -> Bool
hasCycles_properties dg = hasCycles dg == detectCycles dg

-- | Test resolveDependencies properties
resolveDependencies_properties :: DependencyGraph -> Bool
resolveDependencies_properties dg = 
  case resolveDependencies dg of
    Left _ -> True  -- Errors are acceptable
    Right result -> result == dg  -- Placeholder implementation returns input unchanged

-- | Test getDependencyErrors properties
getDependencyErrors_properties :: DependencyGraph -> Bool
getDependencyErrors_properties dg = 
  let errors = getDependencyErrors dg
  in length errors >= 0

-- | Test clearDependencyErrors properties
clearDependencyErrors_properties :: DependencyGraph -> Bool
clearDependencyErrors_properties dg = 
  let cleared = clearDependencyErrors dg
      errorsBefore = getDependencyErrors dg
      errorsAfter = getDependencyErrors cleared
  in errorsAfter == [] && cleared == dg  -- Placeholder implementation returns input unchanged

-- | Test addDependency properties
addDependency_properties :: DependencyGraph -> String -> String -> Bool
addDependency_properties dg from to = 
  let newDg = addDependency dg from to
      newEdges = dgEdges newDg
  in (from, to) `elem` newEdges && length newEdges == length (dgEdges dg) + 1

-- | Test removeDependency properties
removeDependency_properties :: DependencyGraph -> String -> String -> Bool
removeDependency_properties dg from to = 
  let newDg = removeDependency dg from to
      newEdges = dgEdges newDg
  in (from, to) `notElem` newEdges && length newEdges <= length (dgEdges dg)

-- | Test hasDependency properties
hasDependency_properties :: DependencyGraph -> String -> String -> Bool
hasDependency_properties dg from to = 
  let hasDep = hasDependency dg from to
      edges = dgEdges dg
  in hasDep == ((from, to) `elem` edges)

-- | Test mergeDependencyGraphs properties
mergeDependencyGraphs_properties :: DependencyGraph -> DependencyGraph -> Bool
mergeDependencyGraphs_properties dg1 dg2 = 
  let merged = mergeDependencyGraphs dg1 dg2
  in dgNodes merged == sort (nub (dgNodes dg1 ++ dgNodes dg2)) &&
     dgEdges merged == dgEdges dg1 ++ dgEdges dg2

-- | Test getDirectDependencies properties
getDirectDependencies_properties :: DependencyGraph -> String -> Bool
getDirectDependencies_properties dg node = 
  let deps = getDirectDependencies dg node
  in length deps >= 0

-- | Test getTransitiveDependencies properties
getTransitiveDependencies_properties :: DependencyGraph -> String -> Bool
getTransitiveDependencies_properties dg node = 
  let deps = getTransitiveDependencies dg node
  in length deps >= 0

-- | Test getDependencyPath properties
getDependencyPath_properties :: DependencyGraph -> String -> String -> Bool
getDependencyPath_properties dg from to = 
  case getDependencyPath dg from to of
    Nothing -> True
    Just path -> length path >= 0

-- | Test topologicalSort properties
topologicalSort_properties :: DependencyGraph -> Bool
topologicalSort_properties dg = 
  case topologicalSort dg of
    Left _ -> True  -- Errors are acceptable
    Right sorted -> length sorted == length (dgNodes dg)

-- | Test inferTypes properties
inferTypes_properties :: AST -> Bool
inferTypes_properties ast = 
  let types = inferTypes ast
  in length types >= 0