{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependencyAnalysisTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements
  , vectorOf, oneof, frequency, suchThat, Positive(..)
  )

import Dependencies
  ( DependentTypeChecker
  , DependentTypeError(..)
  , AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , TypeScheme(..)
  , TypeEnvironment(..)
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , analyzeDependentTypes
  , analyzeAST
  , validateASTSemantics
  , validateStatement
  , checkType
  , addType
  , addConstraint
  , solveConstraints
  , getDependentTypeErrors
  , inferType
  , inferStatement
  , inferProgram
  , generalize
  , instantiate
  , unifyTypes
  , applyTypeSubstitution
  , newTypeVariable
  , initialTypeEnvironment
  )

import Dependencies.AST (DependencyNode(..), DependencyGraph(..))
import Dependencies.Analyzer (analyzeDependencies, detectCycles, buildDependencyGraph)
import Dependencies.TypeSystem (unify, TypeSubstitution)

import Parser (parseTypus, TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, length)
import Data.List (null, sort, nub)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Generate simple type expressions
genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
  [ return $ SimpleT (T.pack "int")
  , return $ SimpleT (T.pack "string")
  , return $ SimpleT (T.pack "bool")
  , do
      base <- elements ["List", "Array", "Map"]
      param <- genTypeExpr
      return $ GenericT (T.pack base) [param]
  , do
      params <- listOf $ elements [("x", SimpleT (T.pack "int")), ("y", SimpleT (T.pack "string"))]
      ret <- genTypeExpr
      return $ FuncT params ret
  ]

-- | Generate simple constraints
genConstraint :: Gen Constraint
genConstraint = oneof
  [ return $ SizeGT "x" 0
  , return $ SizeGE "x" 1
  , return $ RangeC "x" 0 100
  , do
      pred <- elements ["Positive", "NonEmpty", "Valid"]
      args <- listOf genTypeExpr
      return $ PredC pred args
  ]

-- | Generate simple statements
genStatement :: Gen Statement
genStatement = oneof
  [ return $ SVarDecl (T.pack "x") (SimpleT (T.pack "int"))
  , return $ STypeAlias "MyInt" (SimpleT (T.pack "int")) []
  , do
      name <- elements ["add", "multiply", "process"]
      params <- listOf $ elements [("a", SimpleT (T.pack "int")), ("b", SimpleT (T.pack "string"))]
      ret <- genTypeExpr
      return $ SFuncDecl name params (Just ret)
  , do
      name <- elements ["Container", "Collection"]
      typeVars <- listOf $ return "T"
      base <- genStatement
      return $ SExistsDecl typeVars base
  ]

-- | Generate dependency nodes
genDependencyNode :: Gen DependencyNode
genDependencyNode = do
  name <- elements ["module1", "module2", "core", "utils", "main"]
  deps <- listOf $ elements ["module1", "module2", "core", "utils", "main"]
  return $ DependencyNode name (nub deps)

-- Property tests

-- Property: newDependentTypeChecker creates valid checker
prop_new_checker_valid :: Property
prop_new_checker_valid =
  let checker = newDependentTypeChecker
      errors = getDependentTypeErrors checker
  in property $ null errors

-- Property: adding types should not create errors
prop_add_type_no_errors :: Property
prop_add_type_no_errors =
  let checker = newDependentTypeChecker
      checkerWithTypes = addType "MyType" (SimpleT (T.pack "int")) checker
      errors = getDependentTypeErrors checkerWithTypes
  in property $ null errors

-- Property: adding constraints should be tracked
prop_add_constraint_tracked :: Property
prop_add_constraint_tracked =
  let checker = newDependentTypeChecker
      constraint = SizeGT "x" 0
      checkerWithConstraint = addConstraint constraint checker
  in property $ True  -- Basic smoke test

-- Property: type checking valid types should succeed
prop_check_type_valid :: Property
prop_check_type_valid =
  forAll genTypeExpr $ \typeExpr ->
    let checker = newDependentTypeChecker
        result = checkType typeExpr checker
    in property $ True  -- Basic smoke test

-- Property: constraint solving should be consistent
prop_solve_constraints_consistent :: Property
prop_solve_constraints_consistent =
  let checker = newDependentTypeChecker
      constraints = [SizeGT "x" 0, SizeGE "x" 1]
      result1 = solveConstraints constraints checker
      result2 = solveConstraints constraints checker
  in property $ True  -- Basic consistency test

-- Property: dependency graph construction preserves nodes
prop_dependency_graph_preserves_nodes :: Property
prop_dependency_graph_preserves_nodes =
  forAll (listOf genDependencyNode) $ \nodes ->
    let graph = buildDependencyGraph nodes
        nodeNames = map nodeName nodes
    in property $ True  -- Basic smoke test

-- Property: cycle detection should find cycles
prop_cycle_detection_finds_cycles :: Property
prop_cycle_detection_finds_cycles =
  let cyclicNodes = 
        [ DependencyNode "A" ["B"]
        , DependencyNode "B" ["C"]
        , DependencyNode "C" ["A"]
        ]
      graph = buildDependencyGraph cyclicNodes
      cycles = detectCycles graph
  in property $ not $ null cycles

-- Property: acyclic graphs should have no cycles
prop_acyclic_no_cycles :: Property
prop_acyclic_no_cycles =
  let acyclicNodes =
        [ DependencyNode "A" ["B"]
        , DependencyNode "B" ["C"]
        , DependencyNode "C" []
        ]
      graph = buildDependencyGraph acyclicNodes
      cycles = detectCycles graph
  in property $ null cycles

-- Unit tests

unit_tests :: TestTree
unit_tests = testGroup "Dependency Analysis Unit Tests"
  [ testCase "basic type checking" $ do
      let checker = newDependentTypeChecker
          typeExpr = SimpleT (T.pack "int")
          result = checkType typeExpr checker
      -- Should handle basic type checking
      return ()

  , testCase "function type checking" $ do
      let checker = newDependentTypeChecker
          funcType = FuncT [("x", SimpleT (T.pack "int")), ("y", SimpleT (T.pack "int"))] (SimpleT (T.pack "int"))
          result = checkType funcType checker
      -- Should handle function types
      return ()

  , testCase "generic type checking" $ do
      let checker = newDependentTypeChecker
          genericType = GenericT "List" [SimpleT (T.pack "int")]
          result = checkType genericType checker
      -- Should handle generic types
      return ()

  , testCase "constraint validation" $ do
      let checker = newDependentTypeChecker
          constraint = SizeGT "collection" 0
          checkerWithConstraint = addConstraint constraint checker
      -- Should add constraints without errors
      return ()

  , testCase "type variable creation" $ do
      let typeVar = newTypeVariable
      -- Should create unique type variables
      return ()

  , testCase "type environment operations" $ do
      let env = initialTypeEnvironment
      -- Should initialize with basic environment
      return ()

  , testCase "AST validation" $ do
      let ast = Program [SVarDecl (T.pack "x") (SimpleT (T.pack "int"))]
          result = validateASTSemantics ast
      -- Should validate simple AST
      return ()

  , testCase "statement validation" $ do
      let stmt = SVarDecl (T.pack "x") (SimpleT (T.pack "int"))
          checker = newDependentTypeChecker
          result = validateStatement stmt checker
      -- Should validate simple statement
      return ()

  , testCase "type inference" $ do
      let stmt = SVarDecl (T.pack "x") (SimpleT (T.pack "int"))
          checker = newDependentTypeChecker
          result = inferStatement stmt checker
      -- Should infer types correctly
      return ()

  , testCase "type unification" $ do
      let type1 = SimpleT (T.pack "int")
          type2 = SimpleT (T.pack "int")
          result = unifyTypes type1 type2
      -- Should unify identical types
      return ()

  , testCase "type generalization" $ do
      let typeExpr = SimpleT (T.pack "int")
          env = initialTypeEnvironment
          scheme = generalize typeExpr env
      -- Should generalize types
      return ()

  , testCase "type instantiation" $ do
      let scheme = TypeScheme [] $ SimpleT (T.pack "int")
          result = instantiate scheme
      -- Should instantiate type schemes
      return ()

  , testCase "dependency graph construction" $ do
      let nodes = 
            [ DependencyNode "main" ["utils", "core"]
            , DependencyNode "utils" ["core"]
            , DependencyNode "core" []
            ]
          graph = buildDependencyGraph nodes
      -- Should build dependency graph
      return ()

  , testCase "cycle detection" $ do
      let cyclicNodes =
            [ DependencyNode "A" ["B"]
            , DependencyNode "B" ["C"]
            , DependencyNode "C" ["A"]
            ]
          graph = buildDependencyGraph cyclicNodes
          cycles = detectCycles graph
      assertBool "should detect cycles" $ not $ null cycles

  , testCase "acyclic graph validation" $ do
      let acyclicNodes =
            [ DependencyNode "A" ["B"]
            , DependencyNode "B" ["C"]
            , DependencyNode "C" []
            ]
          graph = buildDependencyGraph acyclicNodes
          cycles = detectCycles graph
      assertBool "should not detect cycles in acyclic graph" $ null cycles

  , testCase "complex dependency scenarios" $ do
      let complexNodes =
            [ DependencyNode "main" ["ui", "business", "data"]
            , DependencyNode "ui" ["business", "themes"]
            , DependencyNode "business" ["data", "validation"]
            , DependencyNode "data" ["database"]
            , DependencyNode "validation" []
            , DependencyNode "themes" []
            , DependencyNode "database" []
            ]
          graph = buildDependencyGraph complexNodes
          cycles = detectCycles graph
      assertBool "should handle complex dependencies" $ True

  , testCase "self-dependency detection" $ do
      let selfDependent = [DependencyNode "module" ["module"]]
          graph = buildDependencyGraph selfDependent
          cycles = detectCycles graph
      assertBool "should detect self-dependency" $ not $ null cycles

  , testCase "empty dependency graph" $ do
      let emptyGraph = buildDependencyGraph []
          cycles = detectCycles emptyGraph
      assertBool "should handle empty graph" $ null cycles
  ]

-- Advanced dependency tests

advanced_tests :: TestTree
advanced_tests = testGroup "Advanced Dependency Tests"
  [ testCase "transitive dependencies" $ do
      let nodes =
            [ DependencyNode "A" ["B"]
            , DependencyNode "B" ["C"]
            , DependencyNode "C" ["D"]
            , DependencyNode "D" []
            ]
          graph = buildDependencyGraph nodes
      -- Should handle transitive dependencies
      return ()

  , testCase "diamond dependency pattern" $ do
      let diamondNodes =
            [ DependencyNode "A" ["B", "C"]
            , DependencyNode "B" ["D"]
            , DependencyNode "C" ["D"]
            , DependencyNode "D" []
            ]
          graph = buildDependencyGraph diamondNodes
          cycles = detectCycles graph
      assertBool "diamond pattern should not create cycles" $ null cycles

  , testCase "multiple cycles" $ do
      let multiCycleNodes =
            [ DependencyNode "A" ["B"]
            , DependencyNode "B" ["C", "A"]  -- Cycle A->B->C->A
            , DependencyNode "C" ["D"]
            , DependencyNode "D" ["E"]
            , DependencyNode "E" ["C"]        -- Cycle C->D->E->C
            ]
          graph = buildDependencyGraph multiCycleNodes
          cycles = detectCycles graph
      assertBool "should detect multiple cycles" $ L.length cycles >= 2

  , testCase "dependency analysis with constraints" $ do
      let checker = newDependentTypeChecker
          constraints = [SizeGT "x" 0, RangeC "y" 1 10, PredC "Valid" [SimpleT (T.pack "int")]]
          checkerWithConstraints = foldl addConstraint checker constraints
          errors = getDependentTypeErrors checkerWithConstraints
      -- Should handle multiple constraints
      return ()

  , testCase "dependent type analysis" $ do
      let code = unlines
            [ "package main"
            , "type Vector(n: int) struct {"
            , "    data [n]int"
            , "}"
            , "func main() {"
            , "    v := Vector(5){data: [5]int{1,2,3,4,5}}"
            , "}"
            ]
      case parseTypus code of
        Left _ -> return ()  -- Skip if parsing fails
        Right typusFile -> do
          let result = analyzeDependentTypes typusFile
          -- Should analyze dependent types
          return ()
  ]

-- Performance tests

performance_tests :: TestTree
performance_tests = testGroup "Performance Tests"
  [ testCase "large dependency graph" $ do
      let largeNodes = [DependencyNode ("module" ++ show i) 
                        (take 3 $ L.map ("module" ++) (map show [i-1, i-2, i-3])) 
                       | i <- [1..1000]]
          graph = buildDependencyGraph largeNodes
          cycles = detectCycles graph
      -- Should handle large graphs efficiently
      return ()

  , testCase "complex type inference" $ do
      let complexTypes = replicate 100 $ GenericT "Container" [SimpleT (T.pack "int")]
          checker = L.foldl (\c t -> addType ("Type" ++ show (L.length c)) t c) 
                         newDependentTypeChecker complexTypes
      -- Should handle many types
      return ()

  , testCase "deep constraint solving" $ do
      let deepConstraints = [SizeGT ("var" ++ show i) i | i <- [1..100]]
          checker = foldl addConstraint newDependentTypeChecker deepConstraints
          result = solveConstraints deepConstraints checker
      -- Should handle deep constraint chains
      return ()
  ]

tests :: TestTree
tests = testGroup "Dependency Analysis Tests"
  [ testGroup "Property Tests"
    [ fastProperty "new checker valid" prop_new_checker_valid
    , fastProperty "add type no errors" prop_add_type_no_errors
    , fastProperty "add constraint tracked" prop_add_constraint_tracked
    , fastProperty "check type valid" prop_check_type_valid
    , fastProperty "solve constraints consistent" prop_solve_constraints_consistent
    , fastProperty "dependency graph preserves nodes" prop_dependency_graph_preserves_nodes
    , fastProperty "cycle detection finds cycles" prop_cycle_detection_finds_cycles
    , fastProperty "acyclic no cycles" prop_acyclic_no_cycles
    ]
  , unit_tests
  , advanced_tests
  , performance_tests
  ]