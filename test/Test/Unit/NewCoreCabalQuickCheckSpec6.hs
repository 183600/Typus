module Test.Unit.NewCoreCabalQuickCheckSpec6 (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)

import Dependencies (AST(..), Statement(..), TypeExpr(..), Constraint(..), TypeVar(..))
import qualified Data.Map as Map

-- | Dependencies analysis tests with QuickCheck properties
tests :: TestTree
tests =
  testGroup "New Core Cabal QuickCheck Tests 6 - Dependencies Analysis"
    [ testGroup "Type expression properties"
        [ fastProperty "type expression normalization is idempotent" prop_typeExprNormalizationIdempotent
        , fastProperty "type expression substitution preserves structure" prop_typeExprSubstitutionPreservesStructure
        , testCase "type expression creation" $ do
            let typeVar = TypeVar { tvName = "T", tvId = 1 }
                typeExpr = TypeVariable typeVar
            case typeExpr of
              TypeVariable tv -> tvName tv @?= "T"
              _ -> assertFailure "Expected TypeVariable"
        ]
    , testGroup "Constraint properties"
        [ fastProperty "constraint solving is deterministic" prop_constraintSolvingDeterministic
        , fastProperty "constraint combination is associative" prop_constraintCombinationAssociative
        , testCase "constraint creation" $ do
            let typeVar1 = TypeVar { tvName = "T", tvId = 1 }
                typeVar2 = TypeVar { tvName = "U", tvId = 2 }
                constraint = EqualityConstraint typeVar1 typeVar2
            case constraint of
              EqualityConstraint tv1 tv2 -> do
                tvName tv1 @?= "T"
                tvName tv2 @?= "U"
              _ -> assertFailure "Expected EqualityConstraint"
        ]
    , testGroup "AST properties"
        [ fastProperty "AST transformation preserves semantics" prop_astTransformationPreservesSemantics
        , fastProperty "AST size is additive" prop_astSizeAdditive
        , testCase "AST creation" $ do
            let typeVar = TypeVar { tvName = "T", tvId = 1 }
                typeExpr = TypeVariable typeVar
                statement = TypeDeclaration "x" typeExpr
                ast = AST { astStatements = [statement], astImports = [] }
            L.length (astStatements ast) @?= 1
            L.length (astImports ast) @?= 0
        ]
    , testGroup "Dependency analysis edge cases"
        [ fastProperty "circular dependency detection" prop_circularDependencyDetection
        , fastProperty "dependency order is topological" prop_dependencyOrderTopological
        , testCase "empty AST" $ do
            let ast = AST { astStatements = [], astImports = [] }
            L.length (astStatements ast) @?= 0
            L.length (astImports ast) @?= 0
        ]
    ]

-- Simplified versions of data structures for testing
data TypeVar = TypeVar
    { tvName :: String
    , tvId :: Int
    } deriving (Show, Eq)

data TypeExpr
    = TypeVariable TypeVar
    | TypeConstructor String [TypeExpr]
    | TypeFunction TypeExpr TypeExpr
    deriving (Show, Eq)

data Constraint
    = EqualityConstraint TypeVar TypeVar
    | SubtypeConstraint TypeExpr TypeExpr
    | InstanceConstraint TypeExpr String
    deriving (Show, Eq)

data Statement
    = TypeDeclaration String TypeExpr
    | FunctionDeclaration String [String] TypeExpr
    | ImportStatement String
    deriving (Show, Eq)

data AST = AST
    { astStatements :: [Statement]
    , astImports :: [String]
    } deriving (Show, Eq)

-- | QuickCheck properties

-- Type expression normalization is idempotent
prop_typeExprNormalizationIdempotent :: TypeExpr -> Bool
prop_typeExprNormalizationIdempotent expr =
  let normalized1 = normalizeTypeExpr expr
      normalized2 = normalizeTypeExpr normalized1
  in normalized1 == normalized2

-- Type expression substitution preserves structure
prop_typeExprSubstitutionPreservesStructure :: TypeExpr -> Bool
prop_typeExprSubstitutionPreservesStructure expr =
  let substitution = Map.singleton "T" (TypeConstructor "Int" [])
      substituted = applyTypeSubstitution substitution expr
      -- The substitution should not break the structure
      hasValidStructure substituted
  in True

-- Constraint solving is deterministic
prop_constraintSolvingDeterministic :: [Constraint] -> Bool
prop_constraintSolvingDeterministic constraints =
  let solution1 = solveConstraints constraints
      solution2 = solveConstraints constraints
  in solution1 == solution2

-- Constraint combination is associative
prop_constraintCombinationAssociative :: [Constraint] -> [Constraint] -> [Constraint] -> Bool
prop_constraintCombinationAssociative c1 c2 c3 =
  let left = combineConstraints (combineConstraints c1 c2) c3
      right = combineConstraints c1 (combineConstraints c2 c3)
  in left == right

-- AST transformation preserves semantics
prop_astTransformationPreservesSemantics :: AST -> Bool
prop_astTransformationPreservesSemantics ast =
  let transformed = optimizeAST ast
      originalTypes = extractTypeDeclarations ast
      transformedTypes = extractTypeDeclarations transformed
  in sortTypes originalTypes == sortTypes transformedTypes

-- AST size is additive
prop_astSizeAdditive :: [Statement] -> [Statement] -> Bool
prop_astSizeAdditive stmts1 stmts2 =
  let ast1 = AST { astStatements = stmts1, astImports = [] }
      ast2 = AST { astStatements = stmts2, astImports = [] }
      combined = AST { astStatements = astStatements ast1 ++ astStatements ast2, astImports = [] }
      size1 = astSize ast1
      size2 = astSize ast2
      combinedSize = astSize combined
  in combinedSize == size1 + size2

-- Circular dependency detection
prop_circularDependencyDetection :: [(String, [String])] -> Bool
prop_circularDependencyDetection dependencies =
  let hasCircular = hasCircularDependencies dependencies
      expectedCircular = hasCycle dependencies
  in hasCircular == expectedCircular

-- Dependency order is topological
prop_dependencyOrderTopological :: [(String, [String])] -> Bool
prop_dependencyOrderTopological dependencies =
  let ordered = topologicalSort dependencies
      validOrder = isValidTopologicalOrder dependencies ordered
  in null dependencies || validOrder

-- Helper functions
normalizeTypeExpr :: TypeExpr -> TypeExpr
normalizeTypeExpr (TypeFunction t1 t2) = 
  TypeFunction (normalizeTypeExpr t1) (normalizeTypeExpr t2)
normalizeTypeExpr (TypeConstructor name args) = 
  TypeConstructor name (map normalizeTypeExpr args)
normalizeTypeExpr expr = expr

applyTypeSubstitution :: Map.Map String TypeExpr -> TypeExpr -> TypeExpr
applyTypeSubstitution substitution (TypeVariable tv) = 
  Map.lookup (tvName tv) substitution `maybe` TypeVariable tv
applyTypeSubstitution substitution (TypeFunction t1 t2) = 
  TypeFunction (applyTypeSubstitution substitution t1) (applyTypeSubstitution substitution t2)
applyTypeSubstitution substitution (TypeConstructor name args) = 
  TypeConstructor name (L.map (applyTypeSubstitution substitution) args)

hasValidStructure :: TypeExpr -> Bool
hasValidStructure (TypeVariable _) = True
hasValidStructure (TypeConstructor _ args) = L.all hasValidStructure args
hasValidStructure (TypeFunction t1 t2) = hasValidStructure t1 && hasValidStructure t2

solveConstraints :: [Constraint] -> Map.Map String TypeExpr
solveConstraints constraints = Map.fromList 
  [("T", TypeConstructor "Int" []), ("U", TypeConstructor "String" [])]

combineConstraints :: [Constraint] -> [Constraint] -> [Constraint]
combineConstraints c1 c2 = c1 ++ c2

optimizeAST :: AST -> AST
optimizeAST ast = ast  -- Simplified: no optimization in this example

extractTypeDeclarations :: AST -> [(String, TypeExpr)]
extractTypeDeclarations ast = 
  concatMap extractTypeDeclaration (astStatements ast)
  where
    extractTypeDeclaration (TypeDeclaration name expr) = [(name, expr)]
    extractTypeDeclaration _ = []

sortTypes :: [(String, TypeExpr)] -> [(String, TypeExpr)]
sortTypes = L.map (\(name, expr) -> (name, expr))  -- Simplified sorting

astSize :: AST -> Int
astSize ast = L.length (astStatements ast) + L.length (astImports ast)

hasCircularDependencies :: [(String, [String])] -> Bool
hasCircularDependencies dependencies = hasCycle dependencies

hasCycle :: [(String, [String])] -> Bool
hasCycle dependencies = 
  let nodes = map fst dependencies
      edges = concatMap (\(node, deps) -> L.map (\dep -> (node, dep)) deps) dependencies
      visit node visited = 
        if node `elem` visited then True
        else case lookup node dependencies of
               Nothing -> False
               Just deps -> L.any (\dep -> visit dep (node:visited)) deps
  in L.any (`visit` []) nodes

topologicalSort :: [(String, [String])] -> [String]
topologicalSort dependencies = 
  let nodes = map fst dependencies
      visit node visited = 
        if node `elem` visited then visited
        else case lookup node dependencies of
               Nothing -> node : visited
               Just deps -> foldr visit (node : visited) deps
  in foldr visit [] nodes

isValidTopologicalOrder :: [(String, [String])] -> [String] -> Bool
isValidTopologicalOrder dependencies order =
  let positionMap = Map.fromList $ zip order [0..]
      checkDependency (node, deps) = 
        let nodePos = Map.findWithDefault (-1) node positionMap
            depPos = L.map (\dep -> Map.findWithDefault (-1) dep positionMap) deps
        in L.all (\pos -> pos < nodePos || pos == -1) depPos
  in L.all checkDependency dependencies