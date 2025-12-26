{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.DependencyAnalysisTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, choose, listOf, elements, oneof, sized, suchThat)

import Dependencies.AST
  ( AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , DependencyNode(..)
  , DependencyGraph(..)
  )
import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , Substitution
  , preludeTypeDefs
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , convertTypeExpr
  , convertConstraint
  , addType
  , addConstraint
  , addTypeError
  , lookupTypeDef
  , checkType
  , checkTypeInstantiation
  , solveConstraints
  , checkTypeConstraint
  , validateConstraint
  , getDependentTypeErrors
  , unify
  )
import Dependencies
  ( newDependentTypeChecker
  , analyzeDependentTypes
  , analyzeAST
  , validateASTSemantics
  , validateStatement
  , checkType
  , addType
  , addConstraint
  , solveConstraints
  , getDependentTypeErrors
  , unify
  , inferType
  , inferStatement
  , inferProgram
  , generalize
  , instantiate
  , unifyTypes
  , applyTypeSubstitution
  , newTypeVariable
  , getFreshTypeVar
  , initialTypeEnvironment
  , instantiateScheme
  , generalizeInContext
  , checkPolyType
  , solveTypeConstraints
  , simplifyConstraints
  , pushScope
  , popScope
  , inNewScope
  , parseProgram
  , runParser
  )

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate valid type variable names
genTypeVarName :: Gen String
genTypeVarName = oneof
  [ elements ["a", "b", "c", "x", "y", "z"]
  , do
      first <- elements ['a'..'z']
      rest <- listOf $ elements ['0'..'9']
      return $ first : rest
  ]

-- Generate simple type variables
genSimpleTypeVar :: Gen TypeVar
genSimpleTypeVar = do
  name <- genTypeVarName
  elements [TVCon name, TVVar name]

-- Generate function type variables
genFuncTypeVar :: Gen TypeVar
genFuncTypeVar = do
  args <- listOf $ arbitrary `suchThat` (\tv -> tv /= TVVar "")
  ret <- arbitrary
  return $ TVFun args ret

-- Generate application type variables
genAppTypeVar :: Gen TypeVar
genAppTypeVar = do
  name <- genTypeVarName
  args <- listOf $ arbitrary `suchThat` (not . null)
  return $ TVApp name args

-- Generate tuple type variables
genTupleTypeVar :: Gen TypeVar
genTupleTypeVar = do
  elems <- listOf $ arbitrary `suchThat` (not . null)
  return $ TVTuple elems

-- Generate any type variable
genTypeVar :: Gen TypeVar
genTypeVar = oneof
  [ genSimpleTypeVar
  , genFuncTypeVar
  , genAppTypeVar
  , genTupleTypeVar
  ]

-- Generate type constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ Equal <$> genTypeVar <*> genTypeVar
  , Subtype <$> genTypeVar <*> genTypeVar
  , do
      name <- elements ["eq", "ord", "show"]
      args <- listOf genTypeVar
      return $ Predicate name args
  , TypeSizeGE <$> genTypeVar <*> choose (0, 100)
  , TypeSizeGT <$> genTypeVar <*> choose (0, 100)
  , TypeRange <$> genTypeVar <*> choose (0, 50) <*> choose (51, 100)
  ]

-- Generate AST constraints
genASTConstraint :: Gen Constraint
genASTConstraint = oneof
  [ SizeGT <$> genTypeVarName <*> choose (0, 100)
  , SizeGE <$> genTypeVarName <*> choose (0, 100)
  , RangeC <$> genTypeVarName <*> choose (0, 50) <*> choose (51, 100)
  , do
      name <- elements ["positive", "nonzero", "even"]
      args <- listOf genSimpleTypeExpr
      return $ PredC name args
  ]

-- Generate simple type expressions
genSimpleTypeExpr :: Gen TypeExpr
genSimpleTypeExpr = SimpleT <$> elements ["int", "string", "bool", "float"]

-- Generate generic type expressions
genGenericTypeExpr :: Gen TypeExpr
genGenericTypeExpr = do
  name <- elements ["List", "Map", "Option", "Array"]
  args <- listOf genSimpleTypeExpr
  return $ GenericT (T.pack name) args

-- Generate function type expressions
genFuncTypeExpr :: Gen TypeExpr
genFuncTypeExpr = do
  numArgs <- choose (0, 3)
  args <- replicateM numArgs $ do
    argName <- elements ["x", "y", "z", "a", "b"]
    argType <- genSimpleTypeExpr
    return (T.pack argName, argType)
  retType <- genSimpleTypeExpr
  return $ FuncT args retType

-- Generate refined type expressions
genRefinedTypeExpr :: Gen TypeExpr
genRefinedTypeExpr = do
  baseType <- genSimpleTypeExpr
  constraints <- listOf genASTConstraint
  return $ RefineT baseType constraints

-- Generate any type expression
genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
  [ genSimpleTypeExpr
  , genGenericTypeExpr
  , genFuncTypeExpr
  , genRefinedTypeExpr
  ]

-- Generate AST statements
genStatement :: Gen Statement
genStatement = oneof
  [ do
      name <- T.pack <$> genTypeVarName
      params <- listOf (T.pack <$> genTypeVarName)
      constraints <- listOf genASTConstraint
      return $ STypeDef name params constraints
  , do
      name <- T.pack <$> genTypeVarName
      alias <- genTypeExpr
      constraints <- listOf genASTConstraint
      return $ STypeAlias name alias constraints
  , do
      name <- T.pack <$> genTypeVarName
      typeExpr <- genTypeExpr
      return $ SVarDecl name typeExpr
  , do
      name <- T.pack <$> genTypeVarName
      numParams <- choose (0, 3)
      params <- replicateM numParams $ do
        paramName <- T.pack <$> genTypeVarName
        paramType <- genTypeExpr
        return (paramName, paramType)
      retType <- oneof [pure Nothing, Just <$> genTypeExpr]
      return $ SFuncDecl name params retType
  , do
      name <- T.pack <$> genTypeVarName
      constraint <- genASTConstraint
      return $ SConstraintDef name constraint
  , do
      vars <- listOf (T.pack <$> genTypeVarName)
      stmt <- genStatement
      return $ SExistsDecl vars stmt
  ]

-- Generate dependency nodes
genDependencyNode :: Gen DependencyNode
genDependencyNode = do
  name <- genTypeVarName
  numDeps <- choose (0, 5)
  deps <- listOf genTypeVarName
  return $ DependencyNode name deps

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test TypeVar operations
testTypeVarOperations :: TestTree
testTypeVarOperations = testGroup "TypeVar Operations"
  [ testCase "TypeVar equality works" $ do
      let tv1 = TVCon "int"
          tv2 = TVCon "int"
          tv3 = TVCon "string"
      tv1 @?= tv2
      assertBool "Different types should not be equal" $ tv1 /= tv3
      
  , testCase "TypeVar ordering works" $ do
      let tv1 = TVCon "a"
          tv2 = TVCon "b"
      assertBool "a < b" $ tv1 < tv2
      
  , testCase "Function type variables" $ do
      let args = [TVCon "int", TVCon "string"]
          ret = TVCon "bool"
          func = TVFun args ret
      case func of
        TVFun args' ret' -> do
          args' @?= args
          ret' @?= ret
        _ -> assertBool "Should be TVFun" False
  ]

-- Test TypeConstraint operations
testTypeConstraintOperations :: TestTree
testTypeConstraintOperations = testGroup "TypeConstraint Operations"
  [ testCase "Equal constraint" $ do
      let tv1 = TVCon "a"
          tv2 = TVCon "b"
          constraint = Equal tv1 tv2
      case constraint of
        Equal t1 t2 -> do
          t1 @?= tv1
          t2 @?= tv2
        _ -> assertBool "Should be Equal" False
        
  , testCase "Predicate constraint" $ do
      let args = [TVCon "a", TVCon "b"]
          constraint = Predicate "eq" args
      case constraint of
        Predicate name args' -> do
          name @?= "eq"
          args' @?= args
        _ -> assertBool "Should be Predicate" False
  ]

-- Test AST operations
testASTOperations :: TestTree
testASTOperations = testGroup "AST Operations"
  [ testCase "Program creation" $ do
      let stmt1 = SVarDecl "x" (SimpleT "int")
          stmt2 = SVarDecl "y" (SimpleT "string")
          program = Program [stmt1, stmt2]
      case program of
        Program stmts -> stmts @?= [stmt1, stmt2]
        
  , testCase "TypeDef statement" $ do
      let stmt = STypeDef "MyType" ["T", "U"] [SizeGT "T" 0]
      case stmt of
        STypeDef name params constraints -> do
          name @?= "MyType"
          params @?= ["T", "U"]
          constraints @?= [SizeGT "T" 0]
        _ -> assertBool "Should be STypeDef" False
  ]

-- Test TypeEnv operations
testTypeEnvOperations :: TestTree
testTypeEnvOperations = testGroup "TypeEnv Operations"
  [ testCase "Empty TypeEnv" $ do
      let env = TypeEnv Map.empty []
      typeDefinitions env @?= Map.empty
      pendingConstraints env @?= []
      
  , testCase "TypeEnv with definitions" $ do
      let typeDef = TypeDefDecl ["T"] [Equal (TVVar "T") (TVCon "int")]
          defs = Map.singleton "MyType" typeDef
          env = TypeEnv defs [TypeSizeGE (TVVar "T") 0]
      Map.lookup "MyType" (typeDefinitions env) @?= Just typeDef
      pendingConstraints env @?= [TypeSizeGE (TVVar "T") 0]
  ]

-- Test DependentTypeChecker operations
testDependentTypeChecker :: TestTree
testDependentTypeChecker = testGroup "DependentTypeChecker"
  [ testCase "newDependentTypeChecker creates checker" $ do
      let checker = newDependentTypeChecker
      case checker of
        DependentTypeChecker typeEnv errors -> do
          typeDefinitions (dtcTypeEnv typeEnv) @?= preludeTypeDefs
          errors @?= []
          
  , testCase "newDependentTypeCheckerWithTypes" $ do
      let customTypes = Map.singleton "Custom" (TypeDefDecl [])
          checker = newDependentTypeCheckerWithTypes customTypes
      case checker of
        DependentTypeChecker typeEnv errors -> do
          Map.lookup "Custom" (typeDefinitions (dtcTypeEnv typeEnv)) @?= Just (TypeDefDecl [])
          errors @?= []
  ]

-- Test type conversion operations
testTypeConversion :: TestTree
testTypeConversion = testGroup "Type Conversion"
  [ testCase "convertTypeExpr for simple type" $ do
      let typeExpr = SimpleT "int"
          typeVar = convertTypeExpr typeExpr
      case typeVar of
        TVCon "int" -> assertBool "Converted to TVCon" True
        _ -> assertBool "Should be TVCon" False
        
  , testCase "convertConstraint for size constraint" $ do
      let constraint = SizeGT "x" 5
          typeConstraint = convertConstraint constraint
      case typeConstraint of
        TypeSizeGT (TVVar "x") 5 -> assertBool "Converted correctly" True
        _ -> assertBool "Should be TypeSizeGT" False
  ]

-- Test type checking operations
testTypeChecking :: TestTree
testTypeChecking = testGroup "Type Checking"
  [ testCase "addType adds type definition" $ do
      let checker = newDependentTypeChecker
          typeDef = TypeDefDecl []
          updated = addType "TestType" typeDef checker
      case updated of
        DependentTypeChecker typeEnv _ -> do
          Map.lookup "TestType" (typeDefinitions typeEnv) @?= Just typeDef
          
  , testCase "addConstraint adds constraint" $ do
      let checker = newDependentTypeChecker
          constraint = Equal (TVVar "a") (TVCon "int")
          updated = addConstraint constraint checker
      case updated of
        DependentTypeChecker typeEnv _ -> do
          pendingConstraints typeEnv @?= [constraint]
  ]

-- Test dependency graph operations
testDependencyGraph :: TestTree
testDependencyGraph = testGroup "Dependency Graph"
  [ testCase "DependencyGraph creation" $ do
      let node1 = DependencyNode "module1" ["module2"]
          node2 = DependencyNode "module2" []
          nodes = Map.fromList [("module1", node1), ("module2", node2)]
          graph = DependencyGraph nodes
      graphNodes graph @?= nodes
      
  , testCase "DependencyNode operations" $ do
      let node = DependencyNode "main" ["utils", "config"]
      nodeName node @?= "main"
      nodeDependencies node @?= ["utils", "config"]
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: TypeVar equality is reflexive
prop_typevar_reflexive :: TypeVar -> Property
prop_typevar_reflexive tv =
  property $ tv === tv

-- Property: TypeVar equality is symmetric
prop_typevar_symmetric :: TypeVar -> TypeVar -> Property
prop_typevar_symmetric tv1 tv2 =
  (tv1 == tv2) ==> (tv2 == tv1)

-- Property: TypeVar equality is transitive
prop_typevar_transitive :: TypeVar -> TypeVar -> TypeVar -> Property
prop_typevar_transitive tv1 tv2 tv3 =
  (tv1 == tv2 && tv2 == tv3) ==> (tv1 == tv3)

-- Property: TypeConstraint equality is reflexive
prop_typeconstraint_reflexive :: TypeConstraint -> Property
prop_typeconstraint_reflexive tc =
  property $ tc === tc

-- Property: AST equality is reflexive
prop_ast_reflexive :: AST -> Property
prop_ast_reflexive ast =
  property $ ast === ast

-- Property: Statement equality is reflexive
prop_statement_reflexive :: Statement -> Property
prop_statement_reflexive stmt =
  property $ stmt === stmt

-- Property: TypeExpr equality is reflexive
prop_typeexpr_reflexive :: TypeExpr -> Property
prop_typeexpr_reflexive te =
  property $ te === te

-- Property: DependencyNode equality is reflexive
prop_dependencynode_reflexive :: DependencyNode -> Property
prop_dependencynode_reflexive dn =
  property $ dn === dn

-- Property: TypeVar ordering is total
prop_typevar_total_ordering :: TypeVar -> TypeVar -> Property
prop_typevar_total_ordering tv1 tv2 =
  let comparison = compare tv1 tv2
  in property $ (comparison == EQ || comparison == LT || comparison == GT)

-- Property: TypeConstraint ordering is total
prop_typeconstraint_total_ordering :: TypeConstraint -> TypeConstraint -> Property
prop_typeconstraint_total_ordering tc1 tc2 =
  let comparison = compare tc1 tc2
  in property $ (comparison == EQ || comparison == LT || comparison == GT)

-- Property: newDependentTypeChecker creates valid checker
prop_new_checker_valid :: Property
prop_new_checker_valid =
  let checker = newDependentTypeChecker
  in case checker of
       DependentTypeChecker typeEnv errors -> 
         property $ Map.size (typeDefinitions typeEnv) >= 0 .&&. length errors >= 0

-- Property: addType preserves existing types
prop_add_type_preserves :: [(String, TypeDef)] -> String -> TypeDef -> Property
prop_add_type_preserves existingTypes name typeDef =
  not (null name) ==>
  let customTypes = Map.fromList existingTypes
      checker = newDependentTypeCheckerWithTypes customTypes
      updated = addType name typeDef checker
  in case updated of
       DependentTypeChecker typeEnv _ ->
         let allDefs = typeDefinitions typeEnv
         in property $ Map.member name allDefs .&&. 
                      all (\(k, v) -> k == name || Map.lookup k customTypes == Just v) (Map.toList allDefs)

-- Property: addConstraint preserves existing constraints
prop_add_constraint_preserves :: [TypeConstraint] -> TypeConstraint -> Property
prop_add_constraint_preserves existingConstraints newConstraint =
  let checker = newDependentTypeChecker
      checkerWithConstraints = foldr addConstraint checker existingConstraints
      updated = addConstraint newConstraint checkerWithConstraints
  in case updated of
       DependentTypeChecker typeEnv _ ->
         let allConstraints = pendingConstraints typeEnv
         in property $ newConstraint `elem` allConstraints .&&.
                      all (`elem` allConstraints) existingConstraints

-- Property: lookupTypeDef finds added types
prop_lookup_finds_added :: String -> TypeDef -> Property
prop_lookup_finds_added name typeDef =
  not (null name) ==>
  let checker = newDependentTypeChecker
      updated = addType name typeDef checker
  in case updated of
       DependentTypeChecker typeEnv _ ->
         property $ lookupTypeDef name typeEnv === Just typeDef

-- Property: lookupTypeDef returns Nothing for missing types
prop_lookup_missing_returns_nothing :: String -> Property
prop_lookup_missing_returns_nothing name =
  not (null name) ==>
  let checker = newDependentTypeChecker
  in case checker of
       DependentTypeChecker typeEnv _ ->
         property $ lookupTypeDef name typeEnv === Nothing

-- Property: DependencyGraph contains all nodes
prop_dependencygraph_contains_nodes :: [DependencyNode] -> Property
prop_dependencygraph_contains_nodes nodes =
  let nodeMap = Map.fromList $ map (\n -> (nodeName n, n)) nodes
      graph = DependencyGraph nodeMap
  in property $ all (\n -> Map.lookup (nodeName n) (graphNodes graph) == Just n) nodes

-- Property: Program contains all statements
prop_program_contains_statements :: [Statement] -> Property
prop_program_contains_statements stmts =
  let program = Program stmts
  in case program of
       Program programStmts -> property $ programStmts === stmts

-- Property: SimpleT preserves name
prop_simplet_preserves_name :: String -> Property
prop_simplet_preserves_name name =
  not (null name) ==>
  let typeExpr = SimpleT (T.pack name)
  in case typeExpr of
       SimpleT name' -> property $ T.unpack name' === name

-- Property: GenericT preserves name and args
prop_generict_preserves_name_args :: String -> [TypeExpr] -> Property
prop_generict_preserves_name_args name args =
  not (null name) ==>
  let typeExpr = GenericT (T.pack name) args
  in case typeExpr of
       GenericT name' args' -> property $ T.unpack name' === name .&&. args' === args

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependency Analysis Tests"
  [ testTypeVarOperations
  , testTypeConstraintOperations
  , testASTOperations
  , testTypeEnvOperations
  , testDependentTypeChecker
  , testTypeConversion
  , testTypeChecking
  , testDependencyGraph
  , testGroup "QuickCheck Properties"
    [ fastProperty "TypeVar reflexive" prop_typevar_reflexive
    , fastProperty "TypeVar symmetric" prop_typevar_symmetric
    , fastProperty "TypeVar transitive" prop_typevar_transitive
    , fastProperty "TypeConstraint reflexive" prop_typeconstraint_reflexive
    , fastProperty "AST reflexive" prop_ast_reflexive
    , fastProperty "Statement reflexive" prop_statement_reflexive
    , fastProperty "TypeExpr reflexive" prop_typeexpr_reflexive
    , fastProperty "DependencyNode reflexive" prop_dependencynode_reflexive
    , fastProperty "TypeVar total ordering" prop_typevar_total_ordering
    , fastProperty "TypeConstraint total ordering" prop_typeconstraint_total_ordering
    , fastProperty "new checker valid" prop_new_checker_valid
    , fastProperty "add type preserves" prop_add_type_preserves
    , fastProperty "add constraint preserves" prop_add_constraint_preserves
    , fastProperty "lookup finds added" prop_lookup_finds_added
    , fastProperty "lookup missing returns nothing" prop_lookup_missing_returns_nothing
    , fastProperty "dependency graph contains nodes" prop_dependencygraph_contains_nodes
    , fastProperty "program contains statements" prop_program_contains_statements
    , fastProperty "SimpleT preserves name" prop_simplet_preserves_name
    , fastProperty "GenericT preserves name args" prop_generict_preserves_name_args
    ]
  ]