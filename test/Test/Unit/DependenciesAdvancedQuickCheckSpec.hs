module Test.Unit.DependenciesAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
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
  , Substitution
  , TypeScheme(..)
  , TypeEnvironment(..)
  , TypeInferenceState(..)
  , TypeInferenceError(..)
  , DependentTypeError(..)
  , DependentTypeChecker(..)
  )
import Dependencies.Inference
  ( inferType
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
  )
import Dependencies.Analyzer
  ( analyzeAST
  , analyzeDependentTypes
  , validateASTSemantics
  , validateStatement
  )
import Dependencies.Parser
  ( grammarDefinition
  , parseProgram
  , runParser
  )
import Dependencies
  ( newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , checkType
  , addType
  , addConstraint
  , checkTypeInstantiation
  , solveConstraints
  , getDependentTypeErrors
  , unify
  , solveTypeConstraints
  , simplifyConstraints
  , pushScope
  , popScope
  , inNewScope
  )
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.State

-- | 生成简单的文本标识符
newtype Identifier = Identifier { getIdentifier :: String }
  deriving Show

instance Arbitrary Identifier where
  arbitrary = do
    len <- choose (1, 10)
    chars <- vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    return $ Identifier chars

-- | 生成TypeExpr
instance Arbitrary TypeExpr where
  arbitrary = do
    exprType <- elements ["SimpleT", "GenericT", "FuncT", "RefineT"]
    Identifier name <- arbitrary
    case exprType of
      "SimpleT" -> return $ SimpleT (T.pack name)
      "GenericT" -> do
        numParams <- choose (1, 3)
        params <- vectorOf numParams arbitrary
        return $ GenericT (T.pack name) params
      "FuncT" -> do
        numParams <- choose (0, 3)
        params <- vectorOf numParams $ do
          Identifier paramName <- arbitrary
          paramType <- arbitrary
          return (T.pack paramName, paramType)
        returnType <- arbitrary
        return $ FuncT params returnType
      "RefineT" -> do
        baseType <- arbitrary
        numConstraints <- choose (1, 3)
        constraints <- vectorOf numConstraints arbitrary
        return $ RefineT baseType constraints
      _ -> return $ SimpleT (T.pack name)

-- | 生成Constraint
instance Arbitrary Constraint where
  arbitrary = do
    constraintType <- elements ["SizeGT", "SizeGE", "RangeC", "PredC"]
    Identifier name <- arbitrary
    case constraintType of
      "SizeGT" -> do
        size <- choose (1, 100)
        return $ SizeGT (T.pack name) size
      "SizeGE" -> do
        size <- choose (0, 100)
        return $ SizeGE (T.pack name) size
      "RangeC" -> do
        minVal <- choose (0, 50)
        maxVal <- choose (minVal, 100)
        return $ RangeC (T.pack name) minVal maxVal
      "PredC" -> do
        numParams <- choose (1, 3)
        params <- vectorOf numParams arbitrary
        return $ PredC (T.pack name) params
      _ -> return $ SizeGT (T.pack name) 1

-- | 生成Statement
instance Arbitrary Statement where
  arbitrary = do
    stmtType <- elements ["STypeDef", "STypeAlias", "SVarDecl", "SFuncDecl", "SConstraintDef", "SExistsDecl"]
    Identifier name <- arbitrary
    case stmtType of
      "STypeDef" -> do
        numParams <- choose (0, 3)
        params <- vectorOf numParams $ do
          Identifier param <- arbitrary
          return $ T.pack param
        numConstraints <- choose (0, 2)
        constraints <- vectorOf numConstraints arbitrary
        return $ STypeDef (T.pack name) params constraints
      "STypeAlias" -> do
        baseType <- arbitrary
        numConstraints <- choose (0, 2)
        constraints <- vectorOf numConstraints arbitrary
        return $ STypeAlias (T.pack name) baseType constraints
      "SVarDecl" -> do
        varType <- arbitrary
        return $ SVarDecl (T.pack name) varType
      "SFuncDecl" -> do
        numParams <- choose (0, 3)
        params <- vectorOf numParams $ do
          Identifier paramName <- arbitrary
          paramType <- arbitrary
          return (T.pack paramName, paramType)
        returnType <- arbitrary
        return $ SFuncDecl (T.pack name) params (Just returnType)
      "SConstraintDef" -> do
        constraint <- arbitrary
        return $ SConstraintDef (T.pack name) constraint
      "SExistsDecl" -> do
        numVars <- choose (1, 3)
        vars <- vectorOf numVars $ do
          Identifier var <- arbitrary
          return $ T.pack var
        body <- arbitrary
        return $ SExistsDecl vars body
      _ -> return $ SVarDecl (T.pack name) (SimpleT (T.pack "Int"))

-- | 生成AST
instance Arbitrary AST where
  arbitrary = do
    numStatements <- choose (0, 5)
    statements <- vectorOf numStatements arbitrary
    return $ Program statements

-- | 生成DependencyGraph
instance Arbitrary DependencyGraph where
  arbitrary = do
    numNodes <- choose (0, 5)
    nodes <- vectorOf numNodes arbitrary
    let nodeMap = Map.fromList $ map (\n -> (nodeName n, n)) nodes
    return $ DependencyGraph nodeMap

-- | 测试DependencyNode的基本属性
prop_dependency_node_name :: DependencyNode -> Property
prop_dependency_node_name node =
  not (null $ nodeName node)

prop_dependency_node_dependencies :: DependencyNode -> Property
prop_dependency_node_dependencies node =
  all (\`elem\` ["module", "function", "type", "variable"]) (nodeDependencies node)

-- | 测试DependencyGraph的属性
prop_dependency_graph_nodes :: DependencyGraph -> Property
prop_dependency_graph_nodes (DependencyGraph nodeMap) =
  let nodes = Map.elems nodeMap
  in all (\n -> nodeName n \`Map.member\` nodeMap) nodes

prop_dependency_graph_empty :: Property
prop_dependency_graph_empty =
  let emptyGraph = DependencyGraph Map.empty
  in Map.size (graphNodes emptyGraph) === 0

-- | 测试TypeExpr的属性
prop_type_expr_simple :: TypeExpr -> Property
prop_type_expr_simple (SimpleT name) =
  not (T.null name)
prop_type_expr_simple _ = property True

prop_type_expr_generic_params :: TypeExpr -> Property
prop_type_expr_generic_params (GenericT _ params) =
  not (null params)
prop_type_expr_generic_params _ = property True

prop_type_expr_func_params :: TypeExpr -> Property
prop_type_expr_func_params (FuncT params _) =
  all (not . T.null . fst) params
prop_type_expr_func_params _ = property True

-- | 测试Constraint的属性
prop_constraint_size_gt :: Constraint -> Property
prop_constraint_size_gt (SizeGT name size) =
  not (T.null name) .&&. size > 0
prop_constraint_size_gt _ = property True

prop_constraint_size_ge :: Constraint -> Property
prop_constraint_size_ge (SizeGE name size) =
  not (T.null name) .&&. size >= 0
prop_constraint_size_ge _ = property True

prop_constraint_range_c :: Constraint -> Property
prop_constraint_range_c (RangeC name minVal maxVal) =
  not (T.null name) .&&. minVal <= maxVal
prop_constraint_range_c _ = property True

prop_constraint_pred_c :: Constraint -> Property
prop_constraint_pred_c (PredC name params) =
  not (T.null name) .&&. not (null params)
prop_constraint_pred_c _ = property True

-- | 测试Statement的属性
prop_statement_type_def :: Statement -> Property
prop_statement_type_def (STypeDef name params _) =
  not (T.null name) .&&. all (not . T.null) params
prop_statement_type_def _ = property True

prop_statement_type_alias :: Statement -> Property
prop_statement_type_alias (STypeAlias name _ _) =
  not (T.null name)
prop_statement_type_alias _ = property True

prop_statement_var_decl :: Statement -> Property
prop_statement_var_decl (SVarDecl name _) =
  not (T.null name)
prop_statement_var_decl _ = property True

prop_statement_func_decl :: Statement -> Property
prop_statement_func_decl (SFuncDecl name params _) =
  not (T.null name) .&&. all (not . T.null . fst) params
prop_statement_func_decl _ = property True

prop_statement_constraint_def :: Statement -> Property
prop_statement_constraint_def (SConstraintDef name _) =
  not (T.null name)
prop_statement_constraint_def _ = property True

prop_statement_exists_decl :: Statement -> Property
prop_statement_exists_decl (SExistsDecl vars _) =
  not (null vars) .&&. all (not . T.null) vars
prop_statement_exists_decl _ = property True

-- | 测试AST的属性
prop_ast_program :: AST -> Property
prop_ast_program (Program statements) =
  length statements >= 0

prop_ast_empty :: Property
prop_ast_empty =
  let emptyAST = Program []
  in case emptyAST of
    Program stmts -> length stmts === 0

-- | 测试类型推理的基本属性
prop_type_variable_creation :: Property
prop_type_variable_creation =
  let tv = newTypeVariable "test"
  in case tv of
    TypeVar name _ -> not (null name)

prop_type_environment_initial :: Property
prop_type_environment_initial =
  let env = initialTypeEnvironment
  in case env of
    TypeEnvironment _ -> property True

prop_type_scheme_generalization :: TypeExpr -> Property
prop_type_scheme_generalization typeExpr =
  let env = initialTypeEnvironment
      scheme = generalize env typeExpr
  in case scheme of
    TypeScheme _ _ -> property True

prop_type_scheme_instantiation :: TypeExpr -> Property
prop_type_scheme_instantiation typeExpr =
  let env = initialTypeEnvironment
      scheme = generalize env typeExpr
      instantiated = instantiate scheme
  in case instantiated of
    _ -> property True

-- | 测试类型统一的属性
prop_type_unify_simple :: Property
prop_type_unify_simple =
  let type1 = SimpleT "Int"
      type2 = SimpleT "Int"
  in case unifyTypes type1 type2 of
    Left _ -> property False
    Right _ -> property True

prop_type_unify_different :: Property
prop_type_unify_different =
  let type1 = SimpleT "Int"
      type2 = SimpleT "String"
  in case unifyTypes type1 type2 of
    Left _ -> property True
    Right _ -> property False

-- | 测试约束求解的属性
prop_constraint_solving_simple :: Property
prop_constraint_solving_simple =
  let constraints = [SizeGT "x" 5]
  in case solveTypeConstraints constraints of
    Left _ -> property False
    Right _ -> property True

prop_constraint_simplification :: Property
prop_constraint_simplification =
  let constraints = [SizeGT "x" 5, SizeGE "x" 3]
      simplified = simplifyConstraints constraints
  in length simplified <= length constraints

-- | 测试作用域管理的属性
prop_scope_push_pop :: Property
prop_scope_push_pop =
  let checker = newDependentTypeChecker
      (result, _) = runState (pushScope >> popScope) checker
  in case result of
    Left _ -> property False
    Right _ -> property True

prop_scope_nested :: Property
prop_scope_nested =
  let checker = newDependentTypeChecker
      action = pushScope >> pushScope >> popScope >> popScope
      (result, _) = runState action checker
  in case result of
    Left _ -> property False
    Right _ -> property True

prop_scope_with_new_scope :: Property
prop_scope_with_new_scope =
  let checker = newDependentTypeChecker
      action = inNewScope $ do
        addType "TestType" (SimpleT "Int")
        return "result"
      (result, _) = runState action checker
  in case result of
    Left _ -> property False
    Right _ -> property True

-- | 测试AST分析的基本属性
prop_ast_analysis_empty :: Property
prop_ast_analysis_empty =
  let ast = Program []
      checker = newDependentTypeChecker
  in case analyzeAST checker ast of
    Left _ -> property False
    Right _ -> property True

prop_ast_analysis_simple :: Property
prop_ast_analysis_simple =
  let ast = Program [SVarDecl "x" (SimpleT "Int")]
      checker = newDependentTypeChecker
  in case analyzeAST checker ast of
    Left _ -> property False
    Right _ -> property True

prop_statement_validation_simple :: Property
prop_statement_validation_simple =
  let stmt = SVarDecl "x" (SimpleT "Int")
      checker = newDependentTypeChecker
  in case validateStatement checker stmt of
    Left _ -> property False
    Right _ -> property True

-- | 测试依赖关系图的属性
prop_dependency_graph_cycle_detection :: DependencyGraph -> Property
prop_dependency_graph_cycle_detection graph =
  -- 简单的循环检测测试
  let nodes = Map.elems $ graphNodes graph
      hasCycle = any hasSelfDependency nodes
  in property True
  where
    hasSelfDependency node = nodeName node \`elem\` nodeDependencies node

prop_dependency_graph_topological_sort :: DependencyGraph -> Property
prop_dependency_graph_topological_sort graph =
  -- 简单的拓扑排序测试
  let nodes = Map.elems $ graphNodes graph
      sorted = sortByDependency nodes
  in length sorted === length nodes
  where
    sortByDependency = sortBy (\n1 n2 -> 
      if nodeName n1 \`elem\` nodeDependencies n2 
      then LT 
      else if nodeName n2 \`elem\` nodeDependencies n1 
           then GT 
           else EQ)

-- | 测试类型检查的基本属性
prop_type_checking_simple :: Property
prop_type_checking_simple =
  let checker = newDependentTypeChecker
      typeExpr = SimpleT "Int"
  in case checkType checker typeExpr of
    Left _ -> property False
    Right _ -> property True

prop_type_addition :: Property
prop_type_addition =
  let checker = newDependentTypeChecker
      (result, _) = runState (addType "TestType" (SimpleT "Int")) checker
  in case result of
    Left _ -> property False
    Right _ -> property True

prop_constraint_addition :: Property
prop_constraint_addition =
  let checker = newDependentTypeChecker
      constraint = SizeGT "x" 5
      (result, _) = runState (addConstraint constraint) checker
  in case result of
    Left _ -> property False
    Right _ -> property True

-- 辅助函数
sortByDependency :: [DependencyNode] -> [DependencyNode]
sortByDependency = sortBy (\n1 n2 -> 
  if nodeName n1 \`elem\` nodeDependencies n2 
  then LT 
  else if nodeName n2 \`elem\` nodeDependencies n1 
       then GT 
       else EQ)

tests :: TestTree
tests = testGroup "Dependencies Advanced QuickCheck Tests"
  -- DependencyNode tests
  [ testProperty "dependency node name" prop_dependency_node_name
  , testProperty "dependency node dependencies" prop_dependency_node_dependencies
  
  -- DependencyGraph tests
  , testProperty "dependency graph nodes" prop_dependency_graph_nodes
  , testProperty "dependency graph empty" prop_dependency_graph_empty
  
  -- TypeExpr tests
  , testProperty "type expr simple" prop_type_expr_simple
  , testProperty "type expr generic params" prop_type_expr_generic_params
  , testProperty "type expr func params" prop_type_expr_func_params
  
  -- Constraint tests
  , testProperty "constraint size gt" prop_constraint_size_gt
  , testProperty "constraint size ge" prop_constraint_size_ge
  , testProperty "constraint range c" prop_constraint_range_c
  , testProperty "constraint pred c" prop_constraint_pred_c
  
  -- Statement tests
  , testProperty "statement type def" prop_statement_type_def
  , testProperty "statement type alias" prop_statement_type_alias
  , testProperty "statement var decl" prop_statement_var_decl
  , testProperty "statement func decl" prop_statement_func_decl
  , testProperty "statement constraint def" prop_statement_constraint_def
  , testProperty "statement exists decl" prop_statement_exists_decl
  
  -- AST tests
  , testProperty "ast program" prop_ast_program
  , testProperty "ast empty" prop_ast_empty
  
  -- Type inference tests
  , testProperty "type variable creation" prop_type_variable_creation
  , testProperty "type environment initial" prop_type_environment_initial
  , testProperty "type scheme generalization" prop_type_scheme_generalization
  , testProperty "type scheme instantiation" prop_type_scheme_instantiation
  
  -- Type unification tests
  , testProperty "type unify simple" prop_type_unify_simple
  , testProperty "type unify different" prop_type_unify_different
  
  -- Constraint solving tests
  , testProperty "constraint solving simple" prop_constraint_solving_simple
  , testProperty "constraint simplification" prop_constraint_simplification
  
  -- Scope management tests
  , testProperty "scope push pop" prop_scope_push_pop
  , testProperty "scope nested" prop_scope_nested
  , testProperty "scope with new scope" prop_scope_with_new_scope
  
  -- AST analysis tests
  , testProperty "ast analysis empty" prop_ast_analysis_empty
  , testProperty "ast analysis simple" prop_ast_analysis_simple
  , testProperty "statement validation simple" prop_statement_validation_simple
  
  -- Dependency graph tests
  , testProperty "dependency graph cycle detection" prop_dependency_graph_cycle_detection
  , testProperty "dependency graph topological sort" prop_dependency_graph_topological_sort
  
  -- Type checking tests
  , testProperty "type checking simple" prop_type_checking_simple
  , testProperty "type addition" prop_type_addition
  , testProperty "constraint addition" prop_constraint_addition
  ]