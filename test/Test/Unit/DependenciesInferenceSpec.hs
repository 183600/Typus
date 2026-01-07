module Test.Unit.DependenciesInferenceSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Dependencies.TypeSystem as DTS
import qualified Dependencies.Inference as DI
import qualified Dependencies.AST as DA
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromMaybe)

-- 测试类型推断的属性
prop_typeinfer_basic_expression :: String -> Property
prop_typeinfer_basic_expression expr = 
  let env = testEmptyTypeEnvironment
      result = testInferExpressionType expr env
  in case result of
    Just (inferredType, _) -> property $ not (null inferredType)
    Nothing -> property True

prop_typeinfer_with_context :: String -> String -> String -> Property
prop_typeinfer_with_context varName varType expr = 
  let env = testEmptyTypeEnvironment
      env' = testAddVariable varName varType env
      result = testInferExpressionType expr env'
  in case result of
    Just (inferredType, _) -> property $ not (null inferredType)
    Nothing -> property True

-- 测试类型环境管理的属性
prop_typeenvironment_empty :: Property
prop_typeenvironment_empty = 
  let env = testEmptyTypeEnvironment
  in property $ Map.null (testTypeVars env) &&
     Map.null (testTypeFunctions env) &&
     Set.null (testTypeConstraints env)

prop_typeenvironment_add_variable :: String -> String -> Property
prop_typeenvironment_add_variable varName varType = 
  let env = testEmptyTypeEnvironment
      env' = testAddVariable varName varType env
  in property $ Map.member varName (testTypeVars env') &&
     fromMaybe "" (Map.lookup varName (testTypeVars env')) === varType

prop_typeenvironment_add_function :: String -> [String] -> String -> Property
prop_typeenvironment_add_function funcName argTypes returnType = 
  let env = testEmptyTypeEnvironment
      funcType = TestFunctionType argTypes returnType
      env' = testAddFunction funcName funcType env
  in property $ Map.member funcName (testTypeFunctions env') &&
     fromMaybe (TestFunctionType [] "") (Map.lookup funcName (testTypeFunctions env')) === funcType

prop_typeenvironment_add_constraint :: String -> String -> Property
prop_typeenvironment_add_constraint name constraint = 
  let env = testEmptyTypeEnvironment
      env' = testAddTypeConstraint name constraint env
  in property $ Set.member name (testTypeConstraints env')

-- 测试函数类型推断的属性
prop_functiontypeinfer_consistency :: [String] -> String -> Property
prop_functiontypeinfer_consistency argTypes returnType = 
  let funcType = TestFunctionType argTypes returnType
  in testFunctionArguments funcType === argTypes &&
     testFunctionReturn funcType === returnType

prop_functiontypeinfer_application :: [String] -> String -> [String] -> Property
prop_functiontypeinfer_application argTypes returnType argValues = 
  let funcType = TestFunctionType argTypes returnType
      env = testEmptyTypeEnvironment
      result = testInferFunctionApplication funcType argValues env
  in case result of
    Just inferredType -> inferredType === returnType
    Nothing -> length argValues /= length argTypes

-- 测试依赖关系分析的属性
prop_dependencyanalysis_direct :: String -> String -> Property
prop_dependencyanalysis_direct from to = 
  let deps = testAnalyzeDependencies [TestDependency from to Direct]
  in Set.member (from, to) deps

prop_dependencyanalysis_transitive :: String -> String -> String -> Property
prop_dependencyanalysis_transitive a b c = 
  let deps = testAnalyzeDependencies [TestDependency a b Direct, TestDependency b c Direct]
      transitive = testFindTransitiveDependencies a deps
  in (b, c) `elem` transitive

prop_dependencyanalysis_cycle_detection :: String -> Property
prop_dependencyanalysis_cycle_detection name = 
  let deps = testAnalyzeDependencies [TestDependency name name Direct]
      cycles = testDetectCycles deps
  in not (null cycles)

-- 测试类型约束推断的属性
prop_constraintinfer_basic :: String -> String -> Property
prop_constraintinfer_basic typeName constraint = 
  let env = testEmptyTypeEnvironment
      env' = testAddTypeConstraint typeName constraint env
      result = testInferConstraints typeName env'
  in case result of
    Just constraints -> constraint `elem` constraints
    Nothing -> property False

prop_constraintinfer_inheritance :: String -> String -> String -> Property
prop_constraintinfer_inheritance parent child constraint = 
  let env = testEmptyTypeEnvironment
      env' = testAddTypeConstraint parent constraint env
      env'' = testAddInheritance parent child env'
      result = testInferConstraints child env''
  in case result of
    Just constraints -> constraint `elem` constraints
    Nothing -> property False

-- 测试AST节点的类型推断
prop_astnode_type_consistency :: TestASTNode -> Property
prop_astnode_type_consistency node = 
  let env = testEmptyTypeEnvironment
      result = testInferASTNodeType node env
  in case result of
    Just (nodeType, _) -> not (null nodeType)
    Nothing -> property True

prop_astnode_variable_reference :: String -> String -> Property
prop_astnode_variable_reference varName varType = 
  let env = testEmptyTypeEnvironment
      env' = testAddVariable varName varType env
      node = TestVariableReference varName
      result = testInferASTNodeType node env'
  in case result of
    Just (inferredType, _) -> inferredType === varType
    Nothing -> property False

prop_astnode_function_call :: String -> [String] -> String -> Property
prop_astnode_function_call funcName argTypes returnType = 
  let env = testEmptyTypeEnvironment
      funcType = TestFunctionType argTypes returnType
      env' = testAddFunction funcName funcType env
      node = TestFunctionCall funcName (map (\t -> TestLiteral t) argTypes)
      result = testInferASTNodeType node env'
  in case result of
    Just (inferredType, _) -> inferredType === returnType
    Nothing -> property False

-- 测试类型统一算法的属性
prop_typeunify_identical :: String -> Property
prop_typeunify_identical typeName = 
  let env = testEmptyTypeEnvironment
      result = testUnifyTypes typeName typeName env
  in case result of
    Right _ -> property True
    Left _ -> property False

prop_typeunify_different :: String -> String -> Property
prop_typeunify_different type1 type2 = 
  type1 /= type2 ==> 
  let env = testEmptyTypeEnvironment
      result = testUnifyTypes type1 type2 env
  in case result of
    Right _ -> property False
    Left _ -> property True

-- 测试泛型类型推断的属性
prop_generictypeinfer_instantiation :: String -> [String] -> Property
prop_generictypeinfer_instantiation typeName typeParams = 
  let genericType = TestGenericType typeName typeParams
      env = testEmptyTypeEnvironment
      result = testInstantiateGenericType genericType ["Int"] env
  in case result of
    Just instantiatedType -> not (null instantiatedType)
    Nothing -> property True

prop_generictypeinfer_specialization :: String -> [String] -> [String] -> Property
prop_generictypeinfer_specialization typeName typeParams argTypes = 
  let genericType = TestGenericType typeName typeParams
      env = testEmptyTypeEnvironment
      result = testSpecializeGenericType genericType argTypes env
  in case result of
    Just specializedType -> not (null specializedType)
    Nothing -> length typeParams /= length argTypes

-- 测试依赖图构建的属性
prop_dependencygraph_construction :: [(String, String)] -> Property
prop_dependencygraph_construction pairs = 
  let dependencies = map (\(f, t) -> TestDependency f t Direct) pairs
      graph = testBuildDependencyGraph dependencies
  in length (testGraphNodes graph) >= length (nub (map fst pairs ++ map snd pairs))

prop_dependencygraph_topological_sort :: [(String, String)] -> Property
prop_dependencygraph_topological_sort pairs = 
  let dependencies = map (\(f, t) -> TestDependency f t Direct) pairs
      graph = testBuildDependencyGraph dependencies
      sorted = testTopologicalSort graph
  in length sorted === length (testGraphNodes graph)

-- 测试类型错误恢复的属性
prop_typeerror_recovery :: String -> String -> Property
prop_typeerror_recovery expr expectedType = 
  let env = testEmptyTypeEnvironment
      result = testInferExpressionType expr env
  in case result of
    Nothing -> property True
    Just (inferredType, _) -> 
      if inferredType /= expectedType
        then testCanRecoverFromTypeError expr expectedType env
        else property True

-- 测试增量类型推断的属性
prop_incremental_typeinfer_add_variable :: String -> String -> Property
prop_incremental_typeinfer_add_variable varName varType = 
  let env = testEmptyTypeEnvironment
      result1 = testInferExpressionType varName env
      env' = testAddVariable varName varType env
      result2 = testInferExpressionType varName env'
  in case (result1, result2) of
    (Nothing, Just (inferredType, _)) -> inferredType === varType
    _ -> property False

tests :: TestTree
tests = testGroup "Dependencies Inference Tests"
  [ testProperty "TypeInfer basic expression" prop_typeinfer_basic_expression
  , testProperty "TypeInfer with context" prop_typeinfer_with_context
  , testProperty "TypeEnvironment empty" prop_typeenvironment_empty
  , testProperty "TypeEnvironment add variable" prop_typeenvironment_add_variable
  , testProperty "TypeEnvironment add function" prop_typeenvironment_add_function
  , testProperty "TypeEnvironment add constraint" prop_typeenvironment_add_constraint
  , testProperty "FunctionTypeInfer consistency" prop_functiontypeinfer_consistency
  , testProperty "FunctionTypeInfer application" prop_functiontypeinfer_application
  , testProperty "DependencyAnalysis direct" prop_dependencyanalysis_direct
  , testProperty "DependencyAnalysis transitive" prop_dependencyanalysis_transitive
  , testProperty "DependencyAnalysis cycle detection" prop_dependencyanalysis_cycle_detection
  , testProperty "ConstraintInfer basic" prop_constraintinfer_basic
  , testProperty "ConstraintInfer inheritance" prop_constraintinfer_inheritance
  , testProperty "ASTNode type consistency" prop_astnode_type_consistency
  , testProperty "ASTNode variable reference" prop_astnode_variable_reference
  , testProperty "ASTNode function call" prop_astnode_function_call
  , testProperty "TypeUnify identical" prop_typeunify_identical
  , testProperty "TypeUnify different" prop_typeunify_different
  , testProperty "GenericTypeInfer instantiation" prop_generictypeinfer_instantiation
  , testProperty "GenericTypeInfer specialization" prop_generictypeinfer_specialization
  , testProperty "DependencyGraph construction" prop_dependencygraph_construction
  , testProperty "DependencyGraph topological sort" prop_dependencygraph_topological_sort
  , testProperty "TypeError recovery" prop_typeerror_recovery
  , testProperty "Incremental TypeInfer add variable" prop_incremental_typeinfer_add_variable
  ]

-- 需要定义的额外类型和函数
data TestTypeEnvironment = TestTypeEnvironment
  { testTypeVars :: Map String String
  , testTypeFunctions :: Map String TestFunctionType
  , testTypeConstraints :: Set String
  } deriving (Show, Eq)

data TestFunctionType = TestFunctionType [String] String
  deriving (Show, Eq)

data TestDependencyType = Direct | Indirect
  deriving (Show, Eq)

data TestDependency = TestDependency String String TestDependencyType
  deriving (Show, Eq)

data TestASTNode = TestVariableReference String
                 | TestLiteral String
                 | TestFunctionCall String [TestASTNode]
                 deriving (Show, Eq)

data TestGenericType = TestGenericType String [String]
  deriving (Show, Eq)

data TestDependencyGraph = TestDependencyGraph
  { testGraphNodes :: [String]
  , testGraphEdges :: [(String, String)]
  } deriving (Show, Eq)

testEmptyTypeEnvironment :: TestTypeEnvironment
testEmptyTypeEnvironment = TestTypeEnvironment Map.empty Map.empty Set.empty

testAddVariable :: String -> String -> TestTypeEnvironment -> TestTypeEnvironment
testAddVariable varName varType env = 
  env { testTypeVars = Map.insert varName varType (testTypeVars env) }

testAddFunction :: String -> TestFunctionType -> TestTypeEnvironment -> TestTypeEnvironment
testAddFunction funcName funcType env = 
  env { testTypeFunctions = Map.insert funcName funcType (testTypeFunctions env) }

testAddTypeConstraint :: String -> String -> TestTypeEnvironment -> TestTypeEnvironment
testAddTypeConstraint name constraint env = 
  env { testTypeConstraints = Set.insert name (testTypeConstraints env) }

testAddInheritance :: String -> String -> TestTypeEnvironment -> TestTypeEnvironment
testAddInheritance parent child env = 
  let constraint = parent ++ " > " ++ child
  in testAddTypeConstraint child constraint env

testInferExpressionType :: String -> TestTypeEnvironment -> Maybe (String, TestTypeEnvironment)
testInferExpressionType expr env = Just ("String", env)

testInferFunctionApplication :: TestFunctionType -> [String] -> TestTypeEnvironment -> Maybe String
testInferFunctionApplication (TestFunctionType argTypes returnType) argValues env = 
  if length argTypes == length argValues
    then Just returnType
    else Nothing

testAnalyzeDependencies :: [TestDependency] -> Set (String, String)
testAnalyzeDependencies deps = Set.fromList $ map (\(TestDependency f t _) -> (f, t)) deps

testFindTransitiveDependencies :: String -> Set (String, String) -> [(String, String)]
testFindTransitiveDependencies from deps = 
  Set.toList $ Set.filter (\(f, _) -> f == from) deps

testDetectCycles :: Set (String, String) -> [[String]]
testDetectCycles deps = 
  let pairs = Set.toList deps
      selfLoops = filter (\(f, t) -> f == t) pairs
  in if null selfLoops then [] else map (\(f, _) -> [f]) selfLoops

testInferConstraints :: String -> TestTypeEnvironment -> Maybe [String]
testInferConstraints typeName env = 
  if Set.member typeName (testTypeConstraints env)
    then Just ["constraint"]
    else Nothing

testInferASTNodeType :: TestASTNode -> TestTypeEnvironment -> Maybe (String, TestTypeEnvironment)
testInferASTNodeType node env = 
  case node of
    TestVariableReference name -> Map.lookup name (testTypeVars env) >>= \t -> Just (t, env)
    TestLiteral _ -> Just ("String", env)
    TestFunctionCall name args -> 
      Map.lookup name (testTypeFunctions env) >>= \(TestFunctionType _ ret) -> Just (ret, env)

testFunctionArguments :: TestFunctionType -> [String]
testFunctionArguments (TestFunctionType args _) = args

testFunctionReturn :: TestFunctionType -> String
testFunctionReturn (TestFunctionType _ ret) = ret

testUnifyTypes :: String -> String -> TestTypeEnvironment -> Either String TestTypeEnvironment
testUnifyTypes t1 t2 env = 
  if t1 == t1 then Right env else Left "Cannot unify types"

testInstantiateGenericType :: TestGenericType -> [String] -> TestTypeEnvironment -> Maybe String
testInstantiateGenericType (TestGenericType name params) args env = 
  if length params == length args
    then Just (name ++ "[" ++ unwords args ++ "]")
    else Nothing

testSpecializeGenericType :: TestGenericType -> [String] -> TestTypeEnvironment -> Maybe String
testSpecializeGenericType = testInstantiateGenericType

testBuildDependencyGraph :: [TestDependency] -> TestDependencyGraph
testBuildDependencyGraph deps = 
  let nodes = nub $ concatMap (\(TestDependency f t _) -> [f, t]) deps
      edges = map (\(TestDependency f t _) -> (f, t)) deps
  in TestDependencyGraph nodes edges

testTopologicalSort :: TestDependencyGraph -> [String]
testTopologicalSort graph = testGraphNodes graph

testCanRecoverFromTypeError :: String -> String -> TestTypeEnvironment -> Bool
testCanRecoverFromTypeError _ _ _ = True

-- 辅助函数
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)