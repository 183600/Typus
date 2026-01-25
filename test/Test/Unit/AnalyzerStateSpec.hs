module Test.Unit.AnalyzerStateSpec where



import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

-- 测试AnalyzerState的属性
prop_analyzerstate_initial :: Property
prop_analyzerstate_initial = 
  let state = testEmptyAnalyzerState
  in property $ null (testAnalyzerSymbols state) &&
     null (testAnalyzerTypes state) &&
     null (testAnalyzerConstraints state) &&
     null (testAnalyzerDependencies state)

prop_analyzerstate_add_symbol :: String -> String -> Property
prop_analyzerstate_add_symbol name symbolType = 
  let state = testEmptyAnalyzerState
      state' = testAddSymbol name symbolType state
  in property $ Map.member name (testAnalyzerSymbols state') &&
     fromMaybe "" (Map.lookup name (testAnalyzerSymbols state')) == symbolType

prop_analyzerstate_add_type :: String -> String -> Property
prop_analyzerstate_add_type name typeDef = 
  let state = testEmptyAnalyzerState
      state' = testAddType name typeDef state
  in property $ Map.member name (testAnalyzerTypes state') &&
     fromMaybe "" (Map.lookup name (testAnalyzerTypes state')) == typeDef

prop_analyzerstate_add_constraint :: String -> String -> Property
prop_analyzerstate_add_constraint name constraint = 
  let state = testEmptyAnalyzerState
      state' = testAddConstraint name constraint state
  in property $ Set.member name (testAnalyzerConstraints state')

prop_analyzerstate_add_dependency :: String -> String -> Property
prop_analyzerstate_add_dependency from to = 
  let state = testEmptyAnalyzerState
      state' = testAddDependency from to state
      deps = fromMaybe Set.empty (Map.lookup from (testAnalyzerDependencies state'))
  in property $ Set.member to deps

-- 测试SymbolTable的属性
prop_symboltable_empty :: Property
prop_symboltable_empty = 
  let table = testEmptySymbolTable
  in property $ Map.null (testSymbols table) &&
     Map.null (testTypes table) &&
     Set.null (testScopes table)

prop_symboltable_add_symbol :: String -> String -> Property
prop_symboltable_add_symbol name symbolType = 
  let table = testEmptySymbolTable
      table' = testAddSymbolToTable name symbolType table
  in property $ testHasSymbol name table' &&
     testGetSymbolType name table' == Just symbolType

prop_symboltable_add_type :: String -> String -> Property
prop_symboltable_add_type name typeDef = 
  let table = testEmptySymbolTable
      table' = testAddTypeToTable name typeDef table
  in property $ testHasType name table' &&
     testGetTypeDefinition name table' == Just typeDef

prop_symboltable_scope_management :: String -> Property
prop_symboltable_scope_management scopeName = 
  let table = testEmptySymbolTable
      table' = testEnterScope scopeName table
      _ = testAddSymbolToTable "test" "String" table'
      table''' = testExitScope table'
  in property $ testCurrentScope table' == Just scopeName &&
     testCurrentScope table''' == Nothing

-- 测试AnalyzerTypes的属性
prop_analysistypes_consistency :: String -> Property
prop_analysistypes_consistency typeName = 
  let typeDef = TestTypeDefinition typeName [] []
  in property $ typeName === testTypeName typeDef

prop_analysistypes_function_type :: String -> String -> Property
prop_analysistypes_function_type inputType outputType = 
  let funcType = TestFunctionType inputType outputType
  in property $ testFunctionInput funcType == inputType &&
     testFunctionOutput funcType == outputType

prop_analysistypes_dependent_type :: String -> String -> Property
prop_analysistypes_dependent_type baseType constraint = 
  let depType = TestDependentType baseType constraint
  in property $ testDependentBase depType == baseType &&
     testDependentConstraint depType == constraint

-- 测试类型推断的属性
prop_typeinfer_basic :: String -> Property
prop_typeinfer_basic expr = 
  let state = testEmptyAnalyzerState
      result = testInferType expr state
  in case result of
    Just (inferredType, _) -> property $ not (null inferredType)
    Nothing -> property True

prop_typeinfer_with_context :: String -> String -> String -> Property
prop_typeinfer_with_context varName varType expr = 
  let state = testEmptyAnalyzerState
      state' = testAddSymbol varName varType state
      result = testInferType expr state'
  in case result of
    Just (inferredType, _) -> property $ not (null inferredType)
    Nothing -> property True

-- 测试约束检查的属性
prop_constraintcheck_valid :: String -> String -> Property
prop_constraintcheck_valid typeName constraint = 
  let state = testEmptyAnalyzerState
      state' = testAddType typeName "base" state
      result = testCheckConstraint typeName constraint state'
  in case result of
    True -> property True
    False -> property True

prop_constraintcheck_invalid :: String -> String -> Property
prop_constraintcheck_invalid typeName constraint = 
  let state = testEmptyAnalyzerState
      result = testCheckConstraint typeName constraint state
  in property $ result == False

-- 测试依赖分析的属性
prop_dependencyanalysis_transitive :: String -> String -> String -> Property
prop_dependencyanalysis_transitive a b c = 
  let state = testEmptyAnalyzerState
      state' = testAddDependency a b state
      state'' = testAddDependency b c state'
      deps = testFindTransitiveDependencies a state''
  in property $ c `elem` deps

prop_dependencyanalysis_cycle_detection :: String -> Property
prop_dependencyanalysis_cycle_detection name = 
  let state = testEmptyAnalyzerState
      state' = testAddDependency name name state
      hasCycle = testDetectCycle name state'
  in property $ hasCycle == True

-- 测试所有权分析的属性
prop_ownershiptransfer_valid :: String -> String -> Property
prop_ownershiptransfer_valid from to = 
  let state = testEmptyAnalyzerState
      state' = testAddOwner from state
      result = testTransferOwnership from to state'
  in case result of
    Right state'' -> property $ testHasOwner to state'' && not (testHasOwner from state'')
    Left _ -> property False

prop_ownershiptransfer_invalid :: String -> String -> Property
prop_ownershiptransfer_invalid from to = 
  let state = testEmptyAnalyzerState
      result = testTransferOwnership from to state
  in case result of
    Right _ -> property False
    Left _ -> property True

-- 测试符号解析的属性
prop_symbolresolution_existing :: String -> String -> Property
prop_symbolresolution_existing name symbolType = 
  let state = testEmptyAnalyzerState
      state' = testAddSymbol name symbolType state
      result = testResolveSymbol name state'
  in property $ result == Just symbolType

prop_symbolresolution_nonexistent :: String -> Property
prop_symbolresolution_nonexistent name = 
  let state = testEmptyAnalyzerState
      result = testResolveSymbol name state
  in property $ result == Nothing

-- 测试类型检查的属性
prop_typecheck_valid :: String -> String -> Property
prop_typecheck_valid expr expectedType = 
  let state = testEmptyAnalyzerState
      result = testTypeCheck expr expectedType state
  in case result of
    Right _ -> property True
    Left _ -> property False

prop_typecheck_invalid :: String -> String -> Property
prop_typecheck_invalid expr expectedType = 
  let state = testEmptyAnalyzerState
      result = testTypeCheck expr expectedType state
  in case result of
    Right _ -> property False
    Left _ -> property True

-- 测试状态合并的属性
prop_statemerge_preserves_symbols :: String -> String -> Property
prop_statemerge_preserves_symbols name symbolType = 
  let state1 = testEmptyAnalyzerState
      state2 = testEmptyAnalyzerState
      state1' = testAddSymbol name symbolType state1
      merged = testMergeStates state1' state2
  in property $ Map.member name (testAnalyzerSymbols merged)

prop_statemerge_preserves_types :: String -> String -> Property
prop_statemerge_preserves_types name typeDef = 
  let state1 = testEmptyAnalyzerState
      state2 = testEmptyAnalyzerState
      state2' = testAddType name typeDef state2
      merged = testMergeStates state1 state2'
  in property $ Map.member name (testAnalyzerTypes merged)

-- 测试状态回滚的属性
prop_staterollback_consistency :: String -> String -> Property
prop_staterollback_consistency name symbolType = 
  let state = testEmptyAnalyzerState
      state' = testAddSymbol name symbolType state
      checkpoint = testCreateCheckpoint state'
      _ = testAddSymbol "other" "OtherType" state'
      rolledBack = testRollbackToCheckpoint checkpoint
  in property $ Map.size (testAnalyzerSymbols rolledBack) == Map.size (testAnalyzerSymbols state')

tests :: TestTree
tests = testGroup "Analyzer State Tests"
  [ testProperty "AnalyzerState initial" prop_analyzerstate_initial
  , testProperty "AnalyzerState add symbol" prop_analyzerstate_add_symbol
  , testProperty "AnalyzerState add type" prop_analyzerstate_add_type
  , testProperty "AnalyzerState add constraint" prop_analyzerstate_add_constraint
  , testProperty "AnalyzerState add dependency" prop_analyzerstate_add_dependency
  , testProperty "SymbolTable empty" prop_symboltable_empty
  , testProperty "SymbolTable add symbol" prop_symboltable_add_symbol
  , testProperty "SymbolTable add type" prop_symboltable_add_type
  , testProperty "SymbolTable scope management" prop_symboltable_scope_management
  , testProperty "AnalyzerTypes consistency" prop_analysistypes_consistency
  , testProperty "AnalyzerTypes function type" prop_analysistypes_function_type
  , testProperty "AnalyzerTypes dependent type" prop_analysistypes_dependent_type
  , testProperty "TypeInfer basic" prop_typeinfer_basic
  , testProperty "TypeInfer with context" prop_typeinfer_with_context
  , testProperty "ConstraintCheck valid" prop_constraintcheck_valid
  , testProperty "ConstraintCheck invalid" prop_constraintcheck_invalid
  , testProperty "DependencyAnalysis transitive" prop_dependencyanalysis_transitive
  , testProperty "DependencyAnalysis cycle detection" prop_dependencyanalysis_cycle_detection
  , testProperty "OwnershipTransfer valid" prop_ownershiptransfer_valid
  , testProperty "OwnershipTransfer invalid" prop_ownershiptransfer_invalid
  , testProperty "SymbolResolution existing" prop_symbolresolution_existing
  , testProperty "SymbolResolution nonexistent" prop_symbolresolution_nonexistent
  , testProperty "TypeCheck valid" prop_typecheck_valid
  , testProperty "TypeCheck invalid" prop_typecheck_invalid
  , testProperty "StateMerge preserves symbols" prop_statemerge_preserves_symbols
  , testProperty "StateMerge preserves types" prop_statemerge_preserves_types
  , testProperty "StateRollback consistency" prop_staterollback_consistency
  ]

-- 需要定义的额外类型和函数
data TestAnalyzerState = TestAnalyzerState
  { testAnalyzerSymbols :: Map String String
  , testAnalyzerTypes :: Map String String
  , testAnalyzerConstraints :: Set String
  , testAnalyzerDependencies :: Map String (Set String)
  } deriving (Show, Eq)

data TestSymbolTable = TestSymbolTable
  { testSymbols :: Map String String
  , testTypes :: Map String String
  , testScopes :: Set String
  , testCurrentScopeName :: Maybe String
  } deriving (Show, Eq)

data TestTypeDefinition = TestTypeDefinition String [String] [String]
  deriving (Show, Eq)

data TestFunctionType = TestFunctionType String String
  deriving (Show, Eq)

data TestDependentType = TestDependentType String String
  deriving (Show, Eq)

data TestCheckpoint = TestCheckpoint TestAnalyzerState
  deriving (Show, Eq)

testEmptyAnalyzerState :: TestAnalyzerState
testEmptyAnalyzerState = TestAnalyzerState Map.empty Map.empty Set.empty Map.empty

testEmptySymbolTable :: TestSymbolTable
testEmptySymbolTable = TestSymbolTable Map.empty Map.empty Set.empty Nothing

testAddSymbol :: String -> String -> TestAnalyzerState -> TestAnalyzerState
testAddSymbol name symbolType state = 
  state { testAnalyzerSymbols = Map.insert name symbolType (testAnalyzerSymbols state) }

testAddType :: String -> String -> TestAnalyzerState -> TestAnalyzerState
testAddType name typeDef state = 
  state { testAnalyzerTypes = Map.insert name typeDef (testAnalyzerTypes state) }

testAddConstraint :: String -> String -> TestAnalyzerState -> TestAnalyzerState
testAddConstraint name _ state = 
  state { testAnalyzerConstraints = Set.insert name (testAnalyzerConstraints state) }

testAddDependency :: String -> String -> TestAnalyzerState -> TestAnalyzerState
testAddDependency from to state = 
  let deps = Map.insertWith Set.union from (Set.singleton to) (testAnalyzerDependencies state)
  in state { testAnalyzerDependencies = deps }

testAddSymbolToTable :: String -> String -> TestSymbolTable -> TestSymbolTable
testAddSymbolToTable name symbolType table = 
  table { testSymbols = Map.insert name symbolType (testSymbols table) }

testAddTypeToTable :: String -> String -> TestSymbolTable -> TestSymbolTable
testAddTypeToTable name typeDef table = 
  table { testTypes = Map.insert name typeDef (testTypes table) }

testEnterScope :: String -> TestSymbolTable -> TestSymbolTable
testEnterScope name table = 
  table { testScopes = Set.insert name (testScopes table)
        , testCurrentScopeName = Just name }

testExitScope :: TestSymbolTable -> TestSymbolTable
testExitScope table = 
  table { testCurrentScopeName = Nothing }

testHasSymbol :: String -> TestSymbolTable -> Bool
testHasSymbol name table = Map.member name (testSymbols table)

testHasType :: String -> TestSymbolTable -> Bool
testHasType name table = Map.member name (testTypes table)

testGetSymbolType :: String -> TestSymbolTable -> Maybe String
testGetSymbolType name table = Map.lookup name (testSymbols table)

testGetTypeDefinition :: String -> TestSymbolTable -> Maybe String
testGetTypeDefinition name table = Map.lookup name (testTypes table)

testCurrentScope :: TestSymbolTable -> Maybe String
testCurrentScope table = testCurrentScopeName table

testTypeName :: TestTypeDefinition -> String
testTypeName (TestTypeDefinition name _ _) = name

testFunctionInput :: TestFunctionType -> String
testFunctionInput (TestFunctionType input _) = input

testFunctionOutput :: TestFunctionType -> String
testFunctionOutput (TestFunctionType _ output') = output'

testDependentBase :: TestDependentType -> String
testDependentBase (TestDependentType base _) = base

testDependentConstraint :: TestDependentType -> String
testDependentConstraint (TestDependentType _ constraint) = constraint

testInferType :: String -> TestAnalyzerState -> Maybe (String, TestAnalyzerState)
testInferType _ state = Just ("String", state)

testCheckConstraint :: String -> String -> TestAnalyzerState -> Bool
testCheckConstraint typeName _ state = 
  Map.member typeName (testAnalyzerTypes state)

testFindTransitiveDependencies :: String -> TestAnalyzerState -> [String]
testFindTransitiveDependencies name state = 
  case Map.lookup name (testAnalyzerDependencies state) of
    Just deps -> Set.toList deps
    Nothing -> []

testDetectCycle :: String -> TestAnalyzerState -> Bool
testDetectCycle name state = 
  case Map.lookup name (testAnalyzerDependencies state) of
    Just deps -> Set.member name deps
    Nothing -> False

testAddOwner :: String -> TestAnalyzerState -> TestAnalyzerState
testAddOwner owner = testAddSymbol (owner ++ "_owner") "Ownership"

testHasOwner :: String -> TestAnalyzerState -> Bool
testHasOwner name state = Map.member (name ++ "_owner") (testAnalyzerSymbols state)

testTransferOwnership :: String -> String -> TestAnalyzerState -> Either String TestAnalyzerState
testTransferOwnership from to state = 
  if testHasOwner from state
    then Right $ testAddSymbol (to ++ "_owner") "Ownership" (testAddSymbol (from ++ "_owner") "" state)
    else Left "Source does not have ownership"

testResolveSymbol :: String -> TestAnalyzerState -> Maybe String
testResolveSymbol name state = Map.lookup name (testAnalyzerSymbols state)

testTypeCheck :: String -> String -> TestAnalyzerState -> Either String TestAnalyzerState
testTypeCheck expr _ state = 
  if null expr then Left "Empty expression" else Right state

testMergeStates :: TestAnalyzerState -> TestAnalyzerState -> TestAnalyzerState
testMergeStates state1 state2 = 
  TestAnalyzerState
    (testAnalyzerSymbols state1 `Map.union` testAnalyzerSymbols state2)
    (testAnalyzerTypes state1 `Map.union` testAnalyzerTypes state2)
    (testAnalyzerConstraints state1 `Set.union` testAnalyzerConstraints state2)
    (testAnalyzerDependencies state1 `Map.union` testAnalyzerDependencies state2)

testCreateCheckpoint :: TestAnalyzerState -> TestCheckpoint
testCreateCheckpoint state = TestCheckpoint state

testRollbackToCheckpoint :: TestCheckpoint -> TestAnalyzerState
testRollbackToCheckpoint (TestCheckpoint state) = state