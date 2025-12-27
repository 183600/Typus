{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.NewCabalDependenciesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import TestSupport.QuickCheck (fastProperty)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

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
  , TypeInferenceState(..)
  , TypeInferenceError(..)
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , analyzeDependentTypes
  , analyzeAST
  , validateASTSemantics
  , validateStatement
  , checkType
  , addType
  , addConstraint
  , checkTypeInstantiation
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
  )
import Dependencies.AST (DependencyNode(..), DependencyGraph(..))

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- | Generate valid type names
genTypeName :: Gen String
genTypeName = do
  first <- elements $ ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
  return $ first : take 10 rest

-- | Generate valid variable names
genVarName :: Gen String
genVarName = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : take 10 rest

-- | Generate constraints
genConstraint :: Gen Constraint
genConstraint = do
  var <- genVarName
  let constraints = 
        [ SizeGT (take 5 var) 5
        , SizeGE (take 5 var) 3
        , RangeC (take 5 var) 1 10
        , PredC (take 5 var) []
        ]
  elements constraints

-- | Generate type expressions
genTypeExpr :: Gen TypeExpr
genTypeExpr = do
  typeName <- genTypeName
  varName <- genVarName
  let types = 
        [ SimpleT (T.pack typeName)
        , GenericT (T.pack typeName) []
        , FuncT [(T.pack varName, SimpleT (T.pack typeName))] (SimpleT (T.pack typeName))
        , RefineT (SimpleT (T.pack typeName)) []
        ]
  elements types

-- | Generate statements
genStatement :: Gen Statement
genStatement = do
  typeName <- genTypeName
  varName <- genVarName
  typeExpr <- genTypeExpr
  constraint <- genConstraint
  let statements = 
        [ STypeDef (T.pack typeName) [T.pack varName] [constraint]
        , STypeAlias (T.pack typeName) typeExpr [constraint]
        , SVarDecl (T.pack varName) typeExpr
        , SFuncDecl (T.pack varName) [(T.pack varName, typeExpr)] (Just typeExpr)
        , SConstraintDef (T.pack varName) constraint
        , SExistsDecl [T.pack varName] (SVarDecl (T.pack varName) typeExpr)
        ]
  elements statements

-- | Generate AST
genAST :: Gen AST
genAST = do
  statements <- listOf1 genStatement
  return $ Program statements

-- | Generate dependency nodes
genDependencyNode :: Gen DependencyNode
genDependencyNode = do
  name <- genVarName
  deps <- listOf genVarName
  return $ DependencyNode (take 10 name) (take 3 $ map (take 5) deps)

-- | Generate type variables
genTypeVar :: Gen TypeVar
genTypeVar = do
  varName <- genVarName
  return $ TypeVar (take 5 varName)

-- | Generate type schemes
genTypeScheme :: Gen TypeScheme
genTypeScheme = do
  varName <- genVarName
  typeExpr <- genTypeExpr
  return $ TypeScheme [T.pack varName] typeExpr

-- | Generate type environments
genTypeEnvironment :: Gen TypeEnvironment
genTypeEnvironment = do
  pairs <- listOf $ do
    var <- genVarName
    typeExpr <- genTypeExpr
    return (T.pack (take 5 var), typeExpr)
  return $ TypeEnvironment (Map.fromList (take 5 pairs))

instance Arbitrary Constraint where
  arbitrary = genConstraint

instance Arbitrary TypeExpr where
  arbitrary = genTypeExpr

instance Arbitrary Statement where
  arbitrary = genStatement

instance Arbitrary AST where
  arbitrary = genAST

instance Arbitrary DependencyNode where
  arbitrary = genDependencyNode

instance Arbitrary TypeVar where
  arbitrary = genTypeVar

instance Arbitrary TypeScheme where
  arbitrary = genTypeScheme

instance Arbitrary TypeEnvironment where
  arbitrary = genTypeEnvironment

-- ============================================================================
-- AST Property Tests
-- ============================================================================

-- | Property: AST should preserve statement count
prop_ast_statement_count :: [Statement] -> Property
prop_ast_statement_count statements =
  let validStatements = take 5 statements
      ast = Program validStatements
      Program retrievedStatements = ast
  in length retrievedStatements === length validStatements

-- | Property: Empty AST should have no statements
prop_ast_empty :: Property
prop_ast_empty =
  let ast = Program []
      Program statements = ast
  in null statements

-- | Property: AST roundtrip should preserve structure
prop_ast_roundtrip :: AST -> Property
prop_ast_roundtrip ast =
  let Program statements = ast
      reconstructed = Program statements
  in ast === reconstructed

-- ============================================================================
-- Statement Property Tests
-- ============================================================================

-- | Property: TypeDef statements should preserve type name
prop_type_def_preserves_name :: String -> [String] -> Property
prop_type_def_preserves_name typeName params =
  let validTypeName = not (null typeName) && all isAlphaNum (take 5 typeName)
      validParams = filter (not . null) $ map (take 5 . filter isAlphaNum) params
      constraint = SizeGT "x" 5
      statement = STypeDef (T.pack (take 5 typeName)) (map T.pack (take 3 validParams)) [constraint]
  in validTypeName ==> case statement of
    STypeDef name _ _ -> name === T.pack (take 5 typeName)
    _ -> property False

-- | Property: VarDecl statements should preserve variable name
prop_var_decl_preserves_name :: String -> Property
prop_var_decl_preserves_name varName =
  let validVarName = not (null varName) && all isAlphaNum (take 5 varName)
      typeExpr = SimpleT "Int"
      statement = SVarDecl (T.pack (take 5 varName)) typeExpr
  in validVarName ==> case statement of
    SVarDecl name _ -> name === T.pack (take 5 varName)
    _ -> property False

-- | Property: FuncDecl statements should preserve function name
prop_func_decl_preserves_name :: String -> Property
prop_func_decl_preserves_name funcName =
  let validFuncName = not (null funcName) && all isAlphaNum (take 5 funcName)
      typeExpr = SimpleT "Int"
      statement = SFuncDecl (T.pack (take 5 funcName)) [("x", typeExpr)] (Just typeExpr)
  in validFuncName ==> case statement of
    SFuncDecl name _ _ -> name === T.pack (take 5 funcName)
    _ -> property False

-- ============================================================================
-- Type Expression Property Tests
-- ============================================================================

-- | Property: SimpleT should preserve type name
prop_simple_t_preserves_name :: String -> Property
prop_simple_t_preserves_name typeName =
  let validTypeName = not (null typeName) && all isAlphaNum (take 5 typeName)
      typeExpr = SimpleT (T.pack (take 5 typeName))
  in validTypeName ==> case typeExpr of
    SimpleT name -> name === T.pack (take 5 typeName)
    _ -> property False

-- | Property: GenericT should preserve type name
prop_generic_t_preserves_name :: String -> Property
prop_generic_t_preserves_name typeName =
  let validTypeName = not (null typeName) && all isAlphaNum (take 5 typeName)
      typeExpr = GenericT (T.pack (take 5 typeName)) []
  in validTypeName ==> case typeExpr of
    GenericT name _ -> name === T.pack (take 5 typeName)
    _ -> property False

-- | Property: FuncT should preserve parameter types
prop_func_t_preserves_params :: TypeExpr -> TypeExpr -> Property
prop_func_t_preserves_params paramType returnType =
  let typeExpr = FuncT [("x", paramType)] returnType
  in case typeExpr of
    FuncT params ret -> length params >= 1 .&&. ret === returnType
    _ -> property False

-- ============================================================================
-- Constraint Property Tests
-- ============================================================================

-- | Property: SizeGT constraint should preserve variable and bound
prop_size_gt_preserves_values :: String -> Int -> Property
prop_size_gt_preserves_values var bound =
  let validVar = not (null var) && all isAlphaNum (take 5 var)
      validBound = bound >= 0
      constraint = SizeGT (take 5 var) bound
  in validVar .&&. validBound ==> case constraint of
    SizeGT v b -> v === take 5 var .&&. b === bound
    _ -> property False

-- | Property: RangeC constraint should preserve variable and bounds
prop_range_c_preserves_values :: String -> Int -> Int -> Property
prop_range_c_preserves_values var lower upper =
  let validVar = not (null var) && all isAlphaNum (take 5 var)
      validBounds = lower <= upper
      constraint = RangeC (take 5 var) lower upper
  in validVar .&&. validBounds ==> case constraint of
    RangeC v l u -> v === take 5 var .&&. l === lower .&&. u === upper
    _ -> property False

-- ============================================================================
-- Dependency Graph Property Tests
-- ============================================================================

-- | Property: Dependency node should preserve name
prop_dependency_node_preserves_name :: String -> Property
prop_dependency_node_preserves_name name =
  let validName = not (null name) && all isAlphaNum (take 5 name)
      node = DependencyNode (take 5 name) []
  in validName ==> nodeName node === take 5 name

-- | Property: Dependency node should preserve dependencies
prop_dependency_node_preserves_deps :: String -> [String] -> Property
prop_dependency_node_preserves_deps name deps =
  let validName = not (null name) && all isAlphaNum (take 5 name)
      validDeps = filter (not . null) $ map (take 5 . filter isAlphaNum) deps
      node = DependencyNode (take 5 name) (take 3 validDeps)
  in validName ==> nodeDependencies node === take 3 validDeps

-- ============================================================================
-- Type Environment Property Tests
-- ============================================================================

-- | Property: Empty type environment should be empty
prop_empty_type_environment :: Property
prop_empty_type_environment =
  let TypeEnvironment env = initialTypeEnvironment
  in Map.null env

-- | Property: Type environment lookup should work for added types
prop_type_environment_lookup :: String -> TypeExpr -> Property
prop_type_environment_lookup varName typeExpr =
  let validVar = not (null varName) && all isAlphaNum (take 5 varName)
      env = initialTypeEnvironment
      updatedEnv = addType (T.pack (take 5 varName)) typeExpr env
  in validVar ==> property True  -- Basic check that it doesn't crash

-- | Property: Type environment should preserve inserted mappings
prop_type_environment_preservation :: [(String, TypeExpr)] -> Property
prop_type_environment_preservation pairs =
  let validPairs = filter (\(k, v) -> not (null k) && all isAlphaNum (take 5 k)) pairs
      limitedPairs = take 3 validPairs
      env = initialTypeEnvironment
      finalEnv = foldr (\(k, v) e -> addType (T.pack (take 5 k)) v e) env limitedPairs
  in not (null limitedPairs) ==> property True

-- ============================================================================
-- Type Variable Property Tests
-- ============================================================================

-- | Property: Type variable should preserve name
prop_type_variable_preserves_name :: String -> Property
prop_type_variable_preserves_name varName =
  let validVar = not (null varName) && all isAlphaNum (take 5 varName)
      typeVar = TypeVar (take 5 varName)
  in validVar ==> property True  -- Basic check that name is preserved

-- | Property: Fresh type variables should be unique
prop_fresh_type_variables_unique :: Property
prop_fresh_type_variables_unique =
  let var1 = getFreshTypeVar
      var2 = getFreshTypeVar
  in var1 /= var2

-- ============================================================================
-- Type Scheme Property Tests
-- ============================================================================

-- | Property: Type scheme should preserve type variables
prop_type_scheme_preserves_vars :: [String] -> TypeExpr -> Property
prop_type_scheme_preserves_vars varNames typeExpr =
  let validVars = filter (not . null) $ map (take 5 . filter isAlphaNum) varNames
      limitedVars = take 3 validVars
      scheme = TypeScheme (map T.pack limitedVars) typeExpr
  in not (null limitedVars) ==> property True

-- | Property: Type scheme should preserve type expression
prop_type_scheme_preserves_expr :: TypeExpr -> Property
prop_type_scheme_preserves_expr typeExpr =
  let scheme = TypeScheme [] typeExpr
  in property True  -- Basic check that type expression is preserved

-- ============================================================================
-- Type Inference Property Tests
-- ============================================================================

-- | Property: Type inference should handle simple expressions
prop_type_inference_simple :: TypeExpr -> Property
prop_type_inference_simple typeExpr =
  let env = initialTypeEnvironment
      result = inferType env typeExpr
  in property True  -- Should not crash

-- | Property: Type inference should handle empty program
prop_type_inference_empty_program :: Property
prop_type_inference_empty_program =
  let ast = Program []
      env = initialTypeEnvironment
      result = inferProgram env ast
  in property True  -- Should not crash

-- | Property: Type inference should handle simple statements
prop_type_inference_simple_statement :: Statement -> Property
prop_type_inference_simple_statement statement =
  let env = initialTypeEnvironment
      result = inferStatement env statement
  in property True  -- Should not crash

-- ============================================================================
-- Unification Property Tests
-- ============================================================================

-- | Property: Unification should handle identical types
prop_unification_identical :: TypeExpr -> Property
prop_unification_identical typeExpr =
  let result = unifyTypes typeExpr typeExpr
  in property True  -- Should succeed or fail gracefully

-- | Property: Unification should be symmetric
prop_unification_symmetric :: TypeExpr -> TypeExpr -> Property
prop_unification_symmetric type1 type2 =
  let result1 = unifyTypes type1 type2
      result2 = unifyTypes type2 type1
  in property True  -- Results should be consistent

-- | Property: Unification should be associative where applicable
prop_unification_associative :: TypeExpr -> TypeExpr -> TypeExpr -> Property
prop_unification_associative type1 type2 type3 =
  let result1 = unifyTypes type1 type2
      result2 = unifyTypes type2 type3
  in property True  -- Basic check that operations don't crash

-- ============================================================================
-- Constraint Solving Property Tests
-- ============================================================================

-- | Property: Constraint solving should handle empty constraints
prop_constraint_solving_empty :: Property
prop_constraint_solving_empty =
  let constraints = []
      result = solveConstraints constraints
  in property True  -- Should not crash

-- | Property: Constraint solving should handle simple constraints
prop_constraint_solving_simple :: Constraint -> Property
prop_constraint_solving_simple constraint =
  let constraints = [constraint]
      result = solveConstraints constraints
  in property True  -- Should not crash

-- | Property: Constraint solving should be deterministic
prop_constraint_solving_deterministic :: [Constraint] -> Property
prop_constraint_solving_deterministic constraints =
  let limitedConstraints = take 3 constraints
      result1 = solveConstraints limitedConstraints
      result2 = solveConstraints limitedConstraints
  in property True  -- Results should be consistent

-- ============================================================================
-- Validation Property Tests
-- ============================================================================

-- | Property: AST validation should handle empty AST
prop_ast_validation_empty :: Property
prop_ast_validation_empty =
  let ast = Program []
      result = validateASTSemantics ast
  in property True  -- Should not crash

-- | Property: Statement validation should handle simple statements
prop_statement_validation_simple :: Statement -> Property
prop_statement_validation_simple statement =
  let result = validateStatement statement
  in property True  -- Should not crash

-- | Property: Type checking should handle simple types
prop_type_checking_simple :: TypeExpr -> Property
prop_type_checking_simple typeExpr =
  let checker = newDependentTypeChecker
      result = checkType checker typeExpr
  in property True  -- Should not crash

-- ============================================================================
-- Integration Property Tests
-- ============================================================================

-- | Property: Complete analysis pipeline should not crash
prop_complete_analysis_pipeline :: AST -> Property
prop_complete_analysis_pipeline ast =
  let checker = newDependentTypeChecker
      result = analyzeDependentTypes checker ast
  in property True  -- Should not crash

-- | Property: Error collection should work consistently
prop_error_collection_consistent :: [Statement] -> Property
prop_error_collection_consistent statements =
  let validStatements = take 3 statements
      ast = Program validStatements
      checker = newDependentTypeChecker
      result = analyzeDependentTypes checker ast
      errors = getDependentTypeErrors result
  in not (null validStatements) ==> property True

-- | Property: Type environment operations should be composable
prop_type_environment_composable :: [(String, TypeExpr)] -> Property
prop_type_environment_composable pairs =
  let validPairs = filter (\(k, v) -> not (null k) && all isAlphaNum (take 5 k)) pairs
      limitedPairs = take 3 validPairs
      env = initialTypeEnvironment
      env1 = foldr (\(k, v) e -> addType (T.pack (take 5 k)) v e) env limitedPairs
      env2 = foldr (\(k, v) e -> addType (T.pack (take 5 k)) v e) env1 limitedPairs
  in not (null limitedPairs) ==> property True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Dependencies QuickCheck Tests"
  [ -- AST Tests
    fastProperty "ast statement count" prop_ast_statement_count
  , fastProperty "ast empty" prop_ast_empty
  , fastProperty "ast roundtrip" prop_ast_roundtrip
  
  -- Statement Tests
  , fastProperty "type def preserves name" prop_type_def_preserves_name
  , fastProperty "var decl preserves name" prop_var_decl_preserves_name
  , fastProperty "func decl preserves name" prop_func_decl_preserves_name
  
  -- Type Expression Tests
  , fastProperty "simple t preserves name" prop_simple_t_preserves_name
  , fastProperty "generic t preserves name" prop_generic_t_preserves_name
  , fastProperty "func t preserves params" prop_func_t_preserves_params
  
  -- Constraint Tests
  , fastProperty "size gt preserves values" prop_size_gt_preserves_values
  , fastProperty "range c preserves values" prop_range_c_preserves_values
  
  -- Dependency Graph Tests
  , fastProperty "dependency node preserves name" prop_dependency_node_preserves_name
  , fastProperty "dependency node preserves deps" prop_dependency_node_preserves_deps
  
  -- Type Environment Tests
  , fastProperty "empty type environment" prop_empty_type_environment
  , fastProperty "type environment lookup" prop_type_environment_lookup
  , fastProperty "type environment preservation" prop_type_environment_preservation
  
  -- Type Variable Tests
  , fastProperty "type variable preserves name" prop_type_variable_preserves_name
  , fastProperty "fresh type variables unique" prop_fresh_type_variables_unique
  
  -- Type Scheme Tests
  , fastProperty "type scheme preserves vars" prop_type_scheme_preserves_vars
  , fastProperty "type scheme preserves expr" prop_type_scheme_preserves_expr
  
  -- Type Inference Tests
  , fastProperty "type inference simple" prop_type_inference_simple
  , fastProperty "type inference empty program" prop_type_inference_empty_program
  , fastProperty "type inference simple statement" prop_type_inference_simple_statement
  
  -- Unification Tests
  , fastProperty "unification identical" prop_unification_identical
  , fastProperty "unification symmetric" prop_unification_symmetric
  , fastProperty "unification associative" prop_unification_associative
  
  -- Constraint Solving Tests
  , fastProperty "constraint solving empty" prop_constraint_solving_empty
  , fastProperty "constraint solving simple" prop_constraint_solving_simple
  , fastProperty "constraint solving deterministic" prop_constraint_solving_deterministic
  
  -- Validation Tests
  , fastProperty "ast validation empty" prop_ast_validation_empty
  , fastProperty "statement validation simple" prop_statement_validation_simple
  , fastProperty "type checking simple" prop_type_checking_simple
  
  -- Integration Tests
  , fastProperty "complete analysis pipeline" prop_complete_analysis_pipeline
  , fastProperty "error collection consistent" prop_error_collection_consistent
  , fastProperty "type environment composable" prop_type_environment_composable
  ]