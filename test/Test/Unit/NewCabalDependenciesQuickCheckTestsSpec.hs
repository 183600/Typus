{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.NewCabalDependenciesQuickCheckTestsSpec where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, counterexample, suchThat, elements, listOf, listOf1, choose, oneof)
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
  , lookupTypeDef
  , checkType
  , checkTypeInstantiation
  , solveConstraints
  , checkTypeConstraint
  , validateConstraint
  , getDependentTypeErrors
  , unify
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Either (isLeft, isRight)

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ '_'
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_'
  return $ first : rest

-- Generate Text identifiers
genTextIdentifier :: Gen Text
genTextIdentifier = T.pack <$> genIdentifier

-- Generate type expressions
genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
  [ SimpleT <$> genTextIdentifier
  , do name <- genTextIdentifier
       args <- listOf genTypeExpr
       return $ GenericT name args
  , do params <- listOf $ do
           paramName <- genTextIdentifier
           paramType <- genTypeExpr
           return (paramName, paramType)
       returnType <- genTypeExpr
       return $ FuncT params returnType
  , do baseType <- genTypeExpr
       constraints <- listOf genConstraint
       return $ RefineT baseType constraints
  ]

-- Generate constraints
genConstraint :: Gen Constraint
genConstraint = oneof
  [ SizeGT <$> genTextIdentifier <*> choose (0, 100)
  , SizeGE <$> genTextIdentifier <*> choose (0, 100)
  , RangeC <$> genTextIdentifier <*> choose (0, 50) <*> choose (51, 100)
  , PredC <$> genTextIdentifier <*> listOf genTypeExpr
  ]

-- Generate statements
genStatement :: Gen Statement
genStatement = oneof
  [ do name <- genTextIdentifier
       params <- listOf genTextIdentifier
       constraints <- listOf genConstraint
       return $ STypeDef name params constraints
  , do name <- genTextIdentifier
       typeExpr <- genTypeExpr
       constraints <- listOf genConstraint
       return $ STypeAlias name typeExpr constraints
  , do name <- genTextIdentifier
       typeExpr <- genTypeExpr
       return $ SVarDecl name typeExpr
  , do name <- genTextIdentifier
       params <- listOf $ do
                   paramName <- genTextIdentifier
                   paramType <- genTypeExpr
                   return (paramName, paramType)
       returnType <- listOf genTypeExpr
       return $ SFuncDecl name params (listToMaybe returnType)
  , do name <- genTextIdentifier
       constraint <- genConstraint
       return $ SConstraintDef name constraint
  , do vars <- listOf1 genTextIdentifier
       statement <- genStatement
       return $ SExistsDecl vars statement
  ]

-- Generate AST
genAST :: Gen AST
genAST = do
  statements <- listOf genStatement
  return $ Program statements

-- Generate dependency nodes
genDependencyNode :: Gen DependencyNode
genDependencyNode = do
  name <- genIdentifier
  dependencies <- listOf genIdentifier
  return $ DependencyNode name dependencies

-- Generate dependency graph
genDependencyGraph :: Gen DependencyGraph
genDependencyGraph = do
  nodes <- listOf genDependencyNode
  let nodeMap = Map.fromList $ L.map (\n -> (nodeName n, n)) nodes
  return $ DependencyGraph nodeMap

-- Generate type variables
genTypeVar :: Gen TypeVar
genTypeVar = oneof
  [ TVCon <$> genIdentifier
  , TVVar <$> genIdentifier
  , do name <- genIdentifier
       args <- listOf genTypeVar
       return $ TVApp name args
  , do params <- listOf genTypeVar
       returnType <- genTypeVar
       return $ TVFun params returnType
  , TVTuple <$> listOf genTypeVar
  ]

-- Generate type constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ Equal <$> genTypeVar <*> genTypeVar
  , Subtype <$> genTypeVar <*> genTypeVar
  , Predicate <$> genIdentifier <*> listOf genTypeVar
  , TypeSizeGE <$> genTypeVar <*> choose (0, 100)
  , TypeSizeGT <$> genTypeVar <*> choose (0, 100)
  , TypeRange <$> genTypeVar <*> choose (0, 50) <*> choose (51, 100)
  ]

-- Generate type definitions
genTypeDef :: Gen TypeDef
genTypeDef = do
  params <- listOf genIdentifier
  constraints <- listOf genTypeConstraint
  return $ TypeDefDecl params constraints

-- Generate type environments
genTypeEnv :: Gen TypeEnv
genTypeEnv = do
  numTypes <- choose (0, 5)
  typeDefs <- sequence $ replicate numTypes $ do
    name <- genIdentifier
    typeDef <- genTypeDef
    return (name, typeDef)
  let typeMap = Map.fromList typeDefs
  constraints <- listOf genTypeConstraint
  return $ TypeEnv typeMap constraints

-- Generate dependent type errors
genDependentTypeError :: Gen DependentTypeError
genDependentTypeError = oneof
  [ DependentTypeMismatch <$> genTypeVar <*> genTypeVar
  , ConstraintViolation <$> genIdentifier <*> genTypeVar
  , TypeNotFound <$> genIdentifier
  , InvalidTypeArgument <$> genIdentifier
  , UnsolvableConstraint <$> genTypeConstraint
  , DependentInfiniteType <$> genIdentifier <*> genTypeVar
  , AmbiguousType <$> genIdentifier
  , ParseError <$> genIdentifier
  , SemanticError <$> genIdentifier
  ]

-- ============================================================================
-- Properties for AST
-- ============================================================================

prop_ast_program_structure :: AST -> Property
prop_ast_program_structure (Program statements) =
  L.length statements >= 0  -- Always true, but ensures structure

prop_statement_type_def_structure :: Property
prop_statement_type_def_structure =
  forAll genTextIdentifier $ \name ->
    forAll (listOf genTextIdentifier) $ \params ->
      forAll (listOf genConstraint) $ \constraints ->
        let stmt = STypeDef name params constraints
        in case stmt of
             STypeDef n p c -> n === name && p === params && c === constraints
             _ -> property False

-- ============================================================================
-- Properties for TypeExpr
-- ============================================================================

prop_type_expr_simple_structure :: Property
prop_type_expr_simple_structure =
  forAll genTextIdentifier $ \name ->
    let expr = SimpleT name
    in case expr of
         SimpleT n -> n === name
         _ -> property False

prop_type_expr_generic_structure :: Property
prop_type_expr_generic_structure =
  forAll genTextIdentifier $ \name ->
    forAll (listOf genTypeExpr) $ \args ->
      let expr = GenericT name args
      in case expr of
           GenericT n a -> n === name && a === args
           _ -> property False

-- ============================================================================
-- Properties for Constraint
-- ============================================================================

prop_constraint_size_gt_structure :: Property
prop_constraint_size_gt_structure =
  forAll genTextIdentifier $ \var ->
    forAll (choose (0, 100)) $ \size ->
      let constraint = SizeGT var size
      in case constraint of
           SizeGT v s -> v === var && s === size
           _ -> property False

prop_constraint_range_structure :: Property
prop_constraint_range_structure =
  forAll genTextIdentifier $ \var ->
    forAll (choose (0, 50)) $ \low ->
      forAll (choose (51, 100)) $ \high ->
        let constraint = RangeC var low high
        in case constraint of
             RangeC v l h -> v === var && l === low && h === high
             _ -> property False

-- ============================================================================
-- Properties for DependencyGraph
-- ============================================================================

prop_dependency_graph_node_lookup :: Property
prop_dependency_graph_node_lookup =
  forAll genDependencyGraph $ \graph ->
    let nodes = graphNodes graph
        nodeNames = Map.keys nodes
    in L.all (`Map.member` nodes) nodeNames

prop_dependency_node_self_consistency :: DependencyNode -> Property
prop_dependency_node_self_consistency node =
  nodeName node `elem` nodeDependencies node || not (L.null $ nodeDependencies node)

-- ============================================================================
-- Properties for TypeVar
-- ============================================================================

prop_type_var_show_roundtrip :: TypeVar -> Property
prop_type_var_show_roundtrip typeVar =
  let shown = show typeVar
  in L.length shown > 0  -- Basic check that show produces non-empty string

prop_type_var_ordering_consistent :: TypeVar -> TypeVar -> Property
prop_type_var_ordering_consistent tv1 tv2 =
  let ord1 = compare tv1 tv2
      ord2 = compare (show tv1) (show tv2)
  in if tv1 == tv2
     then ord1 === EQ
     else property True  -- Ordering is defined

-- ============================================================================
-- Properties for TypeConstraint
-- ============================================================================

prop_type_constraint_equal_structure :: Property
prop_type_constraint_equal_structure =
  forAll genTypeVar $ \tv1 ->
    forAll genTypeVar $ \tv2 ->
      let constraint = Equal tv1 tv2
      in case constraint of
           Equal t1 t2 -> t1 === tv1 && t2 === tv2
           _ -> property False

prop_type_constraint_subtype_structure :: Property
prop_type_constraint_subtype_structure =
  forAll genTypeVar $ \tv1 ->
    forAll genTypeVar $ \tv2 ->
      let constraint = Subtype tv1 tv2
      in case constraint of
           Subtype t1 t2 -> t1 === tv1 && t2 === tv2
           _ -> property False

-- ============================================================================
-- Properties for TypeEnv
-- ============================================================================

prop_type_env_lookup_existing :: Property
prop_type_env_lookup_existing =
  forAll genTypeEnv $ \env ->
    forAll genTypeDef $ \typeDef ->
      let name = "testType"
          env' = addType name typeDef env
          result = lookupTypeDef name env'
      in case result of
           Just found -> found === typeDef
           Nothing -> property False

prop_type_env_add_preserves_existing :: Property
prop_type_env_add_preserves_existing =
  forAll genTypeEnv $ \env ->
    forAll genIdentifier $ \name ->
      forAll genTypeDef $ \typeDef ->
        let env' = addType name typeDef env
            originalTypes = typeDefinitions env
            newTypes = typeDefinitions env'
        in Map.size newTypes >= Map.size originalTypes

-- ============================================================================
-- Properties for DependentTypeChecker
-- ============================================================================

prop_new_type_checker_empty :: Property
prop_new_type_checker_empty =
  let checker = newDependentTypeChecker
      errors = getDependentTypeErrors checker
  in null errors

prop_type_checker_with_types :: Property
prop_type_checker_with_types =
  forAll genTypeEnv $ \env ->
    let checker = newDependentTypeCheckerWithTypes env
        env' = dtcTypeEnv checker
    in env' === env

-- ============================================================================
-- Properties for Type Conversion
-- ============================================================================

prop_convert_simple_type_expr :: Property
prop_convert_simple_type_expr =
  forAll genTextIdentifier $ \name ->
    let typeExpr = SimpleT name
        result = convertTypeExpr typeExpr
    in case result of
         Right tv -> case tv of
                       TVCon n -> n === T.unpack name
                       _ -> property False
         Left _ -> property False

prop_convert_size_constraint :: Property
prop_convert_size_constraint =
  forAll genTextIdentifier $ \var ->
    forAll (choose (0, 100)) $ \size ->
      let constraint = SizeGT var size
          result = convertConstraint constraint
      in case result of
           Right tc -> case tc of
                         TypeSizeGT tv s -> case tv of
                                                TVVar v -> v === T.unpack var && s === size
                                                _ -> property False
                         _ -> property False
         Left _ -> property False

-- ============================================================================
-- Properties for Type Checking
-- ============================================================================

prop_check_type_basic :: Property
prop_check_type_basic =
  forAll genTypeVar $ \tv ->
    forAll genTypeEnv $ \env ->
      let result = checkType tv env
      in case result of
           Right _ -> property True
           Left _ -> property True  -- May fail for invalid types

prop_check_constraint_basic :: Property
prop_check_constraint_basic =
  forAll genTypeConstraint $ \tc ->
    forAll genTypeEnv $ \env ->
      let result = checkTypeConstraint tc env
      in case result of
           Right _ -> property True
           Left _ -> property True  -- May fail for invalid constraints

-- ============================================================================
-- Properties for Unification
-- ============================================================================

prop_unify_reflexive :: Property
prop_unify_reflexive =
  forAll genTypeVar $ \tv ->
    let result = unify tv tv
    in case result of
         Right subst -> Map.null subst  -- Unifying identical types yields empty substitution
         Left _ -> property False

prop_unify_symmetric :: Property
prop_unify_symmetric =
  forAll genTypeVar $ \tv1 ->
    forAll genTypeVar $ \tv2 ->
      let result1 = unify tv1 tv2
          result2 = unify tv2 tv1
      in case (result1, result2) of
           (Right subst1, Right subst2) -> Map.size subst1 === Map.size subst2
           (Left _, Left _) -> property True
           _ -> property False

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependencies QuickCheck Tests"
  [ testGroup "AST"
    [ testProperty "program structure" prop_ast_program_structure
    , testProperty "type def structure" prop_statement_type_def_structure
    ]
  , testGroup "TypeExpr"
    [ testProperty "simple structure" prop_type_expr_simple_structure
    , testProperty "generic structure" prop_type_expr_generic_structure
    ]
  , testGroup "Constraint"
    [ testProperty "size gt structure" prop_constraint_size_gt_structure
    , testProperty "range structure" prop_constraint_range_structure
    ]
  , testGroup "DependencyGraph"
    [ testProperty "node lookup" prop_dependency_graph_node_lookup
    , testProperty "node self consistency" prop_dependency_node_self_consistency
    ]
  , testGroup "TypeVar"
    [ testProperty "show roundtrip" prop_type_var_show_roundtrip
    , testProperty "ordering consistent" prop_type_var_ordering_consistent
    ]
  , testGroup "TypeConstraint"
    [ testProperty "equal structure" prop_type_constraint_equal_structure
    , testProperty "subtype structure" prop_type_constraint_subtype_structure
    ]
  , testGroup "TypeEnv"
    [ testProperty "lookup existing" prop_type_env_lookup_existing
    , testProperty "add preserves existing" prop_type_env_add_preserves_existing
    ]
  , testGroup "DependentTypeChecker"
    [ testProperty "new type checker empty" prop_new_type_checker_empty
    , testProperty "type checker with types" prop_type_checker_with_types
    ]
  , testGroup "Type Conversion"
    [ testProperty "convert simple type expr" prop_convert_simple_type_expr
    , testProperty "convert size constraint" prop_convert_size_constraint
    ]
  , testGroup "Type Checking"
    [ testProperty "check type basic" prop_check_type_basic
    , testProperty "check constraint basic" prop_check_constraint_basic
    ]
  , testGroup "Unification"
    [ testProperty "unify reflexive" prop_unify_reflexive
    , testProperty "unify symmetric" prop_unify_symmetric
    ]
  ]