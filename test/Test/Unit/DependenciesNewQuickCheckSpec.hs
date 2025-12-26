{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, vectorOf, elements, oneof)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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

-- Arbitrary instances for Dependencies types

instance Arbitrary AST where
  arbitrary = do
    statements <- vectorOf 3 arbitrary
    return $ Program statements

instance Arbitrary Statement where
  arbitrary = oneof
    [ STypeDef <$> genText <*> vectorOf 2 genText <*> vectorOf 2 arbitrary
    , STypeAlias <$> genText <*> arbitrary <*> vectorOf 2 arbitrary
    , SVarDecl <$> genText <*> arbitrary
    , SFuncDecl <$> genText <*> vectorOf 2 (genText `pairWith` arbitrary) <*> oneof [return Nothing, Just <$> arbitrary]
    , SConstraintDef <$> genText <*> arbitrary
    , SExistsDecl <$> vectorOf 2 genText <*> arbitrary
    ]

instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ SimpleT <$> genText
    , GenericT <$> genText <*> vectorOf 2 arbitrary
    , FuncT <$> vectorOf 2 (genText `pairWith` arbitrary) <*> arbitrary
    , RefineT <$> arbitrary <*> vectorOf 2 arbitrary
    ]

instance Arbitrary Constraint where
  arbitrary = oneof
    [ SizeGT <$> genText <*> choose (0, 100)
    , SizeGE <$> genText <*> choose (0, 100)
    , RangeC <$> genText <*> choose (0, 50) <*> choose (51, 100)
    , PredC <$> genText <*> vectorOf 2 arbitrary
    ]

instance Arbitrary DependencyNode where
  arbitrary = do
    name <- genText
    deps <- vectorOf 2 genText
    return $ DependencyNode (T.unpack name) (map T.unpack deps)

instance Arbitrary DependencyGraph where
  arbitrary = do
    nodes <- vectorOf 3 arbitrary
    let nodeMap = Map.fromList $ map (\n -> (nodeName n, n)) nodes
    return $ DependencyGraph nodeMap

instance Arbitrary TypeVar where
  arbitrary = oneof
    [ TVCon <$> genText
    , TVVar <$> genText
    , TVApp <$> genText <*> vectorOf 2 arbitrary
    , TVFun <$> vectorOf 2 arbitrary <*> arbitrary
    , TVTuple <$> vectorOf 2 arbitrary
    ]

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ Equal <$> arbitrary <*> arbitrary
    , Subtype <$> arbitrary <*> arbitrary
    , Predicate <$> genText <*> vectorOf 2 arbitrary
    , TypeSizeGE <$> arbitrary <*> choose (0, 100)
    , TypeSizeGT <$> arbitrary <*> choose (0, 100)
    , TypeRange <$> arbitrary <*> choose (0, 50) <*> choose (51, 100)
    ]

instance Arbitrary DependentTypeError where
  arbitrary = oneof
    [ DependentTypeMismatch <$> arbitrary <*> arbitrary
    , ConstraintViolation <$> genText <*> arbitrary
    , TypeNotFound <$> genText
    , InvalidTypeArgument <$> genText
    , UnsolvableConstraint <$> arbitrary
    , DependentInfiniteType <$> genText <*> arbitrary
    , AmbiguousType <$> genText
    , ParseError <$> genText
    , SemanticError <$> genText
    ]

instance Arbitrary TypeDef where
  arbitrary = do
    params <- vectorOf 2 genText
    constraints <- vectorOf 2 arbitrary
    return $ TypeDefDecl params constraints

instance Arbitrary TypeEnv where
  arbitrary = do
    typeDefs <- arbitrary
    pendingConstraints <- vectorOf 2 arbitrary
    return $ TypeEnv typeDefs pendingConstraints

instance Arbitrary DependentTypeChecker where
  arbitrary = do
    typeEnv <- arbitrary
    errors <- vectorOf 2 arbitrary
    return $ DependentTypeChecker typeEnv errors

-- Helper generators
genText :: Gen Text
genText = T.pack <$> genSafeString

genSafeString :: Gen String
genSafeString = do
  size <- choose (1, 10)
  vectorOf size $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

pairWith :: Gen a -> Gen b -> Gen (a, b)
pairWith genA genB = do
  a <- genA
  b <- genB
  return (a, b)

-- Property: Program constructor preserves statement list
prop_program_preserves_statements :: [Statement] -> Property
prop_program_preserves_statements statements =
  let program = Program statements
  in case program of
       Program stmts -> property $ stmts === statements

-- Property: SimpleT constructor preserves type name
prop_simple_t_preserves_name :: Text -> Property
prop_simple_t_preserves_name name =
  let typeExpr = SimpleT name
  in case typeExpr of
       SimpleT n -> property $ n === name

-- Property: GenericT constructor preserves name and arguments
prop_generic_t_preserves_args :: Text -> [TypeExpr] -> Property
prop_generic_t_preserves_args name args =
  let typeExpr = GenericT name args
  in case typeExpr of
       GenericT n a -> property $ n === name .&&. a === args

-- Property: FuncT constructor preserves parameters and return type
prop_func_t_preserves_signature :: [(Text, TypeExpr)] -> TypeExpr -> Property
prop_func_t_preserves_signature params retType =
  let typeExpr = FuncT params retType
  in case typeExpr of
       FuncT p r -> property $ p === params .&&. r === retType

-- Property: RefineT constructor preserves base type and constraints
prop_refine_t_preserves_constraints :: TypeExpr -> [Constraint] -> Property
prop_refine_t_preserves_constraints baseType constraints =
  let typeExpr = RefineT baseType constraints
  in case typeExpr of
       RefineT b c -> property $ b === baseType .&&. c === constraints

-- Property: SizeGT constraint preserves variable and size
prop_size_gt_preserves_values :: Text -> Int -> Property
prop_size_gt_preserves_values var size =
  let constraint = SizeGT var size
  in case constraint of
       SizeGT v s -> property $ v === var .&&. s === size

-- Property: SizeGE constraint preserves variable and size
prop_size_ge_preserves_values :: Text -> Int -> Property
prop_size_ge_preserves_values var size =
  let constraint = SizeGE var size
  in case constraint of
       SizeGE v s -> property $ v === var .&&. s === size

-- Property: RangeC constraint preserves variable and range
prop_range_c_preserves_values :: Text -> Int -> Int -> Property
prop_range_c_preserves_values var minVal maxVal =
  let constraint = RangeC var minVal maxVal
  in case constraint of
       RangeC v mn mx -> property $ v === var .&&. mn === minVal .&&. mx === maxVal

-- Property: PredC constraint preserves predicate and arguments
prop_pred_c_preserves_values :: Text -> [TypeExpr] -> Property
prop_pred_c_preserves_values predName args =
  let constraint = PredC predName args
  in case constraint of
       PredC p a -> property $ p === predName .&&. a === args

-- Property: DependencyNode constructor preserves name and dependencies
prop_dependency_node_preserves_fields :: Text -> [Text] -> Property
prop_dependency_node_preserves_fields name deps =
  let node = DependencyNode (T.unpack name) (map T.unpack deps)
  in property $ nodeName node === T.unpack name .&&.
             nodeDependencies node === map T.unpack deps

-- Property: DependencyGraph constructor preserves node map
prop_dependency_graph_preserves_nodes :: [DependencyNode] -> Property
prop_dependency_graph_preserves_nodes nodes =
  let nodeMap = Map.fromList $ map (\n -> (nodeName n, n)) nodes
      graph = DependencyGraph nodeMap
  in property $ graphNodes graph === nodeMap

-- Property: newDependentTypeChecker creates checker with prelude types
prop_new_dependent_type_checker_has_prelude :: Property
prop_new_dependent_type_checker_has_prelude =
  let checker = newDependentTypeChecker
      typeEnv = dtcTypeEnv checker
      typeDefs = typeDefinitions typeEnv
  in property $ Map.size typeDefs >= Map.size preludeTypeDefs .&&.
             all (`Map.member` typeDefs) (Map.keys preludeTypeDefs)

-- Property: newDependentTypeCheckerWithTypes creates checker with custom types
prop_new_dependent_type_checker_with_custom_types :: [(String, [String], [TypeConstraint])] -> Property
prop_new_dependent_type_checker_with_custom_types typeDefs =
  not (null typeDefs) ==>
  let checker = newDependentTypeCheckerWithTypes typeDefs
      typeEnv = dtcTypeEnv checker
      typeDefsMap = typeDefinitions typeEnv
      expectedNames = map (\(n, _, _) -> n) typeDefs
  in property $ all (`Map.member` typeDefsMap) expectedNames

-- Property: lookupTypeDef finds existing type
prop_lookup_type_def_finds_existing :: String -> TypeDef -> Property
prop_lookup_type_def_finds_existing typeName typeDef =
  let checker = newDependentTypeCheckerWithTypes [(typeName, [], [])]
      result = lookupTypeDef typeName checker
  in property $ result === Just typeDef

-- Property: lookupTypeDef returns Nothing for non-existing type
prop_lookup_type_def_nothing_for_nonexisting :: String -> Property
prop_lookup_type_def_nothing_for_nonexisting typeName =
  let checker = newDependentTypeChecker
      result = lookupTypeDef typeName checker
  in property $ result === Nothing

-- Property: addType adds type to environment
prop_add_type_adds_to_environment :: String -> [String] -> [TypeConstraint] -> Property
prop_add_type_adds_to_environment typeName params constraints =
  let checker = newDependentTypeChecker
      updatedChecker = addType typeName params constraints checker
      typeEnv = dtcTypeEnv updatedChecker
      typeDefs = typeDefinitions typeEnv
  in property $ Map.member typeName typeDefs

-- Property: convertTypeExpr handles SimpleT correctly
prop_convert_type_expr_simple_t :: Text -> Property
prop_convert_type_expr_simple_t name =
  let typeExpr = SimpleT name
      params = Set.empty
      result = convertTypeExpr params typeExpr
  in case result of
       TVCon n -> property $ n === T.unpack name
       _ -> property False

-- Property: convertConstraint handles SizeGT correctly
prop_convert_constraint_size_gt :: Text -> Int -> Property
prop_convert_constraint_size_gt var size =
  let constraint = SizeGT var size
      params = Set.fromList [T.unpack var]
      result = convertConstraint params constraint
  in case result of
       TypeSizeGT (TVVar v) s -> property $ v === T.unpack var .&&. s === size
       _ -> property False

-- Property: convertConstraint handles SizeGE correctly
prop_convert_constraint_size_ge :: Text -> Int -> Property
prop_convert_constraint_size_ge var size =
  let constraint = SizeGE var size
      params = Set.fromList [T.unpack var]
      result = convertConstraint params constraint
  in case result of
       TypeSizeGE (TVVar v) s -> property $ v === T.unpack var .&&. s === size
       _ -> property False

-- Property: convertConstraint handles RangeC correctly
prop_convert_constraint_range_c :: Text -> Int -> Int -> Property
prop_convert_constraint_range_c var minVal maxVal =
  let constraint = RangeC var minVal maxVal
      params = Set.fromList [T.unpack var]
      result = convertConstraint params constraint
  in case result of
       TypeRange (TVVar v) mn mx -> property $ v === T.unpack var .&&. mn === minVal .&&. mx === maxVal
       _ -> property False

-- Property: getDependentTypeErrors returns errors from checker
prop_get_dependent_type_errors_returns_errors :: [DependentTypeError] -> Property
prop_get_dependent_type_errors_returns_errors errors =
  let checker = DependentTypeChecker (TypeEnv Map.empty []) errors
      result = getDependentTypeErrors checker
  in property $ result === errors

-- Property: TVCon constructor preserves constructor name
prop_tv_con_preserves_name :: String -> Property
prop_tv_con_preserves_name name =
  let typeVar = TVCon name
  in case typeVar of
       TVCon n -> property $ n === name

-- Property: TVVar constructor preserves variable name
prop_tv_var_preserves_name :: String -> Property
prop_tv_var_preserves_name name =
  let typeVar = TVVar name
  in case typeVar of
       TVVar n -> property $ n === name

-- Property: TVApp constructor preserves constructor and arguments
prop_tv_app_preserves_args :: String -> [TypeVar] -> Property
prop_tv_app_preserves_args constructor args =
  let typeVar = TVApp constructor args
  in case typeVar of
       TVApp c a -> property $ c === constructor .&&. a === args

-- Property: TVFun constructor preserves parameters and return type
prop_tv_fun_preserves_signature :: [TypeVar] -> TypeVar -> Property
prop_tv_fun_preserves_signature params retType =
  let typeVar = TVFun params retType
  in case typeVar of
       TVFun p r -> property $ p === params .&&. r === retType

-- Property: TVTuple constructor preserves elements
prop_tv_tuple_preserves_elements :: [TypeVar] -> Property
prop_tv_tuple_preserves_elements elements =
  let typeVar = TVTuple elements
  in case typeVar of
       TVTuple e -> property $ e === elements

-- Property: Equal constraint preserves both type variables
prop_equal_preserves_types :: TypeVar -> TypeVar -> Property
prop_equal_preserves_types typeVar1 typeVar2 =
  let constraint = Equal typeVar1 typeVar2
  in case constraint of
       Equal t1 t2 -> property $ t1 === typeVar1 .&&. t2 === typeVar2

-- Property: Subtype constraint preserves both type variables
prop_subtype_preserves_types :: TypeVar -> TypeVar -> Property
prop_subtype_preserves_types typeVar1 typeVar2 =
  let constraint = Subtype typeVar1 typeVar2
  in case constraint of
       Subtype t1 t2 -> property $ t1 === typeVar1 .&&. t2 === typeVar2

-- Property: Predicate constraint preserves name and arguments
prop_predicate_preserves_args :: String -> [TypeVar] -> Property
prop_predicate_preserves_args predName args =
  let constraint = Predicate predName args
  in case constraint of
       Predicate p a -> property $ p === predName .&&. a === args

tests :: TestTree
tests = testGroup "Dependencies New QuickCheck Tests"
  [ fastProperty "Program constructor preserves statement list" prop_program_preserves_statements
  , fastProperty "SimpleT constructor preserves type name" prop_simple_t_preserves_name
  , fastProperty "GenericT constructor preserves name and arguments" prop_generic_t_preserves_args
  , fastProperty "FuncT constructor preserves parameters and return type" prop_func_t_preserves_signature
  , fastProperty "RefineT constructor preserves base type and constraints" prop_refine_t_preserves_constraints
  , fastProperty "SizeGT constraint preserves variable and size" prop_size_gt_preserves_values
  , fastProperty "SizeGE constraint preserves variable and size" prop_size_ge_preserves_values
  , fastProperty "RangeC constraint preserves variable and range" prop_range_c_preserves_values
  , fastProperty "PredC constraint preserves predicate and arguments" prop_pred_c_preserves_values
  , fastProperty "DependencyNode constructor preserves name and dependencies" prop_dependency_node_preserves_fields
  , fastProperty "DependencyGraph constructor preserves node map" prop_dependency_graph_preserves_nodes
  , fastProperty "newDependentTypeChecker creates checker with prelude types" prop_new_dependent_type_checker_has_prelude
  , fastProperty "newDependentTypeCheckerWithTypes creates checker with custom types" prop_new_dependent_type_checker_with_custom_types
  , fastProperty "lookupTypeDef finds existing type" prop_lookup_type_def_finds_existing
  , fastProperty "lookupTypeDef returns Nothing for non-existing type" prop_lookup_type_def_nothing_for_nonexisting
  , fastProperty "addType adds type to environment" prop_add_type_adds_to_environment
  , fastProperty "convertTypeExpr handles SimpleT correctly" prop_convert_type_expr_simple_t
  , fastProperty "convertConstraint handles SizeGT correctly" prop_convert_constraint_size_gt
  , fastProperty "convertConstraint handles SizeGE correctly" prop_convert_constraint_size_ge
  , fastProperty "convertConstraint handles RangeC correctly" prop_convert_constraint_range_c
  , fastProperty "getDependentTypeErrors returns errors from checker" prop_get_dependent_type_errors_returns_errors
  , fastProperty "TVCon constructor preserves constructor name" prop_tv_con_preserves_name
  , fastProperty "TVVar constructor preserves variable name" prop_tv_var_preserves_name
  , fastProperty "TVApp constructor preserves constructor and arguments" prop_tv_app_preserves_args
  , fastProperty "TVFun constructor preserves parameters and return type" prop_tv_fun_preserves_signature
  , fastProperty "TVTuple constructor preserves elements" prop_tv_tuple_preserves_elements
  , fastProperty "Equal constraint preserves both type variables" prop_equal_preserves_types
  , fastProperty "Subtype constraint preserves both type variables" prop_subtype_preserves_types
  , fastProperty "Predicate constraint preserves name and arguments" prop_predicate_preserves_args
  ]