module Test.Unit.NewDependenciesCorePropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck ((===), property, testProperty, Property, Arbitrary(..), Gen, choose, listOf, elements, forAll, oneof, suchThat)

import Dependencies.TypeSystem
  ( TypeVar(..), TypeConstraint(..), DependentTypeError(..), TypeDef(..), TypeEnv(..)
  , DependentTypeChecker(..), Substitution
  , newDependentTypeChecker, addType, addConstraint, checkType, solveConstraints
  , lookupTypeDef, checkTypeInstantiation, getDependentTypeErrors
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate type constructor names
genTypeName :: Gen String
genTypeName = elements ["int", "string", "bool", "list", "array", "option", "result"]

-- Generate type variable names
genTypeVarName :: Gen String
genTypeVarName = do
  prefix <- elements ["a", "b", "c", "x", "y", "z", "t", "u", "v"]
  suffix <- choose (0, 10)
  pure $ prefix ++ show suffix

-- Generate simple type variables
genSimpleTypeVar :: Gen TypeVar
genSimpleTypeVar = oneof
  [ TVCon <$> genTypeName
  , TVVar <$> genTypeVarName
  ]

-- Generate function type variables
genFunctionTypeVar :: Gen TypeVar
genFunctionTypeVar = do
  args <- listOf genSimpleTypeVar `suchThat` (not . null)
  result <- genSimpleTypeVar
  pure $ TVFun args result

-- Generate tuple type variables
genTupleTypeVar :: Gen TypeVar
genTupleTypeVar = do
  elems <- listOf genSimpleTypeVar `suchThat` (not . null)
  pure $ TVTuple elems

-- Generate application type variables
genAppTypeVar :: Gen TypeVar
genAppTypeVar = do
  name <- genTypeName
  args <- listOf genSimpleTypeVar
  pure $ TVApp name args

-- Generate L.any type variable
genTypeVar :: Gen TypeVar
genTypeVar = oneof
  [ genSimpleTypeVar
  , genFunctionTypeVar
  , genTupleTypeVar
  , genAppTypeVar
  ]

-- Generate type constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ Equal <$> genTypeVar <*> genTypeVar
  , Subtype <$> genTypeVar <*> genTypeVar
  , Predicate <$> genTypeName <*> listOf genTypeVar
  , TypeSizeGE <$> genTypeVar <*> choose (0, 100)
  , TypeSizeGT <$> genTypeVar <*> choose (0, 100)
  , TypeRange <$> genTypeVar <*> choose (0, 50) <*> choose (51, 100)
  ]

-- Generate dependent type errors
genDependentTypeError :: Gen DependentTypeError
genDependentTypeError = oneof
  [ DependentTypeMismatch <$> genTypeVar <*> genTypeVar
  , ConstraintViolation <$> genTypeName <*> genTypeVar
  , TypeNotFound <$> genTypeName
  , InvalidTypeArgument <$> genTypeName
  , UnsolvableConstraint <$> genTypeConstraint
  , DependentInfiniteType <$> genTypeVarName <*> genTypeVar
  , AmbiguousType <$> genTypeName
  , ParseError <$> genTypeName
  , SemanticError <$> genTypeName
  ]

-- Generate type definitions
genTypeDef :: Gen TypeDef
genTypeDef = do
  params <- listOf genTypeVarName
  constraints <- listOf genTypeConstraint
  pure $ TypeDefDecl params constraints

-- ============================================================================
-- Property Tests for TypeVar
-- ============================================================================

-- Property: TypeVar equality should be reflexive
prop_type_var_equality_reflexive :: Property
prop_type_var_equality_reflexive = 
  forAll genTypeVar $ \tv ->
    tv === tv

-- Property: TypeVar equality should be symmetric
prop_type_var_equality_symmetric :: Property
prop_type_var_equality_symmetric = 
  forAll genTypeVar $ \tv1 ->
    forAll genTypeVar $ \tv2 ->
      (tv1 == tv2) === (tv2 == tv1)

-- Property: TVCon with same name should be equal
prop_tv_con_equality :: Property
prop_tv_con_equality = 
  forAll genTypeName $ \name ->
    let tv1 = TVCon name
        tv2 = TVCon name
    in tv1 === tv2

-- Property: TVVar with same name should be equal
prop_tv_var_equality :: Property
prop_tv_var_equality = 
  forAll genTypeVarName $ \name ->
    let tv1 = TVVar name
        tv2 = TVVar name
    in tv1 === tv2

-- Property: TypeVar ordering should be consistent
prop_type_var_ordering_consistency :: Property
prop_type_var_ordering_consistency = 
  forAll genTypeVar $ \tv1 ->
    forAll genTypeVar $ \tv2 ->
      let comparison = compare tv1 tv2
          reverseComparison = compare tv2 tv1
      in if tv1 == tv2 then comparison === EQ else comparison === negate reverseComparison

-- ============================================================================
-- Property Tests for TypeConstraint
-- ============================================================================

-- Property: TypeConstraint equality should be reflexive
prop_type_constraint_equality_reflexive :: Property
prop_type_constraint_equality_reflexive = 
  forAll genTypeConstraint $ \tc ->
    tc === tc

-- Property: TypeConstraint equality should be symmetric
prop_type_constraint_equality_symmetric :: Property
prop_type_constraint_equality_symmetric = 
  forAll genTypeConstraint $ \tc1 ->
    forAll genTypeConstraint $ \tc2 ->
      (tc1 == tc2) === (tc2 == tc1)

-- Property: Equal constraints with same types should be equal
prop_equal_constraint_equality :: Property
prop_equal_constraint_equality = 
  forAll genTypeVar $ \tv1 ->
    forAll genTypeVar $ \tv2 ->
      let constraint1 = Equal tv1 tv2
          constraint2 = Equal tv1 tv2
      in constraint1 === constraint2

-- Property: TypeRange constraints should have valid bounds
prop_type_range_valid_bounds :: Property
prop_type_range_valid_bounds = 
  forAll genTypeVar $ \tv ->
    forAll (choose (0, 50)) $ \minVal ->
      forAll (choose (51, 100)) $ \maxVal ->
        let constraint = TypeRange tv minVal maxVal
        in minVal < maxVal

-- ============================================================================
-- Property Tests for DependentTypeError
-- ============================================================================

-- Property: DependentTypeError equality should be reflexive
prop_dependent_type_error_equality_reflexive :: Property
prop_dependent_type_error_equality_reflexive = 
  forAll genDependentTypeError $ \err ->
    err === err

-- Property: DependentTypeError equality should be symmetric
prop_dependent_type_error_equality_symmetric :: Property
prop_dependent_type_error_equality_symmetric = 
  forAll genDependentTypeError $ \err1 ->
    forAll genDependentTypeError $ \err2 ->
      (err1 == err2) === (err2 == err1)

-- Property: TypeNotFound errors should be equal only for same type name
prop_type_not_found_equality :: Property
prop_type_not_found_equality = 
  forAll genTypeName $ \name1 ->
    forAll genTypeName $ \name2 ->
      let error1 = TypeNotFound name1
          error2 = TypeNotFound name2
      in (error1 == error2) === (name1 == name2)

-- ============================================================================
-- Property Tests for TypeDef
-- ============================================================================

-- Property: TypeDef equality should be reflexive
prop_type_def_equality_reflexive :: Property
prop_type_def_equality_reflexive = 
  forAll genTypeDef $ \td ->
    td === td

-- Property: TypeDef equality should be symmetric
prop_type_def_equality_symmetric :: Property
prop_type_def_equality_symmetric = 
  forAll genTypeDef $ \td1 ->
    forAll genTypeDef $ \td2 ->
      (td1 == td2) === (td2 == td1)

-- Property: TypeDef with same params L.and constraints should be equal
prop_type_def_structural_equality :: Property
prop_type_def_structural_equality = 
  forAll (listOf genTypeVarName) $ \params ->
    forAll (listOf genTypeConstraint) $ \constraints ->
      let td1 = TypeDefDecl params constraints
          td2 = TypeDefDecl params constraints
      in td1 === td2

-- ============================================================================
-- Property Tests for TypeEnv
-- ============================================================================

-- Property: TypeEnv equality should be reflexive
prop_type_env_equality_reflexive :: Property
prop_type_env_equality_reflexive = 
  forAll (listOf genTypeDef) $ \typedefs ->
    forAll (listOf genTypeConstraint) $ \constraints ->
      let typeMap = Map.fromList $ zip (map show typedefs) typedefs
          env = TypeEnv typeMap constraints
      in env === env

-- Property: Adding types to environment should preserve existing types
prop_add_type_preservation :: Property
prop_add_type_preservation = 
  forAll genTypeDef $ \td ->
    forAll (listOf genTypeDef) $ \existingTypes ->
      let typeMap = Map.fromList $ zip (map show existingTypes) existingTypes
          env = TypeEnv typeMap []
          newEnv = addType "test_type" td env
          originalTypes = Map.lookup "test_type" (typeDefinitions newEnv)
      in originalTypes === Just td

-- ============================================================================
-- Property Tests for DependentTypeChecker
-- ============================================================================

-- Property: New dependent type checker should have no errors
prop_new_checker_no_errors :: Property
prop_new_checker_no_errors = 
  let checker = newDependentTypeChecker
      errors = getDependentTypeErrors checker
  in null errors

-- Property: Adding constraints should increase pending constraints count
prop_add_constraint_increases_count :: Property
prop_add_constraint_increases_count = 
  forAll genTypeConstraint $ \constraint ->
    let checker = newDependentTypeChecker
        newChecker = addConstraint constraint checker
        originalEnv = dtcTypeEnv checker
        newEnv = dtcTypeEnv newChecker
        originalCount = L.length (pendingConstraints originalEnv)
        newCount = L.length (pendingConstraints newEnv)
    in newCount === originalCount + 1

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_type_var_creation :: IO ()
test_type_var_creation = do
  let simple = TVCon "int"
      variable = TVVar "a"
      function = TVFun [TVCon "int", TVCon "string"] (TVCon "bool")
      tuple = TVTuple [TVCon "int", TVCon "string"]
      app = TVApp "list" [TVCon "int"]
  
  show simple @?= "TVCon \"int\""
  show variable @?= "TVVar \"a\""
  show function @?= "TVFun [TVCon \"int\",TVCon \"string\"] (TVCon \"bool\")"
  show tuple @?= "TVTuple [TVCon \"int\",TVCon \"string\"]"
  show app @?= "TVApp \"list\" [TVCon \"int\"]"

test_type_constraint_creation :: IO ()
test_type_constraint_creation = do
  let equal = Equal (TVVar "a") (TVVar "b")
      subtype = Subtype (TVCon "int") (TVCon "number")
      predicate = Predicate "numeric" [TVVar "a"]
      sizeGE = TypeSizeGE (TVVar "list") 5
      sizeGT = TypeSizeGT (TVVar "list") 0
      range = TypeRange (TVVar "int") 0 100
  
  show equal @?= "Equal (TVVar \"a\") (TVVar \"b\")"
  show subtype @?= "Subtype (TVCon \"int\") (TVCon \"number\")"
  show predicate @?= "Predicate \"numeric\" [TVVar \"a\"]"
  show sizeGE @?= "TypeSizeGE (TVVar \"list\") 5"
  show sizeGT @?= "TypeSizeGT (TVVar \"list\") 0"
  show range @?= "TypeRange (TVVar \"int\") 0 100"

test_dependent_type_error_creation :: IO ()
test_dependent_type_error_creation = do
  let mismatch = DependentTypeMismatch (TVVar "a") (TVVar "b")
      violation = ConstraintViolation "numeric" (TVVar "a")
      notFound = TypeNotFound "UnknownType"
      invalid = InvalidTypeArgument "int"
      unsolvable = UnsolvableConstraint (Equal (TVVar "a") (TVVar "b"))
  
  show mismatch @?= "DependentTypeMismatch (TVVar \"a\") (TVVar \"b\")"
  show violation @?= "ConstraintViolation \"numeric\" (TVVar \"a\")"
  show notFound @?= "TypeNotFound \"UnknownType\""
  show invalid @?= "InvalidTypeArgument \"int\""
  show unsolvable @?= "UnsolvableConstraint (Equal (TVVar \"a\") (TVVar \"b\"))"

test_type_environment_operations :: IO ()
test_type_environment_operations = do
  let checker = newDependentTypeChecker
      typeDef = TypeDefDecl ["T"] [Equal (TVVar "T") (TVCon "int")]
      newChecker = addType "MyType" typeDef checker
      
      -- Test type lookup
      foundType = lookupTypeDef "MyType" newChecker
      missingType = lookupTypeDef "MissingType" newChecker
  
  foundType @?= Just typeDef
  missingType @?= Nothing

test_constraint_operations :: IO ()
test_constraint_operations = do
  let checker = newDependentTypeChecker
      constraint1 = Equal (TVVar "a") (TVCon "int")
      constraint2 = Subtype (TVVar "b") (TVVar "a")
      
      checker1 = addConstraint constraint1 checker
      checker2 = addConstraint constraint2 checker1
      
      env1 = dtcTypeEnv checker1
      env2 = dtcTypeEnv checker2
      
      constraints1 = pendingConstraints env1
      constraints2 = pendingConstraints env2
  
  L.length constraints1 @?= 1
  L.length constraints2 @?= 2
  L.head constraints1 @?= constraint1
  last constraints2 @?= constraint2

test_complex_type_scenarios :: IO ()
test_complex_type_scenarios = do
  -- Test complex function types
  let funcType = TVFun 
        [ TVCon "int", TVApp "list" [TVVar "a"] ]
        ( TVApp "option" [TVVar "a"] )
      
      -- Test complex constraints
      complexConstraint = Predicate "monad" 
        [ TVApp "list" [TVVar "a"], TVVar "a" ]
      
      -- Test complex type definition
      complexTypeDef = TypeDefDecl
        [ "a", "b" ]
        [ Equal (TVVar "a") (TVCon "int")
        , Subtype (TVVar "b") (TVVar "a")
        , TypeSizeGE (TVApp "list" [TVVar "b"]) 1
        ]
      
      -- Test type environment with multiple types
      checker1 = newDependentTypeChecker
      checker2 = addType "ComplexType" complexTypeDef checker1
      checker3 = addConstraint complexConstraint checker2
  
  -- Verify complex type properties
  show funcType @?= "TVFun [TVCon \"int\",TVApp \"list\" [TVVar \"a\"]] (TVApp \"option\" [TVVar \"a\"])"
  
  -- Verify constraint properties
  show complexConstraint @?= "Predicate \"monad\" [TVApp \"list\" [TVVar \"a\"],TVVar \"a\"]"
  
  -- Verify type definition properties
  L.length (tdParams complexTypeDef) @?= 2
  L.length (tdConstraints complexTypeDef) @?= 3
  
  -- Verify environment operations
  let env3 = dtcTypeEnv checker3
  L.length (Map.toList (typeDefinitions env3)) @?= 1
  L.length (pendingConstraints env3) @?= 1

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Dependencies Core Properties Tests"
  [ -- TypeVar properties
    testProperty "TypeVar equality reflexive" prop_type_var_equality_reflexive
  , testProperty "TypeVar equality symmetric" prop_type_var_equality_symmetric
  , testProperty "TVCon equality" prop_tv_con_equality
  , testProperty "TVVar equality" prop_tv_var_equality
  , testProperty "TypeVar ordering consistency" prop_type_var_ordering_consistency
  
  -- TypeConstraint properties
  , testProperty "TypeConstraint equality reflexive" prop_type_constraint_equality_reflexive
  , testProperty "TypeConstraint equality symmetric" prop_type_constraint_equality_symmetric
  , testProperty "Equal constraint equality" prop_equal_constraint_equality
  , testProperty "TypeRange valid bounds" prop_type_range_valid_bounds
  
  -- DependentTypeError properties
  , testProperty "DependentTypeError equality reflexive" prop_dependent_type_error_equality_reflexive
  , testProperty "DependentTypeError equality symmetric" prop_dependent_type_error_equality_symmetric
  , testProperty "TypeNotFound equality" prop_type_not_found_equality
  
  -- TypeDef properties
  , testProperty "TypeDef equality reflexive" prop_type_def_equality_reflexive
  , testProperty "TypeDef equality symmetric" prop_type_def_equality_symmetric
  , testProperty "TypeDef structural equality" prop_type_def_structural_equality
  
  -- TypeEnv properties
  , testProperty "TypeEnv equality reflexive" prop_type_env_equality_reflexive
  , testProperty "Add type preservation" prop_add_type_preservation
  
  -- DependentTypeChecker properties
  , testProperty "New checker no errors" prop_new_checker_no_errors
  , testProperty "Add constraint increases count" prop_add_constraint_increases_count
  
  -- Unit tests
  , testCase "TypeVar creation" test_type_var_creation
  , testCase "TypeConstraint creation" test_type_constraint_creation
  , testCase "DependentTypeError creation" test_dependent_type_error_creation
  , testCase "Type environment operations" test_type_environment_operations
  , testCase "Constraint operations" test_constraint_operations
  , testCase "Complex type scenarios" test_complex_type_scenarios
  ]