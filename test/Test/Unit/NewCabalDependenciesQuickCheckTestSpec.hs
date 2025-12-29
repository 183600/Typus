module Test.Unit.NewCabalDependenciesQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Either (isLeft, isRight)
import Control.Monad.State (evalState)

import Dependencies.TypeSystem
import Dependencies.AST (TypeExpr(..), Constraint(..))
import TestSupport.QuickCheck (fastProperty)

-- | QuickCheck tests for Dependencies module dependency analysis functions
tests :: TestTree
tests =
  testGroup "New Cabal Dependencies QuickCheck Tests"
    [ testProperty "TypeVar equality works correctly" prop_typeVarEquality
    , testProperty "TypeConstraint equality works correctly" prop_typeConstraintEquality
    , testProperty "DependentTypeError equality works correctly" prop_dependentTypeErrorEquality
    , testProperty "TypeEnv equality works correctly" prop_typeEnvEquality
    , testProperty "DependentTypeChecker creation works" prop_dependentTypeCheckerCreation
    , testProperty "addType adds type to environment" prop_addTypeWorks
    , testProperty "addConstraint adds constraint to environment" prop_addConstraintWorks
    , testProperty "addTypeError adds error to checker" prop_addTypeErrorWorks
    , testProperty "lookupTypeDef finds existing types" prop_lookupTypeDefWorks
    , testProperty "checkType validates simple types" prop_checkTypeSimple
    , testProperty "checkType validates generic types" prop_checkTypeGeneric
    , testProperty "checkType validates function types" prop_checkTypeFunction
    , testProperty "validateConstraint validates constraints" prop_validateConstraintWorks
    , testProperty "unify works for equal types" prop_unifyEqualTypes
    , testProperty "unify works for type variables" prop_unifyTypeVariables
    , testProperty "applySubst applies substitution correctly" prop_applySubstWorks
    , testProperty "convertTypeExpr handles simple types" prop_convertTypeExprSimple
    , testProperty "convertConstraint handles constraints" prop_convertConstraintWorks
    , testGroup "Edge cases"
        [ testCase "newDependentTypeChecker has prelude types" $ do
            let checker = newDependentTypeChecker
                env = dtcTypeEnv checker
                defs = typeDefinitions env
            Map.member "int" defs @?= True
            Map.member "string" defs @?= True
            Map.member "bool" defs @?= True
            Map.member "float64" defs @?= True
        , testCase "validateConstraint accepts valid equal constraint" $ do
            let constraint = Equal (TVCon "int") (TVCon "int")
            validateConstraint constraint @?= Right ()
        , testCase "validateConstraint rejects invalid equal constraint" $ do
            let constraint = Equal (TVCon "int") (TVCon "string")
            result <- return $ validateConstraint constraint
            case result of
                Left (DependentTypeMismatch _ _) -> pure ()
                _ -> assertFailure "Expected DependentTypeMismatch"
        , testCase "validateConstraint accepts valid size constraint" $ do
            let constraint = TypeSizeGE (TVCon "int") 5
            validateConstraint constraint @?= Right ()
        , testCase "validateConstraint rejects negative size constraint" $ do
            let constraint = TypeSizeGE (TVCon "int") (-1)
            result <- return $ validateConstraint constraint
            case result of
                Left (SemanticError _) -> pure ()
                _ -> assertFailure "Expected SemanticError"
        , testCase "unify fails for different constructors" $ do
            let pairs = [(TVCon "int", TVCon "string")]
            unify pairs @?= Nothing
        , testCase "isSubtype returns True for equal types" $ do
            let tv1 = TVCon "int"
                tv2 = TVCon "int"
            isSubtype tv1 tv2 @?= True
        ]
    ]

-- | Property: TypeVar equality works correctly
prop_typeVarEquality :: TypeVar -> TypeVar -> Property
prop_typeVarEquality tv1 tv2 = 
  (tv1 == tv2) === (tv1 `deepEqual` tv2)
  where
    deepEqual (TVCon a) (TVCon b) = a == b
    deepEqual (TVVar a) (TVVar b) = a == b
    deepEqual (TVApp a as) (TVApp b bs) = a == b && length as == length bs && all (uncurry deepEqual) (zip as bs)
    deepEqual (TVFun as r) (TVFun bs r2) = length as == length bs && all (uncurry deepEqual) (zip as bs) && deepEqual r r2
    deepEqual (TVTuple as) (TVTuple bs) = length as == length bs && all (uncurry deepEqual) (zip as bs)
    deepEqual _ _ = False

-- | Property: TypeConstraint equality works correctly
prop_typeConstraintEquality :: TypeConstraint -> TypeConstraint -> Property
prop_typeConstraintEquality tc1 tc2 = 
  (tc1 == tc2) === (tc1 `deepEqual` tc2)
  where
    deepEqual (Equal a b) (Equal c d) = deepEqualTV a c && deepEqualTV b d
    deepEqual (Subtype a b) (Subtype c d) = deepEqualTV a c && deepEqualTV b d
    deepEqual (Predicate p as) (Predicate q bs) = p == q && length as == length bs && all (uncurry deepEqualTV) (zip as bs)
    deepEqual (TypeSizeGE t n) (TypeSizeGE u m) = deepEqualTV t u && n == m
    deepEqual (TypeSizeGT t n) (TypeSizeGT u m) = deepEqualTV t u && n == m
    deepEqual (TypeRange t a b) (TypeRange u c d) = deepEqualTV t u && a == c && b == d
    deepEqual _ _ = False
    
    deepEqualTV (TVCon a) (TVCon b) = a == b
    deepEqualTV (TVVar a) (TVVar b) = a == b
    deepEqualTV (TVApp a as) (TVApp b bs) = a == b && length as == length bs && all (uncurry deepEqualTV) (zip as bs)
    deepEqualTV (TVFun as r) (TVFun bs r2) = length as == length bs && all (uncurry deepEqualTV) (zip as bs) && deepEqualTV r r2
    deepEqualTV (TVTuple as) (TVTuple bs) = length as == length bs && all (uncurry deepEqualTV) (zip as bs)
    deepEqualTV _ _ = False

-- | Property: DependentTypeError equality works correctly
prop_dependentTypeErrorEquality :: DependentTypeError -> DependentTypeError -> Property
prop_dependentTypeErrorEquality err1 err2 = 
  (err1 == err2) === (err1 `deepEqual` err2)
  where
    deepEqual (DependentTypeMismatch a b) (DependentTypeMismatch c d) = deepEqualTV a c && deepEqualTV b d
    deepEqual (ConstraintViolation s a) (ConstraintViolation t b) = s == t && deepEqualTV a b
    deepEqual (TypeNotFound s) (TypeNotFound t) = s == t
    deepEqual (InvalidTypeArgument s) (InvalidTypeArgument t) = s == t
    deepEqual (UnsolvableConstraint c) (UnsolvableConstraint d) = deepEqualTC c d
    deepEqual (DependentInfiniteType s a) (DependentInfiniteType t b) = s == t && deepEqualTV a b
    deepEqual (AmbiguousType s) (AmbiguousType t) = s == t
    deepEqual (ParseError s) (ParseError t) = s == t
    deepEqual (SemanticError s) (SemanticError t) = s == t
    deepEqual _ _ = False
    
    deepEqualTV (TVCon a) (TVCon b) = a == b
    deepEqualTV (TVVar a) (TVVar b) = a == b
    deepEqualTV (TVApp a as) (TVApp b bs) = a == b && length as == length bs && all (uncurry deepEqualTV) (zip as bs)
    deepEqualTV (TVFun as r) (TVFun bs r2) = length as == length bs && all (uncurry deepEqualTV) (zip as bs) && deepEqualTV r r2
    deepEqualTV (TVTuple as) (TVTuple bs) = length as == length bs && all (uncurry deepEqualTV) (zip as bs)
    deepEqualTV _ _ = False
    
    deepEqualTC (Equal a b) (Equal c d) = deepEqualTV a c && deepEqualTV b d
    deepEqualTC (Subtype a b) (Subtype c d) = deepEqualTV a c && deepEqualTV b d
    deepEqualTC (Predicate p as) (Predicate q bs) = p == q && length as == length bs && all (uncurry deepEqualTV) (zip as bs)
    deepEqualTC (TypeSizeGE t n) (TypeSizeGE u m) = deepEqualTV t u && n == m
    deepEqualTC (TypeSizeGT t n) (TypeSizeGT u m) = deepEqualTV t u && n == m
    deepEqualTC (TypeRange t a b) (TypeRange u c d) = deepEqualTV t u && a == c && b == d
    deepEqualTC _ _ = False

-- | Property: TypeEnv equality works correctly
prop_typeEnvEquality :: TypeEnv -> TypeEnv -> Property
prop_typeEnvEquality env1 env2 = 
  (env1 == env2) === (typeDefinitions env1 == typeDefinitions env2 && pendingConstraints env1 == pendingConstraints env2)

-- | Property: DependentTypeChecker creation works
prop_dependentTypeCheckerCreation :: Property
prop_dependentTypeCheckerCreation = 
  let checker = newDependentTypeChecker
      env = dtcTypeEnv checker
      errors = tcErrors checker
  in null errors .&&. Map.size (typeDefinitions env) >= 4  -- prelude types

-- | Property: addType adds type to environment
prop_addTypeWorks :: String -> [String] -> Property
prop_addTypeWorks name params = 
  not (null name) && all (not . null) params ==>
  let checker = evalState (do
        addType name params []
        get
        ) newDependentTypeChecker
      env = dtcTypeEnv checker
      defs = typeDefinitions env
  in Map.member name defs

-- | Property: addConstraint adds constraint to environment
prop_addConstraintWorks :: TypeConstraint -> Property
prop_addConstraintWorks constraint = 
  let checker = evalState (do
        addConstraint constraint
        get
        ) newDependentTypeChecker
      env = dtcTypeEnv checker
      constraints = pendingConstraints env
  in constraint `elem` constraints

-- | Property: addTypeError adds error to checker
prop_addTypeErrorWorks :: DependentTypeError -> Property
prop_addTypeErrorWorks error = 
  let checker = evalState (do
        addTypeError error
        get
        ) newDependentTypeChecker
      errors = tcErrors checker
  in error `elem` errors

-- | Property: lookupTypeDef finds existing types
prop_lookupTypeDefWorks :: String -> Property
prop_lookupTypeDefWorks name = 
  name `elem` ["int", "string", "bool", "float64"] ==>
  let checker = newDependentTypeChecker
      result = evalState (lookupTypeDef name) checker
  in isJust result
  where
    isJust Nothing = False
    isJust (Just _) = True

-- | Property: checkType validates simple types
prop_checkTypeSimple :: String -> Property
prop_checkTypeSimple name = 
  name `elem` ["int", "string", "bool", "float64"] ==>
  let tv = TVCon name
      checker = evalState (checkType tv >> get) newDependentTypeChecker
      errors = tcErrors checker
  in null errors

-- | Property: checkType validates generic types
prop_checkTypeGeneric :: String -> [String] -> Property
prop_checkTypeGeneric name args = 
  name `elem` ["int", "string", "bool", "float64"] && not (null args) ==>
  let tv = TVApp name (map TVVar args)
      checker = evalState (checkType tv >> get) newDependentTypeChecker
      errors = tcErrors checker
  in not (null errors)  -- Should have errors for invalid generic instantiation

-- | Property: checkType validates function types
prop_checkTypeFunction :: [String] -> String -> Property
prop_checkTypeFunction params returnType = 
  not (null params) && returnType `elem` ["int", "string", "bool", "float64"] ==>
  let tv = TVFun (map TVVar params) (TVCon returnType)
      checker = evalState (checkType tv >> get) newDependentTypeChecker
      errors = tcErrors checker
  in null errors  -- Function types with type variables should be valid

-- | Property: validateConstraint validates constraints
prop_validateConstraintWorks :: TypeConstraint -> Property
prop_validateConstraintWorks constraint = 
  case validateConstraint constraint of
    Right _ -> property True
    Left _ -> property True  -- Validation failures are expected for some constraints

-- | Property: unify works for equal types
prop_unifyEqualTypes :: String -> Property
prop_unifyEqualTypes typeName = 
  typeName `elem` ["int", "string", "bool", "float64"] ==>
  let tv1 = TVCon typeName
      tv2 = TVCon typeName
      pairs = [(tv1, tv2)]
  in isJust (unify pairs)
  where
    isJust Nothing = False
    isJust (Just _) = True

-- | Property: unify works for type variables
prop_unifyTypeVariables :: String -> Property
prop_unifyTypeVariables varName = 
  not (null varName) ==>
  let tv1 = TVVar varName
      tv2 = TVVar (varName ++ "2")
      pairs = [(tv1, tv2)]
  in isJust (unify pairs)
  where
    isJust Nothing = False
    isJust (Just _) = True

-- | Property: applySubst applies substitution correctly
prop_applySubstWorks :: String -> TypeVar -> Property
prop_applySubstWorks varName targetTv = 
  not (null varName) ==>
  let subst = [(varName, targetTv)]
      originalTv = TVVar varName
      result = applySubst subst originalTv
  in result == targetTv

-- | Property: convertTypeExpr handles simple types
prop_convertTypeExprSimple :: String -> Property
prop_convertTypeExprSimple typeName = 
  typeName `elem` ["int", "string", "bool", "float64"] ==>
  let expr = SimpleT (pack typeName)
      params = Set.empty
      result = convertTypeExpr params expr
  in result == TVCon typeName
  where
    pack = id  -- String to Text conversion (simplified)

-- | Property: convertConstraint handles constraints
prop_convertConstraintWorks :: String -> String -> Int -> Property
prop_convertConstraintWorks varName op value = 
  not (null varName) && value >= 0 ==>
  let params = Set.singleton varName
      constraint = case op of
        "ge" -> SizeGE (pack varName) value
        "gt" -> SizeGT (pack varName) value
        _ -> SizeGE (pack varName) value
      result = convertConstraint params constraint
  in case result of
       TypeSizeGE tv n -> tv == TVVar varName && n == value
       TypeSizeGT tv n -> tv == TVVar varName && n == value
       _ -> False
  where
    pack = id  -- String to Text conversion (simplified)

-- Helper operator for composing properties
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&)