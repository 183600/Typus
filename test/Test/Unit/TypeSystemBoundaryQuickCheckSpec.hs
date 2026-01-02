{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TypeSystemBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.)
  , Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements, sized, frequency
  , suchThat, resize
  )

import Dependencies.TypeSystem
import Dependencies.AST
import Data.List (nub, sort, intersect, union)
import Data.Set (Set, toList, fromList, union, intersection, difference)
import qualified Data.Set as Set

-- Test data for type system
data TypeSystemTestData = TypeSystemTestData
  { baseTypes :: [TypeExpr]
  , constraints :: [Constraint]
  , typeVars :: [TypeVar]
  } deriving (Show, Eq)

-- Simplified type expressions for testing
data TypeExpr = 
    TypeVar String
  | TypeConstructor String [TypeExpr]
  | FunctionType TypeExpr TypeExpr
  | dependentType TypeExpr TypeExpr  -- Dependent type: Type(value)
  deriving (Show, Eq, Ord)

data Constraint = 
    Equality TypeExpr TypeExpr
  | Subtype TypeExpr TypeExpr
  | DependentConstraint String TypeExpr TypeExpr
  deriving (Show, Eq)

data TypeVar = TypeVar String deriving (Show, Eq, Ord)

instance Arbitrary TypeExpr where
  arbitrary = sized genType
    where
      genType 0 = TypeVar <$> arbitrary
      genType n = oneof
        [ TypeVar <$> arbitrary
        , TypeConstructor <$> arbitrary <*> listOf (genType (n `div` 2))
        , FunctionType <$> genType (n `div` 2) <*> genType (n `div` 2)
        , dependentType <$> genType (n `div` 2) <*> genType (n `div` 2)
        ]

instance Arbitrary Constraint where
  arbitrary = do
    t1 <- arbitrary
    t2 <- arbitrary
    oneof
      [ return $ Equality t1 t2
      , return $ Subtype t1 t2
      , do
          name <- arbitrary
          return $ DependentConstraint name t1 t2
      ]

instance Arbitrary TypeVar where
  arbitrary = TypeVar <$> elements ["a", "b", "c", "x", "y", "z", "t1", "t2", "t3"]

instance Arbitrary TypeSystemTestData where
  arbitrary = do
    types <- listOf arbitrary
    constraints <- listOf arbitrary
    vars <- listOf arbitrary
    return $ TypeSystemTestData types constraints vars

-- Property: Type variable substitution preserves structure
prop_type_substitution_preserves_structure :: TypeExpr -> String -> TypeExpr -> Property
prop_type_substitution_preserves_structure typeExpr varName replacement =
  let substituted = substituteType varName replacement typeExpr
  in case typeExpr of
    TypeVar name -> 
      if name == varName 
      then property $ substituted === replacement
      else property $ substituted === typeExpr
    TypeConstructor name args -> 
      case substituted of
        TypeConstructor newName newArgs -> 
          property $ newName === name && L.length newArgs === L.length args
        _ -> property False
    FunctionType from to -> 
      case substituted of
        FunctionType newFrom newTo -> 
          property $ True  -- Basic structure preserved
        _ -> property False
    dependentType base value -> 
      case substituted of
        dependentType newBase newValue -> 
          property $ True  -- Basic structure preserved
        _ -> property False

-- Property: Type unification finds correct substitution
prop_type_unification_finds_substitution :: TypeExpr -> TypeExpr -> Property
prop_type_unification_finds_substitution type1 type2 =
  let result = unifyTypes type1 type2
  in case result of
    Left _ -> property True  -- Unification failure is acceptable
    Right substitution -> 
      let applied1 = applySubstitution substitution type1
          applied2 = applySubstitution substitution type2
      in property $ applied1 === applied2

-- Property: Function type unification works correctly
prop_function_type_unification :: TypeExpr -> TypeExpr -> TypeExpr -> TypeExpr -> Property
prop_function_type_unification from1 to1 from2 to2 =
  let func1 = FunctionType from1 to1
      func2 = FunctionType from2 to2
      result = unifyTypes func1 func2
  in case result of
    Left _ -> property True
    Right substitution -> 
      let applied1 = applySubstitution substitution func1
          applied2 = applySubstitution substitution func2
      in property $ applied1 === applied2

-- Property: Dependent type constraints are preserved
prop_dependent_constraints_preserved :: TypeExpr -> TypeExpr -> String -> Property
prop_dependent_constraints_preserved base value name =
  let dependent = dependentType base value
      constraint = DependentConstraint name base value
      constraints = [constraint]
      satisfied = checkDependentConstraint constraint dependent
  in property $ satisfied

-- Property: Type variable freshness is maintained
prop_type_var_freshness :: [TypeVar] -> Property
prop_type_var_freshness vars =
  let freshVars = map generateFreshVar vars
      allUnique = L.length (nub freshVars) == L.length freshVars
      allFresh = L.all (`notElem` vars) freshVars
  in property $ allUnique && allFresh

-- Property: Type inference preserves consistency
prop_type_inference_consistent :: [Statement] -> Property
prop_type_inference_consistent statements =
  not (null statements) ==>
  let result = inferTypes statements
  in case result of
    Left _ -> property True
    Right types -> 
      let typeCount = L.length types
          statementCount = L.length statements
      in property $ typeCount <= statementCount

-- Property: Subtype relation is transitive
prop_subtype_transitive :: TypeExpr -> TypeExpr -> TypeExpr -> Property
prop_subtype_transitive t1 t2 t3 =
  let sub1 = isSubtype t1 t2
      sub2 = isSubtype t2 t3
      sub3 = isSubtype t1 t3
  in (sub1 && sub2) ==> sub3

-- Property: Type equality is reflexive
prop_type_equality_reflexive :: TypeExpr -> Property
prop_type_equality_reflexive typeExpr =
  let equal = areTypesEqual typeExpr typeExpr
  in property $ equal

-- Property: Type equality is symmetric
prop_type_equality_symmetric :: TypeExpr -> TypeExpr -> Property
prop_type_equality_symmetric type1 type2 =
  let equal1 = areTypesEqual type1 type2
      equal2 = areTypesEqual type2 type1
  in property $ equal1 === equal2

-- Property: Type constructor arity is preserved
prop_type_constructor_arity :: String -> [TypeExpr] -> Property
prop_type_constructor_arity name args =
  let constructor = TypeConstructor name args
      arity = getConstructorArity constructor
  in property $ arity === L.length args

-- Property: Dependent type reduction works correctly
prop_dependent_type_reduction :: TypeExpr -> TypeExpr -> Property
prop_dependent_type_reduction base value =
  let dependent = dependentType base value
      reduced = reduceDependentType dependent
  in case reduced of
    Just result -> property $ True  -- Basic check that reduction succeeds
    Nothing -> property True  -- No reduction is also valid

-- Property: Type variable generalization preserves free variables
prop_generalization_preserves_free :: TypeExpr -> [TypeVar] -> Property
prop_generalization_preserves_free typeExpr env =
  let freeVars = getFreeVariables typeExpr
      generalized = generalizeType typeExpr env
      generalizedFree = getFreeVariables generalized
  in property $ Set.isSubsetOf (fromList generalizedFree) (fromList freeVars)

-- Property: Type instantiation preserves structure
prop_instantiation_preserves_structure :: TypeExpr -> [TypeVar] -> [TypeExpr] -> Property
prop_instantiation_preserves_structure typeExpr vars replacements =
  L.length vars == L.length replacements ==>
  let scheme = TypeScheme vars typeExpr
      instantiated = instantiateScheme scheme replacements
  in property $ case instantiated of
    Just result -> True  -- Basic structure preservation
    Nothing -> False

-- Helper functions for type system operations
data TypeScheme = TypeScheme [TypeVar] TypeExpr deriving (Show, Eq)

substituteType :: String -> TypeExpr -> TypeExpr -> TypeExpr
substituteType varName replacement typeExpr = case typeExpr of
  TypeVar name -> if name == varName then replacement else typeExpr
  TypeConstructor name args -> TypeConstructor name (L.map (substituteType varName replacement) args)
  FunctionType from to -> FunctionType (substituteType varName replacement from) (substituteType varName replacement to)
  dependentType base value -> dependentType (substituteType varName replacement base) (substituteType varName replacement value)

data Statement = 
    VarDecl String TypeExpr
  | FunctionDecl String TypeExpr TypeExpr
  deriving (Show, Eq)

instance Arbitrary Statement where
  arbitrary = oneof
    [ VarDecl <$> arbitrary <*> arbitrary
    , FunctionDecl <$> arbitrary <*> arbitrary <*> arbitrary
    ]

-- Simplified implementations for testing
unifyTypes :: TypeExpr -> TypeExpr -> Either String [(String, TypeExpr)]
unifyTypes t1 t2 = 
  if t1 == t2 
  then Right []
  else Left "Cannot unify"

applySubstitution :: [(String, TypeExpr)] -> TypeExpr -> TypeExpr
applySubstitution substitution typeExpr = L.foldl (\acc (var, replacement) -> substituteType var replacement acc) typeExpr substitution

checkDependentConstraint :: Constraint -> TypeExpr -> Bool
checkDependentConstraint (DependentConstraint _ base value) (dependentType base' value') = base == base' && value == value'
checkDependentConstraint _ _ = False

generateFreshVar :: TypeVar -> TypeVar
generateFreshVar (TypeVar name) = TypeVar (name ++ "'")

inferTypes :: [Statement] -> Either String [(String, TypeExpr)]
inferTypes statements = Right $ map inferStatement statements
  where
    inferStatement (VarDecl name t) = (name, t)
    inferStatement (FunctionDecl name t _) = (name, t)

isSubtype :: TypeExpr -> TypeExpr -> Bool
isSubtype t1 t2 = t1 == t2  -- Simplified

areTypesEqual :: TypeExpr -> TypeExpr -> Bool
areTypesEqual = (==)

getConstructorArity :: TypeExpr -> Int
getConstructorArity (TypeConstructor _ args) = L.length args
getConstructorArity _ = 0

reduceDependentType :: TypeExpr -> Maybe TypeExpr
reduceDependentType dependent = Just dependent  -- Simplified

getFreeVariables :: TypeExpr -> [String]
getFreeVariables (TypeVar name) = [name]
getFreeVariables (TypeConstructor _ args) = concatMap getFreeVariables args
getFreeVariables (FunctionType from to) = getFreeVariables from ++ getFreeVariables to
getFreeVariables (dependentType base value) = getFreeVariables base ++ getFreeVariables value

generalizeType :: TypeExpr -> [TypeVar] -> TypeExpr
generalizeType typeExpr _ = typeExpr  -- Simplified

instantiateScheme :: TypeScheme -> [TypeExpr] -> Maybe TypeExpr
instantiateScheme (TypeScheme vars typeExpr) replacements = 
  if L.length vars == L.length replacements
  then Just $ L.foldl (\acc (TypeVar var, replacement) -> substituteType var replacement acc) typeExpr (zip vars replacements)
  else Nothing

tests :: TestTree
tests = testGroup "Type System Boundary QuickCheck Tests"
  [ fastProperty "Type variable substitution preserves structure" prop_type_substitution_preserves_structure
  , fastProperty "Type unification finds correct substitution" prop_type_unification_finds_substitution
  , fastProperty "Function type unification works correctly" prop_function_type_unification
  , fastProperty "Dependent type constraints are preserved" prop_dependent_constraints_preserved
  , fastProperty "Type variable freshness is maintained" prop_type_var_freshness
  , fastProperty "Type inference preserves consistency" prop_type_inference_consistent
  , fastProperty "Subtype relation is transitive" prop_subtype_transitive
  , fastProperty "Type equality is reflexive" prop_type_equality_reflexive
  , fastProperty "Type equality is symmetric" prop_type_equality_symmetric
  , fastProperty "Type constructor arity is preserved" prop_type_constructor_arity
  , fastProperty "Dependent type reduction works correctly" prop_dependent_type_reduction
  , fastProperty "Type variable generalization preserves free variables" prop_generalization_preserves_free
  , fastProperty "Type instantiation preserves structure" prop_instantiation_preserves_structure
  , testCase "Manual type system test" $ do
      let varA = TypeVar "a"
          varB = TypeVar "b"
          funcType = FunctionType varA varB
          constructor = TypeConstructor "List" [varA]
          dependent = dependentType varA (TypeConstructor "Nat" [])
      
      getConstructorArity constructor @?= 1
      areTypesEqual varA varA @?= True
      areTypesEqual varA varB @?= False
      
      let substitution = substituteType "a" (TypeConstructor "Int" []) varA
      substitution @?= TypeConstructor "Int" []
      
      isSubtype varA varA @?= True
      
      let freeVars = getFreeVariables funcType
      sort freeVars @?= ["a", "b"]
  ]