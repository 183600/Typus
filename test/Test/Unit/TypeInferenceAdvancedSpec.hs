{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.TypeInferenceAdvancedSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (sort, (\\))
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Mock data types for advanced type inference testing
data TypeVar = TypeVar
  { typeVarName :: String
  , typeVarId :: Int
  , typeVarKind :: String
  } deriving (Show, Eq, Ord)

data TypeConstructor = TypeConstructor
  { typeConName :: String
  , typeConArity :: Int
  } deriving (Show, Eq, Ord)

data Type = TypeVarType TypeVar
          | TypeConType TypeConstructor [Type]
          | TypeFunType Type Type
          | TypeForAllType [TypeVar] Type
          deriving (Show, Eq)

data TypeScheme = TypeScheme
  { schemeVars :: [TypeVar]
  , schemeType :: Type
  } deriving (Show, Eq)

data Constraint = EqualityConstraint Type Type
                | TypeClassConstraint String Type
                deriving (Show, Eq)

data Substitution = Substitution
  { substitutionMap :: Map.Map TypeVar Type
  } deriving (Show, Eq)

data InferenceState = InferenceState
  { stateSubstitution :: Substitution
  , stateConstraints :: [Constraint]
  , stateNextVarId :: Int
  } deriving (Show, Eq)

data InferenceResult = InferenceResult
  { resultType :: Type
  , resultState :: InferenceState
  , resultErrors :: [String]
  } deriving (Show, Eq)

-- Mock type inference functions
emptySubstitution :: Substitution
emptySubstitution = Substitution Map.empty

composeSubstitutions :: Substitution -> Substitution -> Substitution
composeSubstitutions s1 s2 = 
  let map1 = substitutionMap s1
      map2 = substitutionMap s2
      composed = Map.union map1 map2
  in Substitution composed

applySubstitution :: Substitution -> Type -> Type
applySubstitution sub (TypeVarType var) = 
  case Map.lookup var (substitutionMap sub) of
    Just typ -> typ
    Nothing -> TypeVarType var
applySubstitution sub (TypeConType con args) = 
  TypeConType con (map (applySubstitution sub) args)
applySubstitution sub (TypeFunType argType resType) = 
  TypeFunType (applySubstitution sub argType) (applySubstitution sub resType)
applySubstitution sub (TypeForAllType vars typ) = 
  TypeForAllType vars (applySubstitution sub typ)

unifyTypes :: Type -> Type -> InferenceState -> Either String InferenceState
unifyTypes type1 type2 state = 
  case (type1, type2) of
    (TypeVarType var, typ) -> 
      Right state { stateSubstitution = Substitution (Map.insert var typ (substitutionMap $ stateSubstitution state)) }
    (typ, TypeVarType var) -> 
      Right state { stateSubstitution = Substitution (Map.insert var typ (substitutionMap $ stateSubstitution state)) }
    (TypeConType con1 args1, TypeConType con2 args2) | con1 == con2 -> 
      foldr (\(arg1, arg2) accState -> 
        case accState of
          Left err -> Left err
          Right s -> unifyTypes arg1 arg2 s
      ) (Right state) (zip args1 args2)
    (TypeFunType arg1 res1, TypeFunType arg2 res2) -> do
      state1 <- unifyTypes arg1 arg2 state
      unifyTypes res1 res2 state1
    (TypeForAllType _ typ1, TypeForAllType _ typ2) -> 
      unifyTypes typ1 typ2 state
    _ -> Left "Cannot unify types"

instantiateType :: TypeScheme -> InferenceState -> (Type, InferenceState)
instantiateType scheme state = 
  let vars = schemeVars scheme
      typ = schemeType scheme
      freshVars = [TypeVarType (TypeVar (typeVarName var) (stateNextVarId state + i) (typeVarKind var)) | (var, i) <- zip vars [0..]]
      substitution = Substitution (Map.fromList (zip vars freshVars))
      newTyp = applySubstitution substitution typ
      newState = state { stateNextVarId = stateNextVarId state + length vars }
  in (newTyp, newState)

generalizeType :: Type -> InferenceState -> TypeScheme
generalizeType typ state = 
  let freeVars = extractFreeVars typ
      substitution = stateSubstitution state
      constrainedVars = Map.keys (substitutionMap substitution)
      quantifiedVars = freeVars \\ constrainedVars
  in TypeScheme quantifiedVars typ
  where
    extractFreeVars (TypeVarType var) = [var]
    extractFreeVars (TypeConType _ args) = concatMap extractFreeVars args
    extractFreeVars (TypeFunType argType resType) = 
      extractFreeVars argType ++ extractFreeVars resType
    extractFreeVars (TypeForAllType vars typ) = 
      extractFreeVars typ \\ vars

tests :: TestTree
tests = testGroup "Advanced Type Inference Tests"
  [ testGroup "Type variables"
    [ testCase "creates type variables correctly" $ do
        let var = TypeVar "T" 1 "Type"
        typeVarName var @?= "T"
        typeVarId var @?= 1
        typeVarKind var @?= "Type"
      
    , testCase "compares type variables correctly" $ do
        let var1 = TypeVar "T" 1 "Type"
            var2 = TypeVar "T" 2 "Type"
            var3 = TypeVar "U" 1 "Type"
        var1 @?= var1
        assertBool "var1 should not equal var2" (var1 /= var2)
        assertBool "var1 should not equal var3" (var1 /= var3)
      
    , testCase "orders type variables correctly" $ do
        let var1 = TypeVar "T" 1 "Type"
            var2 = TypeVar "U" 2 "Type"
            var3 = TypeVar "T" 2 "Type"
        sort [var2, var1, var3] @?= [var1, var3, var2]
    ]
  
  , testGroup "Type constructors"
    [ testCase "creates type constructors correctly" $ do
        let con = TypeConstructor "List" 1
        typeConName con @?= "List"
        typeConArity con @?= 1
      
    , testCase "compares type constructors correctly" $ do
        let con1 = TypeConstructor "List" 1
            con2 = TypeConstructor "List" 1
            con3 = TypeConstructor "Maybe" 1
        con1 @?= con2
        assertBool "con1 should not equal con3" (con1 /= con3)
    ]

  , testGroup "Types"
    [ testCase "creates variable types correctly" $ do
        let var = TypeVar "T" 1 "Type"
            typ = TypeVarType var
        case typ of
          TypeVarType v -> v @?= var
          _ -> assertFailure "Expected TypeVarType"
        
    , testCase "creates constructor types correctly" $ do
        let con = TypeConstructor "List" 1
            argType = TypeVarType (TypeVar "T" 1 "Type")
            typ = TypeConType con [argType]
        case typ of
          TypeConType c args -> do
            c @?= con
            args @?= [argType]
          _ -> assertFailure "Expected TypeConType"
        
    , testCase "creates function types correctly" $ do
        let argType = TypeVarType (TypeVar "T" 1 "Type")
            resType = TypeVarType (TypeVar "U" 2 "Type")
            typ = TypeFunType argType resType
        case typ of
          TypeFunType arg res -> do
            arg @?= argType
            res @?= resType
          _ -> assertFailure "Expected TypeFunType"
        
    , testCase "creates polymorphic types correctly" $ do
        let var = TypeVar "T" 1 "Type"
            innerType = TypeVarType var
            typ = TypeForAllType [var] innerType
        case typ of
          TypeForAllType vars t -> do
            vars @?= [var]
            t @?= innerType
          _ -> assertFailure "Expected TypeForAllType"
    ]

  , testGroup "Type schemes"
    [ testCase "creates type schemes correctly" $ do
        let var = TypeVar "T" 1 "Type"
            typ = TypeVarType var
            scheme = TypeScheme [var] typ
        schemeVars scheme @?= [var]
        schemeType scheme @?= typ
      
    , testCase "handles empty type schemes" $ do
        let typ = TypeConType (TypeConstructor "Int" 0) []
            scheme = TypeScheme [] typ
        schemeVars scheme @?= []
        schemeType scheme @?= typ
    ]

  , testGroup "Constraints"
    [ testCase "creates equality constraints correctly" $ do
        let var = TypeVar "T" 1 "Type"
            typ1 = TypeVarType var
            typ2 = TypeConType (TypeConstructor "Int" 0) []
            constraint = EqualityConstraint typ1 typ2
        case constraint of
          EqualityConstraint t1 t2 -> do
            t1 @?= typ1
            t2 @?= typ2
          _ -> assertFailure "Expected EqualityConstraint"
        
    , testCase "creates type class constraints correctly" $ do
        let var = TypeVar "T" 1 "Type"
            typ = TypeVarType var
            constraint = TypeClassConstraint "Eq" typ
        case constraint of
          TypeClassConstraint className t -> do
            className @?= "Eq"
            t @?= typ
          _ -> assertFailure "Expected TypeClassConstraint"
    ]

  , testGroup "Substitutions"
    [ testCase "creates empty substitution" $ do
        let sub = emptySubstitution
        Map.size (substitutionMap sub) @?= 0
      
    , testCase "composes substitutions correctly" $ do
        let var1 = TypeVar "T" 1 "Type"
            var2 = TypeVar "U" 2 "Type"
            typ1 = TypeConType (TypeConstructor "Int" 0) []
            typ2 = TypeConType (TypeConstructor "String" 0) []
            sub1 = Substitution (Map.fromList [(var1, typ1)])
            sub2 = Substitution (Map.fromList [(var2, typ2)])
            composed = composeSubstitutions sub1 sub2
        Map.size (substitutionMap composed) @?= 2
      
    , testCase "applies substitutions to variable types" $ do
        let var = TypeVar "T" 1 "Type"
            typ1 = TypeVarType var
            typ2 = TypeConType (TypeConstructor "Int" 0) []
            sub = Substitution (Map.fromList [(var, typ2)])
            result = applySubstitution sub typ1
        result @?= typ2
      
    , testCase "applies substitutions to constructor types" $ do
        let var = TypeVar "T" 1 "Type"
            argType = TypeVarType var
            typ1 = TypeConType (TypeConstructor "List" 1) [argType]
            typ2 = TypeConType (TypeConstructor "Int" 0) []
            sub = Substitution (Map.fromList [(var, typ2)])
            result = applySubstitution sub typ1
        case result of
          TypeConType con args -> do
            con @?= TypeConstructor "List" 1
            args @?= [typ2]
          _ -> assertFailure "Expected TypeConType"
    ]

  , testGroup "Type unification"
    [ testCase "unifies variable with type" $ do
        let var = TypeVar "T" 1 "Type"
            typ1 = TypeVarType var
            typ2 = TypeConType (TypeConstructor "Int" 0) []
            state = InferenceState emptySubstitution [] 1
            result = unifyTypes typ1 typ2 state
        case result of
          Right newState -> do
            let sub = stateSubstitution newState
            Map.lookup var (substitutionMap sub) @?= Just typ2
          Left _ -> assertFailure "Expected successful unification"
        
    , testCase "unifies identical constructor types" $ do
        let typ1 = TypeConType (TypeConstructor "Int" 0) []
            typ2 = TypeConType (TypeConstructor "Int" 0) []
            state = InferenceState emptySubstitution [] 1
            result = unifyTypes typ1 typ2 state
        case result of
          Right _ -> return ()
          Left err -> assertFailure $ "Expected successful unification, got: " ++ err
        
    , testCase "fails to unify different constructor types" $ do
        let typ1 = TypeConType (TypeConstructor "Int" 0) []
            typ2 = TypeConType (TypeConstructor "String" 0) []
            state = InferenceState emptySubstitution [] 1
            result = unifyTypes typ1 typ2 state
        case result of
          Right _ -> assertFailure "Expected unification to fail"
          Left _ -> return ()
        
    , testCase "unifies function types" $ do
        let var1 = TypeVar "T" 1 "Type"
            var2 = TypeVar "U" 2 "Type"
            typ1 = TypeFunType (TypeVarType var1) (TypeVarType var2)
            typ2 = TypeFunType (TypeConType (TypeConstructor "Int" 0) []) 
                              (TypeConType (TypeConstructor "String" 0) [])
            state = InferenceState emptySubstitution [] 1
            result = unifyTypes typ1 typ2 state
        case result of
          Right newState -> do
            let sub = stateSubstitution newState
            Map.lookup var1 (substitutionMap sub) @?= Just (TypeConType (TypeConstructor "Int" 0) [])
            Map.lookup var2 (substitutionMap sub) @?= Just (TypeConType (TypeConstructor "String" 0) [])
          Left _ -> assertFailure "Expected successful unification"
    ]

  , testGroup "Type instantiation and generalization"
    [ testCase "instantiates type schemes correctly" $ do
        let var = TypeVar "T" 1 "Type"
            typ = TypeVarType var
            scheme = TypeScheme [var] typ
            state = InferenceState emptySubstitution [] 1
            (instantiatedType, newState) = instantiateType scheme state
        case instantiatedType of
          TypeVarType newVar -> do
            typeVarName newVar @?= "T"
            typeVarId newVar @?= 1
            typeVarKind newVar @?= "Type"
          _ -> assertFailure "Expected TypeVarType"
        stateNextVarId newState @?= 2
      
    , testCase "generalizes types correctly" $ do
        let var = TypeVar "T" 1 "Type"
            typ = TypeVarType var
            state = InferenceState emptySubstitution [] 1
            scheme = generalizeType typ state
        schemeVars scheme @?= [var]
        schemeType scheme @?= typ
      
    , testCase "generalizes types with substitutions" $ do
        let var1 = TypeVar "T" 1 "Type"
            typ1 = TypeVarType var1
            typ2 = TypeConType (TypeConstructor "Int" 0) []
            sub = Substitution (Map.fromList [(var1, typ2)])
            state = InferenceState sub [] 1
            scheme = generalizeType typ1 state
        schemeVars scheme @?= []
        schemeType scheme @?= typ1
    ]

  , testGroup "Inference state"
    [ testCase "creates inference state correctly" $ do
        let state = InferenceState emptySubstitution [] 1
        stateSubstitution state @?= emptySubstitution
        stateConstraints state @?= []
        stateNextVarId state @?= 1
      
    , testCase "updates next variable ID" $ do
        let state = InferenceState emptySubstitution [] 1
            newState = state { stateNextVarId = 5 }
        stateNextVarId newState @?= 5
    ]

  , testGroup "QuickCheck properties"
    [ testCase "substitution composition is associative" $ do
        let sub1 = emptySubstitution
            sub2 = emptySubstitution  
            sub3 = emptySubstitution
            composed1 = composeSubstitutions sub1 (composeSubstitutions sub2 sub3)
            composed2 = composeSubstitutions (composeSubstitutions sub1 sub2) sub3
        substitutionMap composed1 @?= substitutionMap composed2
        
    , testCase "substitution application is idempotent for ground types" $ do
        let sub = emptySubstitution
            typ = TypeConType (TypeConstructor "Int" 0) []
            result1 = applySubstitution sub typ
            result2 = applySubstitution sub result1
        result1 @?= result2
           
    , testCase "unification is symmetric" $ do
        let typ1 = TypeConType (TypeConstructor "Int" 0) []
            typ2 = TypeConType (TypeConstructor "String" 0) []
            state = InferenceState emptySubstitution [] 1
            result1 = unifyTypes typ1 typ2 state
            result2 = unifyTypes typ2 typ1 state
        case (result1, result2) of
              (Right _, Right _) -> assertBool "Both should succeed" True
              (Left _, Left _) -> assertBool "Both should fail" True
              _ -> assertFailure "Results should be consistent"
    ]

  , testGroup "Edge cases"
    [ testCase "handles empty types" $ do
        let typ = TypeConType (TypeConstructor "Unit" 0) []
            state = InferenceState emptySubstitution [] 1
            result = unifyTypes typ typ state
        case result of
          Right _ -> return ()
          Left _ -> assertFailure "Expected successful unification"
        
    , testCase "handles recursive types" $ do
        let var = TypeVar "T" 1 "Type"
            typ1 = TypeVarType var
            typ2 = TypeConType (TypeConstructor "List" 1) [typ1]
            state = InferenceState emptySubstitution [] 1
            result = unifyTypes typ1 typ2 state
        case result of
          Right newState -> do
            let sub = stateSubstitution newState
            Map.lookup var (substitutionMap sub) @?= Just typ2
          Left _ -> assertFailure "Expected successful unification"
        
    , testCase "handles complex function types" $ do
        let var1 = TypeVar "T" 1 "Type"
            var2 = TypeVar "U" 2 "Type"
            var3 = TypeVar "V" 3 "Type"
            innerFunc = TypeFunType (TypeVarType var1) (TypeVarType var2)
            outerFunc = TypeFunType innerFunc (TypeVarType var3)
            state = InferenceState emptySubstitution [] 1
            result = unifyTypes outerFunc outerFunc state
        case result of
          Right _ -> return ()
          Left _ -> assertFailure "Expected successful unification"
        
    , testCase "handles large type environments" $ do
        let vars = [TypeVar ("T" ++ show i) i "Type" | i <- [1..50]]
            types = [TypeVarType var | var <- vars]
            state = InferenceState emptySubstitution [] 1
            result = foldr (\typ acc -> case acc of
                                           Left err -> Left err
                                           Right s -> unifyTypes typ typ s) (Right state) types
        case result of
          Right _ -> return ()
          Left _ -> assertFailure "Expected successful unification"
    ]
  ]