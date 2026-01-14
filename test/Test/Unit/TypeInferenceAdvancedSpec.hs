{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.TypeInferenceAdvancedSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort, nub, intersect, union, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Set as Set
import qualified Data.Map as Map
import SourceLocation (SourcePos(..), SourceSpan(..))

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
applySubstitution sub (TypeFunType argType resultType) = 
  TypeFunType (applySubstitution sub argType) (applySubstitution sub resultType)
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
    (TypeForAllType vars1 typ1, TypeForAllType vars2 typ2) -> 
      unifyTypes typ1 typ2 state
    _ -> Left "Cannot unify types"

instantiateType :: TypeScheme -> InferenceState -> (Type, InferenceState)
instantiateType scheme state = 
  let vars = schemeVars scheme
      typ = schemeType scheme
      freshVars = [TypeVar (typeVarName var) (stateNextVarId state + i) (typeVarKind var) | (var, i) <- zip vars [0..]]
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
    extractFreeVars (TypeFunType argType resultType) = 
      extractFreeVars argType ++ extractFreeVars resultType
    extractFreeVars (TypeForAllType vars typ) = 
      extractFreeVars typ \\ vars

spec :: Spec
spec = describe "Advanced Type Inference Tests" $ do

  describe "Type variables" $ do
    it "creates type variables correctly" $ do
      let var = TypeVar "T" 1 "Type"
      typeVarName var `shouldBe` "T"
      typeVarId var `shouldBe` 1
      typeVarKind var `shouldBe` "Type"
      
    it "compares type variables correctly" $ do
      let var1 = TypeVar "T" 1 "Type"
          var2 = TypeVar "T" 2 "Type"
          var3 = TypeVar "U" 1 "Type"
      var1 `shouldBe` var1
      var1 `shouldNotBe` var2
      var1 `shouldNotBe` var3
      
    it "orders type variables correctly" $ do
      let var1 = TypeVar "T" 1 "Type"
          var2 = TypeVar "U" 2 "Type"
          var3 = TypeVar "T" 2 "Type"
      sort [var2, var1, var3] `shouldBe` [var1, var3, var2]

  describe "Type constructors" $ do
    it "creates type constructors correctly" $ do
      let con = TypeConstructor "List" 1
      typeConName con `shouldBe` "List"
      typeConArity con `shouldBe` 1
      
    it "compares type constructors correctly" $ do
      let con1 = TypeConstructor "List" 1
          con2 = TypeConstructor "List" 1
          con3 = TypeConstructor "Maybe" 1
      con1 `shouldBe` con2
      con1 `shouldNotBe` con3

  describe "Types" $ do
    it "creates variable types correctly" $ do
      let var = TypeVar "T" 1 "Type"
          typ = TypeVarType var
      case typ of
        TypeVarType v -> v `shouldBe` var
        _ -> expectationFailure "Expected TypeVarType"
        
    it "creates constructor types correctly" $ do
      let con = TypeConstructor "List" 1
          argType = TypeVarType (TypeVar "T" 1 "Type")
          typ = TypeConType con [argType]
      case typ of
        TypeConType c args -> do
          c `shouldBe` con
          args `shouldBe` [argType]
        _ -> expectationFailure "Expected TypeConType"
        
    it "creates function types correctly" $ do
      let argType = TypeVarType (TypeVar "T" 1 "Type")
          resultType = TypeVarType (TypeVar "U" 2 "Type")
          typ = TypeFunType argType resultType
      case typ of
        TypeFunType arg res -> do
          arg `shouldBe` argType
          res `shouldBe` resultType
        _ -> expectationFailure "Expected TypeFunType"
        
    it "creates polymorphic types correctly" $ do
      let var = TypeVar "T" 1 "Type"
          innerType = TypeVarType var
          typ = TypeForAllType [var] innerType
      case typ of
        TypeForAllType vars t -> do
          vars `shouldBe` [var]
          t `shouldBe` innerType
        _ -> expectationFailure "Expected TypeForAllType"

  describe "Type schemes" $ do
    it "creates type schemes correctly" $ do
      let var = TypeVar "T" 1 "Type"
          typ = TypeVarType var
          scheme = TypeScheme [var] typ
      schemeVars scheme `shouldBe` [var]
      schemeType scheme `shouldBe` typ
      
    it "handles empty type schemes" $ do
      let typ = TypeConType (TypeConstructor "Int" 0) []
          scheme = TypeScheme [] typ
      schemeVars scheme `shouldBe` []
      schemeType scheme `shouldBe` typ

  describe "Constraints" $ do
    it "creates equality constraints correctly" $ do
      let var = TypeVar "T" 1 "Type"
          typ1 = TypeVarType var
          typ2 = TypeConType (TypeConstructor "Int" 0) []
          constraint = EqualityConstraint typ1 typ2
      case constraint of
        EqualityConstraint t1 t2 -> do
          t1 `shouldBe` typ1
          t2 `shouldBe` typ2
        _ -> expectationFailure "Expected EqualityConstraint"
        
    it "creates type class constraints correctly" $ do
      let var = TypeVar "T" 1 "Type"
          typ = TypeVarType var
          constraint = TypeClassConstraint "Eq" typ
      case constraint of
        TypeClassConstraint className t -> do
          className `shouldBe` "Eq"
          t `shouldBe` typ
        _ -> expectationFailure "Expected TypeClassConstraint"

  describe "Substitutions" $ do
    it "creates empty substitution" $ do
      let sub = emptySubstitution
      Map.size (substitutionMap sub) `shouldBe` 0
      
    it "composes substitutions correctly" $ do
      let var1 = TypeVar "T" 1 "Type"
          var2 = TypeVar "U" 2 "Type"
          typ1 = TypeConType (TypeConstructor "Int" 0) []
          typ2 = TypeConType (TypeConstructor "String" 0) []
          sub1 = Substitution (Map.fromList [(var1, typ1)])
          sub2 = Substitution (Map.fromList [(var2, typ2)])
          composed = composeSubstitutions sub1 sub2
      Map.size (substitutionMap composed) `shouldBe` 2
      
    it "applies substitutions to variable types" $ do
      let var = TypeVar "T" 1 "Type"
          typ1 = TypeVarType var
          typ2 = TypeConType (TypeConstructor "Int" 0) []
          sub = Substitution (Map.fromList [(var, typ2)])
          result = applySubstitution sub typ1
      result `shouldBe` typ2
      
    it "applies substitutions to constructor types" $ do
      let var = TypeVar "T" 1 "Type"
          argType = TypeVarType var
          typ1 = TypeConType (TypeConstructor "List" 1) [argType]
          typ2 = TypeConType (TypeConstructor "Int" 0) []
          sub = Substitution (Map.fromList [(var, typ2)])
          result = applySubstitution sub typ1
      case result of
        TypeConType con args -> do
          con `shouldBe` TypeConstructor "List" 1
          args `shouldBe` [typ2]
        _ -> expectationFailure "Expected TypeConType"

  describe "Type unification" $ do
    it "unifies variable with type" $ do
      let var = TypeVar "T" 1 "Type"
          typ1 = TypeVarType var
          typ2 = TypeConType (TypeConstructor "Int" 0) []
          state = InferenceState emptySubstitution [] 1
          result = unifyTypes typ1 typ2 state
      case result of
        Right newState -> do
          let sub = stateSubstitution newState
          Map.lookup var (substitutionMap sub) `shouldBe` Just typ2
        Left _ -> expectationFailure "Expected successful unification"
        
    it "unifies identical constructor types" $ do
      let typ1 = TypeConType (TypeConstructor "Int" 0) []
          typ2 = TypeConType (TypeConstructor "Int" 0) []
          state = InferenceState emptySubstitution [] 1
          result = unifyTypes typ1 typ2 state
      case result of
        Right _ -> return ()
        Left err -> expectationFailure $ "Expected successful unification, got: " ++ err
        
    it "fails to unify different constructor types" $ do
      let typ1 = TypeConType (TypeConstructor "Int" 0) []
          typ2 = TypeConType (TypeConstructor "String" 0) []
          state = InferenceState emptySubstitution [] 1
          result = unifyTypes typ1 typ2 state
      case result of
        Right _ -> expectationFailure "Expected unification to fail"
        Left _ -> return ()
        
    it "unifies function types" $ do
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
          Map.lookup var1 (substitutionMap sub) `shouldBe` Just (TypeConType (TypeConstructor "Int" 0) [])
          Map.lookup var2 (substitutionMap sub) `shouldBe` Just (TypeConType (TypeConstructor "String" 0) [])
        Left _ -> expectationFailure "Expected successful unification"

  describe "Type instantiation and generalization" $ do
    it "instantiates type schemes correctly" $ do
      let var = TypeVar "T" 1 "Type"
          typ = TypeVarType var
          scheme = TypeScheme [var] typ
          state = InferenceState emptySubstitution [] 1
          (instantiatedType, newState) = instantiateType scheme state
      case instantiatedType of
        TypeVarType newVar -> do
          typeVarName newVar `shouldBe` "T"
          typeVarId newVar `shouldBe` 1
          typeVarKind newVar `shouldBe` "Type"
        _ -> expectationFailure "Expected TypeVarType"
      stateNextVarId newState `shouldBe` 2
      
    it "generalizes types correctly" $ do
      let var = TypeVar "T" 1 "Type"
          typ = TypeVarType var
          state = InferenceState emptySubstitution [] 1
          scheme = generalizeType typ state
      schemeVars scheme `shouldBe` [var]
      schemeType scheme `shouldBe` typ
      
    it "generalizes types with substitutions" $ do
      let var1 = TypeVar "T" 1 "Type"
          var2 = TypeVar "U" 2 "Type"
          typ1 = TypeVarType var1
          typ2 = TypeConType (TypeConstructor "Int" 0) []
          sub = Substitution (Map.fromList [(var1, typ2)])
          state = InferenceState sub [] 1
          scheme = generalizeType typ1 state
      schemeVars scheme `shouldBe` []
      schemeType scheme `shouldBe` typ1

  describe "Inference state" $ do
    it "creates inference state correctly" $ do
      let state = InferenceState emptySubstitution [] 1
      stateSubstitution state `shouldBe` emptySubstitution
      stateConstraints state `shouldBe` []
      stateNextVarId state `shouldBe` 1
      
    it "updates next variable ID" $ do
      let state = InferenceState emptySubstitution [] 1
          newState = state { stateNextVarId = 5 }
      stateNextVarId newState `shouldBe` 5

  describe "QuickCheck properties" $ do
    it "substitution composition is associative" $ property $
      \sub1 sub2 sub3 ->
        let composed1 = composeSubstitutions sub1 (composeSubstitutions sub2 sub3)
            composed2 = composeSubstitutions (composeSubstitutions sub1 sub2) sub3
        in substitutionMap composed1 `shouldBe` substitutionMap composed2
        
    it "substitution application is idempotent for ground types" $ property $
      \sub typ ->
        let isGround (TypeVarType _) = False
            isGround (TypeConType _ args) = all isGround args
            isGround (TypeFunType arg res) = isGround arg && isGround res
            isGround (TypeForAllType _ t) = isGround t
        in if isGround typ
           then applySubstitution sub (applySubstitution sub typ) `shouldBe` applySubstitution sub typ
           else True
           
    it "unification is symmetric" $ property $
      \typ1 typ2 state ->
        let result1 = unifyTypes typ1 typ2 state
            result2 = unifyTypes typ2 typ1 state
        in case (result1, result2) of
              (Right _, Right _) -> True
              (Left _, Left _) -> True
              _ -> False  -- One succeeds, other fails - shouldn't happen but we handle it

  describe "Edge cases" $ do
    it "handles empty types" $ do
      let typ = TypeConType (TypeConstructor "Unit" 0) []
          state = InferenceState emptySubstitution [] 1
          result = unifyTypes typ typ state
      case result of
        Right _ -> return ()
        Left _ -> expectationFailure "Expected successful unification"
        
    it "handles recursive types" $ do
      let var = TypeVar "T" 1 "Type"
          typ1 = TypeVarType var
          typ2 = TypeConType (TypeConstructor "List" 1) [typ1]
          state = InferenceState emptySubstitution [] 1
          result = unifyTypes typ1 typ2 state
      case result of
        Right newState -> do
          let sub = stateSubstitution newState
          Map.lookup var (substitutionMap sub) `shouldBe` Just typ2
        Left _ -> expectationFailure "Expected successful unification"
        
    it "handles complex function types" $ do
      let var1 = TypeVar "T" 1 "Type"
          var2 = TypeVar "U" 2 "Type"
          var3 = TypeVar "V" 3 "Type"
          innerFunc = TypeFunType (TypeVarType var1) (TypeVarType var2)
          outerFunc = TypeFunType innerFunc (TypeVarType var3)
          state = InferenceState emptySubstitution [] 1
          result = unifyTypes outerFunc outerFunc state
      case result of
        Right _ -> return ()
        Left _ -> expectationFailure "Expected successful unification"
        
    it "handles large type environments" $ do
      let vars = [TypeVar ("T" ++ show i) i "Type" | i <- [1..50]]
          types = [TypeVarType var | var <- vars]
          state = InferenceState emptySubstitution [] 1
          result = foldr (\typ acc -> case acc of
                                         Left err -> Left err
                                         Right s -> unifyTypes typ typ s) (Right state) types
      case result of
        Right _ -> return ()
        Left _ -> expectationFailure "Expected successful unification"