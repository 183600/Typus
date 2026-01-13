{-# LANGUAGE OverloadedStrings #-}

module Dependencies.TypeSystem (
  -- Core types
  TypeVar(..),
  TypeConstraint(..),
  DependentTypeError(..),
  TypeDef(..),
  TypeEnv(..),
  DependentTypeChecker(..),
  Substitution,
  
  -- Re-export from Inference module
  TypeScheme(..),
  TypeEnvironment(..),
  TypeInferenceState(..),
  TypeInferenceError(..),

  -- Environments
  preludeTypeDefs,
  newDependentTypeChecker,
  newDependentTypeCheckerWithTypes,

  -- Conversion helpers
  convertTypeExpr,
  convertTypeExprAndRefinements,
  convertConstraint,

  -- Type environment operations
  addType,
  addConstraint,
  addTypeError,
  lookupTypeDef,
  checkType,
  checkTypeInstantiation,
  solveConstraints,
  checkTypeConstraint,
  validateConstraint,
  getDependentTypeErrors,
  unify
) where

import Control.Monad (when)
import Control.Monad.State (State, get, put, modify)
import Data.Either (partitionEithers)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Dependencies.AST (TypeExpr(SimpleT, GenericT, RefineT, FuncT), Constraint(RangeC, PredC, SizeGE, SizeGT))
import Control.Monad (unless)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, gets, modify)
import Data.IORef
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Text as T

-- Hindley-Milner style type inference -----------------------------------------

data TypeScheme = Forall [String] TypeVar
  deriving (Show, Eq)

data TypeEnvironment = TypeEnvironment
  { teTypes         :: Map.Map String TypeDef
  , teSchemes       :: Map.Map String TypeScheme
  , teCurrentLevel  :: Int
  , teNextTypeVarId :: IORef Int
  }

data TypeInferenceState = TypeInferenceState
  { typeEnv         :: TypeEnvironment
  , currentSubst    :: Substitution
  , inferenceErrors :: [TypeInferenceError]
  }

data TypeInferenceError
  = UnificationFailure TypeVar TypeVar
  | InfiniteType String TypeVar
  | UnboundVariable String
  | TypeMismatchError TypeVar TypeVar
  | ConstraintNotSatisfied TypeConstraint
  | OccursCheckFailed String TypeVar
  | GenericEscape String TypeVar
  deriving (Show, Eq)

type TypeInference = StateT TypeInferenceState (ExceptT TypeInferenceError IO)

data TypeVar
  = TVCon String
  | TVVar String
  | TVApp String [TypeVar]
  | TVFun [TypeVar] TypeVar
  | TVTuple [TypeVar]
  deriving (Show, Eq, Ord)

data TypeConstraint
  = Equal TypeVar TypeVar
  | Subtype TypeVar TypeVar
  | Predicate String [TypeVar]
  | TypeSizeGE TypeVar Int
  | TypeSizeGT TypeVar Int
  | TypeRange TypeVar Int Int
  deriving (Show, Eq, Ord)

data DependentTypeError
  = DependentTypeMismatch TypeVar TypeVar
  | ConstraintViolation String TypeVar
  | TypeNotFound String
  | InvalidTypeArgument String
  | UnsolvableConstraint TypeConstraint
  | DependentInfiniteType String TypeVar
  | AmbiguousType String
  | ParseError String
  | SemanticError String
  | DependentTypeError String
  deriving (Show, Eq)

data TypeDef = TypeDefDecl
  { tdParams      :: [String]
  , tdConstraints :: [TypeConstraint]
  } deriving (Show, Eq)

data TypeEnv = TypeEnv
  { typeDefinitions    :: Map.Map String TypeDef
  , pendingConstraints :: [TypeConstraint]
  } deriving (Show, Eq)

data DependentTypeChecker = DependentTypeChecker
  { dtcTypeEnv :: TypeEnv
  , tcErrors   :: [DependentTypeError]
  } deriving (Show, Eq)

type Substitution = Map.Map String TypeVar

-- Prelude definitions ---------------------------------------------------------

preludeTypeDefs :: Map.Map String TypeDef
preludeTypeDefs = Map.fromList
  [ ("int",      TypeDefDecl [] [])
  , ("string",   TypeDefDecl [] [])
  , ("bool",     TypeDefDecl [] [])
  , ("float64",  TypeDefDecl [] [])
  ]

newDependentTypeChecker :: DependentTypeChecker
newDependentTypeChecker = DependentTypeChecker
  { dtcTypeEnv = TypeEnv preludeTypeDefs []
  , tcErrors = []
  }

newDependentTypeCheckerWithTypes :: [(String, [String], [TypeConstraint])] -> DependentTypeChecker
newDependentTypeCheckerWithTypes typeDefs =
  let defs = Map.fromList [ (n, TypeDefDecl ps cs) | (n, ps, cs) <- typeDefs ]
   in DependentTypeChecker
        { dtcTypeEnv = TypeEnv (preludeTypeDefs <> defs) []
        , tcErrors = []
        }

-- Conversion ------------------------------------------------------------------

convertTypeExprAndRefinements :: Set.Set String -> TypeExpr -> (TypeVar, [TypeConstraint])
convertTypeExprAndRefinements params te = case te of
  SimpleT n ->
    ( nameToTypeVar params n
    , []
    )
  GenericT n args ->
    let argPairs = map (convertTypeExprAndRefinements params) args
        argTVs = map fst argPairs
        argCs  = concatMap snd argPairs
    in ( TVApp (T.unpack n) argTVs
       , argCs
       )
  FuncT ps rt ->
    let psPairs = [ convertTypeExprAndRefinements params t | (_,t) <- ps ]
        psTVs = map fst psPairs
        psCs  = concatMap snd psPairs
        (rtTV, rtCs) = convertTypeExprAndRefinements params rt
    in ( TVFun psTVs rtTV, psCs <> rtCs )
  RefineT base cs ->
    let (bTV, bCs) = convertTypeExprAndRefinements params base
        cs' = map (convertConstraint params) cs
    in (bTV, bCs <> cs')

convertTypeExpr :: Set.Set String -> TypeExpr -> TypeVar
convertTypeExpr params t = fst (convertTypeExprAndRefinements params t)

convertConstraint :: Set.Set String -> Constraint -> TypeConstraint
convertConstraint params c = case c of
  SizeGE name k ->
    TypeSizeGE (nameToTypeVar params name) k
  SizeGT name k ->
    TypeSizeGT (nameToTypeVar params name) k
  RangeC name a b ->
    TypeRange (nameToTypeVar params name) a b
  PredC pname args ->
    let tvars = map (convertTypeExpr params) args
    in Predicate (T.unpack pname) tvars

nameToTypeVar :: Set.Set String -> Text -> TypeVar
nameToTypeVar params n =
  let s = T.unpack n
  in if s `Set.member` params
        then TVVar s
        else if isLowerStart s
          then TVVar s
          else TVCon s
  where
    isLowerStart (c:_) = (c >= 'a' && c <= 'z') || c == '_'
    isLowerStart _ = False

-- Type environment operations -------------------------------------------------

addType :: String -> [String] -> [TypeConstraint] -> State DependentTypeChecker ()
addType name params cs = do
  st <- get
  let env = dtcTypeEnv st
      defs = typeDefinitions env
      def  = TypeDefDecl params cs
      defs' = Map.insert name def defs
  put st { dtcTypeEnv = env { typeDefinitions = defs' } }

addConstraint :: TypeConstraint -> State DependentTypeChecker ()
addConstraint c = do
  st <- get
  let env = dtcTypeEnv st
  put st { dtcTypeEnv = env { pendingConstraints = c : pendingConstraints env } }

addTypeError :: DependentTypeError -> State DependentTypeChecker ()
addTypeError e = modify (\st -> st { tcErrors = e : tcErrors st })

lookupTypeDef :: String -> State DependentTypeChecker (Maybe TypeDef)
lookupTypeDef n = do
  st <- get
  pure $ Map.lookup n (typeDefinitions (dtcTypeEnv st))

checkType :: TypeVar -> State DependentTypeChecker ()
checkType tv = case tv of
  TVCon n -> do
    mdef <- lookupTypeDef n
    case mdef of
      Nothing -> addTypeError (TypeNotFound n)
      Just (TypeDefDecl ps _) ->
        when (not (null ps)) $
          addTypeError (InvalidTypeArgument n)
  TVVar _ -> pure ()
  TVApp n args -> do
    mdef <- lookupTypeDef n
    case mdef of
      Nothing -> addTypeError (TypeNotFound n)
      Just (TypeDefDecl ps cs) -> do
        when (length ps /= length args) $
          addTypeError (InvalidTypeArgument n)
        mapM_ checkType args
        let subst = zip ps args
        mapM_ (checkTypeConstraint subst) cs
  TVFun ps rt -> mapM_ checkType ps >> checkType rt
  TVTuple xs -> mapM_ checkType xs

checkTypeInstantiation :: String -> [TypeVar] -> State DependentTypeChecker ()
checkTypeInstantiation n args = do
  mdef <- lookupTypeDef n
  case mdef of
    Nothing -> addTypeError (TypeNotFound n)
    Just (TypeDefDecl ps cs) -> do
      when (length ps /= length args) $
        addTypeError (InvalidTypeArgument n)
      mapM_ checkType args
      let subst = zip ps args
      mapM_ (checkTypeConstraint subst) cs

solveConstraints :: State DependentTypeChecker Bool
solveConstraints = do
  st <- get
  let cs = pendingConstraints (dtcTypeEnv st)
      (eqs, others) = partitionEithers (map pickEq cs)
      pickEq cc = case cc of
        Equal a b   -> Left (a,b)
        Subtype a b -> Left (a,b)
        _           -> Right cc
  case unify eqs of
    Nothing -> do
      addTypeError (SemanticError "failed to unify type equalities/subtypes")
      st' <- get
      put st' { dtcTypeEnv = (dtcTypeEnv st') { pendingConstraints = [] } }
      pure False
    Just subst -> do
      let cs' = map (applySubstC subst) others
          results = map validateConstraint cs'
          (errs, _) = partitionEithers (map toEither results)
          toEither :: Either a b -> Either a b
          toEither (Left e)  = Left e
          toEither (Right x) = Right x
      mapM_ addTypeError errs
      st' <- get
      put st' { dtcTypeEnv = (dtcTypeEnv st') { pendingConstraints = [] } }
      pure (null errs)

checkTypeConstraint :: [(String, TypeVar)] -> TypeConstraint -> State DependentTypeChecker ()
checkTypeConstraint subst c =
  case validateConstraint (applySubstC subst c) of
    Right _ -> pure ()
    Left e  -> addTypeError e

validateConstraint :: TypeConstraint -> Either DependentTypeError ()
validateConstraint c = case c of
  Equal a b ->
    if a == b then Right () else Left (DependentTypeMismatch a b)
  Subtype a b ->
    if isSubtype a b then Right () else Left (DependentTypeMismatch a b)
  TypeSizeGE _ n ->
    if n >= 0 then Right () else Left (SemanticError "size >= n must have n >= 0")
  TypeSizeGT _ n ->
    if n >= 0 then Right () else Left (SemanticError "size > n must have n >= 0")
  TypeRange _ a b ->
    if a <= b then Right () else Left (SemanticError "invalid range: min > max")
  Predicate _ _ ->
    Right ()

getDependentTypeErrors :: DependentTypeChecker -> [DependentTypeError]
getDependentTypeErrors = reverse . tcErrors

-- Internal helpers ------------------------------------------------------------

type Subst = [(String, TypeVar)]

applySubst :: Subst -> TypeVar -> TypeVar
applySubst s tv = case tv of
  TVVar x ->
    case lookup x s of
      Nothing -> TVVar x
      Just t  -> if t == TVVar x then TVVar x else applySubst s t
  TVCon _ -> tv
  TVApp f args -> TVApp f (map (applySubst s) args)
  TVFun ps rt  -> TVFun (map (applySubst s) ps) (applySubst s rt)
  TVTuple xs   -> TVTuple (map (applySubst s) xs)

applySubstC :: Subst -> TypeConstraint -> TypeConstraint
applySubstC s c = case c of
  Equal a b       -> Equal (applySubst s a) (applySubst s b)
  Subtype a b     -> Subtype (applySubst s a) (applySubst s b)
  Predicate p xs  -> Predicate p (map (applySubst s) xs)
  TypeSizeGE t n  -> TypeSizeGE (applySubst s t) n
  TypeSizeGT t n  -> TypeSizeGT (applySubst s t) n
  TypeRange t a b -> TypeRange (applySubst s t) a b

occurs :: String -> TypeVar -> Bool
occurs x tv = case tv of
  TVVar y       -> x == y
  TVCon _       -> False
  TVApp _ args  -> any (occurs x) args
  TVFun ps rt   -> any (occurs x) ps || occurs x rt
  TVTuple xs    -> any (occurs x) xs

unify :: [(TypeVar, TypeVar)] -> Maybe Subst
unify = go []
  where
    go s [] = Just s
    go s ((a,b):rest)
      | a == b = go s rest
      | otherwise =
          case (a,b) of
            (TVVar x, t)
              | occurs x t -> Nothing
              | otherwise  -> go ((x,t):s) (applyPairs (x,t) rest)
            (t, TVVar x)    -> go s ((TVVar x,t):rest)
            (TVCon f, TVCon g)
              | f == g      -> go s rest
              | otherwise   -> Nothing
            (TVApp f xs, TVApp g ys)
              | f == g && length xs == length ys
                            -> go s (zip xs ys ++ rest)
              | otherwise   -> Nothing
            (TVFun ps1 r1, TVFun ps2 r2)
              | length ps1 == length ps2
                            -> go s (zip ps1 ps2 ++ [(r1,r2)] ++ rest)
              | otherwise   -> Nothing
            (TVTuple xs, TVTuple ys)
              | length xs == length ys
                            -> go s (zip xs ys ++ rest)
              | otherwise   -> Nothing
            _               -> Nothing

    applyPairs (x,t) = map (\(l,r) -> (applySubst [(x,t)] l, applySubst [(x,t)] r))

isSubtype :: TypeVar -> TypeVar -> Bool
isSubtype a b = a == b
