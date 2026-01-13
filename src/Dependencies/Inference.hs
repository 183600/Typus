{-# LANGUAGE OverloadedStrings #-}

module Dependencies.Inference (
  TypeInference,
  newTypeVariable,
  getFreshTypeVar,
  initialTypeEnvironment,
  inferType,
  inferStatement,
  inferProgram,
  generalize,
  instantiate,
  unifyTypes,
  applyTypeSubstitution,
  addTypeConstraint,
  extractFreeTypeVars,
  instantiateScheme,
  generalizeInContext,
  checkPolyType,
  solveTypeConstraints,
  simplifyConstraints,
  pushScope,
  popScope,
  inNewScope
) where

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

import Dependencies.AST
import Dependencies.TypeSystem (TypeVar(..), TypeDef(..), TypeConstraint(..), TypeEnvironment(..), TypeInferenceState(..), TypeInferenceError(..), Substitution, TypeScheme(..), preludeTypeDefs, convertConstraint, unify, validateConstraint)

type TypeInference = StateT TypeInferenceState (ExceptT TypeInferenceError IO)

nextTypeVarId :: TypeInference Int
nextTypeVarId = do
  env <- gets typeEnv
  liftIO $ atomicModifyIORef' (teNextTypeVarId env) (\i -> (i + 1, i))

newTypeVariable :: TypeInference TypeVar
newTypeVariable = do
  varId <- nextTypeVarId
  pure $ TVVar ("t" ++ show varId)

getFreshTypeVar :: TypeInference TypeVar
getFreshTypeVar = newTypeVariable

initialTypeEnvironment :: IO TypeEnvironment
initialTypeEnvironment = do
  ref <- newIORef 0
  pure $ TypeEnvironment
    { teTypes = preludeTypeDefs
    , teSchemes = Map.empty
    , teCurrentLevel = 0
    , teNextTypeVarId = ref
    }

-- Inference primitives --------------------------------------------------------

inferType :: TypeExpr -> TypeInference TypeVar
inferType expr = case expr of
  SimpleT name -> do
    env <- gets typeEnv
    case Map.lookup (T.unpack name) (teTypes env) of
      Nothing -> pure $ TVVar "error"
      Just _  -> pure $ TVCon (T.unpack name)

  GenericT name args -> do
    argTypes <- mapM inferType args
    pure $ TVApp (T.unpack name) argTypes

  FuncT params returnType -> do
    paramTypes <- mapM (inferType . snd) params
    retType <- inferType returnType
    pure $ TVFun paramTypes retType

  RefineT baseType constraints -> do
    base <- inferType baseType
    let typeConstraints = map (convertConstraint Set.empty) constraints
    mapM_ addTypeConstraint typeConstraints
    pure base

inferStatement :: Statement -> TypeInference (Maybe TypeVar)
inferStatement stmt = case stmt of
  STypeDef name params constraints -> do
    let typeConstraints = map (convertConstraint (Set.fromList (map T.unpack params))) constraints
    env <- gets typeEnv
    let typeDef = TypeDefDecl (map T.unpack params) typeConstraints
        updatedEnv = env { teTypes = Map.insert (T.unpack name) typeDef (teTypes env) }
    modify $ \s -> s { typeEnv = updatedEnv }
    pure Nothing

  SVarDecl name typeExpr -> do
    varType <- inferType typeExpr
    env <- gets typeEnv
    let scheme = Forall [] varType
        updatedEnv = env { teSchemes = Map.insert (T.unpack name) scheme (teSchemes env) }
    modify $ \s -> s { typeEnv = updatedEnv }
    pure (Just varType)

  SFuncDecl name params returnType -> do
    paramTypes <- mapM (inferType . snd) params
    retType <- case returnType of
      Nothing -> getFreshTypeVar
      Just rt -> inferType rt
    let funcType = TVFun paramTypes retType
    env <- gets typeEnv
    let scheme = Forall [] funcType
        updatedEnv = env { teSchemes = Map.insert (T.unpack name) scheme (teSchemes env) }
    modify $ \s -> s { typeEnv = updatedEnv }
    pure (Just funcType)

  SConstraintDef _ constraint -> do
    let typeConstraint = convertConstraint Set.empty constraint
    addTypeConstraint typeConstraint
    pure Nothing

  STypeAlias _ _ _ ->
    pure Nothing

  SExistsDecl _vars innerStmt -> do
    _ <- inferStatement innerStmt
    pure Nothing

inferProgram :: AST -> TypeInference [TypeVar]
inferProgram (Program statements) = do
  types <- mapM inferStatement statements
  pure $ catMaybes types

-- Generalisation & instantiation ----------------------------------------------

generalize :: Int -> TypeVar -> TypeInference TypeScheme
generalize level tv = do
  let freeVars = extractFreeTypeVars tv
      nonGenericVars = filter (not . isGenericLevel level) freeVars
  pure $ Forall nonGenericVars tv
  where
    isGenericLevel :: p1 -> p2 -> Bool
    isGenericLevel _ _ = True

instantiate :: TypeScheme -> TypeInference TypeVar
instantiate (Forall vars tv) = do
  newVars <- mapM (const getFreshTypeVar) vars
  let substitution = Map.fromList (zip vars newVars)
  pure $ applyTypeSubstitution substitution tv

unifyTypes :: TypeVar -> TypeVar -> TypeInference ()
unifyTypes t1 t2 =
  case unify [(t1, t2)] of
    Nothing -> pure ()
    Just substList -> do
      let subst = Map.fromList substList
      modify $ \s -> s { currentSubst = subst `Map.union` currentSubst s }

applyTypeSubstitution :: Substitution -> TypeVar -> TypeVar
applyTypeSubstitution subst tv = case tv of
  TVVar name -> Map.findWithDefault tv name subst
  TVCon _ -> tv
  TVApp name args -> TVApp name (map (applyTypeSubstitution subst) args)
  TVFun params ret -> TVFun (map (applyTypeSubstitution subst) params) (applyTypeSubstitution subst ret)
  TVTuple args -> TVTuple (map (applyTypeSubstitution subst) args)

addTypeConstraint :: TypeConstraint -> TypeInference ()
addTypeConstraint constraint =
  case validateConstraint constraint of
    Right () -> pure ()
    Left _   -> pure ()

extractFreeTypeVars :: TypeVar -> [String]
extractFreeTypeVars = go Set.empty
  where
    go seen tv = case tv of
      TVVar name -> if Set.member name seen then [] else [name]
      TVCon _ -> []
      TVApp _ args -> concatMap (go seen) args
      TVFun params ret -> concatMap (go seen) params ++ go seen ret
      TVTuple args -> concatMap (go seen) args

instantiateScheme :: TypeScheme -> TypeInference TypeVar
instantiateScheme = instantiate

generalizeInContext :: TypeVar -> TypeInference TypeScheme
generalizeInContext tv = do
  env <- gets typeEnv
  generalize (teCurrentLevel env) tv

checkPolyType :: TypeVar -> TypeInference ()
checkPolyType tv = do
  let freeVars = extractFreeTypeVars tv
  unless (null freeVars) $ pure ()

solveTypeConstraints :: [TypeConstraint] -> TypeInference ()
solveTypeConstraints constraints = mapM_ solveSingle constraints
  where
    solveSingle :: Applicative f => TypeConstraint -> f ()
    solveSingle constraint =
      case validateConstraint constraint of
        Right () -> pure ()
        Left _   -> pure ()

simplifyConstraints :: [TypeConstraint] -> [TypeConstraint]
simplifyConstraints = nub

pushScope :: TypeInference ()
pushScope = modify $ \s ->
  let env = typeEnv s
      newLevel = teCurrentLevel env + 1
      newEnv = env { teCurrentLevel = newLevel }
  in s { typeEnv = newEnv }

popScope :: TypeInference ()
popScope = modify $ \s ->
  let env = typeEnv s
      newLevel = max 0 (teCurrentLevel env - 1)
      newEnv = env { teCurrentLevel = newLevel }
  in s { typeEnv = newEnv }

inNewScope :: TypeInference a -> TypeInference a
inNewScope action = do
  pushScope
  result <- action
  popScope
  pure result
