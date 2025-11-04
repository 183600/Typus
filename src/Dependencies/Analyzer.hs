{-# LANGUAGE OverloadedStrings #-}

module Dependencies.Analyzer (
  analyzeDependentTypes,
  analyzeAST,
  validateASTSemantics,
  validateStatement
) where

import Control.Monad (mapM_)
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Text as T

import Dependencies.AST
import Dependencies.Parser (runParser)
import Dependencies.TypeSystem

analyzeDependentTypes :: String -> [DependentTypeError]
analyzeDependentTypes src =
  case runParser src of
    Left e   -> [ParseError e]
    Right ast ->
      let (errs, _) = runState (validateASTSemantics ast) newDependentTypeChecker
      in errs

analyzeAST :: AST -> [DependentTypeError]
analyzeAST ast =
  let (errs, _) = runState (validateASTSemantics ast) newDependentTypeChecker
  in errs

validateASTSemantics :: AST -> State DependentTypeChecker [DependentTypeError]
validateASTSemantics (Program ss) = do
  mapM_ validateStatement ss
  _ <- solveConstraints
  st <- get
  pure (reverse (tcErrors st))

validateStatement :: Statement -> State DependentTypeChecker ()
validateStatement stmt = case stmt of
  STypeDef name params cs -> do
    let cs' = map (convertConstraint (Set.fromList (map T.unpack params))) cs
    addType (T.unpack name) (map T.unpack params) cs'

  STypeAlias name target cs -> do
    let (tv, extraCs) = convertTypeExprAndRefinements Set.empty target
    checkType tv
    mapM_ addConstraint extraCs
    let cs' = map (convertConstraint Set.empty) cs
    addType (T.unpack name) [] cs'

  SVarDecl _name texpr -> do
    let (tv, extraCs) = convertTypeExprAndRefinements Set.empty texpr
    checkType tv
    mapM_ addConstraint extraCs

  SFuncDecl _name params rt -> do
    mapM_ (\(_n,t) -> do
              let (pt, pcs) = convertTypeExprAndRefinements Set.empty t
              checkType pt
              mapM_ addConstraint pcs) params
    case rt of
      Nothing -> pure ()
      Just t  -> do
        let (rv, rcs) = convertTypeExprAndRefinements Set.empty t
        checkType rv
        mapM_ addConstraint rcs

  SConstraintDef _ c -> do
    let c' = convertConstraint Set.empty c
    addConstraint c'

  SExistsDecl _vars innerStmt ->
    validateStatement innerStmt
