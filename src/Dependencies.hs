module Dependencies (
  -- Dependent type checker
  DependentTypeChecker,
  DependentTypeError(..),

  -- AST
  AST(..),
  Statement(..),
  TypeExpr(..),
  Constraint(..),

  -- Type system entities
  TypeVar(..),
  TypeConstraint(..),
  Substitution,

  -- Hindley-Milner inference
  TypeScheme(..),
  TypeEnvironment(..),
  TypeInferenceState(..),
  TypeInferenceError(..),

  -- Construction & usage
  newDependentTypeChecker,
  newDependentTypeCheckerWithTypes,
  analyzeDependentTypes,
  analyzeAST,
  validateASTSemantics,
  validateStatement,

  -- Core operations
  checkType,
  addType,
  addConstraint,
  checkTypeInstantiation,
  solveConstraints,
  getDependentTypeErrors,
  unify,

  -- Inference operations
  inferType,
  inferStatement,
  inferProgram,
  generalize,
  instantiate,
  unifyTypes,
  applyTypeSubstitution,
  newTypeVariable,
  getFreshTypeVar,
  initialTypeEnvironment,

  -- Generic helpers
  instantiateScheme,
  generalizeInContext,
  checkPolyType,

  -- Constraint solving
  solveTypeConstraints,
  simplifyConstraints,

  -- Scope management
  pushScope,
  popScope,
  inNewScope,

  -- Parsing
  grammarDefinition,
  parseProgram,
  runParser
) where

import Dependencies.AST
import Dependencies.Analyzer (analyzeAST, analyzeDependentTypes, validateASTSemantics, validateStatement)
import Dependencies.Inference
import Dependencies.Parser (grammarDefinition, parseProgram, runParser)
import Dependencies.TypeSystem
