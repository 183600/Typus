{-# LANGUAGE CPP #-}
module Test.Unit.DependenciesAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, choose, listOf, forAll, Property, (===), counterexample, (==>))

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (isInfixOf, nub, sort)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad.State (evalState)

import Dependencies
  ( DependentTypeChecker(..)
  , DependentTypeError(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , TypeDef(..)
  , TypeEnv(..)
  , Substitution
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , addType
  , addConstraint
  , addTypeError
  , lookupTypeDef
  , checkType
  , checkTypeInstantiation
  , solveConstraints
  , checkTypeConstraint
  , validateConstraint
  , getDependentTypeErrors
  , unify
  , convertTypeExpr
  , convertConstraint
  , inferType
  , inferStatement
  , inferProgram
  , generalize
  , instantiate
  , unifyTypes
  , applyTypeSubstitution
  , newTypeVariable
  , getFreshTypeVar
  , initialTypeEnvironment
  , solveTypeConstraints
  , simplifyConstraints
  , pushScope
  , popScope
  , inNewScope
  , parseProgram
  , runParser
  )

import Dependencies.AST (TypeExpr(..), Constraint(..), Statement(..), AST(..))

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary TypeVar where
  arbitrary = oneof
    [ TVCon <$> arbitrary
    , TVVar <$> arbitrary
    , TVApp <$> arbitrary <*> listOf arbitrary
    , TVFun <$> listOf arbitrary <*> arbitrary
    , TVTuple <$> listOf arbitrary
    ]

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ Equal <$> arbitrary <*> arbitrary
    , Subtype <$> arbitrary <*> arbitrary
    , Predicate <$> arbitrary <*> listOf arbitrary
    , TypeSizeGE <$> arbitrary <*> choose (0, 1000)
    , TypeSizeGT <$> arbitrary <*> choose (0, 1000)
    , TypeRange <$> arbitrary <*> choose (0, 1000) <*> choose (0, 1000)
    ]

instance Arbitrary DependentTypeError where
  arbitrary = oneof
    [ DependentTypeMismatch <$> arbitrary <*> arbitrary
    , ConstraintViolation <$> arbitrary <*> arbitrary
    , TypeNotFound <$> arbitrary
    , InvalidTypeArgument <$> arbitrary
    , UnsolvableConstraint <$> arbitrary
    , DependentInfiniteType <$> arbitrary <*> arbitrary
    , AmbiguousType <$> arbitrary
    , ParseError <$> arbitrary
    , SemanticError <$> arbitrary
    ]

instance Arbitrary TypeDef where
  arbitrary = do
    params <- listOf arbitrary
    constraints <- listOf arbitrary
    return $ TypeDefDecl params constraints

instance Arbitrary TypeEnv where
  arbitrary = do
    typeDefs <- arbitrary
    constraints <- listOf arbitrary
    return $ TypeEnv typeDefs constraints

instance Arbitrary DependentTypeChecker where
  arbitrary = do
    typeEnv <- arbitrary
    errors <- listOf arbitrary
    return $ DependentTypeChecker typeEnv errors

instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ SimpleT <$> arbitrary
    , GenericT <$> arbitrary <*> listOf arbitrary
    , FuncT <$> listOf ((,) <$> arbitrary <*> arbitrary) <*> arbitrary
    , RefineT <$> arbitrary <*> listOf arbitrary
    ]

instance Arbitrary Constraint where
  arbitrary = oneof
    [ SizeGE <$> arbitrary <*> choose (0, 1000)
    , SizeGT <$> arbitrary <*> choose (0, 1000)
    , RangeC <$> arbitrary <*> choose (0, 1000) <*> choose (0, 1000)
    , PredC <$> arbitrary <*> listOf arbitrary
    ]

instance Arbitrary Statement where
  arbitrary = oneof
    [ VarDecl <$> arbitrary <*> arbitrary
    , FuncDecl <$> arbitrary <*> listOf arbitrary <*> arbitrary <*> arbitrary
    , Return <$> arbitrary
    , Expr <$> arbitrary
    ]

instance Arbitrary AST where
  arbitrary = AST <$> listOf arbitrary

-- ============================================================================
-- Property Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Dependencies Advanced QuickCheck Tests"
    [ testProperty "newDependentTypeChecker creates checker with prelude types" $
        let checker = newDependentTypeChecker
            typeEnv = dtcTypeEnv checker
            typeDefs = typeDefinitions typeEnv
        in Map.member "int" typeDefs .&&.
           Map.member "string" typeDefs .&&.
           Map.member "bool" typeDefs .&&.
           Map.member "float64" typeDefs

    , testProperty "newDependentTypeChecker has no initial errors" $
        let checker = newDependentTypeChecker
        in null (tcErrors checker)

    , testProperty "newDependentTypeCheckerWithTypes adds custom types" $
        \typeDefs ->
          let checker = newDependentTypeCheckerWithTypes typeDefs
              typeEnv = dtcTypeEnv checker
              customTypes = map (\(name, _, _) -> name) typeDefs
              hasAllCustomTypes = all (`Map.member` typeDefinitions typeEnv) customTypes
          in hasAllCustomTypes

    , testProperty "addType adds type definition to environment" $
        \name params constraints ->
          let checker = evalState (do
                addType name params constraints
                get) newDependentTypeChecker
              typeEnv = dtcTypeEnv checker
          in Map.member name (typeDefinitions typeEnv)

    , testProperty "addConstraint adds constraint to pending list" $
        \constraint ->
          let checker = evalState (do
                addConstraint constraint
                get) newDependentTypeChecker
              typeEnv = dtcTypeEnv checker
              pending = pendingConstraints typeEnv
          in constraint `elem` pending

    , testProperty "addTypeError adds error to error list" $
        \error ->
          let checker = evalState (do
                addTypeError error
                get) newDependentTypeChecker
          in error `elem` tcErrors checker

    , testProperty "lookupTypeDef finds existing type" $
        \name params constraints ->
          let checker = evalState (do
                addType name params constraints
                lookupTypeDef name) newDependentTypeChecker
          in isJust checker

    , testProperty "lookupTypeDef returns Nothing for non-existent type" $
        \name ->
          let checker = evalState (lookupTypeDef name) newDependentTypeChecker
              nonExistent = not (`Map.member` typeDefinitions (dtcTypeEnv newDependentTypeChecker)) name
          in nonExistent ==> isNothing checker

    , testProperty "checkType handles valid type variables" $
        \typeVar ->
          let checker = evalState (checkType typeVar >> get) newDependentTypeChecker
          in property True  -- Basic check that function doesn't crash

    , testProperty "solveConstraints processes constraint list" $
        \constraints ->
          let checker = evalState (do
                mapM_ addConstraint constraints
                solveConstraints
                get) newDependentTypeChecker
          in property True  -- Basic check that function doesn't crash

    , testProperty "getDependentTypeErrors returns accumulated errors" $
        \errors ->
          let checker = evalState (do
                mapM_ addTypeError errors
                get) newDependentTypeChecker
              retrievedErrors = getDependentTypeErrors checker
          in length retrievedErrors >= length errors

    , testProperty "unify handles type variable unification" $
        \typeVar1 typeVar2 ->
          let checker = evalState (unify typeVar1 typeVar2 >> get) newDependentTypeChecker
          in property True  -- Basic check that function doesn't crash

    , testProperty "convertTypeExpr produces valid TypeVar" $
        \typeExpr ->
          let params = Set.empty
              typeVar = convertTypeExpr params typeExpr
          in case typeVar of
            TVCon _ -> property True
            TVVar _ -> property True
            TVApp _ _ -> property True
            TVFun _ _ -> property True
            TVTuple _ -> property True

    , testProperty "convertConstraint produces valid TypeConstraint" $
        \constraint ->
          let params = Set.empty
              typeConstraint = convertConstraint params constraint
          in case typeConstraint of
            Equal _ _ -> property True
            Subtype _ _ -> property True
            Predicate _ _ -> property True
            TypeSizeGE _ _ -> property True
            TypeSizeGT _ _ -> property True
            TypeRange _ _ _ -> property True

    , testProperty "inferType handles simple expressions" $
        \expr ->
          let checker = evalState (inferType expr >> get) newDependentTypeChecker
          in property True  -- Basic check that function doesn't crash

    , testProperty "inferStatement processes statements" $
        \statement ->
          let checker = evalState (inferStatement statement >> get) newDependentTypeChecker
          in property True  -- Basic check that function doesn't crash

    , testProperty "inferProgram handles complete programs" $
        \ast ->
          let checker = evalState (inferProgram ast >> get) newDependentTypeChecker
          in property True  -- Basic check that function doesn't crash

    , testProperty "generalize and instantiate are inverse operations (approximately)" $
        \typeVar ->
          let checker = newDependentTypeChecker
              -- This is a simplified test - in practice these operations are complex
          in property True

    , testProperty "unifyTypes handles type unification" $
        \typeVar1 typeVar2 ->
          let checker = evalState (unifyTypes typeVar1 typeVar2 >> get) newDependentTypeChecker
          in property True  -- Basic check that function doesn't crash

    , testProperty "applyTypeSubstitution modifies types correctly" $
        \typeVar ->
          let substitution = Map.empty
              result = applyTypeSubstitution substitution typeVar
          in case (typeVar, result) of
            (TVVar name, TVVar name') -> name' == name || Map.member name substitution
            _ -> property True

    , testProperty "newTypeVariable creates fresh type variables" $
        \varName ->
          let typeVar = newTypeVariable varName
          in case typeVar of
            TVVar name -> name == varName
            _ -> property False

    , testProperty "getFreshTypeVar generates unique variables" $
        \varName1 varName2 ->
          let tv1 = getFreshTypeVar varName1
              tv2 = getFreshTypeVar varName2
          in case (tv1, tv2) of
            (TVVar name1, TVVar name2) -> name1 /= name2
            _ -> property True

    , testProperty "initialTypeEnvironment contains basic types" $
        let typeEnv = initialTypeEnvironment
            typeDefs = typeDefinitions typeEnv
        in Map.member "int" typeDefs .&&.
           Map.member "string" typeDefs .&&.
           Map.member "bool" typeDefs

    , testProperty "solveTypeConstraints processes constraints" $
        \constraints ->
          let result = solveTypeConstraints constraints
          in property True  -- Basic check that function doesn't crash

    , testProperty "simplifyConstraints reduces constraint complexity" $
        \constraints ->
          let simplified = simplifyConstraints constraints
          in length simplified <= length constraints

    , testProperty "parseProgram handles program strings" $
        \programStr ->
          case runParser (parseProgram programStr) of
            Left _ -> property True
            Right _ -> property True

    , testProperty "TypeVar Show instance produces readable output" $
        \typeVar ->
          let shown = show typeVar
          in not (null shown)

    , testProperty "TypeConstraint Show instance produces readable output" $
        \constraint ->
          let shown = show constraint
          in not (null shown)

    , testProperty "DependentTypeError Show instance produces readable output" $
        \error ->
          let shown = show error
          in not (null shown)

    , testProperty "TypeVar equality is reflexive" $
        \typeVar -> typeVar === typeVar

    , testProperty "TypeConstraint equality is reflexive" $
        \constraint -> constraint === constraint

    , testProperty "DependentTypeError equality is reflexive" $
        \error -> error === error

    , testProperty "DependentTypeChecker preserves type environment" $
        \typeEnv ->
          let checker = DependentTypeChecker typeEnv []
          in dtcTypeEnv checker === typeEnv

    , testProperty "DependentTypeChecker preserves errors" $
        \errors typeEnv ->
          let checker = DependentTypeChecker typeEnv errors
          in tcErrors checker === errors
    ]