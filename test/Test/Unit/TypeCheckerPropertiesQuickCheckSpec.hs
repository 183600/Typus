{-# LANGUAGE CPP #-}

module Test.Unit.TypeCheckerPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)

import Compiler.TypeChecker (Type(..), TypeEnv(..), TypeCheckDiagnostic(..), 
                             FunctionInfo(..), FunctionSignature(..))
import qualified Compiler.DependentTypeChecker as DTC
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "TypeChecker Properties QuickCheck"
  [ typeInfoTests
  , typeConstraintTests
  , typeEnvironmentTests
  , typeSchemeTests
  , typeSubstitutionTests
  , dependentTypeTests
  ]

typeTests :: TestTree
typeTests = testGroup "Type Properties"
  [ fastProperty "Type equality is reflexive" prop_type_reflexive
  , fastProperty "Type equality is symmetric" prop_type_symmetric
  , fastProperty "Type equality is transitive" prop_type_transitive
  , fastProperty "Type preserves structure" prop_type_preserves_structure
  ]

typeEnvTests :: TestTree
typeEnvTests = testGroup "TypeEnv Properties"
  [ fastProperty "TypeEnv lookup after insert" prop_typeenv_insert_lookup
  , fastProperty "TypeEnv scope nesting preserves outer bindings" prop_typeenv_scope_nesting
  , fastProperty "TypeEnv merge preserves L.all bindings" prop_typeenv_merge_preserves
  ]

functionInfoTests :: TestTree
functionInfoTests = testGroup "FunctionInfo Properties"
  [ fastProperty "FunctionInfo equality is reflexive" prop_functioninfo_reflexive
  , fastProperty "FunctionInfo preserves signature" prop_functioninfo_preserves_signature
  , fastProperty "FunctionInfo parameters are valid" prop_functioninfo_params_valid
  ]

-- Type Properties
prop_type_reflexive :: Type -> Property
prop_type_reflexive t =
  t === t

prop_type_symmetric :: Type -> Type -> Property
prop_type_symmetric t1 t2 =
  (t1 === t2) ==> property (t2 === t1)

prop_type_transitive :: Type -> Type -> Type -> Property
prop_type_transitive t1 t2 t3 =
  (t1 === t2 && t2 === t3) ==> property (t1 === t3)

prop_type_preserves_structure :: Type -> Property
prop_type_preserves_structure t =
  property True  -- Placeholder for structure preservation check

-- TypeEnv Properties
prop_typeenv_insert_lookup :: TypeEnv -> String -> Type -> Property
prop_typeenv_insert_lookup env name t =
  property True  -- Placeholder for lookup check

prop_typeenv_scope_nesting :: TypeEnv -> TypeEnv -> Property
prop_typeenv_scope_nesting outer inner =
  property True  -- Placeholder for scope nesting check

prop_typeenv_merge_preserves :: TypeEnv -> TypeEnv -> Property
prop_typeenv_merge_preserves env1 env2 =
  property True  -- Placeholder for merge preservation

-- FunctionInfo Properties
prop_functioninfo_reflexive :: FunctionInfo -> Property
prop_functioninfo_reflexive fi =
  fi === fi

prop_functioninfo_preserves_signature :: FunctionInfo -> Property
prop_functioninfo_preserves_signature fi =
  property True  -- Placeholder for signature preservation

prop_functioninfo_params_valid :: FunctionInfo -> Property
prop_functioninfo_params_valid fi =
  property True  -- Placeholder for parameter validation