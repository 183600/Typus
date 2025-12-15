{-# LANGUAGE CPP #-}

module Test.Unit.TypeSystemPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Monad.State (execState, evalState)

import Dependencies.TypeSystem
import Dependencies.AST (TypeExpr(..))

prop_typevar_equality_reflexive :: Property
prop_typevar_equality_reflexive =
  forAll genTypeVar $ \tv ->
  tv === tv
  where
    genTypeVar = elements [TVCon "Int", TVCon "String", TVVar "a", TVVar "b"]

prop_typevar_equality_symmetric :: Property
prop_typevar_equality_symmetric =
  forAll genTypeVarPair $ \(tv1, tv2) ->
  (tv1 == tv2) === (tv2 == tv1)
  where
    genTypeVarPair = do
      tv1 <- elements [TVCon "Int", TVCon "String", TVVar "a"]
      tv2 <- elements [TVCon "Int", TVCon "String", TVVar "a"]
      return (tv1, tv2)

prop_convertTypeExpr_simple :: Property
prop_convertTypeExpr_simple =
  forAll genSimpleTypeExpr $ \texpr ->
  let tv = convertTypeExpr Set.empty texpr
  in property True
  where
    genSimpleTypeExpr = elements [SimpleT (T.pack "Int"), SimpleT (T.pack "String"), SimpleT (T.pack "Bool")]

prop_newDependentTypeChecker_has_prelude :: Property
prop_newDependentTypeChecker_has_prelude =
  let checker = newDependentTypeChecker
      preludeNames = ["int", "string", "bool", "float64"]
      hasType name = case evalState (lookupTypeDef name) checker of
                       Just _ -> True
                       Nothing -> False
  in all hasType preludeNames === True

prop_getDependentTypeErrors_initially_empty :: Property
prop_getDependentTypeErrors_initially_empty =
  let checker = newDependentTypeChecker
  in getDependentTypeErrors checker === []

prop_typeConstraint_equality :: Property
prop_typeConstraint_equality =
  let c1 = Equal (TVCon "Int") (TVCon "Int")
      c2 = Equal (TVCon "Int") (TVCon "Int")
  in c1 === c2

prop_typeDef_equality :: Property
prop_typeDef_equality =
  let td1 = TypeDefDecl [] []
      td2 = TypeDefDecl [] []
  in td1 === td2

prop_typeEnv_has_definitions :: Property
prop_typeEnv_has_definitions =
  let checker = newDependentTypeChecker
      env = dtcTypeEnv checker
      defs = typeDefinitions env
  in not (Map.null defs) === True

tests :: TestTree
tests = testGroup "TypeSystem Properties QuickCheck Tests"
  [ fastProperty "TypeVar equality is reflexive" prop_typevar_equality_reflexive
  , fastProperty "TypeVar equality is symmetric" prop_typevar_equality_symmetric
  , fastProperty "convertTypeExpr handles simple types" prop_convertTypeExpr_simple
  , fastProperty "newDependentTypeChecker has prelude types" prop_newDependentTypeChecker_has_prelude
  , fastProperty "getDependentTypeErrors initially empty" prop_getDependentTypeErrors_initially_empty
  , fastProperty "TypeConstraint equality works" prop_typeConstraint_equality
  , fastProperty "TypeDef equality works" prop_typeDef_equality
  , fastProperty "TypeEnv has definitions" prop_typeEnv_has_definitions
  ]
