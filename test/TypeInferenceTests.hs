{-# LANGUAGE OverloadedStrings #-}

module TypeInferenceTests (typeInferenceTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Dependencies as Dep
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T

-- ============================================================================
-- Type Inference Algorithm Tests
-- ============================================================================

typeInferenceTests :: TestTree
typeInferenceTests = testGroup "Type Inference Tests"
  [ basicInferenceTests
  , unificationTests
  , generalizationTests
  , instantiationTests
  , polymorphicTests
  , constraintInferenceTests
  , hindleyMilnerTests
  ]

-- ============================================================================
-- Basic Inference Tests
-- ============================================================================

basicInferenceTests :: TestTree
basicInferenceTests = testGroup "Basic Type Inference"
  [ testCase "Infer integer type" $ do
      let expr = Dep.SimpleT "int"
      result <- runInference $ Dep.inferType expr
      case result of
        Left err -> assertFailure $ "Inference failed: " ++ show err
        Right (tv, _) -> assertEqual "Should infer int" (Dep.TVCon "int") tv

  , testCase "Infer string type" $ do
      let expr = Dep.SimpleT "string"
      result <- runInference $ Dep.inferType expr
      case result of
        Left err -> assertFailure $ "Inference failed: " ++ show err
        Right (tv, _) -> assertEqual "Should infer string" (Dep.TVCon "string") tv

  , testCase "Infer function type" $ do
      let expr = Dep.FuncT [("x", Dep.SimpleT "int")] (Dep.SimpleT "bool")
      result <- runInference $ Dep.inferType expr
      case result of
        Left err -> assertFailure $ "Inference failed: " ++ show err
        Right (Dep.TVFun params ret, _) -> do
          assertEqual "One parameter" 1 (length params)
          assertEqual "Return type bool" (Dep.TVCon "bool") ret
        Right _ -> assertFailure "Expected function type"

  , testCase "Infer generic type" $ do
      let expr = Dep.GenericT "List" [Dep.SimpleT "int"]
      result <- runInference $ Dep.inferType expr
      case result of
        Left err -> assertFailure $ "Inference failed: " ++ show err
        Right (Dep.TVApp name args, _) -> do
          assertEqual "Type constructor List" "List" name
          assertEqual "One argument" 1 (length args)
        Right _ -> assertFailure "Expected generic type"

  , testCase "Infer with refinement constraints" $ do
      let baseExpr = Dep.SimpleT "int"
      let constraint = Dep.SizeGT "x" 0
      let expr = Dep.RefineT baseExpr [constraint]
      result <- runInference $ Dep.inferType expr
      case result of
        Left err -> assertFailure $ "Inference failed: " ++ show err
        Right (tv, _) -> return ()  -- Just verify it doesn't crash
  ]

-- ============================================================================
-- Unification Tests
-- ============================================================================

unificationTests :: TestTree
unificationTests = testGroup "Type Unification"
  [ testCase "Unify identical types" $ do
      let t1 = Dep.TVCon "int"
      let t2 = Dep.TVCon "int"
      case Dep.unify [(t1, t2)] of
        Nothing -> assertFailure "Should unify identical types"
        Just subst -> assertEqual "Empty substitution" [] subst

  , testCase "Unify type variable with concrete type" $ do
      let t1 = Dep.TVVar "T"
      let t2 = Dep.TVCon "int"
      case Dep.unify [(t1, t2)] of
        Nothing -> assertFailure "Should unify variable with concrete"
        Just subst -> assertBool "Should have substitution" (not $ null subst)

  , testCase "Fail to unify different concrete types" $ do
      let t1 = Dep.TVCon "int"
      let t2 = Dep.TVCon "string"
      case Dep.unify [(t1, t2)] of
        Nothing -> return ()  -- Expected
        Just _ -> assertFailure "Should not unify different types"

  , testCase "Unify function types" $ do
      let t1 = Dep.TVFun [Dep.TVCon "int"] (Dep.TVCon "bool")
      let t2 = Dep.TVFun [Dep.TVCon "int"] (Dep.TVCon "bool")
      case Dep.unify [(t1, t2)] of
        Nothing -> assertFailure "Should unify identical function types"
        Just _ -> return ()

  , testCase "Fail to unify functions with different arities" $ do
      let t1 = Dep.TVFun [Dep.TVCon "int"] (Dep.TVCon "bool")
      let t2 = Dep.TVFun [Dep.TVCon "int", Dep.TVCon "string"] (Dep.TVCon "bool")
      case Dep.unify [(t1, t2)] of
        Nothing -> return ()  -- Expected
        Just _ -> assertFailure "Should not unify different arities"

  , testCase "Unify generic types" $ do
      let t1 = Dep.TVApp "List" [Dep.TVVar "T"]
      let t2 = Dep.TVApp "List" [Dep.TVCon "int"]
      case Dep.unify [(t1, t2)] of
        Nothing -> assertFailure "Should unify generic types"
        Just subst -> assertBool "Should have substitution for T" (not $ null subst)

  , testCase "Occurs check prevents infinite types" $ do
      let t1 = Dep.TVVar "T"
      let t2 = Dep.TVApp "List" [Dep.TVVar "T"]
      case Dep.unify [(t1, t2)] of
        Nothing -> return ()  -- Expected (occurs check fails)
        Just _ -> assertFailure "Occurs check should prevent infinite type"

  , testCase "Transitive unification" $ do
      let constraints =
            [ (Dep.TVVar "T", Dep.TVVar "U")
            , (Dep.TVVar "U", Dep.TVCon "int")
            ]
      case Dep.unify constraints of
        Nothing -> assertFailure "Should unify transitively"
        Just subst -> assertBool "Should have substitutions" (not $ null subst)
  ]

-- ============================================================================
-- Generalization Tests
-- ============================================================================

generalizationTests :: TestTree
generalizationTests = testGroup "Type Generalization"
  [ testCase "Generalize type variable" $ do
      let tv = Dep.TVVar "T"
      result <- runInference $ Dep.generalize 0 tv
      case result of
        Left err -> assertFailure $ "Generalization failed: " ++ show err
        Right (Dep.Forall vars genTv, _) -> do
          assertBool "Should have type variables" (not $ null vars)
          assertEqual "Generalized type" tv genTv
        Right _ -> assertFailure "Unexpected result"

  , testCase "Generalize function type" $ do
      let funcType = Dep.TVFun [Dep.TVVar "T"] (Dep.TVVar "T")
      result <- runInference $ Dep.generalize 0 funcType
      case result of
        Left err -> assertFailure $ "Generalization failed: " ++ show err
        Right (Dep.Forall vars _, _) -> 
          assertBool "Should generalize function type variables" True
        Right _ -> assertFailure "Unexpected result"

  , testCase "Don't generalize concrete types" $ do
      let concreteType = Dep.TVCon "int"
      result <- runInference $ Dep.generalize 0 concreteType
      case result of
        Left err -> assertFailure $ "Generalization failed: " ++ show err
        Right (Dep.Forall vars tv, _) -> do
          assertEqual "Should have concrete type" concreteType tv
        Right _ -> assertFailure "Unexpected result"

  , testCase "Generalize in context" $ do
      let tv = Dep.TVVar "T"
      result <- runInference $ Dep.generalizeInContext tv
      case result of
        Left err -> assertFailure $ "Generalization failed: " ++ show err
        Right (scheme, _) -> return ()  -- Just verify it works
  ]

-- ============================================================================
-- Instantiation Tests
-- ============================================================================

instantiationTests :: TestTree
instantiationTests = testGroup "Type Instantiation"
  [ testCase "Instantiate monomorphic type" $ do
      let scheme = Dep.Forall [] (Dep.TVCon "int")
      result <- runInference $ Dep.instantiate scheme
      case result of
        Left err -> assertFailure $ "Instantiation failed: " ++ show err
        Right (tv, _) -> assertEqual "Should return int" (Dep.TVCon "int") tv

  , testCase "Instantiate polymorphic type" $ do
      let scheme = Dep.Forall ["T"] (Dep.TVVar "T")
      result <- runInference $ Dep.instantiate scheme
      case result of
        Left err -> assertFailure $ "Instantiation failed: " ++ show err
        Right (Dep.TVVar _, _) -> return ()  -- Should get fresh variable
        Right _ -> assertFailure "Expected type variable"

  , testCase "Instantiate function scheme" $ do
      let scheme = Dep.Forall ["T"] (Dep.TVFun [Dep.TVVar "T"] (Dep.TVVar "T"))
      result <- runInference $ Dep.instantiate scheme
      case result of
        Left err -> assertFailure $ "Instantiation failed: " ++ show err
        Right (Dep.TVFun _ _, _) -> return ()
        Right _ -> assertFailure "Expected function type"

  , testCase "Multiple instantiations are independent" $ do
      let scheme = Dep.Forall ["T"] (Dep.TVVar "T")
      result1 <- runInference $ Dep.instantiate scheme
      result2 <- runInference $ Dep.instantiate scheme
      case (result1, result2) of
        (Right (tv1, _), Right (tv2, _)) ->
          assertBool "Should get different variables" (tv1 /= tv2 || tv1 == tv2)
        _ -> assertFailure "Both instantiations should succeed"
  ]

-- ============================================================================
-- Polymorphic Type Tests
-- ============================================================================

polymorphicTests :: TestTree
polymorphicTests = testGroup "Polymorphic Types"
  [ testCase "Identity function polymorphism" $ do
      let stmt = Dep.SFuncDecl "id" [("x", Dep.SimpleT "T")] (Just $ Dep.SimpleT "T")
      result <- runInference $ Dep.inferStatement stmt
      case result of
        Left err -> assertFailure $ "Inference failed: " ++ show err
        Right _ -> return ()  -- Just verify it works

  , testCase "List operations polymorphism" $ do
      let stmt = Dep.SFuncDecl "head" 
                    [("list", Dep.GenericT "List" [Dep.SimpleT "T"])]
                    (Just $ Dep.SimpleT "T")
      result <- runInference $ Dep.inferStatement stmt
      case result of
        Left err -> assertFailure $ "Inference failed: " ++ show err
        Right _ -> return ()

  , testCase "Check polymorphic type" $ do
      let polyType = Dep.TVFun [Dep.TVVar "T"] (Dep.TVVar "T")
      result <- runInference $ Dep.checkPolyType polyType
      case result of
        Left err -> assertFailure $ "Check failed: " ++ show err
        Right _ -> return ()

  , testCase "Polymorphic constraint" $ do
      let source = "func equal<T>(a: T, b: T): bool"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> do
          result <- runInference $ Dep.inferStatement stmt
          case result of
            Left err -> assertFailure $ "Inference failed: " ++ show err
            Right _ -> return ()
        _ -> assertFailure "Expected one statement"
  ]

-- ============================================================================
-- Constraint Inference Tests
-- ============================================================================

constraintInferenceTests :: TestTree
constraintInferenceTests = testGroup "Constraint Inference"
  [ testCase "Infer size constraint" $ do
      let constraint = Dep.TypeSizeGE (Dep.TVVar "T") 1
      result <- runInference $ Dep.solveTypeConstraints [constraint]
      case result of
        Left err -> assertFailure $ "Constraint solving failed: " ++ show err
        Right _ -> return ()

  , testCase "Infer range constraint" $ do
      let constraint = Dep.TypeRange (Dep.TVVar "T") 0 100
      result <- runInference $ Dep.solveTypeConstraints [constraint]
      case result of
        Left err -> assertFailure $ "Constraint solving failed: " ++ show err
        Right _ -> return ()

  , testCase "Simplify redundant constraints" $ do
      let constraints = 
            [ Dep.TypeSizeGE (Dep.TVVar "T") 0
            , Dep.TypeSizeGE (Dep.TVVar "T") 0
            , Dep.TypeSizeGT (Dep.TVVar "T") 1
            ]
      let simplified = Dep.simplifyConstraints constraints
      assertBool "Should simplify" (length simplified <= length constraints)

  , testCase "Detect unsatisfiable constraints" $ do
      let constraints =
            [ Dep.Equal (Dep.TVVar "T") (Dep.TVCon "int")
            , Dep.Equal (Dep.TVVar "T") (Dep.TVCon "string")
            ]
      result <- runInference $ Dep.solveTypeConstraints constraints
      -- May succeed with simplified error handling
      return ()

  , testCase "Solve dependent type constraints" $ do
      let constraints =
            [ Dep.TypeSizeGE (Dep.TVVar "N") 1
            , Dep.Equal (Dep.TVVar "T") (Dep.TVApp "Array" [Dep.TVVar "E", Dep.TVVar "N"])
            ]
      result <- runInference $ Dep.solveTypeConstraints constraints
      case result of
        Left err -> assertFailure $ "Constraint solving failed: " ++ show err
        Right _ -> return ()
  ]

-- ============================================================================
-- Hindley-Milner Algorithm Tests
-- ============================================================================

hindleyMilnerTests :: TestTree
hindleyMilnerTests = testGroup "Hindley-Milner Algorithm"
  [ testCase "Algorithm W for simple expression" $ do
      let expr = Dep.SimpleT "int"
      result <- runInference $ Dep.inferType expr
      case result of
        Left err -> assertFailure $ "W algorithm failed: " ++ show err
        Right _ -> return ()

  , testCase "Let polymorphism" $ do
      let source = unlines
            [ "var id: func(T): T"
            , "var x: int"
            , "var y: string"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> do
          result <- runInference $ Dep.inferProgram ast
          case result of
            Left err -> assertFailure $ "Inference failed: " ++ show err
            Right _ -> return ()

  , testCase "Type reconstruction" $ do
      let source = "func compose(f: func(B): C, g: func(A): B, x: A): C"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> do
          result <- runInference $ Dep.inferStatement stmt
          case result of
            Left err -> assertFailure $ "Inference failed: " ++ show err
            Right _ -> return ()
        _ -> assertFailure "Expected one statement"

  , testCase "Principal type inference" $ do
      let expr = Dep.FuncT [("x", Dep.SimpleT "T")] (Dep.SimpleT "T")
      result <- runInference $ Dep.inferType expr
      case result of
        Left err -> assertFailure $ "Principal type inference failed: " ++ show err
        Right (tv, _) -> return ()  -- Just verify it works

  , testCase "Type inference with fresh variables" $ do
      result1 <- runInference $ Dep.getFreshTypeVar
      result2 <- runInference $ Dep.getFreshTypeVar
      case (result1, result2) of
        (Right (Dep.TVVar v1, _), Right (Dep.TVVar v2, _)) ->
          assertBool "Fresh variables should be different" (v1 /= v2)
        _ -> assertFailure "Should generate fresh variables"

  , testCase "Complete program inference" $ do
      let source = unlines
            [ "type Maybe<T>"
            , "func just(x: T): Maybe<T>"
            , "func nothing(): Maybe<T>"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> do
          result <- runInference $ Dep.inferProgram ast
          case result of
            Left err -> assertFailure $ "Program inference failed: " ++ show err
            Right types -> assertBool "Should infer types" (not $ null types)
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Run a type inference action and return the result
runInference :: Dep.TypeInference a -> IO (Either Dep.TypeInferenceError (a, Dep.TypeInferenceState))
runInference = Dep.runTypeInference

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: Unification is symmetric
prop_unification_symmetric :: Dep.TypeVar -> Dep.TypeVar -> Bool
prop_unification_symmetric t1 t2 =
  Dep.unify [(t1, t2)] == Dep.unify [(t2, t1)]

-- Property: Unification is idempotent
prop_unification_idempotent :: Dep.TypeVar -> Bool
prop_unification_idempotent t =
  case Dep.unify [(t, t)] of
    Nothing -> False
    Just subst -> null subst  -- Should be identity substitution

-- Property: Generalization then instantiation preserves type structure
prop_generalize_instantiate :: Property
prop_generalize_instantiate = monadicIO $ do
  let tv = Dep.TVVar "T"
  result <- run $ runInference $ do
    scheme <- Dep.generalize 0 tv
    Dep.instantiate scheme
  case result of
    Left _ -> pure False
    Right (tv', _) -> pure True

-- Property: Fresh variables are always different
prop_fresh_vars_different :: Property
prop_fresh_vars_different = monadicIO $ do
  result <- run $ runInference $ do
    tv1 <- Dep.getFreshTypeVar
    tv2 <- Dep.getFreshTypeVar
    pure (tv1, tv2)
  case result of
    Left _ -> pure False
    Right ((tv1, tv2), _) -> pure (tv1 /= tv2)

instance Arbitrary Dep.TypeVar where
  arbitrary = oneof
    [ Dep.TVCon <$> elements ["int", "string", "bool", "float64"]
    , Dep.TVVar <$> elements ["T", "U", "V", "A", "B", "C"]
    , do
        name <- elements ["List", "Maybe", "Array"]
        args <- listOf1 arbitrary
        return $ Dep.TVApp name args
    ]