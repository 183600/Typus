{-# LANGUAGE OverloadedStrings #-}

module TypeSystemTests (typeSystemTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Dependencies as Dep
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State

-- ============================================================================
-- Type System Unit Tests
-- ============================================================================

typeSystemTests :: TestTree
typeSystemTests = testGroup "Type System Tests"
  [ basicTypeCheckingTests
  , typeDefinitionTests
  , typeInstantiationTests
  , typeConstraintTests
  , typeEnvironmentTests
  , typeVariableTests
  ]

-- ============================================================================
-- Basic Type Checking Tests
-- ============================================================================

basicTypeCheckingTests :: TestTree
basicTypeCheckingTests = testGroup "Basic Type Checking"
  [ testCase "Check primitive types exist" $ do
      let checker = Dep.newDependentTypeChecker
      let env = Dep.dtcTypeEnv checker
      let typeDefs = Dep.typeDefinitions env
      assertBool "int type should exist" (Map.member "int" typeDefs)
      assertBool "string type should exist" (Map.member "string" typeDefs)
      assertBool "bool type should exist" (Map.member "bool" typeDefs)
      assertBool "float64 type should exist" (Map.member "float64" typeDefs)

  , testCase "Check concrete type" $ do
      let checker = Dep.newDependentTypeChecker
      let ((), checker') = runState (Dep.checkType (Dep.TVCon "int")) checker
      assertEqual "No errors for valid type" [] (Dep.tcErrors checker')

  , testCase "Reject undefined type" $ do
      let checker = Dep.newDependentTypeChecker
      let ((), checker') = runState (Dep.checkType (Dep.TVCon "undefined")) checker
      assertBool "Should have error for undefined type" (not $ null $ Dep.tcErrors checker')

  , testCase "Check function type" $ do
      let checker = Dep.newDependentTypeChecker
      let funcType = Dep.TVFun [Dep.TVCon "int", Dep.TVCon "string"] (Dep.TVCon "bool")
      let ((), checker') = runState (Dep.checkType funcType) checker
      assertEqual "No errors for valid function type" [] (Dep.tcErrors checker')

  , testCase "Reject function with invalid parameter types" $ do
      let checker = Dep.newDependentTypeChecker
      let funcType = Dep.TVFun [Dep.TVCon "undefined"] (Dep.TVCon "bool")
      let ((), checker') = runState (Dep.checkType funcType) checker
      assertBool "Should have error for invalid parameter" (not $ null $ Dep.tcErrors checker')

  , testCase "Check tuple type" $ do
      let checker = Dep.newDependentTypeChecker
      let tupleType = Dep.TVTuple [Dep.TVCon "int", Dep.TVCon "string"]
      let ((), checker') = runState (Dep.checkType tupleType) checker
      assertEqual "No errors for valid tuple type" [] (Dep.tcErrors checker')
  ]

-- ============================================================================
-- Type Definition Tests
-- ============================================================================

typeDefinitionTests :: TestTree
typeDefinitionTests = testGroup "Type Definition"
  [ testCase "Add simple type definition" $ do
      let checker = Dep.newDependentTypeChecker
      let ((), checker') = runState (Dep.addType "MyType" [] []) checker
      let env = Dep.dtcTypeEnv checker'
      assertBool "MyType should be defined" (Map.member "MyType" (Dep.typeDefinitions env))

  , testCase "Add generic type definition" $ do
      let checker = Dep.newDependentTypeChecker
      let ((), checker') = runState (Dep.addType "List" ["T"] []) checker
      let env = Dep.dtcTypeEnv checker'
      case Map.lookup "List" (Dep.typeDefinitions env) of
        Nothing -> assertFailure "List type not found"
        Just typeDef -> assertEqual "Should have one type parameter" 1 (length $ Dep.tdParams typeDef)

  , testCase "Add type with constraints" $ do
      let checker = Dep.newDependentTypeChecker
      let constraint = Dep.TypeSizeGE (Dep.TVVar "T") 1
      let ((), checker') = runState (Dep.addType "NonEmpty" ["T"] [constraint]) checker
      let env = Dep.dtcTypeEnv checker'
      case Map.lookup "NonEmpty" (Dep.typeDefinitions env) of
        Nothing -> assertFailure "NonEmpty type not found"
        Just typeDef -> do
          assertEqual "Should have one type parameter" 1 (length $ Dep.tdParams typeDef)
          assertEqual "Should have one constraint" 1 (length $ Dep.tdConstraints typeDef)

  , testCase "Type definition with multiple parameters" $ do
      let checker = Dep.newDependentTypeChecker
      let ((), checker') = runState (Dep.addType "Map" ["K", "V"] []) checker
      let env = Dep.dtcTypeEnv checker'
      case Map.lookup "Map" (Dep.typeDefinitions env) of
        Nothing -> assertFailure "Map type not found"
        Just typeDef -> assertEqual "Should have two type parameters" 2 (length $ Dep.tdParams typeDef)
  ]

-- ============================================================================
-- Type Instantiation Tests
-- ============================================================================

typeInstantiationTests :: TestTree
typeInstantiationTests = testGroup "Type Instantiation"
  [ testCase "Instantiate simple generic type" $ do
      let checker = Dep.newDependentTypeChecker
      let checker1 = execState (Dep.addType "List" ["T"] []) checker
      let ((), checker') = runState (Dep.checkTypeInstantiation "List" [Dep.TVCon "int"]) checker1
      assertEqual "No errors for valid instantiation" [] (Dep.tcErrors checker')

  , testCase "Reject instantiation with wrong arity" $ do
      let checker = Dep.newDependentTypeChecker
      let checker1 = execState (Dep.addType "List" ["T"] []) checker
      let ((), checker') = runState (Dep.checkTypeInstantiation "List" [Dep.TVCon "int", Dep.TVCon "string"]) checker1
      assertBool "Should have error for wrong arity" (not $ null $ Dep.tcErrors checker')

  , testCase "Instantiate with type variable" $ do
      let checker = Dep.newDependentTypeChecker
      let checker1 = execState (Dep.addType "List" ["T"] []) checker
      let ((), checker') = runState (Dep.checkTypeInstantiation "List" [Dep.TVVar "U"]) checker1
      assertEqual "No errors for type variable instantiation" [] (Dep.tcErrors checker')

  , testCase "Check nested generic instantiation" $ do
      let checker = Dep.newDependentTypeChecker
      let checker1 = execState (Dep.addType "List" ["T"] []) checker
      let listOfLists = Dep.TVApp "List" [Dep.TVApp "List" [Dep.TVCon "int"]]
      let ((), checker') = runState (Dep.checkType listOfLists) checker1
      assertEqual "No errors for nested instantiation" [] (Dep.tcErrors checker')
  ]

-- ============================================================================
-- Type Constraint Tests
-- ============================================================================

typeConstraintTests :: TestTree
typeConstraintTests = testGroup "Type Constraints"
  [ testCase "Size constraint >= 0" $ do
      let constraint = Dep.TypeSizeGE (Dep.TVCon "int") 0
      assertEqual "Valid size constraint" (Right ()) (Dep.validateConstraint constraint)

  , testCase "Size constraint > 0" $ do
      let constraint = Dep.TypeSizeGT (Dep.TVCon "int") 1
      assertEqual "Valid size constraint" (Right ()) (Dep.validateConstraint constraint)

  , testCase "Range constraint" $ do
      let constraint = Dep.TypeRange (Dep.TVCon "int") 0 10
      assertEqual "Valid range constraint" (Right ()) (Dep.validateConstraint constraint)

  , testCase "Invalid range constraint" $ do
      let constraint = Dep.TypeRange (Dep.TVCon "int") 10 0
      case Dep.validateConstraint constraint of
        Left _ -> return ()
        Right _ -> assertFailure "Should reject invalid range"

  , testCase "Equality constraint" $ do
      let constraint = Dep.Equal (Dep.TVCon "int") (Dep.TVCon "int")
      assertEqual "Valid equality constraint" (Right ()) (Dep.validateConstraint constraint)

  , testCase "Inequality constraint" $ do
      let constraint = Dep.Equal (Dep.TVCon "int") (Dep.TVCon "string")
      case Dep.validateConstraint constraint of
        Left _ -> return ()
        Right _ -> assertFailure "Should reject type mismatch"

  , testCase "Predicate constraint" $ do
      let constraint = Dep.Predicate "IsPositive" [Dep.TVCon "int"]
      assertEqual "Predicate constraints accepted" (Right ()) (Dep.validateConstraint constraint)

  , testCase "Add constraint to environment" $ do
      let checker = Dep.newDependentTypeChecker
      let constraint = Dep.TypeSizeGE (Dep.TVVar "T") 1
      let ((), checker') = runState (Dep.addConstraint constraint) checker
      let env = Dep.dtcTypeEnv checker'
      assertBool "Constraint added to pending" (constraint `elem` Dep.pendingConstraints env)
  ]

-- ============================================================================
-- Type Environment Tests
-- ============================================================================

typeEnvironmentTests :: TestTree
typeEnvironmentTests = testGroup "Type Environment"
  [ testCase "Empty environment has prelude types" $ do
      let checker = Dep.newDependentTypeChecker
      let env = Dep.dtcTypeEnv checker
      assertBool "Should have at least 4 prelude types" 
        (Map.size (Dep.typeDefinitions env) >= 4)

  , testCase "Add multiple types" $ do
      let checker = Dep.newDependentTypeChecker
      let actions = do
            Dep.addType "Type1" [] []
            Dep.addType "Type2" [] []
            Dep.addType "Type3" [] []
      let checker' = execState actions checker
      let env = Dep.dtcTypeEnv checker'
      let defs = Dep.typeDefinitions env
      assertBool "Type1 defined" (Map.member "Type1" defs)
      assertBool "Type2 defined" (Map.member "Type2" defs)
      assertBool "Type3 defined" (Map.member "Type3" defs)

  , testCase "Type shadowing" $ do
      let checker = Dep.newDependentTypeChecker
      let actions = do
            Dep.addType "MyType" ["T"] []
            Dep.addType "MyType" ["T", "U"] []  -- Redefine
      let checker' = execState actions checker
      let env = Dep.dtcTypeEnv checker'
      case Map.lookup "MyType" (Dep.typeDefinitions env) of
        Nothing -> assertFailure "MyType not found"
        Just typeDef -> assertEqual "Should have new arity" 2 (length $ Dep.tdParams typeDef)
  ]

-- ============================================================================
-- Type Variable Tests
-- ============================================================================

typeVariableTests :: TestTree
typeVariableTests = testGroup "Type Variables"
  [ testCase "Type variable is valid" $ do
      let checker = Dep.newDependentTypeChecker
      let ((), checker') = runState (Dep.checkType (Dep.TVVar "T")) checker
      assertEqual "Type variable should not cause error" [] (Dep.tcErrors checker')

  , testCase "Occurs check" $ do
      let tv = Dep.TVVar "T"
      let recursive = Dep.TVApp "List" [tv]
      assertBool "Should detect occurs" (Dep.occurs "T" recursive)

  , testCase "No occurs in different variable" $ do
      let tv = Dep.TVVar "U"
      assertBool "Should not detect occurs" (not $ Dep.occurs "T" tv)

  , testCase "Occurs in nested structure" $ do
      let nested = Dep.TVFun [Dep.TVVar "T"] (Dep.TVApp "List" [Dep.TVVar "T"])
      assertBool "Should detect occurs in nested" (Dep.occurs "T" nested)

  , testProperty "Occurs check property" $ \name ->
      let tv = Dep.TVVar name
      in Dep.occurs name tv

  , testProperty "Apply substitution idempotent" $ \name ->
      let tv = Dep.TVCon name
          subst = []
      in Dep.applySubst subst tv == tv
  ]

-- ============================================================================
-- Property-based Tests
-- ============================================================================

instance Arbitrary Dep.TypeVar where
  arbitrary = oneof
    [ Dep.TVCon <$> elements ["int", "string", "bool", "float64"]
    , Dep.TVVar <$> elements ["T", "U", "V", "X", "Y", "Z"]
    ]

instance Arbitrary Dep.TypeConstraint where
  arbitrary = oneof
    [ Dep.TypeSizeGE <$> arbitrary <*> choose (0, 100)
    , Dep.TypeSizeGT <$> arbitrary <*> choose (0, 100)
    , Dep.TypeRange <$> arbitrary <*> choose (0, 50) <*> choose (51, 100)
    ]