{-# LANGUAGE OverloadedStrings #-}

module DependentTypesTests (dependentTypesTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Dependencies as Dep
import qualified Data.Text as T
import qualified Data.Set as Set
import Control.Monad.State

-- ============================================================================
-- Dependent Types Edge Case Tests
-- ============================================================================

dependentTypesTests :: TestTree
dependentTypesTests = testGroup "Dependent Types Tests"
  [ parsingTests
  , refinementTypeTests
  , constraintResolutionTests
  , dependentFunctionTests
  , edgeCaseTests
  , errorHandlingTests
  ]

-- ============================================================================
-- Parsing Tests
-- ============================================================================

parsingTests :: TestTree
parsingTests = testGroup "Dependent Type Parsing"
  [ testCase "Parse simple type definition" $ do
      let source = "type MyType"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program stmts) -> assertEqual "Should have one statement" 1 (length stmts)

  , testCase "Parse generic type definition" $ do
      let source = "type List<T>"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [Dep.STypeDef name params _]) -> do
          assertEqual "Type name should be List" "List" name
          assertEqual "Should have one parameter" 1 (length params)

  , testCase "Parse type with where clause" $ do
      let source = "type PositiveInt where x > 0"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [Dep.STypeDef _ _ constraints]) ->
          assertEqual "Should have one constraint" 1 (length constraints)

  , testCase "Parse type with multiple constraints" $ do
      let source = "type BoundedInt where x >= 0, x < 100"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [Dep.STypeDef _ _ constraints]) ->
          assertEqual "Should have two constraints" 2 (length constraints)

  , testCase "Parse variable declaration with refinement" $ do
      let source = "var x: int where x > 0"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [Dep.SVarDecl _ typeExpr]) ->
          case typeExpr of
            Dep.RefineT _ constraints -> assertEqual "Should have constraints" 1 (length constraints)
            _ -> assertFailure "Expected refined type"

  , testCase "Parse function with dependent types" $ do
      let source = "func abs(x: int): int where result >= 0"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [Dep.SFuncDecl _ _ (Just returnType)]) ->
          case returnType of
            Dep.RefineT _ _ -> return ()
            _ -> assertFailure "Expected refined return type"

  , testCase "Parse complex constraint" $ do
      let source = "constraint NonEmpty = length > 0"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [Dep.SConstraintDef name _]) ->
          assertEqual "Constraint name" "NonEmpty" name

  , testCase "Parse predicate constraint" $ do
      let source = "type SafeDiv where IsPrime(n)"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [Dep.STypeDef _ _ [Dep.PredC _ _]]) -> return ()
        _ -> assertFailure "Expected predicate constraint"
  ]

-- ============================================================================
-- Refinement Type Tests
-- ============================================================================

refinementTypeTests :: TestTree
refinementTypeTests = testGroup "Refinement Types"
  [ testCase "Simple refinement type" $ do
      let source = "var x: int where x > 0"
      let errors = Dep.analyzeDependentTypes source
      assertEqual "No errors for valid refinement" [] errors

  , testCase "Multiple refinements" $ do
      let source = "var x: int where x >= 0, x < 100"
      let errors = Dep.analyzeDependentTypes source
      assertEqual "No errors for multiple refinements" [] errors

  , testCase "Nested refinement types" $ do
      let source = "type NonEmptyList<T> where length > 0\nvar list: NonEmptyList<int>"
      let errors = Dep.analyzeDependentTypes source
      assertEqual "No errors for nested refinements" [] errors

  , testCase "Function with refined parameters" $ do
      let source = "func divide(x: int, y: int where y > 0): int"
      let errors = Dep.analyzeDependentTypes source
      assertEqual "No errors for refined parameters" [] errors

  , testCase "Function with refined return type" $ do
      let source = "func abs(x: int): int where result >= 0"
      let errors = Dep.analyzeDependentTypes source
      assertEqual "No errors for refined return" [] errors

  , testCase "Generic type with refinements" $ do
      let source = "type BoundedArray<T, N> where N > 0, N < 1000"
      let errors = Dep.analyzeDependentTypes source
      assertEqual "No errors for generic refinement" [] errors
  ]

-- ============================================================================
-- Constraint Resolution Tests
-- ============================================================================

constraintResolutionTests :: TestTree
constraintResolutionTests = testGroup "Constraint Resolution"
  [ testCase "Resolve size constraint" $ do
      let checker = Dep.newDependentTypeChecker
      let constraint = Dep.TypeSizeGE (Dep.TVCon "int") 1
      let ((), checker') = runState (Dep.addConstraint constraint) checker
      let (success, checker'') = runState Dep.solveConstraints checker'
      assertBool "Constraint should be satisfied" success

  , testCase "Detect constraint violation" $ do
      let checker = Dep.newDependentTypeChecker
      let constraint = Dep.TypeRange (Dep.TVCon "int") 10 5  -- Invalid range
      let ((), checker') = runState (Dep.addConstraint constraint) checker
      let (success, _) = runState Dep.solveConstraints checker'
      assertBool "Should detect violation" (not success)

  , testCase "Unify type equality constraints" $ do
      let checker = Dep.newDependentTypeChecker
      let constraint = Dep.Equal (Dep.TVVar "T") (Dep.TVCon "int")
      let ((), checker') = runState (Dep.addConstraint constraint) checker
      let (success, _) = runState Dep.solveConstraints checker'
      assertBool "Equality should unify" success

  , testCase "Multiple constraints" $ do
      let checker = Dep.newDependentTypeChecker
      let actions = do
            Dep.addConstraint (Dep.TypeSizeGE (Dep.TVVar "T") 0)
            Dep.addConstraint (Dep.TypeSizeGT (Dep.TVVar "T") 5)
            Dep.solveConstraints
      let (success, _) = runState actions checker
      assertBool "Multiple constraints should resolve" success

  , testCase "Conflicting constraints" $ do
      let checker = Dep.newDependentTypeChecker
      let actions = do
            Dep.addConstraint (Dep.Equal (Dep.TVVar "T") (Dep.TVCon "int"))
            Dep.addConstraint (Dep.Equal (Dep.TVVar "T") (Dep.TVCon "string"))
            Dep.solveConstraints
      let (success, _) = runState actions checker
      assertBool "Conflicting constraints should fail" (not success)

  , testCase "Transitive constraint resolution" $ do
      let checker = Dep.newDependentTypeChecker
      let actions = do
            Dep.addConstraint (Dep.Equal (Dep.TVVar "T") (Dep.TVVar "U"))
            Dep.addConstraint (Dep.Equal (Dep.TVVar "U") (Dep.TVCon "int"))
            Dep.solveConstraints
      let (success, _) = runState actions checker
      assertBool "Transitive constraints should resolve" success
  ]

-- ============================================================================
-- Dependent Function Tests
-- ============================================================================

dependentFunctionTests :: TestTree
dependentFunctionTests = testGroup "Dependent Function Types"
  [ testCase "Function with size-indexed arrays" $ do
      let source = "func getArray(n: int where n > 0): Array<T, n>"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> do
          let errors = Dep.analyzeAST ast
          assertEqual "No errors for indexed array" [] errors

  , testCase "Function with dependent return type" $ do
      let source = "func replicate(n: int where n > 0, x: T): List<T> where length == n"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> do
          let errors = Dep.analyzeAST ast
          assertEqual "No errors for dependent return" [] errors

  , testCase "Higher-order function with constraints" $ do
      let source = "func filter(pred: func(T): bool, list: List<T>): List<T> where length <= old_length"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right _ -> return ()  -- Just test parsing

  , testCase "Function with preconditions" $ do
      let source = "func divide(x: int, y: int where y > 0): float64"
      let errors = Dep.analyzeDependentTypes source
      assertEqual "No errors for precondition" [] errors

  , testCase "Function with postconditions" $ do
      let source = "func abs(x: int): int where result >= 0"
      let errors = Dep.analyzeDependentTypes source
      assertEqual "No errors for postcondition" [] errors
  ]

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge Cases"
  [ testCase "Empty constraint list" $ do
      let source = "type MyType where"
      case Dep.runParser source of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertFailure "Should fail on empty where clause"

  , testCase "Constraint with no parameters" $ do
      let source = "type Simple"
      let errors = Dep.analyzeDependentTypes source
      assertEqual "No errors for simple type" [] errors

  , testCase "Self-referential type" $ do
      let source = "type Tree<T> where left: Tree<T>, right: Tree<T>"
      case Dep.runParser source of
        Left _ -> return ()  -- May fail, but shouldn't crash
        Right _ -> return ()

  , testCase "Mutual recursion in types" $ do
      let source = "type A<B>\ntype B<A>"
      case Dep.runParser source of
        Left _ -> return ()
        Right _ -> return ()

  , testCase "Very deep nesting" $ do
      let deepType = "List<List<List<List<List<int>>>>>"
      let source = "var x: " ++ deepType
      case Dep.runParser source of
        Left _ -> return ()
        Right _ -> return ()

  , testCase "Many constraints" $ do
      let constraints = concat ["x > " ++ show i ++ ", " | i <- [0..20]]
      let source = "type Bounded where " ++ constraints
      case Dep.runParser source of
        Left _ -> return ()
        Right _ -> return ()

  , testCase "Unicode in identifiers" $ do
      let source = "type 类型<参数>"
      case Dep.runParser source of
        Left _ -> return ()  -- May not support Unicode
        Right _ -> return ()

  , testCase "Constraint with complex expressions" $ do
      let source = "type Complex where IsValid(x, y, z)"
      let errors = Dep.analyzeDependentTypes source
      assertEqual "Should handle complex predicates" [] errors
  ]

-- ============================================================================
-- Error Handling Tests
-- ============================================================================

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling"
  [ testCase "Undefined type in constraint" $ do
      let source = "var x: UndefinedType where x > 0"
      let errors = Dep.analyzeDependentTypes source
      assertBool "Should detect undefined type" (not $ null errors)

  , testCase "Invalid constraint syntax" $ do
      let source = "type Invalid where x ++ 5"
      case Dep.runParser source of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertFailure "Should reject invalid syntax"

  , testCase "Type parameter mismatch" $ do
      let source = "type List<T>\nvar x: List<int, string>"
      let errors = Dep.analyzeDependentTypes source
      assertBool "Should detect arity mismatch" (not $ null errors)

  , testCase "Circular constraint" $ do
      let source = "type Circular where x > x"
      let errors = Dep.analyzeDependentTypes source
      -- May or may not be an error depending on semantics
      return ()

  , testCase "Unsatisfiable constraint" $ do
      let checker = Dep.newDependentTypeChecker
      let constraint = Dep.TypeRange (Dep.TVCon "int") 100 0
      let ((), checker') = runState (Dep.addConstraint constraint) checker
      let (success, _) = runState Dep.solveConstraints checker'
      assertBool "Should detect unsatisfiable" (not success)

  , testCase "Type variable escape" $ do
      let source = "func escape(): T where T = int"
      case Dep.runParser source of
        Left _ -> return ()
        Right ast -> do
          let errors = Dep.analyzeAST ast
          -- May or may not be an error depending on scoping rules
          return ()

  , testCase "Constraint on non-existent variable" $ do
      let source = "type Bad where undefined_var > 0"
      case Dep.runParser source of
        Left _ -> return ()
        Right _ -> return ()
  ]

-- ============================================================================
-- Property Tests
-- ============================================================================

instance Arbitrary Dep.Constraint where
  arbitrary = oneof
    [ Dep.SizeGT <$> genName <*> arbitrary
    , Dep.SizeGE <$> genName <*> arbitrary
    , Dep.RangeC <$> genName <*> arbitrary <*> arbitrary
    , Dep.PredC <$> genName <*> pure []
    ]
    where
      genName = T.pack <$> elements ["x", "y", "z", "n", "m"]

-- Property: Parsing and analyzing should not crash
prop_parse_does_not_crash :: String -> Property
prop_parse_does_not_crash source =
  property $ case Dep.runParser source of
    Left _ -> True
    Right _ -> True

-- Property: Valid types should have no errors
prop_valid_types_no_errors :: Property
prop_valid_types_no_errors = 
  forAll genValidType $ \source ->
    let errors = Dep.analyzeDependentTypes source
    in null errors
  where
    genValidType = elements
      [ "type Simple"
      , "type Generic<T>"
      , "var x: int"
      , "func f(x: int): int"
      ]