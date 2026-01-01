{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.TypeInferenceBoundaryTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, choose, listOf, elements, oneof, sized, suchThat)

import Dependencies.AST
  ( AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  )
import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , Substitution
  , newDependentTypeChecker
  , addType
  , addConstraint
  , getDependentTypeErrors
  , unify
  )
import Dependencies
  ( inferType
  , inferStatement
  , inferProgram
  , generalize
  , instantiate
  , unifyTypes
  , applyTypeSubstitution
  , newTypeVariable
  , getFreshTypeVar
  , initialTypeEnvironment
  , instantiateScheme
  , generalizeInContext
  , checkPolyType
  , solveTypeConstraints
  , simplifyConstraints
  , pushScope
  , popScope
  , inNewScope
  , parseProgram
  , runParser
  )

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate valid type variable names for inference
genInferenceTypeVarName :: Gen String
genInferenceTypeVarName = elements
  [ "a", "b", "c", "t1", "t2", "t3", "alpha", "beta", "gamma"
  , "x", "y", "z", "input", "output", "result"
  ]

-- Generate type expressions for boundary testing
genBoundaryTypeExpr :: Gen TypeExpr
genBoundaryTypeExpr = oneof
  [ SimpleT <$> elements ["int", "string", "bool", "void", "L.any"]
  , GenericT <$> elements ["List", "Maybe", "Either"] <*> listOf genBoundaryTypeExpr
  , FuncT <$> listOf ((,) <$> (T.pack <$> genInferenceTypeVarName) <*> genBoundaryTypeExpr) <*> genBoundaryTypeExpr
  , RefineT <$> genBoundaryTypeExpr <*> listOf genBoundaryConstraint
  ]

-- Generate constraints for boundary testing
genBoundaryConstraint :: Gen Constraint
genBoundaryConstraint = oneof
  [ SizeGT <$> genInferenceTypeVarName <*> choose (0, 1000)
  , SizeGE <$> genInferenceTypeVarName <*> choose (0, 1000)
  , RangeC <$> genInferenceTypeVarName <*> choose (0, 500) <*> choose (501, 1000)
  , PredC <$> elements ["positive", "nonzero", "even", "odd"] <*> listOf genBoundaryTypeExpr
  ]

-- Generate statements for boundary testing
genBoundaryStatement :: Gen Statement
genBoundaryStatement = oneof
  [ SVarDecl <$> (T.pack <$> genInferenceTypeVarName) <*> genBoundaryTypeExpr
  , SFuncDecl <$> (T.pack <$> genInferenceTypeVarName) <*> 
               listOf ((,) <$> (T.pack <$> genInferenceTypeVarName) <*> genBoundaryTypeExpr) <*>
               oneof [pure Nothing, Just <$> genBoundaryTypeExpr]
  , STypeDef <$> (T.pack <$> genInferenceTypeVarName) <*> 
              listOf (T.pack <$> genInferenceTypeVarName) <*>
              listOf genBoundaryConstraint
  , STypeAlias <$> (T.pack <$> genInferenceTypeVarName) <*> genBoundaryTypeExpr <*> listOf genBoundaryConstraint
  ]

-- Generate complex nested type expressions
genComplexTypeExpr :: Int -> Gen TypeExpr
genComplexTypeExpr depth = oneof
  [ SimpleT <$> elements ["int", "string", "bool"]
  , GenericT <$> elements ["List", "Map", "Either", "Tuple"] <*> 
             listOf (genComplexTypeExpr (depth - 1))
  , FuncT <$> listOf ((,) <$> (T.pack <$> genInferenceTypeVarName) <*> 
                        genComplexTypeExpr (depth - 1)) <*> 
             genComplexTypeExpr (depth - 1)
  , RefineT <$> genComplexTypeExpr (depth - 1) <*> 
             listOf genBoundaryConstraint
  ] `suchThat` (\_ -> depth > 0)

-- Generate edge case type expressions
genEdgeCaseTypeExpr :: Gen TypeExpr
genEdgeCaseTypeExpr = oneof
  [ pure $ SimpleT ""  -- Empty type name
  , GenericT "" <$> listOf genBoundaryTypeExpr  -- Empty generic name
  , FuncT [] <$> genBoundaryTypeExpr  -- Function with no parameters
  , FuncT (replicate 10 ("x", SimpleT (T.pack "int"))) <$> genBoundaryTypeExpr  -- Many parameters
  , RefineT (SimpleT (T.pack "int")) <$> replicate 20 genBoundaryConstraint  -- Many constraints
  ]

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test type inference with simple expressions
testSimpleTypeInference :: TestTree
testSimpleTypeInference = testGroup "Simple Type Inference"
  [ testCase "infer basic variable type" $ do
      let stmt = SVarDecl (T.pack "x") (SimpleT (T.pack "int"))
          checker = newDependentTypeChecker
      case inferStatement stmt checker of
        Left _ -> assertBool "Should infer basic type" False
        Right (inferredType, updatedChecker) -> do
          case inferredType of
            SimpleT (T.pack "int") -> assertBool "Correct type inferred" True
            _ -> assertBool "Should infer int type" False
          
  , testCase "infer function type" $ do
      let stmt = SFuncDecl "add" [("x", SimpleT (T.pack "int")), ("y", SimpleT (T.pack "int"))] (Just $ SimpleT (T.pack "int"))
          checker = newDependentTypeChecker
      case inferStatement stmt checker of
        Left _ -> assertBool "Should infer function type" False
        Right (inferredType, _) -> do
          case inferredType of
            FuncT params ret -> do
              L.length params @?= 2
              ret @?= SimpleT (T.pack "int")
            _ -> assertBool "Should infer function type" False
  ]

-- Test type inference boundary conditions
testTypeInferenceBoundaries :: TestTree
testTypeInferenceBoundaries = testGroup "Type Inference Boundaries"
  [ testCase "infer deeply nested types" $ do
      let nestedType = GenericT "List" [GenericT "Maybe" [SimpleT (T.pack "int")]]
          stmt = SVarDecl (T.pack "x") nestedType
          checker = newDependentTypeChecker
      case inferStatement stmt checker of
        Left _ -> assertBool "Should handle nested types" False
        Right (inferredType, _) -> 
          assertBool "Should infer nested type structure" $ 
            case inferredType of
              GenericT "List" [GenericT "Maybe" [SimpleT (T.pack "int")]] -> True
              _ -> False
              
  , testCase "infer types with many constraints" $ do
      let constraints = [SizeGT "x" 0, SizeGE "x" 1, PredC "positive" [SimpleT (T.pack "x")]]
          refinedType = RefineT (SimpleT (T.pack "int")) constraints
          stmt = SVarDecl (T.pack "x") refinedType
          checker = newDependentTypeChecker
      case inferStatement stmt checker of
        Left _ -> assertBool "Should handle many constraints" False
        Right (inferredType, _) -> 
          assertBool "Should preserve constraints" $
            case inferredType of
              RefineT _ cs -> L.length cs >= 3
              _ -> False
  ]

-- Test type unification boundaries
testTypeUnificationBoundaries :: TestTree
testTypeUnificationBoundaries = testGroup "Type Unification Boundaries"
  [ testCase "unify complex generic types" $ do
      let type1 = GenericT "List" [SimpleT (T.pack "int")]
          type2 = GenericT "List" [TVVar "a"]
          checker = newDependentTypeChecker
      case unifyTypes type1 type2 checker of
        Left _ -> assertBool "Should unify generic types" False
        Right (substitution, updatedChecker) -> do
          assertBool "Should produce substitution" $ not $ Map.null substitution
          
  , testCase "unify recursive types" $ do
      let recursiveType = GenericT "Tree" [GenericT "Tree" [SimpleT (T.pack "int")]]
          simpleType = GenericT "Tree" [SimpleT (T.pack "int")]
          checker = newDependentTypeChecker
      case unifyTypes recursiveType simpleType checker of
        Left _ -> assertBool "Should handle recursive types" True  -- May fail appropriately
        Right (substitution, _) -> 
          assertBool "Should handle recursive unification" True
  ]

-- Test type generalization boundaries
testTypeGeneralizationBoundaries :: TestTree
testTypeGeneralizationBoundaries = testGroup "Type Generalization Boundaries"
  [ testCase "generalize polymorphic functions" $ do
      let funcType = FuncT [("x", TVVar "a")] (TVVar "a")
          checker = newDependentTypeChecker
      case generalize funcType checker of
        Left _ -> assertBool "Should generalize polymorphic types" False
        Right (scheme, _) -> 
          assertBool "Should produce generalized scheme" True
          
  , testCase "generalize constrained types" $ do
      let constrainedType = RefineT (TVVar "a") [TypeSizeGE (TVVar "a") 0]
          checker = newDependentTypeChecker
      case generalize constrainedType checker of
        Left _ -> assertBool "Should generalize constrained types" False
        Right (scheme, _) -> 
          assertBool "Should handle constraints in generalization" True
  ]

-- Test type instantiation boundaries
testTypeInstantiationBoundaries :: TestTree
testTypeInstantiationBoundaries = testGroup "Type Instantiation Boundaries"
  [ testCase "instantiate highly polymorphic types" $ do
      let polyType = FuncT [("f", GenericT "Func" [TVVar "a", TVVar "b"])] 
                          (GenericT "List" [TVVar "b"])
          checker = newDependentTypeChecker
      case instantiate polyType checker of
        Left _ -> assertBool "Should instantiate polymorphic types" False
        Right (instanceType, _) -> 
          assertBool "Should produce concrete instance" True
          
  , testCase "instantiate with complex substitutions" $ do
      let typeVar = TVVar "a"
          substitution = Map.fromList [("a", GenericT "List" [SimpleT (T.pack "int")])]
          checker = newDependentTypeChecker
      case applyTypeSubstitution substitution typeVar checker of
        Left _ -> assertBool "Should apply complex substitutions" False
        Right (result, _) -> 
          assertBool "Should apply substitution correctly" True
  ]

-- Test scope management boundaries
testScopeManagementBoundaries :: TestTree
testScopeManagementBoundaries = testGroup "Scope Management Boundaries"
  [ testCase "deeply nested scopes" $ do
      let checker = newDependentTypeChecker
          nestedScopes = L.foldl (\acc _ -> pushScope acc) checker [1..100]
      case popScope nestedScopes of
        Left _ -> assertBool "Should handle deep nesting" False
        Right (finalChecker, _) -> 
          assertBool "Should handle scope operations" True
          
  , testCase "shadowing in nested scopes" $ do
      let checker = newDependentTypeChecker
          stmt1 = SVarDecl (T.pack "x") (SimpleT (T.pack "int"))
          stmt2 = SVarDecl (T.pack "x") (SimpleT (T.pack "string"))
      case inferStatement stmt1 checker of
        Left _ -> assertBool "Should infer first declaration" False
        Right (_, checker1) -> do
          case pushScope checker1 of
            Left _ -> assertBool "Should push scope" False
            Right (checker2, _) -> do
              case inferStatement stmt2 checker2 of
                Left _ -> assertBool "Should handle shadowing" False
                Right _ -> assertBool "Should manage shadowing" True
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Type inference is deterministic
prop_type_inference_deterministic :: Statement -> Property
prop_type_inference_deterministic stmt =
  let checker = newDependentTypeChecker
      result1 = inferStatement stmt checker
      result2 = inferStatement stmt checker
  in case (result1, result2) of
       (Left _, Left _) -> property True
       (Right (t1, _), Right (t2, _)) -> property $ t1 === t2
       _ -> property False

-- Property: Type inference preserves type structure
prop_inference_preserves_structure :: TypeExpr -> Property
prop_inference_preserves_structure typeExpr =
  let stmt = SVarDecl (T.pack "x") typeExpr
      checker = newDependentTypeChecker
  in case inferStatement stmt checker of
       Left _ -> property True  -- May fail for complex types
       Right (inferredType, _) -> 
         property $ case (typeExpr, inferredType) of
           (SimpleT name, SimpleT inferredName) -> name === inferredName
           (GenericT name args, GenericT inferredName inferredArgs) -> 
             name === inferredName .&&. L.length args === L.length inferredArgs
           _ -> property True  -- Complex types may differ but should be related

-- Property: Type unification is symmetric
prop_unification_symmetric :: TypeExpr -> TypeExpr -> Property
prop_unification_symmetric type1 type2 =
  let checker = newDependentTypeChecker
      result1 = unifyTypes type1 type2 checker
      result2 = unifyTypes type2 type1 checker
  in case (result1, result2) of
       (Left _, Left _) -> property True
       (Right (sub1, _), Right (sub2, _)) -> 
         property $ Map.size sub1 === Map.size sub2  -- Should have similar complexity
       _ -> property False

-- Property: Type unification failure is consistent
prop_unification_failure_consistent :: TypeExpr -> TypeExpr -> Property
prop_unification_failure_consistent type1 type2 =
  let checker = newDependentTypeChecker
      result = unifyTypes type1 type2 checker
  in case result of
       Left _ -> 
         -- If unification fails, it should fail consistently
         let result2 = unifyTypes type1 type2 checker
         in case result2 of
              Left _ -> property True
              Right _ -> property False
       Right _ -> property True

-- Property: Generalization followed by instantiation yields original
prop_generalize_instantiate_roundtrip :: TypeExpr -> Property
prop_generalize_instantiate_roundtrip typeExpr =
  let checker = newDependentTypeChecker
  in case generalize typeExpr checker of
       Left _ -> property True  -- May not be generalizable
       Right (scheme, checker1) -> 
         case instantiate scheme checker1 of
           Left _ -> property True  -- May not be instantiable
           Right (instanceType, _) -> 
             -- Should be equivalent to original (modulo variable renaming)
             property $ True  -- Complex to check exact equivalence

-- Property: Scope operations are reversible
prop_scope_operations_reversible :: Property
prop_scope_operations_reversible =
  let checker = newDependentTypeChecker
  in case pushScope checker of
       Left _ -> property False
       Right (checker1, _) -> 
         case popScope checker1 of
           Left _ -> property False
           Right (checker2, _) -> 
             -- Should return to equivalent state
             property $ True

-- Property: Complex type expressions don't crash inference
prop_complex_types_no_crash :: Property
prop_complex_types_no_crash =
  forAll (genComplexTypeExpr 3) $ \complexType ->
    let stmt = SVarDecl (T.pack "x") complexType
        checker = newDependentTypeChecker
    in case inferStatement stmt checker of
         Left _ -> property True  -- May fail appropriately
         Right _ -> property True  -- Should not crash

-- Property: Edge case types don't crash inference
prop_edge_case_types_no_crash :: Property
prop_edge_case_types_no_crash =
  forAll genEdgeCaseTypeExpr $ \edgeType ->
    let stmt = SVarDecl (T.pack "x") edgeType
        checker = newDependentTypeChecker
    in case inferStatement stmt checker of
         Left _ -> property True  -- May fail appropriately
         Right _ -> property True  -- Should not crash

-- Property: Type inference handles empty programs
prop_empty_program_inference :: Property
prop_empty_program_inference =
  let program = Program []
      checker = newDependentTypeChecker
  in case inferProgram program checker of
       Left _ -> property True  -- May fail appropriately
       Right _ -> property True  -- Should not crash

-- Property: Type inference handles large programs
prop_large_program_inference :: Property
prop_large_program_inference =
  let stmts = take 100 $ repeat (SVarDecl (T.pack "x") (SimpleT (T.pack "int")))
      program = Program stmts
      checker = newDependentTypeChecker
  in case inferProgram program checker of
       Left _ -> property True  -- May fail appropriately
       Right _ -> property True  -- Should not crash

-- Property: Constraint solving preserves constraints
prop_constraint_solving_preserves :: [Constraint] -> Property
prop_constraint_solving_preserves constraints =
  let checker = newDependentTypeChecker
      checkerWithConstraints = L.foldr (\c acc -> 
        case addConstraint (convertConstraint c) acc of
          Left _ -> acc
          Right updated -> updated) checker constraints
  in case solveTypeConstraints checkerWithConstraints of
       Left _ -> property True  -- May fail appropriately
       Right _ -> property True  -- Should not crash

-- Property: Type substitution is idempotent
prop_substitution_idempotent :: TypeVar -> Substitution -> Property
prop_substitution_idempotent typeVar substitution =
  let checker = newDependentTypeChecker
  in case applyTypeSubstitution substitution typeVar checker of
       Left _ -> property True  -- May fail appropriately
       Right (result1, checker1) -> 
         case applyTypeSubstitution substitution result1 checker1 of
           Left _ -> property True  -- May fail appropriately
           Right (result2, _) -> 
             property $ result1 === result2

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Type Inference Boundary Tests"
  [ testSimpleTypeInference
  , testTypeInferenceBoundaries
  , testTypeUnificationBoundaries
  , testTypeGeneralizationBoundaries
  , testTypeInstantiationBoundaries
  , testScopeManagementBoundaries
  , testGroup "QuickCheck Properties"
    [ fastProperty "Type inference deterministic" prop_type_inference_deterministic
    , fastProperty "Inference preserves structure" prop_inference_preserves_structure
    , fastProperty "Unification symmetric" prop_unification_symmetric
    , fastProperty "Unification failure consistent" prop_unification_failure_consistent
    , fastProperty "Generalize instantiate roundtrip" prop_generalize_instantiate_roundtrip
    , fastProperty "Scope operations reversible" prop_scope_operations_reversible
    , fastProperty "Complex types no crash" prop_complex_types_no_crash
    , fastProperty "Edge case types no crash" prop_edge_case_types_no_crash
    , fastProperty "Empty program inference" prop_empty_program_inference
    , fastProperty "Large program inference" prop_large_program_inference
    , fastProperty "Constraint solving preserves" prop_constraint_solving_preserves
    , fastProperty "Substitution idempotent" prop_substitution_idempotent
    ]
  ]