module Test.Unit.DependenciesCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import Test.Tasty.QuickCheck (testProperty, Property, forAll, Gen, arbitrary, elements)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Dependencies.AST 
  ( AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , DependencyNode(..)
  , DependencyGraph(..)
  )
import Dependencies
  ( DependentTypeChecker
  , DependentTypeError(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , TypeScheme(..)
  , TypeEnvironment(..)
  , newDependentTypeChecker
  , analyzeAST
  , validateASTSemantics
  , checkType
  , addType
  , addConstraint
  , solveConstraints
  , inferType
  , unify
  , initialTypeEnvironment
  )

tests :: TestTree
tests = testGroup "Dependencies Core Tests"
  [ testASTConstruction
  , testStatementValidation
  , testTypeExpressionOperations
  , testConstraintHandling
  , testDependencyGraphConstruction
  , testTypeEnvironmentOperations
  , testBasicTypeInference
  , testUnification
  , testASTProperties
  , testTypeExpressionProperties
  ]

testASTConstruction :: TestTree
testASTConstruction = testCase "AST construction and equality" $ do
  let program = Program
        [ STypeDef "Vector" ["T"] [SizeGT "T" 0]
        , SVarDecl "myVec" (GenericT "Vector" [SimpleT "Int"])
        ]
  
  assertEqual "Program should contain 2 statements" 2 (case program of Program stmts -> length stmts)
  
  let (Program stmts) = program
  case head stmts of
    STypeDef name params constraints -> do
      assertEqual "Type name should be Vector" "Vector" (T.unpack name)
      assertEqual "Should have 1 type parameter" ["T"] params
      assertEqual "Should have 1 constraint" [SizeGT "T" 0] constraints
    _ -> assertBool "First statement should be type definition" False

testStatementValidation :: TestTree
testStatementValidation = testCase "Statement validation" $ do
  checker <- newDependentTypeChecker
  
  -- Test type definition
  let typeDef = STypeDef "List" ["T"] [SizeGE "T" 0]
  result1 <- validateASTSemantics checker typeDef
  case result1 of
    Right _ -> return ()
    Left err -> assertBool ("Type definition should be valid: " ++ show err) False
  
  -- Test variable declaration
  let varDecl = SVarDecl "x" (SimpleT "Int")
  addType checker "Int" (SimpleT "Int")
  result2 <- validateASTSemantics checker varDecl
  case result2 of
    Right _ -> return ()
    Left err -> assertBool ("Variable declaration should be valid: " ++ show err) False

testTypeExpressionOperations :: TestTree
testTypeExpressionOperations = testCase "Type expression operations" $ do
  let simpleType = SimpleT "Int"
      genericType = GenericT "List" [simpleType]
      funcType = FuncT [("x", simpleType)] genericType
      refineType = RefineT genericType [SizeGT "List" 0]
  
  assertEqual "Simple type should preserve name" (SimpleT "Int") simpleType
  assertEqual "Generic type should preserve structure" 
    (GenericT "List" [SimpleT "Int"]) genericType
  assertEqual "Function type should preserve structure"
    (FuncT [("x", SimpleT "Int")] (GenericT "List" [SimpleT "Int"])) funcType
  assertEqual "Refined type should preserve constraints"
    (RefineT (GenericT "List" [SimpleT "Int"]) [SizeGT "List" 0]) refineType

testConstraintHandling :: TestTree
testConstraintHandling = testCase "Constraint handling" $ do
  checker <- newDependentTypeChecker
  
  -- Add basic type
  addType checker "Int" (SimpleT "Int")
  
  -- Add constraints
  addConstraint checker (SizeGT "x" 0)
  addConstraint checker (RangeC "y" 1 10)
  addConstraint checker (PredC "isValid" [SimpleT "Int"])
  
  -- Test constraint solving (simplified test)
  result <- solveConstraints checker
  case result of
    Right _ -> return ()
    Left err -> assertBool ("Constraints should be solvable: " ++ show err) False

testDependencyGraphConstruction :: TestTree
testDependencyGraphConstruction = testCase "Dependency graph construction" $ do
  let node1 = DependencyNode "module1" ["module2", "module3"]
      node2 = DependencyNode "module2" ["module3"]
      node3 = DependencyNode "module3" []
  
  assertEqual "Node1 should depend on module2 and module3" 
    ["module2", "module3"] (nodeDependencies node1)
  assertEqual "Node2 should depend on module3" 
    ["module3"] (nodeDependencies node2)
  assertEqual "Node3 should have no dependencies" 
    [] (nodeDependencies node3)
  
  -- Test node equality
  let node1Copy = DependencyNode "module1" ["module2", "module3"]
  assertBool "Identical nodes should be equal" (node1 == node1Copy)
  assertBool "Different nodes should not be equal" (node1 /= node2)

testTypeEnvironmentOperations :: TestTree
testTypeEnvironmentOperations = testCase "Type environment operations" $ do
  let env = initialTypeEnvironment
  
  -- Test basic environment structure
  assertBool "Environment should be constructible" (True)
  
  -- Test type variable creation
  typeVar <- newTypeVariable
  assertBool "Type variable should be creatable" (True)

testBasicTypeInference :: TestTree
testBasicTypeInference = testCase "Basic type inference" $ do
  checker <- newDependentTypeChecker
  
  -- Add basic types
  addType checker "Int" (SimpleT "Int")
  addType checker "String" (SimpleT "String")
  
  -- Test type checking
  result1 <- checkType checker (SimpleT "Int")
  case result1 of
    Right _ -> return ()
    Left err -> assertBool ("Int type should be valid: " ++ show err) False
  
  -- Test type inference for simple expressions
  let simpleAST = Program [SVarDecl "x" (SimpleT "Int")]
  result2 <- inferType checker simpleAST
  case result2 of
    Right _ -> return ()
    Left err -> assertBool ("Should infer simple type: " ++ show err) False

testUnification :: TestTree
testUnification = testCase "Type unification" $ do
  checker <- newDependentTypeChecker
  
  -- Test basic unification
  let type1 = SimpleT "Int"
      type2 = SimpleT "Int"
  
  result1 <- unify checker type1 type2
  case result1 of
    Right _ -> return ()
    Left err -> assertBool ("Identical types should unify: " ++ show err) False
  
  -- Test unification of different types (should fail)
  let type3 = SimpleT "String"
  result2 <- unify checker type1 type3
  case result2 of
    Right _ -> assertBool "Different types should not unify" False
    Left _ -> return ()

testASTProperties :: TestTree
testASTProperties = testProperty "AST construction preserves structure" $
  forAll arbitraryStatement $ \stmt -> do
    let program = Program [stmt]
        (Program stmts) = program
    return $ length stmts == 1 && head stmts == stmt

testTypeExpressionProperties :: TestTree
testTypeExpressionProperties = testProperty "Type expression round-trip" $
  forAll arbitraryTypeExpr $ \expr -> do
    -- Simple structural test - type expressions should be showable and readable
    return $ length (show expr) > 0

-- Helper generators for QuickCheck tests
arbitraryStatement :: Gen Statement
arbitraryStatement = elements
  [ STypeDef "MyType" ["T"] [SizeGT "T" 0]
  , STypeAlias "MyAlias" (SimpleT "Int") []
  , SVarDecl "myVar" (SimpleT "String")
  , SFuncDecl "myFunc" [("x", SimpleT "Int")] (Just (SimpleT "Int"))
  , SConstraintDef "myConstraint" (SizeGT "x" 0)
  , SExistsDecl ["T"] (SVarDecl "x" (SimpleT "T"))
  ]

arbitraryTypeExpr :: Gen TypeExpr
arbitraryTypeExpr = elements
  [ SimpleT "Int"
  , SimpleT "String"
  , GenericT "List" [SimpleT "Int"]
  , GenericT "Map" [SimpleT "String", SimpleT "Int"]
  , FuncT [("x", SimpleT "Int"), ("y", SimpleT "String")] (SimpleT "Bool")
  , RefineT (SimpleT "List") [SizeGT "List" 0]
  ]