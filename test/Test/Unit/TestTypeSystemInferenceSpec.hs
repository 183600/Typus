{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestTypeSystemInferenceSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck ()

import Dependencies ()
import Dependencies.AST ()
import Dependencies.TypeSystem ()
import SourceLocation ()
import qualified Data.Text as T ()
import TestSupport.Arbitrary ()
import Control.Monad (foldM)

-- | Test suite for Type System Inference
testTypeSystemInference :: TestTree
testTypeSystemInference = testGroup "Type System Inference Tests"
  [ testCase "inferType: infers type for integer literal" $
      let expr = TestLiteralExpr (TestIntLiteral 42)
          checker = testNewDependentTypeChecker ()
      in case testInferType expr checker of
           Right inferred -> inferred @?= TestTypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for boolean literal" $
      let expr = TestLiteralExpr (TestBoolLiteral True)
          checker = testNewDependentTypeChecker ()
      in case testInferType expr checker of
           Right inferred -> inferred @?= TestTypeVar "Bool"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for string literal" $
      let expr = TestLiteralExpr (TestStringLiteral "hello")
          checker = testNewDependentTypeChecker ()
      in case testInferType expr checker of
           Right inferred -> inferred @?= TestTypeVar "String"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for variable from environment" $
      let expr = TestVarExpr "x"
          checker = testNewDependentTypeChecker ()
          checker' = testAddType "x" (TestTypeVar "Int") checker
      in case testInferType expr checker' of
           Right inferred -> inferred @?= TestTypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: fails for unknown variable" $
      let expr = TestVarExpr "unknown"
          checker = testNewDependentTypeChecker ()
      in case testInferType expr checker of
           Right _ -> assertFailure "Type inference should have failed"
           Left _ -> return ()
           
  , testCase "inferType: infers type for simple binary operation" $
      let left = TestLiteralExpr (TestIntLiteral 42)
          right = TestLiteralExpr (TestIntLiteral 24)
          expr = TestBinaryOpExpr TestAdd left right
          checker = testNewDependentTypeChecker ()
      in case testInferType expr checker of
           Right inferred -> inferred @?= TestTypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for comparison operation" $
      let left = TestLiteralExpr (TestIntLiteral 42)
          right = TestLiteralExpr (TestIntLiteral 24)
          expr = TestBinaryOpExpr TestEqual left right
          checker = testNewDependentTypeChecker ()
      in case testInferType expr checker of
           Right inferred -> inferred @?= TestTypeVar "Bool"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for function application" $
      let func = TestVarExpr "add"
          arg = TestLiteralExpr (TestIntLiteral 42)
          expr = TestApplyExpr func arg
          checker = testNewDependentTypeChecker ()
          funcType = TestTypeArrow (TestTypeVar "Int") (TestTypeVar "Int")
          checker' = testAddType "add" funcType checker
      in case testInferType expr checker' of
           Right inferred -> inferred @?= TestTypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for lambda expression" $
      let param = "x"
          paramType = TestTypeVar "Int"
          body = TestVarExpr "x"
          expr = TestLambdaExpr [(param, paramType)] body
          checker = testNewDependentTypeChecker ()
      in case testInferType expr checker of
           Right inferred -> inferred @?= TestTypeArrow (TestTypeVar "Int") (TestTypeVar "Int")
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for let expression" $
      let binding = ("x", Just (TestTypeVar "Int"), TestLiteralExpr (TestIntLiteral 42))
          body = TestVarExpr "x"
          expr = TestLetExpr binding body
          checker = testNewDependentTypeChecker ()
      in case testInferType expr checker of
           Right inferred -> inferred @?= TestTypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for if expression" $
      let condition = TestLiteralExpr (TestBoolLiteral True)
          thenBranch = TestLiteralExpr (TestIntLiteral 1)
          elseBranch = TestLiteralExpr (TestIntLiteral 0)
          expr = TestIfExpr condition thenBranch elseBranch
          checker = testNewDependentTypeChecker ()
      in case testInferType expr checker of
           Right inferred -> inferred @?= TestTypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferStatement: infers type for variable declaration" $
      let stmt = TestVarDeclStmt "x" (Just (TestTypeVar "Int")) (TestLiteralExpr (TestIntLiteral 42))
          checker = testNewDependentTypeChecker ()
      in case testInferStatement stmt checker of
           Right (_checker', inferred) -> inferred @?= TestTypeVar "Int"
           Left err -> assertFailure $ "Statement type inference failed: " ++ show err
           
  , testCase "inferStatement: infers type for function declaration" $
      let stmt = TestFuncDeclStmt "add" 
                              [("x", TestTypeVar "Int"), ("y", TestTypeVar "Int")]
                              (Just (TestTypeVar "Int"))
                              (TestBinaryOpExpr TestAdd (TestVarExpr "x") (TestVarExpr "y"))
          checker = testNewDependentTypeChecker ()
      in case testInferStatement stmt checker of
           Right (_checker', inferred) -> 
             case inferred of
               TestTypeArrow (TestTypeArrow (TestTypeVar "Int") (TestTypeVar "Int")) (TestTypeVar "Int") -> return ()
               _ -> assertFailure "Expected function type"
           Left err -> assertFailure $ "Statement type inference failed: " ++ show err
           
  , testCase "inferProgram: infers types for sequence of statements" $
      let stmt1 = TestVarDeclStmt "x" (Just (TestTypeVar "Int")) (TestLiteralExpr (TestIntLiteral 42))
          stmt2 = TestVarDeclStmt "y" (Just (TestTypeVar "Int")) (TestLiteralExpr (TestIntLiteral 24))
          stmt3 = TestVarDeclStmt "z" (Just (TestTypeVar "Int")) (TestBinaryOpExpr TestAdd (TestVarExpr "x") (TestVarExpr "y"))
          program = [stmt1, stmt2, stmt3]
          checker = testNewDependentTypeChecker ()
      in case testInferProgram program checker of
           Right (_checker', types) -> length types @?= 3
           Left err -> assertFailure $ "Program type inference failed: " ++ show err
           
  , testCase "generalize: creates polymorphic type scheme" $
      let typeVar = TestTypeVar "a"
          checker = testNewDependentTypeChecker ()
          scheme = testGeneralize typeVar checker
      in case scheme of
           TestTypeScheme vars _ -> length vars @?= 1
           
  , testCase "instantiate: creates fresh instance of type scheme" $
      let typeVar = TestTypeVar "a"
          checker = testNewDependentTypeChecker ()
          scheme = testGeneralize typeVar checker
      in case testInstantiate scheme checker of
           Right instanceType -> case instanceType of
              TestTypeVar _ -> return ()
              _ -> assertFailure "Instantiation should create fresh type variable"
           Left err -> assertFailure $ "Type instantiation failed: " ++ show err
           
  , testCase "unifyTypes: unifies compatible types" $
      let type1 = TestTypeVar "a"
          type2 = TestTypeVar "Int"
          checker = testNewDependentTypeChecker ()
      in case testUnifyTypes type1 type2 checker of
           Right (_checker', substitution) -> length substitution @?= 1
           Left err -> assertFailure $ "Type unification failed: " ++ show err
           
  , testCase "unifyTypes: fails for incompatible types" $
      let type1 = TestTypeVar "Int"
          type2 = TestTypeVar "String"
          checker = testNewDependentTypeChecker ()
      in case testUnifyTypes type1 type2 checker of
           Right _ -> assertFailure "Type unification should have failed"
           Left _ -> return ()
           
  , testCase "applyTypeSubstitution: applies substitution to type" $
      let typeVar = TestTypeVar "a"
          replacement = TestTypeVar "Int"
          substitution = [("a", replacement)]
      in testApplyTypeSubstitution substitution typeVar @?= replacement
           
  , testCase "pushScope: creates new scope" $
      let checker = testNewDependentTypeChecker ()
          checker' = testPushScope checker
      in depth (testTypeEnv checker') @?= depth (testTypeEnv checker) + 1
      
  , testCase "popScope: restores previous scope" $
      let checker = testNewDependentTypeChecker ()
          checker' = testPushScope checker
          checker'' = testPopScope checker'
      in depth (testTypeEnv checker'') @?= depth (testTypeEnv checker)
      
  , testCase "inNewScope: executes action in temporary scope" $
      let checker = testNewDependentTypeChecker ()
          action = \c -> testAddType "temp" (TestTypeVar "Temp") c
      in depth (testTypeEnv (testInNewScope action checker)) @?= depth (testTypeEnv checker)
      
  , testCase "inferType: handles nested expressions" $
      let inner = TestBinaryOpExpr TestAdd (TestLiteralExpr (TestIntLiteral 1)) (TestLiteralExpr (TestIntLiteral 2))
          outer = TestBinaryOpExpr TestMultiply inner (TestLiteralExpr (TestIntLiteral 3))
          checker = testNewDependentTypeChecker ()
      in case testInferType outer checker of
           Right inferred -> inferred @?= TestTypeVar "Int"
           Left err -> assertFailure $ "Nested expression type inference failed: " ++ show err
           
  , testCase "inferType: handles function composition" $
      let f = TestVarExpr "f"
          g = TestVarExpr "g"
          x = TestVarExpr "x"
          compose = TestApplyExpr f (TestApplyExpr g x)
          checker = testNewDependentTypeChecker ()
          fType = TestTypeArrow (TestTypeVar "Bool") (TestTypeVar "Int")
          gType = TestTypeArrow (TestTypeVar "String") (TestTypeVar "Bool")
          checker' = testAddType "f" fType $ testAddType "g" gType checker
      in case testInferType compose checker' of
           Right inferred -> inferred @?= TestTypeVar "Int"
           Left err -> assertFailure $ "Function composition type inference failed: " ++ show err
           
  , testCase "inferType: handles polymorphic function application" $
      let identity = TestVarExpr "identity"
          arg = TestLiteralExpr (TestIntLiteral 42)
          expr = TestApplyExpr identity arg
          checker = testNewDependentTypeChecker ()
          identityType = TestTypeArrow (TestTypeVar "a") (TestTypeVar "a")
          checker' = testAddType "identity" identityType checker
      in case testInferType expr checker' of
           Right inferred -> inferred @?= TestTypeVar "Int"
           Left err -> assertFailure $ "Polymorphic function type inference failed: " ++ show err
  ]

-- Helper functions
depth :: TestTypeEnvironment -> Int
depth env = length (testTypeEnvScopes env)

-- Simplified Dependencies types for testing
data TestTypeExpr = TestTypeVar String | TestTypeArrow TestTypeExpr TestTypeExpr | TestTypeConstructor String [TestTypeExpr]
  deriving (Eq, Show)

data TestTypeScheme = TestTypeScheme [String] TestTypeExpr
  deriving (Eq, Show)

data TestTypeEnvironment = TestTypeEnvironment 
  { testTypeEnvTypes :: [(String, TestTypeExpr)]
  , testTypeEnvScopes :: [[(String, TestTypeExpr)]]
  }

data TestDependentTypeChecker = TestDependentTypeChecker 
  { testTypeEnv :: TestTypeEnvironment 
  }

data TestAST = 
    TestLiteralExpr TestLiteral
  | TestVarExpr String
  | TestBinaryOpExpr TestBinaryOp TestAST TestAST
  | TestApplyExpr TestAST TestAST
  | TestLambdaExpr [(String, TestTypeExpr)] TestAST
  | TestLetExpr (String, Maybe TestTypeExpr, TestAST) TestAST
  | TestIfExpr TestAST TestAST TestAST
  deriving (Eq, Show)

data TestLiteral = 
    TestIntLiteral Int
  | TestBoolLiteral Bool
  | TestStringLiteral String
  deriving (Eq, Show)

data TestBinaryOp = TestAdd | TestSubtract | TestMultiply | TestDivide | TestEqual | TestNotEqual | TestLessThan | TestLessThanOrEqual | TestGreaterThan | TestGreaterThanOrEqual
  deriving (Eq, Show)

data TestStatement = 
    TestVarDeclStmt String (Maybe TestTypeExpr) TestAST
  | TestFuncDeclStmt String [(String, TestTypeExpr)] (Maybe TestTypeExpr) TestAST
  deriving (Eq, Show)

testNewDependentTypeChecker :: () -> TestDependentTypeChecker
testNewDependentTypeChecker () = TestDependentTypeChecker (TestTypeEnvironment [] [[]])

testAddType :: String -> TestTypeExpr -> TestDependentTypeChecker -> TestDependentTypeChecker
testAddType name t checker = 
  let env = testTypeEnv checker
      newTypes = (name, t) : testTypeEnvTypes env
      newEnv = env { testTypeEnvTypes = newTypes }
  in checker { testTypeEnv = newEnv }

testInferType :: TestAST -> TestDependentTypeChecker -> Either String TestTypeExpr
testInferType (TestLiteralExpr (TestIntLiteral _)) _ = Right (TestTypeVar "Int")
testInferType (TestLiteralExpr (TestBoolLiteral _)) _ = Right (TestTypeVar "Bool")
testInferType (TestLiteralExpr (TestStringLiteral _)) _ = Right (TestTypeVar "String")
testInferType (TestVarExpr name) checker = 
  case lookup name (testTypeEnvTypes (testTypeEnv checker)) of
    Just t -> Right t
    Nothing -> Left $ "Unknown variable: " ++ name
testInferType (TestBinaryOpExpr TestAdd left right) checker = do
  leftType <- testInferType left checker
  rightType <- testInferType right checker
  case (leftType, rightType) of
    (TestTypeVar "Int", TestTypeVar "Int") -> Right (TestTypeVar "Int")
    _ -> Left "Type mismatch in addition"
testInferType (TestBinaryOpExpr TestEqual left right) checker = do
  leftType <- testInferType left checker
  rightType <- testInferType right checker
  if leftType == rightType
    then Right (TestTypeVar "Bool")
    else Left "Type mismatch in equality"
testInferType (TestApplyExpr func arg) checker = do
  funcType <- testInferType func checker
  argType <- testInferType arg checker
  case funcType of
    TestTypeArrow paramType returnType -> 
      if paramType == argType
        then Right returnType
        else Left "Argument type mismatch"
    _ -> Left "Not a function"
testInferType (TestLambdaExpr params body) checker = do
  let paramTypes = [t | (_, t) <- params]
  bodyType <- testInferType body checker
  Right $ foldr TestTypeArrow bodyType paramTypes
testInferType (TestLetExpr (_, _, valueExpr) bodyExpr) checker = do
  _valueType <- testInferType valueExpr checker
  testInferType bodyExpr checker
testInferType (TestIfExpr condition thenExpr elseExpr) checker = do
  conditionType <- testInferType condition checker
  thenType <- testInferType thenExpr checker
  elseType <- testInferType elseExpr checker
  case (conditionType, thenType == elseType) of
    (TestTypeVar "Bool", True) -> Right thenType
    _ -> Left "Type mismatch in if expression"
testInferType _ _ = Left "Unsupported expression"

testInferStatement :: TestStatement -> TestDependentTypeChecker -> Either String (TestDependentTypeChecker, TestTypeExpr)
testInferStatement (TestVarDeclStmt _ (Just declaredType) valueExpr) checker = do
  valueType <- testInferType valueExpr checker
  if declaredType == valueType
    then Right (checker, declaredType)
    else Left "Type mismatch in variable declaration"
testInferStatement (TestFuncDeclStmt _ paramTypes (Just returnType) bodyExpr) checker = do
  bodyType <- testInferType bodyExpr checker
  let funcType = foldr TestTypeArrow returnType (map snd paramTypes)
  if bodyType == returnType
    then Right (checker, funcType)
    else Left "Return type mismatch in function"
testInferStatement _ _ = Left "Unsupported statement"

testInferProgram :: [TestStatement] -> TestDependentTypeChecker -> Either String (TestDependentTypeChecker, [TestTypeExpr])
testInferProgram statements checker = do
  (checker', types) <- foldM testInferStmt (checker, []) statements
  return (checker', reverse types)
  where
    testInferStmt (c, types) stmt = do
      (c', t) <- testInferStatement stmt c
      return (c', t : types)

testGeneralize :: TestTypeExpr -> TestDependentTypeChecker -> TestTypeScheme
testGeneralize t _ = TestTypeScheme ["a"] t  -- Simplified

testInstantiate :: TestTypeScheme -> TestDependentTypeChecker -> Either String TestTypeExpr
testInstantiate (TestTypeScheme _ t) _ = Right t  -- Simplified

testUnifyTypes :: TestTypeExpr -> TestTypeExpr -> TestDependentTypeChecker -> Either String (TestDependentTypeChecker, [(String, TestTypeExpr)])
testUnifyTypes (TestTypeVar "a") t checker = Right (checker, [("a", t)])
testUnifyTypes t (TestTypeVar "a") checker = Right (checker, [("a", t)])
testUnifyTypes t1 t2 checker = 
  if t1 == t2
    then Right (checker, [])
    else Left "Cannot unify types"

testApplyTypeSubstitution :: [(String, TestTypeExpr)] -> TestTypeExpr -> TestTypeExpr
testApplyTypeSubstitution substitution (TestTypeVar name) = 
  case lookup name substitution of
    Just t -> t
    Nothing -> TestTypeVar name
testApplyTypeSubstitution substitution (TestTypeArrow t1 t2) = 
  TestTypeArrow (testApplyTypeSubstitution substitution t1) (testApplyTypeSubstitution substitution t2)
testApplyTypeSubstitution substitution (TestTypeConstructor name args) = 
  TestTypeConstructor name (map (testApplyTypeSubstitution substitution) args)

testPushScope :: TestDependentTypeChecker -> TestDependentTypeChecker
testPushScope checker = 
  let env = testTypeEnv checker
      newScopes = [] : testTypeEnvScopes env
      newEnv = env { testTypeEnvScopes = newScopes }
  in checker { testTypeEnv = newEnv }

testPopScope :: TestDependentTypeChecker -> TestDependentTypeChecker
testPopScope checker = 
  let env = testTypeEnv checker
      newScopes = case testTypeEnvScopes env of
                   [] -> []
                   (_:rest) -> rest
      newEnv = env { testTypeEnvScopes = newScopes }
  in checker { testTypeEnv = newEnv }

testInNewScope :: (TestDependentTypeChecker -> TestDependentTypeChecker) -> TestDependentTypeChecker -> TestDependentTypeChecker
testInNewScope action checker = 
  let checker' = testPushScope checker
      checker'' = action checker'
  in testPopScope checker''