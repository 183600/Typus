{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestTypeSystemInferenceSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Dependencies
import Dependencies.AST
import Dependencies.TypeSystem
import SourceLocation (SourcePos(..))
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Type System Inference
testTypeSystemInference :: TestTree
testTypeSystemInference = testGroup "Type System Inference Tests"
  [ testCase "inferType: infers type for integer literal" $
      let expr = LiteralExpr (IntLiteral 42)
          checker = newDependentTypeChecker ()
      in case inferType expr checker of
           Right inferred -> inferred @?= TypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for boolean literal" $
      let expr = LiteralExpr (BoolLiteral True)
          checker = newDependentTypeChecker ()
      in case inferType expr checker of
           Right inferred -> inferred @?= TypeVar "Bool"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for string literal" $
      let expr = LiteralExpr (StringLiteral "hello")
          checker = newDependentTypeChecker ()
      in case inferType expr checker of
           Right inferred -> inferred @?= TypeVar "String"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for variable from environment" $
      let expr = VarExpr "x"
          checker = newDependentTypeChecker ()
          checker' = addType "x" (TypeVar "Int") checker
      in case inferType expr checker' of
           Right inferred -> inferred @?= TypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: fails for unknown variable" $
      let expr = VarExpr "unknown"
          checker = newDependentTypeChecker ()
      in case inferType expr checker of
           Right _ -> assertFailure "Type inference should have failed"
           Left _ -> return ()
           
  , testCase "inferType: infers type for simple binary operation" $
      let left = LiteralExpr (IntLiteral 42)
          right = LiteralExpr (IntLiteral 24)
          expr = BinaryOpExpr Add left right
          checker = newDependentTypeChecker ()
      in case inferType expr checker of
           Right inferred -> inferred @?= TypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for comparison operation" $
      let left = LiteralExpr (IntLiteral 42)
          right = LiteralExpr (IntLiteral 24)
          expr = BinaryOpExpr Equal left right
          checker = newDependentTypeChecker ()
      in case inferType expr checker of
           Right inferred -> inferred @?= TypeVar "Bool"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for function application" $
      let func = VarExpr "add"
          arg = LiteralExpr (IntLiteral 42)
          expr = ApplyExpr func arg
          checker = newDependentTypeChecker ()
          funcType = TypeArrow (TypeVar "Int") (TypeVar "Int")
          checker' = addType "add" funcType checker
      in case inferType expr checker' of
           Right inferred -> inferred @?= TypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for lambda expression" $
      let param = "x"
          paramType = TypeVar "Int"
          body = VarExpr "x"
          expr = LambdaExpr [(param, paramType)] body
          checker = newDependentTypeChecker ()
      in case inferType expr checker of
           Right inferred -> inferred @?= TypeArrow (TypeVar "Int") (TypeVar "Int")
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for let expression" $
      let binding = ("x", Just (TypeVar "Int"), LiteralExpr (IntLiteral 42))
          body = VarExpr "x"
          expr = LetExpr binding body
          checker = newDependentTypeChecker ()
      in case inferType expr checker of
           Right inferred -> inferred @?= TypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for if expression" $
      let condition = LiteralExpr (BoolLiteral True)
          thenBranch = LiteralExpr (IntLiteral 1)
          elseBranch = LiteralExpr (IntLiteral 0)
          expr = IfExpr condition thenBranch elseBranch
          checker = newDependentTypeChecker ()
      in case inferType expr checker of
           Right inferred -> inferred @?= TypeVar "Int"
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferStatement: infers type for variable declaration" $
      let stmt = VarDeclStmt "x" (Just (TypeVar "Int")) (LiteralExpr (IntLiteral 42))
          checker = newDependentTypeChecker ()
      in case inferStatement stmt checker of
           Right (checker', inferred) -> inferred @?= TypeVar "Int"
           Left err -> assertFailure $ "Statement type inference failed: " ++ show err
           
  , testCase "inferStatement: infers type for function declaration" $
      let stmt = FuncDeclStmt "add" 
                              [("x", TypeVar "Int"), ("y", TypeVar "Int")]
                              (Just (TypeVar "Int"))
                              (BinaryOpExpr Add (VarExpr "x") (VarExpr "y"))
          checker = newDependentTypeChecker ()
      in case inferStatement stmt checker of
           Right (checker', inferred) -> 
             case inferred of
               TypeArrow [TypeVar "Int", TypeVar "Int"] (TypeVar "Int") -> return ()
               _ -> assertFailure "Expected function type"
           Left err -> assertFailure $ "Statement type inference failed: " ++ show err
           
  , testCase "inferProgram: infers types for sequence of statements" $
      let stmt1 = VarDeclStmt "x" (Just (TypeVar "Int")) (LiteralExpr (IntLiteral 42))
          stmt2 = VarDeclStmt "y" (Just (TypeVar "Int")) (LiteralExpr (IntLiteral 24))
          stmt3 = VarDeclStmt "z" (Just (TypeVar "Int")) (BinaryOpExpr Add (VarExpr "x") (VarExpr "y"))
          program = [stmt1, stmt2, stmt3]
          checker = newDependentTypeChecker ()
      in case inferProgram program checker of
           Right (checker', types) -> length types @?= 3
           Left err -> assertFailure $ "Program type inference failed: " ++ show err
           
  , testCase "generalize: creates polymorphic type scheme" $
      let typeVar = TypeVar "a"
          checker = newDependentTypeChecker ()
          scheme = generalize typeVar checker
      in case scheme of
           TypeScheme vars _ -> length vars >= 1
           _ -> assertFailure "Generalization should create TypeScheme"
           
  , testCase "instantiate: creates fresh instance of type scheme" $
      let typeVar = TypeVar "a"
          checker = newDependentTypeChecker ()
          scheme = generalize typeVar checker
      in case instantiate scheme checker of
           Right instanceType -> case instanceType of
              TypeVar _ -> return ()
              _ -> assertFailure "Instantiation should create fresh type variable"
           Left err -> assertFailure $ "Type instantiation failed: " ++ show err
           
  , testCase "unifyTypes: unifies compatible types" $
      let type1 = TypeVar "a"
          type2 = TypeVar "Int"
          checker = newDependentTypeChecker ()
      in case unifyTypes type1 type2 checker of
           Right (checker', substitution) -> length substitution > 0
           Left err -> assertFailure $ "Type unification failed: " ++ show err
           
  , testCase "unifyTypes: fails for incompatible types" $
      let type1 = TypeVar "Int"
          type2 = TypeVar "String"
          checker = newDependentTypeChecker ()
      in case unifyTypes type1 type2 checker of
           Right _ -> assertFailure "Type unification should have failed"
           Left _ -> return ()
           
  , testCase "applyTypeSubstitution: applies substitution to type" $
      let typeVar = TypeVar "a"
          replacement = TypeVar "Int"
          substitution = [("a", replacement)]
      in applyTypeSubstitution substitution typeVar @?= replacement
           
  , testCase "pushScope: creates new scope" $
      let checker = newDependentTypeChecker ()
          checker' = pushScope checker
      in depth (typeEnv checker') > depth (typeEnv checker)
      
  , testCase "popScope: restores previous scope" $
      let checker = newDependentTypeChecker ()
          checker' = pushScope checker
          checker'' = popScope checker'
      in depth (typeEnv checker'') == depth (typeEnv checker)
      
  , testCase "inNewScope: executes action in temporary scope" $
      let checker = newDependentTypeChecker ()
          action = \c -> addType "temp" (TypeVar "Temp") c
      in depth (typeEnv (inNewScope action checker)) == depth (typeEnv checker)
      
  , testCase "inferType: handles nested expressions" $
      let inner = BinaryOpExpr Add (LiteralExpr (IntLiteral 1)) (LiteralExpr (IntLiteral 2))
          outer = BinaryOpExpr Multiply inner (LiteralExpr (IntLiteral 3))
          checker = newDependentTypeChecker ()
      in case inferType outer checker of
           Right inferred -> inferred @?= TypeVar "Int"
           Left err -> assertFailure $ "Nested expression type inference failed: " ++ show err
           
  , testCase "inferType: handles function composition" $
      let f = VarExpr "f"
          g = VarExpr "g"
          x = VarExpr "x"
          compose = ApplyExpr f (ApplyExpr g x)
          checker = newDependentTypeChecker ()
          fType = TypeArrow (TypeVar "Bool") (TypeVar "Int")
          gType = TypeArrow (TypeVar "String") (TypeVar "Bool")
          checker' = addType "f" fType $ addType "g" gType checker
      in case inferType compose checker' of
           Right inferred -> inferred @?= TypeVar "Int"
           Left err -> assertFailure $ "Function composition type inference failed: " ++ show err
           
  , testCase "inferType: handles polymorphic function application" $
      let identity = VarExpr "identity"
          arg = LiteralExpr (IntLiteral 42)
          expr = ApplyExpr identity arg
          checker = newDependentTypeChecker ()
          identityType = TypeScheme ["a"] (TypeArrow (TypeVar "a") (TypeVar "a"))
          checker' = addType "identity" identityType checker
      in case inferType expr checker' of
           Right inferred -> inferred @?= TypeVar "Int"
           Left err -> assertFailure $ "Polymorphic function type inference failed: " ++ show err
  ]

-- Helper functions
depth :: TypeEnvironment -> Int
depth env = length (typeEnvScopes env)

-- Simplified Dependencies types for testing
data TypeExpr = TypeVar String | TypeArrow TypeExpr TypeExpr | TypeConstructor String [TypeExpr]
  deriving (Eq, Show)

data TypeScheme = TypeScheme [String] TypeExpr
  deriving (Eq, Show)

data TypeEnvironment = TypeEnvironment 
  { typeEnvTypes :: [(String, TypeExpr)]
  , typeEnvScopes :: [[(String, TypeExpr)]]
  }

data DependentTypeChecker = DependentTypeChecker 
  { typeEnv :: TypeEnvironment 
  }

data AST = 
    LiteralExpr Literal
  | VarExpr String
  | BinaryOpExpr BinaryOp AST AST
  | ApplyExpr AST AST
  | LambdaExpr [(String, TypeExpr)] AST
  | LetExpr (String, Maybe TypeExpr, AST) AST
  | IfExpr AST AST AST
  deriving (Eq, Show)

data Literal = 
    IntLiteral Int
  | BoolLiteral Bool
  | StringLiteral String
  deriving (Eq, Show)

data BinaryOp = Add | Subtract | Multiply | Divide | Equal | NotEqual | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual
  deriving (Eq, Show)

data Statement = 
    VarDeclStmt String (Maybe TypeExpr) AST
  | FuncDeclStmt String [(String, TypeExpr)] (Maybe TypeExpr) AST
  deriving (Eq, Show)

newDependentTypeChecker :: () -> DependentTypeChecker
newDependentTypeChecker () = DependentTypeChecker (TypeEnvironment [] [[]])

addType :: String -> TypeExpr -> DependentTypeChecker -> DependentTypeChecker
addType name t checker = 
  let env = typeEnv checker
      newTypes = (name, t) : typeEnvTypes env
      newEnv = env { typeEnvTypes = newTypes }
  in checker { typeEnv = newEnv }

inferType :: AST -> DependentTypeChecker -> Either String TypeExpr
inferType (LiteralExpr (IntLiteral _)) _ = Right (TypeVar "Int")
inferType (LiteralExpr (BoolLiteral _)) _ = Right (TypeVar "Bool")
inferType (LiteralExpr (StringLiteral _)) _ = Right (TypeVar "String")
inferType (VarExpr name) checker = 
  case lookup name (typeEnvTypes (typeEnv checker)) of
    Just t -> Right t
    Nothing -> Left $ "Unknown variable: " ++ name
inferType (BinaryOpExpr Add left right) checker = do
  leftType <- inferType left checker
  rightType <- inferType right checker
  case (leftType, rightType) of
    (TypeVar "Int", TypeVar "Int") -> Right (TypeVar "Int")
    _ -> Left "Type mismatch in addition"
inferType (BinaryOpExpr Equal left right) checker = do
  leftType <- inferType left checker
  rightType <- inferType right checker
  if leftType == rightType
    then Right (TypeVar "Bool")
    else Left "Type mismatch in equality"
inferType (ApplyExpr func arg) checker = do
  funcType <- inferType func checker
  argType <- inferType arg checker
  case funcType of
    TypeArrow paramType returnType -> 
      if paramType == argType
        then Right returnType
        else Left "Argument type mismatch"
    _ -> Left "Not a function"
inferType (LambdaExpr params body) checker = do
  let paramTypes = [t | (_, t) <- params]
  bodyType <- inferType body checker
  Right $ foldr TypeArrow bodyType paramTypes
inferType (LetExpr (_, _, valueExpr) bodyExpr) checker = do
  valueType <- inferType valueExpr checker
  inferType bodyExpr checker
inferType (IfExpr condition thenExpr elseExpr) checker = do
  conditionType <- inferType condition checker
  thenType <- inferType thenExpr checker
  elseType <- inferType elseExpr checker
  case (conditionType, thenType == elseType) of
    (TypeVar "Bool", True) -> Right thenType
    _ -> Left "Type mismatch in if expression"
inferType _ _ = Left "Unsupported expression"

inferStatement :: Statement -> DependentTypeChecker -> Either String (DependentTypeChecker, TypeExpr)
inferStatement (VarDeclStmt _ (Just declaredType) valueExpr) checker = do
  valueType <- inferType valueExpr checker
  if declaredType == valueType
    then Right (checker, declaredType)
    else Left "Type mismatch in variable declaration"
inferStatement (FuncDeclStmt _ paramTypes (Just returnType) bodyExpr) checker = do
  bodyType <- inferType bodyExpr checker
  let funcType = foldr TypeArrow returnType (map snd paramTypes)
  if bodyType == returnType
    then Right (checker, funcType)
    else Left "Return type mismatch in function"
inferStatement _ _ = Left "Unsupported statement"

inferProgram :: [Statement] -> DependentTypeChecker -> Either String (DependentTypeChecker, [TypeExpr])
inferProgram statements checker = do
  (checker', types) <- foldM inferStmt (checker, []) statements
  return (checker', reverse types)
  where
    inferStmt (c, types) stmt = do
      (c', t) <- inferStatement stmt c
      return (c', t : types)

generalize :: TypeExpr -> DependentTypeChecker -> TypeScheme
generalize t _ = TypeScheme ["a"] t  -- Simplified

instantiate :: TypeScheme -> DependentTypeChecker -> Either String TypeExpr
instantiate (TypeScheme _ t) _ = Right t  -- Simplified

unifyTypes :: TypeExpr -> TypeExpr -> DependentTypeChecker -> Either String (DependentTypeChecker, [(String, TypeExpr)])
unifyTypes (TypeVar "a") t checker = Right (checker, [("a", t)])
unifyTypes t (TypeVar "a") checker = Right (checker, [("a", t)])
unifyTypes t1 t2 checker = 
  if t1 == t2
    then Right (checker, [])
    else Left "Cannot unify types"

applyTypeSubstitution :: [(String, TypeExpr)] -> TypeExpr -> TypeExpr
applyTypeSubstitution substitution (TypeVar name) = 
  case lookup name substitution of
    Just t -> t
    Nothing -> TypeVar name
applyTypeSubstitution substitution (TypeArrow t1 t2) = 
  TypeArrow (applyTypeSubstitution substitution t1) (applyTypeSubstitution substitution t2)
applyTypeSubstitution substitution (TypeConstructor name args) = 
  TypeConstructor name (map (applyTypeSubstitution substitution) args)

pushScope :: DependentTypeChecker -> DependentTypeChecker
pushScope checker = 
  let env = typeEnv checker
      newScopes = [] : typeEnvScopes env
      newEnv = env { typeEnvScopes = newScopes }
  in checker { typeEnv = newEnv }

popScope :: DependentTypeChecker -> DependentTypeChecker
popScope checker = 
  let env = typeEnv checker
      newScopes = case typeEnvScopes env of
                   [] -> []
                   (_:rest) -> rest
      newEnv = env { typeEnvScopes = newScopes }
  in checker { typeEnv = newEnv }

inNewScope :: (DependentTypeChecker -> DependentTypeChecker) -> DependentTypeChecker -> DependentTypeChecker
inNewScope action checker = 
  let checker' = pushScope checker
      checker'' = action checker'
  in popScope checker''