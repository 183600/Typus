{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module DependencyAnalysisTestSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen)
import qualified Data.Set as Set
import Data.List (nub, (\\), intersect)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Dependencies.AST as Dep
import qualified Dependencies.TypeSystem as Dep

-- Helper generators for dependency analysis tests
genVarName :: Gen String
genVarName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
  return (first : rest)

genTypeName :: Gen String
genTypeName = do
  first <- elements ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return (first : rest)

genFunName :: Gen String
genFunName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return (first : rest)

genSimpleTypeExpr :: Gen Dep.TypeExpr
genSimpleTypeExpr = oneof
  [ Dep.TypeConstructor <$> genTypeName
  , Dep.TypeVariable <$> genVarName
  ]

genFunctionTypeExpr :: Gen Dep.TypeExpr
genFunctionTypeExpr = do
  paramCount <- choose (1, 3)
  paramTypes <- vectorOf paramCount genSimpleTypeExpr
  returnType <- genSimpleTypeExpr
  return $ Dep.TypeFunction paramTypes returnType

genTypeExpr :: Gen Dep.TypeExpr
genTypeExpr = oneof
  [ genSimpleTypeExpr
  , genFunctionTypeExpr
  ]

genSimpleStatement :: Gen Dep.Statement
genSimpleStatement = oneof
  [ do
      varName <- genVarName
      typeExpr <- genTypeExpr
      return $ Dep.VarDecl varName typeExpr
  , do
      funName <- genFunName
      paramCount <- choose (0, 3)
      params <- vectorOf paramCount genVarName
      returnType <- genTypeExpr
      return $ Dep.FunDecl funName params returnType
  ]

genComplexStatement :: Gen Dep.Statement
genComplexStatement = oneof
  [ do
      varName <- genVarName
      funName <- genFunName
      argCount <- choose (0, 3)
      args <- vectorOf argCount genVarName
      return $ Dep.FunctionCall varName funName args
  , do
      condition <- genVarName
      thenStmt <- genSimpleStatement
      elseStmt <- genSimpleStatement
      return $ Dep.IfStatement condition thenStmt elseStmt
  , do
      varName <- genVarName
      collection <- genVarName
      body <- genSimpleStatement
      return $ Dep.ForLoop varName collection body
  ]

genStatement :: Gen Dep.Statement
genStatement = oneof [genSimpleStatement, genComplexStatement]

genAST :: Gen Dep.AST
genAST = do
  stmtCount <- choose (1, 10)
  statements <- vectorOf stmtCount genStatement
  return $ Dep.AST statements

instance Arbitrary Dep.TypeExpr where
  arbitrary = genTypeExpr

instance Arbitrary Dep.Statement where
  arbitrary = genStatement

instance Arbitrary Dep.AST where
  arbitrary = genAST

-- Test properties for dependency analysis

-- Property 1: AST statements are preserved in order
prop_astStatementOrder :: [Dep.Statement] -> Bool
prop_astStatementOrder statements =
  let ast = Dep.AST statements
  in case ast of
    Dep.AST stmts -> stmts == statements
    _ -> False

-- Property 2: Variable declarations preserve variable names
prop_varDeclPreservesName :: String -> Dep.TypeExpr -> Bool
prop_varDeclPreservesName varName typeExpr =
  let stmt = Dep.VarDecl varName typeExpr
  in case stmt of
    Dep.VarDecl name t -> name == varName && t == typeExpr
    _ -> False

-- Property 3: Function declarations preserve function names
prop_funDeclPreservesName :: String -> [String] -> Dep.TypeExpr -> Bool
prop_funDeclPreservesName funName params returnType =
  let stmt = Dep.FunDecl funName params returnType
  in case stmt of
    Dep.FunDecl name p r -> name == funName && p == params && r == returnType
    _ -> False

-- Property 4: Function calls preserve function and argument names
prop_functionCallPreservesNames :: String -> String -> [String] -> Bool
prop_functionCallPreservesNames varName funName args =
  let stmt = Dep.FunctionCall varName funName args
  in case stmt of
    Dep.FunctionCall v f a -> v == varName && f == funName && a == args
    _ -> False

-- Property 5: Type constructors preserve type names
prop_typeConstructorPreservesName :: String -> Bool
prop_typeConstructorPreservesName typeName =
  let typeExpr = Dep.TypeConstructor typeName
  in case typeExpr of
    Dep.TypeConstructor name -> name == typeName
    _ -> False

-- Property 6: Type variables preserve variable names
prop_typeVariablePreservesName :: String -> Bool
prop_typeVariablePreservesName varName =
  let typeExpr = Dep.TypeVariable varName
  in case typeExpr of
    Dep.TypeVariable name -> name == varName
    _ -> False

-- Property 7: Function types have correct parameter count
prop_functionTypeParamCount :: [Dep.TypeExpr] -> Dep.TypeExpr -> Bool
prop_functionTypeParamCount paramTypes returnType =
  let funcType = Dep.TypeFunction paramTypes returnType
  in case funcType of
    Dep.TypeFunction ps rt -> length ps == length paramTypes && rt == returnType
    _ -> False

-- Property 8: Free variables in type expressions are preserved
prop_freeVarsPreserved :: Dep.TypeExpr -> Bool
prop_freeVarsPreserved typeExpr =
  let freeVars = Dep.freeVars typeExpr
      -- Check that all free variables are actually variables in the expression
      isVarInExpr var = case typeExpr of
        Dep.TypeVariable name -> name == var
        Dep.TypeFunction params ret -> 
          any (\p -> case p of Dep.TypeVariable name -> name == var; _ -> False) params ||
          case ret of Dep.TypeVariable name -> name == var; _ -> False
        _ -> False
  in all isVarInExpr freeVars

dependencyAnalysisTests :: TestTree
dependencyAnalysisTests = testGroup "Dependency Analysis Tests"
  [ testProperties "AST Properties"
    [ ("AST statements are preserved in order", prop_astStatementOrder)
    ]
  , testProperties "Statement Properties"
    [ ("Variable declarations preserve variable names", prop_varDeclPreservesName)
    , ("Function declarations preserve function names", prop_funDeclPreservesName)
    , ("Function calls preserve function and argument names", prop_functionCallPreservesNames)
    ]
  , testProperties "Type Expression Properties"
    [ ("Type constructors preserve type names", prop_typeConstructorPreservesName)
    , ("Type variables preserve variable names", prop_typeVariablePreservesName)
    , ("Function types have correct parameter count", prop_functionTypeParamCount)
    , ("Free variables in type expressions are preserved", prop_freeVarsPreserved)
    ]
  ]