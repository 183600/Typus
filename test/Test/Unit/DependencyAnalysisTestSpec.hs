{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.DependencyAnalysisTestSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen)
import qualified Data.Set as Set
import Data.List (nub, (\\), intersect)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
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
  [ Dep.SimpleT <$> (T.pack <$> genTypeName)
  , Dep.SimpleT <$> (T.pack <$> genVarName)
  ]

genFunctionTypeExpr :: Gen Dep.TypeExpr
genFunctionTypeExpr = do
  paramCount <- choose (1, 3)
  paramTypes <- vectorOf paramCount genSimpleTypeExpr
  returnType <- genSimpleTypeExpr
  return $ Dep.FuncT (zip (map (T.pack . (("p" ++) . show)) [1..paramCount]) paramTypes) returnType

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
      return $ Dep.SVarDecl (T.pack varName) typeExpr
  , do
      funName <- genFunName
      paramCount <- choose (0, 3)
      params <- vectorOf paramCount $ do
        p <- genVarName
        t <- genTypeExpr
        return (T.pack p, t)
      returnType <- genTypeExpr
      return $ Dep.SFuncDecl (T.pack funName) params (Just returnType)
  ]

genComplexStatement :: Gen Dep.Statement
genComplexStatement = oneof
  [ do
      varName <- genVarName
      typeExpr <- genTypeExpr
      return $ Dep.SVarDecl (T.pack varName) typeExpr
  , do
      funName <- genFunName
      paramCount <- choose (0, 3)
      params <- vectorOf paramCount $ do
        p <- genVarName
        t <- genTypeExpr
        return (T.pack p, t)
      returnType <- genTypeExpr
      return $ Dep.SFuncDecl (T.pack funName) params (Just returnType)
  
  ]

genStatement :: Gen Dep.Statement
genStatement = oneof [genSimpleStatement, genComplexStatement]

genAST :: Gen Dep.AST
genAST = do
  stmtCount <- choose (1, 10)
  statements <- vectorOf stmtCount genStatement
  return $ Dep.Program statements

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
  let ast = Dep.Program statements
  in case ast of
    Dep.Program stmts -> stmts == statements

-- Property 2: Variable declarations preserve variable names
prop_varDeclPreservesName :: String -> Dep.TypeExpr -> Bool
prop_varDeclPreservesName varName typeExpr =
  let stmt = Dep.SVarDecl (T.pack varName) typeExpr
  in case stmt of
    Dep.SVarDecl name t -> T.unpack name == varName && t == typeExpr
    Dep.STypeDef _ _ _ -> False
    Dep.STypeAlias _ _ _ -> False
    Dep.SFuncDecl _ _ _ -> False
    Dep.SConstraintDef _ _ -> False
    Dep.SExistsDecl _ _ -> False

-- Property 3: Function declarations preserve function names
prop_funDeclPreservesName :: String -> [(String, Dep.TypeExpr)] -> Dep.TypeExpr -> Bool
prop_funDeclPreservesName funName params returnType =
  let params' = [(T.pack n, t) | (n, t) <- params]
      stmt = Dep.SFuncDecl (T.pack funName) params' (Just returnType)
  in case stmt of
    Dep.SFuncDecl name p r -> T.unpack name == funName && [(T.unpack n, t) | (n, t) <- p] == params && r == Just returnType
    _ -> False

-- Property 4: Variable declarations preserve variable names (alternative)
prop_varDeclPreservesNameAlt :: String -> Dep.TypeExpr -> Bool
prop_varDeclPreservesNameAlt varName typeExpr =
  let stmt = Dep.SVarDecl (T.pack varName) typeExpr
  in case stmt of
    Dep.SVarDecl name t -> T.unpack name == varName && t == typeExpr
    _ -> False

-- Property 5: Type constructors preserve type names
prop_typeConstructorPreservesName :: String -> Bool
prop_typeConstructorPreservesName typeName =
  let typeExpr = Dep.SimpleT (T.pack typeName)
  in case typeExpr of
    Dep.SimpleT name -> T.unpack name == typeName
    _ -> False

-- Property 6: Generic types preserve type names and parameters
prop_genericTypePreservesNameAndParams :: String -> [Dep.TypeExpr] -> Bool
prop_genericTypePreservesNameAndParams typeName typeParams =
  let typeExpr = Dep.GenericT (T.pack typeName) typeParams
  in case typeExpr of
    Dep.GenericT name params -> T.unpack name == typeName && params == typeParams
    _ -> False

-- Property 7: Function types have correct parameter count
prop_functionTypeParamCount :: [Dep.TypeExpr] -> Dep.TypeExpr -> Bool
prop_functionTypeParamCount paramTypes returnType =
  let funcType = Dep.FuncT (zip (map (T.pack . (("p" ++) . show)) [1..length paramTypes]) paramTypes) returnType
  in case funcType of
    Dep.FuncT ps rt -> length ps == length paramTypes && rt == returnType
    _ -> False

-- Property 8: Simple types preserve type names
prop_simpleTypePreservesName :: String -> Bool
prop_simpleTypePreservesName typeName =
  let typeExpr = Dep.SimpleT (T.pack typeName)
  in case typeExpr of
    Dep.SimpleT name -> T.unpack name == typeName
    _ -> False

dependencyAnalysisTests :: TestTree
dependencyAnalysisTests = testGroup "Dependency Analysis Tests"
  [ testProperties "AST Properties"
    [ ("AST statements are preserved in order", property prop_astStatementOrder)
    ]
  , testProperties "Statement Properties"
    [ ("Variable declarations preserve variable names", property prop_varDeclPreservesName)
    , ("Function declarations preserve function names", property prop_funDeclPreservesName)
    ]
  , testProperties "Type Expression Properties"
    [ ("Type constructors preserve type names", property prop_typeConstructorPreservesName)
    , ("Function types have correct parameter count", property prop_functionTypeParamCount)
    ]
  ]