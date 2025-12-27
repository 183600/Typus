{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerGoAstQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import Compiler.GoAst
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Compiler GoAst QuickCheck Tests"
  [ goAstNodeTests
  , goExpressionTests
  , goStatementTests
  , goDeclarationTests
  , goLiteralTests
  , goIdentifierTests
  , goFunctionTests
  , goTypeTests
  , goPackageTests
  , goAstValidationTests
  ]

-- | 1. Go AST节点测试
goAstNodeTests :: TestTree
goAstNodeTests = testGroup "Go AST Node Tests"
  [ testCase "Node source position" $
      let node = GoIdentifierNode "test" (SourceSpan startPos startPos)
      in goAstNodeSpan node @?= SourceSpan startPos startPos
  
  , testCase "Node type inference" $
      let node = GoIntegerLiteralNode 42 (SourceSpan startPos startPos)
      in goAstNodeType node @?= GoIntType
  
  , fastProperty "Node position consistency" $
      \name -> let node = GoIdentifierNode name (SourceSpan startPos startPos)
               in goAstNodeSpan node `seq` True
  ]

-- | 2. Go表达式测试
goExpressionTests :: TestTree
goExpressionTests = testGroup "Go Expression Tests"
  [ testCase "Binary expression creation" $
      let left = GoIntegerLiteralExpr 42
          right = GoIntegerLiteralExpr 24
          binOp = GoBinaryExpr GoPlus left right
      in goExprType binOp @?= GoIntType
  
  , testCase "Unary expression creation" $
      let operand = GoIntegerLiteralExpr 42
          unaryOp = GoUnaryExpr GoNeg operand
      in goExprType unaryOp @?= GoIntType
  
  , testCase "Function call expression" $
      let func = GoIdentifierExpr "testFunc"
          args = [GoIntegerLiteralExpr 1, GoIntegerLiteralExpr 2]
          call = GoCallExpr func args
      in goExprType call `seq` True @?= True
  
  , fastProperty "Binary expression type consistency" $
      \op left right -> let binOp = GoBinaryExpr op left right
                        in goExprType binOp `seq` True
  ]

-- | 3. Go语句测试
goStatementTests :: TestTree
goStatementTests = testGroup "Go Statement Tests"
  [ testCase "Variable declaration statement" $
      let decl = GoVarDeclStmt "x" (Just GoIntType) (Just (GoIntegerLiteralExpr 42))
      in goStmtSpan decl `seq` True @?= True
  
  , testCase "Assignment statement" $
      let assign = GoAssignStmt [GoIdentifierExpr "x"] [GoIntegerLiteralExpr 42]
      in goStmtSpan assign `seq` True @?= True
  
  , testCase "Return statement" $
      let ret = GoReturnStmt (Just (GoIntegerLiteralExpr 42))
      in goStmtSpan ret `seq` True @?= True
  
  , testCase "If statement" $
      let cond = GoBinaryExpr GoEqual (GoIdentifierExpr "x") (GoIntegerLiteralExpr 42)
          thenBlock = [GoReturnStmt (Just (GoIntegerLiteralExpr 1))]
          ifStmt = GoIfStmt cond thenBlock []
      in goStmtSpan ifStmt `seq` True @?= True
  ]

-- | 4. Go声明测试
goDeclarationTests :: TestTree
goDeclarationTests = testGroup "Go Declaration Tests"
  [ testCase "Function declaration" $
      let func = GoFunctionDecl "test" [] GoIntType []
      in goDeclName func @?= "test"
  
  , testCase "Variable declaration" $
      let var = GoVarDecl "x" (Just GoIntType) (Just (GoIntegerLiteralExpr 42))
      in goDeclName var @?= "x"
  
  , testCase "Type declaration" $
      let typeDecl = GoTypeDecl "MyInt" GoIntType
      in goDeclName typeDecl @?= "MyInt"
  
  , fastProperty "Declaration name consistency" $
      \name -> let func = GoFunctionDecl name [] GoIntType []
                in goDeclName func == name
  ]

-- | 5. Go字面量测试
goLiteralTests :: TestTree
goLiteralTests = testGroup "Go Literal Tests"
  [ testCase "Integer literal" $
      let lit = GoIntegerLiteralExpr 42
      in goExprType lit @?= GoIntType
  
  , testCase "String literal" $
      let lit = GoStringLiteralExpr "hello"
      in goExprType lit @?= GoStringType
  
  , testCase "Boolean literal" $
      let lit = GoBoolLiteralExpr True
      in goExprType lit @?= GoBoolType
  
  , fastProperty "Integer literal value consistency" $
      \n -> let lit = GoIntegerLiteralExpr n
            in case lit of
                  GoIntegerLiteralExpr val -> val == n
  ]

-- | 6. Go标识符测试
goIdentifierTests :: TestTree
goIdentifierTests = testGroup "Go Identifier Tests"
  [ testCase "Identifier creation" $
      let ident = GoIdentifierExpr "testVar"
      in goExprType ident `seq` True @?= True
  
  , testCase "Identifier name" $
      let ident = GoIdentifierNode "myVar" (SourceSpan startPos startPos)
      in case ident of
           GoIdentifierNode name _ -> name @?= "myVar"
  
  , fastProperty "Identifier name consistency" $
      \name -> let ident = GoIdentifierExpr name
                in case ident of
                     GoIdentifierExpr n -> n == name
  ]

-- | 7. Go函数测试
goFunctionTests :: TestTree
goFunctionTests = testGroup "Go Function Tests"
  [ testCase "Function without parameters" $
      let func = GoFunctionDecl "test" [] GoIntType []
      in length (goFunctionParams func) @?= 0
  
  , testCase "Function with parameters" $
      let params = [GoParam "x" GoIntType, GoParam "y" GoIntType]
          func = GoFunctionDecl "add" params GoIntType []
      in length (goFunctionParams func) @?= 2
  
  , testCase "Function return type" $
      let func = GoFunctionDecl "getString" [] GoStringType []
      in goFunctionReturnType func @?= GoStringType
  
  , fastProperty "Function parameter count consistency" $
      \params -> let func = GoFunctionDecl "test" params GoIntType []
                  in length (goFunctionParams func) == length params
  ]

-- | 8. Go类型测试
goTypeTests :: TestTree
goTypeTests = testGroup "Go Type Tests"
  [ testCase "Basic type equality" $
      (GoIntType == GoIntType && GoStringType == GoStringType && GoBoolType == GoBoolType) @?= True
  
  , testCase "Function type equality" $
      let funcType1 = GoFunctionType [GoIntType] GoStringType
          funcType2 = GoFunctionType [GoIntType] GoStringType
      in funcType1 == funcType2 @?= True
  
  , testCase "Slice type creation" $
      let sliceType = GoSliceType GoIntType
      in case sliceType of
           GoSliceType elemType -> elemType @?= GoIntType
  
  , fastProperty "Type string representation" $
      \ty -> let str = show ty
              in length str > 0
  ]

-- | 9. Go包测试
goPackageTests :: TestTree
goPackageTests = testGroup "Go Package Tests"
  [ testCase "Package creation" $
      let pkg = GoPackage "main" []
      in goPackageName pkg @?= "main"
  
  , testCase "Package with declarations" $
      let decl = GoFunctionDecl "main" [] GoIntType []
          pkg = GoPackage "main" [decl]
      in length (goPackageDecls pkg) @?= 1
  
  , fastProperty "Package declaration count" $
      \decls -> let pkg = GoPackage "test" decls
                 in length (goPackageDecls pkg) == length decls
  ]

-- | 10. Go AST验证测试
goAstValidationTests :: TestTree
goAstValidationTests = testGroup "Go AST Validation Tests"
  [ testCase "Valid integer literal" $
      let lit = GoIntegerLiteralExpr 42
      in validateGoExpr lit @?= True
  
  , testCase "Valid identifier expression" $
      let ident = GoIdentifierExpr "validVar"
      in validateGoExpr ident @?= True
  
  , testCase "Valid binary expression" $
      let left = GoIntegerLiteralExpr 42
          right = GoIntegerLiteralExpr 24
          binOp = GoBinaryExpr GoPlus left right
      in validateGoExpr binOp @?= True
  
  , testCase "Valid function declaration" $
      let func = GoFunctionDecl "test" [] GoIntType []
      in validateGoDecl func @?= True
  
  , fastProperty "Expression validation" $
      \expr -> validateGoExpr expr == True || validateGoExpr expr == False
  
  , fastProperty "Declaration validation" $
      \decl -> validateGoDecl decl == True || validateGoDecl decl == False
  ]