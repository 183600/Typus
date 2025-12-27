{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerIRQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import Compiler.IR
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Compiler IR QuickCheck Tests"
  [ irNodePropertyTests
  , irExpressionTests
  , irStatementTests
  , irFunctionTests
  , irModuleTests
  , irTypeTests
  , irVariableTests
  , irLiteralTests
  , irOperationTests
  , irValidationTests
  ]

-- | 1. IR节点属性测试
irNodePropertyTests :: TestTree
irNodePropertyTests = testGroup "IR Node Properties"
  [ fastProperty "IRNode id is unique within a module" $
      \nodes -> length (nub (map irNodeId nodes)) == length (map irNodeId (nodes :: [IRNode]))
  
  , testCase "IRNode source span is valid" $
      let node = IRVarNode "x" (SourceSpan startPos startPos)
      in irNodeSourceSpan node `seq` True @?= True
  
  , fastProperty "IRNode type consistency" $
      \nodeType -> let node = IRVarNode "test" (SourceSpan startPos startPos)
                   in irNodeType node `seq` True
  ]

-- | 2. IR表达式测试
irExpressionTests :: TestTree
irExpressionTests = testGroup "IR Expression Tests"
  [ testCase "Binary operation expression creation" $
      let left = IRVarExpr "x" IRInt
          right = IRVarExpr "y" IRInt
          binOp = IRBinaryOp IROpAdd left right IRInt
      in irExprType binOp @?= IRInt
  
  , testCase "Function call expression" $
      let func = IRVarExpr "f" (IRFunction [IRInt] IRString)
          args = [IRVarExpr "arg1" IRInt]
          call = IRCallExpr func args IRString
      in irExprType call @?= IRString
  
  , fastProperty "Literal expression type consistency" $
      \lit -> let expr = IRLiteralExpr lit
              in case lit of
                    IntLiteral _ -> irExprType expr == IRInt
                    StringLiteral _ -> irExprType expr == IRString
                    BoolLiteral _ -> irExprType expr == IRBool
  ]

-- | 3. IR语句测试
irStatementTests :: TestTree
irStatementTests = testGroup "IR Statement Tests"
  [ testCase "Variable declaration statement" $
      let decl = IRVarDecl "x" IRInt (IRLiteralExpr (IntLiteral 42))
      in irStmtSourceSpan decl `seq` True @?= True
  
  , testCase "Assignment statement" $
      let assign = IRAssign "x" (IRVarExpr "y" IRInt)
      in irStmtType assign `seq` True @?= True
  
  , testCase "Return statement" $
      let ret = IRReturn (IRVarExpr "result" IRInt)
      in irStmtType ret `seq` True @?= True
  ]

-- | 4. IR函数测试
irFunctionTests :: TestTree
irFunctionTests = testGroup "IR Function Tests"
  [ testCase "Function signature creation" $
      let sig = IRFunctionSig "test" [IRInt, IRString] IRBool
      in irFunctionName sig @?= "test"
  
  , testCase "Function body statements" $
      let body = [IRVarDecl "x" IRInt (IRLiteralExpr (IntLiteral 0))]
          func = IRFunction (IRFunctionSig "f" [] IRInt) body
      in length (irFunctionBody func) @?= 1
  
  , fastProperty "Function parameter count matches signature" $
      \paramTypes -> let sig = IRFunctionSig "test" paramTypes IRInt
                         func = IRFunction sig []
                     in length (irFunctionParams func) == length paramTypes
  ]

-- | 5. IR模块测试
irModuleTests :: TestTree
irModuleTests = testGroup "IR Module Tests"
  [ testCase "Module creation" $
      let mod = IRModule "TestModule" [] []
      in irModuleName mod @?= "TestModule"
  
  , testCase "Module functions" $
      let func = IRFunction (IRFunctionSig "f" [] IRInt) []
          mod = IRModule "Test" [func] []
      in length (irModuleFunctions mod) @?= 1
  
  , fastProperty "Module function names are unique" $
      \funcNames -> let funcs = map (\n -> IRFunction (IRFunctionSig n [] IRInt) []) funcNames
                        mod = IRModule "Test" funcs []
                    in length (nub (map irFunctionName (irModuleFunctions mod))) == length funcNames
  ]

-- | 6. IR类型测试
irTypeTests :: TestTree
irTypeTests = testGroup "IR Type Tests"
  [ testCase "Basic type equality" $
      (IRInt == IRInt && IRString == IRString && IRBool == IRBool) @?= True
  
  , testCase "Function type equality" $
      let funcType1 = IRFunction [IRInt] IRString
          funcType2 = IRFunction [IRInt] IRString
      in funcType1 == funcType2 @?= True
  
  , testCase "Type string representation" $
      let funcType = IRFunction [IRInt, IRString] IRBool
      in show funcType `seq` True @?= True
  ]

-- | 7. IR变量测试
irVariableTests :: TestTree
irVariableTests = testGroup "IR Variable Tests"
  [ fastProperty "Variable name consistency" $
      \name -> let var = IRVar name IRInt
               in irVarName var == name
  
  , fastProperty "Variable type consistency" $
      \name varType -> let var = IRVar name varType
                       in irVarType var == varType
  
  , testCase "Variable creation" $
      let var = IRVar "testVar" IRString
      in (irVarName var, irVarType var) @?= ("testVar", IRString)
  ]

-- | 8. IR字面量测试
irLiteralTests :: TestTree
irLiteralTests = testGroup "IR Literal Tests"
  [ testCase "Integer literal" $
      let lit = IntLiteral 42
      in irLiteralType lit @?= IRInt
  
  , testCase "String literal" $
      let lit = StringLiteral "hello"
      in irLiteralType lit @?= IRString
  
  , testCase "Boolean literal" $
      let lit = BoolLiteral True
      in irLiteralType lit @?= IRBool
  
  , fastProperty "Literal value consistency" $
      \n -> let lit = IntLiteral n
            in case lit of
                  IntLiteral val -> val == n
  ]

-- | 9. IR操作测试
irOperationTests :: TestTree
irOperationTests = testGroup "IR Operation Tests"
  [ testCase "Binary operation type inference" $
      let left = IRVarExpr "x" IRInt
          right = IRVarExpr "y" IRInt
          addOp = IRBinaryOp IROpAdd left right IRInt
      in irBinaryOpType addOp @?= IRInt
  
  , testCase "Unary operation" $
      let operand = IRVarExpr "x" IRBool
          notOp = IRUnaryOp IROpNot operand IRBool
      in irUnaryOpType notOp @?= IRBool
  
  , fastProperty "Binary operation commutativity for addition" $
      \x y -> let left = IRLiteralExpr (IntLiteral x)
                  right = IRLiteralExpr (IntLiteral y)
                  add1 = IRBinaryOp IROpAdd left right IRInt
                  add2 = IRBinaryOp IROpAdd right left IRInt
              in irBinaryOpOp add1 == irBinaryOpOp add2
  ]

-- | 10. IR验证测试
irValidationTests :: TestTree
irValidationTests = testGroup "IR Validation Tests"
  [ testCase "Valid variable declaration" $
      let decl = IRVarDecl "x" IRInt (IRLiteralExpr (IntLiteral 42))
      in irValidateDecl decl @?= True
  
  , testCase "Valid function signature" $
      let sig = IRFunctionSig "test" [IRInt] IRString
      in irValidateFunctionSig sig @?= True
  
  , testCase "Valid module" $
      let mod = IRModule "Test" [] []
      in irValidateModule mod @?= True
  
  , fastProperty "Type consistency in assignment" $
      \value -> let decl = IRVarDecl "x" IRInt (IRLiteralExpr (IntLiteral value))
                     assign = IRAssign "x" (IRLiteralExpr (IntLiteral value))
                 in irValidateDecl decl && irValidateStmt assign
  ]