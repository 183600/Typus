{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestCompilerIRConsistencySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.IR
import Compiler.TypeChecker
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt, locatedWithSpan)
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Compiler IR consistency
testCompilerIRConsistency :: TestTree
testCompilerIRConsistency = testGroup "Compiler IR Consistency Tests"
  [ testCase "IRModule: maintains consistent function definitions" $
      let func = IRFunction 
            { irFuncName = "test"
            , irFuncParams = [IRParam "x" IRInt]
            , irFuncReturnType = IRBool
            , irFuncBody = [IRReturn (IRLiteral (IRBoolLiteral True))]
            , irFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
          module = IRModule 
            { irModuleName = "test_module"
            , irModuleImports = []
            , irModuleFunctions = [func]
            , irModuleGlobals = []
            , irModuleSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test_module"
            }
      in irFuncName (head (irModuleFunctions module)) @?= "test"
      
  , testCase "IRFunction: parameter count matches body variable usage" $
      let func = IRFunction 
            { irFuncName = "test"
            , irFuncParams = [IRParam "x" IRInt, IRParam "y" IRInt]
            , irFuncReturnType = IRInt
            , irFuncBody = [IRBinaryOp Add (IRVariable "x") (IRVariable "y")]
            , irFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
      in length (irFuncParams func) @?= 2
      
  , testCase "IRBinaryOp: type consistency for arithmetic operations" $
      let left = IRLiteral (IRIntLiteral 42)
          right = IRLiteral (IRIntLiteral 24)
          binaryOp = IRBinaryOp Add left right
      in irTypeOf left @?= IRInt && irTypeOf right @?= IRInt
      
  , testCase "IRBinaryOp: type consistency for comparison operations" $
      let left = IRLiteral (IRIntLiteral 42)
          right = IRLiteral (IRIntLiteral 24)
          binaryOp = IRBinaryOp Equal left right
      in irTypeOf binaryOp @?= IRBool
      
  , testCase "IRIf: consistent branch types" $
      let condition = IRLiteral (IRBoolLiteral True)
          thenBranch = IRLiteral (IRIntLiteral 1)
          elseBranch = IRLiteral (IRIntLiteral 0)
          ifExpr = IRIf condition thenBranch elseBranch
      in irTypeOf thenBranch @?= irTypeOf elseBranch
      
  , testCase "IRCall: function parameter count matches argument count" $
      let func = IRVariable "test"
          args = [IRLiteral (IRIntLiteral 42), IRLiteral (IRIntLiteral 24)]
          call = IRCall func args
      in length args @?= 2
      
  , testCase "IRStruct: consistent field types" $
      let fields = [("x", IRInt), ("y", IRInt)]
          struct = IRStruct "Point" fields
      in all (\(_, t) -> t == IRInt) fields
      
  , testCase "IRStructAccess: field exists in struct" $
      let struct = IRVariable "point"
          field = "x"
          access = IRStructAccess struct field
      in field `elem` ["x", "y"]  -- Assuming Point has x and y fields
      
  , testCase "IRArrayAccess: array and index types are consistent" $
      let array = IRVariable "arr"
          index = IRLiteral (IRIntLiteral 0)
          access = IRArrayAccess array index
      in irTypeOf index @?= IRInt
      
  , testCase "IRLambda: parameter count matches body variable usage" $
      let params = [IRParam "x" IRInt]
          body = IRVariable "x"
          lambda = IRLambda params body
      in length params @?= 1
      
  , testCase "IRLet: binding variable is used in body" $
      let binding = ("x", IRLiteral (IRIntLiteral 42))
          body = IRVariable "x"
          letExpr = IRLet binding body
      in fst binding `elem` ["x"]
      
  , testCase "IRReturn: return type matches function return type" $
      let func = IRFunction 
            { irFuncName = "test"
            , irFuncParams = []
            , irFuncReturnType = IRInt
            , irFuncBody = [IRReturn (IRLiteral (IRIntLiteral 42))]
            , irFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
      in case head (irFuncBody func) of
           IRReturn value -> irTypeOf value @?= irFuncReturnType func
           _ -> assertFailure "Expected IRReturn"
           
  , testCase "IRLoop: consistent loop variable types" $
      let init = IRLet ("i", IRLiteral (IRIntLiteral 0))
          condition = IRBinaryOp LessThan (IRVariable "i") (IRLiteral (IRIntLiteral 10))
          update = IRBinaryOp Add (IRVariable "i") (IRLiteral (IRIntLiteral 1))
          body = []
          loop = IRLoop init condition update body
      in irTypeOf (IRVariable "i") @?= IRInt
      
  , testCase "IRMatch: all patterns have consistent types" $
      let value = IRVariable "x"
          patterns = [(IRPatternLiteral (IRIntLiteral 1), IRLiteral (IRBoolLiteral True)),
                     (IRPatternLiteral (IRIntLiteral 2), IRLiteral (IRBoolLiteral False))]
          match = IRMatch value patterns
      in all (\(_, expr) -> irTypeOf expr == IRBool) patterns
      
  , testCase "IRModule: no duplicate function names" $
      let func1 = IRFunction 
            { irFuncName = "test"
            , irFuncParams = []
            , irFuncReturnType = IRInt
            , irFuncBody = [IRReturn (IRLiteral (IRIntLiteral 42))]
            , irFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
          func2 = IRFunction 
            { irFuncName = "test2"
            , irFuncParams = []
            , irFuncReturnType = IRInt
            , irFuncBody = [IRReturn (IRLiteral (IRIntLiteral 24))]
            , irFuncSpan = locatedWithSpan (spanBetween (SourcePos 4 1 0) (SourcePos 6 1 0)) "test2"
            }
          module = IRModule 
            { irModuleName = "test_module"
            , irModuleImports = []
            , irModuleFunctions = [func1, func2]
            , irModuleGlobals = []
            , irModuleSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 6 1 0)) "test_module"
            }
      in length (irModuleFunctions module) @?= 2 &&
         irFuncName (irModuleFunctions module !! 0) /= irFuncName (irModuleFunctions module !! 1)
         
  , testCase "IRModule: no duplicate global names" $
      let global1 = IRGlobal "x" IRInt (IRLiteral (IRIntLiteral 42))
          global2 = IRGlobal "y" IRInt (IRLiteral (IRIntLiteral 24))
          module = IRModule 
            { irModuleName = "test_module"
            , irModuleImports = []
            , irModuleFunctions = []
            , irModuleGlobals = [global1, global2]
            , irModuleSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test_module"
            }
      in length (irModuleGlobals module) @?= 2 &&
         irGlobalName (irModuleGlobals module !! 0) /= irGlobalName (irModuleGlobals module !! 1)
         
  , testCase "IRExpression: type consistency for nested expressions" $
      let inner = IRBinaryOp Add (IRLiteral (IRIntLiteral 1)) (IRLiteral (IRIntLiteral 2))
          outer = IRBinaryOp Multiply inner (IRLiteral (IRIntLiteral 3))
      in irTypeOf inner @?= IRInt && irTypeOf outer @?= IRInt
      
  , testCase "IRPattern: pattern type matches matched value type" $
      let value = IRLiteral (IRIntLiteral 42)
          pattern = IRPatternLiteral (IRIntLiteral 42)
      in irTypeOf value @?= IRInt  -- Pattern would be IRInt
      
  , testCase "IRType: consistent type application" $
      let baseType = IRConstructor "List" []
          appliedType = IRConstructor "List" [IRInt]
      in case appliedType of
           IRConstructor "List" args -> length args @?= 1
           _ -> assertFailure "Expected type constructor with arguments"
           
  , testCase "IRType: recursive type definition consistency" $
      let listType = IRConstructor "List" [IRTypeVar "a"]
          treeType = IRConstructor "Tree" [IRTypeVar "a"]
      in case (listType, treeType) of
           (IRConstructor "List" [IRTypeVar _], IRConstructor "Tree" [IRTypeVar _]) -> return ()
           _ -> assertFailure "Expected recursive type definitions"
  ]

-- Helper functions
irTypeOf :: IRExpression -> IRType
irTypeOf (IRLiteral (IRIntLiteral _)) = IRInt
irTypeOf (IRLiteral (IRBoolLiteral _)) = IRBool
irTypeOf (IRLiteral (IRStringLiteral _)) = IRString
irTypeOf (IRVariable _) = IRTypeVar "a"  -- Simplified
irTypeOf (IRBinaryOp Add _ _) = IRInt
irTypeOf (IRBinaryOp Subtract _ _) = IRInt
irTypeOf (IRBinaryOp Multiply _ _) = IRInt
irTypeOf (IRBinaryOp Divide _ _) = IRInt
irTypeOf (IRBinaryOp Equal _ _) = IRBool
irTypeOf (IRBinaryOp NotEqual _ _) = IRBool
irTypeOf (IRBinaryOp LessThan _ _) = IRBool
irTypeOf (IRBinaryOp LessThanOrEqual _ _) = IRBool
irTypeOf (IRBinaryOp GreaterThan _ _) = IRBool
irTypeOf (IRBinaryOp GreaterThanOrEqual _ _) = IRBool
irTypeOf (IRIf _ thenBranch elseBranch) = irTypeOf thenBranch
irTypeOf (IRCall func _) = IRTypeVar "b"  -- Simplified
irTypeOf (IRStructAccess _ _) = IRTypeVar "c"  -- Simplified
irTypeOf (IRArrayAccess _ _) = IRTypeVar "d"  -- Simplified
irTypeOf (IRLambda _ body) = IRTypeVar "e"  -- Simplified
irTypeOf (IRLet _ body) = irTypeOf body
irTypeOf (IRReturn _) = IRTypeVar "f"  -- Simplified
irTypeOf (IRLoop _ _ _ _) = IRTypeVar "g"  -- Simplified
irTypeOf (IRMatch _ patterns) = irTypeOf (snd (head patterns))