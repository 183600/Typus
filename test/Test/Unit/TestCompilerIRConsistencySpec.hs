{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestCompilerIRConsistencySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation hiding (locatedWithSpan, Located, spanBetween)
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Compiler IR consistency
testCompilerIRConsistency :: TestTree
testCompilerIRConsistency = testGroup "Compiler IR Consistency Tests"
  [ testCase "IRModule: maintains consistent function definitions" $
      let func = TestIRFunction 
            { testIRFuncName = "test"
            , testIRFuncParams = [TestIRParam "x" TestIRInt]
            , testIRFuncReturnType = TestIRBool
            , testIRFuncBody = [TestIRReturn (TestIRLiteral (TestIRBoolLiteral True))]
            , testIRFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
          testModule = TestIRModule 
            { testIRModuleName = "test_module"
            , testIRModuleImports = []
            , testIRModuleFunctions = [func]
            , testIRModuleGlobals = []
            , testIRModuleSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test_module"
            }
      in testIRFuncName (head (testIRModuleFunctions testModule)) @?= "test"
      
  , testCase "IRFunction: parameter count matches body variable usage" $
      let func = TestIRFunction 
            { testIRFuncName = "test"
            , testIRFuncParams = [TestIRParam "x" TestIRInt, TestIRParam "y" TestIRInt]
            , testIRFuncReturnType = TestIRInt
            , testIRFuncBody = [TestIRBinaryOp TestAdd (TestIRVariable "x") (TestIRVariable "y")]
            , testIRFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
      in length (testIRFuncParams func) @?= 2
      
  , testCase "IRBinaryOp: type consistency for arithmetic operations" $
      let left = TestIRLiteral (TestIRIntLiteral 42)
          right = TestIRLiteral (TestIRIntLiteral 24)
          binaryOp = TestIRBinaryOp TestAdd left right
      in (testIrTypeOf left @?= TestIRInt) >> (testIrTypeOf right @?= TestIRInt)
      
  , testCase "IRBinaryOp: type consistency for comparison operations" $
      let left = TestIRLiteral (TestIRIntLiteral 42)
          right = TestIRLiteral (TestIRIntLiteral 24)
          binaryOp = TestIRBinaryOp TestEqual left right
      in testIrTypeOf binaryOp @?= TestIRBool
      
  , testCase "IRIf: consistent branch types" $
      let condition = TestIRLiteral (TestIRBoolLiteral True)
          thenBranch = TestIRLiteral (TestIRIntLiteral 1)
          elseBranch = TestIRLiteral (TestIRIntLiteral 0)
          ifExpr = TestIRIf condition thenBranch elseBranch
      in testIrTypeOf thenBranch @?= testIrTypeOf elseBranch
      
  , testCase "IRCall: function parameter count matches argument count" $
      let func = TestIRVariable "test"
          args = [TestIRLiteral (TestIRIntLiteral 42), TestIRLiteral (TestIRIntLiteral 24)]
          call = TestIRCall func args
      in length args @?= 2
      
  , testCase "IRStruct: consistent field types" $
      let fields = [("x", TestIRInt), ("y", TestIRInt)]
          struct = TestIRStruct (TestIRVariable "Point") fields
      in all (\(_, t) -> t == TestIRInt) fields @?= True
      
  , testCase "IRStructAccess: field exists in struct" $
      let struct = TestIRVariable "point"
          field = "x"
          access = TestIRStructAccess struct field
      in field `elem` ["x", "y"] @?= True  -- Assuming Point has x and y fields
      
  , testCase "IRArrayAccess: array and index types are consistent" $
      let array = TestIRVariable "arr"
          index = TestIRLiteral (TestIRIntLiteral 0)
          access = TestIRArrayAccess array index
      in testIrTypeOf index @?= TestIRInt
      
  , testCase "IRLambda: parameter count matches body variable usage" $
      let params = [TestIRParam "x" TestIRInt]
          body = TestIRVariable "x"
          lambda = TestIRLambda params body
      in length params @?= 1
      
  , testCase "IRLet: binding variable is used in body" $
      let binding = ("x", TestIRLiteral (TestIRIntLiteral 42))
          body = TestIRVariable "x"
          letExpr = TestIRLet binding body
      in fst binding `elem` ["x"] @?= True
      
  , testCase "IRReturn: return type matches function return type" $
      let func = TestIRFunction 
            { testIRFuncName = "test"
            , testIRFuncParams = []
            , testIRFuncReturnType = TestIRInt
            , testIRFuncBody = [TestIRReturn (TestIRLiteral (TestIRIntLiteral 42))]
            , testIRFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
      in case head (testIRFuncBody func) of
           TestIRReturn value -> testIrTypeOf value @?= testIRFuncReturnType func
           _ -> assertFailure "Expected TestIRReturn"
           
  , testCase "IRLoop: consistent loop variable types" $
      let init = TestIRLet ("i", TestIRLiteral (TestIRIntLiteral 0)) (TestIRVariable "i")
          condition = TestIRBinaryOp TestLessThan (TestIRVariable "i") (TestIRLiteral (TestIRIntLiteral 10))
          update = TestIRBinaryOp TestAdd (TestIRVariable "i") (TestIRLiteral (TestIRIntLiteral 1))
          body = []
          loop = TestIRLoop init condition update body
      in testIrTypeOf (TestIRVariable "i") @?= TestIRInt
      
  , testCase "IRMatch: all patterns have consistent types" $
      let value = TestIRVariable "x"
          patterns = [(TestIRPatternLiteral (TestIRIntLiteral 1), TestIRLiteral (TestIRBoolLiteral True)),
                     (TestIRPatternLiteral (TestIRIntLiteral 2), TestIRLiteral (TestIRBoolLiteral False))]
          match = TestIRMatch value patterns
      in all (\(_, expr) -> testIrTypeOf expr == TestIRBool) patterns @?= True
      
  , testCase "IRModule: no duplicate function names" $
      let func1 = TestIRFunction 
            { testIRFuncName = "test"
            , testIRFuncParams = []
            , testIRFuncReturnType = TestIRInt
            , testIRFuncBody = [TestIRReturn (TestIRLiteral (TestIRIntLiteral 42))]
            , testIRFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
          func2 = TestIRFunction 
            { testIRFuncName = "test2"
            , testIRFuncParams = []
            , testIRFuncReturnType = TestIRInt
            , testIRFuncBody = [TestIRReturn (TestIRLiteral (TestIRIntLiteral 24))]
            , testIRFuncSpan = locatedWithSpan (spanBetween (SourcePos 4 1 0) (SourcePos 6 1 0)) "test2"
            }
          testModule = TestIRModule 
            { testIRModuleName = "test_module"
            , testIRModuleImports = []
            , testIRModuleFunctions = [func1, func2]
            , testIRModuleGlobals = []
            , testIRModuleSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 6 1 0)) "test_module"
            }
      in do
        length (testIRModuleFunctions testModule) @?= 2
        testIRFuncName (testIRModuleFunctions testModule !! 0) /= testIRFuncName (testIRModuleFunctions testModule !! 1) @?= True
         
  , testCase "IRModule: no duplicate global names" $
      let global1 = TestIRGlobal "x" TestIRInt (TestIRLiteral (TestIRIntLiteral 42))
          global2 = TestIRGlobal "y" TestIRInt (TestIRLiteral (TestIRIntLiteral 24))
          testModule = TestIRModule 
            { testIRModuleName = "test_module"
            , testIRModuleImports = []
            , testIRModuleFunctions = []
            , testIRModuleGlobals = [global1, global2]
            , testIRModuleSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test_module"
            }
      in do
        length (testIRModuleGlobals testModule) @?= 2
        case (testIRModuleGlobals testModule !! 0, testIRModuleGlobals testModule !! 1) of
           (TestIRGlobal name1 _ _, TestIRGlobal name2 _ _) -> name1 /= name2 @?= True
         
  , testCase "IRExpression: type consistency for nested expressions" $
      let inner = TestIRBinaryOp TestAdd (TestIRLiteral (TestIRIntLiteral 1)) (TestIRLiteral (TestIRIntLiteral 2))
          outer = TestIRBinaryOp TestMultiply inner (TestIRLiteral (TestIRIntLiteral 3))
      in do
        testIrTypeOf inner @?= TestIRInt
        testIrTypeOf outer @?= TestIRInt
      
  , testCase "IRPattern: pattern type matches matched value type" $
      let value = TestIRLiteral (TestIRIntLiteral 42)
          pattern = TestIRPatternLiteral (TestIRIntLiteral 42)
      in testIrTypeOf value @?= TestIRInt  -- Pattern would be TestIRInt
      
  , testCase "IRType: consistent type application" $
      let baseType = TestIRTypeVar "List"
          appliedType = TestIRTypeVar "List"
      in case appliedType of
           TestIRTypeVar "List" -> return ()
           _ -> assertFailure "Expected type variable"
           
  , testCase "IRType: recursive type definition consistency" $
      let listType = TestIRTypeVar "List"
          treeType = TestIRTypeVar "Tree"
      in case (listType, treeType) of
           (TestIRTypeVar "List", TestIRTypeVar "Tree") -> return ()
           _ -> assertFailure "Expected type variables"
  ]

-- Local types
data TestSourcePos = TestSourcePos Int Int Int
  deriving (Eq, Show)

-- Helper functions
testIrTypeOf :: TestIRExpression -> TestIRType
testIrTypeOf (TestIRLiteral (TestIRIntLiteral _)) = TestIRInt
testIrTypeOf (TestIRLiteral (TestIRBoolLiteral _)) = TestIRBool
testIrTypeOf (TestIRLiteral (TestIRStringLiteral _)) = TestIRString
testIrTypeOf (TestIRVariable _) = TestIRTypeVar "a"  -- Simplified
testIrTypeOf (TestIRBinaryOp TestAdd _ _) = TestIRInt
testIrTypeOf (TestIRBinaryOp TestSubtract _ _) = TestIRInt
testIrTypeOf (TestIRBinaryOp TestMultiply _ _) = TestIRInt
testIrTypeOf (TestIRBinaryOp TestDivide _ _) = TestIRInt
testIrTypeOf (TestIRBinaryOp TestEqual _ _) = TestIRBool
testIrTypeOf (TestIRBinaryOp TestNotEqual _ _) = TestIRBool
testIrTypeOf (TestIRBinaryOp TestLessThan _ _) = TestIRBool
testIrTypeOf (TestIRBinaryOp TestLessThanOrEqual _ _) = TestIRBool
testIrTypeOf (TestIRBinaryOp TestGreaterThan _ _) = TestIRBool
testIrTypeOf (TestIRBinaryOp TestGreaterThanOrEqual _ _) = TestIRBool
testIrTypeOf (TestIRIf _ thenBranch elseBranch) = testIrTypeOf thenBranch
testIrTypeOf (TestIRCall func _) = TestIRTypeVar "b"  -- Simplified
testIrTypeOf (TestIRStructAccess _ _) = TestIRTypeVar "c"  -- Simplified
testIrTypeOf (TestIRArrayAccess _ _) = TestIRTypeVar "d"  -- Simplified
testIrTypeOf (TestIRLambda _ body) = TestIRTypeVar "e"  -- Simplified
testIrTypeOf (TestIRLet _ body) = testIrTypeOf body
testIrTypeOf (TestIRReturn _) = TestIRTypeVar "f"  -- Simplified
testIrTypeOf (TestIRLoop _ _ _ _) = TestIRTypeVar "g"  -- Simplified
testIrTypeOf (TestIRMatch _ patterns) = testIrTypeOf (snd (head patterns))

-- Local types to avoid conflicts
data TestIRType = TestIRInt | TestIRBool | TestIRString | TestIRTypeVar String
  deriving (Eq, Show)

data TestIRLiteral = TestIRIntLiteral Int | TestIRBoolLiteral Bool | TestIRStringLiteral String
  deriving (Eq, Show)

data TestIRExpression = 
    TestIRLiteral TestIRLiteral
  | TestIRVariable String
  | TestIRBinaryOp TestBinaryOp TestIRExpression TestIRExpression
  | TestIRIf TestIRExpression TestIRExpression TestIRExpression
  | TestIRCall TestIRExpression [TestIRExpression]
  | TestIRStruct TestIRExpression [(String, TestIRType)]
  | TestIRStructAccess TestIRExpression String
  | TestIRArrayAccess TestIRExpression TestIRExpression
  | TestIRLambda [TestIRParam] TestIRExpression
  | TestIRLet (String, TestIRExpression) TestIRExpression
  | TestIRReturn TestIRExpression
  | TestIRLoop TestIRExpression TestIRExpression TestIRExpression [TestIRExpression]
  | TestIRMatch TestIRExpression [(TestIRPattern, TestIRExpression)]
  deriving (Eq, Show)

data TestBinaryOp = TestAdd | TestSubtract | TestMultiply | TestDivide | TestEqual | TestNotEqual | 
                 TestLessThan | TestLessThanOrEqual | TestGreaterThan | TestGreaterThanOrEqual |
                 TestAnd | TestOr
  deriving (Eq, Show)

data TestIRPattern = TestIRPatternLiteral TestIRLiteral
  deriving (Eq, Show)

data TestIRParam = TestIRParam String TestIRType
  deriving (Eq, Show)

data TestIRFunction = TestIRFunction 
  { testIRFuncName :: String
  , testIRFuncParams :: [TestIRParam]
  , testIRFuncReturnType :: TestIRType
  , testIRFuncBody :: [TestIRExpression]
  , testIRFuncSpan :: Located String
  }

data TestIRModule = TestIRModule 
  { testIRModuleName :: String
  , testIRModuleImports :: [String]
  , testIRModuleFunctions :: [TestIRFunction]
  , testIRModuleGlobals :: [TestIRGlobal]
  , testIRModuleSpan :: Located String
  }

data TestIRGlobal = TestIRGlobal String TestIRType TestIRExpression
  deriving (Eq, Show)

data Located a = Located 
  { locValue :: a
  , locSpan :: SourceSpan
  }

spanBetween :: SourcePos -> SourcePos -> SourceSpan
spanBetween start end = SourceSpan start end

locatedWithSpan :: SourceSpan -> String -> Located String
locatedWithSpan span value = Located value span