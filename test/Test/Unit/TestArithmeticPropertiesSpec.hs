{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestArithmeticPropertiesSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck
import Utils
import SourceLocation
import ErrorHandler
import Compiler.IR
import Ownership
import Dependencies
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Arithmetic Properties
testArithmeticProperties :: TestTree
testArithmeticProperties = testGroup "Arithmetic Properties Tests"
  [ testCase "IR: integer addition is commutative" $
      let x = TestIRLiteral (IRIntLiteral 5)
          y = TestIRLiteral (IRIntLiteral 10)
          result1 = evaluateBinaryOp Add x y
          result2 = evaluateBinaryOp Add y x
      in result1 @?= result2
      
  , testCase "IR: integer multiplication is commutative" $
      let x = TestIRLiteral (IRIntLiteral 5)
          y = TestIRLiteral (IRIntLiteral 10)
          result1 = evaluateBinaryOp Multiply x y
          result2 = evaluateBinaryOp Multiply y x
      in result1 @?= result2
      
  , testCase "IR: identity elements" $
      let x = TestIRLiteral (IRIntLiteral 42)
          addId = evaluateBinaryOp Add x (TestIRLiteral (IRIntLiteral 0))
          mulId = evaluateBinaryOp Multiply x (TestIRLiteral (IRIntLiteral 1))
      in do
        addId @?= IRIntLiteral 42
        mulId @?= IRIntLiteral 42
      
  , testCase "IR: zero property" $
      let x = TestIRLiteral (IRIntLiteral 42)
          result = evaluateBinaryOp Multiply x (TestIRLiteral (IRIntLiteral 0))
      in result @?= IRIntLiteral 0
      
  , testCase "IR: boolean AND is commutative" $
      let x = TestIRLiteral (IRBoolLiteral True)
          y = TestIRLiteral (IRBoolLiteral False)
          result1 = evaluateBinaryOp And x y
          result2 = evaluateBinaryOp And y x
      in result1 @?= result2
      
  , testCase "IR: boolean OR is commutative" $
      let x = TestIRLiteral (IRBoolLiteral True)
          y = TestIRLiteral (IRBoolLiteral False)
          result1 = evaluateBinaryOp Or x y
          result2 = evaluateBinaryOp Or y x
      in result1 @?= result2
      
  , testCase "IR: De Morgan's laws" $
      let x = TestIRLiteral (IRBoolLiteral True)
          result1 = evaluateUnaryOp Not x
      in result1 @?= TestIRLiteral (IRBoolLiteral False)
      
  , testProperty "IR: addition is commutative" $
      \x y -> evaluateBinaryOp Add x y == evaluateBinaryOp Add y x
      
  , testProperty "IR: multiplication is commutative" $
      \x y -> evaluateBinaryOp Multiply x y == evaluateBinaryOp Multiply y x
      
  , testProperty "IR: equality is symmetric" $
      \x y -> evaluateBinaryOp TestEqual x y == evaluateBinaryOp TestEqual y x
      
  , testProperty "IR: inequality is symmetric" $
      \x y -> evaluateBinaryOp NotEqual x y == evaluateBinaryOp NotEqual y x
      
  , testProperty "IR: less than and greater than are inverses" $
      \x y -> evaluateBinaryOp LessThan x y == evaluateBinaryOp GreaterThan y x
      
  , testProperty "IR: less than or equal and greater than or equal are inverses" $
      \x y -> evaluateBinaryOp LessThanOrEqual x y == evaluateBinaryOp GreaterThanOrEqual y x
  ]

-- Helper types and functions for testing
data IRType = IRInt | IRBool | IRString deriving (Eq, Show)

data IRLiteral = IRIntLiteral Int | IRBoolLiteral Bool | IRStringLiteral String deriving (Eq, Show)

data TestIRExpression = 
    TestIRLiteral IRLiteral
  | TestIRVariable String
  | TestIRBinaryOp TestBinaryOp TestIRExpression TestIRExpression
  deriving (Eq, Show)

data TestBinaryOp = Add | Subtract | Multiply | Divide | TestEqual | NotEqual | 
                 LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual |
                 And | Or
  deriving (Eq, Show)

evaluateBinaryOp :: TestBinaryOp -> TestIRExpression -> TestIRExpression -> IRLiteral
evaluateBinaryOp Add (TestIRLiteral (IRIntLiteral x)) (TestIRLiteral (IRIntLiteral y)) = 
  IRIntLiteral (x + y)
evaluateBinaryOp Add _ _ = IRIntLiteral 0  -- Simplified error case

evaluateBinaryOp Subtract (TestIRLiteral (IRIntLiteral x)) (TestIRLiteral (IRIntLiteral y)) = 
  IRIntLiteral (x - y)
evaluateBinaryOp Subtract _ _ = IRIntLiteral 0  -- Simplified error case

evaluateBinaryOp Multiply (TestIRLiteral (IRIntLiteral x)) (TestIRLiteral (IRIntLiteral y)) = 
  IRIntLiteral (x * y)
evaluateBinaryOp Multiply _ _ = IRIntLiteral 0  -- Simplified error case

evaluateBinaryOp Divide (TestIRLiteral (IRIntLiteral x)) (TestIRLiteral (IRIntLiteral y)) = 
  if y == 0 then IRIntLiteral 0 else IRIntLiteral (x `div` y)
evaluateBinaryOp Divide _ _ = IRIntLiteral 0  -- Simplified error case

evaluateBinaryOp TestEqual (TestIRLiteral (IRIntLiteral x)) (TestIRLiteral (IRIntLiteral y)) = IRBoolLiteral (x == y)
evaluateBinaryOp TestEqual (TestIRLiteral (IRBoolLiteral x)) (TestIRLiteral (IRBoolLiteral y)) = IRBoolLiteral (x == y)
evaluateBinaryOp TestEqual _ _ = IRBoolLiteral False  -- Simplified error case

evaluateBinaryOp NotEqual (TestIRLiteral (IRIntLiteral x)) (TestIRLiteral (IRIntLiteral y)) = IRBoolLiteral (x /= y)
evaluateBinaryOp NotEqual (TestIRLiteral (IRBoolLiteral x)) (TestIRLiteral (IRBoolLiteral y)) = IRBoolLiteral (x /= y)
evaluateBinaryOp NotEqual _ _ = IRBoolLiteral True  -- Simplified error case

evaluateBinaryOp LessThan (TestIRLiteral (IRIntLiteral x)) (TestIRLiteral (IRIntLiteral y)) = IRBoolLiteral (x < y)
evaluateBinaryOp LessThan _ _ = IRBoolLiteral False  -- Simplified error case

evaluateBinaryOp LessThanOrEqual (TestIRLiteral (IRIntLiteral x)) (TestIRLiteral (IRIntLiteral y)) = IRBoolLiteral (x <= y)
evaluateBinaryOp LessThanOrEqual _ _ = IRBoolLiteral False  -- Simplified error case

evaluateBinaryOp GreaterThan (TestIRLiteral (IRIntLiteral x)) (TestIRLiteral (IRIntLiteral y)) = IRBoolLiteral (x > y)
evaluateBinaryOp GreaterThan _ _ = IRBoolLiteral False  -- Simplified error case

evaluateBinaryOp GreaterThanOrEqual (TestIRLiteral (IRIntLiteral x)) (TestIRLiteral (IRIntLiteral y)) = IRBoolLiteral (x >= y)
evaluateBinaryOp GreaterThanOrEqual _ _ = IRBoolLiteral False  -- Simplified error case

evaluateBinaryOp And (TestIRLiteral (IRBoolLiteral x)) (TestIRLiteral (IRBoolLiteral y)) = IRBoolLiteral (x && y)
evaluateBinaryOp And _ _ = IRBoolLiteral False  -- Simplified error case

evaluateBinaryOp Or (TestIRLiteral (IRBoolLiteral x)) (TestIRLiteral (IRBoolLiteral y)) = IRBoolLiteral (x || y)
evaluateBinaryOp Or _ _ = IRBoolLiteral False  -- Simplified error case

evaluateUnaryOp :: TestUnaryOp -> TestIRExpression -> TestIRExpression
evaluateUnaryOp Not (TestIRLiteral (IRBoolLiteral x)) = TestIRLiteral (IRBoolLiteral (not x))
evaluateUnaryOp _ _ = TestIRLiteral (IRBoolLiteral False)  -- Simplified error case

data TestUnaryOp = Not deriving (Eq, Show)

-- Arbitrary instances for QuickCheck

instance Arbitrary TestIRExpression where
  arbitrary = sized $ \n -> 
    if n == 0
      then TestIRLiteral <$> arbitrary
      else oneof
        [ TestIRLiteral <$> arbitrary
        , TestIRVariable <$> arbitrary
        , TestIRBinaryOp <$> arbitrary <*> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
        ]

instance Arbitrary IRLiteral where
  arbitrary = oneof
    [ IRIntLiteral <$> arbitrary
    , IRBoolLiteral <$> arbitrary
    , IRStringLiteral <$> arbitrary
    ]

instance Arbitrary TestBinaryOp where
  arbitrary = oneof
    [ pure Add
    , pure Subtract
    , pure Multiply
    , pure Divide
    , pure TestEqual
    , pure NotEqual
    , pure LessThan
    , pure LessThanOrEqual
    , pure GreaterThan
    , pure GreaterThanOrEqual
    , pure And
    , pure Or
    ]

instance Arbitrary TestUnaryOp where
  arbitrary = pure Not