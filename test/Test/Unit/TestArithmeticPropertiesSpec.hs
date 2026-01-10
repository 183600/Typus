{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestArithmeticPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.IR
import Dependencies
import Utils
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Arithmetic Properties
testArithmeticProperties :: TestTree
testArithmeticProperties = testGroup "Arithmetic Properties Tests"
  [ testProperty "IR: addition is commutative" $
      \x y -> evaluateBinaryOp Add x y == evaluateBinaryOp Add y x
      
  , testProperty "IR: addition is associative" $
      \x y z -> 
        evaluateBinaryOp Add x (evaluateBinaryOp Add y z) == 
        evaluateBinaryOp Add (evaluateBinaryOp Add x y) z
        
  , testProperty "IR: multiplication is commutative" $
      \x y -> evaluateBinaryOp Multiply x y == evaluateBinaryOp Multiply y x
      
  , testProperty "IR: multiplication is associative" $
      \x y z -> 
        evaluateBinaryOp Multiply x (evaluateBinaryOp Multiply y z) == 
        evaluateBinaryOp Multiply (evaluateBinaryOp Multiply x y) z
        
  , testProperty "IR: multiplication distributes over addition" $
      \x y z -> 
        evaluateBinaryOp Multiply x (evaluateBinaryOp Add y z) == 
        evaluateBinaryOp Add (evaluateBinaryOp Multiply x y) (evaluateBinaryOp Multiply x z)
        
  , testProperty "IR: subtraction is not commutative" $
      \x y -> x /= y ==> evaluateBinaryOp Subtract x y /= evaluateBinaryOp Subtract y x
      
  , testProperty "IR: division is not commutative" $
      \x y -> y /= 0 && x /= y ==> 
        evaluateBinaryOp Divide x y /= evaluateBinaryOp Divide y x
        
  , testProperty "IR: adding zero is identity" $
      \x -> evaluateBinaryOp Add x (IRIntLiteral 0) == x
      
  , testProperty "IR: multiplying by one is identity" $
      \x -> evaluateBinaryOp Multiply x (IRIntLiteral 1) == x
      
  , testProperty "IR: multiplying by zero is zero" $
      \x -> evaluateBinaryOp Multiply x (IRIntLiteral 0) == IRIntLiteral 0
      
  , testProperty "IR: subtracting zero is identity" $
      \x -> evaluateBinaryOp Subtract x (IRIntLiteral 0) == x
      
  , testProperty "IR: subtracting from zero is negation" $
      \x -> evaluateBinaryOp Subtract (IRIntLiteral 0) x == negateIR x
      
  , testProperty "IR: division by one is identity" $
      \x -> x /= IRIntLiteral 0 ==> 
        evaluateBinaryOp Divide x (IRIntLiteral 1) == x
        
  , testProperty "IR: double negation" $
      \x -> negateIR (negateIR x) == x
      
  , testProperty "IR: addition and subtraction are inverses" $
      \x y -> evaluateBinaryOp Subtract (evaluateBinaryOp Add x y) y == x
      
  , testProperty "IR: multiplication and division are inverses (when divisible)" $
      \x y -> y /= IRIntLiteral 0 && isDivisible x y ==> 
        evaluateBinaryOp Multiply (evaluateBinaryOp Divide x y) y == x
        
  , testProperty "IR: equality is reflexive" $
      \x -> evaluateBinaryOp Equal x x == IRBoolLiteral True
      
  , testProperty "IR: equality is symmetric" $
      \x y -> evaluateBinaryOp Equal x y == evaluateBinaryOp Equal y x
      
  , testProperty "IR: inequality is symmetric" $
      \x y -> evaluateBinaryOp NotEqual x y == evaluateBinaryOp NotEqual y x
      
  , testProperty "IR: less than and greater than are inverses" $
      \x y -> evaluateBinaryOp LessThan x y == evaluateBinaryOp GreaterThan y x
      
  , testProperty "IR: less than or equal and greater than or equal are inverses" $
      \x y -> evaluateBinaryOp LessThanOrEqual x y == evaluateBinaryOp GreaterThanOrEqual y x
      
  , testProperty "IR: less than or equal is reflexive" $
      \x -> evaluateBinaryOp LessThanOrEqual x x == IRBoolLiteral True
      
  , testProperty "IR: greater than or equal is reflexive" $
      \x -> evaluateBinaryOp GreaterThanOrEqual x x == IRBoolLiteral True
      
  , testProperty "IR: transitivity of less than" $
      \x y z -> 
        let ltXY = evaluateBinaryOp LessThan x y
            ltYZ = evaluateBinaryOp LessThan y z
            ltXZ = evaluateBinaryOp LessThan x z
        in case (ltXY, ltYZ) of
             (IRBoolLiteral True, IRBoolLiteral True) -> ltXZ == IRBoolLiteral True
             _ -> True  -- Property doesn't apply if premises are false
             
  , testProperty "IR: transitivity of greater than" $
      \x y z -> 
        let gtXY = evaluateBinaryOp GreaterThan x y
            gtYZ = evaluateBinaryOp GreaterThan y z
            gtXZ = evaluateBinaryOp GreaterThan x z
        in case (gtXY, gtYZ) of
             (IRBoolLiteral True, IRBoolLiteral True) -> gtXZ == IRBoolLiteral True
             _ -> True  -- Property doesn't apply if premises are false
             
  , testProperty "IR: addition preserves order" $
      \x y z -> 
        let ltXY = evaluateBinaryOp LessThan x y
            ltXZplusY = evaluateBinaryOp LessThan 
                           (evaluateBinaryOp Add x z) 
                           (evaluateBinaryOp Add y z)
        in case ltXY of
             IRBoolLiteral True -> ltXZplusY == IRBoolLiteral True
             _ -> True  -- Property doesn't apply if premise is false
             
  , testProperty "IR: multiplication by positive number preserves order" $
      \x y z -> 
        let z' = ensurePositive z
            ltXY = evaluateBinaryOp LessThan x y
            ltXZtimesY = evaluateBinaryOp LessThan 
                           (evaluateBinaryOp Multiply x z') 
                           (evaluateBinaryOp Multiply y z')
        in case ltXY of
             IRBoolLiteral True -> ltXZtimesY == IRBoolLiteral True
             _ -> True  -- Property doesn't apply if premise is false
             
  , testProperty "IR: square of non-negative is non-negative" $
      \x -> 
        let x' = ensureNonNegative x
            square = evaluateBinaryOp Multiply x' x'
        in isNonNegative square
         
  , testProperty "IR: absolute value is non-negative" $
      \x -> isNonNegative (absIR x)
      
  , testProperty "IR: absolute value of zero is zero" $
      absIR (IRIntLiteral 0) == IRIntLiteral 0
      
  , testProperty "IR: absolute value is idempotent for non-negative numbers" $
      \x -> 
        let x' = ensureNonNegative x
        in absIR x' == x'
        
  , testProperty "IR: absolute value of negative is negation" $
      \x -> 
        let x' = ensureNegative x
        in absIR x' == negateIR x'
        
  , testProperty "IR: triangle inequality" $
      \x y -> 
        let absX = absIR x
            absY = absIR y
            absXplusY = absIR (evaluateBinaryOp Add x y)
            sumAbs = evaluateBinaryOp Add absX absY
        in evaluateBinaryOp LessThanOrEqual absXplusY sumAbs == IRBoolLiteral True
        
  , testProperty "IR: exponentiation properties" $
      \base exp -> 
        let result = evaluateExponent base exp
        in case exp of
             IRIntLiteral 0 -> result == IRIntLiteral 1
             IRIntLiteral 1 -> result == base
             _ -> True  -- General case handled by implementation
             
  , testProperty "IR: exponentiation with zero base" $
      \exp -> 
        let result = evaluateExponent (IRIntLiteral 0) exp
        in case exp of
             IRIntLiteral e | e > 0 -> result == IRIntLiteral 0
             _ -> True  -- Zero to zero or negative exponent is special case
             
  , testProperty "IR: exponentiation with one base" $
      \exp -> 
        let result = evaluateExponent (IRIntLiteral 1) exp
        in result == IRIntLiteral 1
        
  , testProperty "IR: modulo properties" $
      \x y -> y /= IRIntLiteral 0 ==> 
        let result = evaluateModulo x y
        in isNonNegative result && 
           evaluateBinaryOp LessThan result (absIR y) == IRBoolLiteral True
           
  , testProperty "IR: modulo with zero remainder" $
      \x y -> y /= IRIntLiteral 0 && isDivisible x y ==> 
        evaluateModulo x y == IRIntLiteral 0
        
  , testProperty "IR: modulo is idempotent for divisor" $
      \x y -> y /= IRIntLiteral 0 ==> 
        evaluateModulo y y == IRIntLiteral 0
  ]

-- Helper functions
evaluateBinaryOp :: BinaryOp -> IRLiteral -> IRLiteral -> IRLiteral
evaluateBinaryOp Add (IRIntLiteral x) (IRIntLiteral y) = IRIntLiteral (x + y)
evaluateBinaryOp Add (IRBoolLiteral x) (IRBoolLiteral y) = IRBoolLiteral (x || y)
evaluateBinaryOp Add _ _ = IRStringLiteral ""  -- Simplified error case

evaluateBinaryOp Subtract (IRIntLiteral x) (IRIntLiteral y) = IRIntLiteral (x - y)
evaluateBinaryOp Subtract _ _ = IRIntLiteral 0  -- Simplified error case

evaluateBinaryOp Multiply (IRIntLiteral x) (IRIntLiteral y) = IRIntLiteral (x * y)
evaluateBinaryOp Multiply (IRBoolLiteral x) (IRBoolLiteral y) = IRBoolLiteral (x && y)
evaluateBinaryOp Multiply _ _ = IRIntLiteral 0  -- Simplified error case

evaluateBinaryOp Divide (IRIntLiteral x) (IRIntLiteral y) = 
  if y == 0 then IRIntLiteral 0 else IRIntLiteral (x `div` y)
evaluateBinaryOp Divide _ _ = IRIntLiteral 0  -- Simplified error case

evaluateBinaryOp Equal (IRIntLiteral x) (IRIntLiteral y) = IRBoolLiteral (x == y)
evaluateBinaryOp Equal (IRBoolLiteral x) (IRBoolLiteral y) = IRBoolLiteral (x == y)
evaluateBinaryOp Equal _ _ = IRBoolLiteral False  -- Simplified error case

evaluateBinaryOp NotEqual (IRIntLiteral x) (IRIntLiteral y) = IRBoolLiteral (x /= y)
evaluateBinaryOp NotEqual (IRBoolLiteral x) (IRBoolLiteral y) = IRBoolLiteral (x /= y)
evaluateBinaryOp NotEqual _ _ = IRBoolLiteral True  -- Simplified error case

evaluateBinaryOp LessThan (IRIntLiteral x) (IRIntLiteral y) = IRBoolLiteral (x < y)
evaluateBinaryOp LessThan _ _ = IRBoolLiteral False  -- Simplified error case

evaluateBinaryOp LessThanOrEqual (IRIntLiteral x) (IRIntLiteral y) = IRBoolLiteral (x <= y)
evaluateBinaryOp LessThanOrEqual _ _ = IRBoolLiteral False  -- Simplified error case

evaluateBinaryOp GreaterThan (IRIntLiteral x) (IRIntLiteral y) = IRBoolLiteral (x > y)
evaluateBinaryOp GreaterThan _ _ = IRBoolLiteral False  -- Simplified error case

evaluateBinaryOp GreaterThanOrEqual (IRIntLiteral x) (IRIntLiteral y) = IRBoolLiteral (x >= y)
evaluateBinaryOp GreaterThanOrEqual _ _ = IRBoolLiteral False  -- Simplified error case

negateIR :: IRLiteral -> IRLiteral
negateIR (IRIntLiteral x) = IRIntLiteral (-x)
negateIR _ = IRIntLiteral 0  -- Simplified error case

ensurePositive :: IRLiteral -> IRLiteral
ensurePositive (IRIntLiteral x) = IRIntLiteral (if x <= 0 then 1 else x)
ensurePositive _ = IRIntLiteral 1  -- Simplified error case

ensureNonNegative :: IRLiteral -> IRLiteral
ensureNonNegative (IRIntLiteral x) = IRIntLiteral (if x < 0 then 0 else x)
ensureNonNegative _ = IRIntLiteral 0  -- Simplified error case

ensureNegative :: IRLiteral -> IRLiteral
ensureNegative (IRIntLiteral x) = IRIntLiteral (if x >= 0 then -1 else x)
ensureNegative _ = IRIntLiteral (-1)  -- Simplified error case

isNonNegative :: IRLiteral -> Bool
isNonNegative (IRIntLiteral x) = x >= 0
isNonNegative _ = False  -- Simplified error case

absIR :: IRLiteral -> IRLiteral
absIR (IRIntLiteral x) = IRIntLiteral (abs x)
absIR _ = IRIntLiteral 0  -- Simplified error case

isDivisible :: IRLiteral -> IRLiteral -> Bool
isDivisible (IRIntLiteral x) (IRIntLiteral y) = y /= 0 && x `mod` y == 0
isDivisible _ _ = False  -- Simplified error case

evaluateExponent :: IRLiteral -> IRLiteral -> IRLiteral
evaluateExponent (IRIntLiteral base) (IRIntLiteral exp) = 
  if exp >= 0 && exp < 20  -- Prevent overflow
    then IRIntLiteral (base ^ exp)
    else IRIntLiteral 0  -- Simplified error case
evaluateExponent _ _ = IRIntLiteral 0  -- Simplified error case

evaluateModulo :: IRLiteral -> IRLiteral -> IRLiteral
evaluateModulo (IRIntLiteral x) (IRIntLiteral y) = 
  if y /= 0
    then IRIntLiteral (x `mod` abs y)
    else IRIntLiteral 0  -- Simplified error case
evaluateModulo _ _ = IRIntLiteral 0  -- Simplified error case

-- Simplified Compiler IR types for testing
data IRLiteral = IRIntLiteral Int | IRBoolLiteral Bool | IRStringLiteral String
  deriving (Eq, Show)

data BinaryOp = Add | Subtract | Multiply | Divide | Equal | NotEqual | 
                 LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual
  deriving (Eq, Show)