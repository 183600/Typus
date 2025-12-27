{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.CompilerIRConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import Test.Tasty.HUnit (testCase, assertBool)

-- Mock IR (Intermediate Representation) types for testing
data MockIRType = MockIRInt | MockIRBool | MockIRString | MockIRFunction MockIRType MockIRType
  deriving (Show, Eq)

data MockIRInstruction = 
    MockIRLoad String MockIRType
  | MockIRStore String MockIRType
  | MockIRAdd String String String
  | MockIRSub String String String
  | MockIRCall String String [String]
  deriving (Show, Eq)

data MockIRProgram = MockIRProgram [MockIRInstruction]
  deriving (Show, Eq)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

instance Arbitrary MockIRType where
  arbitrary = oneof
    [ pure MockIRInt
    , pure MockIRBool
    , pure MockIRString
    , MockIRFunction <$> arbitrary <*> arbitrary
    ]

instance Arbitrary MockIRInstruction where
  arbitrary = oneof
    [ MockIRLoad <$> arbitrary <*> arbitrary
    , MockIRStore <$> arbitrary <*> arbitrary
    , MockIRAdd <$> arbitrary <*> arbitrary <*> arbitrary
    , MockIRSub <$> arbitrary <*> arbitrary <*> arbitrary
    , MockIRCall <$> arbitrary <*> arbitrary <*> listOf arbitrary
    ]

instance Arbitrary MockIRProgram where
  arbitrary = MockIRProgram <$> listOf arbitrary

-- Generate variable names
genVarName :: Gen String
genVarName = do
  prefix <- elements ["var", "temp", "result", "arg", "x", "y", "z"]
  suffix <- arbitrary `suchThat` (\n -> n >= 0 && n < 100)
  return $ prefix ++ show suffix

-- ============================================================================
-- Mock Functions (simplified versions for testing)
-- ============================================================================

mockTypeCheck :: MockIRProgram -> Either String MockIRProgram
mockTypeCheck program = 
  -- Simplified type checking - just check for basic consistency
  let instructions = case program of
        MockIRProgram instrs -> instrs
      typeChecks = map checkInstructionType instructions
  in if all id typeChecks
     then Right program
     else Left "Type error"
  where
    checkInstructionType (MockIRLoad _ _) = True
    checkInstructionType (MockIRStore _ _) = True
    checkInstructionType (MockIRAdd _ _ _) = True
    checkInstructionType (MockIRSub _ _ _) = True
    checkInstructionType (MockIRCall _ _ _) = True

mockOptimize :: MockIRProgram -> MockIRProgram
mockOptimize program = 
  -- Simplified optimization - remove redundant operations
  case program of
    MockIRProgram instrs -> 
      let optimized = removeRedundant instrs
      in MockIRProgram optimized
  where
    removeRedundant [] = []
    removeRedundant (i1:i2:rest) = 
      if isRedundantPair i1 i2
      then removeRedundant rest
      else i1 : removeRedundant (i2:rest)
    removeRedundant [i] = [i]
    
    isRedundantPair (MockIRLoad x t) (MockIRStore x' t') = x == x' && t == t'
    isRedundantPair _ _ = False

mockValidateIR :: MockIRProgram -> Bool
mockValidateIR program = 
  case program of
    MockIRProgram instrs -> all validateInstruction instrs
  where
    validateInstruction (MockIRLoad name _) = not (null name)
    validateInstruction (MockIRStore name _) = not (null name)
    validateInstruction (MockIRAdd dest src1 src2) = 
      not (null dest) && not (null src1) && not (null src2)
    validateInstruction (MockIRSub dest src1 src2) = 
      not (null dest) && not (null src1) && not (null src2)
    validateInstruction (MockIRCall dest func args) = 
      not (null dest) && not (null func) && all (not . null) args

mockCountInstructions :: MockIRProgram -> Int
mockCountInstructions program = 
  case program of
    MockIRProgram instrs -> length instrs

mockHasInstructions :: MockIRProgram -> Bool
mockHasInstructions program = mockCountInstructions program > 0

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Type checking preserves program structure
prop_typeCheckPreservesStructure :: MockIRProgram -> Property
prop_typeCheckPreservesStructure program = 
  case mockTypeCheck program of
    Right program' -> program' === program
    Left _ -> property True

-- Property: Optimization never increases instruction count
prop_optimizationNeverIncreases :: MockIRProgram -> Property
prop_optimizationNeverIncreases program = 
  let originalCount = mockCountInstructions program
      optimized = mockOptimize program
      optimizedCount = mockCountInstructions optimized
  in optimizedCount <= originalCount

-- Property: Optimization preserves validity
prop_optimizationPreservesValidity :: MockIRProgram -> Property
prop_optimizationPreservesValidity program = 
  let isValid = mockValidateIR program
      optimized = mockOptimize program
      optimizedIsValid = mockValidateIR optimized
  in isValid ==> optimizedIsValid

-- Property: Empty program is always valid
prop_emptyProgramValid :: Property
prop_emptyProgramValid = 
  let emptyProgram = MockIRProgram []
  in mockValidateIR emptyProgram === True

-- Property: Type checking is deterministic
prop_typeCheckDeterministic :: MockIRProgram -> Property
prop_typeCheckDeterministic program = 
  let result1 = mockTypeCheck program
      result2 = mockTypeCheck program
  in result1 === result2

-- Property: Optimization is deterministic
prop_optimizationDeterministic :: MockIRProgram -> Property
prop_optimizationDeterministic program = 
  let result1 = mockOptimize program
      result2 = mockOptimize program
  in result1 === result2

-- Property: Optimization is idempotent
prop_optimizationIdempotent :: MockIRProgram -> Property
prop_optimizationIdempotent program = 
  let once = mockOptimize program
      twice = mockOptimize once
  in once === twice

-- Property: Adding instructions increases count
prop_addingInstructionsIncreasesCount :: MockIRProgram -> MockIRInstruction -> Property
prop_addingInstructionsIncreasesCount program instruction = 
  let originalCount = mockCountInstructions program
      newProgram = case program of
        MockIRProgram instrs -> MockIRProgram (instrs ++ [instruction])
      newCount = mockCountInstructions newProgram
  in newCount === originalCount + 1

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler IR Consistency QuickCheck Tests"
  [ testProperty "Type checking preserves program structure" prop_typeCheckPreservesStructure
  , testProperty "Optimization never increases instruction count" prop_optimizationNeverIncreases
  , testProperty "Optimization preserves validity" prop_optimizationPreservesValidity
  , testProperty "Empty program is always valid" prop_emptyProgramValid
  , testProperty "Type checking is deterministic" prop_typeCheckDeterministic
  , testProperty "Optimization is deterministic" prop_optimizationDeterministic
  , testProperty "Optimization is idempotent" prop_optimizationIdempotent
  , testProperty "Adding instructions increases count" prop_addingInstructionsIncreasesCount
  , testCase "IR consistency edge cases" $ do
      -- Test empty program
      let emptyProgram = MockIRProgram []
      assertBool "Empty program should be valid" $ mockValidateIR emptyProgram
      assertBool "Empty program should type check" $ 
        case mockTypeCheck emptyProgram of
          Right _ -> True
          Left _ -> False
      
      -- Test simple program
      let simpleProgram = MockIRProgram 
            [ MockIRLoad "x" MockIRInt
            , MockIRLoad "y" MockIRInt
            , MockIRAdd "z" "x" "y"
            ]
      assertBool "Simple program should be valid" $ mockValidateIR simpleProgram
      assertBool "Simple program should type check" $ 
        case mockTypeCheck simpleProgram of
          Right _ -> True
          Left _ -> False
      
      -- Test optimization
      let programWithRedundancy = MockIRProgram
            [ MockIRLoad "x" MockIRInt
            , MockIRStore "x" MockIRInt
            , MockIRLoad "y" MockIRInt
            ]
      let optimized = mockOptimize programWithRedundancy
      assertBool "Optimization should remove redundancy" $ 
        mockCountInstructions optimized < mockCountInstructions programWithRedundancy
  ]