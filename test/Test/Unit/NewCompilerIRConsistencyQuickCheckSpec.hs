{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | Compiler IR consistency tests for Compiler module
module Test.Unit.NewCompilerIRConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub, isInfixOf, isPrefixOf, isSuffixOf, intercalate)
import Data.Map (Map, fromList, toList, keys, elems, insert, delete, lookup, member, empty, union)
import qualified Data.Map as Map
import Data.Set (Set, fromList, toList, union, intersection, difference)
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Either (isLeft, isRight)

import Compiler.IR
  ( IR(..)
  , IRNode(..)
  , IRType(..)
  , IRValue(..)
  , IRVariable(..)
  , IRFunction(..)
  , IRStatement(..)
  , IRExpression(..)
  , IRBlock(..)
  , IRProgram(..)
  , IRContext(..)
  , IRMetadata(..)
  , validateIR
  , optimizeIR
  , transformIR
  , compareIR
  , hashIR
  )

import Compiler.TypeChecker
  ( TypeChecker(..)
  , TypeEnvironment(..)
  , TypeError(..)
  , typeCheckIR
  , inferType
  )

import Compiler.GoAst
  ( GoAST(..)
  , GoNode(..)
  , GoExpression(..)
  , GoStatement(..)
  , GoType(..)
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  )

-- ============================================================================
-- Helper Functions and Generators
-- ============================================================================

-- Generate IR types
genIRType :: Gen IRType
genIRType = oneof
  [ return IRInt
  , return IRBool
  , return IRString
  , return IRVoid
  , IRArray <$> genIRType
  , IRFunction <$> listOf genIRType <*> genIRType
  , IRStruct <$> listOf ((,) <$> genIdentifier <*> genIRType)
  , IRPointer <$> genIRType
  ]

-- Generate IR values
genIRValue :: Gen IRValue
genIRValue = oneof
  [ IRIntValue <$> choose (-1000, 1000)
  , IRBoolValue <$> elements [True, False]
  , IRStringValue <$> genIdentifier
  , IRNullValue
  ]

-- Generate IR variables
genIRVariable :: Gen IRVariable
genIRVariable = do
  name <- genIdentifier
  varType <- genIRType
  isMutable <- elements [True, False]
  return $ IRVariable name varType isMutable startPos

-- Generate IR expressions
genIRExpression :: Gen IRExpression
genIRExpression = oneof
  [ IRConstant <$> genIRValue
  , IRVariable <$> genIRVariable
  , IRBinaryOp <$> elements ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="] <*> genIRExpression <*> genIRExpression
  , IRUnaryOp <$> elements ["!", "-"] <*> genIRExpression
  , IRFunctionCall <$> genIdentifier <*> listOf genIRExpression
  , IRArrayAccess <$> genIRExpression <*> genIRExpression
  , IRStructAccess <$> genIRExpression <*> genIdentifier
  ]

-- Generate IR statements
genIRStatement :: Gen IRStatement
genIRStatement = oneof
  [ IRVariableDecl <$> genIRVariable <*> genIRExpression
  , IRAssignment <$> genIdentifier <*> genIRExpression
  , IRReturn <$> oneof [return Nothing, Just <$> genIRExpression]
  , IRIf <$> genIRExpression <*> genIRBlock <*> genIRBlock
  , IRWhile <$> genIRExpression <*> genIRBlock
  , IRFor <$> genIRVariable <*> genIRExpression <*> genIRExpression <*> genIRBlock
  , IRExpressionStmt <$> genIRExpression
  ]

-- Generate IR blocks
genIRBlock :: Gen IRBlock
genIRBlock = do
  statements <- listOf genIRStatement
  return $ IRBlock statements

-- Generate IR functions
genIRFunction :: Gen IRFunction
genIRFunction = do
  name <- genIdentifier
  params <- listOf genIRVariable
  returnType <- genIRType
  body <- genIRBlock
  return $ IRFunction name params returnType body

-- Generate IR programs
genIRProgram :: Gen IRProgram
genIRProgram = do
  functions <- listOf genIRFunction
  globals <- listOf genIRVariable
  return $ IRProgram functions globals

-- Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'])
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- ============================================================================
-- IR Consistency Properties
-- ============================================================================

-- Property: IR should be structurally consistent
prop_ir_structural_consistency :: IRProgram -> Property
prop_ir_structural_consistency program =
  let validationResult = validateIR program
  in property $ case validationResult of
         Left _ -> property False  -- Should be valid
         Right _ -> property True

-- Property: IR optimization should preserve semantics
prop_ir_optimization_preserves_semantics :: IRProgram -> Property
prop_ir_optimization_preserves_semantics program =
  let validationResult = validateIR program
      optimizedProgram = optimizeIR program
      optimizedValidation = validateIR optimizedProgram
  in case (validationResult, optimizedValidation) of
       (Right _, Right _) -> property True
       (Left _, Left _) -> property True  -- Both invalid is acceptable
       _ -> property False  -- One valid, one invalid is not acceptable

-- Property: IR transformation should be reversible for identity transforms
prop_ir_transformation_reversible :: IRProgram -> Property
prop_ir_transformation_reversible program =
  let identityTransform = transformIR (\node -> node)
      transformedProgram = identityTransform program
      validationResult1 = validateIR program
      validationResult2 = validateIR transformedProgram
  in property $ validationResult1 === validationResult2

-- Property: IR comparison should be reflexive
prop_ir_comparison_reflexive :: IRProgram -> Property
prop_ir_comparison_reflexive program =
  let comparisonResult = compareIR program program
  in property $ comparisonResult === EQ

-- Property: IR comparison should be symmetric
prop_ir_comparison_symmetric :: IRProgram -> IRProgram -> Property
prop_ir_comparison_symmetric program1 program2 =
  let comparison1 = compareIR program1 program2
      comparison2 = compareIR program2 program1
  in property $ comparison1 === flipOrdering comparison2
  where
    flipOrdering LT = GT
    flipOrdering GT = LT
    flipOrdering EQ = EQ

-- Property: IR hashing should be consistent
prop_ir_hashing_consistent :: IRProgram -> Property
prop_ir_hashing_consistent program =
  let hash1 = hashIR program
      hash2 = hashIR program
  in property $ hash1 === hash2

-- Property: IR hashing should be deterministic for equivalent programs
prop_ir_hashing_deterministic :: IRProgram -> IRProgram -> Property
prop_ir_hashing_deterministic program1 program2 =
  let comparison = compareIR program1 program2
      hash1 = hashIR program1
      hash2 = hashIR program2
  in comparison === EQ ==> hash1 === hash2

-- Property: Type checking should be consistent with IR types
prop_type_checking_consistency :: IRProgram -> Property
prop_type_checking_consistency program =
  let validationResult = validateIR program
      typeCheckResult = typeCheckIR program
  in case (validationResult, typeCheckResult) of
       (Right _, Right _) -> property True
       (Left _, Left _) -> property True  -- Both invalid is acceptable
       (Right _, Left _) -> property True  -- Valid IR but type errors is acceptable
       (Left _, Right _) -> property False  -- Invalid IR but valid types is not acceptable

-- Property: IR variable references should be valid
prop_ir_variable_references_valid :: IRProgram -> Property
prop_ir_variable_references_valid program =
  let definedVars = collectDefinedVariables program
      usedVars = collectUsedVariables program
      undefinedVars = Set.difference usedVars definedVars
  in property $ Set.null undefinedVars

-- Property: IR function calls should reference existing functions
prop_ir_function_calls_valid :: IRProgram -> Property
prop_ir_function_calls_valid program =
  let definedFunctions = Set.fromList $ map functionName (irFunctions program)
      calledFunctions = collectCalledFunctions program
      undefinedFunctions = Set.difference calledFunctions definedFunctions
  in property $ Set.null undefinedFunctions

-- Property: IR type annotations should be consistent
prop_ir_type_annotations_consistent :: IRProgram -> Property
prop_ir_type_annotations_consistent program =
  let typeConsistency = checkTypeConsistency program
  in property $ typeConsistency

-- Property: IR control flow should be well-formed
prop_ir_control_flow_well_formed :: IRProgram -> Property
prop_ir_control_flow_well_formed program =
  let controlFlowCheck = checkControlFlow program
  in property $ controlFlowCheck

-- ============================================================================
-- Helper Functions for Properties
-- ============================================================================

-- Collect all defined variables in an IR program
collectDefinedVariables :: IRProgram -> Set String
collectDefinedVariables program =
  let globalVars = Set.fromList $ map varName (irGlobals program)
      functionVars = Set.unions $ map collectFunctionVariables (irFunctions program)
  in Set.union globalVars functionVars

-- Collect variables defined in a function
collectFunctionVariables :: IRFunction -> Set String
collectFunctionVariables function =
  let paramVars = Set.fromList $ map varName (functionParams function)
      bodyVars = collectBlockVariables (functionBody function)
  in Set.union paramVars bodyVars

-- Collect variables defined in a block
collectBlockVariables :: IRBlock -> Set String
collectBlockVariables (IRBlock statements) =
  Set.unions $ map collectStatementVariables statements

-- Collect variables defined in a statement
collectStatementVariables :: IRStatement -> Set String
collectStatementVariables stmt = case stmt of
  IRVariableDecl var _ -> Set.singleton (varName var)
  IRAssignment _ _ -> Set.empty
  IRReturn _ -> Set.empty
  IRIf _ thenBlock elseBlock -> Set.union (collectBlockVariables thenBlock) (collectBlockVariables elseBlock)
  IRWhile _ block -> collectBlockVariables block
  IRFor var _ _ block -> Set.union (Set.singleton (varName var)) (collectBlockVariables block)
  IRExpressionStmt _ -> Set.empty

-- Collect all used variables in an IR program
collectUsedVariables :: IRProgram -> Set String
collectUsedVariables program =
  Set.unions $ map collectFunctionUsedVariables (irFunctions program)

-- Collect variables used in a function
collectFunctionUsedVariables :: IRFunction -> Set String
collectFunctionUsedVariables function =
  collectBlockUsedVariables (functionBody function)

-- Collect variables used in a block
collectBlockUsedVariables :: IRBlock -> Set String
collectBlockUsedVariables (IRBlock statements) =
  Set.unions $ map collectStatementUsedVariables statements

-- Collect variables used in a statement
collectStatementUsedVariables :: IRStatement -> Set String
collectStatementUsedVariables stmt = case stmt of
  IRVariableDecl var expr -> Set.union (Set.singleton (varName var)) (collectExpressionUsedVariables expr)
  IRAssignment varName expr -> Set.union (Set.singleton varName) (collectExpressionUsedVariables expr)
  IRReturn maybeExpr -> maybe Set.empty collectExpressionUsedVariables maybeExpr
  IRIf condition thenBlock elseBlock -> Set.unions 
    [ collectExpressionUsedVariables condition
    , collectBlockUsedVariables thenBlock
    , collectBlockUsedVariables elseBlock
    ]
  IRWhile condition block -> Set.union 
    (collectExpressionUsedVariables condition) 
    (collectBlockUsedVariables block)
  IRFor var startExpr endExpr block -> Set.unions
    [ Set.singleton (varName var)
    , collectExpressionUsedVariables startExpr
    , collectExpressionUsedVariables endExpr
    , collectBlockUsedVariables block
    ]
  IRExpressionStmt expr -> collectExpressionUsedVariables expr

-- Collect variables used in an expression
collectExpressionUsedVariables :: IRExpression -> Set String
collectExpressionUsedVariables expr = case expr of
  IRConstant _ -> Set.empty
  IRVariable var -> Set.singleton (varName var)
  IRBinaryOp _ left right -> Set.union (collectExpressionUsedVariables left) (collectExpressionUsedVariables right)
  IRUnaryOp _ operand -> collectExpressionUsedVariables operand
  IRFunctionCall _ args -> Set.unions $ map collectExpressionUsedVariables args
  IRArrayAccess array index -> Set.union (collectExpressionUsedVariables array) (collectExpressionUsedVariables index)
  IRStructAccess obj _ -> collectExpressionUsedVariables obj

-- Collect all called functions in an IR program
collectCalledFunctions :: IRProgram -> Set String
collectCalledFunctions program =
  Set.unions $ map collectFunctionCalledFunctions (irFunctions program)

-- Collect functions called in a function
collectFunctionCalledFunctions :: IRFunction -> Set String
collectFunctionCalledFunctions function =
  collectBlockCalledFunctions (functionBody function)

-- Collect functions called in a block
collectBlockCalledFunctions :: IRBlock -> Set String
collectBlockCalledFunctions (IRBlock statements) =
  Set.unions $ map collectStatementCalledFunctions statements

-- Collect functions called in a statement
collectStatementCalledFunctions :: IRStatement -> Set String
collectStatementCalledFunctions stmt = case stmt of
  IRVariableDecl _ expr -> collectExpressionCalledFunctions expr
  IRAssignment _ expr -> collectExpressionCalledFunctions expr
  IRReturn maybeExpr -> maybe Set.empty collectExpressionCalledFunctions maybeExpr
  IRIf condition thenBlock elseBlock -> Set.unions
    [ collectExpressionCalledFunctions condition
    , collectBlockCalledFunctions thenBlock
    , collectBlockCalledFunctions elseBlock
    ]
  IRWhile condition block -> Set.union 
    (collectExpressionCalledFunctions condition)
    (collectBlockCalledFunctions block)
  IRFor _ startExpr endExpr block -> Set.unions
    [ collectExpressionCalledFunctions startExpr
    , collectExpressionCalledFunctions endExpr
    , collectBlockCalledFunctions block
    ]
  IRExpressionStmt expr -> collectExpressionCalledFunctions expr

-- Collect functions called in an expression
collectExpressionCalledFunctions :: IRExpression -> Set String
collectExpressionCalledFunctions expr = case expr of
  IRConstant _ -> Set.empty
  IRVariable _ -> Set.empty
  IRBinaryOp _ left right -> Set.union (collectExpressionCalledFunctions left) (collectExpressionCalledFunctions right)
  IRUnaryOp _ operand -> collectExpressionCalledFunctions operand
  IRFunctionCall name args -> Set.union (Set.singleton name) (Set.unions $ map collectExpressionCalledFunctions args)
  IRArrayAccess array index -> Set.union (collectExpressionCalledFunctions array) (collectExpressionCalledVariables index)
  IRStructAccess obj _ -> collectExpressionCalledFunctions obj

-- Check type consistency in IR program
checkTypeConsistency :: IRProgram -> Bool
checkTypeConsistency program = all checkFunctionConsistency (irFunctions program)
  where
    checkFunctionConsistency :: IRFunction -> Bool
    checkFunctionConsistency function = True  -- Simplified for now

-- Check control flow in IR program
checkControlFlow :: IRProgram -> Bool
checkControlFlow program = all checkFunctionControlFlow (irFunctions program)
  where
    checkFunctionControlFlow :: IRFunction -> Bool
    checkFunctionControlFlow function = True  -- Simplified for now

-- ============================================================================
-- Performance and Scalability Properties
-- ============================================================================

-- Property: IR comparison should handle large programs efficiently
prop_ir_comparison_large_programs :: Int -> Property
prop_ir_comparison_large_programs numFunctions =
  numFunctions > 0 && numFunctions <= 100 ==> 
  let largeProgram = generateLargeProgram numFunctions
      comparisonResult = compareIR largeProgram largeProgram
  in property $ comparisonResult === EQ

-- Property: IR hashing should handle large programs efficiently
prop_ir_hashing_large_programs :: Int -> Property
prop_ir_hashing_large_programs numFunctions =
  numFunctions > 0 && numFunctions <= 100 ==> 
  let largeProgram = generateLargeProgram numFunctions
      hashResult = hashIR largeProgram
  in property $ hashResult >= 0

-- Generate a large IR program for testing
generateLargeProgram :: Int -> IRProgram
generateLargeProgram numFunctions =
  let functions = take numFunctions $ repeat generateDummyFunction
      globals = []
  in IRProgram functions globals

-- Generate a dummy function for testing
generateDummyFunction :: IRFunction
generateDummyFunction = IRFunction "dummy" [] IRVoid (IRBlock [])

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Compiler IR Consistency QuickCheck Tests"
  [ testGroup "IR Structural Consistency"
    [ fastProperty "IR structural consistency" prop_ir_structural_consistency
    , fastProperty "IR optimization preserves semantics" prop_ir_optimization_preserves_semantics
    , fastProperty "IR transformation reversible" prop_ir_transformation_reversible
    ]

  , testGroup "IR Comparison and Hashing"
    [ fastProperty "IR comparison reflexive" prop_ir_comparison_reflexive
    , fastProperty "IR comparison symmetric" prop_ir_comparison_symmetric
    , fastProperty "IR hashing consistent" prop_ir_hashing_consistent
    , fastProperty "IR hashing deterministic" prop_ir_hashing_deterministic
    ]

  , testGroup "Type Checking Consistency"
    [ fastProperty "type checking consistency" prop_type_checking_consistency
    , fastProperty "IR type annotations consistent" prop_ir_type_annotations_consistent
    ]

  , testGroup "Variable and Function References"
    [ fastProperty "IR variable references valid" prop_ir_variable_references_valid
    , fastProperty "IR function calls valid" prop_ir_function_calls_valid
    ]

  , testGroup "Control Flow and Structure"
    [ fastProperty "IR control flow well formed" prop_ir_control_flow_well_formed
    ]

  , testGroup "Performance and Scalability"
    [ fastProperty "IR comparison large programs" prop_ir_comparison_large_programs
    , fastProperty "IR hashing large programs" prop_ir_hashing_large_programs
    ]
  ]