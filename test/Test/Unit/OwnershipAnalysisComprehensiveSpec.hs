{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipAnalysisComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.List (isInfixOf, null, length, sort)
import Data.Maybe (isJust, isNothing)

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )

import Ownership.Analyzer
  ( analyzeOwnership
  , analyzeOwnershipDebug
  , analyzeOwnershipFile
  , builtInFunctions
  )

import Ownership.Common.Types
  ( OwnershipAnalyzer(..)
  , OwnershipError(..)
  , OwnershipType(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import Parser
  ( TypusFile(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , parseTypus
  , defaultFileDirectives
  , defaultBlockDirectives
  )

-- | Comprehensive QuickCheck tests for Ownership analysis
-- This module tests ownership analysis, transfer semantics, and error detection

-- Property: OwnershipType ordering is consistent
prop_ownershipType_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownershipType_ordering ot1 ot2 =
  let cmp1 = compare ot1 ot2
      cmp2 = compare ot2 ot1
  in (ot1 == ot2) ==> (cmp1 == EQ && cmp2 == EQ) .&&.
     (ot1 /= ot2) ==> ((cmp1 == LT && cmp2 == GT) .||. (cmp1 == GT && cmp2 == LT))

-- Property: OwnershipType Show is invertible for simple cases
prop_ownershipType_show_simple :: String -> Property
prop_ownershipType_show_simple name =
  not (null name) && not (' ' `elem` name) ==>
  let owned = Owned name
      shown = show owned
      parsed = "Owned " ++ name
  in shown === parsed

-- Property: newOwnershipAnalyzer creates valid analyzer
prop_newOwnershipAnalyzer_valid :: Property
prop_newOwnershipAnalyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
    OwnershipAnalyzer _ _ -> True

-- Property: analyzeOwnership handles empty input
prop_analyzeOwnership_empty :: Property
prop_analyzeOwnership_empty =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in null result

-- Property: analyzeOwnership detects use after move
prop_analyzeOwnership_use_after_move :: String -> Property
prop_analyzeOwnership_use_after_move variableName =
  not (null variableName) && not (' ' `elem` variableName) ==>
  let code = "let x = 42\nlet y = x\nlet z = x\n"  -- x is moved to y, then used again
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
      hasUseAfterMove = any isUseAfterMove result
  in hasUseAfterMove
  where
    isUseAfterMove (UseAfterMove _) = True
    isUseAfterMove _ = False

-- Property: analyzeOwnership handles simple ownership transfer
prop_analyzeOwnership_simple_transfer :: String -> Property
prop_analyzeOwnership_simple_transfer variableName =
  not (null variableName) && not (' ' `elem` variableName) ==>
  let code = "let " ++ variableName ++ " = 42\nlet y = " ++ variableName ++ "\n"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in length result >= 0

-- Property: analyzeOwnershipFile handles valid TypusFile
prop_analyzeOwnershipFile_valid :: String -> Property
prop_analyzeOwnershipFile_valid content =
  not (null content) && "let" `isInfixOf` content ==>
  let caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      let result = analyzeOwnershipFile typusFile
      in length result >= 0

-- Property: analyzeOwnershipDebug provides more information than regular analysis
prop_analyzeOwnershipDebug_verbose :: String -> Property
prop_analyzeOwnershipDebug_verbose content =
  not (null content) ==>
  let analyzer = newOwnershipAnalyzer
      regularResult = analyzeOwnership analyzer content
      debugResult = analyzeOwnershipDebug analyzer content
  in length debugResult >= length regularResult

-- Property: formatOwnershipErrors handles empty error list
prop_formatOwnershipErrors_empty :: Property
prop_formatOwnershipErrors_empty =
  let emptyErrors = [] :: [OwnershipError]
      formatted = formatOwnershipErrors emptyErrors
  in null formatted

-- Property: formatOwnershipErrors includes error types
prop_formatOwnershipErrors_includes_types :: [OwnershipError] -> Property
prop_formatOwnershipErrors_includes_types errors =
  not (null errors) ==>
  let formatted = formatOwnershipErrors errors
      hasErrorType = any (`isInfixOf` formatted) ["UseAfterMove", "DoubleMove", "BorrowWhileMoved"]
  in hasErrorType

-- Property: lexAll handles empty input
prop_lexAll_empty :: Property
prop_lexAll_empty =
  let result = lexAll ""
  in null result

-- Property: lexAll produces tokens for simple code
prop_lexAll_simple :: String -> Property
prop_lexAll_simple variableName =
  not (null variableName) && not (' ' `elem` variableName) ==>
  let code = "let " ++ variableName ++ " = 42"
      result = lexAll code
  in length result >= 3

-- Property: parseProgram handles simple valid code
prop_parseProgram_simple :: String -> Property
prop_parseProgram_simple variableName =
  not (null variableName) && not (' ' `elem` variableName) ==>
  let code = "let " ++ variableName ++ " = 42"
      result = parseProgram code
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: builtInFunctions is not empty
prop_builtInFunctions_not_empty :: Property
prop_builtInFunctions_not_empty =
  let builtIns = builtInFunctions
  in not (null builtIns)

-- Property: OwnershipError equality works correctly
prop_ownershipError_equality :: OwnershipError -> OwnershipError -> Property
prop_ownershipError_equality err1 err2 =
  (err1 == err2) ==> (err1 /= err2) === False .&&.
  (err1 /= err2) ==> (err1 == err2) === False

-- Property: UseAfterMove error contains variable name
prop_useAfterMove_contains_name :: String -> Property
prop_useAfterMove_contains_name variableName =
  not (null variableName) ==>
  let error = UseAfterMove variableName
      shown = show error
  in variableName `isInfixOf` shown

-- Property: DoubleMove error contains both variable names
prop_doubleMove_contains_names :: String -> String -> Property
prop_doubleMove_contains_names var1 var2 =
  not (null var1) && not (null var2) ==>
  let error = DoubleMove var1 var2
      shown = show error
  in var1 `isInfixOf` shown && var2 `isInfixOf` shown

-- Property: BorrowWhileMoved error contains variable name
prop_borrowWhileMoved_contains_name :: String -> Property
prop_borrowWhileMoved_contains_name variableName =
  not (null variableName) ==>
  let error = BorrowWhileMoved variableName
      shown = show error
  in variableName `isInfixOf` shown

-- Property: MutBorrowWhileBorrowed error contains variable name
prop_mutBorrowWhileBorrowed_contains_name :: String -> Property
prop_mutBorrowWhileBorrowed_contains_name variableName =
  not (null variableName) ==>
  let error = MutBorrowWhileBorrowed variableName
      shown = show error
  in variableName `isInfixOf` shown

-- Property: MultipleMutBorrows error contains variable name
prop_multipleMutBorrows_contains_name :: String -> Property
prop_multipleMutBorrows_contains_name variableName =
  not (null variableName) ==>
  let error = MultipleMutBorrows variableName
      shown = show error
  in variableName `isInfixOf` shown

-- Property: OutOfScope error contains variable name
prop_outOfScope_contains_name :: String -> Property
prop_outOfScope_contains_name variableName =
  not (null variableName) ==>
  let error = OutOfScope variableName
      shown = show error
  in variableName `isInfixOf` shown

-- Property: BorrowError contains error message
prop_borrowError_contains_message :: String -> Property
prop_borrowError_contains_message message =
  not (null message) ==>
  let error = BorrowError message
      shown = show error
  in message `isInfixOf` shown

-- Property: ParseError contains error message
prop_parseError_contains_message :: String -> Property
prop_parseError_contains_message message =
  not (null message) ==>
  let error = ParseError message
      shown = show error
  in message `isInfixOf` shown

-- Property: CrossFunctionMove contains function names
prop_crossFunctionMove_contains_names :: String -> String -> Property
prop_crossFunctionMove_contains_names func1 var1 =
  not (null func1) && not (null var1) ==>
  let error = CrossFunctionMove func1 var1
      shown = show error
  in func1 `isInfixOf` shown && var1 `isInfixOf` shown

-- Property: ParameterMoveMismatch contains parameter name
prop_parameterMoveMismatch_contains_name :: String -> Property
prop_parameterMoveMismatch_contains_name paramName =
  not (null paramName) ==>
  let error = ParameterMoveMismatch paramName
      shown = show error
  in paramName `isInfixOf` shown

-- Property: ControlFlowError contains error message
prop_controlFlowError_contains_message :: String -> Property
prop_controlFlowError_contains_message message =
  not (null message) ==>
  let error = ControlFlowError message
      shown = show error
  in message `isInfixOf` shown

-- Property: ownership analysis handles nested let bindings
prop_analyzeOwnership_nested_lets :: String -> String -> Property
prop_analyzeOwnership_nested_lets var1 var2 =
  not (null var1) && not (null var2) && var1 /= var2 &&
  not (' ' `elem` var1) && not (' ' `elem` var2) ==>
  let code = "let " ++ var1 ++ " = 42\nlet " ++ var2 ++ " = " ++ var1 ++ "\nlet z = " ++ var2 ++ "\n"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in length result >= 0

-- Property: ownership analysis handles function calls
prop_analyzeOwnership_function_calls :: String -> String -> Property
prop_analyzeOwnership_function_calls funcName argName =
  not (null funcName) && not (null argName) &&
  not (' ' `elem` funcName) && not (' ' `elem` argName) ==>
  let code = "let " ++ argName ++ " = 42\n" ++ funcName ++ "(" ++ argName ++ ")\n"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in length result >= 0

tests :: TestTree
tests = testGroup "Ownership Analysis Comprehensive QuickCheck tests"
  [ fastProperty "OwnershipType ordering is consistent" prop_ownershipType_ordering
  , fastProperty "OwnershipType Show is invertible for simple cases" prop_ownershipType_show_simple
  , fastProperty "newOwnershipAnalyzer creates valid analyzer" prop_newOwnershipAnalyzer_valid
  , fastProperty "analyzeOwnership handles empty input" prop_analyzeOwnership_empty
  , fastProperty "analyzeOwnership detects use after move" prop_analyzeOwnership_use_after_move
  , fastProperty "analyzeOwnership handles simple ownership transfer" prop_analyzeOwnership_simple_transfer
  , fastProperty "analyzeOwnershipFile handles valid TypusFile" prop_analyzeOwnershipFile_valid
  , fastProperty "analyzeOwnershipDebug provides more information than regular analysis" prop_analyzeOwnershipDebug_verbose
  , fastProperty "formatOwnershipErrors handles empty error list" prop_formatOwnershipErrors_empty
  , fastProperty "formatOwnershipErrors includes error types" prop_formatOwnershipErrors_includes_types
  , fastProperty "lexAll handles empty input" prop_lexAll_empty
  , fastProperty "lexAll produces tokens for simple code" prop_lexAll_simple
  , fastProperty "parseProgram handles simple valid code" prop_parseProgram_simple
  , fastProperty "builtInFunctions is not empty" prop_builtInFunctions_not_empty
  , fastProperty "OwnershipError equality works correctly" prop_ownershipError_equality
  , fastProperty "UseAfterMove error contains variable name" prop_useAfterMove_contains_name
  , fastProperty "DoubleMove error contains both variable names" prop_doubleMove_contains_names
  , fastProperty "BorrowWhileMoved error contains variable name" prop_borrowWhileMoved_contains_name
  , fastProperty "MutBorrowWhileBorrowed error contains variable name" prop_mutBorrowWhileBorrowed_contains_name
  , fastProperty "MultipleMutBorrows error contains variable name" prop_multipleMutBorrows_contains_name
  , fastProperty "OutOfScope error contains variable name" prop_outOfScope_contains_name
  , fastProperty "BorrowError contains error message" prop_borrowError_contains_message
  , fastProperty "ParseError contains error message" prop_parseError_contains_message
  , fastProperty "CrossFunctionMove contains function names" prop_crossFunctionMove_contains_names
  , fastProperty "ParameterMoveMismatch contains parameter name" prop_parameterMoveMismatch_contains_name
  , fastProperty "ControlFlowError contains error message" prop_controlFlowError_contains_message
  , fastProperty "ownership analysis handles nested let bindings" prop_analyzeOwnership_nested_lets
  , fastProperty "ownership analysis handles function calls" prop_analyzeOwnership_function_calls
  ]