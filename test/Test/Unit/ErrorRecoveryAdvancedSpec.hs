{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ErrorRecoveryAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, listOf, elements)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, length, take, drop, lines)
import Data.Char (isSpace, isAlphaNum)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Exception (try, SomeException)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Ownership (OwnershipError(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim)

-- ============================================================================
-- Error Recovery Test Data Generators
-- ============================================================================

-- Generate malformed function declarations
genMalformedFunction :: Gen String
genMalformedFunction = elements
  [ "func test( { return 42; }"                    -- missing parameter
  , "func test(int x { return 42; }"               -- missing closing parenthesis
  , "func test(int x) return 42; }"                -- missing opening brace
  , "func test(int x) { return 42;"                -- missing closing brace
  , "func (int x) { return 42; }"                  -- missing function name
  , "test(int x) { return 42; }"                   -- missing func keyword
  , "func test int x) { return 42; }"              -- malformed parameter list
  , "func test(int x { return 42"                  -- multiple errors
  ]

-- Generate malformed type declarations
genMalformedType :: Gen String
genMalformedType = elements
  [ "type TestStruct struct { field int"           -- missing closing brace
  , "type TestStruct struct  field int }"          -- missing opening brace
  , "type struct { field int }"                    -- missing type name
  , "TestStruct struct { field int }"              -- missing type keyword
  , "type TestStruct { field int }"                -- missing struct keyword
  , "type TestStruct struct { field }"             -- missing field type
  , "type TestStruct struct { int field }"         -- malformed field
  ]

-- Generate malformed expressions
genMalformedExpression :: Gen String
genMalformedExpression = elements
  [ "x + + 5"                                      -- double operator
  , "x * / 5"                                      -- incompatible operators
  , "func()()"                                     -- double call
  , "arr["                                         -- missing index
  , "arr[5"                                        -- missing closing bracket
  , "obj."                                         -- missing field
  , "if (x) { } else"                             -- dangling else
  , "for (;;"                                      -- missing closing parenthesis
  , "return"                                       -- missing return value
  , "return x +"                                   -- incomplete expression
  ]

-- Generate malformed import statements
genMalformedImport :: Gen String
genMalformedImport = elements
  [ "import"                                       -- missing package
  , "import \""                                    -- missing closing quote
  , "import \"package"                             -- missing closing quote
  , "import package"                               -- missing quotes
  , "package"                                      -- missing import keyword
  , "\"package\""                                  -- missing import keyword
  ]

-- ============================================================================
-- Syntax Error Recovery Tests
-- ============================================================================

testMissingBraceRecovery :: TestTree
testMissingBraceRecovery = testCase "Missing brace recovery" $ do
  let input = unlines
        [ "func test() {"
        , "  if (true) {"
        , "    return 42"
        , "  // Missing closing braces"
        , ""
        , "func another() {"
        , "  return 24"
        , "}"
        ]
  
  result <- parseTypus input "missing_brace.typus"
  case result of
    Left err -> do
      let errStr = show err
      assertBool "Error should mention missing brace" ("brace" `isInfixOf` errStr || "}" `isInfixOf` errStr)
      assertBool "Error should include line information" ("line" `isInfixOf` errStr)
    Right file -> assertFailure "Should detect missing brace error"

testUnmatchedParenthesesRecovery :: TestTree
testUnmatchedParenthesesRecovery = testCase "Unmatched parentheses recovery" $ do
  let input = unlines
        [ "func test() {"
        , "  x := (5 + 10"
        , "  y := x * 2"
        , "  return y"
        , "}"
        ]
  
  result <- parseTypus input "unmatched_paren.typus"
  case result of
    Left err -> do
      let errStr = show err
      assertBool "Error should mention parenthesis" ("parenthes" `isInfixOf` errStr || "(" `isInfixOf` errStr)
      assertBool "Error should include location" (any (`isInfixOf` errStr) ["line", "column"])
    Right file -> assertFailure "Should detect unmatched parentheses"

testMultipleErrorRecovery :: TestTree
testMultipleErrorRecovery = testCase "Multiple error recovery" $ do
  let input = unlines
        [ "func test() {"
        , "  x := (5 + 10"        -- missing closing parenthesis
        , "  y := x * 2"
        , "  if (x > 0 {"         -- missing closing parenthesis and brace
        , "    return y"
        , "  // Multiple errors here"
        , ""
        , "func another( { return 24; }"  -- malformed function
        ]
  
  result <- parseTypus input "multiple_errors.typus"
  case result of
    Left err -> do
      let errStr = show err
      assertBool "Should detect multiple errors" (length (lines errStr) > 1 || "error" `isInfixOf` errStr)
    Right file -> assertFailure "Should detect multiple errors"

testIncompleteStatementRecovery :: TestTree
testIncompleteStatementRecovery = testCase "Incomplete statement recovery" $ do
  let input = unlines
        [ "func test() {"
        , "  x :="              -- incomplete assignment
        , "  y := 5"
        , "  return"
        , "}"
        ]
  
  result <- parseTypus input "incomplete.typus"
  case result of
    Left err -> do
      let errStr = show err
      assertBool "Should detect incomplete statement" ("incomplete" `isInfixOf` errStr || "unexpected" `isInfixOf` errStr)
    Right file -> assertFailure "Should detect incomplete statement"

-- ============================================================================
-- Semantic Error Recovery Tests
-- ============================================================================

testUndefinedVariableRecovery :: TestTree
testUndefinedVariableRecovery = testCase "Undefined variable recovery" $ do
  let input = unlines
        [ "func test() {"
        , "  x := undefined_var + 5"
        , "  y := another_undefined * 2"
        , "  return x + y"
        , "}"
        ]
  
  result <- compile "undefined_var.typus" input
  case result of
    Left errs -> do
      assertBool "Should detect undefined variables" (length errs >= 1)
      let errStr = show (head errs)
      assertBool "Error should mention undefined" ("undefined" `isInfixOf` errStr || "not found" `isInfixOf` errStr)
    Right success -> assertFailure "Should detect undefined variables"

testTypeMismatchRecovery :: TestTree
testTypeMismatchRecovery = testCase "Type mismatch recovery" $ do
  let input = unlines
        [ "func test() {"
        , "  x := 5"           -- int
        , "  y := \"hello\""   -- string
        , "  z := x + y"       -- type mismatch
        , "  return z"
        , "}"
        ]
  
  result <- compile "type_mismatch.typus" input
  case result of
    Left errs -> do
      assertBool "Should detect type mismatch" (length errs >= 1)
      let errStr = show (head errs)
      assertBool "Error should mention type" ("type" `isInfixOf` errStr || "mismatch" `isInfixOf` errStr)
    Right success -> assertFailure "Should detect type mismatch"

testFunctionCallErrorRecovery :: TestTree
testFunctionCallErrorRecovery = testCase "Function call error recovery" $ do
  let input = unlines
        [ "func test() {"
        , "  result := nonexistent_func(5, \"hello\")"
        , "  another := wrong_args(42)"  -- wrong number of arguments
        , "  return result"
        , "}"
        ]
  
  result <- compile "func_call_error.typus" input
  case result of
    Left errs -> do
      assertBool "Should detect function call errors" (length errs >= 1)
      let errStr = show (head errs)
      assertBool "Error should mention function" ("function" `isInfixOf` errStr || "undefined" `isInfixOf` errStr)
    Right success -> assertFailure "Should detect function call errors"

-- ============================================================================
-- Ownership Error Recovery Tests
-- ============================================================================

testOwnershipViolationRecovery :: TestTree
testOwnershipViolationRecovery = testCase "Ownership violation recovery" $ do
  let input = unlines
        [ "//! ownership: on"
        , "package main"
        , ""
        , "func test() {"
        , "  owned := create_owned()"
        , "  moved := move(owned)"
        , "  used := owned.use()  -- Use after move"
        , "  return used"
        , "}"
        ]
  
  result <- compile "ownership_violation.typus" input
  case result of
    Left errs -> do
      assertBool "Should detect ownership violation" (length errs >= 1)
      let errStr = show (head errs)
      assertBool "Error should mention ownership" ("ownership" `isInfixOf` errStr || "move" `isInfixOf` errStr)
    Right success -> assertFailure "Should detect ownership violation"

testBorrowingErrorRecovery :: TestTree
testBorrowingErrorRecovery = testCase "Borrowing error recovery" $ do
  let input = unlines
        [ "//! ownership: on"
        , "package main"
        , ""
        , "func test() {"
        , "  data := create_data()"
        , "  borrow1 := borrow(&data)"
        , "  borrow2 := borrow(&data)"  -- Multiple mutable borrows
        , "  use(borrow1)"
        , "  return"
        , "}"
        ]
  
  result <- compile "borrowing_error.typus" input
  case result of
    Left errs -> do
      assertBool "Should detect borrowing error" (length errs >= 1)
      let errStr = show (head errs)
      assertBool "Error should mention borrow" ("borrow" `isInfixOf` errStr || "reference" `isInfixOf` errStr)
    Right success -> assertFailure "Should detect borrowing error"

-- ============================================================================
-- Dependent Types Error Recovery Tests
-- ============================================================================

testDependentTypeViolationRecovery :: TestTree
testDependentTypeViolationRecovery = testCase "Dependent type violation recovery" $ do
  let input = unlines
        [ "//! dependent_types: on"
        , "package main"
        , ""
        , "type Vector(n: int) struct {"
        , "  data [n]int"
        , "}"
        , ""
        , "func test() {"
        , "  v := Vector(5){[1,2,3]}"  -- Length mismatch
        , "  return v"
        , "}"
        ]
  
  result <- compile "dependent_type_error.typus" input
  case result of
    Left errs -> do
      assertBool "Should detect dependent type violation" (length errs >= 1)
      let errStr = show (head errs)
      assertBool "Error should mention dependent type" ("dependent" `isInfixOf` errStr || "length" `isInfixOf` errStr)
    Right success -> assertFailure "Should detect dependent type violation"

testTypeConstraintViolationRecovery :: TestTree
testTypeConstraintViolationRecovery = testCase "Type constraint violation recovery" $ do
  let input = unlines
        [ "//! dependent_types: on"
        , "package main"
        , ""
        , "type PositiveInt = {x: int | x > 0}"
        , ""
        , "func test() {"
        , "  x: PositiveInt = -5"  -- Violates constraint
        , "  return x"
        , "}"
        ]
  
  result <- compile "constraint_violation.typus" input
  case result of
    Left errs -> do
      assertBool "Should detect constraint violation" (length errs >= 1)
      let errStr = show (head errs)
      assertBool "Error should mention constraint" ("constraint" `isInfixOf` errStr || "violation" `isInfixOf` errStr)
    Right success -> assertFailure "Should detect constraint violation"

-- ============================================================================
-- Error Context and Location Tests
-- ============================================================================

testErrorContextPreservation :: TestTree
testErrorContextPreservation = testCase "Error context preservation" $ do
  let input = unlines
        [ "package main"
        , ""
        , "func outer() {"
        , "  func inner() {"
        , "    x := (5 + 10"  -- Error in nested context
        , "    return x"
        , "  }"
        , "  return inner()"
        , "}"
        ]
  
  result <- parseTypus input "context.typus"
  case result of
    Left err -> do
      let errStr = show err
      assertBool "Error should include line number" ("line" `isInfixOf` errStr)
      assertBool "Error should include function context" ("inner" `isInfixOf` errStr || "outer" `isInfixOf` errStr)
    Right file -> assertFailure "Should detect syntax error"

testErrorLocationAccuracy :: TestTree
testErrorLocationAccuracy = testCase "Error location accuracy" $ do
  let input = unlines
        [ "func test() {"
        , "  x := 5"
        , "  y := x +"           -- Line 3, column with error
        , "  z := 10"
        , "  return z"
        , "}"
        ]
  
  result <- parseTypus input "location.typus"
  case result of
    Left err -> do
      let errStr = show err
      assertBool "Error should point to correct line" ("3" `isInfixOf` errStr)
      assertBool "Error should include column info" ("column" `isInfixOf` errStr || "col" `isInfixOf` errStr)
    Right file -> assertFailure "Should detect syntax error"

-- ============================================================================
-- QuickCheck Property Tests for Error Recovery
-- ============================================================================

-- Property: Error messages should never be empty
propErrorMessagesNotEmpty :: String -> Property
propErrorMessagesNotEmpty malformedInput = 
  case parseTypus malformedInput "prop_test.typus" of
    Right _ -> property True   -- Successful parse is OK
    Left err -> 
      let errStr = show err
      in property (not (null errStr))

-- Property: Error messages should contain location information
propErrorMessagesContainLocation :: String -> Property
propErrorMessagesContainLocation malformedInput = 
  case parseTypus malformedInput "prop_test.typus" of
    Right _ -> property True   -- Successful parse is OK
    Left err -> 
      let errStr = show err
      in property (any (`isInfixOf` errStr) ["line", "column", "position"])

-- Property: Error recovery should never crash
propErrorRecoveryNeverCrashes :: String -> Property
propErrorRecoveryNeverCrashes input = 
  let result = parseTypus input "prop_test.typus"
  in case result of
       Left _ -> property True   -- Error is expected
       Right _ -> property True   -- Success is also OK

-- Property: Malformed functions should produce errors
propMalformedFunctionsError :: String -> Property
propMalformedFunctionsError funcName = 
  let input = "func " ++ funcName ++ "( { return 42; }"  -- Always malformed
  in case parseTypus input "malformed_func.typus" of
       Right _ -> property False  -- Should not succeed
       Left err -> property True   -- Should error

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Advanced Error Recovery Test Suite"
  [ testGroup "Syntax Error Recovery Tests"
      [ testMissingBraceRecovery
      , testUnmatchedParenthesesRecovery
      , testMultipleErrorRecovery
      , testIncompleteStatementRecovery
      ]
  
  , testGroup "Semantic Error Recovery Tests"
      [ testUndefinedVariableRecovery
      , testTypeMismatchRecovery
      , testFunctionCallErrorRecovery
      ]
  
  , testGroup "Ownership Error Recovery Tests"
      [ testOwnershipViolationRecovery
      , testBorrowingErrorRecovery
      ]
  
  , testGroup "Dependent Types Error Recovery Tests"
      [ testDependentTypeViolationRecovery
      , testTypeConstraintViolationRecovery
      ]
  
  , testGroup "Error Context and Location Tests"
      [ testErrorContextPreservation
      , testErrorLocationAccuracy
      ]
  
  , testGroup "QuickCheck Error Recovery Property Tests"
      [ testProperty "Error messages not empty" propErrorMessagesNotEmpty
      , testProperty "Error messages contain location" propErrorMessagesContainLocation
      , testProperty "Error recovery never crashes" propErrorRecoveryNeverCrashes
      , testProperty "Malformed functions error" propMalformedFunctionsError
      ]
  ]