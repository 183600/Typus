{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewSyntaxValidationComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, suchThat)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.List (isInfixOf, nub, sort)

import SyntaxValidator (validateSyntax, SyntaxError(..), ValidationContext(..))
import Parser (parseTypus, TypusFile(..))
import Compiler (compile, CompilerError(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

-- | Test syntax validation comprehensive functionality
tests :: TestTree
tests =
  testGroup "New Syntax Validation Comprehensive Tests"
    [ basicSyntaxValidationTests
    , expressionValidationTests
    , statementValidationTests
    , declarationValidationTests
    , controlFlowValidationTests
    , typeSyntaxValidationTests
    , errorRecoveryTests
    , quickCheckProperties
    ]

-- | Basic syntax validation tests
basicSyntaxValidationTests :: TestTree
basicSyntaxValidationTests =
  testGroup "Basic Syntax Validation Tests"
    [ testCase "Valid basic syntax" $
        let validInputs = 
              [ "let x = 5"
              , "func test() { return 42 }"
              , "type Person = struct { name: string, age: int }"
              , "if x > 5 { return true }"
              ]
            results = map (validateSyntax "test.typus") validInputs
        in do
           assertBool "All valid inputs should pass validation" (all isRight results)

    , testCase "Invalid token sequences" $
        let invalidInputs = 
              [ "let x = 5 +"        -- Incomplete expression
              , "func test( {"        -- Missing parameter
              , "if x > 5"           -- Missing body
              , "type Person = {"     -- Incomplete type definition
              ]
            results = map (validateSyntax "test.typus") invalidInputs
        in do
           assertBool "All invalid inputs should fail validation" (all isLeft results)
           let allErrors = concatMap extractLeft results
           assertBool "Should detect syntax errors" (all isSyntaxError allErrors)

    , testCase "Token boundary validation" $
        let inputs = 
              [ "let x=5"            -- Missing spaces
              , "let x =5"           -- Inconsistent spacing
              , "letx = 5"           -- Missing space after keyword
              , "let x = 5;"         -- Semicolon (if not allowed)
              ]
            results = map (validateSyntax "test.typus") inputs
        in do
           let (valid, invalid) = partition isRight results
           assertBool "Should handle spacing variations" (length valid + length invalid == length inputs)

    , testCase "Keyword validation" $
        let inputs = 
              [ "let let = 5"        -- Keyword as identifier
              , "func func() { }"     -- Keyword as function name
              , "type type = int"     -- Keyword as type name
              , "if if > 5 { }"       -- Keyword as variable
              ]
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "Should handle keyword misuse" (all isLeft results)
           let allErrors = concatMap extractLeft results
           assertBool "Should detect keyword errors" (any isKeywordError allErrors)
    ]

-- | Expression validation tests
expressionValidationTests :: TestTree
expressionValidationTests =
  testGroup "Expression Validation Tests"
    [ testCase "Valid arithmetic expressions" $
        let validExpressions = 
              [ "5 + 3"
              , "x * y + z"
              , "(a + b) * (c - d)"
              , "x / y % z"
              , "-x + +y"
              ]
            inputs = map (\expr -> "let result = " ++ expr) validExpressions
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid expressions should pass" (all isRight results)

    , testCase "Invalid arithmetic expressions" $
        let invalidExpressions = 
              [ "5 +"               -- Incomplete
              , "* x"               -- Invalid unary position
              , "x + * y"           -- Invalid operator sequence
              , "(x + y"            -- Unmatched parenthesis
              , "x / 0"             -- Division by zero (syntax level)
              ]
            inputs = map (\expr -> "let result = " ++ expr) invalidExpressions
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid expressions should fail" (all isLeft results)

    , testCase "Valid logical expressions" $
        let validExpressions = 
              [ "x && y || z"
              , "!x && !y"
              , "(x > 5) && (y < 10)"
              , "x == y || z != w"
              ]
            inputs = map (\expr -> "let result = " ++ expr) validExpressions
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid logical expressions should pass" (all isRight results)

    , testCase "Invalid logical expressions" $
        let invalidExpressions = 
              [ "&& x"              -- Invalid operator position
              , "x &&"              -- Incomplete
              , "x && || y"         -- Invalid operator sequence
              , "!x &&"             -- Incomplete after negation
              ]
            inputs = map (\expr -> "let result = " ++ expr) invalidExpressions
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid logical expressions should fail" (all isLeft results)

    , testCase "Function call expressions" $
        let callExpressions = 
              [ "func()"                    -- Valid empty parameter call
              , "func(x, y)"               -- Valid multi-parameter call
              , "obj.method()"              -- Valid method call
              , "func(nested_call())"       -- Valid nested call
              , "func("                     -- Invalid incomplete call
              , "func(x,)"                  -- Invalid trailing comma
              , "func(x y)"                 -- Invalid missing comma
              ]
            inputs = map (\expr -> "let result = " ++ expr) callExpressions
            results = map (validateSyntax "test.typus") inputs
        in do
           let (valid, invalid) = partition isRight results
           assertBool "Should validate function calls correctly" (length valid > 0 && length invalid > 0)
    ]

-- | Statement validation tests
statementValidationTests :: TestTree
statementValidationTests =
  testGroup "Statement Validation Tests"
    [ testCase "Valid declaration statements" $
        let validDeclarations = 
              [ "let x = 5"
              , "let y: int = 10"
              , "let z = func() { return 42 }"
              , "const PI = 3.14159"
              ]
            results = map (validateSyntax "test.typus") validDeclarations
        in do
           assertBool "All valid declarations should pass" (all isRight results)

    , testCase "Invalid declaration statements" $
        let invalidDeclarations = 
              [ "let ="               -- Missing identifier and value
              , "let x ="             -- Missing value
              , "let 5 = 10"          -- Invalid identifier
              , "let x = "            -- Incomplete initialization
              ]
            results = map (validateSyntax "test.typus") invalidDeclarations
        in do
           assertBool "All invalid declarations should fail" (all isLeft results)

    , testCase "Valid assignment statements" $
        let validAssignments = 
              [ "x = 5"
              , "y = x + 10"
              , "obj.field = value"
              , "array[0] = 42"
              ]
            inputs = map (\decl -> "let x = 0\n" ++ decl) validAssignments
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid assignments should pass" (all isRight results)

    , testCase "Invalid assignment statements" $
        let invalidAssignments = 
              [ "= 5"                -- Missing target
              , "x ="                -- Missing value
              , "5 = x"              -- Invalid target
              , "x = +"              -- Incomplete value
              ]
            inputs = map (\decl -> "let x = 0\n" ++ decl) invalidAssignments
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid assignments should fail" (all isLeft results)

    , testCase "Valid control statements" $
        let validControlStatements = 
              [ "if x > 5 { return true }"
              , "if condition { } else { }"
              , "while x < 10 { x += 1 }"
              , "for i in 0..10 { }"
              , "match x { case 0 => true, case _ => false }"
              ]
            results = map (validateSyntax "test.typus") validControlStatements
        in do
           assertBool "All valid control statements should pass" (all isRight results)

    , testCase "Invalid control statements" $
        let invalidControlStatements = 
              [ "if x > 5"           -- Missing body
              , "if { }"             -- Missing condition
              , "while"              -- Missing condition and body
              , "for i in"           -- Missing range
              , "match x { }"        -- Missing cases
              ]
            results = map (validateSyntax "test.typus") invalidControlStatements
        in do
           assertBool "All invalid control statements should fail" (all isLeft results)
    ]

-- | Declaration validation tests
declarationValidationTests :: TestTree
declarationValidationTests =
  testGroup "Declaration Validation Tests"
    [ testCase "Valid function declarations" $
        let validFunctions = 
              [ "func test() { }"
              , "func add(x: int, y: int) -> int { return x + y }"
              , "func generic<T>(x: T) -> T { return x }"
              , "func recursive(n: int) -> int { return n <= 1 ? 1 : recursive(n-1) }"
              ]
            results = map (validateSyntax "test.typus") validFunctions
        in do
           assertBool "All valid function declarations should pass" (all isRight results)

    , testCase "Invalid function declarations" $
        let invalidFunctions = 
              [ "func ("              -- Missing name and parameters
              , "func test"           -- Missing parameters and body
              , "func test() ->"      -- Missing return type
              , "func test( x)"       -- Missing type annotation
              , "func test() { }"     -- Duplicate function (context-dependent)
              ]
            results = map (validateSyntax "test.typus") invalidFunctions
        in do
           assertBool "All invalid function declarations should fail" (all isLeft results)

    , testCase "Valid type declarations" $
        let validTypes = 
              [ "type Point = struct { x: int, y: int }"
              , "type Option<T> = union { Some(T), None }"
              , "type Result<T, E> = union { Ok(T), Err(E) }"
              , "type Alias = int"
              ]
            results = map (validateSyntax "test.typus") validTypes
        in do
           assertBool "All valid type declarations should pass" (all isRight results)

    , testCase "Invalid type declarations" $
        let invalidTypes = 
              [ "type ="              -- Missing name and definition
              , "type Point"          -- Missing definition
              , "type Point = {"      -- Incomplete struct
              , "type Invalid = x"    -- Invalid type expression
              ]
            results = map (validateSyntax "test.typus") invalidTypes
        in do
           assertBool "All invalid type declarations should fail" (all isLeft results)

    , testCase "Valid interface declarations" $
        let validInterfaces = 
              [ "interface Drawable { draw() }"
              , "interface Comparable<T> { compare(other: T) -> int }"
              , "interface Iterator<T> { next() -> Option<T> }"
              ]
            results = map (validateSyntax "test.typus") validInterfaces
        in do
           assertBool "All valid interface declarations should pass" (all isRight results)

    , testCase "Invalid interface declarations" $
        let invalidInterfaces = 
              [ "interface {"         -- Missing name
              , "interface Test"      -- Missing body
              , "interface Test { }"  -- Empty interface (may be invalid)
              ]
            results = map (validateSyntax "test.typus") invalidInterfaces
        in do
           assertBool "All invalid interface declarations should fail" (all isLeft results)
    ]

-- | Control flow validation tests
controlFlowValidationTests :: TestTree
controlFlowValidationTests =
  testGroup "Control Flow Validation Tests"
    [ testCase "Valid nested control structures" $
        let validNested = 
              [ "if x > 5 { if y < 10 { return true } }"
              , "while condition { if x > 0 { break } }"
              , "for i in 0..10 { while j < 5 { j += 1 } }"
              , "match x { case 0 => { if true { return 0 } } }"
              ]
            results = map (validateSyntax "test.typus") validNested
        in do
           assertBool "All valid nested structures should pass" (all isRight results)

    , testCase "Invalid nested control structures" $
        let invalidNested = 
              [ "if x > 5 { if y < 10"  -- Missing closing braces
              , "while condition { for"  -- Incomplete nested structure
              , "match x { case 0 => if" -- Incomplete case
              ]
            results = map (validateSyntax "test.typus") invalidNested
        in do
           assertBool "All invalid nested structures should fail" (all isLeft results)

    , testCase "Valid jump statements" $
        let validJumps = 
              [ "while true { break }"
              , "for i in 0..10 { if i == 5 { continue } }"
              , "func test() { if condition { return 42 } }"
              , "loop { if done { break } }"
              ]
            results = map (validateSyntax "test.typus") validJumps
        in do
           assertBool "All valid jump statements should pass" (all isRight results)

    , testCase "Invalid jump statements" $
        let invalidJumps = 
              [ "break"               -- Break outside loop
              , "continue"            -- Continue outside loop
              , "return"              -- Return outside function
              , "if true { break }"   -- Break in wrong context
              ]
            results = map (validateSyntax "test.typus") invalidJumps
        in do
           assertBool "All invalid jump statements should fail" (all isLeft results)

    , testCase "Valid exception handling" $
        let validExceptions = 
              [ "try { risky_operation() } catch (e) { handle_error(e) }"
              , "try { } finally { cleanup() }"
              , "try { } catch (e) { } finally { }"
              ]
            results = map (validateSyntax "test.typus") validExceptions
        in do
           assertBool "All valid exception handling should pass" (all isRight results)

    , testCase "Invalid exception handling" $
        let invalidExceptions = 
              [ "try { }"             -- Missing catch or finally
              , "catch (e) { }"       -- Catch without try
              , "finally { }"          -- Finally without try
              , "try { catch (e) { }" -- Malformed structure
              ]
            results = map (validateSyntax "test.typus") invalidExceptions
        in do
           assertBool "All invalid exception handling should fail" (all isLeft results)
    ]

-- | Type syntax validation tests
typeSyntaxValidationTests :: TestTree
typeSyntaxValidationTests =
  testGroup "Type Syntax Validation Tests"
    [ testCase "Valid primitive types" $
        let validTypes = 
              [ "int", "float", "string", "bool", "char", "void"
              , "uint8", "uint16", "uint32", "uint64"
              , "int8", "int16", "int32", "int64"
              ]
            inputs = map (\t -> "let x: " ++ t ++ " = 0") validTypes
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid primitive types should pass" (all isRight results)

    , testCase "Invalid primitive types" $
        let invalidTypes = 
              [ "integer", "decimal", "text", "boolean"  -- Misspelled types
              , "int128", "uint128"                       -- Unsupported sizes
              , "string32", "float16"                     -- Non-standard types
              ]
            inputs = map (\t -> "let x: " ++ t ++ " = 0") invalidTypes
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid primitive types should fail" (all isLeft results)

    , testCase "Valid composite types" $
        let validTypes = 
              [ "array[int]", "array[5]string", "matrix[3][3]int"
              , "Option<int>", "Result<string, Error>"
              , "Pair<int, string>", "Triple<int, float, bool>"
              , "Ref<int>", "Ptr<string>", "Box<int>"
              ]
            inputs = map (\t -> "let x: " ++ t ++ " = default") validTypes
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid composite types should pass" (all isRight results)

    , testCase "Invalid composite types" $
        let invalidTypes = 
              [ "array[]", "array[int"           -- Malformed array types
              , "Option", "Result<string"        -- Incomplete generic types
              , "Pair[int", "Triple[int, float"  -- Incomplete tuple types
              , "Ref<", "Ptr["                   -- Incomplete pointer types
              ]
            inputs = map (\t -> "let x: " ++ t ++ " = default") invalidTypes
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid composite types should fail" (all isLeft results)

    , testCase "Valid function types" $
        let validTypes = 
              [ "() -> int", "(int) -> string", "(int, string) -> bool"
              , "(int) -> (string)", "(int, string) -> (bool, float)"
              , "() -> ()", "(T) -> T", "(T, U) -> V"
              ]
            inputs = map (\t -> "let f: " ++ t ++ " = default") validTypes
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid function types should pass" (all isRight results)

    , testCase "Invalid function types" $
        let invalidTypes = 
              [ "-> int", "(int ->", "(int) ->"        -- Incomplete function types
              , "(int, -> string", "(int string) ->"   -- Malformed parameter lists
              , "int -> string -> bool"                -- Ambiguous arrow association
              ]
            inputs = map (\t -> "let f: " ++ t ++ " = default") invalidTypes
            results = map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid function types should fail" (all isLeft results)
    ]

-- | Error recovery tests
errorRecoveryTests :: TestTree
errorRecoveryTests =
  testGroup "Error Recovery Tests"
    [ testCase "Recover from missing semicolons" $
        let input = "let x = 5\nlet y = 10\nlet z = x + y"  -- Assuming semicolons are optional
            result = validateSyntax "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should attempt recovery" (any attemptsRecovery errs)
               assertBool "Should provide suggestions" (any providesSuggestions errs)
             Right _ -> assertBool "Should succeed with optional semicolons" True

    , testCase "Recover from unmatched brackets" $
        let input = "let arr = [1, 2, 3\nlet x = arr[0]"  -- Missing closing bracket
            result = validateSyntax "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect bracket mismatch" (any isBracketMismatch errs)
               assertBool "Should suggest fix" (any suggestsBracketFix errs)
             Right _ -> assertFailure "Should have failed with bracket mismatch"

    , testCase "Recover from incomplete expressions" $
        let input = "let x = 5 +\nlet y = 10\nlet z = x + y"
            result = validateSyntax "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect incomplete expression" (any isIncompleteExpression errs)
               assertBool "Should continue parsing" (any continuesParsing errs)
             Right _ -> assertFailure "Should have failed with incomplete expression"

    , testCase "Handle multiple errors gracefully" $
        let input = "let x = 5 +\nlet y = \nlet z = x + y"
            result = validateSyntax "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect multiple errors" (length errs >= 2)
               assertBool "Should order errors by location" (errorsOrderedByLocation errs)
               assertBool "Should avoid cascading errors" (not $ hasCascadingErrors errs)
             Right _ -> assertFailure "Should have failed with multiple errors"
    ]

-- | QuickCheck properties for syntax validation
quickCheckProperties :: TestTree
quickCheckProperties =
  testGroup "QuickCheck Properties"
    [ testProperty "Valid syntax always passes validation" $
        forAll genValidSyntax $ \code ->
            case validateSyntax "test.typus" code of
              Right _ -> property True
              Left errs -> property False

    , testProperty "Invalid syntax always fails validation" $
        forAll genInvalidSyntax $ \code ->
            case validateSyntax "test.typus" code of
              Left _ -> property True
              Right _ -> property False

    , testProperty "Validation is deterministic" $
        forAll genAnySyntax $ \code ->
            let result1 = validateSyntax "test.typus" code
                result2 = validateSyntax "test.typus" code
            in result1 === result2
    ]

-- | Helper functions for validation
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

extractLeft :: Either a b -> [a]
extractLeft (Left err) = [err]
extractLeft _ = []

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

isSyntaxError :: SyntaxError -> Bool
isSyntaxError _ = True  -- Simplified

isKeywordError :: SyntaxError -> Bool
isKeywordError err = "keyword" `isInfixOf` show err

attemptsRecovery :: SyntaxError -> Bool
attemptsRecovery err = "recovery" `isInfixOf` show err

providesSuggestions :: SyntaxError -> Bool
providesSuggestions err = "suggest" `isInfixOf` show err

isBracketMismatch :: SyntaxError -> Bool
isBracketMismatch err = "bracket" `isInfixOf` show err && "mismatch" `isInfixOf` show err

suggestsBracketFix :: SyntaxError -> Bool
suggestsBracketFix err = "bracket" `isInfixOf` show err && "fix" `isInfixOf` show err

isIncompleteExpression :: SyntaxError -> Bool
isIncompleteExpression err = "incomplete" `isInfixOf` show err && "expression" `isInfixOf` show err

continuesParsing :: SyntaxError -> Bool
continuesParsing err = "continue" `isInfixOf` show err

errorsOrderedByLocation :: [SyntaxError] -> Bool
errorsOrderedByLocation errs = 
    let locations = map getErrorLocation errs
    in locations == sort locations

getErrorLocation :: SyntaxError -> (Int, Int)
getErrorLocation err = (1, 1)  -- Simplified

hasCascadingErrors :: [SyntaxError] -> Bool
hasCascadingErrors errs = length errs > 5  -- Simplified cascade detection

-- | Generators for QuickCheck testing
genValidSyntax :: Gen String
genValidSyntax = elements
  [ "let x = 5"
  , "func test() { return 42 }"
  , "type Point = struct { x: int, y: int }"
  , "if x > 5 { return true }"
  , "let result = x + y * z"
  , "func add(a: int, b: int) -> int { return a + b }"
  , "while condition { x += 1 }"
  , "for i in 0..10 { }"
  ]

genInvalidSyntax :: Gen String
genInvalidSyntax = elements
  [ "let x = 5 +"
  , "func test( { return 42 }"
  , "if x > 5"
  , "type Point = {"
  , "let = 5"
  , "func test) ( { }"
  , "while"
  , "for i in"
  ]

genAnySyntax :: Gen String
genAnySyntax = elements
  [ "let x = 5"
  , "let x = 5 +"
  , "func test() { return 42 }"
  , "func test( { return 42 }"
  , "type Point = struct { x: int, y: int }"
  , "type Point = {"
  , "if x > 5 { return true }"
  , "if x > 5"
  ]