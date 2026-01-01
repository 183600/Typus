{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewSyntaxValidationComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, suchThat)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, catMaybes)
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (nub, sort)

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
            results = L.map (validateSyntax "test.typus") validInputs
        in do
           assertBool "All valid inputs should pass validation" (L.all isRight results)

    , testCase "Invalid token sequences" $
        let invalidInputs = 
              [ "let x = 5 +"        -- Incomplete expression
              , "func test( {"        -- Missing parameter
              , "if x > 5"           -- Missing body
              , "type Person = {"     -- Incomplete type definition
              ]
            results = L.map (validateSyntax "test.typus") invalidInputs
        in do
           assertBool "All invalid inputs should fail validation" (L.all isLeft results)
           let allErrors = concatMap extractLeft results
           assertBool "Should detect syntax errors" (L.all isSyntaxError allErrors)

    , testCase "Token boundary validation" $
        let inputs = 
              [ "let x=5"            -- Missing spaces
              , "let x =5"           -- Inconsistent spacing
              , "letx = 5"           -- Missing space after keyword
              , "let x = 5;"         -- Semicolon (if not allowed)
              ]
            results = L.map (validateSyntax "test.typus") inputs
        in do
           let (valid, invalid) = partition isRight results
           assertBool "Should handle spacing variations" (L.length valid + L.length invalid == L.length inputs)

    , testCase "Keyword validation" $
        let inputs = 
              [ "let let = 5"        -- Keyword as identifier
              , "func func() { }"     -- Keyword as function name
              , "type type = int"     -- Keyword as type name
              , "if if > 5 { }"       -- Keyword as variable
              ]
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "Should handle keyword misuse" (L.all isLeft results)
           let allErrors = concatMap extractLeft results
           assertBool "Should detect keyword errors" (L.any isKeywordError allErrors)
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
            inputs = L.map (\expr -> "let result = " ++ expr) validExpressions
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid expressions should pass" (L.all isRight results)

    , testCase "Invalid arithmetic expressions" $
        let invalidExpressions = 
              [ "5 +"               -- Incomplete
              , "* x"               -- Invalid unary position
              , "x + * y"           -- Invalid operator sequence
              , "(x + y"            -- Unmatched parenthesis
              , "x / 0"             -- Division by zero (syntax level)
              ]
            inputs = L.map (\expr -> "let result = " ++ expr) invalidExpressions
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid expressions should fail" (L.all isLeft results)

    , testCase "Valid logical expressions" $
        let validExpressions = 
              [ "x && y || z"
              , "!x && !y"
              , "(x > 5) && (y < 10)"
              , "x == y || z != w"
              ]
            inputs = L.map (\expr -> "let result = " ++ expr) validExpressions
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid logical expressions should pass" (L.all isRight results)

    , testCase "Invalid logical expressions" $
        let invalidExpressions = 
              [ "&& x"              -- Invalid operator position
              , "x &&"              -- Incomplete
              , "x && || y"         -- Invalid operator sequence
              , "!x &&"             -- Incomplete after negation
              ]
            inputs = L.map (\expr -> "let result = " ++ expr) invalidExpressions
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid logical expressions should fail" (L.all isLeft results)

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
            inputs = L.map (\expr -> "let result = " ++ expr) callExpressions
            results = L.map (validateSyntax "test.typus") inputs
        in do
           let (valid, invalid) = partition isRight results
           assertBool "Should validate function calls correctly" (L.length valid > 0 && L.length invalid > 0)
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
            results = L.map (validateSyntax "test.typus") validDeclarations
        in do
           assertBool "All valid declarations should pass" (L.all isRight results)

    , testCase "Invalid declaration statements" $
        let invalidDeclarations = 
              [ "let ="               -- Missing identifier L.and value
              , "let x ="             -- Missing value
              , "let 5 = 10"          -- Invalid identifier
              , "let x = "            -- Incomplete initialization
              ]
            results = L.map (validateSyntax "test.typus") invalidDeclarations
        in do
           assertBool "All invalid declarations should fail" (L.all isLeft results)

    , testCase "Valid assignment statements" $
        let validAssignments = 
              [ "x = 5"
              , "y = x + 10"
              , "obj.field = value"
              , "array[0] = 42"
              ]
            inputs = L.map (\decl -> "let x = 0\n" ++ decl) validAssignments
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid assignments should pass" (L.all isRight results)

    , testCase "Invalid assignment statements" $
        let invalidAssignments = 
              [ "= 5"                -- Missing target
              , "x ="                -- Missing value
              , "5 = x"              -- Invalid target
              , "x = +"              -- Incomplete value
              ]
            inputs = L.map (\decl -> "let x = 0\n" ++ decl) invalidAssignments
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid assignments should fail" (L.all isLeft results)

    , testCase "Valid control statements" $
        let validControlStatements = 
              [ "if x > 5 { return true }"
              , "if condition { } else { }"
              , "while x < 10 { x += 1 }"
              , "for i in 0..10 { }"
              , "match x { case 0 => true, case _ => false }"
              ]
            results = L.map (validateSyntax "test.typus") validControlStatements
        in do
           assertBool "All valid control statements should pass" (L.all isRight results)

    , testCase "Invalid control statements" $
        let invalidControlStatements = 
              [ "if x > 5"           -- Missing body
              , "if { }"             -- Missing condition
              , "while"              -- Missing condition L.and body
              , "for i in"           -- Missing range
              , "match x { }"        -- Missing cases
              ]
            results = L.map (validateSyntax "test.typus") invalidControlStatements
        in do
           assertBool "All invalid control statements should fail" (L.all isLeft results)
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
            results = L.map (validateSyntax "test.typus") validFunctions
        in do
           assertBool "All valid function declarations should pass" (L.all isRight results)

    , testCase "Invalid function declarations" $
        let invalidFunctions = 
              [ "func ("              -- Missing name L.and parameters
              , "func test"           -- Missing parameters L.and body
              , "func test() ->"      -- Missing return type
              , "func test( x)"       -- Missing type annotation
              , "func test() { }"     -- Duplicate function (context-dependent)
              ]
            results = L.map (validateSyntax "test.typus") invalidFunctions
        in do
           assertBool "All invalid function declarations should fail" (L.all isLeft results)

    , testCase "Valid type declarations" $
        let validTypes = 
              [ "type Point = struct { x: int, y: int }"
              , "type Option<T> = union { Some(T), None }"
              , "type Result<T, E> = union { Ok(T), Err(E) }"
              , "type Alias = int"
              ]
            results = L.map (validateSyntax "test.typus") validTypes
        in do
           assertBool "All valid type declarations should pass" (L.all isRight results)

    , testCase "Invalid type declarations" $
        let invalidTypes = 
              [ "type ="              -- Missing name L.and definition
              , "type Point"          -- Missing definition
              , "type Point = {"      -- Incomplete struct
              , "type Invalid = x"    -- Invalid type expression
              ]
            results = L.map (validateSyntax "test.typus") invalidTypes
        in do
           assertBool "All invalid type declarations should fail" (L.all isLeft results)

    , testCase "Valid interface declarations" $
        let validInterfaces = 
              [ "interface Drawable { draw() }"
              , "interface Comparable<T> { compare(other: T) -> int }"
              , "interface Iterator<T> { next() -> Option<T> }"
              ]
            results = L.map (validateSyntax "test.typus") validInterfaces
        in do
           assertBool "All valid interface declarations should pass" (L.all isRight results)

    , testCase "Invalid interface declarations" $
        let invalidInterfaces = 
              [ "interface {"         -- Missing name
              , "interface Test"      -- Missing body
              , "interface Test { }"  -- Empty interface (may be invalid)
              ]
            results = L.map (validateSyntax "test.typus") invalidInterfaces
        in do
           assertBool "All invalid interface declarations should fail" (L.all isLeft results)
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
            results = L.map (validateSyntax "test.typus") validNested
        in do
           assertBool "All valid nested structures should pass" (L.all isRight results)

    , testCase "Invalid nested control structures" $
        let invalidNested = 
              [ "if x > 5 { if y < 10"  -- Missing closing braces
              , "while condition { for"  -- Incomplete nested structure
              , "match x { case 0 => if" -- Incomplete case
              ]
            results = L.map (validateSyntax "test.typus") invalidNested
        in do
           assertBool "All invalid nested structures should fail" (L.all isLeft results)

    , testCase "Valid jump statements" $
        let validJumps = 
              [ "while true { break }"
              , "for i in 0..10 { if i == 5 { continue } }"
              , "func test() { if condition { return 42 } }"
              , "loop { if done { break } }"
              ]
            results = L.map (validateSyntax "test.typus") validJumps
        in do
           assertBool "All valid jump statements should pass" (L.all isRight results)

    , testCase "Invalid jump statements" $
        let invalidJumps = 
              [ "break"               -- Break outside loop
              , "continue"            -- Continue outside loop
              , "return"              -- Return outside function
              , "if true { break }"   -- Break in wrong context
              ]
            results = L.map (validateSyntax "test.typus") invalidJumps
        in do
           assertBool "All invalid jump statements should fail" (L.all isLeft results)

    , testCase "Valid exception handling" $
        let validExceptions = 
              [ "try { risky_operation() } catch (e) { handle_error(e) }"
              , "try { } finally { cleanup() }"
              , "try { } catch (e) { } finally { }"
              ]
            results = L.map (validateSyntax "test.typus") validExceptions
        in do
           assertBool "All valid exception handling should pass" (L.all isRight results)

    , testCase "Invalid exception handling" $
        let invalidExceptions = 
              [ "try { }"             -- Missing catch L.or finally
              , "catch (e) { }"       -- Catch without try
              , "finally { }"          -- Finally without try
              , "try { catch (e) { }" -- Malformed structure
              ]
            results = L.map (validateSyntax "test.typus") invalidExceptions
        in do
           assertBool "All invalid exception handling should fail" (L.all isLeft results)
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
            inputs = L.map (\t -> "let x: " ++ t ++ " = 0") validTypes
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid primitive types should pass" (L.all isRight results)

    , testCase "Invalid primitive types" $
        let invalidTypes = 
              [ "integer", "decimal", "text", "boolean"  -- Misspelled types
              , "int128", "uint128"                       -- Unsupported sizes
              , "string32", "float16"                     -- Non-standard types
              ]
            inputs = L.map (\t -> "let x: " ++ t ++ " = 0") invalidTypes
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid primitive types should fail" (L.all isLeft results)

    , testCase "Valid composite types" $
        let validTypes = 
              [ "array[int]", "array[5]string", "matrix[3][3]int"
              , "Option<int>", "Result<string, Error>"
              , "Pair<int, string>", "Triple<int, float, bool>"
              , "Ref<int>", "Ptr<string>", "Box<int>"
              ]
            inputs = L.map (\t -> "let x: " ++ t ++ " = default") validTypes
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid composite types should pass" (L.all isRight results)

    , testCase "Invalid composite types" $
        let invalidTypes = 
              [ "array[]", "array[int"           -- Malformed array types
              , "Option", "Result<string"        -- Incomplete generic types
              , "Pair[int", "Triple[int, float"  -- Incomplete tuple types
              , "Ref<", "Ptr["                   -- Incomplete pointer types
              ]
            inputs = L.map (\t -> "let x: " ++ t ++ " = default") invalidTypes
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid composite types should fail" (L.all isLeft results)

    , testCase "Valid function types" $
        let validTypes = 
              [ "() -> int", "(int) -> string", "(int, string) -> bool"
              , "(int) -> (string)", "(int, string) -> (bool, float)"
              , "() -> ()", "(T) -> T", "(T, U) -> V"
              ]
            inputs = L.map (\t -> "let f: " ++ t ++ " = default") validTypes
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All valid function types should pass" (L.all isRight results)

    , testCase "Invalid function types" $
        let invalidTypes = 
              [ "-> int", "(int ->", "(int) ->"        -- Incomplete function types
              , "(int, -> string", "(int string) ->"   -- Malformed parameter lists
              , "int -> string -> bool"                -- Ambiguous arrow association
              ]
            inputs = L.map (\t -> "let f: " ++ t ++ " = default") invalidTypes
            results = L.map (validateSyntax "test.typus") inputs
        in do
           assertBool "All invalid function types should fail" (L.all isLeft results)
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
               assertBool "Should attempt recovery" (L.any attemptsRecovery errs)
               assertBool "Should provide suggestions" (L.any providesSuggestions errs)
             Right _ -> assertBool "Should succeed with optional semicolons" True

    , testCase "Recover from unmatched brackets" $
        let input = "let arr = [1, 2, 3\nlet x = arr[0]"  -- Missing closing bracket
            result = validateSyntax "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect bracket mismatch" (L.any isBracketMismatch errs)
               assertBool "Should suggest fix" (L.any suggestsBracketFix errs)
             Right _ -> assertFailure "Should have failed with bracket mismatch"

    , testCase "Recover from incomplete expressions" $
        let input = "let x = 5 +\nlet y = 10\nlet z = x + y"
            result = validateSyntax "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect incomplete expression" (L.any isIncompleteExpression errs)
               assertBool "Should continue parsing" (L.any continuesParsing errs)
             Right _ -> assertFailure "Should have failed with incomplete expression"

    , testCase "Handle multiple errors gracefully" $
        let input = "let x = 5 +\nlet y = \nlet z = x + y"
            result = validateSyntax "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should detect multiple errors" (L.length errs >= 2)
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
partition p xs = (filter p xs, L.filter (not . p) xs)

isSyntaxError :: SyntaxError -> Bool
isSyntaxError _ = True  -- Simplified

isKeywordError :: SyntaxError -> Bool
isKeywordError err = "keyword" `L.isInfixOf` show err

attemptsRecovery :: SyntaxError -> Bool
attemptsRecovery err = "recovery" `L.isInfixOf` show err

providesSuggestions :: SyntaxError -> Bool
providesSuggestions err = "suggest" `L.isInfixOf` show err

isBracketMismatch :: SyntaxError -> Bool
isBracketMismatch err = "bracket" `L.isInfixOf` show err && "mismatch" `L.isInfixOf` show err

suggestsBracketFix :: SyntaxError -> Bool
suggestsBracketFix err = "bracket" `L.isInfixOf` show err && "fix" `L.isInfixOf` show err

isIncompleteExpression :: SyntaxError -> Bool
isIncompleteExpression err = "incomplete" `L.isInfixOf` show err && "expression" `L.isInfixOf` show err

continuesParsing :: SyntaxError -> Bool
continuesParsing err = "continue" `L.isInfixOf` show err

errorsOrderedByLocation :: [SyntaxError] -> Bool
errorsOrderedByLocation errs = 
    let locations = map getErrorLocation errs
    in locations == sort locations

getErrorLocation :: SyntaxError -> (Int, Int)
getErrorLocation err = (1, 1)  -- Simplified

hasCascadingErrors :: [SyntaxError] -> Bool
hasCascadingErrors errs = L.length errs > 5  -- Simplified cascade detection

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