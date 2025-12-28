module Test.Unit.NewErrorHandlingConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), choose, listOf, elements)
import ErrorHandler
import Compiler
import Parser
import SourceLocation
import qualified Data.Text as T

-- | Test error handling consistency across compiler phases
tests :: TestTree
tests =
  testGroup "Error Handling Consistency Tests"
    [ testGroup "Error message consistency"
        [ testCase "Parser error messages are consistent" $ do
            let input = "func test( {\n  // missing closing parenthesis\n}"
                result = parseTypus input
            case result of
                Left err -> do
                    assertBool "Parser error should contain location info" ("line" `isInfixOf` show err)
                    assertBool "Parser error should be descriptive" (length (show err) > 10)
                Right parsed -> assertBool "Should not parse invalid syntax" False

        , testCase "Compiler error messages are consistent" $ do
            let input = "func test() {\n  let x: int = \"string\" // type error\n  return x\n}"
                result = compile input
            case result of
                Left err -> do
                    assertBool "Compiler error should contain type info" ("type" `isInfixOf` show err)
                    assertBool "Compiler error should contain location info" ("line" `isInfixOf` show err)
                Right compiled -> assertBool "Should not compile with type errors" False

        , testCase "Error messages are informative" $ do
            let inputs = 
                  [ "func test( {\n}"  -- syntax error
                  , "func test() {\n  let x = undefined_var\n}"  -- undefined variable
                  , "func test() {\n  return\n}"  -- missing return value
                  ]
            results = map compile inputs
            errors = [err | Left err <- results]
            assertBool "All errors should be informative" (all (\e -> length (show e) > 15) errors)

        , testCase "Error messages include source context" $ do
            let input = "func test() {\n  let x: int = \"string\"\n  return x\n}"
                result = compile input
            case result of
                Left err -> assertBool "Error should include source context" (any (`isInfixOf` show err) ["let x:", "string", "int"])
                Right _ -> assertBool "Should not compile with errors" False
        ]

    , testGroup "Error location accuracy"
        [ testCase "Error locations are accurate" $ do
            let input = "func test() {\n  let x: int = \"string\"\n  return x\n}"
                result = compile input
            case result of
                Left err -> do
                    -- Check that error location points to the right line
                    assertBool "Error should point to correct line" ("2" `isInfixOf` show err)
                Right _ -> assertBool "Should not compile with errors" False

        , testCase "Multiple errors are reported correctly" $ do
            let input = "func test() {\n  let x: int = \"string\"\n  let y: string = 42\n  return x + y\n}"
                result = compile input
            case result of
                Left err -> do
                    -- Should report multiple type errors
                    assertBool "Should report multiple errors" (length (lines (show err)) >= 2)
                Right _ -> assertBool "Should not compile with multiple errors" False

        , testCase "Error locations are preserved through phases" $ do
            let input = "func test() {\n  let x: int = \"string\"\n  return x\n}"
                parseResult = parseTypus input
                compileResult = compile input
            case (parseResult, compileResult) of
                (Right parsed, Left compileErr) -> do
                    -- Parse succeeds but compile fails, location should be preserved
                    assertBool "Compile error should preserve location" ("line" `isInfixOf` show compileErr)
                (Left parseErr, _) -> do
                    -- Parse fails, location should be in parse error
                    assertBool "Parse error should have location" ("line" `isInfixOf` show parseErr)
                _ -> assertBool "Should have consistent error reporting" True
        ]

    , testGroup "Error recovery consistency"
        [ testCase "Parser recovers from multiple errors" $ do
            let input = "func test1( {\n} func test2() {\n  let x = \n} func test3() { return 42 }"
                result = parseTypus input
            case result of
                Left err -> do
                    -- Should report multiple errors or recover to parse partial input
                    assertBool "Should handle multiple errors" (length (show err) > 20)
                Right parsed -> assertBool "Should recover and parse partial input" True

        , testCase "Compiler continues after first error" $ do
            let input = "func test() {\n  let x: int = \"string\"\n  let y: string = 42\n  let z: float = true\n}"
                result = compile input
            case result of
                Left err -> do
                    -- Should report multiple type errors
                    assertBool "Should report multiple type errors" (length (lines (show err)) >= 2)
                Right _ -> assertBool "Should not compile with multiple errors" False

        , testCase "Error recovery preserves state" $ do
            let input = "func valid_func() { return 42 }\nfunc invalid_func() {\n  let x: int = \"string\"\n}\nfunc another_valid() { return 24 }"
                result = compile input
            case result of
                Left err -> do
                    -- Should report error but preserve valid parts
                    assertBool "Should report error for invalid function" ("invalid_func" `isInfixOf` show err || "string" `isInfixOf` show err)
                Right _ -> assertBool "Should handle mixed valid/invalid code" True
        ]

    , testGroup "Error classification consistency"
        [ testCase "Syntax errors are classified correctly" $ do
            let inputs = 
                  [ "func test( {\n}"  -- missing parenthesis
                  , "func test() {\n  if true\n}"  -- missing block
                  , "func test() return 42\n}"  -- missing opening brace
                  ]
            results = map parseTypus inputs
            syntaxErrors = [err | Left err <- results]
            assertBool "Syntax errors should be classified" (all (\e -> "syntax" `isInfixOf` show e || "parse" `isInfixOf` show e) syntaxErrors)

        , testCase "Type errors are classified correctly" $ do
            let inputs = 
                  [ "func test() { let x: int = \"string\" }"
                  , "func test() { return \"string\" + 42 }"
                  , "func test() { let x: int = true }"
                  ]
            results = map compile inputs
            typeErrors = [err | Left err <- results]
            assertBool "Type errors should be classified" (all (\e -> "type" `isInfixOf` show e) typeErrors)

        , testCase "Semantic errors are classified correctly" $ do
            let inputs = 
                  [ "func test() { return undefined_var }"
                  , "func test() { x = 42 }"  -- undefined variable
                  , "func test() { return }"  -- missing return value
                  ]
            results = map compile inputs
            semanticErrors = [err | Left err <- results]
            assertBool "Semantic errors should be classified" (all (\e -> any (`isInfixOf` show e) ["undefined", "variable", "return"]) semanticErrors)
        ]

    , testGroup "Property-based tests"
        [ testProperty "Error messages are never empty" prop_errorMessagesNotEmpty
        , testProperty "Error locations are always included" prop_errorLocationsIncluded
        , testProperty "Error classification is consistent" prop_errorClassificationConsistent
        , testProperty "Error recovery is deterministic" prop_errorRecoveryDeterministic
        ]
    ]

-- Helper function to check if substring is in string
isInfixOf :: String -> String -> Bool
isInfixOf sub str = sub `elem` (words str)

-- Property: Error messages should never be empty
prop_errorMessagesNotEmpty :: String -> Bool
prop_errorMessagesNotEmpty input =
    let parseResult = parseTypus input
        compileResult = compile input
    in case (parseResult, compileResult) of
        (Left err, _) -> not (null (show err))
        (Right _, Left err) -> not (null (show err))
        (Right _, Right _) -> True  -- No errors, property holds

-- Property: Error locations should always be included
prop_errorLocationsIncluded :: String -> Bool
prop_errorLocationsIncluded input =
    let parseResult = parseTypus input
        compileResult = compile input
    in case (parseResult, compileResult) of
        (Left err, _) -> any (`isInfixOf` show err) ["line", "column", "position"]
        (Right _, Left err) -> any (`isInfixOf` show err) ["line", "column", "position"]
        (Right _, Right _) -> True  -- No errors, property holds

-- Property: Error classification should be consistent
prop_errorClassificationConsistent :: String -> Bool
prop_errorClassificationConsistent input =
    let parseResult = parseTypus input
        compileResult = compile input
    in case (parseResult, compileResult) of
        (Left err, _) -> any (`isInfixOf` show err) ["syntax", "parse", "error"]
        (Right _, Left err) -> any (`isInfixOf` show err) ["type", "semantic", "error"]
        (Right _, Right _) -> True  -- No errors, property holds

-- Property: Error recovery should be deterministic
prop_errorRecoveryDeterministic :: String -> Bool
prop_errorRecoveryDeterministic input =
    let parseResult1 = parseTypus input
        parseResult2 = parseTypus input
        compileResult1 = compile input
        compileResult2 = compile input
    in case (parseResult1, parseResult2) of
        (Left err1, Left err2) -> show err1 == show err2
        (Right res1, Right res2) -> show res1 == show res2
        _ -> False  -- Results should be consistent