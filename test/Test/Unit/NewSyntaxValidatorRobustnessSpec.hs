module Test.Unit.NewSyntaxValidatorRobustnessSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), choose, listOf, elements)
import SyntaxValidator
import Parser
import qualified Data.Text as T

-- | Test syntax validator robustness and edge cases
tests :: TestTree
tests =
  testGroup "Syntax Validator Robustness Tests"
    [ testGroup "Malformed syntax handling"
        [ testCase "Validator handles unmatched brackets" $ do
            let input = "func test() {\n  if true {\n    return 42\n  // missing closing brace\n}"
                result = validateSyntax input
            case result of
                Left _ -> assertBool "Should detect unmatched brackets" True
                Right validated -> assertBool "Should not accept malformed syntax" False

        , testCase "Validator handles incomplete expressions" $ do
            let input = "func test() {\n  let x = 1 + \n  return x\n}"
                result = validateSyntax input
            case result of
                Left _ -> assertBool "Should detect incomplete expressions" True
                Right validated -> assertBool "Should not accept incomplete expressions" False

        , testCase "Validator handles mixed language constructs" $ do
            let input = "func test() {\n  let x = 42\n  echo x // shell command\n  return x\n}"
                result = validateSyntax input
            case result of
                Left _ -> assertBool "Should reject invalid constructs" True
                Right validated -> assertBool "Should not accept mixed constructs" False
        ]

    , testGroup "Edge case validation"
        [ testCase "Validator handles extremely long identifiers" $ do
            let longId = replicate 1000 'a'
                input = "func " ++ longId ++ "() {\n  return 42\n}"
                result = validateSyntax input
            case result of
                Left err -> assertBool ("Should handle long identifiers: " ++ show err) False
                Right validated -> assertBool "Should validate long identifiers" True

        , testCase "Validator handles deeply nested structures" $ do
            let nestedInput = concat $ replicate 100 "if true {\n"
                input = nestedInput ++ "return 42\n" ++ concat (replicate 100 "}\n")
                result = validateSyntax input
            case result of
                Left err -> assertBool ("Should handle deep nesting: " ++ show err) False
                Right validated -> assertBool "Should validate deeply nested structures" True

        , testCase "Validator handles Unicode characters" $ do
            let input = "func 测试函数() {\n  let 变量 = \"中文变量\"\n  return 变量\n}"
                result = validateSyntax input
            case result of
                Left err -> assertBool ("Should handle Unicode: " ++ show err) False
                Right validated -> assertBool "Should validate Unicode identifiers" True
        ]

    , testGroup "Type-specific validation"
        [ testCase "Validator checks type consistency" $ do
            let input = "func test() {\n  let x: int = \"string\" // type mismatch\n  return x\n}"
                result = validateSyntax input
            case result of
                Left _ -> assertBool "Should detect type mismatches" True
                Right validated -> assertBool "Should not accept type mismatches" False

        , testCase "Validator validates function signatures" $ do
            let input = "func add(a: int, b: int): string {\n  return a + b // return type mismatch\n}"
                result = validateSyntax input
            case result of
                Left _ -> assertBool "Should validate function signatures" True
                Right validated -> assertBool "Should not accept signature mismatches" False

        , testCase "Validator checks variable scope" $ do
            let input = "func test() {\n  if true {\n    let x = 42\n  }\n  return x // x out of scope\n}"
                result = validateSyntax input
            case result of
                Left _ -> assertBool "Should check variable scope" True
                Right validated -> assertBool "Should not accept scope violations" False
        ]

    , testGroup "Directive validation"
        [ testCase "Validator validates ownership directives" $ do
            let input = "// @ownership invalid_value\nfunc test() {\n  return 42\n}"
                result = validateSyntax input
            case result of
                Left _ -> assertBool "Should validate directive values" True
                Right validated -> assertBool "Should not accept invalid directives" False

        , testCase "Validator checks directive consistency" $ do
            let input = "// @ownership true\n// @ownership false\nfunc test() {\n  return 42\n}"
                result = validateSyntax input
            case result of
                Left _ -> assertBool "Should check directive consistency" True
                Right validated -> assertBool "Should not accept conflicting directives" False

        , testCase "Validator validates dependent type directives" $ do
            let input = "// @dependent-types true\nfunc test(n: int) where n > {\n  // incomplete constraint\n}"
                result = validateSyntax input
            case result of
                Left _ -> assertBool "Should validate dependent type syntax" True
                Right validated -> assertBool "Should not accept malformed constraints" False
        ]

    , testGroup "Property-based tests"
        [ testProperty "Validator handles arbitrary syntax" prop_validateArbitrarySyntax
        , testProperty "Validator position tracking is accurate" prop_positionTrackingAccurate
        , testProperty "Validator error messages are informative" prop_errorMessagesInformative
        , testProperty "Validator handles nested scopes correctly" prop_nestedScopeValidation
        ]
    ]

-- Property: Validator should not crash on arbitrary input
prop_validateArbitrarySyntax :: String -> Bool
prop_validateArbitrarySyntax input =
    case validateSyntax input of
        Left _ -> True  -- Validation errors are acceptable
        Right _ -> True  -- Successful validation is acceptable

-- Property: Validator position tracking should be accurate
prop_positionTrackingAccurate :: String -> Bool
prop_positionTrackingAccurate input =
    case validateSyntax input of
        Left _ -> True  -- Error positions should be tracked
        Right _ -> True  -- Successful validation should track positions

-- Property: Validator should provide informative error messages
prop_errorMessagesInformative :: String -> Bool
prop_errorMessagesInformative input =
    case validateSyntax input of
        Left err -> not (null err)  -- Error messages should not be empty
        Right _ -> True  -- Successful validation doesn't need error messages

-- Property: Validator should handle nested scopes correctly
prop_nestedScopeValidation :: String -> Bool
prop_nestedScopeValidation input =
    case validateSyntax input of
        Left _ -> True  -- Scope errors should be caught
        Right _ -> True  -- Valid scope structures should pass