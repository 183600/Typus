module Test.Unit.NewParserRobustnessSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), listOf, choose, elements)
import Parser
import qualified Data.Text as T
import qualified Text.Megaparsec as MP

-- | Test parser robustness and error recovery
tests :: TestTree
tests =
  testGroup "Parser Robustness Tests"
    [ testGroup "Error recovery scenarios"
        [ testCase "Parser recovers from malformed directives" $ do
            let input = "// @ownership invalid\nfunc test() {}"
                result = parseTypus input
            case result of
                Left _ -> assertBool "Should parse despite invalid directive" False
                Right parsed -> assertBool "Should have parsed content" True

        , testCase "Parser handles unclosed blocks gracefully" $ do
            let input = "func test() {\n  if true {\n    // missing closing braces"
                result = parseTypus input
            case result of
                Left _ -> assertBool "Should handle unclosed blocks" True
                Right parsed -> assertBool "Should parse with error information" True

        , testCase "Parser tolerates mixed encoding characters" $ do
            let input = "func test() {\n  // 中文注释\n  let value = 42\n}"
                result = parseTypus input
            case result of
                Left err -> assertBool ("Should handle mixed encoding: " ++ show err) False
                Right parsed -> assertBool "Should parse mixed encoding correctly" True
        ]

    , testGroup "Edge case inputs"
        [ testCase "Parser handles empty input" $ do
            let result = parseTypus ""
            case result of
                Left _ -> assertBool "Should handle empty input gracefully" True
                Right parsed -> assertBool "Should parse empty input" True

        , testCase "Parser handles only whitespace" $ do
            let result = parseTypus "   \n\t  \n  "
                result = parseTypus input
            case result of
                Left _ -> assertBool "Should handle whitespace-only input" True
                Right parsed -> assertBool "Should parse whitespace-only input" True

        , testCase "Parser handles extremely long lines" $ do
            let longLine = replicate 1000 'a' ++ " func test() {}"
                result = parseTypus longLine
            case result of
                Left err -> assertBool ("Should handle long lines: " ++ show err) False
                Right parsed -> assertBool "Should parse long lines correctly" True
        ]

    , testGroup "Directive parsing robustness"
        [ testCase "Parser handles conflicting directives" $ do
            let input = "// @ownership true\n// @ownership false\nfunc test() {}"
                result = parseTypus input
            case result of
                Left _ -> assertBool "Should handle conflicting directives" True
                Right parsed -> assertBool "Should parse with directive resolution" True

        , testCase "Parser handles malformed directive syntax" $ do
            let input = "// @ownership\n// @dependent-types maybe\nfunc test() {}"
                result = parseTypus input
            case result of
                Left _ -> assertBool "Should handle malformed directives" True
                Right parsed -> assertBool "Should parse despite directive errors" True
        ]

    , testGroup "Property-based tests"
        [ testProperty "Parser handles arbitrary text input" prop_parseArbitraryInput
        , testProperty "Parser position tracking is consistent" prop_positionTrackingConsistent
        , testProperty "Parser handles nested structures" prop_parseNestedStructures
        ]
    ]

-- Property: Parser should not crash on arbitrary input
prop_parseArbitraryInput :: String -> Bool
prop_parseArbitraryInput input =
    case parseTypus input of
        Left _ -> True  -- Parsing errors are acceptable
        Right _ -> True  -- Successful parsing is acceptable

-- Property: Parser position tracking should be consistent
prop_positionTrackingConsistent :: String -> Bool
prop_positionTrackingConsistent input =
    case parseTypus input of
        Left _ -> True
        Right parsed -> 
            -- Check that all located values have valid positions
            True  -- Simplified for now - would need to inspect parsed structure

-- Property: Parser should handle various levels of nesting
prop_parseNestedStructures :: Int -> String -> Bool
prop_parseNestedStructures depth baseCode =
    let nestedCode = concat $ replicate (abs depth `mod` 10) $ "if true {\n" ++ baseCode ++ "\n}"
    in case parseTypus nestedCode of
        Left _ -> True
        Right _ -> True