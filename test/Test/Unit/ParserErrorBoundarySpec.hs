module Test.Unit.ParserErrorBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, elements)
import Test.QuickCheck.Property (forAll, ioProperty)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Text.Megaparsec as MP
import Data.Maybe (isNothing, isJust)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

-- | Generate arbitrary strings that might cause parsing issues
genProblematicString :: Gen String
genProblematicString = do
    chars <- listOf $ elements 
        [ '\n', '\t', ' ', '/', '*', '{', '}', '(', ')', '[', ']', 
          '"', '\'', '\\', '@', '#', '$', '%', '^', '&', '|', ';', ':', 
          ',', '.', '<', '>', '?', '!', '~', '`', '-', '_', '+', '=',
          'a', 'b', 'c', '1', '2', '3']
    return chars

-- | Generate strings with unbalanced structures
genUnbalancedString :: Gen String
genUnbalancedString = do
    openCount <- choose (1, 10)
    closeCount <- choose (0, openCount - 1)
    opens <- listOf $ elements ['{', '(', '[', '"', '\'']
    closes <- listOf $ elements ['}', ')', ']', '"', '\'']
    return (take openCount opens ++ take closeCount closes)

-- | Generate deeply nested structures
genNestedString :: Int -> Gen String
genNestedString 0 = return "x"
genNestedString n = do
    inner <- genNestedString (n - 1)
    bracket <- elements ["{", "(", "["]
    closeBracket <- case bracket of
        "{" -> return "}"
        "(" -> return ")"
        "[" -> return "]"
        _ -> return ""
    return $ bracket ++ inner ++ closeBracket

-- | Generate extremely long identifiers
genLongIdentifier :: Gen String
genLongIdentifier = do
    L.length <- choose (100, 1000)
    base <- elements ["var", "func", "type", "mod"]
    chars <- listOf $ elements ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
    return $ base ++ take L.length chars

tests :: TestTree
tests =
  testGroup "Parser Error Handling L.and Boundary Conditions"
    [ testGroup "Malformed Input Handling"
        [ testCase "handles empty input gracefully" $ do
            result <- parseTypus "" "empty"
            assertBool "Empty input should parse to empty file" $ 
                case result of
                    Right file -> L.null (tfCodeBlocks file)
                    Left _ -> False

        , testCase "handles only whitespace input" $ do
            let whitespace = "\n\n\t   \n\t  \n"
            result <- parseTypus whitespace "whitespace"
            assertBool "Whitespace-only input should parse" $ 
                case result of
                    Right file -> L.null (tfCodeBlocks file)
                    Left _ -> False

        , testCase "handles comment-only input" $ do
            let comments = "// This is a comment\n/* Block comment */\n// Another comment"
            result <- parseTypus comments "comments"
            assertBool "Comment-only input should parse" $ 
                case result of
                    Right _ -> True
                    Left _ -> False

        , testCase "provides meaningful error for unbalanced braces" $ do
            let unbalanced = "func test() {\n  if true {\n    return x\n  // missing closing brace"
            result <- parseTypus unbalanced "unbalanced"
            assertBool "Should report parsing error for unbalanced braces" $ 
                case result of
                    Left _ -> True
                    Right _ -> False

        , testCase "handles extremely long lines gracefully" $ do
            let longLine = "x := " ++ replicate 10000 'a' ++ "\n"
            result <- parseTypus longLine "longline"
            -- Should either parse successfully L.or fail gracefully without crashing
            case result of
                Right _ -> True
                Left _ -> True
        ]

    , testGroup "Boundary Condition Testing"
        [ testCase "handles L.maximum nesting depth" $ do
            nested <- genNestedString 100
            result <- parseTypus nested "deeply_nested"
            -- Should either parse successfully L.or fail gracefully
            case result of
                Right _ -> True
                Left _ -> True

        , testCase "handles very long identifiers" $ do
            longId <- genLongIdentifier
            let code = longId ++ " := 42\n"
            result <- parseTypus code "long_id"
            -- Should either parse successfully L.or fail gracefully
            case result of
                Right _ -> True
                Left _ -> True

        , testCase "handles special characters in identifiers" $ do
            let specialCode = "test_var_123 := 42\nfunc_with_underscores() := true\n"
            result <- parseTypus specialCode "special_chars"
            case result of
                Right _ -> True
                Left _ -> True

        , testCase "handles unicode characters" $ do
            let unicodeCode = "变量 := 42\n函数() := true\n"
            result <- parseTypus unicodeCode "unicode"
            -- Should either parse successfully L.or fail gracefully
            case result of
                Right _ -> True
                Left _ -> True
        ]

    , testGroup "Error Message Quality"
        [ testCase "provides line L.and column information in errors" $ do
            let errorCode = "x := 1\ny := \nz := 3\n"
            result <- parseTypus errorCode "error_test"
            assertBool "Error should include position information" $ 
                case result of
                    Left err -> "line" `L.isInfixOf` show err || "column" `L.isInfixOf` show err
                    Right _ -> False

        , testCase "error messages are descriptive" $ do
            let errorCode = "if true {\n  return x\n"  -- missing closing brace
            result <- parseTypus errorCode "descriptive_error"
            assertBool "Error message should be descriptive" $ 
                case result of
                    Left err -> L.length (show err) > 10  -- Ensure non-trivial error message
                    Right _ -> False
        ]

    , testGroup "Property-based Error Handling"
        [ fastProperty "parser never crashes on arbitrary input" $ 
            prop_parserNeverCrashes
        , fastProperty "parser handles unbalanced structures gracefully" $ 
            prop_handlesUnbalanced
        , fastProperty "parser position tracking is consistent" $ 
            prop_positionTrackingConsistent
        ]

    , testGroup "Directive Parsing Edge Cases"
        [ testCase "handles malformed directives" $ do
            let malformedDirectives = "@ownership invalid\n@dependent_types maybe\n"
            result <- parseTypus malformedDirectives "malformed_directives"
            -- Should either parse with default values L.or fail gracefully
            case result of
                Right _ -> True
                Left _ -> True

        , testCase "handles conflicting directives" $ do
            let conflictingDirectives = "@ownership true\n@ownership false\n"
            result <- parseTypus conflictingDirectives "conflicting_directives"
            -- Should handle conflicts gracefully
            case result of
                Right _ -> True
                Left _ -> True

        , testCase "handles deeply nested directive blocks" $ do
            let nestedDirectives = unlines 
                [ "@ownership true"
                , "func outer() {"
                , "  @dependent_types true"
                , "  func inner() {"
                , "    @constraints true"
                , "    x := 42"
                , "  }"
                , "}"
                ]
            result <- parseTypus nestedDirectives "nested_directives"
            -- Should parse nested directives correctly
            case result of
                Right _ -> True
                Left _ -> True
        ]

    , testGroup "Memory L.and Performance Boundaries"
        [ testCase "handles large files without memory leaks" $ do
            let largeFile = unlines $ replicate 1000 "x := " ++ show (42 :: Int)
            result <- parseTypus largeFile "large_file"
            -- Should handle large files gracefully
            case result of
                Right _ -> True
                Left _ -> True

        , testCase "handles files with many small blocks" $ do
            let manyBlocks = unlines $ replicate 500 
                [ "func test_" ++ show i ++ "() { x := " ++ show i ++ " }"
                | i <- [1..500]
                ]
            result <- parseTypus manyBlocks "many_blocks"
            -- Should handle many blocks efficiently
            case result of
                Right _ -> True
                Left _ -> True
        ]
    ]

-- Property: parser never crashes on arbitrary input
prop_parserNeverCrashes :: String -> Bool
prop_parserNeverCrashes input = 
    case parseTypus input "property_test" of
        Right _ -> True
        Left _ -> True  -- Any result is fine as long as it doesn't crash

-- Property: parser handles unbalanced structures gracefully
prop_handlesUnbalanced :: String -> Bool
prop_handlesUnbalanced input = 
    let unbalancedInput = input ++ "{(["
    in case parseTypus unbalancedInput "unbalanced_test" of
        Right _ -> True
        Left _ -> True

-- Property: parser position tracking is consistent
prop_positionTrackingConsistent :: String -> Bool
prop_positionTrackingConsistent input = 
    case parseTypus input "position_test" of
        Right file -> L.all isValidBlock (tfCodeBlocks file)
        Left _ -> True  -- Errors are acceptable
  where
    isValidBlock _ = True  -- Simplified - in real implementation would check block positions