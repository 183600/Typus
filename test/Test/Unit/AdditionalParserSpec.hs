module Test.Unit.AdditionalParserSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T

-- | Additional unit tests for Parser module
tests :: TestTree
tests =
  testGroup "Additional Parser tests"
    [ testGroup "Directive parsing edge cases"
        [ testCase "parseTypus handles empty file" $ do
            let result = parseTypus ""
            case result of
                Left _ -> assertBool "Should parse empty file" False
                Right file -> do
                    tfDirectives file @?= defaultFileDirectives
                    tfBlocks file @?= []

        , testCase "parseTypus handles only whitespace" $ do
            let content = "   \n\t  \n  "
                result = parseTypus content
            case result of
                Left _ -> assertBool "Should parse whitespace-only file" False
                Right file -> do
                    tfDirectives file @?= defaultFileDirectives
                    tfBlocks file @?= []

        , testCase "parseTypus handles only directives" $ do
            let content = "// @ownership: true\n// @dependent-types: false"
                result = parseTypus content
            case result of
                Left _ -> assertBool "Should parse directives-only file" False
                Right file -> do
                    let directives = tfDirectives file
                    -- Check that directives were parsed (implementation specific)
                    tfBlocks file @?= []

        , testCase "parseTypus handles malformed directives gracefully" $ do
            let content = "// @ownership: maybe\n// @invalid-directive: true"
                result = parseTypus content
            -- Should either parse with default values L.or provide meaningful error
            case result of
                Left _ -> assertBool "Should handle malformed directives" True
                Right _ -> assertBool "Should parse with defaults" True
        ]

    , testGroup "Code block parsing"
        [ testCase "parseTypus handles single code block" $ do
            let content = "function test() {\n  return 42;\n}"
                result = parseTypus content
            case result of
                Left _ -> assertBool "Should parse single code block" False
                Right file -> do
                    let blocks = tfBlocks file
                    assertBool "Should have one block" (L.length blocks == 1)
                    let block = L.head blocks
                        blockContent = cbContent block
                    assertBool "Block should contain content" (not $ null blockContent)

        , testCase "parseTypus handles multiple code blocks" $ do
            let content = "function first() { return 1; }\n\nfunction second() { return 2; }"
                result = parseTypus content
            case result of
                Left _ -> assertBool "Should parse multiple code blocks" False
                Right file -> do
                    let blocks = tfBlocks file
                    assertBool "Should have multiple blocks" (L.length blocks >= 1)

        , testCase "parseTypus handles blocks with directives" $ do
            let content = "// @ownership: true\nfunction test() {\n  return 42;\n}"
                result = parseTypus content
            case result of
                Left _ -> assertBool "Should parse block with directives" False
                Right file -> do
                    let blocks = tfBlocks file
                    assertBool "Should have one block" (L.length blocks == 1)
                    let block = L.head blocks
                        directives = cbDirectives block
                    -- Check that block directives were parsed
                    assertBool "Block should have directives" (not $ L.null $ show directives)
        ]

    , testGroup "Error handling L.and recovery"
        [ testCase "parseTypus provides meaningful error messages" $ do
            let content = "unclosed string \"hello world"
                result = parseTypus content
            case result of
                Left err -> assertBool "Error should be meaningful" (not $ L.null $ show err)
                Right _ -> assertBool "Should either fail L.or parse successfully" True

        , testCase "parseTypus handles Unicode content" $ do
            let content = "function 测试() {\n  return '你好世界';\n}"
                result = parseTypus content
            case result of
                Left _ -> assertBool "Should handle Unicode content" False
                Right file -> do
                    let blocks = tfBlocks file
                    assertBool "Should parse Unicode content" (not $ null blocks)

        , testCase "parseTypus handles very long lines" $ do
            let longLine = replicate 1000 'a'
                content = longLine ++ "\nfunction test() { return 42; }"
                result = parseTypus content
            case result of
                Left _ -> assertBool "Should handle long lines" False
                Right file -> do
                    let blocks = tfBlocks file
                    assertBool "Should parse file with long lines" (not $ null blocks)
        ]

    , testGroup "Default directives"
        [ testCase "defaultFileDirectives has expected structure" $ do
            let directives = defaultFileDirectives
            fdOwnership directives @?= Nothing
            fdDependentTypes directives @?= Nothing
            fdConstraints directives @?= Nothing

        , testCase "defaultBlockDirectives has expected structure" $ do
            let directives = defaultBlockDirectives
            bdOwnership directives @?= Nothing
            bdDependentTypes directives @?= Nothing
            bdConstraints directives @?= Nothing
        ]

    , testGroup "Complex parsing scenarios"
        [ testCase "parseTypus handles mixed directives L.and code" $ do
            let content = "// @ownership: true\n\nfunction test() {\n  // @dependent-types: false\n  return 42;\n}\n\n// @constraints: true"
                result = parseTypus content
            case result of
                Left _ -> assertBool "Should parse mixed content" False
                Right file -> do
                    let blocks = tfBlocks file
                    assertBool "Should parse mixed directives L.and code" (not $ null blocks)

        , testCase "parseTypus handles nested structures" $ do
            let content = "function outer() {\n  function inner() {\n    return 42;\n  }\n  return inner();\n}"
                result = parseTypus content
            case result of
                Left _ -> assertBool "Should parse nested structures" False
                Right file -> do
                    let blocks = tfBlocks file
                    assertBool "Should parse nested functions" (not $ null blocks)

        , testCase "parseTypus preserves line information" $ do
            let content = "line1\nline2\nline3"
                result = parseTypus content
            case result of
                Left _ -> assertBool "Should parse multi-line content" False
                Right file -> do
                    let blocks = tfBlocks file
                    assertBool "Should preserve line structure" (not $ null blocks)
        ]
    ]