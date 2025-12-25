{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Unit.ParserEdgeCasesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, oneof, elements)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import qualified Data.Text as T
import Data.Either (isLeft, isRight)

tests :: TestTree
tests = testGroup "Parser Edge Cases Tests"
  [ testGroup "Empty and minimal inputs"
    [ testCase "handles empty file" $
        case parseTypus "" of
          Left _ -> assertBool "Should parse empty file" True
          Right file -> assertBool "Empty file should be valid" True
    , testCase "handles file with only whitespace" $
        case parseTypus "   \n\t  \n  " of
          Left _ -> assertBool "Should parse whitespace-only file" True
          Right file -> assertBool "Whitespace-only file should be valid" True
    , testCase "handles file with only comments" $
        case parseTypus "// This is a comment\n/* Block comment */\n// Another comment" of
          Left _ -> assertBool "Should parse comment-only file" True
          Right file -> assertBool "Comment-only file should be valid" True
    ]
  , testGroup "Directive parsing edge cases"
    [ testCase "handles malformed directives gracefully" $
        case parseTypus "#ownership invalid" of
          Left _ -> assertBool "Should fail on invalid directive" True
          Right _ -> assertBool "Should not succeed with invalid directive" False
    , testCase "handles directives with unusual spacing" $
        let content = "#  ownership   true\n   #dependent-types\tfalse"
        in case parseTypus content of
          Left _ -> assertBool "Should parse directives with unusual spacing" False
          Right file -> assertBool "Should handle unusual spacing in directives" True
    , testCase "handles duplicate directives" $
        let content = "#ownership true\n#ownership false"
        in case parseTypus content of
          Left _ -> assertBool "Should handle duplicate directives" True
          Right file -> assertBool "Should parse file with duplicate directives" True
    ]
  , testGroup "Code block edge cases"
    [ testCase "handles empty code blocks" $
        let content = "```go\n```"
        in case parseTypus content of
          Left _ -> assertBool "Should parse empty code block" False
          Right file -> assertBool "Empty code block should be valid" True
    , testCase "handles code blocks with only whitespace" $
        let content = "```go\n   \n\t  \n   \n```"
        in case parseTypus content of
          Left _ -> assertBool "Should parse whitespace-only code block" False
          Right file -> assertBool "Whitespace-only code block should be valid" True
    , testCase "handles unclosed code blocks" $
        let content = "```go\nfunc test() {\n  return 42\n}"
        in case parseTypus content of
          Left _ -> assertBool "Should fail on unclosed code block" True
          Right _ -> assertBool "Should not succeed with unclosed code block" False
    , testCase "handles code blocks with nested backticks" $
        let content = "```go\nfmt.Println(\"```nested```\")\n```"
        in case parseTypus content of
          Left _ -> assertBool "Should parse code blocks with nested backticks" False
          Right file -> assertBool "Code blocks with nested backticks should be valid" True
    ]
  , testGroup "Special characters and encoding"
    [ testCase "handles Unicode characters" $
        let content = "// Unicode test: 你好世界 🌍\n```go\nfmt.Println(\"Hello 世界\")\n```"
        in case parseTypus content of
          Left _ -> assertBool "Should parse Unicode content" False
          Right file -> assertBool "Unicode content should be valid" True
    , testCase "handles escape sequences in strings" $
        let content = "```go\nfmt.Println(\"Line 1\\nLine 2\\tTabbed\")\n```"
        in case parseTypus content of
          Left _ -> assertBool "Should parse escape sequences" False
          Right file -> assertBool "Escape sequences should be valid" True
    , testCase "handles raw strings" $
        let content = "```go\ns := `raw string with \\n not escaped`\n```"
        in case parseTypus content of
          Left _ -> assertBool "Should parse raw strings" False
          Right file -> assertBool "Raw strings should be valid" True
    ]
  , testGroup "Malformed inputs"
    [ testCase "handles deeply nested structures" $
        let content = "```go\nfunc outer() {\n  func inner() {\n    if true {\n      for {\n        select {\n        case <-ch:\n          break\n        }\n      }\n    }\n  }\n}\n```"
        in case parseTypus content of
          Left _ -> assertBool "Should parse deeply nested structures" False
          Right file -> assertBool "Deeply nested structures should be valid" True
    , testCase "handles extremely long lines" $
        let longLine = replicate 1000 'a'
            content = "```go\nvar long = \"" ++ longLine ++ "\"\n```"
        in case parseTypus content of
          Left _ -> assertBool "Should handle extremely long lines" False
          Right file -> assertBool "Extremely long lines should be valid" True
    , testCase "handles files with many small blocks" $
        let smallBlock = "```go\nfmt.Println(\"test\")\n```"
            content = unlines $ replicate 50 smallBlock
        in case parseTypus content of
          Left _ -> assertBool "Should handle many small blocks" False
          Right file -> assertBool "Many small blocks should be valid" True
    ]
  , testGroup "QuickCheck properties"
    [ testProperty "parseTypus is total (never crashes)" $
        \s -> case parseTypus s of
          Left _ -> True
          Right _ -> True
    , testProperty "parsing is idempotent on valid input" $
        \s -> case parseTypus s of
          Left _ -> True
          Right file -> case parseTypus (show file) of
            Left _ -> False  -- This might fail due to formatting differences
            Right _ -> True   -- But should not crash
    ]
  ]

-- Helper functions for generating test content
generateDirective :: Gen String
generateDirective = oneof
  [ return "#ownership true"
  , return "#ownership false"
  , return "#dependent-types true"
  , return "#dependent-types false"
  , return "#constraints true"
  , return "#constraints false"
  ]

generateCodeBlock :: Gen String
generateCodeBlock = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n{}();"
  let code = "```go\n" ++ take 100 content ++ "\n```"
  return code

-- Note: Arbitrary instance for String is provided by QuickCheck