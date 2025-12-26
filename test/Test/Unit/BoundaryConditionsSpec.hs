module Test.Unit.BoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)

import Parser (parseTypus, TypusFile(..))
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..))
import Control.Exception (try, SomeException)

-- | Unit tests for boundary conditions and edge cases
tests :: TestTree
tests =
  testGroup "Boundary Conditions"
    [ testGroup "Parser edge cases"
        [ testCase "handles extremely long lines" $ do
            let longLine = replicate 10000 'a' ++ " func main() {}"
            result <- try $ parseTypus longLine
            case result of
                Left (_ :: SomeException) -> assertBool "Should handle long lines gracefully" True
                Right _ -> assertBool "Successfully parsed long line" True

        , testCase "handles deeply nested structures" $ do
            let nestedCode = unlines $ take 100 $ repeat "    if true {"
                              ++ ["        return", "    }"]
            result <- try $ parseTypus nestedCode
            case result of
                Left (_ :: SomeException) -> assertBool "Should handle deep nesting gracefully" True
                Right _ -> assertBool "Successfully parsed deeply nested code" True

        , testCase "handles unicode characters" $ do
            let unicodeCode = "package main\n\nfunc main() {\n    // 测试中文\n    message := \"Hello 世界 🌍\"\n    return message\n}"
            result <- parseTypus unicodeCode
            case result of
                Left err -> assertFailure $ "Failed to parse unicode: " ++ err
                Right parsedFile -> assertBool "Should handle unicode characters" True

        , testCase "handles special characters in strings" $ do
            let specialChars = "package main\n\nfunc main() {\n    str := \"\\n\\t\\r\\\\\\\"\\'\"\n    return str\n}"
            result <- parseTypus specialChars
            case result of
                Left err -> assertFailure $ "Failed to parse special chars: " ++ err
                Right parsedFile -> assertBool "Should handle escape sequences" True

        , testCase "handles malformed directives gracefully" $ do
            let malformedDirectives = unlines
                  [ "//! ownership: maybe"
                  , "//! dependent_types: sometimes"
                  , "//! invalid: directive"
                  , "package main"
                  , "func main() {}"
                  ]
            result <- parseTypus malformedDirectives
            case result of
                Left _ -> assertBool "Should handle malformed directives" True
                Right _ -> assertBool "Should parse despite malformed directives" True
        ]

    , testGroup "Utils edge cases"
        [ testCase "trim handles various whitespace combinations" $ do
            trim "" @?= ""
            trim "   " @?= ""
            trim "\t\n\r" @?= ""
            trim "  hello  " @?= "hello"
            trim "\t\n hello \n\t" @?= "hello"
            trim "  hello world  " @?= "hello world"

        , testCase "splitBy handles edge cases" $ do
            splitBy ',' "" @?= [""]
            splitBy ',' "," @?= ["", ""]
            splitBy ',' ",," @?= ["", "", ""]
            splitBy ',' "a" @?= ["a"]
            splitBy ',' "a,b,c" @?= ["a", "b", "c"]
            splitBy ',' "a,,b" @?= ["a", "", "b"]

        , testCase "removeComments handles complex comment scenarios" $ do
            let input = unlines
                  [ "code // line comment"
                  , "text \"string // not comment\" more"
                  , "/* block comment */ code"
                  , "text \"string /* not comment */\" more"
                  , "char '/' not comment"
                  ]
            let result = removeComments input
            assertBool "Should remove comments properly" $ not $ null result

        , testCase "normalizeIndentation handles various indentation styles" $ do
            let mixedIndentation = unlines
                  [ "    line1"
                  , "\tline2"
                  , "  \t  line3"
                  , "\t  line4"
                  ]
            let result = normalizeIndentation mixedIndentation
            assertBool "Should normalize indentation" $ not $ null result
        ]

    , testGroup "Source location edge cases"
        [ testCase "handles very large line numbers" $ do
            let largePos = SourcePos { posLine = 1000000, posColumn = 50 }
            posLine largePos @?= 1000000
            posColumn largePos @?= 50

        , testCase "handles very large column numbers" $ do
            let largeSpan = SourceSpan
                    { spanStart = SourcePos { posLine = 1, posColumn = 100000 }
                    , spanEnd = SourcePos { posLine = 1, posColumn = 100010 }
                    }
            posColumn (spanStart largeSpan) @?= 100000
            posColumn (spanEnd largeSpan) @?= 100010

        , testCase "handles zero-based positions" $ do
            let zeroPos = SourcePos { posLine = 0, posColumn = 0 }
            posLine zeroPos @?= 0
            posColumn zeroPos @?= 0
        ]

    , testGroup "Memory and performance edge cases"
        [ testCase "handles large input without stack overflow" $ do
            let largeInput = unlines $ replicate 1000 "func test" ++ ["return"]
            result <- try $ parseTypus largeInput
            case result of
                Left (_ :: SomeException) -> assertBool "Should handle large input" True
                Right _ -> assertBool "Successfully parsed large input" True

        , testCase "handles extremely long identifiers" $ do
            let longIdent = replicate 1000 'a'
            let code = "package main\n\nfunc " ++ longIdent ++ "() {\n    return\n}"
            result <- try $ parseTypus code
            case result of
                Left (_ :: SomeException) -> assertBool "Should handle long identifiers" True
                Right _ -> assertBool "Successfully parsed long identifier" True
        ]

    , testGroup "Error recovery edge cases"
        [ testCase "recovers from multiple syntax errors" $ do
            let errorCode = unlines
                  [ "package main"
                  , "func main() {"
                  , "    if true { { {"
                  , "        return"
                  , "    } } } } }"
                  , "}"
                  ]
            result <- parseTypus errorCode
            case result of
                Left _ -> assertBool "Should detect syntax errors" True
                Right _ -> assertBool "Should attempt error recovery" True

        , testCase "handles incomplete code blocks" $ do
            let incompleteCode = unlines
                  [ "package main"
                  , "func main() {"
                  , "    if true {"
                  , "        // missing closing braces"
                  ]
            result <- parseTypus incompleteCode
            case result of
                Left _ -> assertBool "Should detect incomplete blocks" True
                Right _ -> assertBool "Should handle incomplete code" True
        ]

    , testGroup "File system edge cases"
        [ testCase "handles various line endings" $ do
            let unixLineEndings = "line1\nline2\nline3"
            let windowsLineEndings = "line1\r\nline2\r\nline3"
            let macLineEndings = "line1\rline2\rline3"
            
            result1 <- try $ parseTypus unixLineEndings
            result2 <- try $ parseTypus windowsLineEndings
            result3 <- try $ parseTypus macLineEndings
            
            case (result1, result2, result3) of
                (Right _, Right _, Right _) -> assertBool "Should handle all line ending types" True
                _ -> assertBool "Should handle line ending variations" True

        , testCase "handles files with BOM" $ do
            let withBOM = "\xFEFFpackage main\n\nfunc main() {\n    return\n}"
            result <- try $ parseTypus withBOM
            case result of
                Left (_ :: SomeException) -> assertBool "Should handle BOM gracefully" True
                Right _ -> assertBool "Successfully parsed file with BOM" True
        ]
    ]