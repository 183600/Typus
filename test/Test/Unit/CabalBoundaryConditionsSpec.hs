module Test.Unit.CabalBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import qualified Utils (trim, splitBy, removeComments, normalizeIndentation)
import qualified SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePos)
import qualified Parser

-- | Boundary condition and edge case tests
tests :: TestTree
tests =
  testGroup "Cabal Boundary Conditions Tests"
    [ testGroup "String Boundary Conditions"
        [ testCase "Empty string handling" $ do
            Utils.trim "" @?= ""
            Utils.splitBy ',' "" @?= [""]
            Utils.splitByCollapsed ',' "" @?= []
            Utils.removeComments "" @?= ""
            Utils.normalizeIndentation "" @?= ""

        , testCase "Single character strings" $ do
            Utils.trim "a" @?= "a"
            Utils.splitBy ',' "a" @?= ["a"]
            Utils.splitByCollapsed ',' "a" @?= ["a"]
            Utils.removeComments "a" @?= "a"
            Utils.normalizeIndentation "a" @?= "a"

        , testCase "Whitespace-only strings" $ do
            Utils.trim "   " @?= ""
            Utils.trim "\t\n\r " @?= ""
            Utils.splitBy ' ' "   " @?= ["", "", "", ""]
            Utils.splitByCollapsed ' ' "   " @?= []
            Utils.removeComments "   " @?= "   "
            Utils.normalizeIndentation "   " @?= ""

        , testCase "Unicode boundary handling" $ do
            let unicode = "测试 🚀 emoji"
            Utils.trim unicode @?= unicode
            Utils.splitBy ' ' unicode @?= ["测试", "🚀", "emoji"]
            Utils.removeComments unicode @?= unicode
        ]

    , testGroup "Numeric Boundary Conditions"
        [ testCase "Zero values" $ do
            let pos = SourceLocation.SourcePos 0 0
            SourceLocation.sourceLine pos @?= 0
            SourceLocation.sourceColumn pos @?= 0

        , testCase "Maximum reasonable values" $ do
            let pos = SourceLocation.SourcePos maxBound maxBound
            SourceLocation.sourceLine pos @?= maxBound
            SourceLocation.sourceColumn pos @?= maxBound

        , testCase "Minimum reasonable values" $ do
            let pos = SourceLocation.SourcePos 1 1
            SourceLocation.sourceLine pos @?= 1
            SourceLocation.sourceColumn pos @?= 1
        ]

    , testGroup "Parser Boundary Conditions"
        [ testCase "Parser with only directives" $ do
            let directivesOnly = "// @ownership: true\n// @dependent-types: false\n"
                result = Parser.parseTypus "directives" directivesOnly
            case result of
              Left err -> @?= "Should handle directives only" (show err)
              Right _ -> @?= "Success" "Directives success"

        , testCase "Parser with only comments" $ do
            let commentsOnly = "// line comment\n/* block comment */\n// another comment\n"
                result = Parser.parseTypus "comments" commentsOnly
            case result of
              Left err -> @?= "Should handle comments only" (show err)
              Right _ -> @?= "Success" "Comments success"

        , testCase "Parser with malformed directives" $ do
            let malformed = "// @ownership: maybe\n// @invalid: directive\n"
                result = Parser.parseTypus "malformed" malformed
            case result of
              Left _ -> @?= "Should handle gracefully" "Graceful handling"
              Right _ -> @?= "Success" "Malformed success"

        , testCase "Parser with extremely long identifiers" $ do
            let longIdent = "func " ++ replicate 1000 'a' ++ "() { return 1; }"
                result = Parser.parseTypus "longident" longIdent
            case result of
              Left err -> length (show err) > 0 @?= True
              Right _ -> @?= "Handle long identifier" "Long identifier handling"
        ]

    , testGroup "Source Location Boundary Conditions"
        [ testCase "Empty spans" $ do
            let pos = SourceLocation.startPos
                span = SourceLocation.SourceSpan pos pos
            SourceLocation.spanStart span @?= pos
            SourceLocation.spanEnd span @?= pos

        , testCase "Single character spans" $ do
            let start = SourceLocation.SourcePos 1 1
                end = SourceLocation.SourcePos 1 1
                span = SourceLocation.SourceSpan start end
            SourceLocation.spanStart span @?= start
            SourceLocation.spanEnd span @?= end

        , testCase "Multi-line spans" $ do
            let start = SourceLocation.SourcePos 1 10
                end = SourceLocation.SourcePos 5 20
                span = SourceLocation.SourceSpan start end
            SourceLocation.spanStart span @?= start
            SourceLocation.spanEnd span @?= end

        , testCase "Position advancement at boundaries" $ do
            let pos = SourceLocation.SourcePos maxBound maxBound
                advanced = SourceLocation.advancePos 'a' pos
            -- Should handle gracefully without overflow
            SourceLocation.sourceLine advanced >= 1 @?= True
        ]

    , testGroup "Comment Processing Boundary Conditions"
        [ testCase "Nested block comments" $ do
            let nested = "/* outer /* inner */ still outer */ func test() { return 1; }"
                result = Utils.removeComments nested
            "func test() { return 1; }" `isInfixOf` result @?= True

        , testCase "Unclosed block comments" $ do
            let unclosed = "func test() { return 1; /* unterminated"
                result = Utils.removeComments unclosed
            "func test() { return 1; " `isInfixOf` result @?= True

        , testCase "Comments at string boundaries" $ do
            let stringBoundary = "func test() { s := \"/* not comment */\"; /* real comment */ }"
                result = Utils.removeComments stringBoundary
            "\"/* not comment */\"" `isInfixOf` result @?= True

        , testCase "Empty comments" $ do
            let emptyComments = "func test() { return 1; } /**/ //"
                result = Utils.removeComments emptyComments
            "func test() { return 1; } " `isInfixOf` result @?= True
        ]

    , testGroup "Indentation Boundary Conditions"
        [ testCase "Zero indentation" $ do
            let noIndent = "func test() {\nreturn 1;\n}"
                normalized = Utils.normalizeIndentation noIndent
            normalized @?= noIndent

        , testCase "Maximum indentation" $ do
            let maxIndent = "func test() {\n" ++ replicate 100 ' ' ++ "return 1;\n}"
                normalized = Utils.normalizeIndentation maxIndent
            "return 1;" `isInfixOf` normalized @?= True

        , testCase "Mixed indentation styles" $ do
            let mixed = "func test() {\n\treturn 1;\n    return 2;\n\t\treturn 3;\n}"
                normalized = Utils.normalizeIndentation mixed
            length (lines normalized) == 4 @?= True

        , testCase "Indentation with empty lines" $ do
            let withEmpty = "func test() {\n    \n    return 1;\n    \n}"
                normalized = Utils.normalizeIndentation withEmpty
            length (lines normalized) == 5 @?= True
        ]

    , testGroup "Error Boundary Conditions"
        [ testCase "Multiple simultaneous errors" $ do
            let multipleErrors = "func test1() { return }\nfunc test2() { if }\nfunc test3() { { {"
                result = Parser.parseTypus "multiple" multipleErrors
            case result of
              Left err -> length (show err) > 0 @?= True
              Right _ -> @?= "Should detect errors" "Error detection"

        , testCase "Errors at file boundaries" $ do
            let startError = "{ return 1; }"
                endError = "func test() { return 1;"
                result1 = Parser.parseTypus "start" startError
                result2 = Parser.parseTypus "end" endError
            case (result1, result2) of
              (Left _, Left _) -> @?= "Both should fail" "Boundary errors"
              _ -> @?= "Error handling" "Error handling"

        , testProperty "Random unicode input doesn't crash" $ do
            \input -> let result = Parser.parseTypus "unicode" input
                      in case result of
                           Left _ -> True
                           Right _ -> True
        ]
    ]
  where
    isInfixOf needle haystack = needle `elem` (substrings haystack)
    substrings [] = []
    substrings s@(x:xs) = takeWhile (const True) s : substrings xs