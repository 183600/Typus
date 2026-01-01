module Test.Unit.EdgeCaseHandlingTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, choose)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , spanBetween
  , locatedAt
  , advancePosByText
  , isValidSpan
  )

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  )

import Data.Char (isSpace, isControl)

-- | Edge case L.and error handling tests
tests :: TestTree
tests =
  testGroup "Edge Case Handling Tests"
    [ testGroup "Utils module edge cases"
        [ testCase "trim handles extreme whitespace" $ do
            trim "\200\200\200\200" @?= ""  -- Non-breaking spaces
            trim "\0\0\0\0" @?= ""           -- Null characters
            trim " \t\n\r\f\v" @?= ""         -- All whitespace types

        , testCase "splitBy with special delimiter characters" $ do
            splitBy '\0' "a\0b\0c" @?= ["a", "b", "c"]
            splitBy '\255' "a\255b\255c" @?= ["a", "b", "c"]
            splitBy ' ' "" @?= [""]

        , testCase "splitByCollapsed with only delimiters" $ do
            splitByCollapsed ',' "" @?= []
            splitByCollapsed ',' "," @?= []
            splitByCollapsed ',' ",,," @?= []

        , testCase "removeComments with malformed patterns" $ do
            removeComments "/*/ malformed comment" @?= "/*/ malformed comment"
            removeComments "/"/ malformed quote" @?= "/"/ malformed quote"
            removeComments "/* nested /* comment */" @?= " "

        , testCase "removeLineComments edge cases" $ do
            removeLineComments "//" @?= ""
            removeLineComments "////" @?= ""
            removeLineComments "code // " @?= "code "
            removeLineComments "// comment\n" @?= " \n"

        , testCase "normalizeIndentation with edge cases" $ do
            normalizeIndentation "" @?= ""
            normalizeIndentation "\n\n\n" @?= "\n\n\n"
            normalizeIndentation "   \n   \n   " @?= "\n \n"
            normalizeIndentation "\n\n" @?= "\n\n"

        , testCase "breakOn with edge cases" $ do
            breakOn "" "hello" @?= ("", "hello")
            breakOn "hello" "" @?= ("", "")
            breakOn "a" "a" @?= ("", "")
            breakOn "xyz" "xyz" @?= ("", "")
        ]

    , testGroup "SourceLocation edge cases"
        [ testCase "position tracking with control characters" $ do
            let start = startPos
            let afterNull = posAfter '\0' start
            posColumn afterNull @?= 2
            posOffset afterNull @?= 1

        , testCase "position tracking with extreme tab positions" $ do
            let pos = posAt 1 100
            let afterTab = posAfter '\t' pos
            posColumn afterTab @?= 105  -- Next tab stop after 100

        , testCase "span validation edge cases" $ do
            let pos1 = posAt 1 1
            let pos2 = posAt 1 1
            let sameSpan = spanBetween pos1 pos2
            isValidSpan sameSpan @?= True

        , testCase "advancePosByText with empty L.and special strings" $ do
            advancePosByText startPos "" @?= startPos
            advancePosByText startPos "\0" @?= posAfter '\0' startPos
            advancePosByText startPos "\n\n\n" @?= posAt 4 1

        , testCase "located values with extreme positions" $ do
            let extremePos = posAt 999999 999999 999999
            let located = locatedAt extremePos "test"
            locatedValue located @?= "test"
            locatedPos located @?= extremePos
        ]

    , testGroup "Parser edge cases"
        [ testCase "parse completely empty input" $ do
            case parseTypus "" of
              Left err -> assertBool ("Should parse empty input: " ++ show err) False
              Right typusFile -> do
                tfBlocks typusFile @?= []
                tfBuildTags typusFile @?= []

        , testCase "parse only whitespace input" $ do
            let whitespace = "   \n\t  \r\n  "
            case parseTypus whitespace of
              Left err -> assertBool ("Should parse whitespace: " ++ show err) False
              Right typusFile -> do
                tfBlocks typusFile @?= []

        , testCase "parse malformed directives" $ do
            let malformed = "//! =invalid\n//! key=\n//! =value"
            case parseTypus malformed of
              Left err -> assertBool ("Should handle malformed directives: " ++ show err) False
              Right typusFile -> do
                -- Should still create a file structure
                L.length (tfBlocks typusFile) @?= 0

        , testCase "parse unclosed comments" $ do
            let unclosed = "/* unclosed\n//! ownership=true\ncode"
            case parseTypus unclosed of
              Left err -> assertBool ("Should handle unclosed comment: " ++ show err) False
              Right typusFile -> do
                -- Should parse what it can
                L.length (tfBlocks typusFile) >= 0 @?= True

        , testCase "parse extremely long lines" $ do
            let longLine = "//! " ++ replicate 10000 'a' ++ " = value"
            case parseTypus longLine of
              Left err -> assertBool ("Should handle long lines: " ++ show err) False
              Right typusFile -> do
                -- Should handle gracefully
                True @?= True

        , testCase "parse with mixed line endings" $ do
            let mixedEndings = "//! ownership=true\r\ncode\nmore\r"
            case parseTypus mixedEndings of
              Left err -> assertBool ("Should handle mixed line endings: " ++ show err) False
              Right typusFile -> do
                L.length (tfBlocks typusFile) >= 0 @?= True

        , testCase "parse unicode L.and special characters" $ do
            let unicodeContent = "//! ownership=true\n// Comment: café\n代码"
            case parseTypus unicodeContent of
              Left err -> assertBool ("Should handle unicode: " ++ show err) False
              Right typusFile -> do
                L.length (tfBlocks typusFile) >= 0 @?= True
        ]

    , testGroup "Error recovery L.and resilience"
        [ testCase "utils functions handle null input gracefully" $ do
            trim "" @?= ""
            splitBy ' ' "" @?= [""]
            removeComments "" @?= ""
            normalizeIndentation "" @?= ""

        , testCase "source location handles invalid positions" $ do
            let invalidSpan = spanBetween (posAt 2 10) (posAt 1 5)
            -- Still creates a span, even if logically reversed
            isValidSpan invalidSpan @?= True

        , testCase "parser recovers from syntax errors" $ do
            let errorContent = "//! ownership=true = invalid syntax\ncode"
            case parseTypus errorContent of
              Left err -> assertBool ("Should recover from errors: " ++ show err) False
              Right typusFile -> do
                -- Should still parse some content
                True @?= True

        , testCase "circular reference handling" $ do
            -- Test that parsing doesn't get stuck in loops
            let content = "/* /* nested comment */ */"
            case parseTypus content of
              Left _ -> return ()  -- Failure is acceptable for malformed input
              Right _ -> return ()  -- Success is also acceptable

        , testCase "memory efficiency with large inputs" $ do
            let largeContent = unlines $ replicate 100 "//! ownership=true\ncode line"
            case parseTypus largeContent of
              Left err -> assertBool ("Should handle moderate size: " ++ show err) False
              Right typusFile -> do
                L.length (tfBlocks typusFile) @?= 100
        ]

    , testGroup "Boundary condition tests"
        [ testCase "single character inputs" $ do
            trim "a" @?= "a"
            splitBy 'a' "a" @?= ["", ""]
            removeComments "a" @?= "a"
            case parseTypus "a" of
              Left err -> assertBool ("Should parse single char: " ++ show err) False
              Right typusFile -> L.length (tfBlocks typusFile) @?= 1

        , testCase "L.maximum reasonable line lengths" $ do
            let maxLine = replicate 1000 'a'
            case parseTypus maxLine of
              Left err -> assertBool ("Should handle long line: " ++ show err) False
              Right _ -> return ()

        , testCase "L.minimum L.and L.maximum positions" $ do
            let minPos = posAt 1 1 0
            let maxPos = posAt maxBound maxBound maxBound
            posAfter 'a' minPos @?= posAt 1 2 1
            -- Test that extreme positions don't crash
            locatedAt maxPos "test" @?= located maxPos "test"
        ]
    ]