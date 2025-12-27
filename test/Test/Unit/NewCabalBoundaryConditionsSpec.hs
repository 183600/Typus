{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)

import qualified Data.Text as T
import Data.Char (isSpace, isLetter, isDigit, isPunctuation, ord)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , advancePosBy
  , advancePosByText
  , emptySpan
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , mapLocated
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

-- | Boundary condition tests for edge cases and error scenarios
tests :: TestTree
tests =
  testGroup "NewCabalBoundaryConditions"
    [ testGroup "SourceLocation boundary cases"
        [ testCase "position with negative coordinates should handle gracefully" $ do
            -- Test handling of edge case positions
            let pos = SourcePos (-1) (-1) (-1)
                advanced = posAfter 'a' pos
            -- Should still advance line/column even with negative start
            posLine advanced @?= (-1)
            posColumn advanced @?= 0
            posOffset advanced @?= 0

        , testCase "span with reversed positions should be invalid" $ do
            let start = SourcePos 5 10 20
                end = SourcePos 3 5 10
                span = spanBetween start end
            isValidSpan span @?= False

        , testCase "empty text advancement" $ do
            let pos = advancePosBy "" startPos
            pos @?= startPos

        , testCase "very large line numbers" $ do
            let largePos = SourcePos 999999 100 10000
                span = emptySpan largePos
            spanStart span @?= largePos
            spanEnd span @?= largePos

        , testCase "position advancement with only newlines" $ do
            let text = "\n\n\n\n\n"
                finalPos = advancePosBy text startPos
            posLine finalPos @?= 6
            posColumn finalPos @?= 1
            posOffset finalPos @?= 5

        , testCase "position advancement with only tabs" $ do
            let text = "\t\t\t\t\t"
                finalPos = advancePosBy text startPos
            posLine finalPos @?= 1
            posColumn finalPos @?= 41  -- Each tab jumps to next 8-column boundary
            posOffset finalPos @?= 5
        ]

    , testGroup "Utils string boundary cases"
        [ testCase "trim with only whitespace" $ do
            let input = "   \t\n\r   \t\n\r   "
                result = trim input
            result @?= ""

        , testCase "trim with null bytes and control characters" $ do
            let input = "\0\1\2\3 content \4\5\6\7"
                result = trim input
            result @?= "\0\1\2\3 content \4\5\6\7"

        , testCase "splitBy with empty string" $ do
            splitBy ',' "" @?= [""]

        , testCase "splitBy with delimiter at boundaries" $ do
            splitBy ',' ",start,end," @?= ["", "start", "end", ""]

        , testCase "splitBy with all delimiters" $ do
            splitBy ',' ",,," @?= ["", "", "", "", ""]

        , testCase "splitByCollapsed with all delimiters" $ do
            splitByCollapsed ',' ",,," @?= []

        , testCase "removeComments with malformed block comments" $ do
            let input = "code /* unclosed comment"
                result = removeComments input
            result @?= "code "

        , testCase "removeComments with nested block comment patterns" $ do
            let input = "code /* outer /* inner */ still outer */ end"
                result = removeComments input
            result @?= "code  end"

        , testCase "removeLineComments with comment at EOF without newline" $ do
            let input = "code // comment at end"
                result = removeLineComments input
            result @?= "code "

        , testCase "normalizeIndentation with only empty lines" $ do
            let input = "\n\n\n\n"
                result = normalizeIndentation input
            result @?= input

        , testCase "normalizeIndentation with mixed tab/space indentation" $ do
            let input = "\tline1\n  \tline2\n\t  line3"
                result = normalizeIndentation input
                lines' = lines result
            length lines' @?= 3

        , testCase "breakOn with empty pattern" $ do
            let (before, after) = breakOn "" "content"
            before @?= ""
            after @?= "content"

        , testCase "breakOn with pattern longer than text" $ do
            let (before, after) = breakOn "longpattern" "short"
            before @?= "short"
            after @?= ""
        ]

    , testGroup "Unicode and internationalization edge cases"
        [ testCase "Unicode whitespace handling in trim" $ do
            let input = "\160\128\129content\130\131"
                result = trim input
            result @?= "\160\128\129content\130\131"  -- These are not ASCII whitespace

        , testCase "Unicode content in splitBy" $ do
            let input = "测试,Hello,世界,World,🚀"
                parts = splitBy ',' input
            parts @?= ["测试", "Hello", "世界", "World", "🚀"]

        , testCase "Unicode comments in removeComments" $ do
            let input = "var x = 42; // 这是注释\nvar y = \"// 不是注释\";"
                result = removeComments input
            result @?= "var x = 42; \nvar y = \"// 不是注释\";"

        , testCase "Multi-byte character position tracking" $ do
            let text = "🚀🌟💫\n测试"
                finalPos = advancePosByText (T.pack text) startPos
            posLine finalPos @?= 2
            posColumn finalPos @?= 3  -- Each emoji counts as 1 column
            posOffset finalPos @?= 7   -- But 3 chars + 1 newline = 4 bytes offset
        ]

    , testGroup "Performance and memory boundary cases"
        [ testCase "very long single line" $ do
            let longLine = replicate 100000 'a'
                finalPos = advancePosBy longLine startPos
            posLine finalPos @?= 1
            posColumn finalPos @?= 100001
            posOffset finalPos @?= 100000

        , testCase "many short lines" $ do
            let manyLines = unlines $ replicate 10000 "x"
                finalPos = advancePosByText (T.pack manyLines) startPos
            posLine finalPos @?= 10001
            posColumn finalPos @?= 1

        , testCase "deep nesting of comments" $ do
            let nested = concat $ replicate 1000 "/*"
                content = nested ++ "content" ++ concat (replicate 1000 "*/")
                result = removeComments content
            result @?= "content"

        , testCase "extreme indentation levels" $ do
            let deepIndent = concat $ replicate 1000 "  "
                content = deepIndent ++ "content"
                result = normalizeIndentation content
            result @?= "content"
        ]

    , testGroup "Error recovery and robustness"
        [ testCase "malformed escape sequences in comments" $ do
            let input = "code /* comment with \\x unbalanced */ more"
                result = removeComments input
            result @?= "code  more"

        , testCase "mixed line endings" $ do
            let input = "line1\r\nline2\nline3\rline4"
                result = normalizeIndentation input
                lines' = lines result
            length lines' @?= 4

        , testCase "null characters in various contexts" $ do
            let input = "before\0middle\0after"
                parts = splitBy '\0' input
            parts @?= ["before", "middle", "after"]

        , testCase "very long identifiers" $ do
            let longId = replicate 10000 'a'
                content = "var " ++ longId ++ " = 42;"
                result = removeComments content
            result @?= content

        , testCase "binary data mixed with text" $ do
            let binary = map chr [0..255] ++ "text" ++ map chr [128..255]
                result = trim binary
            length result @?= length binary
          where chr = toEnum . fromEnum
        ]

    , testGroup "Consistency and invariant tests"
        [ testCase "position advancement consistency" $ do
            let text = "hello\nworld"
                pos1 = advancePosBy text startPos
                pos2 = advancePosBy "hello" startPos
                pos3 = posAfter '\n' pos2
                pos4 = advancePosBy "world" pos3
            pos1 @?= pos4

        , testCase "span merging identity" $ do
            let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
                merged = mergeSpans span span
            merged @?= span

        , testCase "split and join roundtrip with special chars" $ do
            let input = "a,b,,c,\n,d"
                parts = splitBy ',' input
                rejoined = L.intercalate "," parts
            rejoined @?= input

        , testCase "trim idempotency with complex input" $ do
            let input = "  \t\n content \r\n\t  "
                trimmed1 = trim input
                trimmed2 = trim trimmed1
            trimmed1 @?= trimmed2

        , testCase "comment removal idempotency" $ do
            let input = "code // comment\n/* block */ more"
                once = removeComments input
                twice = removeComments once
            once @?= twice
        ]
    ]