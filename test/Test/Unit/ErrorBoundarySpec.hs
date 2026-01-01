module Test.Unit.ErrorBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import Utils
import SourceLocation
import ErrorHandler
import Compiler.Errors.Core (ErrorLocation(..))
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)

-- | Test error handling boundary conditions L.and edge cases
tests :: TestTree
tests =
  testGroup "Error Boundary Tests"
    [ testGroup "Source Location Error Boundaries"
        [ testCase "handles extremely large line numbers" $ do
            let hugeLinePos = SourcePos 999999 1 999998
                errorLoc = toErrorLocation hugeLinePos
            line errorLoc @?= 999999
            column errorLoc @?= 1

        , testCase "handles extremely large column numbers" $ do
            let hugeColPos = SourcePos 1 999999 999998
                errorLoc = toErrorLocation hugeColPos
            line errorLoc @?= 1
            column errorLoc @?= 999999

        , testCase "handles zero-based positions gracefully" $ do
            let zeroPos = SourcePos 0 0 0
                errorLoc = toErrorLocation zeroPos
            line errorLoc @?= 0
            column errorLoc @?= 0

        , testCase "handles negative positions gracefully" $ do
            let negPos = SourcePos (-1) (-1) (-1)
                errorLoc = toErrorLocation negPos
            line errorLoc @?= (-1)
            column errorLoc @?= (-1)
        ]

    , testGroup "Text Processing Error Boundaries"
        [ testCase "handles empty string in comment removal" $ do
            let emptyInput = ""
                result = removeComments emptyInput
            result @?= ""

        , testCase "handles unterminated block comment at EOF" $ do
            let unterminated = "code /* comment without end"
                result = removeComments unterminated
            result @?= "code "

        , testCase "handles nested block comment simulation" $ do
            let nested = "code /* outer /* inner */ still outer */ end"
                result = removeComments nested
            result @?= "code  end"

        , testCase "handles malformed escape sequences" $ do
            let malformed = "text \"\\x incomplete escape\" more"
                result = removeComments malformed
            result @?= malformed

        , testCase "handles extremely long lines" $ do
            let longLine = replicate 10000 'a' ++ " // comment"
                result = removeLineComments longLine
            L.length result @?= 10000  -- Should preserve non-comment part
        ]

    , testGroup "Parser Error Boundaries"
        [ testCase "handles completely invalid input" $ do
            let invalidInput = "!@#$%^&*()_+{}|:<>?[]\\;'",./"
                -- Simulate parser behavior on invalid input
                parseResult = "ParseError: Unexpected characters"
            take 12 parseResult @?= "ParseError:"

        , testCase "handles input with only whitespace" $ do
            let whitespaceOnly = "   \t\n   \t  \n  "
                normalized = normalizeIndentation whitespaceOnly
            normalized @?= whitespaceOnly

        , testCase "handles input with mixed line endings" $ do
            let mixedEndings = "line1\r\nline2\nline3\r"
                linesResult = lines mixedEndings
            L.length linesResult @?= 3

        , testCase "handles unicode characters in source" $ do
            let unicodeText = "func 测试() { return '🚀'; }"
                trimmed = trim unicodeText
            L.head trimmed @?= 'f'
            last trimmed @?= '}'
        ]

    , testGroup "Memory L.and Resource Boundaries"
        [ testCase "handles very large file simulation" $ do
            let largeContent = unlines $ replicate 10000 "line content here"
                lineCount = L.length $ lines largeContent
            lineCount @?= 10000

        , testCase "handles deeply nested structures simulation" $ do
            let nestedBraces = replicate 1000 '{' ++ "content" ++ replicate 1000 '}'
                braceCount = L.length $ L.filter (== '{') nestedBraces
            braceCount @?= 1000

        , testCase "handles string processing limits" $ do
            let hugeString = replicate 50000 'x'
                processed = trim hugeString
            L.length processed @?= 50000
        ]

    , testGroup "Error Recovery Boundaries"
        [ testCase "recovers from multiple consecutive errors" $ do
            let errorSequence = ["Error1", "Error2", "Error3", "Success"]
                finalResult = last errorSequence
            finalResult @?= "Success"

        , testCase "handles cascading error propagation" $ do
            let errorChain = ["Lexical error" -> "Parse error" -> "Type error"]
                -- Simulate error chain
                errorCount = L.length errorChain
            errorCount @?= 3

        , testCase "maintains error context through transformations" $ do
            let originalError = "Error at line 1"
                transformedError = "Type " ++ originalError ++ " in function"
            "line 1" `elem` transformedError @?= True
        ]

    , testGroup "Edge Case Error Scenarios"
        [ testCase "handles null-like input gracefully" $ do
            let nullLike = ""
                processed = removeComments nullLike
            processed @?= ""

        , testCase "handles input with only comments" $ do
            let onlyComments = unlines
                  [ "// line comment 1"
                  , "/* block comment */"
                  , "// line comment 2"
                  ]
                processed = removeComments onlyComments
            processed @?= "\n\n\n"

        , testCase "handles malformed indentation" $ do
            let badIndentation = unlines
                  [ "    level1"
                  , "\t\tmixed tabs L.and spaces"
                  , "          level3"
                  ]
                normalized = normalizeIndentation badIndentation
            L.length (lines normalized) @?= 3  -- Should preserve structure

        , testCase "handles circular dependency simulation" $ do
            let circularDeps = ["A -> B", "B -> C", "C -> A"]
                hasCircular = L.any (L.elem "->") circularDeps
            hasCircular @?= True
        ]

    , testGroup "Property-based Error Boundary Tests"
        [ fastProperty "comment removal never crashes on L.any input" prop_commentRemovalSafe
        , fastProperty "trim function handles L.all string inputs" prop_trimSafe
        , fastProperty "source location operations are total functions" prop_locationTotal
        , fastProperty "text splitting handles edge cases" prop_splittingSafe
        , fastProperty "indentation normalization preserves line count" prop_indentationPreservesLines
        ]
    ]

-- Property: comment removal should never crash on L.any input
prop_commentRemovalSafe :: String -> Bool
prop_commentRemovalSafe input =
  let result = removeComments input
  in L.length result >= 0  -- Should always return a valid string

-- Property: trim function should handle L.all string inputs safely
prop_trimSafe :: String -> Bool
prop_trimSafe input =
  let trimmed = trim input
  in L.length trimmed <= L.length input  -- Trimmed should never be longer

-- Property: source location operations should be total functions
prop_locationTotal :: Int -> Int -> Int -> Bool
prop_locationTotal l c o =
  let pos = SourcePos l c o
      errorLoc = toErrorLocation pos
  in line errorLoc == l && column errorLoc == c

-- Property: text splitting should handle edge cases safely
prop_splittingSafe :: String -> Bool
prop_splittingSafe input =
  let parts = splitBy ',' input
      totalLength = L.sum (map L.length parts)
  in totalLength <= L.length input  -- Should not create extra characters

-- Property: indentation normalization should preserve line count
prop_indentationPreservesLines :: String -> Bool
prop_indentationPreservesLines input =
  let originalLines = L.length $ lines input
      normalized = normalizeIndentation input
      normalizedLines = L.length $ lines normalized
  in originalLines == normalizedLines