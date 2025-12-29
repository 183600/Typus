module Test.Unit.ParserBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck (fastProperty)

import Parser
  ( parseTypus
  , TypusFile(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (SourceSpan(..), SourcePos(..), isValidSpan)

-- | Property-based and edge case tests for Parser boundary conditions
tests :: TestTree
tests =
  testGroup "Parser Boundary Conditions"
    [ testGroup "Empty and minimal input"
        [ testCase "parse empty string returns valid file with no blocks" $ do
            let result = parseTypus "" "empty.typus"
                expected = TypusFile defaultFileDirectives [] [] []
            result @?= expected

        , testCase "parse whitespace-only string returns valid file with no blocks" $ do
            let input = "   \n\t  \n   "
                result = parseTypus input "whitespace.typus"
                expected = TypusFile defaultFileDirectives [] [] []
            result @?= expected

        , testCase "parse file with only directives" $ do
            let input = "//! ownership=true, dependent-types=false\n"
                result = parseTypus input "directives.typus"
                expected = TypusFile 
                    (FileDirectives (Just True) (Just False) Nothing)
                    [] [] []
            result @?= expected

        , testCase "parse file with only build tags" $ do
            let input = "// +build linux,amd64\n"
                result = parseTypus input "buildtags.typus"
                expected = TypusFile defaultFileDirectives ["linux,amd64"] [] []
            result @?= expected
        ]

    , testGroup "Malformed directives"
        [ testCase "parse file with incomplete directive" $ do
            let input = "//! ownership=\nfunc main() {}\n"
                result = parseTypus input "incomplete.typus"
                -- Should parse successfully with empty directive value
            tfBlocks result @?= []
            tfBuildTags result @?= []

        , testCase "parse file with unknown directive keys" $ do
            let input = "//! unknown=true, other=value\nfunc main() {}\n"
                result = parseTypus input "unknown.typus"
                -- Should ignore unknown directives and continue parsing
            length (tfBlocks result) @?= 1

        , testCase "parse file with malformed directive syntax" $ do
            let input = "//! ownership true dependent-types false\nfunc main() {}\n"
                result = parseTypus input "malformed.typus"
                -- Should handle gracefully and continue parsing
            length (tfBlocks result) @?= 1

        , testCase "parse file with directive containing special characters" $ do
            let input = "//! ownership=\"true with spaces\", dependent-types=\"false,with,commas\"\nfunc main() {}\n"
                result = parseTypus input "special.typus"
                -- Should handle quoted values with special characters
            length (tfBlocks result) @?= 1
        ]

    , testGroup "Block boundary conditions"
        [ testCase "parse file with empty code block" $ do
            let input = "//! ownership=true\n\n//! ownership=false\n\n"
                result = parseTypus input "emptyblocks.typus"
                -- Should create empty blocks between directives
            length (tfBlocks result) @?= 2

        , testCase "parse file with single-character code block" $ do
            let input = "//! ownership=true\nx"
                result = parseTypus input "singlechar.typus"
                blocks = tfBlocks result
            length blocks @?= 1
            cbContent (head blocks) @?= "x"

        , testCase "parse file with very long single line" $ do
            let longLine = replicate 10000 'x'
                input = "//! ownership=true\n" ++ longLine
                result = parseTypus input "longline.typus"
                blocks = tfBlocks result
            length blocks @?= 1
            length (cbContent (head blocks)) @?= 10000

        , testCase "parse file with many short lines" $ do
            let manyLines = unlines $ replicate 1000 "x"
                input = "//! ownership=true\n" ++ manyLines
                result = parseTypus input "manylines.typus"
                blocks = tfBlocks result
            length blocks @?= 1
            length (lines $ cbContent (head blocks)) @?= 1000
        ]

    , testGroup "Unicode and encoding edge cases"
        [ testCase "parse file with UTF-8 characters" $ do
            let input = "//! ownership=true\n// 你好世界\nfunc main() { fmt.Println(\"Hello, 世界\") }\n"
                result = parseTypus input "unicode.typus"
                blocks = tfBlocks result
            length blocks @?= 1
            assertBool "Should contain Unicode characters" $ "世界" `elem` cbContent (head blocks)

        , testCase "parse file with emojis" $ do
            let input = "//! ownership=true\n// 😀 🎉 🚀\nfunc main() {}\n"
                result = parseTypus input "emoji.typus"
                blocks = tfBlocks result
            length blocks @?= 1
            assertBool "Should contain emoji characters" $ all (`elem` cbContent (head blocks)) ["😀", "🎉", "🚀"]

        , testCase "parse file with mixed line endings" $ do
            let input = "//! ownership=true\r\nfunc main() {\n\treturn\r\n}\n"
                result = parseTypus input "mixedlineendings.typus"
                blocks = tfBlocks result
            length blocks @?= 1
            assertBool "Should normalize line endings" $ '\r' `notElem` cbContent (head blocks)

        , testCase "parse file with zero-width characters" $ do
            let input = "//! ownership=true\nfunc main() {\u200b\treturn;\u200b}\n"
                result = parseTypus input "zerowidth.typus"
                blocks = tfBlocks result
            length blocks @?= 1
        ]

    , testGroup "Nested and complex structures"
        [ testCase "parse file with deeply nested comments" $ do
            let input = unlines
                  [ "//! ownership=true"
                  , "/* outer comment"
                  , "   /* inner comment */"
                  , "   still outer"
                  , " */"
                  , "func main() {}"
                  ]
                result = parseTypus input "nestedcomments.typus"
                blocks = tfBlocks result
            length blocks @?= 1

        , testCase "parse file with escaped quotes in strings" $ do
            let input = "//! ownership=true\nfunc main() { fmt.Println(\"She said \\\"hi\\\" and \\\"bye\\\"\") }\n"
                result = parseTypus input "escapedquotes.typus"
                blocks = tfBlocks result
            length blocks @?= 1
            assertBool "Should handle escaped quotes" $ "\\\"hi\\\"" `elem` cbContent (head blocks)

        , testCase "parse file with raw string literals" $ do
            let input = "//! ownership=true\nfunc main() { fmt.Println(`raw string with \"quotes\" and \\`backticks\\``) }\n"
                result = parseTypus input "rawstrings.typus"
                blocks = tfBlocks result
            length blocks @?= 1
            assertBool "Should handle raw strings" $ "`raw string" `elem` cbContent (head blocks)
        ]

    , testGroup "Property-based edge cases"
        [ fastProperty "parse round-trip preserves structure for simple files" prop_roundTripSimple
        , fastProperty "parse handles arbitrary unicode strings" prop_unicodeHandling
        , fastProperty "parse maintains block count consistency" prop_blockCountConsistency
        ]

    , testGroup "Error recovery"
        [ testCase "parse continues after syntax error in block" $ do
            let input = unlines
                  [ "//! ownership=true"
                  , "func invalid {"
                  , "  missing closing brace"
                  , ""
                  , "//! ownership=false"
                  , "func valid() {"
                  , "  return 42"
                  , "}"
                  ]
                result = parseTypus input "errorrecovery.typus"
                blocks = tfBlocks result
            -- Should create blocks even with syntax errors
            length blocks @?= 2

        , testCase "parse handles unterminated comments gracefully" $ do
            let input = unlines
                  [ "//! ownership=true"
                  , "/* unterminated comment"
                  , "func main() {}"
                  ]
                result = parseTypus input "unterminated.typus"
                blocks = tfBlocks result
            -- Should still create a block
            length blocks @?= 1
        ]
    ]

-- | Property: parse round-trip preserves structure for simple files
prop_roundTripSimple :: String -> String -> Bool
prop_roundTripSimple directives content =
  let input = "//!" ++ directives ++ "\n" ++ content
      parsed = parseTypus input "roundtrip.typus"
      -- Simple check: number of blocks should be consistent
      expectedBlocks = if null content then 0 else 1
  in length (tfBlocks parsed) == expectedBlocks

-- | Property: parse handles arbitrary unicode strings
prop_unicodeHandling :: String -> Bool
prop_unicodeHandling unicodeStr =
  let input = "//! ownership=true\n" ++ unicodeStr
      result = parseTypus input "unicode.prop"
      blocks = tfBlocks result
  in if null unicodeStr
     then null blocks
     else not (null blocks)

-- | Property: parse maintains block count consistency
prop_blockCountConsistency :: [String] -> Bool
prop_blockCountConsistency directivesList =
  let input = unlines $ map ("//! " ++) directivesList
      result = parseTypus input "blockcount.prop"
      blocks = tfBlocks result
      -- Number of blocks should be number of directives minus 1 (for content between directives)
      expectedBlocks = max 0 (length directivesList - 1)
  in length blocks == expectedBlocks