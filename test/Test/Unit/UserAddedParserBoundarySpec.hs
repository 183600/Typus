module Test.Unit.UserAddedParserBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck (fastProperty)

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..), defaultFileDirectives)
import SourceLocation (SourceSpan(..), SourcePos(..), isValidSpan)

-- | Tests for Parser boundary conditions
tests :: TestTree
tests =
  testGroup "UserAdded Parser Boundary Conditions"
    [ testGroup "Empty L.and minimal input"
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
        ]

    , testGroup "Malformed directives"
        [ testCase "parse file with incomplete directive" $ do
            let input = "//! ownership=\nfunc main() {}\n"
                result = parseTypus input "incomplete.typus"
            tfBlocks result @?= []
            tfBuildTags result @?= []

        , testCase "parse file with unknown directive keys" $ do
            let input = "//! unknown=true, other=value\nfunc main() {}\n"
                result = parseTypus input "unknown.typus"
            L.length (tfBlocks result) @?= 1
        ]

    , testGroup "Block boundary conditions"
        [ testCase "parse file with empty code block" $ do
            let input = "//! ownership=true\n\n//! ownership=false\n\n"
                result = parseTypus input "emptyblocks.typus"
            L.length (tfBlocks result) @?= 2

        , testCase "parse file with single-character code block" $ do
            let input = "//! ownership=true\nx"
                result = parseTypus input "singlechar.typus"
                blocks = tfBlocks result
            L.length blocks @?= 1
            cbContent (L.head blocks) @?= "x"
        ]

    , testGroup "Unicode L.and encoding edge cases"
        [ testCase "parse file with UTF-8 characters" $ do
            let input = "//! ownership=true\n// 你好世界\nfunc main() { fmt.Println(\"Hello, 世界\") }\n"
                result = parseTypus input "unicode.typus"
                blocks = tfBlocks result
            L.length blocks @?= 1
            assertBool "Should contain Unicode characters" $ "世界" `elem` cbContent (L.head blocks)

        , testCase "parse file with emojis" $ do
            let input = "//! ownership=true\n// 😀 🎉 🚀\nfunc main() {}\n"
                result = parseTypus input "emoji.typus"
                blocks = tfBlocks result
            L.length blocks @?= 1
            assertBool "Should contain emoji characters" $ L.all (`elem` cbContent (L.head blocks)) ["😀", "🎉", "🚀"]
        ]

    , testGroup "Property-based edge cases"
        [ fastProperty "parse round-trip preserves structure for simple files" prop_roundTripSimple
        , fastProperty "parse handles arbitrary unicode strings" prop_unicodeHandling
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
            L.length blocks @?= 2
        ]
    ]

-- | Property: parse round-trip preserves structure for simple files
prop_roundTripSimple :: String -> String -> Bool
prop_roundTripSimple directives content =
  let input = "//!" ++ directives ++ "\n" ++ content
      parsed = parseTypus input "roundtrip.typus"
      expectedBlocks = if null content then 0 else 1
  in L.length (tfBlocks parsed) == expectedBlocks

-- | Property: parse handles arbitrary unicode strings
prop_unicodeHandling :: String -> Bool
prop_unicodeHandling unicodeStr =
  let input = "//! ownership=true\n" ++ unicodeStr
      result = parseTypus input "unicode.prop"
      blocks = tfBlocks result
  in if null unicodeStr
     then null blocks
     else not (null blocks)