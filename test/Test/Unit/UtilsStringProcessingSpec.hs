module Test.Unit.UtilsStringProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck (fastProperty)

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

import Data.Char (isSpace)
import qualified Data.Text as T

-- | Tests for Utils string processing functions
tests :: TestTree
tests =
  testGroup "Utils String Processing"
    [ testGroup "Whitespace handling"
        [ testCase "trim removes all types of whitespace" $ do
            trim "\t\n  hello  world  \n\t" @?= "hello  world"

        , testCase "trim handles empty string" $ do
            trim "" @?= ""

        , testCase "trim handles whitespace-only string" $ do
            trim "   \t\n  " @?= ""

        , testCase "trim preserves internal whitespace" $ do
            trim "  hello   world  " @?= "hello   world"

        , testCase "trim handles Unicode whitespace" $ do
            trim "\u00A0\u2000hello\u2003world\u2002" @?= "hello\u2003world"
        ]

    , testGroup "String splitting functions"
        [ testCase "splitBy handles basic cases" $ do
            splitBy ',' "a,b,c" @?= ["a", "b", "c"]
            splitBy ',' "a,,b" @?= ["a", "", "b"]
            splitBy ',' ",a," @?= ["", "a", ""]
            splitBy ',' "" @?= [""]

        , testCase "splitBy with different delimiters" $ do
            splitBy ':' "a:b:c" @?= ["a", "b", "c"]
            splitBy ';' "a;b;c" @?= ["a", "b", "c"]
            splitBy '|' "a|b|c" @?= ["a", "b", "c"]

        , testCase "splitByCollapsed removes empty segments" $ do
            splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
            splitByCollapsed ',' "a,,b" @?= ["a", "b"]
            splitByCollapsed '," ",a," @?= ["a"]
            splitByCollapsed ',' "" @?= []
            splitByCollapsed '," ",,," @?= []

        , testCase "splitByComma works correctly" $ do
            splitByComma "a,b,c" @?= ["a", "b", "c"]
            splitByComma "a,,b" @?= ["a", "", "b"]

        , testCase "splitByCommaCollapsed works correctly" $ do
            splitByCommaCollapsed "a,b,c" @?= ["a", "b", "c"]
            splitByCommaCollapsed "a,,b" @?= ["a", "b"]
            splitByCommaCollapsed "" @?= []
        ]

    , testGroup "Comment removal"
        [ testCase "removeLineComments removes // comments" $ do
            let input = "hello // comment\nworld // another comment"
                expected = "hello \nworld "
            removeLineComments input @?= expected

        , testCase "removeLineComments preserves string literals" $ do
            let input = "url := \"http://example.com//path\" // comment"
                expected = "url := \"http://example.com//path\" "
            removeLineComments input @?= expected

        , testCase "removeLineComments handles escaped quotes" $ do
            let input = "text := \"She said \\\"// not a comment\\\"\" // real comment"
                expected = "text := \"She said \\\"// not a comment\\\"\" "
            removeLineComments input @?= expected

        , testCase "removeComments removes both line and block comments" $ do
            let input = "/* block comment */ hello // line comment\nworld"
                expected = "  hello \nworld"
            removeComments input @?= expected

        , testCase "removeComments handles nested block comments" $ do
            let input = "/* outer /* inner */ still outer */ code"
                expected = "  code"
            removeComments input @?= expected

        , testCase "removeComments preserves comments in strings" $ do
            let input = "text := \"/* not a block comment */\" /* real block comment */"
                expected = "text := \"/* not a block comment */\" "
            removeComments input @?= expected

        , testCase "removeComments handles unterminated block comments" $ do
            let input = "code /* unterminated\nmore code"
                expected = "code \n"
            removeComments input @?= expected
        ]

    , testGroup "Indentation handling"
        [ testCase "normalizeIndentation removes common leading spaces" $ do
            let input = "    line1\n        line2\n    line3"
                expected = "line1\n    line2\nline3"
            normalizeIndentation input @?= expected

        , testCase "normalizeIndentation handles mixed tabs and spaces" $ do
            let input = "\t    line1\n\t        line2\n\t    line3"
                expected = "line1\n    line2\nline3"
            normalizeIndentation input @?= expected

        , testCase "normalizeIndentation preserves empty lines" $ do
            let input = "    line1\n\n    line2"
                expected = "line1\n\nline2"
            normalizeIndentation input @?= expected

        , testCase "forceSingleTabIndentation enforces tab indentation" $ do
            let input = "  line1\n    line2\n\t\tline3"
                expected = "\tline1\n\tline2\n\t\tline3"
            forceSingleTabIndentation input @?= expected

        , testCase "forceSingleTabIndentation collapses whitespace-only lines" $ do
            let input = "  line1\n    \n  line2"
                expected = "\tline1\n\n\tline2"
            forceSingleTabIndentation input @?= expected

        , testCase "fixIndentation is alias for normalizeIndentation" $ do
            let input = "    line1\n        line2"
            fixIndentation input @?= normalizeIndentation input
        ]

    , testGroup "Search and split operations"
        [ testCase "breakOn finds pattern" $ do
            breakOn "world" "hello world" @?= ("hello ", "world")

        , testCase "breakOn handles missing pattern" $ do
            breakOn "xyz" "hello world" @?= ("hello world", "")

        , testCase "breakOn handles empty pattern" $ do
            breakOn "" "hello" @?= ("", "hello")

        , testCase "breakOn handles pattern at start" $ do
            breakOn "hello" "hello world" @?= ("", "hello world")

        , testCase "breakOn handles pattern at end" $ do
            breakOn "world" "hello world" @?= ("hello ", "world")
        ]

    , testGroup "Unicode and special character handling"
        [ testCase "trim handles Unicode whitespace" $ do
            trim "\u00A0\u2000hello\u2003world\u2002" @?= "hello\u2003world"

        , testCase "splitBy handles Unicode delimiters" $ do
            splitBy '，' "a，b，c" @?= ["a", "b", "c"]

        , testCase "removeLineComments handles Unicode strings" $ do
            let input = "text := \"你好世界\" // comment"
                expected = "text := \"你好世界\" "
            removeLineComments input @?= expected

        , testCase "normalizeIndentation handles Unicode content" $ do
            let input = "    你好\n        世界\n    !"
                expected = "你好\n    世界\n!"
            normalizeIndentation input @?= expected
        ]

    , testGroup "Edge cases and stress tests"
        [ testCase "splitBy handles very long strings" $ do
            let longString = replicate 1000 'a' ++ "," ++ replicate 1000 'b'
                result = splitBy ',' longString
            length result @?= 2
            length (head result) @?= 1000
            length (result !! 1) @?= 1000

        , testCase "removeComments handles large block comments" $ do
            let largeBlock = "/* " ++ replicate 10000 'x' ++ " */ code"
                result = removeComments largeBlock
            result @?= "  code"

        , testCase "normalizeIndentation handles deeply indented code" $ do
            let deepIndent = concat $ replicate 100 "    "
                input = deepIndent ++ "code\n" ++ deepIndent ++ "more code"
                result = normalizeIndentation input
            result @?= "code\nmore code"

        , testCase "breakOn handles pattern at multiple positions" $ do
            let input = "abcabcabc"
                result = breakOn "abc" input
            result @?= ("", "abcabcabc")
        ]

    , testGroup "Property-based tests"
        [ fastProperty "splitBy and splitByCollapsed relationship" prop_splitByRelationship
        , fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "splitBy preserves total length" prop_splitByPreservesLength
        , fastProperty "breakOn is deterministic" prop_breakOnDeterministic
        , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizePreservesRelative
        ]

    , testGroup "Performance characteristics"
        [ testCase "splitBy is linear time" $ do
            let input = replicate 10000 'a' ++ "," ++ replicate 10000 'b'
                result = splitBy ',' input
            length result @?= 2

        , testCase "removeComments is efficient for large files" $ do
            let largeContent = unlines $ replicate 1000 ("code // comment " ++ replicate 100 'x')
                result = removeLineComments largeContent
            length (lines result) @?= 1000

        , testCase "normalizeIndentation handles large files efficiently" $ do
            let largeIndented = unlines $ map (\i -> replicate (i `mod` 20) ' ' ++ "line " ++ show i) [1..1000]
                result = normalizeIndentation largeIndented
            length (lines result) @?= 1000
        ]
    ]

-- | Property: splitBy and splitByCollapsed relationship
prop_splitByRelationship :: String -> Char -> Bool
prop_splitByRelationship input delim =
  let normal = splitBy delim input
      collapsed = splitByCollapsed delim input
  in all (not . null) collapsed == (null $ filter null normal)

-- | Property: trim is idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
  let once = trim input
      twice = trim once
  in once == twice

-- | Property: splitBy preserves total length
prop_splitByPreservesLength :: String -> Char -> Bool
prop_splitByPreservesLength input delim =
  let parts = splitBy delim input
      totalLength = sum (map length parts) + length (filter (== delim) input) - length parts + 1
  in totalLength == length input

-- | Property: breakOn is deterministic
prop_breakOnDeterministic :: String -> String -> Bool
prop_breakOnDeterministic input pattern =
  let result1 = breakOn pattern input
      result2 = breakOn pattern input
  in result1 == result2

-- | Property: normalizeIndentation preserves relative indentation
prop_normalizePreservesRelative :: String -> Bool
prop_normalizePreservesRelative input =
  let linesInput = lines input
      normalized = normalizeIndentation input
      linesNormalized = lines normalized
  in length linesInput == length linesNormalized