module Test.Unit.CoreUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), oneof, elements)
import qualified Data.Text as T

import Utils

-- | Core functionality tests for Utils module
tests :: TestTree
tests =
  testGroup "Core Utils Tests"
    [ testGroup "String manipulation functions"
        [ testCase "trim handles various whitespace combinations" $ do
            trim "  hello  " @?= "hello"
            trim "\t\n  hello world  \n\t" @?= "hello world"
            trim "" @?= ""
            trim "   " @?= ""
            trim "no-whitespace" @?= "no-whitespace"

        , testCase "splitBy handles edge cases correctly" $ do
            splitBy ',' "" @?= [""]
            splitBy ',' "a" @?= ["a"]
            splitBy ',' "," @?= ["", ""]
            splitBy ',' "a,b,c" @?= ["a", "b", "c"]
            splitBy ',' "a,,b" @?= ["a", "", "b"]
            splitBy '," ",a,b," @?= ["", "a", "b", ""]

        , testCase "splitByCollapsed removes empty segments" $ do
            splitByCollapsed ',' "" @?= []
            splitByCollapsed ',' "," @?= []
            splitByCollapsed ',' ",," @?= []
            splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
            splitByCollapsed ',' "a,,b" @?= ["a", "b"]
            splitByCollapsed ',' ",a,b," @?= ["a", "b"]

        , testProperty "splitBy length is >= splitByCollapsed length" $ 
            \str -> length (splitBy ',' str) >= length (splitByCollapsed ',' str)
        ]

    , testGroup "Comment handling functions"
        [ testCase "removeLineComments handles basic cases" $ do
            removeLineComments "hello // comment" @?= "hello "
            removeLineComments "// full line comment\nnext line" @?= "\nnext line"
            removeLineComments "no comment here" @?= "no comment here"

        , testCase "removeLineComments respects string literals" $ do
            removeLineComments "text := \"http://example.com//path\" // comment" @?= "text := \"http://example.com//path\" "
            removeLineComments "char := '/' // comment" @?= "char := '/' "
            removeLineComments "escaped := \"She said \\\"// not comment\\\"\" // comment" @?= "escaped := \"She said \\\"// not comment\\\"\" "

        , testCase "removeComments handles block comments" $ do
            removeComments "before /* comment */ after" @?= "before  after"
            removeComments "/* multi\nline\ncomment */ done" @?= "\n\ndone"
            removeComments "/* nested /* not supported */ */" @?= " "

        , testCase "removeComments respects string literals in blocks" $ do
            removeComments "text := \"/* not comment */\" /* real comment */" @?= "text := \"/* not comment */\" "
            removeComments "path := \"C://tmp/*\" /* comment */" @?= "path := \"C://tmp/*\" "
        ]

    , testGroup "Indentation functions"
        [ testCase "normalizeIndentation removes common prefix" $ do
            let input = "    line1\n      line2\n    line3\n"
                expected = "line1\n  line2\nline3\n"
            normalizeIndentation input @?= expected

        , testCase "normalizeIndentation handles empty lines" $ do
            let input = "\n    line1\n\n    line2\n"
                expected = "\nline1\n\nline2\n"
            normalizeIndentation input @?= expected

        , testCase "forceSingleTabIndentation converts to tabs" $ do
            let input = "  line1\n    line2\nline3\n"
                expected = "\tline1\n\tline2\n\tline3\n"
            forceSingleTabIndentation input @?= expected

        , testCase "fixIndentation is alias for normalizeIndentation" $ do
            let input = "  test\n    line\n"
            fixIndentation input @?= normalizeIndentation input
        ]

    , testGroup "Search functions"
        [ testCase "breakOn finds patterns correctly" $ do
            breakOn "world" "hello world" @?= ("hello ", "")
            breakOn "ll" "hello" @?= ("he", "o")
            breakOn "abc" "abc" @?= ("", "")
            breakOn "xyz" "hello" @?= ("hello", "")
            breakOn "" "test" @?= ("", "test")

        , testProperty "breakOn pattern not found returns original string" $
            \str pat -> not (pat `isInfixOf` str) ==> breakOn pat str == (str, "")
          where
            isInfixOf needle haystack = needle `T.isInfixOf` T.pack haystack
        ]

    , testGroup "Property-based tests for core functions"
        [ testProperty "trim is idempotent" $
            \str -> trim (trim str) == trim str

        , testProperty "splitBy followed by join preserves delimiters" $
            \str -> unlines (splitBy '\n' str) == str

        , testProperty "removeComments doesn't change strings without comments" $
            \str -> not (hasCommentMarkers str) ==> removeComments str == str
          where
            hasCommentMarkers s = "//" `isInfixOf` s || "/*" `isInfixOf` s
            isInfixOf needle haystack = needle `T.isInfixOf` T.pack haystack
        ]
    ]