module Test.Unit.AdditionalUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Utils

-- | Additional unit tests for Utils module functions
tests :: TestTree
tests =
  testGroup "Additional Utils tests"
    [ testGroup "breakOn function tests"
        [ testCase "breakOn finds pattern in middle" $ do
            breakOn "ll" "hello" @?= ("he", "o")

        , testCase "breakOn with pattern at start" $ do
            breakOn "ab" "abcdef" @?= ("", "cdef")

        , testCase "breakOn with pattern at end" $ do
            breakOn "cd" "abcd" @?= ("ab", "")

        , testCase "breakOn with non-existent pattern" $ do
            breakOn "xyz" "hello" @?= ("hello", "")

        , testCase "breakOn with empty pattern" $ do
            breakOn "" "hello" @?= ("", "hello")

        , testCase "breakOn with empty string" $ do
            breakOn "a" "" @?= ("", "")
        ]

    , testGroup "normalizeIndentation edge cases"
        [ testCase "normalizeIndentation with empty string" $ do
            normalizeIndentation "" @?= ""

        , testCase "normalizeIndentation with only whitespace" $ do
            normalizeIndentation "   \n  \n   " @?= "\n\n"

        , testCase "normalizeIndentation with mixed tabs and spaces" $ do
            normalizeIndentation "\t  hello\n\t\t  world" @?= "hello\n  world"

        , testCase "normalizeIndentation preserves relative indentation" $ do
            let input = "    a\n      b\n    c"
                expected = "a\n  b\nc"
            normalizeIndentation input @?= expected
        ]

    , testGroup "removeComments complex cases"
        [ testCase "removeComments handles nested-like patterns" $ do
            let input = "code /* outer /* inner */ still outer */ end"
                expected = "code  end"
            removeComments input @?= expected

        , testCase "removeComments preserves string literals with comment patterns" $ do
            let input = "print(\"/* not a comment */\") // real comment"
                expected = "print(\"/* not a comment */\") "
            removeComments input @?= expected

        , testCase "removeComments handles char literals with comment patterns" $ do
            let input = "char c = '/' /* not division */ // comment"
                expected = "char c = '/'  "
            removeComments input @?= expected

        , testCase "removeComments handles escaped quotes in strings" $ do
            let input = "s = \"\\\"/* not comment */\\\" /* real comment */"
                expected = "s = \"\\\"/* not comment */\\\"  "
            removeComments input @?= expected
        ]

    , testGroup "splitBy edge cases"
        [ testCase "splitBy with Unicode characters" $ do
            splitBy '，' "你好，世界，" @?= ["你好", "世界", ""]

        , testCase "splitBy with special delimiter characters" $ do
            splitBy '\n' "line1\nline2\n" @?= ["line1", "line2", ""]

        , testCase "splitByCollapsed with all delimiters" $ do
            splitByCollapsed ',' ",,," @?= []
        ]
    ]