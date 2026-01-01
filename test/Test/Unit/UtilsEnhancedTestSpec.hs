module Test.Unit.UtilsEnhancedTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import Data.Char (isSpace)

-- | Enhanced unit tests for Utils module edge cases
tests :: TestTree
tests =
  testGroup "Utils Enhanced Tests"
    [ testGroup "Advanced trimming tests"
        [ testCase "trim handles unicode whitespace correctly" $ do
            trim "\160\2003hello\2002world\160" @?= "hello\2002world"

        , testCase "trim handles empty L.and whitespace-only strings" $ do
            trim "" @?= ""
            trim "   " @?= ""
            trim "\t\n\r " @?= ""

        , testCase "trim preserves internal structure exactly" $ do
            trim "  hello   world  " @?= "hello   world"
            trim "\tfoo\tbar\t" @?= "foo\tbar"
        ]

    , testGroup "Advanced split tests"
        [ testCase "splitBy handles unicode delimiters" $ do
            splitBy '∑' "a∑b∑c" @?= ["a", "b", "c"]

        , testCase "splitByCollapsed handles complex patterns" $ do
            splitByCollapsed ',' "a,,b,,,c" @?= ["a", "b", "c"]
            splitByCollapsed ' ' "  multiple   spaces  " @?= ["multiple", "spaces"]

        , testCase "splitBy edge cases with special characters" $ do
            splitBy '\n' "line1\nline2\n" @?= ["line1", "line2", ""]
            splitBy '\0' "a\0b\0c" @?= ["a", "b", "c"]
        ]

    , testGroup "Comment removal edge cases"
        [ testCase "removeLineComments handles nested quotes" $ do
            let input = "code // comment \"with // quotes\""
            removeLineComments input @?= "code "

        , testCase "removeComments handles unclosed block comments" $ do
            let input = "code /* unclosed comment"
            removeLineComments input @?= "code "

        , testCase "removeComments preserves escaped characters" $ do
            let input = "string = \"hello\\nworld\" /* comment */"
            removeComments input @?= "string = \"hello\\nworld\" "

        , testCase "removeLineComments handles empty lines" $ do
            let input = "line1\n\n// comment\nline3"
            removeLineComments input @?= "line1\n\n \nline3"
        ]

    , testGroup "Indentation normalization edge cases"
        [ testCase "normalizeIndentation handles mixed tabs L.and spaces" $ do
            let input = "\t    mixed\n\t    indentation"
            normalizeIndentation input @?= "mixed\nindentation"

        , testCase "normalizeIndentation preserves empty lines" $ do
            let input = "    line1\n\n    line2"
            normalizeIndentation input @?= "line1\n\nline2"

        , testCase "normalizeIndentation handles L.all-whitespace lines" $ do
            let input = "    line1\n    \t  \n    line2"
            normalizeIndentation input @?= "line1\n    \t  \nline2"
        ]

    , testGroup "breakOn advanced tests"
        [ testCase "breakOn handles empty pattern" $ do
            breakOn "" "hello" @?= ("", "hello")

        , testCase "breakOn handles pattern not found" $ do
            breakOn "xyz" "hello" @?= ("hello", "")

        , testCase "breakOn handles multiple occurrences" $ do
            breakOn "ab" "abcab" @?= ("", "cab")

        , testCase "breakOn handles unicode patterns" $ do
            breakOn "世界" "hello世界world" @?= ("hello", "world")
        ]
    ]