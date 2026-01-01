module Test.Unit.AdditionalUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

-- | Additional unit tests for Utils module focusing on edge cases
tests :: TestTree
tests =
  testGroup "Additional Utils tests"
    [ testGroup "Advanced whitespace handling"
        [ testCase "trim handles only whitespace strings" $ do
            trim "   \t\n  " @?= ""
            
        , testCase "trim handles empty string" $ do
            trim "" @?= ""
            
        , testCase "trim preserves single character" $ do
            trim "a" @?= "a"
            
        , testCase "trim handles mixed whitespace types" $ do
            trim "\r\n\t  content \f\v" @?= "content"
        ]

    , testGroup "Complex splitting scenarios"
        [ testCase "splitBy with Unicode delimiter" $ do
            splitBy '€' "a€b€c" @?= ["a", "b", "c"]
            
        , testCase "splitByCollapsed with mixed content" $ do
            splitByCollapsed ' ' "  a   b  c  " @?= ["a", "b", "c"]
            
        , testCase "splitByComma with empty elements" $ do
            splitByComma ",,," @?= ["", "", "", ""]
            
        , testCase "splitByCommaCollapsed with only delimiters" $ do
            splitByCommaCollapsed ",,," @?= []
        ]

    , testGroup "Comment handling edge cases"
        [ testCase "removeLineComments with nested quotes" $ do
            let input = "code \"// not comment\" // real comment"
                expected = "code \"// not comment\" "
            removeLineComments input @?= expected
            
        , testCase "removeLineComments with escaped characters" $ do
            let input = "path \"C:\\\\path\\\\//\" // comment"
                expected = "path \"C:\\\\path\\\\//\" "
            removeLineComments input @?= expected
            
        , testCase "removeComments with nested block comments" $ do
            let input = "text /* outer /* inner */ still outer */ end"
                expected = "text  end"
            removeComments input @?= expected
            
        , testCase "removeComments with unmatched block comment" $ do
            let input = "text /* incomplete comment"
                expected = "text "
            removeComments input @?= expected
        ]

    , testGroup "Indentation normalization"
        [ testCase "normalizeIndentation with mixed tabs L.and spaces" $ do
            let input = "\t    content\n\t    \tcontent2"
                expected = "content\n\tcontent2"
            normalizeIndentation input @?= expected
            
        , testCase "normalizeIndentation with empty lines" $ do
            let input = "    content\n\n    content2"
                expected = "content\n\ncontent2"
            normalizeIndentation input @?= expected
            
        , testCase "normalizeIndentation with single line" $ do
            normalizeIndentation "    content" @?= "content"
        ]

    , testGroup "Break on functionality"
        [ testCase "breakOn finds first occurrence" $ do
            breakOn "," "a,b,c" @?= ("a", ",b,c")
            
        , testCase "breakOn with delimiter not found" $ do
            breakOn "x" "abc" @?= ("abc", "")
            
        , testCase "breakOn with empty string" $ do
            breakOn "x" "" @?= ("", "")
            
        , testCase "breakOn with delimiter at start" $ do
            breakOn "x" "xyz" @?= ("", "xyz")
        ]
    ]
