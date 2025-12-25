{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Unit.CoreUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, listOf, elements, (==>)
import qualified Data.Text as T
import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)

tests :: TestTree
tests = testGroup "Core Utils Tests"
  [ testGroup "trim function"
    [ testCase "trims whitespace from both ends" $
        trim "  hello world  " @?= "hello world"
    , testCase "handles empty string" $
        trim "" @?= ""
    , testCase "handles only whitespace" $
        trim "   \t\n   " @?= ""
    , testCase "preserves internal whitespace" $
        trim "  hello   world  " @?= "hello   world"
    ]
  , testGroup "splitBy function"
    [ testCase "splits by comma preserving empty segments" $
        splitBy ',' "a,,b" @?= ["a", "", "b"]
    , testCase "handles leading and trailing delimiters" $
        splitBy ',' ",a," @?= ["", "a", ""]
    , testCase "handles empty string" $
        splitBy ',' "" @?= [""]
    , testCase "handles no delimiters" $
        splitBy ',' "abc" @?= ["abc"]
    ]
  , testGroup "splitByCollapsed function"
    [ testCase "splits by comma collapsing empty segments" $
        splitByCollapsed ',' "a,,b" @?= ["a", "b"]
    , testCase "handles leading and trailing delimiters" $
        splitByCollapsed ',' ",a," @?= ["a"]
    , testCase "handles empty string" $
        splitByCollapsed ',' "" @?= []
    ]
  , testGroup "removeLineComments function"
    [ testCase "removes single line comments" $
        removeLineComments "hello // comment\nworld" @?= "hello \nworld"
    , testCase "preserves comments in strings" $
        removeLineComments "print(\"// not a comment\") // real comment" @?= "print(\"// not a comment\") "
    , testCase "preserves comments in chars" $
        removeLineComments "let c = '/' // not comment" @?= "let c = '/' "
    , testCase "handles escaped quotes in strings" $
        removeLineComments "print(\"\\\"// not comment\") // comment" @?= "print(\"\\\"// not comment\") "
    ]
  , testGroup "normalizeIndentation function"
    [ testCase "removes common prefix indentation" $
        normalizeIndentation "    foo\n      bar" @?= "foo\n  bar"
    , testCase "handles mixed indentation" $
        normalizeIndentation "\tfoo\n\t\tbar" @?= "foo\n\tbar"
    , testCase "preserves empty lines" $
        normalizeIndentation "  foo\n\n  bar" @?= "foo\n\nbar"
    ]
  , testGroup "breakOn function"
    [ testCase "breaks on substring" $
        breakOn "ll" "hello" @?= ("he", "o")
    , testCase "handles not found" $
        breakOn "xyz" "hello" @?= ("hello", "")
    , testCase "handles empty pattern" $
        breakOn "" "hello" @?= ("", "hello")
    , testCase "handles pattern at start" $
        breakOn "he" "hello" @?= ("", "llo")
    ]
  , testGroup "QuickCheck properties"
    [ testProperty "splitBy preserves total length" $
        \c s -> length (concat (splitBy c s)) == length s
    , testProperty "splitByCollapsed never produces empty strings" $
        \c s -> all (not . null) (splitByCollapsed c s)
    , testProperty "trim . trim = trim" $
        \s -> trim (trim s) == trim s
    , testProperty "breakOn pattern pattern = (\"\", pattern)" $
        \p -> not (null p) ==> breakOn p p == ("", "")
    ]
  ]

-- Note: Arbitrary instances for Char and String are provided by QuickCheck