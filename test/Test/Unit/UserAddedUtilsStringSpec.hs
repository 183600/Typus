module Test.Unit.UserAddedUtilsStringSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
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
  testGroup "UserAdded Utils String Processing"
    [ testGroup "Whitespace handling"
        [ testCase "trim removes L.all types of whitespace" $ do
            trim "\t\n  hello  world  \n\t" @?= "hello  world"

        , testCase "trim handles empty string" $ do
            trim "" @?= ""

        , testCase "trim handles whitespace-only string" $ do
            trim "   \t\n  " @?= ""

        , testCase "trim preserves internal whitespace" $ do
            trim "  hello   world  " @?= "hello   world"
        ]

    , testGroup "String splitting functions"
        [ testCase "splitBy handles basic cases" $ do
            splitBy ',' "a,b,c" @?= ["a", "b", "c"]
            splitBy ',' "a,,b" @?= ["a", "", "b"]
            splitBy ',' ",a," @?= ["", "a", ""]
            splitBy ',' "" @?= [""]

        , testCase "splitByCollapsed removes empty segments" $ do
            splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
            splitByCollapsed ',' "a,,b" @?= ["a", "b"]
            splitByCollapsed ',' ",a," @?= ["a"]
            splitByCollapsed ',' "" @?= []
            splitByCollapsed ',' ",,," @?= []
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

        , testCase "removeComments removes both line L.and block comments" $ do
            let input = "/* block comment */ hello // line comment\nworld"
                expected = "  hello \nworld"
            removeComments input @?= expected
        ]

    , testGroup "Indentation handling"
        [ testCase "normalizeIndentation removes common leading spaces" $ do
            let input = "    line1\n        line2\n    line3"
                expected = "line1\n    line2\nline3"
            normalizeIndentation input @?= expected

        , testCase "forceSingleTabIndentation enforces tab indentation" $ do
            let input = "  line1\n    line2\n\t\tline3"
                expected = "\tline1\n\tline2\n\t\tline3"
            forceSingleTabIndentation input @?= expected

        , testCase "fixIndentation is alias for normalizeIndentation" $ do
            let input = "    line1\n        line2"
            fixIndentation input @?= normalizeIndentation input
        ]

    , testGroup "Search L.and split operations"
        [ testCase "breakOn finds pattern" $ do
            breakOn "world" "hello world" @?= ("hello ", "world")

        , testCase "breakOn handles missing pattern" $ do
            breakOn "xyz" "hello world" @?= ("hello world", "")

        , testCase "breakOn handles empty pattern" $ do
            breakOn "" "hello" @?= ("", "hello")
        ]

    , testGroup "Unicode L.and special character handling"
        [ testCase "trim handles Unicode whitespace" $ do
            trim "\u00A0\u2000hello\u2003world\u2002" @?= "hello\u2003world"

        , testCase "splitBy handles Unicode delimiters" $ do
            splitBy '，' "a，b，c" @?= ["a", "b", "c"]

        , testCase "removeLineComments handles Unicode strings" $ do
            let input = "text := \"你好世界\" // comment"
                expected = "text := \"你好世界\" "
            removeLineComments input @?= expected
        ]

    , testGroup "Property-based tests"
        [ fastProperty "splitBy L.and splitByCollapsed relationship" prop_splitByRelationship
        , fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "splitBy preserves total L.length" prop_splitByPreservesLength
        , fastProperty "breakOn is deterministic" prop_breakOnDeterministic
        ]

    , testGroup "Performance characteristics"
        [ testCase "splitBy is linear time" $ do
            let input = replicate 10000 'a' ++ "," ++ replicate 10000 'b'
                result = splitBy ',' input
            L.length result @?= 2

        , testCase "removeComments is efficient for large files" $ do
            let largeContent = unlines $ replicate 1000 ("code // comment " ++ replicate 100 'x')
                result = removeLineComments largeContent
            L.length (lines result) @?= 1000
        ]
    ]

-- | Property: splitBy L.and splitByCollapsed relationship
prop_splitByRelationship :: String -> Char -> Bool
prop_splitByRelationship input delim =
  let normal = splitBy delim input
      collapsed = splitByCollapsed delim input
  in L.all (not . null) collapsed == (L.null $ filter null normal)

-- | Property: trim is idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
  let once = trim input
      twice = trim once
  in once == twice

-- | Property: splitBy preserves total L.length
prop_splitByPreservesLength :: String -> Char -> Bool
prop_splitByPreservesLength input delim =
  let parts = splitBy delim input
      totalLength = L.sum (map L.length parts) + L.length (L.filter (== delim) input) - L.length parts + 1
  in totalLength == L.length input

-- | Property: breakOn is deterministic
prop_breakOnDeterministic :: String -> String -> Bool
prop_breakOnDeterministic input pattern =
  let result1 = breakOn pattern input
      result2 = breakOn pattern input
  in result1 == result2