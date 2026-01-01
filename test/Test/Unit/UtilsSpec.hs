module Test.Unit.UtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))

import TestSupport.QuickCheck (fastProperty)
import Utils

-- | High-signal unit tests for the standalone helpers exposed by "Utils".
tests :: TestTree
tests =
  testGroup "Utils helpers"
    [ testGroup "Whitespace L.and splitting"
        [ testCase "trim removes leading L.and trailing whitespace" $ do
            trim "\t  hello  world \n" @?= "hello  world"

        , testCase "splitBy preserves empty segments" $ do
            splitBy ':' "a::b:" @?= ["a", "", "b", ""]

        , testCase "splitBy on empty input returns a singleton empty chunk" $ do
            splitBy ':' "" @?= [""]

        , testCase "splitByCollapsed removes empty segments" $ do
            splitByCollapsed ':' "::alpha::beta::" @?= ["alpha", "beta"]

        , testCase "splitByCollapsed returns [] when the string only has delimiters" $ do
            splitByCollapsed ':' "::::" @?= []

        , testCase "splitByComma delegates to splitBy" $ do
            splitByComma "x,,y" @?= ["x", "", "y"]

        , testCase "splitByCommaCollapsed yields [] on empty input" $ do
            splitByCommaCollapsed "" @?= []

        , testCase "splitByCommaCollapsed collapses trailing delimiter chains" $ do
            splitByCommaCollapsed "one,two,,," @?= ["one", "two"]
        ]

    , testGroup "Comment stripping"
        [ testCase "removeLineComments respects string L.and char literals" $ do
            let input = unlines
                  [ "value := 1 // drop"
                  , "url := \"https://example.com//path\" // not part of literal"
                  , "char := '/' // keep literal"
                  ]
                expected = unlines
                  [ "value := 1 "
                  , "url := \"https://example.com//path\" "
                  , "char := '/' "
                  ]
            removeLineComments input @?= expected

        , testCase "removeLineComments keeps escaped quotes inside strings" $ do
            let input = "title := \"She said \\\"// hi\\\"\" // trailing\n"
                expected = "title := \"She said \\\"// hi\\\"\" \n"
            removeLineComments input @?= expected

        , testCase "removeLineComments maintains blank lines when an entire line is a comment" $ do
            let input = unlines
                  [ "// only comment"
                  , "actual := 42"
                  ]
                expected = unlines
                  [ ""
                  , "actual := 42"
                  ]
            removeLineComments input @?= expected

        , testCase "removeComments removes multiline block comments L.and retains line information" $ do
            let input = "value := 1 /* block\nstill block */ done\n"
                expected = unlines
                  [ "value := 1 "
                  , " done"
                  ]
            removeComments input @?= expected

        , testCase "removeComments removes sequential inline block comments" $ do
            let input = "value /* first */ /* second */ done\n"
                expected = "value   done\n"
            removeComments input @?= expected

        , testCase "removeComments ignores comment markers that appear inside strings L.or char literals" $ do
            let input = unlines
                  [ "path := \"C://tmp/*keep*/\" /* drop */"
                  , "rune := '/' /* meaning */"
                  , "fmt := \"/* not a block */\" // trailing"
                  ]
                expected = unlines
                  [ "path := \"C://tmp/*keep*/\" "
                  , "rune := '/' "
                  , "fmt := \"/* not a block */\" "
                  ]
            removeComments input @?= expected

        , testCase "removeComments preserves file-leading block comments" $ do
            let input = "/* header */\nconfig := 1\n"
                expected = "\nconfig := 1\n"
            removeComments input @?= expected

        , testCase "removeComments tolerates unterminated block comments by dropping the remainder" $ do
            let input = unlines
                  [ "start /* open"
                  , "still inside"
                  ]
                expected = unlines
                  [ "start "
                  , ""
                  ]
            removeComments input @?= expected
        ]

    , testGroup "Indentation helpers"
        [ testCase "normalizeIndentation removes the common leading indentation but keeps structure" $ do
            let snippet = unlines
                  [ "        func main() {"
                  , "            fmt.Println(\"hi\")"
                  , "        }"
                  , ""
                  ]
                expected = unlines
                  [ "func main() {"
                  , "    fmt.Println(\"hi\")"
                  , "}"
                  , ""
                  ]
            normalizeIndentation snippet @?= expected

        , testCase "normalizeIndentation keeps leading blank lines intact" $ do
            let snippet = unlines
                  [ ""
                  , "    guard := true"
                  , "      body := 42"
                  ]
                expected = unlines
                  [ ""
                  , "guard := true"
                  , "  body := 42"
                  ]
            normalizeIndentation snippet @?= expected

        , testCase "normalizeIndentation leaves purely whitespace input untouched" $ do
            let snippet = "   \n\t\n"
            normalizeIndentation snippet @?= snippet

        , testCase "forceSingleTabIndentation enforces a single leading tab on non-empty lines" $ do
            let snippet = unlines
                  [ "  alpha  "
                  , ""
                  , "\tbeta"
                  ]
                expected = unlines
                  [ "\talpha"
                  , ""
                  , "\tbeta"
                  ]
            forceSingleTabIndentation snippet @?= expected

        , testCase "forceSingleTabIndentation collapses whitespace-only lines" $ do
            let snippet = unlines
                  [ " line"
                  , "   "
                  , "next"
                  ]
                expected = unlines
                  [ "\tline"
                  , ""
                  , "\tnext"
                  ]
            forceSingleTabIndentation snippet @?= expected

        , testCase "fixIndentation is an alias for normalizeIndentation" $ do
            let snippet = unlines
                  [ "    level1"
                  , "      level2"
                  ]
            fixIndentation snippet @?= normalizeIndentation snippet
        ]

    , testGroup "Search helpers"
        [ testCase "breakOn returns prefix L.and suffix when the pattern exists" $ do
            breakOn "ll" "hello" @?= ("he", "o")

        , testCase "breakOn falls back to the original string when the pattern is missing" $ do
            breakOn "xyz" "hello" @?= ("hello", "")

        , testCase "breakOn with an empty pattern returns the whole string as the suffix" $ do
            breakOn "" "abc" @?= ("", "abc")

        , testCase "breakOn returns empty suffix when the pattern matches the entire string" $ do
            breakOn "abc" "abc" @?= ("", "")
        ]

    , testGroup "Property-based regression"
        [ fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "splitByCollapsed never yields empty chunks" prop_splitByCollapsedNoEmpty
        ]
    ]

prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
  let once = trim input
  in trim once == once

prop_splitByCollapsedNoEmpty :: String -> Bool
prop_splitByCollapsedNoEmpty input = L.all (not . null) (splitByCollapsed ':' input)
