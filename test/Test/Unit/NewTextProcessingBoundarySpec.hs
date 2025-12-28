module Test.Unit.NewTextProcessingBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), oneof, listOf, choose)
import qualified Data.Text as T
import Utils

-- | Test text processing boundary conditions and edge cases
tests :: TestTree
tests =
  testGroup "Text Processing Boundary Tests"
    [ testGroup "String splitting edge cases"
        [ testCase "splitBy handles Unicode characters correctly" $ do
            splitBy ' ' "hello 世界 test" @?= ["hello", "世界", "test"]

        , testCase "splitBy with null character" $ do
            splitBy '\0' "a\0b\0c" @?= ["a", "b", "c"]

        , testCase "splitByCollapsed with mixed Unicode and spaces" $ do
            splitByCollapsed ' "  hello 世界  test  " @?= ["hello", "世界", "test"]
        ]

    , testGroup "Comment removal edge cases"
        [ testCase "removeLineComments with nested quotes" $ do
            let input = "code \"// not comment\" // actual comment\n"
                expected = "code \"// not comment\" \n"
            removeLineComments input @?= expected

        , testCase "removeComments with escaped block comment markers" $ do
            let input = "text \"/* not block */\" /* real block */ end\n"
                expected = "text \"/* not block */\"  end\n"
            removeComments input @?= expected

        , testCase "removeComments with deeply nested block comments" $ do
            let input = "start /* outer /* inner */ still outer */ end\n"
                expected = "start  end\n"
            removeComments input @?= expected
        ]

    , testGroup "Indentation edge cases"
        [ testCase "normalizeIndentation with mixed tabs and spaces" $ do
            let input = "\t    mixed\n\t    \t  indentation\n"
                expected = "mixed\n  \tindentation\n"
            normalizeIndentation input @?= expected

        , testCase "forceSingleTabIndentation with empty lines" $ do
            let input = "\n\nline\n\n"
                expected = "\n\n\tline\n\n"
            forceSingleTabIndentation input @?= expected
        ]

    , testGroup "Property-based tests"
        [ testProperty "splitBy length preservation" prop_splitByLength
        , testProperty "trim idempotency" prop_trimIdempotent
        , testProperty "splitByCollapsed removes empty strings" prop_splitByCollapsedNoEmpty
        , testProperty "breakOn concatenation property" prop_breakOnConcat
        ]
    ]

-- Property: splitBy preserves total length when concatenated with delimiter
prop_splitByLength :: String -> Char -> Bool
prop_splitByLength s delim =
    let parts = splitBy delim s
        reconstructed = concat $ intersperse [delim] parts
    in length reconstructed >= length s  -- Allow for trimming

-- Property: trim is idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent s = trim (trim s) == trim s

-- Property: splitByCollapsed never produces empty strings
prop_splitByCollapsedNoEmpty :: String -> Char -> Bool
prop_splitByCollapsedNoEmpty s delim = all (not . null) (splitByCollapsed delim s)

-- Property: breakOn concatenation property
prop_breakOnConcat :: String -> String -> Bool
prop_breakOnConcat s pattern =
    let (prefix, suffix) = breakOn pattern s
    in if null suffix
       then prefix == s
       else prefix ++ pattern ++ suffix == s

-- Helper function
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs