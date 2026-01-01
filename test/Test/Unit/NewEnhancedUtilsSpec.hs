module Test.Unit.NewEnhancedUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, (===), forAll, Gen, choose, arbitrary, listOf1, elements)
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

-- ============================================================================
-- Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf1 $ elements $ " \t\n\r" ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ",.;:!()[]{}<>+-*/%=|&^~?@#"

-- Generate strings with delimiters for split testing
genDelimiterString :: Char -> Gen String
genDelimiterString delim = listOf1 $ elements [delim, 'a', 'b', 'c', ' ', delim, delim]

-- Generate strings with potential comment patterns
genCommentString :: Gen String
genCommentString = listOf1 $ elements $ "//" ++ "/*" ++ "*/" ++ "\"'" ++ ['a'..'z'] ++ [' '] ++ "\n"

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: trim is idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input = trim (trim input) == trim input

-- Property: trim removes only leading/trailing whitespace
prop_trimPreservesInternal :: String -> String -> Bool
prop_trimPreservesInternal prefix suffix =
  let middle = "hello world"
      input = prefix ++ middle ++ suffix
      trimmed = trim input
  in middle `L.isInfixOf` trimmed

-- Property: splitBy L.and splitByCollapsed relationship
prop_splitByCollapsed :: Char -> String -> Bool
prop_splitByCollapsed delim str =
  let normal = splitBy delim str
      collapsed = splitByCollapsed delim str
  in L.all (not . null) collapsed && 
     L.length collapsed <= L.length normal

-- Property: splitByComma is splitBy with comma
prop_splitByComma :: String -> Bool
prop_splitByComma str = splitByComma str == splitBy ',' str

-- Property: splitByCommaCollapsed is splitByCollapsed with comma
prop_splitByCommaCollapsed :: String -> Bool
prop_splitByCommaCollapsed str = splitByCommaCollapsed str == splitByCollapsed ',' str

-- Property: breakOn returns correct split when pattern exists
prop_breakOnFound :: String -> String -> Property
prop_breakOnFound pattern text = 
  pattern `L.isInfixOf` text ==>
  let (prefix, suffix) = breakOn pattern text
  in prefix ++ pattern ++ suffix == text

-- Property: normalizeIndentation preserves relative structure
prop_normalizeIndentationPreservesLines :: String -> Bool
prop_normalizeIndentationPreservesLines input =
  let normalized = normalizeIndentation input
      inputLines = lines input
      normalizedLines = lines normalized
  in L.length inputLines == L.length normalizedLines

-- Property: fixIndentation is alias for normalizeIndentation
prop_fixIndentationAlias :: String -> Bool
prop_fixIndentationAlias input = fixIndentation input == normalizeIndentation input

-- Helper function to check if a string is contained in another
isInfixOf :: Eq a => [a] -> [a] -> Bool
L.isInfixOf needle haystack = L.any (L.isPrefixOf needle) (tails haystack)
  where
    L.isPrefixOf [] _ = True
    L.isPrefixOf _ [] = False
    L.isPrefixOf (x:xs) (y:ys) = x == y && L.isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "New Enhanced Utils Tests"
  [ testGroup "String Processing Properties"
    [ testProperty "trim is idempotent" prop_trimIdempotent
    , testProperty "trim preserves internal content" prop_trimPreservesInternal
    , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed
    , testProperty "splitByComma uses comma delimiter" prop_splitByComma
    , testProperty "splitByCommaCollapsed uses comma delimiter" prop_splitByCommaCollapsed
    , testProperty "breakOn correctly splits when pattern found" prop_breakOnFound
    , testProperty "normalizeIndentation preserves line count" prop_normalizeIndentationPreservesLines
    , testProperty "fixIndentation is normalizeIndentation alias" prop_fixIndentationAlias
    ]

  , testGroup "Advanced Whitespace Handling"
    [ testCase "trim handles mixed whitespace types" $ do
        trim "\t\n  hello  \r\n\t" @?= "hello"

    , testCase "trim handles empty string" $ do
        trim "" @?= ""

    , testCase "trim handles whitespace-only string" $ do
        trim "   \t\n\r  " @?= ""

    , testCase "splitBy handles Unicode characters" $ do
        splitBy '·' "a·b·c" @?= ["a", "b", "c"]

    , testCase "splitByCollapsed handles consecutive delimiters" $ do
        splitByCollapsed '|' "a|||b||c" @?= ["a", "b", "c"]
    ]

  , testGroup "Comment Processing Edge Cases"
    [ testCase "removeLineComments handles empty lines" $ do
        let input = "\n\n"
        removeLineComments input @?= input

    , testCase "removeLineComments handles lines with only whitespace" $ do
        let input = "   \t  \n  \t\n"
        removeLineComments input @?= input

    , testCase "removeLineComments handles multiple comments per line" $ do
        let input = "code // comment 1 // comment 2\n"
            expected = "code  \n"
        removeLineComments input @?= expected

    , testCase "removeComments handles nested comment markers in strings" $ do
        let input = "text \"/* not a comment */\" more /* real comment */ end\n"
            expected = "text \"/* not a comment */\" more  end\n"
        removeComments input @?= expected

    , testCase "removeComments handles unterminated block comment" $ do
        let input = "start /* unterminated\nmore content\n"
            expected = "start \n\n"
        removeComments input @?= expected
    ]

  , testGroup "Indentation Normalization"
    [ testCase "normalizeIndentation handles mixed tabs L.and spaces" $ do
        let input = "\t    mixed\n\t    indentation\n"
            expected = "mixed\nindentation\n"
        normalizeIndentation input @?= expected

    , testCase "normalizeIndentation preserves trailing empty lines" $ do
        let input = "    content\n\n\n"
        normalizeIndentation input @?= "content\n\n\n"

    , testCase "forceSingleTabIndentation handles already tabbed content" $ do
        let input = "\talready\n\ttabbed\n"
        forceSingleTabIndentation input @?= input

    , testCase "forceSingleTabIndentation converts spaces to tabs" $ do
        let input = "    spaces\n        more\n"
            expected = "\tspaces\n\t\tmore\n"
        forceSingleTabIndentation input @?= expected
    ]

  , testGroup "Search Function Edge Cases"
    [ testCase "breakOn with empty pattern returns whole string as suffix" $ do
        breakOn "" "hello" @?= ("", "hello")

    , testCase "breakOn with pattern not found returns whole string as prefix" $ do
        breakOn "xyz" "hello" @?= ("hello", "")

    , testCase "breakOn with pattern at beginning" $ do
        breakOn "hello" "hello world" @?= ("", " world")

    , testCase "breakOn with pattern at end" $ do
        breakOn "world" "hello world" @?= ("hello ", "")

    , testCase "breakOn with multiple occurrences returns first" $ do
        breakOn "ab" "ababcab" @?= ("", "abcab")
    ]
  ]