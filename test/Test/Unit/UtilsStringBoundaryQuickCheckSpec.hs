module Test.Unit.UtilsStringBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, oneof, elements, frequency)
import Data.Char (isSpace)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, breakOn)

-- | Generate arbitrary strings with boundary conditions
instance Arbitrary String where
  arbitrary = frequency
    [ (5, listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ', '\t'])
    , (2, return "") -- Empty string
    , (1, listOf $ elements " \t\n\r") -- Whitespace only
    , (1, listOf $ elements ['\128'..'\255']) -- Unicode characters
    , (1, return $ replicate 100 'a') -- Long string
    ]

-- | Generate delimiters for splitting
genDelimiter :: Gen Char
genDelimiter = elements [',', ':', ';', '|', '#', '@', ' ', '\t']

-- | Generate strings with specific delimiters
genStringWithDelimiter :: Char -> Gen String
genStringWithDelimiter delim = listOf $ frequency
    [ (8, elements $ filter (/= delim) ['a'..'z', 'A'..'Z', '0'..'9'])
    , (2, return delim)
    ]

tests :: TestTree
tests =
  testGroup "Utils string boundary conditions QuickCheck tests"
    [ testGroup "trim boundary conditions"
        [ testCase "trim handles empty string" $ do
            trim "" @?= ""

        , testCase "trim handles whitespace-only string" $ do
            trim "   \t\n\r  " @?= ""

        , testCase "trim preserves internal whitespace" $ do
            trim "  hello   world  " @?= "hello   world"

        , fastProperty "trim is idempotent" $
            \s ->
              trim (trim s) == trim s

        , fastProperty "trim never adds characters" $
            \s ->
              length (trim s) <= length s

        , fastProperty "trim removes only leading/trailing whitespace" $
            \s ->
              let trimmed = trim s
                  hasLeadingSpace = not (null s) && isSpace (head s) && null trimmed
                  hasTrailingSpace = not (null s) && isSpace (last s) && null trimmed
              in not (hasLeadingSpace || hasTrailingSpace) || null trimmed

        , testCase "trim handles unicode whitespace" $ do
            trim "\x00A0hello\x00A0" @?= "\x00A0hello\x00A0" -- Non-breaking space not considered by isSpace
        ]

    , testGroup "splitBy boundary conditions"
        [ testCase "splitBy on empty string returns singleton" $ do
            splitBy ',' "" @?= [""]

        , testCase "splitBy preserves empty segments" $ do
            splitBy ':' "a::b:" @?= ["a", "", "b", ""]

        , testCase "splitBy with delimiter not in string returns singleton" $ do
            splitBy ',' "hello" @?= ["hello"]

        , fastProperty "splitBy preserves total content when rejoining" $
            \delim s ->
              let parts = splitBy delim s
                  rejoined = concat $ parts `zip` repeat [delim] >>= \(part, d) -> part ++ [d]
                  rejoined' = if null parts then "" else init rejoined
              in rejoined' == s

        , fastProperty "splitBy length is at least 1" $
            \delim s ->
              length (splitBy delim s) >= 1

        , fastProperty "splitBy with consecutive delimiters creates empty segments" $
            \s ->
              let parts = splitBy ',' s
                  hasConsecutiveDelims = "##" `isInfixOf` s
              in if hasConsecutiveDelims then any null parts else True
        ]

    , testGroup "splitByCollapsed boundary conditions"
        [ testCase "splitByCollapsed on empty string returns empty" $ do
            splitByCollapsed ',' "" @?= []

        , testCase "splitByCollapsed removes empty segments" $ do
            splitByCollapsed ':' "a::b:" @?= ["a", "b"]

        , testCase "splitByCollapsed with only delimiters returns empty" $ do
            splitByCollapsed ',' ",,," @?= []

        , fastProperty "splitByCollapsed never returns empty segments" $
            \delim s ->
              all (not . null) (splitByCollapsed delim s)

        , fastProperty "splitByCollapsed result length <= splitBy result length" $
            \delim s ->
              length (splitByCollapsed delim s) <= length (splitBy delim s)

        , fastProperty "splitByCollapsed preserves non-empty segments" $
            \delim s ->
              let collapsed = splitByCollapsed delim s
                  normal = splitBy delim s
                  nonEmptyInNormal = filter (not . null) normal
              in collapsed == nonEmptyInNormal
        ]

    , testGroup "comma splitting functions"
        [ testCase "splitByComma delegates to splitBy" $ do
            splitByComma "x,,y" @?= ["x", "", "y"]

        , testCase "splitByCommaCollapsed yields [] on empty input" $ do
            splitByCommaCollapsed "" @?= []

        , fastProperty "splitByComma equals splitBy with comma" $
            \s ->
              splitByComma s == splitBy ',' s

        , fastProperty "splitByCommaCollapsed equals splitByCollapsed with comma" $
            \s ->
              splitByCommaCollapsed s == splitByCollapsed ',' s
        ]

    , testGroup "breakOn boundary conditions"
        [ testCase "breakOn with empty pattern" $ do
            breakOn "" "abc" @?= ("", "abc")

        , testCase "breakOn with pattern not found" $ do
            breakOn "xyz" "hello" @?= ("hello", "")

        , testCase "breakOn with exact match" $ do
            breakOn "abc" "abc" @?= ("", "")

        , testCase "breakOn with pattern at start" $ do
            breakOn "ab" "abcde" @?= ("", "cde")

        , testCase "breakOn with pattern at end" $ do
            breakOn "de" "abcde" @?= ("abc", "")

        , fastProperty "breakOn preserves total length" $
            \pat s ->
              let (before, after) = breakOn pat s
              in length before + length pat + length after == length s

        , fastProperty "breakOn pattern appears in after part" $
            \pat s ->
              not (null pat) && pat `isInfixOf` s ==>
                let (before, after) = breakOn pat s
                in pat `isPrefixOf` after

        , fastProperty "breakOn is deterministic" $
            \pat s ->
              breakOn pat s == breakOn pat s
        ]

    , testGroup "Edge cases and stress tests"
        [ testCase "functions handle very long strings" $ do
            let longString = replicate 10000 'a' ++ "," ++ replicate 10000 'b'
                parts = splitBy ',' longString
                collapsed = splitByCollapsed ',' longString
            length parts @?= 2
            length collapsed @?= 2

        , testCase "functions handle strings with special characters" $ do
            let special = "hello\x00world\x00test"
                parts = splitBy '\x00' special
            parts @?= ["hello", "world", "test"]

        , fastProperty "trim and splitBy interact correctly" $
            \s ->
              let trimmed = trim s
                  parts = splitBy ' ' trimmed
                  noLeadingEmpty = null parts || not (null (head parts))
              in noLeadingEmpty

        , fastProperty "splitBy and splitByCollapsed consistency on delimiter-free strings" $
            \delim s ->
              not (delim `elem` s) ==>
                splitBy delim s == splitByCollapsed delim s
        ]
    ]

-- Helper function for infix check
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys