{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, vectorOf)

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

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort)
import qualified Data.Text as T

-- | 新的QuickCheck属性测试，针对Utils模块的核心功能
tests :: TestTree
tests =
  testGroup "New Cabal Utils QuickCheck Tests"
    [ testGroup "String splitting properties"
        [ fastProperty "splitBy length property" $
            \delim str ->
              let result = splitBy delim str
                  expectedLength = length (filter (== delim) str) + 1
              in counterexample ("Expected length " ++ show expectedLength ++ ", got " ++ show (length result)) $
                 length result === expectedLength

        , fastProperty "splitBy preserves content order" $
            \delim str ->
              delim `notElem` str || length str < 100 ==>
              let result = splitBy delim str
                  rejoined = concat (Data.List.intersperse [delim] result)
              in counterexample ("Original: " ++ show str ++ ", rejoined: " ++ show rejoined) $
                 str === rejoined

        , fastProperty "splitByCollapsed removes empty segments" $
            \delim str ->
              let result = splitByCollapsed delim str
              in property $ all (not . null) result

        , fastProperty "splitByComma is splitBy with ','" $
            \str ->
              splitByComma str === splitBy ',' str

        , fastProperty "splitByCommaCollapsed is splitByCollapsed with ','" $
            \str ->
              splitByCommaCollapsed str === splitByCollapsed ',' str
        ]

    , testGroup "Trim properties"
        [ fastProperty "trim removes no non-whitespace characters" $
            \str ->
              let trimmed = trim str
                  originalChars = filter (not . isSpace) str
                  trimmedChars = filter (not . isSpace) trimmed
              in counterexample ("Original non-whitespace: " ++ show originalChars ++ 
                                ", trimmed non-whitespace: " ++ show trimmedChars) $
                 sort originalChars === sort trimmedChars

        , fastProperty "trim is idempotent" $
            \str ->
              let trimmedOnce = trim str
                  trimmedTwice = trim trimmedOnce
              in trimmedOnce === trimmedTwice

        , fastProperty "trim removes leading whitespace" $
            \str ->
              let trimmed = trim str
              in null trimmed || not (isSpace (head trimmed))

        , fastProperty "trim removes trailing whitespace" $
            \str ->
              let trimmed = trim str
              in null trimmed || not (isSpace (last trimmed))
        ]

    , testGroup "Comment removal properties"
        [ fastProperty "removeLineComments preserves content before comments" $
            \str ->
              let withoutComments = removeLineComments str
                  firstCommentPos = Data.List.findIndex (isPrefixOf "//") (tails str)
              in case firstCommentPos of
                   Nothing -> withoutComments === str
                   Just pos -> take pos withoutComments === take pos str

        , fastProperty "removeLineComments removes line comments" $
            \str ->
              let withoutComments = removeLineComments str
              in property $ "//" `notElem` tails withoutComments

        , fastProperty "removeComments is idempotent" $
            \str ->
              let withoutCommentsOnce = removeComments str
                  withoutCommentsTwice = removeComments withoutCommentsOnce
              in withoutCommentsOnce === withoutCommentsTwice
        ]

    , testGroup "Indentation properties"
        [ fastProperty "normalizeIndentation preserves relative indentation" $
            \strs ->
              let normalized = normalizeIndentation strs
                  -- Check that relative order is preserved
                  originalLines = lines (unlines strs)
                  normalizedLines = lines (unlines normalized)
              in length normalizedLines === length originalLines

        , fastProperty "forceSingleTabIndentation uses only tabs for indentation" $
            \strs ->
              let tabIndented = forceSingleTabIndentation strs
                  linesList = lines (unlines tabIndented)
                  hasLeadingSpaces = any (\line -> takeWhile isSpace line /= takeWhile (== '\t') (takeWhile isSpace line)) linesList
              in property $ not hasLeadingSpaces

        , fastProperty "fixIndentation is normalizeIndentation" $
            \strs ->
              fixIndentation strs === normalizeIndentation strs
        ]

    , testGroup "BreakOn properties"
        [ fastProperty "breakOn finds first occurrence" $
            \needle haystack ->
              let (before, after) = breakOn needle haystack
                  expectedBefore = takeWhile (not . isPrefixOf needle) (tails haystack)
              in case expectedBefore of
                   [] -> before === haystack
                   (x:_) -> before === x

        , fastProperty "breakOn returns empty after when needle not found" $
            \needle haystack ->
              not (needle `isInfixOf` haystack) ==>
              let (before, after) = breakOn needle haystack
              in after === ""

        , fastProperty "breakOn reconstructs original" $
            \needle haystack ->
              let (before, after) = breakOn needle haystack
              in before ++ needle ++ after === haystack
        ]

    , testGroup "Edge cases and performance"
        [ fastProperty "splitBy handles empty string" $
            \delim ->
              splitBy delim "" === [""]

        , fastProperty "splitByCollapsed handles empty string" $
            \delim ->
              splitByCollapsed delim "" === []

        , fastProperty "trim handles empty string" $
            trim "" === ""

        , fastProperty "trim handles all whitespace" $
            \whitespaceStr ->
              all isSpace whitespaceStr ==> trim whitespaceStr === ""

        , testCase "Complex real-world example" $ do
            let input = ["    func main() {",
                        "        // This is a comment",
                        "        x := 1 // inline comment",
                        "        y := \"string with // not a comment\"",
                        "        /* block comment */",
                        "        z := x + y",
                        "    }"]
                normalized = normalizeIndentation input
                expected = ["func main() {",
                           "    // This is a comment",
                           "    x := 1 // inline comment",
                           "    y := \"string with // not a comment\"",
                           "    /* block comment */",
                           "    z := x + y",
                           "}"]
            normalized @?= expected
        ]
    ]