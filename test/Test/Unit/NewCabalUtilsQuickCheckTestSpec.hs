module Test.Unit.NewCabalUtilsQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)

import Utils
import TestSupport.QuickCheck (fastProperty)

-- | QuickCheck tests for Utils module string processing functions
tests :: TestTree
tests =
  testGroup "New Cabal Utils QuickCheck Tests"
    [ testProperty "trim: removing whitespace twice is same as once" prop_trimIdempotent
    , testProperty "trim: result has no leading/trailing whitespace" prop_trimNoWhitespace
    , testProperty "splitBy: concatenating results with delimiter gives original" prop_splitByRoundtrip
    , testProperty "splitByCollapsed: no empty segments in result" prop_splitByCollapsedNoEmpty
    , testProperty "splitByComma: splitBy ',' should equal splitByComma" prop_splitByCommaEquivalence
    , testProperty "removeLineComments: removing comments twice is same as once" prop_removeLineCommentsIdempotent
    , testProperty "removeComments: removing comments twice is same as once" prop_removeCommentsIdempotent
    , testProperty "normalizeIndentation: applying twice is same as once" prop_normalizeIndentationIdempotent
    , testProperty "breakOn: pattern not found returns original string" prop_breakOnNotFound
    , testProperty "breakOn: pattern found at start returns empty prefix" prop_breakOnAtStart
    , testGroup "Edge cases"
        [ testCase "trim handles empty string" $ trim "" @?= ""
        , testCase "trim handles only whitespace" $ trim "   \t\n  " @?= ""
        , testCase "splitBy handles empty string" $ splitBy ',' "" @?= [""]
        , testCase "splitByCollapsed handles only delimiters" $ splitByCollapsed ',' ",,," @?= []
        , testCase "removeLineComments handles empty input" $ removeLineComments "" @?= ""
        , testCase "removeComments handles empty input" $ removeComments "" @?= ""
        , testCase "normalizeIndentation handles empty input" $ normalizeIndentation "" @?= ""
        , testCase "breakOn with empty pattern returns whole string as suffix" $
            breakOn "" "hello" @?= ("", "hello")
        ]
    ]

-- | Property: trim is idempotent (applying twice gives same result)
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input = trim (trim input) == trim input

-- | Property: trim result has no leading or trailing whitespace
prop_trimNoWhitespace :: String -> Bool  
prop_trimNoWhitespace input =
  let trimmed = trim input
  in null trimmed || 
     (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

-- | Property: splitBy roundtrip with delimiter
prop_splitByRoundtrip :: Char -> String -> Bool
prop_splitByRoundtrip delim input = 
  let parts = splitBy delim input
      reconstructed = intercalateWith delim parts
  in reconstructed == input
  where
    intercalateWith :: Char -> [String] -> String
    intercalateWith _ [] = ""
    intercalateWith _ [x] = x
    intercalateWith d (x:xs) = x : d : intercalateWith d xs

-- | Property: splitByCollapsed never returns empty segments
prop_splitByCollapsedNoEmpty :: Char -> String -> Bool
prop_splitByCollapsedNoEmpty delim input = 
  all (not . null) (splitByCollapsed delim input)

-- | Property: splitByComma equals splitBy ','
prop_splitByCommaEquivalence :: String -> Bool
prop_splitByCommaEquivalence input = splitByComma input == splitBy ',' input

-- | Property: removeLineComments is idempotent
prop_removeLineCommentsIdempotent :: String -> Bool
prop_removeLineCommentsIdempotent input = 
  let once = removeLineComments input
  in removeLineComments once == once

-- | Property: removeComments is idempotent
prop_removeCommentsIdempotent :: String -> Bool
prop_removeCommentsIdempotent input = 
  let once = removeComments input
  in removeComments once == once

-- | Property: normalizeIndentation is idempotent
prop_normalizeIndentationIdempotent :: String -> Bool
prop_normalizeIndentationIdempotent input = 
  let once = normalizeIndentation input
  in normalizeIndentation once == once

-- | Property: breakOn when pattern not found returns original as prefix
prop_breakOnNotFound :: String -> String -> Property
prop_breakOnNotFound pat input = 
  not (pat `isInfixOf` input) ==> 
  breakOn pat input === (input, "")

-- | Property: breakOn when pattern at start returns empty prefix
prop_breakOnAtStart :: String -> String -> Property
prop_breakOnAtStart pat suffix = 
  not (null pat) && pat `isPrefixOf` suffix ==>
  breakOn pat (pat ++ suffix) === ("", suffix)

-- | Helper function to check if a string is a substring of another
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = 
  let haystackLen = length haystack
      needleLen = length needle
  in any (\start -> take needleLen (drop start haystack) == needle) [0..haystackLen - needleLen]