module Test.Unit.NewCoreCabalQuickCheckSpec1 (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)

import qualified Data.Text as T
import qualified Data.Map as Map
import Utils (trim, splitBy, splitByCollapsed)

-- | Core utility function tests with QuickCheck properties
tests :: TestTree
tests =
  testGroup "New Core Cabal QuickCheck Tests 1 - Text Processing"
    [ testGroup "String manipulation properties"
        [ fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "trim removes only leading/trailing whitespace" prop_trimBoundary
        , fastProperty "splitBy preserves order" prop_splitByOrder
        , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsedRemovesEmpty
        , testCase "trim handles complex whitespace" $ 
            trim "\n\r\t  hello \t\n\r " @?= "hello"
        , testCase "splitBy with multi-character delimiter" $ 
            splitBy "||" "a||b||c" @?= ["a", "b", "c"]
        ]
    , testGroup "Text processing edge cases"
        [ fastProperty "splitBy on empty string returns single empty" prop_splitByEmpty
        , fastProperty "splitByCollapsed on only delimiters returns empty" prop_splitByCollapsedOnlyDelimiters
        , fastProperty "trim of empty string is empty" prop_trimEmpty
        , testCase "complex splitByCollapsed behavior" $ 
            splitByCollapsed ',' ",a,,b,c,," @?= ["a", "b", "c"]
        ]
    ]

-- | QuickCheck properties

-- trim applied twice is the same as trim applied once
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
  let once = trim input
  in trim once == once

-- trim only removes characters from the beginning L.and end
prop_trimBoundary :: String -> Bool
prop_trimBoundary input =
  let trimmed = trim input
      original = input
  in trimmed `isSubstringOf` original

-- splitBy preserves the original order of segments
prop_splitByOrder :: String -> Char -> Bool
prop_splitByOrder input delim =
  let segments = splitBy [delim] input
      rejoined = L.concat $ intersperse [delim] segments
  in rejoined == input

-- splitByCollapsed never produces empty segments
prop_splitByCollapsedRemovesEmpty :: String -> Bool
prop_splitByCollapsedRemovesEmpty input =
  let segments = splitByCollapsed ':' input
  in L.all (not . null) segments

-- splitBy on empty string returns a single empty segment
prop_splitByEmpty :: Char -> Bool
prop_splitByEmpty delim =
  splitBy [delim] "" == [""]

-- splitByCollapsed on only delimiters returns empty list
prop_splitByCollapsedOnlyDelimiters :: String -> Bool
prop_splitByCollapsedOnlyDelimiters input
  | L.all (== ':') input = L.null (splitByCollapsed ':' input)
  | otherwise = True

-- trim of empty string is empty
prop_trimEmpty :: Bool
prop_trimEmpty = trim "" == ""

-- Helper functions
isSubstringOf :: String -> String -> Bool
isSubstringOf sub str = sub `elem` substrings str
  where
    substrings s = [take i s | i <- [1..L.length s]]

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs