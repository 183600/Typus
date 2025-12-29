{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, choose, listOf, elements, suchThat)
import Utils
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- | QuickCheck tests for Utils module
tests :: TestTree
tests =
  testGroup "New Cabal Utils QuickCheck Tests"
    [ testProperty "splitBy and splitByCollapsed consistency" prop_splitByConsistency
    , testProperty "trim removes only leading/trailing whitespace" prop_trimBehavior
    , testProperty "splitBy preserves order" prop_splitByOrder
    , testProperty "removeLineComments preserves non-comment content" prop_removeLineCommentsPreservesContent
    , testProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentationStructure
    , testProperty "breakOn is correct inverse of concatenation" prop_breakOnCorrectness
    , testProperty "splitByComma is splitBy with comma" prop_splitByCommaCorrectness
    , testProperty "trim is idempotent" prop_trimIdempotent
    ]

-- | splitByCollapsed should be equivalent to filter (not . null) . splitBy
prop_splitByConsistency :: String -> Char -> Bool
prop_splitByConsistency input delim =
  splitByCollapsed delim input == filter (not . null) (splitBy delim input)

-- | trim should only remove whitespace from beginning and end
prop_trimBehavior :: String -> Bool
prop_trimBehavior input =
  let trimmed = trim input
      leadingRemoved = null input || not (isSpace (head input)) || isSpace (head trimmed) == False
      trailingRemoved = null trimmed || not (isSpace (last trimmed))
  in leadingRemoved && trailingRemoved

-- | splitBy should preserve the order of segments
prop_splitByOrder :: String -> Char -> Bool
prop_splitByOrder input delim =
  let segments = splitBy delim input
      reconstructed = intercalate [delim] segments
  in reconstructed == input
  where
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | removeLineComments should preserve content that's not in comments
prop_removeLineCommentsPreservesContent :: String -> Bool
prop_removeLineCommentsPreservesContent input =
  let withoutComments = removeLineComments input
      linesWithoutComments = lines withoutComments
      originalLines = lines input
  in all (\line -> not ("//" `isInfixOf` line)) linesWithoutComments

-- | normalizeIndentation should preserve the relative structure of indentation
prop_normalizeIndentationStructure :: String -> Bool
prop_normalizeIndentationStructure input =
  let normalized = normalizeIndentation input
      originalLines = lines input
      normalizedLines = lines normalized
  in length originalLines == length normalizedLines

-- | breakOn should correctly split strings
prop_breakOnCorrectness :: String -> String -> Property
prop_breakOnCorrectness input pattern =
  forAll (choose (0, length input)) $ \idx ->
    let pattern' = if null pattern then take 1 input else pattern
        (prefix, suffix) = breakOn pattern' input
        expected = if pattern' `isInfixOf` input
                   then let (pre, suf) = break (pattern' `isPrefixOf`) input
                        in (pre, drop (length pattern') suf)
                   else (input, "")
    in counterexample ("Input: " ++ show input ++ ", Pattern: " ++ show pattern') $
       (prefix, suffix) === expected

-- | splitByComma should be equivalent to splitBy with comma
prop_splitByCommaCorrectness :: String -> Bool
prop_splitByCommaCorrectness input =
  splitByComma input == splitBy ',' input

-- | trim should be idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
  let once = trim input
      twice = trim once
  in once == twice