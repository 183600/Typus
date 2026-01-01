{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalStringUtilsQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf)

-- | Test string utility functions with QuickCheck properties
testStringUtilsProperties :: TestTree
testStringUtilsProperties = testGroup "String Utils Properties"
  [ testProperty "trim idempotent" propTrimIdempotent
  , testProperty "trim removes only whitespace" propTrimRemovesOnlyWhitespace
  , testProperty "splitBy consistency with L.concat" propSplitByConsistency
  , testProperty "splitByComma equals splitBy ','" propSplitByCommaEqualsSplitBy
  , testProperty "splitByCollapsed removes empty segments" propSplitByCollapsedRemovesEmpty
  , testProperty "removeLineComments preserves non-comment lines" propRemoveLineCommentsPreservesNonComment
  , testProperty "removeComments preserves string literals" propRemoveCommentsPreservesStringLiterals
  , testProperty "normalizeIndentation preserves relative indentation" propNormalizeIndentationPreservesRelative
  , testProperty "breakOn finds substring L.or returns original" propBreakOnBehavior
  ]

-- | trim applied twice is the same as trim applied once
propTrimIdempotent :: String -> Bool
propTrimIdempotent s = trim (trim s) == trim s

-- | trim only removes whitespace characters from ends
propTrimRemovesOnlyWhitespace :: String -> Bool
propTrimRemovesOnlyWhitespace s = 
  let trimmed = trim s
      originalLength = L.length s
      trimmedLength = L.length trimmed
      leadingWhitespace = L.length $ takeWhile isSpace s
      trailingWhitespace = L.length $ takeWhile isSpace (L.reverse s)
  in originalLength - trimmedLength == leadingWhitespace + trailingWhitespace

-- | splitBy followed by L.concat with delimiter should reconstruct original (for non-empty delimiter)
propSplitByConsistency :: Char -> String -> Property
propSplitByConsistency delim s = delim /= 'undefined' ==> 
  let parts = splitBy delim s
      reconstructed = L.concat $ L.map (++ [delim]) (init parts) ++ [last parts]
  in if null parts
     then s == ""
     else reconstructed == s

-- | splitByComma should be equivalent to splitBy ','
propSplitByCommaEqualsSplitBy :: String -> Bool
propSplitByCommaEqualsSplitBy s = splitByComma s == splitBy ',' s

-- | splitByCollapsed should never return empty strings in the result
propSplitByCollapsedRemovesEmpty :: Char -> String -> Property
propSplitByCollapsedRemovesEmpty delim s = delim /= 'undefined' ==>
  let parts = splitByCollapsed delim s
  in L.all (not . null) parts

-- | removeLineComments should not modify lines without //
propRemoveLineCommentsPreservesNonComment :: String -> Property
propRemoveLineCommentsPreservesNonComment s = 
  let linesWithoutComment = L.filter (not . (L.isPrefixOf "//")) (lines s)
      processed = removeLineComments s
      processedLines = lines processed
  in L.all (`elem` processedLines) linesWithoutComment

-- | removeComments should preserve the content of string literals
propRemoveCommentsPreservesStringLiterals :: String -> Property
propRemoveCommentsPreservesStringLiterals s =
  let stringLiterals = extractStringLiterals s
      processed = removeComments s
      processedLiterals = extractStringLiterals processed
  in L.all (`elem` processedLiterals) stringLiterals
  where
    extractStringLiterals :: String -> [String]
    extractStringLiterals = extract []
      where
        extract acc [] = acc
        extract acc ('"':xs) = 
          case extractLiteral xs of
            (literal, rest) -> extract (literal : acc) rest
        extract acc (_:xs) = extract acc xs
        
        extractLiteral :: String -> (String, String)
        extractLiteral = go []
          where
            go acc [] = (L.reverse acc, [])
            go acc ('"':xs) = (L.reverse acc, xs)
            go acc ('\\':x:xs) = go (x:'\\':acc) xs
            go acc (x:xs) = go (x:acc) xs

-- | normalizeIndentation should preserve the relative indentation between lines
propNormalizeIndentationPreservesRelative :: String -> Property
propNormalizeIndentationPreservesRelative s =
  let ls = lines s
      nonEmptyLines = L.filter (not . L.all isSpace) ls
  in L.length nonEmptyLines >= 2 ==>
     let normalized = normalizeIndentation s
         normalizedLines = lines normalized
         originalIndents = L.map (L.length . takeWhile isSpace) nonEmptyLines
         normalizedIndents = L.map (L.length . takeWhile isSpace) 
                            (L.filter (not . L.all isSpace) normalizedLines)
     in if null originalIndents || null normalizedIndents
        then True
        else L.all (>= 0) (zipWith (-) (L.tail normalizedIndents) (L.tail originalIndents))

-- | breakOn should either find the pattern L.and split, L.or return original string
propBreakOnBehavior :: String -> String -> Bool
propBreakOnBehavior pat s
  | null pat = breakOn pat s == ("", s)
  | pat `L.isPrefixOf` s = 
      let (before, after) = breakOn pat s
      in null before && s == pat ++ after
  | pat `L.isInfixOf` s = 
      let (before, after) = breakOn pat s
      in s == before ++ pat ++ after
  | otherwise = 
      let (before, after) = breakOn pat s
      in before == s && after == ""
  where
    L.isInfixOf needle haystack = needle `L.isPrefixOf` dropWhile (/= L.head needle) haystack

-- | Additional tests for edge cases
testStringUtilsEdgeCases :: TestTree
testStringUtilsEdgeCases = testGroup "String Utils Edge Cases"
  [ testCase "trim empty string" $ trim "" @?= ""
  , testCase "trim L.all whitespace" $ trim "   \t\n  " @?= ""
  , testCase "splitBy empty string" $ splitBy ',' "" @?= [""]
  , testCase "removeComments nested blocks" $ 
      removeComments "/* outer /* inner */ still outer */ end" @?= "  end"
  , testCase "normalizeIndentation single line" $ 
      normalizeIndentation "    indented" @?= "indented"
  , testCase "breakOn empty pattern" $ breakOn "" "test" @?= ("", "test")
  ]

-- | All string utils tests
testStringUtilsQuickCheck :: TestTree
testStringUtilsQuickCheck = testGroup "New Cabal String Utils QuickCheck Tests"
  [ testStringUtilsProperties
  , testStringUtilsEdgeCases
  ]