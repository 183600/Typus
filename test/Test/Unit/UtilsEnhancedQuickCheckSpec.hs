{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.UtilsEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
             removeLineComments, removeComments, normalizeIndentation, breakOn)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

tests :: TestTree
tests = testGroup "Utils Enhanced QuickCheck Tests"
  [ trimProperties
  , splitProperties
  , commentProperties
  , indentationProperties
  , searchProperties
  ]

-- | Trim function properties
trimProperties :: TestTree
trimProperties = testGroup "Trim Properties"
  [ testProperty "trim removes leading L.and trailing whitespace" $
      \s -> trim s === dropWhile isSpace (L.reverse (dropWhile isSpace (L.reverse s)))
  
  , testProperty "trim idempotent" $
      \s -> trim (trim s) === trim s
  
  , testProperty "trim of whitespace-only string is empty" $
      \s -> L.all isSpace s ==> trim s === ""
  
  , testProperty "trim preserves non-whitespace content" $
      \s -> not (L.all isSpace s) ==> not (L.null (trim s)) || L.any (not . isSpace) s
  ]

-- | Split function properties
splitProperties :: TestTree
splitProperties = testGroup "Split Properties"
  [ testProperty "splitBy preserves total L.length" $
      \c s -> L.sum (map L.length (splitBy c s)) === L.length s
  
  , testProperty "splitByCollapsed removes empty segments" $
      \c s -> L.all (not . null) (splitByCollapsed c s)
  
  , testProperty "splitByComma equals splitBy with comma" $
      \s -> splitByComma s === splitBy ',' s
  
  , testProperty "splitByCommaCollapsed equals splitByCollapsed with comma" $
      \s -> splitByCommaCollapsed s === splitByCollapsed ','
  
  , testProperty "splitBy on empty string returns single empty segment" $
      \c -> splitBy c "" === [""]
  
  , testProperty "splitByCollapsed on empty string returns empty list" $
      \c -> splitByCollapsed c "" === []
  
  , testProperty "splitBy on string with only delimiter returns empty segments" $
      \c n -> n > 0 ==> splitBy c (replicate n c) === replicate (n + 1) ""
  ]

-- | Comment removal properties
commentProperties :: TestTree
commentProperties = testGroup "Comment Properties"
  [ testProperty "removeLineComments removes // comments" $
      \prefix comment suffix -> 
        let input = prefix ++ "//" ++ comment ++ "\n" ++ suffix
            result = removeLineComments input
        in "//" `L.isInfixOf` result === False
  
  , testProperty "removeLineComments preserves line breaks" $
      \lines -> 
        let input = unlines lines
            result = removeLineComments input
        in L.length (lines result) === L.length lines
  
  , testProperty "removeComments removes both // L.and /* */ comments" $
      \prefix comment suffix -> 
        let input1 = prefix ++ "//" ++ comment ++ "\n" ++ suffix
            input2 = prefix ++ "/*" ++ comment ++ "*/" ++ suffix
            result1 = removeComments input1
            result2 = removeComments input2
        in "//" `L.isInfixOf` result1 === False .&&. "/*" `L.isInfixOf` result2 === False
  
  , testProperty "removeComments preserves string literals" $
      \s1 s2 -> 
        let input = "\"" ++ s1 ++ "\" // comment\n\"" ++ s2 ++ "\""
            result = removeComments input
        in s1 `L.isInfixOf` result .&&. s2 `L.isInfixOf` result
  
  , testProperty "removeComments preserves character literals" $
      \c1 c2 -> 
        let input = "'" ++ [c1] ++ "' // comment\n'" ++ [c2] ++ "'"
            result = removeComments input
        in c1 `elem` result .&&. c2 `elem` result
  
  , testProperty "removeComments handles escaped quotes in strings" $
      \s -> 
        let input = "\"\\\"" ++ s ++ "\\\"\" // comment"
            result = removeComments input
        in "\\\"" `L.isInfixOf` result
  ]

-- | Indentation normalization properties  
indentationProperties :: TestTree
indentationProperties = testGroup "Indentation Properties"
  [ testProperty "normalizeIndentation preserves relative indentation" $
      \lines -> 
        let input = unlines lines
            result = normalizeIndentation input
            resultLines = lines result
        in L.length resultLines === L.length lines
  
  , testProperty "normalizeIndentation preserves non-empty content" $
      \lines -> 
        let hasContent = L.any (not . L.all isSpace) lines
            input = unlines lines
            result = normalizeIndentation input
            resultHasContent = L.any (not . L.all isSpace) (lines result)
        in hasContent ==> resultHasContent
  
  , testProperty "normalizeIndentation removes common prefix indentation" $
      \lines -> 
        let nonEmpty = L.filter (not . L.all isSpace) lines
            input = unlines lines
            result = normalizeIndentation input
            resultLines = lines result
            leadingSpaces line = L.length (takeWhile isSpace line)
        in not (null nonEmpty) ==> 
           let minLeading = L.minimum (map leadingSpaces nonEmpty)
               resultLeading = map leadingSpaces (L.filter (not . L.all isSpace) resultLines)
           in L.all (<= minLeading) resultLeading
  ]

-- | Search function properties
searchProperties :: TestTree
searchProperties = testGroup "Search Properties"
  [ testProperty "breakOn finds pattern when present" $
      \prefix pattern suffix -> 
        not (null pattern) ==> 
        let input = prefix ++ pattern ++ suffix
            (before, after) = breakOn pattern input
        in pattern `L.isPrefixOf` (prefix ++ pattern) .&&. 
           before ++ pattern ++ after === input
  
  , testProperty "breakOn returns original string when pattern not found" $
      \s pattern -> 
        not (pattern `L.isInfixOf` s) ==> 
        let (before, after) = breakOn pattern s
        in before === s .&&. after === ""
  
  , testProperty "breakOn with empty pattern" $
      \s -> 
        let (before, after) = breakOn "" s
        in before === "" .&&. after === s
  
  , testProperty "breakOn is deterministic" $
      \s pattern -> 
        let (before1, after1) = breakOn pattern s
            (before2, after2) = breakOn pattern s
        in (before1, after1) === (before2, after2)
  ]