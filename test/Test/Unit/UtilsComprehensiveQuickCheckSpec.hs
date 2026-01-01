{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.UtilsComprehensiveQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, breakOn)
import TestSupport.Arbitrary ()

-- | Test suite for Utils module with comprehensive QuickCheck properties
utilsComprehensiveQuickCheckSpec :: TestTree
utilsComprehensiveQuickCheckSpec = testGroup "Utils Comprehensive QuickCheck Tests"
  [ trimProperties
  , splitProperties
  , commentProperties
  , indentationProperties
  , breakOnProperties
  ]

-- | Properties for trim function
trimProperties :: TestTree
trimProperties = testGroup "Trim Properties"
  [ testProperty "trim removes leading L.and trailing whitespace" $
      \s -> trim ("  " ++ s ++ "  ") == trim s
  
  , testProperty "trim idempotent" $
      \s -> trim (trim s) == trim s
  
  , testProperty "trim preserves internal whitespace" $
      \s -> not (null s) ==> 
        let trimmed = trim s
        in L.length (L.filter (== ' ') trimmed) >= L.length (L.filter (== ' ') s) - 2
  
  , testProperty "trim of empty string is empty" $
      trim "" == ""
  
  , testProperty "trim of only whitespace is empty" $
      \ws -> L.all (`elem` " \t\n\r") ws ==> trim ws == ""
  ]

-- | Properties for split functions
splitProperties :: TestTree
splitProperties = testGroup "Split Properties"
  [ testProperty "splitBy preserves empty segments" $
      \delim s -> not (null delim) ==> 
        L.concat (splitBy delim s) == s
  
  , testProperty "splitByCollapsed removes empty segments" $
      \delim s -> not (null delim) ==> 
        let collapsed = splitByCollapsed delim
            normal = splitBy delim
        in L.all (not . null) (collapsed s) && 
           L.length (collapsed s) <= L.length (normal s)
  
  , testProperty "splitByComma equals splitBy ','" $
      \s -> splitByComma s == splitBy ',' s
  
  , testProperty "splitByCommaCollapsed equals splitByCollapsed ','" $
      \s -> splitByCommaCollapsed s == splitByCollapsed ',' s
  
  , testProperty "splitting empty string returns single empty segment" $
      \delim -> not (null delim) ==> splitBy delim "" == [""]
  
  , testProperty "splitting by character not in string returns original" $
      \s delim -> not (null s) && not (delim `elem` s) ==> splitBy delim s == [s]
  ]

-- | Properties for comment removal functions
commentProperties :: TestTree
commentProperties = testGroup "Comment Properties"
  [ testProperty "removeLineComments removes // comments" $
      \prefix suffix -> 
        let input = prefix ++ "// this is a comment\n" ++ suffix
            result = removeLineComments input
        in "// this is a comment" `notElem` lines result
  
  , testProperty "removeLineComments preserves non-comment content" $
      \content -> not (L.any (`elem` "//") content) ==> 
        removeLineComments content == content
  
  , testProperty "removeComments removes both // L.and /* */ comments" $
      \prefix middle suffix ->
        let input = prefix ++ "// line comment\n" ++ middle ++ "/* block comment */" ++ suffix
            result = removeComments input
        in "// line comment" `notElem` lines result &&
           "/* block comment */" `notElem` result
  
  , testProperty "removeComments preserves string literals" $
      \strContent -> 
        let input = "// comment\n\"" ++ strContent ++ "\"\n// another comment"
            result = removeComments input
        in "\"" ++ strContent ++ "\"" `L.isInfixOf` result
  
  , testProperty "comment removal idempotent" $
      \s -> removeComments (removeComments s) == removeComments s
  ]
  where
    L.isInfixOf needle haystack = needle `elem` [take (L.length needle) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]

-- | Properties for indentation functions
indentationProperties :: TestTree
indentationProperties = testGroup "Indentation Properties"
  [ testProperty "normalizeIndentation preserves relative indentation" $
      \lines1 lines2 ->
        let input = unlines ["  " ++ lines1, "    " ++ lines2]
            result = normalizeIndentation input
            resultLines = lines result
        in L.length resultLines == 2 &&
           L.all (not . L.isPrefixOf "  ") resultLines
  
  , testProperty "normalizeIndentation preserves empty lines" $
      \lines1 lines2 lines3 ->
        let input = unlines ["  " ++ lines1, "", "    " ++ lines2, "", "  " ++ lines3]
            result = normalizeIndentation input
            resultLines = lines result
        in L.length (filter null resultLines) == 2
  
  , testProperty "normalizeIndentation of empty string is empty" $
      normalizeIndentation "" == ""
  
  , testProperty "normalizeIndentation preserves line count" $
      \s -> let ls = lines s
                result = normalizeIndentation s
                resultLines = lines result
            in L.length ls == L.length resultLines
  ]
  where
    L.isPrefixOf needle haystack = take (L.length needle) haystack == needle

-- | Properties for breakOn function
breakOnProperties :: TestTree
breakOnProperties = testGroup "BreakOn Properties"
  [ testProperty "breakOn with empty pattern returns (\"\", input)" $
      \s -> breakOn "" s == ("", s)
  
  , testProperty "breakOn finds pattern when present" $
      \prefix pat suffix -> not (null pat) =>
        let input = prefix ++ pat ++ suffix
            (before, after) = breakOn pat input
        in before == prefix && after == suffix
  
  , testProperty "breakOn returns (input, \"\") when pattern not found" $
      \s pat -> not (null pat) && not (pat `L.isInfixOf` s) ==> 
        breakOn pat s == (s, "")
  
  , testProperty "breakOn with pattern at start returns (\"\", rest)" $
      \pat suffix -> not (null pat) =>
        let input = pat ++ suffix
            (before, after) = breakOn pat input
        in before == "" && after == suffix
  
  , testProperty "breakOn with pattern at end returns (prefix, \"\")" $
      \prefix pat -> not (null pat) =>
        let input = prefix ++ pat
            (before, after) = breakOn pat input
        in before == prefix && after == ""
  ]
  where
    L.isInfixOf needle haystack = needle `elem` [take (L.length needle) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]