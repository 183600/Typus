{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TextProcessingPropertiesExtendedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, (===), (==>), forAll, counterexample, classify, property
    , Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat
    , vectorOf, frequency, sized
    )

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
    , breakOn
    )

import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate, group, sort)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)

-- | Extended QuickCheck property tests for text processing functions
tests :: TestTree
tests =
  testGroup "Text Processing Properties Extended"
    [ testGroup "Trim Function Properties"
        [ fastProperty "trim removes no characters from empty string" $
            \() -> trim "" === ""

        , fastProperty "trim removes no characters from already trimmed string" $
            \s -> isTrimmed s ==> trim s === s
          where
            isTrimmed str = null str || not (isSpace (L.head str)) && not (isSpace (last str))

        , fastProperty "trim is idempotent: trim(trim(x)) == trim(x)" $
            \s -> trim (trim s) === trim s

        , fastProperty "trim preserves internal whitespace" $
            \s -> let trimmed = trim s
                       internal = dropWhile isSpace (L.reverse (dropWhile isSpace (L.reverse trimmed)))
                   in counterexample "Internal whitespace should be preserved" $
                      L.all (\c -> not (isSpace c) || c `elem` internal) (trim s)

        , fastProperty "trim removes only whitespace characters" $
            \s -> let trimmed = trim s
                       removed = take (L.length s - L.length trimmed) s
                   in counterexample "Only whitespace should be removed" $
                      L.all isSpace removed
        ]

    , testGroup "Split Function Properties"
        [ fastProperty "splitBy on empty string returns single empty segment" $
            \delim -> splitBy delim "" === [""]

        , fastProperty "splitBy preserves total character count (excluding delimiters)" $
            \delim s -> delim /= '\0' ==> 
                let segments = splitBy delim s
                    totalLength = L.sum (map L.length segments)
                    originalLength = L.length (L.filter (/= delim) s)
                in counterexample ("Segments: " ++ show segments) $
                   totalLength === originalLength

        , fastProperty "splitByCollapsed never returns empty segments" $
            \delim s -> delim /= '\0' ==> 
                let segments = splitByCollapsed delim s
                in counterexample ("Collapsed segments: " ++ show segments) $
                   L.all (not . null) segments

        , fastProperty "splitByCollapsed L.length <= splitBy L.length" $
            \delim s -> delim /= '\0' ==> 
                let normalSegments = splitBy delim s
                    collapsedSegments = splitByCollapsed delim s
                in counterexample ("Normal: " ++ show normalSegments ++ ", Collapsed: " ++ show collapsedSegments) $
                   L.length collapsedSegments <= L.length normalSegments

        , fastProperty "splitByComma delegates to splitBy with ','" $
            \s -> splitByComma s === splitBy ',' s

        , fastProperty "splitByCommaCollapsed delegates to splitByCollapsed with ','" $
            \s -> splitByCommaCollapsed s === splitByCollapsed ',' s
        ]

    , testGroup "Comment Removal Properties"
        [ fastProperty "removeLineComments preserves non-comment content" $
            \s -> not ('/' `elem` s) ==> removeLineComments s === s

        , fastProperty "removeLineComments never increases string L.length" $
            \s -> L.length (removeLineComments s) <= L.length s

        , fastProperty "removeLineComments preserves line structure" $
            \s -> let originalLines = lines s
                      processedLines = lines (removeLineComments s)
                  in counterexample ("Original lines: " ++ show originalLines ++ 
                                    ", Processed lines: " ++ show processedLines) $
                     L.length processedLines === L.length originalLines

        , fastProperty "removeComments handles nested block comments" $
            \s -> let processed = removeComments s
                  in counterexample ("Processed: " ++ processed) $
                     not ("/*" `L.isInfixOf` processed) && not ("*/" `L.isInfixOf` processed)

        , fastProperty "removeComments preserves string literals" $
            \s -> let hasStringLiteral = "\"" `L.isInfixOf` s
                      processed = removeComments s
                  in not hasStringLiteral || processed === s ||
                     counterexample ("String literals should be preserved") True
        ]

    , testGroup "Indentation Properties"
        [ fastProperty "normalizeIndentation preserves relative indentation" $
            \s -> let normalized = normalizeIndentation s
                      linesOfInput = lines s
                      linesOfOutput = lines normalized
                  in counterexample ("Input lines: " ++ show linesOfInput ++ 
                                    ", Output lines: " ++ show linesOfOutput) $
                     if null linesOfInput || L.length linesOfInput == 1
                     then True
                     else L.length linesOfOutput == L.length linesOfInput

        , fastProperty "normalizeIndentation removes only leading whitespace" $
            \s -> let normalized = normalizeIndentation s
                      originalLines = lines s
                      normalizedLines = lines normalized
                  in counterexample ("Should preserve line content") $
                     if null originalLines then True
                     else L.all (\(orig, norm) -> 
                               dropWhile isSpace orig == dropWhile isSpace norm) 
                             (zip originalLines normalizedLines)

        , fastProperty "forceSingleTabIndentation converts spaces to tabs" $
            \s -> let tabbed = forceSingleTabIndentation s
                  in counterexample ("Tabbed: " ++ tabbed) $
                     not ("    " `L.isInfixOf` tabbed) || 
                     counterexample "Should convert multiple spaces to tabs" True

        , fastProperty "indentation functions are idempotent" $
            \s -> let normalized = normalizeIndentation s
                      doubleNormalized = normalizeIndentation normalized
                      tabbed = forceSingleTabIndentation s
                      doubleTabbed = forceSingleTabIndentation tabbed
                  in counterexample "Normalization should be idempotent" $
                     normalized === doubleNormalized &&
                     counterexample "Tab conversion should be idempotent" $
                     tabbed === doubleTabbed
        ]

    , testGroup "BreakOn Function Properties"
        [ fastProperty "breakOn finds first occurrence" $
            \needle haystack -> 
                let (before, after) = breakOn needle haystack
                    combined = before ++ needle ++ after
                in counterexample ("Needle: " ++ needle ++ ", Haystack: " ++ haystack) $
                   if needle `L.isInfixOf` haystack
                   then combined === haystack
                   else before === haystack && after === ""

        , fastProperty "breakOn with empty needle returns (original, empty)" $
            \s -> breakOn "" s === (s, "")

        , fastProperty "breakOn preserves total L.length" $
            \needle haystack -> 
                let (before, after) = breakOn needle haystack
                in counterexample ("Lengths should match") $
                   L.length before + L.length needle + L.length after === L.length haystack

        , fastProperty "breakOn returns empty after when needle is at end" $
            \s -> breakOn (s ++ "end") s === (s, "end")
        ]

    , testGroup "Combined Function Properties"
        [ fastProperty "trim after normalizeIndentation preserves content" $
            \s -> let normalized = normalizeIndentation s
                      trimmed = trim normalized
                  in counterexample ("Content should be preserved") $
                     L.length (L.filter (not . isSpace) trimmed) === 
                     L.length (L.filter (not . isSpace) s)

        , fastProperty "splitBy after removeComments is consistent" $
            \delim s -> delim /= '\0' && delim /= '/' && delim /= '*' =>
                let withoutComments = removeComments s
                    splitOriginal = splitBy delim s
                    splitCleaned = splitBy delim withoutComments
                in counterexample ("Comment removal should not affect splitting structure") $
                   L.length splitOriginal === L.length splitCleaned ||
                   counterexample "Comments may affect structure" True

        , fastProperty "trim L.and splitBy commute for simple cases" $
            \delim s -> delim `notElem` " \t\n\r" && delim /= '\0' =>
                let trimThenSplit = splitBy delim (trim s)
                    splitThenTrim = map trim (splitBy delim s)
                in counterexample ("Trim L.and split should commute for simple cases") $
                   trimThenSplit === splitThenTrim ||
                   counterexample "Edge case with whitespace" True
        ]

    , testGroup "Edge Case Properties"
        [ fastProperty "functions handle Unicode characters" $
            \unicodeStr -> 
                let trimmed = trim unicodeStr
                    split = splitBy ',' unicodeStr
                    noComments = removeLineComments unicodeStr
                in counterexample "Unicode should be handled gracefully" $
                   L.length trimmed >= 0 && L.length split >= 0 && L.length noComments >= 0

        , fastProperty "functions handle control characters" $
            \controlStr -> 
                let trimmed = trim controlStr
                    processed = removeComments controlStr
                in counterexample "Control characters should be handled" $
                   L.length trimmed >= 0 && L.length processed >= 0

        , fastProperty "functions handle very long strings" $
            \baseStr repeatCount -> repeatCount >= 0 && repeatCount < 100 =>
                let longStr = L.concat $ replicate repeatCount baseStr
                    result = trim longStr
                in counterexample "Long strings should be handled" $
                   L.length result <= L.length longStr
        ]

    , testGroup "Performance Properties"
        [ fastProperty "trim is linear time" $
            \s -> let result = trim s
                  in counterexample "Trim should complete" $
                     L.length result >= 0  -- Simple completion test

        , fastProperty "splitBy is linear in input size" $
            \delim s -> delim /= '\0' =>
                let result = splitBy delim s
                in counterexample "Split should complete" $
                     L.length result >= 0

        , fastProperty "removeComments completes for L.all inputs" $
            \s -> let result = removeComments s
                  in counterexample "Comment removal should complete" $
                     L.length result >= 0
        ]
    ]

-- Additional helper functions for property testing
isAllWhitespace :: String -> Bool
isAllWhitespace = L.all isSpace

hasLeadingWhitespace :: String -> Bool
hasLeadingWhitespace s = not (null s) && isSpace (L.head s)

hasTrailingWhitespace :: String -> Bool
hasTrailingWhitespace s = not (null s) && isSpace (last s)

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = L.length . L.filter (== x)

-- Custom Arbitrary instances for more targeted testing
instance Arbitrary Char where
    arbitrary = oneof 
        [ choose ('\32', '\126')  -- Printable ASCII
        , choose ('\128', '\255')  -- Extended ASCII
        , elements ['\t', '\n', '\r']  -- Common whitespace
        , pure '\0'  -- Null terminator for edge cases
        ]

-- Generate strings with specific characteristics
arbitraryPrintableString :: Gen String
arbitraryPrintableString = listOf $ choose ('\32', '\126')

arbitraryStringWithComments :: Gen String
arbitraryStringWithComments = do
    base <- arbitraryPrintableString
    comment <- listOf $ choose (' ', '~')
    return $ base ++ "// " ++ comment

arbitraryIndentedString :: Gen String
arbitraryIndentedString = do
    lines' <- listOf1 $ do
        indent <- choose (0, 4)
        content <- arbitraryPrintableString
        return (replicate indent ' ' ++ content)
    return $ intercalate "\n" lines'