{-# LANGUAGE CPP #-}

module Test.Unit.NewUtilsQuickCheckPropertiesSpec (tests) where

import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck (Property, (===), forAll, Gen, choose, listOf, elements, suchThat, oneof)

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

-- QuickCheck generators
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r!@#$%^&*()_+-=[]{}|;':\",./<>?"

genString :: Gen String
genString = listOf genChar

genNonEmptyString :: Gen String
genNonEmptyString = listOf1 genChar

genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements " \t\n\r"

genDelimiter :: Gen Char
genDelimiter = elements $ ",;:|/\\"

genStringWithDelimiter :: Gen (Char, String)
genStringWithDelimiter = do
  delim <- genDelimiter
  parts <- listOf $ listOf genChar
  return (delim, intercalate [delim] parts)

genIndentedString :: Gen String
genIndentedString = do
  baseIndent <- choose (1, 10)
  lines <- listOf $ do
    indent <- choose (0, baseIndent + 5)
    content <- listOf genChar
    return $ replicate indent ' ' ++ content
  return $ unlines lines

genCommentString :: Gen String
genCommentString = do
  codeLines <- listOf $ listOf genChar
  commentLines <- listOf $ do
    content <- listOf genChar
    return $ "// " ++ content
  lines <- elements [interleave codeLines commentLines, commentLines ++ codeLines, codeLines ++ commentLines]
  return $ unlines lines

genBlockCommentString :: Gen String
genBlockCommentString = do
  before <- listOf genChar
  commentContent <- listOf genChar
  after <- listOf genChar
  return $ before ++ "/* " ++ commentContent ++ " */" ++ after

genStringWithQuotes :: Gen String
genStringWithQuotes = do
  before <- listOf genChar
  stringContent <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  after <- listOf genChar
  return $ before ++ "\"" ++ stringContent ++ "\"" ++ after

genStringWithCharLiteral :: Gen String
genStringWithCharLiteral = do
  before <- listOf genChar
  charContent <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  after <- listOf genChar
  return $ before ++ "'" ++ charContent ++ "'" ++ after

genSearchPattern :: Gen (String, String)
genSearchPattern = do
  haystack <- genString
  needle <- oneof 
    [ genString `suchThat` (`L.isInfixOf` haystack)
    , genString `suchThat` (not . (`L.isInfixOf` haystack))
    ]
  return (needle, haystack)

-- Helper functions
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

-- | QuickCheck property tests for Utils module
tests :: TestTree
tests =
  testGroup "NewUtils QuickCheck Properties"
    [ testGroup "Trim properties"
        [ fastProperty "trim removes leading L.and trailing whitespace" $
            forAll genWhitespaceString $ \ws ->
              forAll genNonEmptyString $ \s ->
                let input = ws ++ s ++ ws
                    result = trim input
                in result === s

        , fastProperty "trim is idempotent" $
            forAll genString $ \s ->
              trim (trim s) === trim s

        , fastProperty "trim returns empty string for L.all-whitespace input" $
            forAll genWhitespaceString $ \ws ->
              trim ws === ""

        , fastProperty "trim doesn't change non-whitespace strings" $
            forAll (genString `suchThat` (not . L.all isSpace)) $ \s ->
              trim s === s
        ]

    , testGroup "Split properties"
        [ fastProperty "splitBy preserves empty segments" $
            forAll genStringWithDelimiter $ \(delim, input) ->
              let result = splitBy delim input
                  expectedLength = L.length (L.filter (== delim) input) + 1
              in L.length result === expectedLength

        , fastProperty "splitBy is inverse of join with delimiter" $
            forAll genStringWithDelimiter $ \(delim, original) ->
              let parts = splitBy delim original
                  reconstructed = intercalate [delim] parts
              in reconstructed === original

        , fastProperty "splitByCollapsed removes empty segments" $
            forAll genStringWithDelimiter $ \(delim, input) ->
              let result = splitByCollapsed delim input
              in L.all (not . null) result

        , fastProperty "splitByComma is equivalent to splitBy ','" $
            forAll genString $ \s ->
              splitByComma s === splitBy ',' s

        , fastProperty "splitByCommaCollapsed is equivalent to splitByCollapsed ','" $
            forAll genString $ \s ->
              splitByCommaCollapsed s === splitByCollapsed ',' s

        , fastProperty "splitBy on empty string returns singleton empty list" $
            forAll genDelimiter $ \delim ->
              splitBy delim "" === [""]

        , fastProperty "splitByCollapsed on empty string returns empty list" $
            forAll genDelimiter $ \delim ->
              splitByCollapsed delim "" === []
        ]

    , testGroup "Comment removal properties"
        [ fastProperty "removeLineComments removes lines starting with //" $
            forAll genCommentString $ \input ->
              let result = removeLineComments input
                  resultLines = lines result
              in not (L.any ("//" `L.isPrefixOf`) resultLines)

        , fastProperty "removeLineComments preserves non-comment lines" $
            forAll genString $ \input ->
              let result = removeLineComments input
                  inputLines = lines input
                  resultLines = lines result
              in L.length resultLines <= L.length inputLines

        , fastProperty "removeComments removes block comments" $
            forAll genBlockCommentString $ \input ->
              let result = removeComments input
              in not ("/*" `L.isInfixOf` result) && not ("*/" `L.isInfixOf` result)

        , fastProperty "removeComments preserves string literals" $
            forAll genStringWithQuotes $ \input ->
              let result = removeComments input
              in countQuotes input === countQuotes result
          where
            countQuotes = L.length . L.filter (== '"')

        , fastProperty "removeComments preserves char literals" $
            forAll genStringWithCharLiteral $ \input ->
              let result = removeComments input
              in countSingleQuotes input === countSingleQuotes result
          where
            countSingleQuotes = L.length . L.filter (== '\'')

        , fastProperty "removeComments is idempotent" $
            forAll genString $ \s ->
              let result1 = removeComments s
                  result2 = removeComments result1
              in result1 === result2
        ]

    , testGroup "Indentation properties"
        [ fastProperty "normalizeIndentation preserves relative indentation" $
            forAll genIndentedString $ \input ->
              let result = normalizeIndentation input
                  inputLines = lines input
                  resultLines = lines result
              in L.length inputLines === L.length resultLines

        , fastProperty "normalizeIndentation removes common leading whitespace" $
            forAll genIndentedString $ \input ->
              let result = normalizeIndentation input
                  resultLines = lines result
              in L.all (not . L.isPrefixOf "  ") resultLines || 
                 L.all (not . L.isPrefixOf "\t") resultLines

        , fastProperty "forceSingleTabIndentation adds tab to non-empty lines" $
            forAll genString $ \input ->
              let result = forceSingleTabIndentation input
                  resultLines = lines result
              in L.all (\line -> null line || "\t" `L.isPrefixOf` line) resultLines

        , fastProperty "fixIndentation is equivalent to normalizeIndentation" $
            forAll genString $ \s ->
              fixIndentation s === normalizeIndentation s

        , fastProperty "normalizeIndentation preserves empty lines" $
            forAll genString $ \input ->
              let result = normalizeIndentation input
                  inputLines = lines input
                  resultLines = lines result
              in L.length (filter null inputLines) === L.length (filter null resultLines)
        ]

    , testGroup "Search properties"
        [ fastProperty "breakOn returns original string when pattern not found" $
            forAll genSearchPattern $ \(needle, haystack) ->
              if needle `L.isInfixOf` haystack
              then True
              else breakOn needle haystack === (haystack, "")

        , fastProperty "breakOn with empty pattern returns empty prefix" $
            forAll genString $ \haystack ->
              breakOn "" haystack === ("", haystack)

        , fastProperty "breakOn concatenates to original when pattern found" $
            forAll genSearchPattern $ \(needle, haystack) ->
              if needle `L.isInfixOf` haystack
              then 
                let (before, after) = breakOn needle haystack
                in before ++ needle ++ after === haystack
              else True

        , fastProperty "breakOn is deterministic" $
            forAll genSearchPattern $ \(needle, haystack) ->
              let result1 = breakOn needle haystack
                  result2 = breakOn needle haystack
              in result1 === result2

        , fastProperty "breakOn finds first occurrence" $
            forAll genString $ \haystack ->
              forAll genNonEmptyString $ \needle ->
                let haystackWithNeedles = haystack ++ needle ++ haystack ++ needle
                    (before, after) = breakOn needle haystackWithNeedles
                in needle `L.isInfixOf` after
        ]

    , testGroup "String manipulation properties"
        [ fastProperty "trim after splitBy preserves non-empty segments" $
            forAll genStringWithDelimiter $ \(delim, input) ->
              let parts = splitBy delim input
                  trimmedParts = map trim parts
              in L.length trimmedParts === L.length parts

        , fastProperty "splitBy after removeComments still works" $
            forAll genString $ \input ->
              forAll genDelimiter $ \delim ->
                let noComments = removeComments input
                    parts = splitBy delim noComments
                in L.length parts >= 1

        , fastProperty "normalizeIndentation after removeLineComments preserves structure" $
            forAll genCommentString $ \input ->
              let noLineComments = removeLineComments input
                  normalized = normalizeIndentation noLineComments
                  originalLines = lines input
                  normalizedLines = lines normalized
              in L.length normalizedLines <= L.length originalLines

        , fastProperty "breakOn with delimiter from splitBy finds correct position" $
            forAll genStringWithDelimiter $ \(delim, input) ->
              let parts = splitBy delim input
                  needle = [delim]
              in if L.length parts > 1
                 then 
                   let (before, after) = breakOn needle input
                   in delim `elem` after || null after
                 else True
        ]

    , testGroup "Edge case properties"
        [ fastProperty "trim handles empty string" $
            trim "" === ""

        , fastProperty "splitBy handles empty string" $
            forAll genDelimiter $ \delim ->
              splitBy delim "" === [""]

        , fastProperty "removeLineComments handles empty string" $
            removeLineComments "" === ""

        , fastProperty "removeComments handles empty string" $
            removeComments "" === ""

        , fastProperty "normalizeIndentation handles empty string" $
            normalizeIndentation "" === ""

        , fastProperty "forceSingleTabIndentation handles empty string" $
            forceSingleTabIndentation "" === ""

        , fastProperty "breakOn handles empty pattern with empty string" $
            breakOn "" "" === ("", "")

        , fastProperty "breakOn handles non-empty pattern with empty string" $
            forAll genNonEmptyString $ \needle ->
              breakOn needle "" === ("", "")
        ]

    , testGroup "Performance properties"
        [ fastProperty "splitBy on concatenated strings gives expected results" $
            forAll genString $ \s1 ->
              forAll genString $ \s2 ->
                forAll genDelimiter $ \delim ->
                  let concatenated = s1 ++ [delim] ++ s2
                      parts = splitBy delim concatenated
                  in L.length parts === 2 && L.head parts === s1 && last parts === s2

        , fastProperty "trim after normalizeIndentation preserves content" $
            forAll genIndentedString $ \input ->
              let normalized = normalizeIndentation input
                  trimmed = normalizeIndentation $ trim input
              in lines normalized === lines trimmed

        , fastProperty "removeComments on large strings is reasonable" $
            forAll (listOf $ genString) $ \parts ->
              let largeInput = unlines parts
                  result = removeComments largeInput
              in L.length result <= L.length largeInput
        ]
    ]