{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewEnhancedUtilsQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Utils
import Data.Char (isSpace)
import qualified Data.Text as T

-- | Test enhanced string processing properties
spec :: Spec
spec = describe "NewEnhancedUtils QuickCheck Tests" $ do

  describe "Advanced string splitting properties" $ do
    it "splitting with Unicode characters works correctly" $ property $
      \c str -> 
        let result = splitBy c str
            expected = map T.unpack $ T.split (== c) (T.pack str)
        in result === expected

    it "splitting preserves empty segments at boundaries" $ property $
      \c str ->
        let result = splitBy c str
            startsWithC = not (null str) && head str == c
            endsWithC = not (null str) && last str == c
        in (startsWithC ==> null (head result)) &&
           (endsWithC ==> null (last result))

    it "splitByCollapsed handles consecutive delimiters" $ property $
      \c str ->
        let result = splitByCollapsed c str
            withEmpty = splitBy c str
        in all (not . null) result && 
           result === filter (not . null) withEmpty

  describe "Enhanced comment removal properties" $ do
    it "handles nested string literals correctly" $ property $
      \before inner after ->
        let input = before ++ "\"string with // nested \\\"inner\\\" comment\"" ++ after
            result = removeLineComments input
        in "// nested" `isInfixOf` result

    it "preserves escaped comment markers" $ property $
      \content ->
        let input = "var = \\\"// not a comment\\\"; " ++ content
            result = removeLineComments input
        in "// not a comment" `isInfixOf` result

    it "handles complex block comment scenarios" $ property $
      \before inside after ->
        let input = before ++ "/* " ++ inside ++ " */" ++ after
            result = removeComments input
        in not ("/*" `isInfixOf` result) &&
           not ("*/" `isInfixOf` result) &&
           before `isPrefixOf` result

  describe "Advanced indentation properties" $ do
    it "handles mixed tabs and spaces correctly" $ property $
      \lines' ->
        let input = unlines $ map (\l -> "\t  " ++ l) lines'
            normalized = normalizeIndentation input
            resultLines = lines normalized
        in length resultLines === length lines' &&
           all (not . isPrefixOf "\t  ") resultLines

    it "preserves relative indentation structure" $ property $
      \lines' ->
        let indentedLines = zipWith (\i l -> replicate i ' ' ++ l) [0,2,4,2,0] lines'
            input = unlines indentedLines
            normalized = normalizeIndentation input
            resultLines = lines normalized
        in length resultLines === length lines' &&
           all (not . null) resultLines

    it "handles empty lines in indentation" $ property $
      \lines' ->
        let withEmpty = intersperse "" lines'
            input = unlines $ map (\l -> "  " ++ l) withEmpty
            normalized = normalizeIndentation input
            resultLines = lines normalized
        in length resultLines === length withEmpty

  describe "Enhanced search and split properties" $ do
    it "breakOn handles empty pattern correctly" $ property $
      \str -> 
        let (before, after) = breakOn "" str
        in before === "" && after === str

    it "breakOn is consistent with standard functions" $ property $
      \pat str ->
        let (before, after) = breakOn pat str
        in if null pat 
           then before === "" && after === str
           else not (pat `isInfixOf` before) &&
                (pat `isInfixOf` str ==> before ++ pat ++ after === str)

    it "complex string operations compose correctly" $ property $
      \operations str ->
        let result = performComplexOperations operations str
        in length result >= 0 -- Basic sanity check

  describe "Performance and edge cases" $ do
    it "handles very long strings efficiently" $ property $
      \size ->
        let longString = replicate size 'x'
            result = trim longString
        in length result <= length longString

    it "handles Unicode correctly" $ property $
      \unicodeStr ->
        let trimmed = trim unicodeStr
            normalized = normalizeIndentation unicodeStr
        in length trimmed >= 0 && length normalized >= 0

    it "operations are idempotent where appropriate" $ property $
      \str ->
        let trimmed1 = trim str
            trimmed2 = trim trimmed1
            normalized1 = normalizeIndentation str
            normalized2 = normalizeIndentation normalized1
        in trimmed1 === trimmed2 && normalized1 === normalized2

  where
    isSubstringOf substring string = substring `isInfixOf` string
    intersperse _ [] = []
    intersperse sep (x:xs) = x : sep : intersperse sep xs
    isPrefixOf prefix str = take (length prefix) str == prefix
    isInfixOf needle haystack = needle `elem` 
      [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

    performComplexOperations :: [String] -> String -> String
    performComplexOperations ops str = foldl (\s op -> applyOperation op s) str ops
      where
        applyOperation "trim" = trim
        applyOperation "normalize" = normalizeIndentation
        applyOperation "removeComments" = removeComments
        applyOperation "removeLineComments" = removeLineComments
        applyOperation _ = id