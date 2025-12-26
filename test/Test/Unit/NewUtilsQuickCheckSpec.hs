{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewUtilsQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Utils
import Data.Char (isSpace)
import qualified Data.Text as T

-- | Test string splitting properties
spec :: Spec
spec = describe "NewUtils QuickCheck Tests" $ do

  describe "splitBy properties" $ do
    it "preserves empty segments when splitting" $ property $
      \c str -> 
        let result = splitBy c str
            expected = map T.unpack $ T.split (== c) (T.pack str)
        in result === expected

    it "splitByCollapsed removes empty segments" $ property $
      \c str ->
        let result = splitByCollapsed c str
            withEmpty = splitBy c str
        in all (not . null) result && 
           result === filter (not . null) withEmpty

    it "splitting and joining with single character preserves original" $ property $
      \c str ->
        let result = splitBy c str
        in not (null str) ==> (c `elem` str) ==> (concat (intersperse [c] result) === str)

  describe "trim properties" $ do
    it "trim removes leading and trailing whitespace" $ property $
      \str ->
        let trimmed = trim str
        in (not (null trimmed) || all isSpace str) &&
           (null trimmed || not (isSpace (head trimmed))) &&
           (null trimmed || not (isSpace (last trimmed)))

    it "trim is idempotent" $ property $
      \str -> trim (trim str) === trim str

    it "trim of all whitespace returns empty" $ property $
      \str -> all isSpace str ==> trim str === ""

  describe "removeLineComments properties" $ do
    it "preserves lines without comments" $ property $
      \str -> not ('/' `elem` str) ==> removeLineComments str === str

    it "removes content after // on each line" $ property $
      \line1 line2 ->
        let input = line1 ++ "//comment\n" ++ line2 ++ "//another"
            result = removeLineComments input
            lines' = lines result
        in length lines' === 2 &&
           all (not . isSubstringOf "//") lines'

    it "preserves // inside string literals" $ property $
      \before after ->
        let input = before ++ "\"string with // inside\" more" ++ after
            result = removeLineComments input
        in "// inside" `isSubstringOf` result

  describe "removeComments properties" $ do
    it "preserves strings without comment markers" $ property $
      \str -> not ('/' `elem` str) ==> removeComments str === str

    it "removes both line and block comments" $ property $
      \code ->
        let withComments = code ++ "// line comment\n/* block\ncomment */" ++ code
            result = removeComments withComments
        in not ("// line comment" `isSubstringOf` result) &&
           not ("/* block" `isSubstringOf` result) &&
           code `isSubstringOf` result

  describe "normalizeIndentation properties" $ do
    it "preserves relative indentation" $ property $
      \lines' ->
        let input = unlines lines'
            normalized = normalizeIndentation input
            resultLines = lines normalized
        in length resultLines === length lines'

    it "removes common prefix indentation" $ property $
      \indent content ->
        let indentedLine = replicate indent ' ' ++ content
            input = unlines [indentedLine, indentedLine]
            normalized = normalizeIndentation input
            resultLines = lines normalized
        in all (not . isPrefixOf "    ") resultLines

  describe "breakOn properties" $ do
    it "finds first occurrence of pattern" $ property $
      \pat str ->
        let (before, after) = breakOn pat str
        in if null pat 
           then before === "" && after === str
           else not (pat `isInfixOf` before) &&
                (pat `isInfixOf` str ==> before ++ pat ++ after === str)

    it "returns original string when pattern not found" $ property $
      \pat str -> not (pat `isInfixOf` str) ==> 
        let (before, after) = breakOn pat str
        in before === str && after === ""

  where
    isSubstringOf substring string = substring `isInfixOf` string
    intersperse _ [] = []
    intersperse sep (x:xs) = x : sep : intersperse sep xs
    isPrefixOf prefix str = take (length prefix) str == prefix
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]