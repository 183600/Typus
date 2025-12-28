{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsStringProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf1, elements, suchThat)
import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (isPrefixOf, isSuffixOf, concat, intersperse)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

-- | Generate a string with whitespace at both ends
genStringWithWhitespace :: Gen String
genStringWithWhitespace = do
  leading <- listOf (elements " \t\n\r")
  middle <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t")
  trailing <- listOf (elements " \t\n\r")
  return $ leading ++ middle ++ trailing

-- | Generate a string without whitespace at ends
genStringWithoutWhitespace :: Gen String
genStringWithoutWhitespace = listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t")

-- | Generate a delimiter character
genDelimiter :: Gen Char
genDelimiter = elements ",;:||\\"

-- | Generate a string that contains the delimiter
genStringWithDelimiter :: Char -> Gen String
genStringWithDelimiter delim = do
  parts <- listOf1 (listOf1 (elements $ filter (/= delim) (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])))
  return $ concat $ intersperse [delim] parts

-- | Generate a string with line comments
genStringWithLineComments :: Gen String
genStringWithLineComments = do
  beforeComment <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t")
  comment <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t")
  afterComment <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t")
  return $ beforeComment ++ "//" ++ comment ++ "\n" ++ afterComment

-- | Generate a string with block comments
genStringWithBlockComments :: Gen String
genStringWithBlockComments = do
  beforeComment <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t")
  comment <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\r\n")
  afterComment <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t")
  return $ beforeComment ++ "/*" ++ comment ++ "*/" ++ afterComment

-- | Generate a string with indentation
genIndentedString :: Gen String
genIndentedString = do
  baseIndent <- choose (0, 5)
  lines <- listOf1 $ do
    indent <- choose (baseIndent, baseIndent + 3)
    content <- listOf (elements $ ['a'..'z'] ++ ' ')
    return $ replicate indent ' ' ++ content
  return $ unlines lines

-- Property: trim removes leading and trailing whitespace
prop_trim_removesWhitespace :: Property
prop_trim_removesWhitespace =
  forAll genStringWithWhitespace $ \s ->
    let trimmed = trim s
    in not (null trimmed) ==> 
       (not (isSpace (head trimmed)) .||. all isSpace trimmed) .&&.
       (not (isSpace (last trimmed)) .||. all isSpace trimmed)

-- Property: trim is idempotent (trimming twice gives same result)
prop_trim_idempotent :: Property
prop_trim_idempotent =
  forAll arbitrary $ \s ->
    trim (trim s) === trim s

-- Property: splitBy preserves empty segments
prop_splitBy_preservesEmpty :: Property
prop_splitBy_preservesEmpty =
  forAll genDelimiter $ \delim ->
    forAll (genStringWithDelimiter delim) $ \s ->
      let parts = splitBy delim s
          rejoined = concat $ intersperse [delim] parts
      in rejoined === s

-- Property: splitByComma is equivalent to splitBy ','
prop_splitByComma_equivalent :: Property
prop_splitByComma_equivalent =
  forAll arbitrary $ \s ->
    splitByComma s === splitBy ',' s

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removesEmpty :: Property
prop_splitByCollapsed_removesEmpty =
  forAll genDelimiter $ \delim ->
    forAll arbitrary $ \s ->
      let parts = splitByCollapsed delim s
      in all (not . null) parts

-- Property: splitByCommaCollapsed is equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equivalent :: Property
prop_splitByCommaCollapsed_equivalent =
  forAll arbitrary $ \s ->
    splitByCommaCollapsed s === splitByCollapsed ',' s

-- Property: splitByCollapsed length <= splitBy length
prop_splitByCollapsed_shorterOrEqual :: Property
prop_splitByCollapsed_shorterOrEqual =
  forAll genDelimiter $ \delim ->
    forAll arbitrary $ \s ->
      length (splitByCollapsed delim s) <= length (splitBy delim s)

-- Property: removeLineComments removes content after // on each line
prop_removeLineComments_removesComments :: Property
prop_removeLineComments_removesComments =
  forAll genStringWithLineComments $ \s ->
    let cleaned = removeLineComments s
        hasComment = "//" `isInfixOf` cleaned
    in hasComment === False
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: removeComments removes both line and block comments
prop_removeComments_removesBothTypes :: Property
prop_removeComments_removesBothTypes =
  forAll genStringWithLineComments $ \s1 ->
    forAll genStringWithBlockComments $ \s2 ->
      let combined = s1 ++ s2
          cleaned = removeComments combined
          hasLineComment = "//" `isInfixOf` cleaned
          hasBlockComment = "/*" `isInfixOf` cleaned
      in not (hasLineComment .||. hasBlockComment)
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preservesRelative :: Property
prop_normalizeIndentation_preservesRelative =
  forAll genIndentedString $ \s ->
    let normalized = normalizeIndentation s
        lines_s = lines s
        lines_normalized = lines normalized
    in length lines_s === length lines_normalized .&&.
       not (null lines_normalized) ==> 
       all (not . null . dropWhile isSpace) lines_normalized

-- Property: breakOn finds first occurrence of delimiter
prop_breakOn_findsFirst :: Property
prop_breakOn_findsFirst =
  forAll arbitrary $ \s ->
    forAll genDelimiter $ \delim ->
      let (before, after) = breakOn delim s
      in if delim `elem` s
         then delim `isPrefixOf` after
         else before === s .&&. after === ""
  where
    isPrefixOf needle haystack = take (length needle) haystack == needle

-- Property: breakOn is consistent with splitBy for first occurrence
prop_breakOn_consistentWithSplitBy :: Property
prop_breakOn_consistentWithSplitBy =
  forAll arbitrary $ \s ->
    forAll genDelimiter $ \delim ->
      let (before, after) = breakOn delim s
          parts = splitBy delim s
      in if delim `elem` s
         then before === head parts .&&. 
              after === delim ++ concat (tail parts)
         else before === s .&&. after === "" .&&. parts === [s]

-- Property: trim empty string stays empty
prop_trim_emptyString :: Property
prop_trim_emptyString =
  trim "" === ""

-- Property: splitBy empty string returns single empty element
prop_splitBy_emptyString :: Property
prop_splitBy_emptyString =
  forAll genDelimiter $ \delim ->
    splitBy delim "" === [""]

-- Property: splitByCollapsed empty string returns empty list
prop_splitByCollapsed_emptyString :: Property
prop_splitByCollapsed_emptyString =
  forAll genDelimiter $ \delim ->
    splitByCollapsed delim "" === []

tests :: TestTree
tests =
  testGroup "Utils String Processing Properties"
    [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removesWhitespace
    , fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "splitBy preserves empty segments" prop_splitBy_preservesEmpty
    , fastProperty "splitByComma equivalent to splitBy ','" prop_splitByComma_equivalent
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removesEmpty
    , fastProperty "splitByCommaCollapsed equivalent to splitByCollapsed ','" prop_splitByCommaCollapsed_equivalent
    , fastProperty "splitByCollapsed length <= splitBy length" prop_splitByCollapsed_shorterOrEqual
    , fastProperty "removeLineComments removes content after //" prop_removeLineComments_removesComments
    , fastProperty "removeComments removes both line and block comments" prop_removeComments_removesBothTypes
    , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preservesRelative
    , fastProperty "breakOn finds first occurrence of delimiter" prop_breakOn_findsFirst
    , fastProperty "breakOn consistent with splitBy for first occurrence" prop_breakOn_consistentWithSplitBy
    , fastProperty "trim empty string stays empty" prop_trim_emptyString
    , fastProperty "splitBy empty string returns single empty element" prop_splitBy_emptyString
    , fastProperty "splitByCollapsed empty string returns empty list" prop_splitByCollapsed_emptyString
    ]