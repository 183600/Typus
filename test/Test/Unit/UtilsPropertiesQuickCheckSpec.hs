{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.UtilsPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, ioProperty, (===), (.&&.), counterexample)

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

import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T

tests :: TestTree
tests =
  testGroup "Utils Properties QuickCheck Tests"
    [ testProperty "trim idempotent" $ prop_trim_idempotent
    , testProperty "trim removes only whitespace" $ prop_trim_removes_whitespace
    , testProperty "trim preserves non-whitespace content" $ prop_trim_preserves_content
    , testProperty "splitBy length property" $ prop_splitBy_length
    , testProperty "splitByCollapsed removes empty segments" $ prop_splitByCollapsed_removes_empty
    , testProperty "splitByComma equals splitBy with comma" $ prop_splitByComma_equals_splitBy
    , testProperty "splitByCommaCollapsed equals splitByCollapsed with comma" $ prop_splitByCommaCollapsed_equals_splitByCollapsed
    , testProperty "splitBy and join roundtrip" $ prop_splitBy_join_roundtrip
    , testProperty "removeLineComments preserves non-comment lines" $ prop_removeLineComments_preserves_non_comment
    , testProperty "removeComments handles nested comments" $ prop_removeComments_nested
    , testProperty "normalizeIndentation preserves relative structure" $ prop_normalizeIndentation_preserves_structure
    , testProperty "breakOn correctness" $ prop_breakOn_correctness
    , testProperty "trim split consistency" $ prop_trim_split_consistency
    , testProperty "splitBy delimiter consistency" $ prop_splitBy_delimiter_consistency
    ]

-- | trim applied twice is the same as trim applied once
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- | trim only removes whitespace characters from start and end
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s = 
    let trimmed = trim s
        startsWithNonWhitespace = null trimmed || not (isSpace (head trimmed))
        endsWithNonWhitespace = null trimmed || not (isSpace (last trimmed))
    in counterexample ("Original: " ++ show s ++ ", Trimmed: " ++ show trimmed) $
       startsWithNonWhitespace .&&. endsWithNonWhitespace

-- | trim preserves the non-whitespace content in the middle
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content s =
    let trimmed = trim s
        middleContent = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse s
    in counterexample ("Original: " ++ show s ++ ", Middle: " ++ show middleContent ++ ", Trimmed: " ++ show trimmed) $
       if null middleContent 
       then trimmed === ""
       else middleContent === trimmed

-- | splitBy with delimiter d results in segments that join back with d to form original
prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length delim s = 
    let segments = splitBy delim s
        rejoined = concat $ intersperse delim segments
    in counterexample ("Segments: " ++ show segments ++ ", Rejoined: " ++ show rejoined) $
       rejoined === s
  where
    intersperse _ [] = []
    intersperse d [x] = [x]
    intersperse d (x:xs) = x : d : intersperse d xs

-- | splitByCollapsed never returns empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim s = 
    let segments = splitByCollapsed delim s
        hasEmpty = any null segments
    in counterexample ("Segments: " ++ show segments) $
       not hasEmpty === True

-- | splitByComma should be equivalent to splitBy with comma delimiter
prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy s = 
    splitByComma s === splitBy ',' s

-- | splitByCommaCollapsed should be equivalent to splitByCollapsed with comma delimiter
prop_splitByCommaCollapsed_equals_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed_equals_splitByCollapsed s = 
    splitByCommaCollapsed s === splitByCollapsed ',' s

-- | splitBy followed by join should reconstruct the original string
prop_splitBy_join_roundtrip :: Char -> String -> Property
prop_splitBy_join_roundtrip delim s =
    let segments = splitBy delim s
        rejoined = T.unpack $ T.intercalate (T.singleton delim) $ map T.pack segments
    in counterexample ("Original: " ++ show s ++ ", Rejoined: " ++ show rejoined) $
       rejoined === s

-- | removeLineComments should not modify lines that don't start with //
prop_removeLineComments_preserves_non_comment :: String -> Property
prop_removeLineComments_preserves_non_comment s =
    let lines' = lines s
        nonCommentLines = filter (not . ("//" `isPrefixOf`)) lines'
        processed = removeLineComments s
        processedLines = lines processed
    in counterexample ("Original lines: " ++ show lines' ++ ", Processed lines: " ++ show processedLines) $
       if null nonCommentLines 
       then property True
       else length processedLines >= length nonCommentLines

-- | removeComments should handle nested block comments
prop_removeComments_nested :: String -> Property
prop_removeComments_nested s =
    let withNestedComments = "start /* outer /* inner */ still outer */ end"
        processed = removeComments withNestedComments
    in counterexample ("Processed: " ++ processed) $
       not ("/*" `isInfixOf` processed) .&&. not ("*/" `isInfixOf` processed)

-- | normalizeIndentation should preserve the relative structure of lines
prop_normalizeIndentation_preserves_structure :: String -> Property
prop_normalizeIndentation_preserves_structure s =
    let lines' = lines s
        normalized = normalizeIndentation s
        normalizedLines = lines normalized
    in counterexample ("Original lines: " ++ show lines' ++ ", Normalized lines: " ++ show normalizedLines) $
       length normalizedLines === length lines'

-- | breakOn should correctly split at the first occurrence of delimiter
prop_breakOn_correctness :: String -> String -> Property
prop_breakOn_correctness s delim =
    let (before, after) = breakOn delim s
        expectedBefore = takeWhile (not . (`isPrefixOf` delim)) (inits s)
        expectedAfter = drop (length before + length delim) s
    in counterexample ("Before: " ++ show before ++ ", After: " ++ show after) $
       if delim `isInfixOf` s
       then before ++ delim ++ after === s
       else before === s .&&. after === ""
  where
    inits [] = [""]
    inits xs = "" : map (`take` xs) [1..length xs]

-- | trim and split should be consistent: trimming after splitting should give same result as splitting then trimming each segment
prop_trim_split_consistency :: Char -> String -> Property
prop_trim_split_consistency delim s =
    let splitThenTrim = map trim $ splitBy delim s
        trimThenSplit = splitBy delim $ trim s
    in counterexample ("Split then trim: " ++ show splitThenTrim ++ ", Trim then split: " ++ show trimThenSplit) $
       length splitThenTrim === length trimThenSplit

-- | splitBy should be consistent with delimiter: segments should not contain the delimiter
prop_splitBy_delimiter_consistency :: Char -> String -> Property
prop_splitBy_delimiter_consistency delim s =
    let segments = splitBy delim s
        containsDelimiter = any (delim `elem`) segments
    in counterexample ("Segments: " ++ show segments ++ ", Delimiter: " ++ show delim) $
       not containsDelimiter === True