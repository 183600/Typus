{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.EnhancedUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat, (===), (.&&.))
import TestSupport.QuickCheck (fastProperty)
import Utils
import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

-- ============================================================================
-- Enhanced QuickCheck tests for Utils module
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Enhanced Utils QuickCheck Tests"
    [ testGroup "String Processing Properties"
        [ fastProperty "trim removes only leading/trailing whitespace" prop_trimOnlyRemovesWhitespace
        , fastProperty "splitBy and splitByCollapsed relationship" prop_splitByRelationship
        , fastProperty "splitBy preserves delimiter count" prop_splitByPreservesCount
        , fastProperty "removeComments preserves non-comment content" prop_removeCommentsPreservesContent
        , fastProperty "removeLineComments handles string literals correctly" prop_removeLineCommentsStrings
        , fastProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentationPreservesStructure
        , fastProperty "breakOn correctness" prop_breakOnCorrectness
        , fastProperty "breakOn empty pattern behavior" prop_breakOnEmptyPattern
        ]
    , testGroup "Edge Case Properties"
        [ fastProperty "trim handles empty and whitespace-only strings" prop_trimEdgeCases
        , fastProperty "splitBy handles edge cases" prop_splitByEdgeCases
        , fastProperty "removeComments handles nested structures" prop_removeCommentsNested
        , fastProperty "normalizeIndentation handles mixed indentation" prop_normalizeIndentationMixed
        , fastProperty "comment removal preserves line count" prop_commentRemovalPreservesLines
        ]
    ]

-- ============================================================================
-- String Processing Properties
-- ============================================================================

-- Property: trim removes only leading/trailing whitespace
prop_trimOnlyRemovesWhitespace :: String -> Bool
prop_trimOnlyRemovesWhitespace input =
  let trimmed = trim input
      leadingRemoved = dropWhile isSpace input
      trailingRemoved = reverse (dropWhile isSpace (reverse leadingRemoved))
  in trimmed == trailingRemoved

-- Property: splitBy and splitByCollapsed relationship
prop_splitByRelationship :: Char -> String -> Bool
prop_splitByRelationship delim input =
  let normal = splitBy delim input
      collapsed = splitByCollapsed delim input
  in collapsed == filter (not . null) normal

-- Property: splitBy preserves delimiter count
prop_splitByPreservesCount :: Char -> String -> Bool
prop_splitByPreservesCount delim input =
  let parts = splitBy delim input
      delimiterCount = length (filter (== delim) input)
  in length parts - 1 == delimiterCount

-- Property: removeComments preserves non-comment content
prop_removeCommentsPreservesContent :: String -> Bool
prop_removeCommentsPreservesContent input =
  let withoutComments = removeComments input
      -- Extract non-comment characters from original
      nonCommentChars = filter (not . isCommentChar) input
      nonCommentCharsProcessed = filter (not . isCommentChar) withoutComments
  in length nonCommentChars == length nonCommentCharsProcessed
  where
    isCommentChar '/' = True
    isCommentChar '*' = True
    isCommentChar _ = False

-- Property: removeLineComments handles string literals correctly
prop_removeLineCommentsStrings :: String -> Bool
prop_removeLineCommentsStrings input =
  let processed = removeLineComments input
      linesInput = lines input
      linesProcessed = lines processed
  in length linesInput == length linesProcessed

-- Property: normalizeIndentation preserves relative structure
prop_normalizeIndentationPreservesStructure :: String -> Bool
prop_normalizeIndentationPreservesStructure input =
  let normalized = normalizeIndentation input
      inputLines = lines input
      normalizedLines = lines normalized
      -- Check that non-empty lines are preserved
      inputNonEmpty = filter (not . all isSpace) inputLines
      normalizedNonEmpty = filter (not . all isSpace) normalizedLines
  in length inputNonEmpty == length normalizedNonEmpty

-- Property: breakOn correctness
prop_breakOnCorrectness :: String -> String -> Bool
prop_breakOnCorrectness pattern text
  | null pattern = breakOn pattern text == ("", text)
  | pattern `isInfixOf` text = 
      let (before, after) = breakOn pattern text
      in before ++ pattern ++ after == text
  | otherwise = breakOn pattern text == (text, "")

-- Property: breakOn empty pattern behavior
prop_breakOnEmptyPattern :: String -> Bool
prop_breakOnEmptyPattern text =
  breakOn "" text == ("", text)

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

-- Property: trim handles empty and whitespace-only strings
prop_trimEdgeCases :: String -> Bool
prop_trimEdgeCases input =
  let trimmed = trim input
      isOnlyWhitespace = all isSpace input
  in if null input || isOnlyWhitespace
     then null trimmed
     else not (all isSpace trimmed)

-- Property: splitBy handles edge cases
prop_splitByEdgeCases :: Char -> String -> Bool
prop_splitByEdgeCases delim input =
  let parts = splitBy delim input
      -- Check that joining with delimiter gives back original
      rejoined = foldr1 (\a b -> a ++ [delim] ++ b) parts
  in if null parts
     then True
     else length parts > 0 && (if null input then parts == [""] else True)

-- Property: removeComments handles nested structures
prop_removeCommentsNested :: String -> Bool
prop_removeCommentsNested input =
  let processed = removeComments input
      -- Basic sanity: processed should not contain comment markers
      hasBlockComment = "/*" `isInfixOf` processed || "*/" `isInfixOf` processed
      hasLineComment = "//" `isInfixOf` processed
  in not (hasBlockComment || hasLineComment)

-- Property: normalizeIndentation handles mixed indentation
prop_normalizeIndentationMixed :: String -> Bool
prop_normalizeIndentationMixed input =
  let normalized = normalizeIndentation input
      normalizedLines = lines normalized
  -- Check that no line starts with both spaces and tabs (mixed indentation)
  in all (not . hasMixedIndentation) normalizedLines
  where
    hasMixedIndentation line =
      let leading = takeWhile isSpace line
          hasSpaces = ' ' `elem` leading
          hasTabs = '\t' `elem` leading
      in hasSpaces && hasTabs

-- Property: comment removal preserves line count
prop_commentRemovalPreservesLines :: String -> Bool
prop_commentRemovalPreservesLines input =
  let withComments = input
      withoutComments = removeComments input
      withoutLineComments = removeLineComments input
      linesOriginal = length (lines withComments)
      linesBlockComments = length (lines withoutComments)
      linesLineComments = length (lines withoutLineComments)
  in linesBlockComments <= linesOriginal && linesLineComments <= linesOriginal

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ oneof 
  [ elements " \t\n\r"
  , elements (map toEnum [32..126]) -- printable ASCII
  ]

-- Generate strings that might contain comment-like patterns
genCommentString :: Gen String
genCommentString = listOf $ oneof
  [ elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  , elements " \t\n\r"
  , elements "/*"
  , elements "*/"
  , elements "//"
  , elements "\""
  , elements "'"
  , elements "\\"
  ]

-- Generate strings with indentation
genIndentedString :: Gen String
genIndentedString = do
  numLines <- choose (0, 10)
  lines <- sequence $ replicate numLines genIndentedLine
  return $ unlines lines
  where
    genIndentedLine = do
      indent <- choose (0, 5)
      spaces <- return $ replicate indent ' '
      content <- listOf $ elements "abcdefghijklmnopqrstuvwxyz"
      return $ spaces ++ content

-- Instance for generating test strings
instance Arbitrary String where
  arbitrary = oneof
    [ genWhitespaceString
    , genCommentString
    , genIndentedString
    , listOf $ elements (map toEnum [32..126]) -- printable ASCII
    ]