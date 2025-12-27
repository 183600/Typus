{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.UtilsStringProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll)
import Test.Tasty.HUnit (testCase, assertBool)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeComments, normalizeIndentation)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = do
  base <- arbitrary
  ws <- elements ["", " ", "  ", "\t", "\n", " \t\n ", "   \n\t  "]
  return $ ws ++ base ++ ws

-- Generate strings with commas
genCommaString :: Gen String
genCommaString = do
  parts <- listOf1 $ elements ["a", "b", "", "c", "", "d"]
  return $ concat $ zipWith (\p i -> if i > 0 then "," ++ p else p) parts [0..]

-- Generate strings with comments
genCommentString :: Gen String
genCommentString = do
  base <- arbitrary
  comment <- elements ["// line comment", "/* block comment */", "/* nested /* comment */ */"]
  pos <- elements [0, 1, 2]
  let parts = [base, comment]
      result = if pos == 0 then comment ++ base
               else if pos == 1 then base ++ comment
               else base ++ comment ++ base
  return result

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Trim is idempotent
prop_trimIdempotent :: String -> Property
prop_trimIdempotent s = trim (trim s) === trim s

-- Property: Trim removes leading and trailing whitespace
prop_trimRemovesWhitespace :: String -> Property
prop_trimRemovesWhitespace s = 
  let trimmed = trim s
      hasLeadingWS = not (null trimmed) && isSpace (head trimmed)
      hasTrailingWS = not (null trimmed) && isSpace (last trimmed)
  in property (not hasLeadingWS && not hasTrailingWS)
  where
    isSpace c = c `elem` " \t\n\r"

-- Property: splitBy delim . intercalate delim == original
prop_splitByRoundtrip :: String -> Property
prop_splitByRoundtrip s = 
  let delim = ','
      parts = splitBy delim s
      reconstructed = concat $ zipWith (\p i -> if i > 0 then [delim] ++ p else p) parts [0..]
  in reconstructed === s

-- Property: splitByCollapsed removes empty parts
prop_splitByCollapsedRemovesEmpty :: String -> Property
prop_splitByCollapsedRemovesEmpty s = 
  let parts = splitByCollapsed ',' s
      hasEmpty = any null parts
  in property (not hasEmpty)

-- Property: splitByComma == splitBy ','
prop_splitByCommaEqSplitBy :: String -> Property
prop_splitByCommaEqSplitBy s = splitByComma s === splitBy ',' s

-- Property: removeComments removes // comments
prop_removeLineComments :: Property
prop_removeLineComments = 
  let input = "code // comment\nmore code"
      result = removeComments input
      hasComment = "// comment" `isInfixOf` result
  in property (not hasComment)

-- Property: removeComments removes /* */ comments
prop_removeBlockComments :: Property
prop_removeBlockComments = 
  let input = "code /* comment */ more code"
      result = removeComments input
      hasComment = "/* comment */" `isInfixOf` result
  in property (not hasComment)

-- Property: normalizeIndentation preserves relative indentation
prop_normalizePreservesRelative :: String -> Property
prop_normalizePreservesRelative s = 
  let lines' = lines s
      normalized = normalizeIndentation s
      normalizedLines = lines normalized
      -- Check that the relative indentation between consecutive lines is preserved
      relativeIndentation preserved = 
        if length lines' < 2 then True
        else 
          let originalDiffs = zipWith (\l1 l2 -> 
                let indent1 = length $ takeWhile isSpace l1
                    indent2 = length $ takeWhile isSpace l2
                in indent2 - indent1) lines' (tail lines')
              normalizedDiffs = zipWith (\l1 l2 -> 
                let indent1 = length $ takeWhile isSpace l1
                    indent2 = length $ takeWhile isSpace l2
                in indent2 - indent1) normalizedLines (tail normalizedLines)
          in originalDiffs == normalizedDiffs
  in property preserved
  where
    isSpace c = c `elem` " \t"

-- ============================================================================
-- Helper Functions
-- ============================================================================

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils String Processing QuickCheck Tests"
  [ testProperty "Trim is idempotent" prop_trimIdempotent
  , testProperty "Trim removes leading and trailing whitespace" prop_trimRemovesWhitespace
  , testProperty "splitBy roundtrip property" prop_splitByRoundtrip
  , testProperty "splitByCollapsed removes empty parts" prop_splitByCollapsedRemovesEmpty
  , testProperty "splitByComma equals splitBy ','" prop_splitByCommaEqSplitBy
  , testProperty "removeComments removes line comments" prop_removeLineComments
  , testProperty "removeComments removes block comments" prop_removeBlockComments
  , testProperty "normalizeIndentation preserves relative indentation" prop_normalizePreservesRelative
  , testCase "String processing edge cases" $ do
      assertBool "Trim handles empty string" $ trim "" == ""
      assertBool "Trim handles only whitespace" $ trim "   \t\n  " == ""
      assertBool "SplitBy handles empty string" $ splitBy ',' "" == [""]
      assertBool "SplitByCollapsed handles empty string" $ splitByCollapsed ',' "" == []
      assertBool "removeComments handles empty string" $ removeComments "" == ""
  ]