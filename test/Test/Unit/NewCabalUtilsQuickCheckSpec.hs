module Test.Unit.NewCabalUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, listOf1, elements)
import Data.Char (isSpace, isLetter, isDigit)
import Data.List (isPrefixOf, isSuffixOf)

import TestSupport.QuickCheck (fastProperty)
import Utils

-- | QuickCheck tests for Utils module covering edge cases and properties
tests :: TestTree
tests =
  testGroup "New Cabal Utils QuickCheck Tests"
    [ testGroup "String manipulation properties"
        [ fastProperty "trim removes only leading/trailing whitespace" prop_trimOnlyWhitespace
        , fastProperty "splitBy preserves order of segments" prop_splitByPreservesOrder
        , fastProperty "splitBy length is original length minus delimiters plus one" prop_splitByLength
        , fastProperty "splitByCollapsed never contains empty strings" prop_splitByCollapsedNoEmpty
        , fastProperty "splitByComma is equivalent to splitBy ','" prop_splitByCommaEquivalence
        , fastProperty "removeLineComments preserves non-comment lines" prop_removeLineCommentsPreservesContent
        , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentationPreservesLines
        , fastProperty "breakOn returns correct prefix when pattern exists" prop_breakOnPrefix
        ]
    
    , testGroup "Edge case tests"
        [ testCase "trim handles empty string" $ 
            trim "" @?= ""
            
        , testCase "trim handles only whitespace" $
            trim "   \t\n  " @?= ""
            
        , testCase "splitBy on empty string returns singleton empty" $
            splitBy ',' "" @?= [""]
            
        , testCase "splitByCollapsed on only delimiters returns empty list" $
            splitByCollapsed ',' ",,," @?= []
            
        , testCase "removeComments handles nested block comments" $ do
            let input = "code /* outer /* inner */ still outer */ end"
                expected = "code  end"
            removeComments input @?= expected
        ]
    
    , testGroup "Regression and robustness tests"
        [ fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "normalizeIndentation is idempotent" prop_normalizeIndentationIdempotent
        , fastProperty "forceSingleTabIndentation is idempotent" prop_forceSingleTabIndentationIdempotent
        , fastProperty "breakOn with empty pattern returns empty prefix" prop_breakOnEmptyPattern
        ]
    ]

-- | Property: trim only removes whitespace characters from ends
prop_trimOnlyWhitespace :: String -> Bool
prop_trimOnlyWhitespace input =
  let trimmed = trim input
      hasLeadingNonWhitespace = not (null input) && not (isSpace (head input))
      hasTrailingNonWhitespace = not (null input) && not (isSpace (last input))
  in if hasLeadingNonWhitespace && hasTrailingNonWhitespace
     then trimmed == input
     else all (`elem` input) trimmed

-- | Property: splitBy preserves the order of segments
prop_splitByPreservesOrder :: String -> Char -> Bool
prop_splitByPreservesOrder input delim =
  let segments = splitBy delim input
      reconstructed = intercalate [delim] segments
  in reconstructed == input

-- | Property: splitBy length relationship
prop_splitByLength :: String -> Char -> Bool
prop_splitByLength input delim =
  let segments = splitBy delim input
      delimCount = length (filter (== delim) input)
  in length segments == delimCount + 1

-- | Property: splitByCollapsed never contains empty strings
prop_splitByCollapsedNoEmpty :: String -> Char -> Bool
prop_splitByCollapsedNoEmpty input delim =
  all (not . null) (splitByCollapsed delim input)

-- | Property: splitByComma is equivalent to splitBy ','
prop_splitByCommaEquivalence :: String -> Bool
prop_splitByCommaEquivalence input =
  splitByComma input == splitBy ',' input

-- | Property: removeLineComments preserves non-comment content
prop_removeLineCommentsPreservesContent :: String -> Bool
prop_removeLineCommentsPreservesContent input =
  let withoutComments = removeLineComments input
      linesWithoutComments = lines withoutComments
      originalLines = lines input
      nonCommentLines = filter (not . isPrefixOf "//") originalLines
  in length linesWithoutComments == length nonCommentLines

-- | Property: normalizeIndentation preserves line count
prop_normalizeIndentationPreservesLines :: String -> Bool
prop_normalizeIndentationPreservesLines input =
  let normalized = normalizeIndentation input
      originalLineCount = length (lines input)
      normalizedLineCount = length (lines normalized)
  in originalLineCount == normalizedLineCount

-- | Property: breakOn returns correct prefix when pattern exists
prop_breakOnPrefix :: String -> String -> Bool
prop_breakOnPrefix input pattern
  | null pattern = True
  | pattern `isInfixOf` input = 
      let (prefix, _) = breakOn pattern input
      in prefix `isPrefixOf` input
  | otherwise = True

-- | Property: trim is idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
  let once = trim input
      twice = trim once
  in once == twice

-- | Property: normalizeIndentation is idempotent
prop_normalizeIndentationIdempotent :: String -> Bool
prop_normalizeIndentationIdempotent input =
  let once = normalizeIndentation input
      twice = normalizeIndentation once
  in once == twice

-- | Property: forceSingleTabIndentation is idempotent
prop_forceSingleTabIndentationIdempotent :: String -> Bool
prop_forceSingleTabIndentationIdempotent input =
  let once = forceSingleTabIndentation input
      twice = forceSingleTabIndentation once
  in once == twice

-- | Property: breakOn with empty pattern returns empty prefix
prop_breakOnEmptyPattern :: String -> Bool
prop_breakOnEmptyPattern input =
  let (prefix, suffix) = breakOn "" input
  in null prefix && suffix == input

-- Helper function for string concatenation
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Helper function for substring check
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys