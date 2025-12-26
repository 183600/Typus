{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Test.Unit.UtilsAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, choose, listOf, forAll, Property, (===), counterexample, (==>))

import qualified Data.Text as T
import Data.Char (isSpace)

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

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

-- Generate strings with potential whitespace
instance Arbitrary String where
  arbitrary = listOf $ oneof
    [ choose (' ', '~')  -- ASCII printable characters
    , elements "\n\t\r"   -- Whitespace characters
    ]

-- Generate non-empty delimiters for split tests
genDelimiter :: Gen Char
genDelimiter = oneof
  [ elements ",;|:"
  , choose ('!', '/')   -- Other ASCII punctuation
  ]

-- Generate strings that might contain comments
genCommentString :: Gen String
genCommentString = listOf $ oneof
  [ choose (' ', '~')           -- Regular characters
  , elements "\n\t\r"           -- Whitespace
  , return '/'                  -- Potential comment start
  , return '*'                  -- Potential block comment
  ]

-- Generate strings with various indentation patterns
genIndentedString :: Gen String
genIndentedString = do
  lines <- listOf $ do
    indent <- choose (0, 8)
    content <- listOf $ choose (' ', '~')
    return $ replicate indent ' ' ++ content
  return $ unlines lines

-- ============================================================================
-- Property Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Utils Advanced QuickCheck Tests"
    [ testProperty "trim removes leading and trailing whitespace" $
        \str ->
          let trimmed = trim str
              hasLeadingSpace = not (null str) && isSpace (head str)
              hasTrailingSpace = not (null str) && isSpace (last str)
          in counterexample ("Original: " ++ show str ++ ", Trimmed: " ++ show trimmed) $
             if hasLeadingSpace || hasTrailingSpace
             then not (null trimmed) ==> not (isSpace (head trimmed)) && not (isSpace (last trimmed))
             else trimmed === str

    , testProperty "trim is idempotent" $
        \str -> trim (trim str) === trim str

    , testProperty "trim never adds characters" $
        \str -> length (trim str) <= length str

    , testProperty "splitBy preserves total length (including delimiters)" $
        \delim str ->
          delim /= '\0' ==>
          let parts = splitBy delim str
              totalLength = sum (map length parts) + length (filter (== delim) str) - length parts
          in totalLength === length str

    , testProperty "splitBy empty string returns single empty part" $
        \delim ->
          delim /= '\0' ==>
          splitBy delim "" === [""]

    , testProperty "splitBy on string with only delimiters returns correct number of empty parts" $
        \delim n ->
          delim /= '\0' && n >= 0 && n <= 20 ==>
          let str = replicate n delim
              parts = splitBy delim str
          in length parts === n + 1 .&&. all null parts

    , testProperty "splitByCollapsed removes empty parts" $
        \delim str ->
          delim /= '\0' ==>
          let parts = splitBy delim str
              collapsed = splitByCollapsed delim str
          in all (not . null) collapsed

    , testProperty "splitByCollapsed length is less than or equal to splitBy" $
        \delim str ->
          delim /= '\0' ==>
          length (splitByCollapsed delim str) <= length (splitBy delim str)

    , testProperty "splitByComma equals splitBy with comma delimiter" $
        \str -> splitByComma str === splitBy ',' str

    , testProperty "splitByCommaCollapsed equals splitByCollapsed with comma delimiter" $
        \str -> splitByCommaCollapsed str === splitByCollapsed ',' str

    , testProperty "removeLineComments removes // comments" $
        \str ->
          let withoutComments = removeLineComments str
              linesWithComments = lines str
              processedLines = lines withoutComments
          in length processedLines <= length linesWithComments

    , testProperty "removeLineComments preserves lines without // comments" $
        \str ->
          not ('/' `elem` str) ==>
          removeLineComments str === str

    , testProperty "removeComments removes both line and block comments" $
        \str ->
          let withoutComments = removeComments str
          in length withoutComments <= length str

    , testProperty "removeComments preserves strings without comment markers" $
        \str ->
          not ('/' `elem` str) ==>
          removeComments str === str

    , testProperty "normalizeIndentation preserves relative indentation" $
        \str ->
          let normalized = normalizeIndentation str
              originalLines = lines str
              normalizedLines = lines normalized
          in length normalizedLines === length originalLines

    , testProperty "normalizeIndentation removes leading spaces from all lines" $
        \str ->
          let normalized = normalizeIndentation str
              normalizedLines = lines normalized
          in all (not . null) normalizedLines ==> 
             all (\line -> null line || not (isSpace (head line))) normalizedLines

    , testProperty "breakOn finds first occurrence or returns original string" $
        \delim str ->
          not (null delim) ==>
          let (before, after) = breakOn delim str
              combined = before ++ delim ++ after
          in if delim `isInfixOf` str
             then combined === str
             else before === str .&&. after === ""

    , testProperty "breakOn empty delimiter returns (\"\", str)" $
        \str -> breakOn "" str === ("", str)

    , testProperty "splitBy and breakOn are consistent for single character delimiters" $
        \delim str ->
          delim /= '\0' && not (null str) ==>
          let parts = splitBy delim str
              (before, after) = breakOn [delim] str
          in if null after
             then parts === [str]
             else parts === before : splitBy delim (tail after)

    , testProperty "trim . splitBy . join preserves non-whitespace content" $
        \delim str ->
          delim /= '\0' && delim `notElem` " \t\n\r" ==>
          let parts = splitBy delim str
              trimmedParts = map trim parts
              rejoined = intercalate [delim] trimmedParts
          in all (not . null) trimmedParts ==> 
             all (not . isSpace . head) trimmedParts

    , testProperty "removeComments preserves line count for block comments" $
        \str ->
          let withoutComments = removeComments str
              originalLines = lines str
              processedLines = lines withoutComments
              hasBlockComment = "/*" `isInfixOf` str && "*/" `isInfixOf` str
          in hasBlockComment ==> length processedLines <= length originalLines
    ]

-- Helper function for intercalate
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Helper function for infix check
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    isPrefixOf _ _ = False

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(x:xs') = xs : tails xs'
