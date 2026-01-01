{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalUtilsQuickCheckTestsSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, counterexample)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = do
  before <- listOf $ elements " \t\n\r"
  middle <- listOf $ arbitrary `suchThat` (/= '\n')
  after <- listOf $ elements " \t\n\r"
  return $ before ++ middle ++ after

-- Generate strings with commas
genCommaString :: Gen String
genCommaString = do
  parts <- listOf1 $ listOf $ arbitrary `suchThat` (/= ',')
  let insertCommas [] = []
      insertCommas (x:xs) = x : concatMap (\p -> "," : p) xs
  return $ insertCommas parts

-- Generate strings with line comments
genLineCommentString :: Gen String
genLineCommentString = do
  before <- listOf $ arbitrary `suchThat` (/= '/')
  comment <- listOf $ arbitrary `suchThat` (/= '\n')
  after <- listOf $ arbitrary
  return $ before ++ "//" ++ comment ++ "\n" ++ after

-- Generate strings with block comments
genBlockCommentString :: Gen String
genBlockCommentString = do
  before <- listOf $ arbitrary `suchThat` (/= '/')
  comment <- listOf $ arbitrary `suchThat` (/= '*')
  after <- listOf $ arbitrary
  return $ before ++ "/*" ++ comment ++ "*/" ++ after

-- Generate multi-line strings with indentation
genIndentedString :: Gen String
genIndentedString = do
  numLines <- choose (1, 5)
  baseIndent <- choose (0, 4)
  lines <- sequence $ replicate numLines $ do
    indent <- choose (baseIndent, baseIndent + 3)
    content <- listOf $ arbitrary `suchThat` (/= '\n')
    return $ replicate indent ' ' ++ content
  return $ unlines lines

-- Generate string L.and pattern for breakOn
genBreakOnInput :: Gen (String, String)
genBreakOnInput = do
  pattern <- listOf1 arbitrary
  before <- listOf $ arbitrary `suchThat` (\c -> c /= '\0' && not (pattern `L.isPrefixOf` [c]))
  after <- listOf arbitrary
  return (before ++ pattern ++ after, pattern)

-- ============================================================================
-- Properties for trim function
-- ============================================================================

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  trim (trim s) === trim s

prop_trim_no_leading_trailing_whitespace :: String -> Property
prop_trim_no_leading_trailing_whitespace s =
  let trimmed = trim s
  in counterexample ("trimmed: " ++ show trimmed) $
     case trimmed of
       [] -> property True
       (c:cs) -> not (isSpace c) && not (isSpace (last cs))

prop_trim_whitespace_only :: String -> Property
prop_trim_whitespace_only s =
  let allWhitespace = L.all isSpace s
  in if allWhitespace
     then trim s === ""
     else property True

-- ============================================================================
-- Properties for splitBy function
-- ============================================================================

prop_splitBy_empty_delimiter :: String -> Property
prop_splitBy_empty_delimiter s =
  splitBy '\0' s === [s]

prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim s =
  L.concat (splitBy delim s) === s

prop_splitBy_comma_consistency :: String -> Property
prop_splitBy_comma_consistency s =
  splitBy ',' s === splitByComma s

-- ============================================================================
-- Properties for splitByCollapsed function
-- ============================================================================

prop_splitByCollapsed_no_empty_segments :: Char -> String -> Property
prop_splitByCollapsed_no_empty_segments delim s =
  L.all (not . null) (splitByCollapsed delim s)

prop_splitByCollapsed_comma_consistency :: String -> Property
prop_splitByCollapsed_comma_consistency s =
  splitByCollapsed ',' s === splitByCommaCollapsed s

prop_splitByCollapsed_subset_of_splitBy :: Char -> String -> Property
prop_splitByCollapsed_subset_of_splitBy delim s =
  let collapsed = splitByCollapsed delim s
      normal = splitBy delim s
  in L.all (`elem` normal) collapsed

-- ============================================================================
-- Properties for removeLineComments function
-- ============================================================================

prop_removeLineComments_removes_slash_slash :: String -> Property
prop_removeLineComments_removes_slash_slash s =
  let withComment = s ++ "//comment\nmore"
      result = removeLineComments withComment
  in "//comment" `notElem` lines result

prop_removeLineComments_preserves_other_content :: String -> Property
prop_removeLineComments_preserves_other_content s =
  let withoutComments = L.filter (not . ("//" `L.isPrefixOf`)) (lines s)
      result = removeLineComments s
      resultLines = L.filter (not . null) $ lines result
  in L.length resultLines >= L.length withoutComments

-- ============================================================================
-- Properties for removeComments function
-- ============================================================================

prop_removeComments_removes_both_types :: String -> Property
prop_removeComments_removes_both_types s =
  let withLineComments = s ++ "//line comment\n"
      withBlockComments = withLineComments ++ "/*block comment*/" ++ s
      result = removeComments withBlockComments
  in "//line comment" `notElem` lines result &&
     "/*block comment*/" `notElem` result

prop_removeComments_preserves_strings :: String -> Property
prop_removeComments_preserves_strings s =
  let stringWithCommentAndString = s ++ "code // comment\n\"string // not comment\" more code"
      result = removeComments stringWithCommentAndString
  in "// comment" `notElem` lines result &&
     "string // not comment" `elem` result

-- ============================================================================
-- Properties for normalizeIndentation function
-- ============================================================================

prop_normalizeIndentation_preserves_relative_indentation :: Property
prop_normalizeIndentation_preserves_relative_indentation =
  forAll genIndentedString $ \s ->
    let normalized = normalizeIndentation s
        originalLines = lines s
        normalizedLines = lines normalized
        getIndent l = L.length $ takeWhile isSpace l
    in if L.length originalLines > 1 && L.any (not . null) originalLines
       then let originalIndents = map getIndent $ L.filter (not . null) originalLines
                normalizedIndents = map getIndent $ L.filter (not . null) normalizedLines
                originalDiffs = zipWith (-) (L.tail originalIndents) (init originalIndents)
                normalizedDiffs = zipWith (-) (L.tail normalizedIndents) (init normalizedIndents)
            in originalDiffs === normalizedDiffs
       else property True

prop_normalizeIndentation_no_leading_whitespace :: Property
prop_normalizeIndentation_no_leading_whitespace =
  forAll genIndentedString $ \s ->
    let normalized = normalizeIndentation s
        lines' = lines normalized
        nonEmptyLines = L.filter (not . null) lines'
    in if null nonEmptyLines
       then property True
       else L.all (not . isSpace . L.head) nonEmptyLines

-- ============================================================================
-- Properties for breakOn function
-- ============================================================================

prop_breakOn_finds_pattern :: Property
prop_breakOn_finds_pattern =
  forAll genBreakOnInput $ \(s, pat) ->
    let (before, after) = breakOn pat s
    in before ++ pat ++ after === s

prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern s =
  breakOn "" s === ("", s)

prop_breakOn_not_found :: String -> String -> Property
prop_breakOn_not_found s pat =
  let pat' = pat ++ "\0"  -- Ensure pattern not in string
      (before, after) = breakOn pat' s
  in if pat' `L.isInfixOf` s
     then property True
     else before === s && after === ""

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils QuickCheck Tests"
  [ testGroup "trim"
    [ testProperty "trim is idempotent" prop_trim_idempotent
    , testProperty "trim removes leading/trailing whitespace" prop_trim_no_leading_trailing_whitespace
    , testProperty "trim of whitespace-only string is empty" prop_trim_whitespace_only
    ]
  , testGroup "splitBy"
    [ testProperty "splitBy with empty delimiter returns original" prop_splitBy_empty_delimiter
    , testProperty "splitBy preserves content when concatenated" prop_splitBy_preserves_content
    , testProperty "splitBy ',' equals splitByComma" prop_splitBy_comma_consistency
    ]
  , testGroup "splitByCollapsed"
    [ testProperty "splitByCollapsed never returns empty segments" prop_splitByCollapsed_no_empty_segments
    , testProperty "splitByCollapsed ',' equals splitByCommaCollapsed" prop_splitByCollapsed_comma_consistency
    , testProperty "splitByCollapsed is subset of splitBy" prop_splitByCollapsed_subset_of_splitBy
    ]
  , testGroup "removeLineComments"
    [ testProperty "removeLineComments removes // comments" prop_removeLineComments_removes_slash_slash
    , testProperty "removeLineComments preserves other content" prop_removeLineComments_preserves_other_content
    ]
  , testGroup "removeComments"
    [ testProperty "removeComments removes both // L.and /* */ comments" prop_removeComments_removes_both_types
    , testProperty "removeComments preserves // inside strings" prop_removeComments_preserves_strings
    ]
  , testGroup "normalizeIndentation"
    [ testProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative_indentation
    , testProperty "normalizeIndentation removes common leading whitespace" prop_normalizeIndentation_no_leading_whitespace
    ]
  , testGroup "breakOn"
    [ testProperty "breakOn finds pattern L.and splits correctly" prop_breakOn_finds_pattern
    , testProperty "breakOn with empty pattern returns empty before" prop_breakOn_empty_pattern
    , testProperty "breakOn when pattern not found returns original" prop_breakOn_not_found
    ]
  ]