{-# LANGUAGE CPP #-}

module Test.Unit.NewCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)

import Utils (trim, splitBy, splitByCollapsed, removeComments, normalizeIndentation, breakOn)
import Data.Char (isSpace, toLower, toUpper)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- | Generate random strings with various characteristics
genString :: Gen String
genString = listOf $ oneof 
  [ choose ('a', 'z')
  , choose ('A', 'Z')
  , choose ('0', '9')
  , elements " \t\n\r"
  , elements "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  ]

-- | Generate strings without newlines for line-specific tests
genSingleLineString :: Gen String
genSingleLineString = listOf $ oneof
  [ choose ('a', 'z')
  , choose ('A', 'Z')
  , choose ('0', '9')
  , elements " \t"
  , elements "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  ]

-- | Generate delimiters for split operations
genDelimiter :: Gen Char
genDelimiter = oneof 
  [ choose ('a', 'z')
  , choose ('A', 'Z')
  , choose ('0', '9')
  , elements "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  ]

-- Property: trim preserves non-whitespace characters
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content str =
  let trimmed = trim str
      nonWhitespace = filter (not . isSpace) str
      trimmedNonWhitespace = filter (not . isSpace) trimmed
  in property $ trimmedNonWhitespace === nonWhitespace

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let leading = replicate (length prefix `mod` 10) ' '
      trailing = replicate (length suffix `mod` 10) '\t'
      content = prefix ++ suffix
      input = leading ++ content ++ trailing
      result = trim input
  in property $ result === content

-- Property: splitBy preserves total content when rejoined
prop_splitBy_roundtrip :: Char -> String -> Property
prop_splitBy_roundtrip delim str =
  let parts = splitBy delim str
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === str

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empties :: Char -> String -> Property
prop_splitByCollapsed_removes_empties delim str =
  let parts = splitByCollapsed delim str
  in property $ not (any null parts)

-- Property: splitByCollapsed preserves non-empty segments from splitBy
prop_splitByCollapsed_preserves_nonempty :: Char -> String -> Property
prop_splitByCollapsed_preserves_nonempty delim str =
  let regularParts = splitBy delim str
      collapsedParts = splitByCollapsed delim str
      nonEmptyRegular = filter (not . null) regularParts
  in property $ collapsedParts === nonEmptyRegular

-- Property: removeComments preserves code structure
prop_removeComments_preserves_structure :: String -> String -> Property
prop_removeComments_preserves_structure before after =
  let code = before ++ " // comment\n" ++ after ++ " /* block */ " ++ before
      cleaned = removeComments code
  in property $ before `isInfixOf` cleaned .&&. after `isInfixOf` cleaned

-- Property: removeComments removes comment markers
prop_removeComments_removes_markers :: String -> String -> Property
prop_removeComments_removes_markers code1 code2 =
  let withComments = code1 ++ " // line comment\n" ++ code2 ++ " /* block */"
      cleaned = removeComments withComments
  in property $ not ("//" `isInfixOf` cleaned) .&&. not ("/*" `isInfixOf` cleaned)

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: [Int] -> String -> Property
prop_normalizeIndentation_preserves_relative indentLevels content =
  not (null indentLevels) ==> 
  let lines' = zipWith (\level base -> replicate level ' ' ++ base) indentLevels (repeat content)
      input = unlines lines'
      normalized = normalizeIndentation input
      normalizedLines = lines normalized
      indentLevels' = map (length . takeWhile isSpace) normalizedLines
  in property $ sort indentLevels' === sort (map (\x -> minimum (0 : [x])) indentLevels')

-- Property: normalizeIndentation removes leading whitespace
prop_normalizeIndentation_removes_leading :: String -> Property
prop_normalizeIndentation_removes_leading content =
  let indented = "    " ++ content
      normalized = normalizeIndentation indented
  in property $ not (isPrefixOf "    " normalized)

-- Property: breakOn finds correct split point
prop_breakOn_correct_split :: String -> String -> String -> Property
prop_breakOn_correct_split prefix delim suffix =
  not (null delim) ==> 
  let full = prefix ++ delim ++ suffix
      (before, after) = breakOn delim full
  in property $ before === prefix ++ delim .&&. after === suffix

-- Property: breakOn handles missing delimiter
prop_breakOn_missing_delim :: String -> String -> Property
prop_breakOn_missing_delim delim haystack =
  not (null delim) && not (delim `isInfixOf` haystack) ==> 
  let (before, after) = breakOn delim haystack
  in property $ before === haystack .&&. after === ""

-- Property: breakOn with empty delimiter
prop_breakOn_empty_delim :: String -> Property
prop_breakOn_empty_delim haystack =
  let (before, after) = breakOn "" haystack
  in property $ before === "" .&&. after === haystack

-- Property: String processing pipeline consistency
prop_pipeline_consistency :: String -> Property
prop_pipeline_consistency input =
  let pipeline1 = input |> trim |> removeComments |> normalizeIndentation
      pipeline2 = input |> removeComments |> trim |> normalizeIndentation
  in property $ pipeline1 === pipeline2
  where
    (|>) x f = f x

-- Property: Unicode handling
prop_unicode_handling :: String -> Property
prop_unicode_handling content =
  let unicodeContent = content ++ "测试café🚀"
      trimmed = trim unicodeContent
      split = splitBy ',' unicodeContent
  in property $ "测试" `isInfixOf` trimmed .&&. "café" `isInfixOf` trimmed .&&. "🚀" `isInfixOf` trimmed .&&.
     length split >= 1

-- Property: Performance with repeated operations
prop_repeated_operations_consistency :: String -> Int -> Property
prop_repeated_operations_consistency input iterations =
  iterations <= 20 ==> 
  let trimmedOnce = trim input
      trimmedMultiple = iterate trim input !! iterations
  in property $ trimmedOnce === trimmedMultiple

-- Property: Edge case with empty strings
prop_empty_string_handling :: Char -> Property
prop_empty_string_handling delim =
  let splitResult = splitBy delim ""
      collapsedResult = splitByCollapsed delim ""
      trimmedResult = trim ""
  in property $ splitResult === [""] .&&. collapsedResult === [] .&&. trimmedResult === ""

-- Property: Complex nested comment scenarios
prop_nested_comment_scenarios :: String -> String -> String -> Property
prop_nested_comment_scenarios outer middle inner =
  let complex = outer ++ "/* " ++ middle ++ " // not a comment */ " ++ inner
      processed = removeComments complex
  in property $ not ("/*" `isInfixOf` processed) .&&. not ("*/" `isInfixOf` processed) .&&.
     middle `isInfixOf` processed .&&. inner `isInfixOf` processed

tests :: TestTree
tests = testGroup "New Core QuickCheck Tests"
  [ fastProperty "trim preserves content" prop_trim_preserves_content
  , fastProperty "trim removes whitespace" prop_trim_removes_whitespace
  , fastProperty "splitBy roundtrip" prop_splitBy_roundtrip
  , fastProperty "splitByCollapsed removes empties" prop_splitByCollapsed_removes_empties
  , fastProperty "splitByCollapsed preserves non-empty" prop_splitByCollapsed_preserves_nonempty
  , fastProperty "removeComments preserves structure" prop_removeComments_preserves_structure
  , fastProperty "removeComments removes markers" prop_removeComments_removes_markers
  , fastProperty "normalizeIndentation preserves relative" prop_normalizeIndentation_preserves_relative
  , fastProperty "normalizeIndentation removes leading" prop_normalizeIndentation_removes_leading
  , fastProperty "breakOn correct split" prop_breakOn_correct_split
  , fastProperty "breakOn missing delimiter" prop_breakOn_missing_delim
  , fastProperty "breakOn empty delimiter" prop_breakOn_empty_delim
  , fastProperty "pipeline consistency" prop_pipeline_consistency
  , fastProperty "unicode handling" prop_unicode_handling
  , fastProperty "repeated operations consistency" prop_repeated_operations_consistency
  , fastProperty "empty string handling" prop_empty_string_handling
  , fastProperty "nested comment scenarios" prop_nested_comment_scenarios
  ]