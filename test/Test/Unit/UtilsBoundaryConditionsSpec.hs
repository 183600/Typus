{-# LANGUAGE CPP #-}

module Test.Unit.UtilsBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

tests :: TestTree
tests = testGroup "Utils Boundary Conditions"
  [ trimBoundaryTests
  , splitBoundaryTests
  , commentBoundaryTests
  , indentationBoundaryTests
  , searchBoundaryTests
  , quickCheckProperties
  ]

trimBoundaryTests :: TestTree
trimBoundaryTests = testGroup "Trim Boundary Tests"
  [ testCase "trim handles empty string" $ do
      trim "" @?= ""
      
  , testCase "trim handles only whitespace" $ do
      trim "   \t\n\r  " @?= ""
      
  , testCase "trim handles unicode whitespace" $ do
      trim "\x2000\x2001\x2002hello\x2003\x2004" @?= "hello"
      
  , testCase "trim preserves internal whitespace" $ do
      trim "  hello   world  " @?= "hello   world"
      
  , testCase "trim handles single character" $ do
      trim "a" @?= "a"
      trim " a" @?= "a"
      trim "a " @?= "a"
  ]

splitBoundaryTests :: TestTree
splitBoundaryTests = testGroup "Split Boundary Tests"
  [ testCase "splitBy on empty string" $ do
      splitBy ',' "" @?= [""]
      
  , testCase "splitBy with no delimiter" $ do
      splitBy ',' "hello" @?= ["hello"]
      
  , testCase "splitBy with only delimiters" $ do
      splitBy ',' ",," @?= ["", "", ""]
      
  , testCase "splitByCollapsed on empty string" $ do
      splitByCollapsed ',' "" @?= []
      
  , testCase "splitByCollapsed with only delimiters" $ do
      splitByCollapsed ',' ",," @?= []
      
  , testCase "splitByComma on empty string" $ do
      splitByComma "" @?= [""]
      
  , testCase "splitByCommaCollapsed on empty string" $ do
      splitByCommaCollapsed "" @?= []
      
  , testCase "splitBy with unicode delimiter" $ do
      splitBy '€' "a€b€c" @?= ["a", "b", "c"]
  ]

commentBoundaryTests :: TestTree
commentBoundaryTests = testGroup "Comment Boundary Tests"
  [ testCase "removeLineComments on empty string" $ do
      removeLineComments "" @?= ""
      
  , testCase "removeLineComments with only comment" $ do
      removeLineComments "// entire line comment" @?= ""
      
  , testCase "removeLineComments with comment at start" $ do
      removeLineComments "// comment\ncode" @?= "\ncode"
      
  , testCase "removeLineComments preserves line endings" $ do
      removeLineComments "line1\n// comment\nline2" @?= "line1\n\nline2"
      
  , testCase "removeComments on empty string" $ do
      removeComments "" @?= ""
      
  , testCase "removeComments with only block comment" $ do
      removeComments "/* entire block */" @?= " "
      
  , testCase "removeComments with nested quotes in block comment" $ do
      removeComments "/* \"// not a comment */ */" @?= " "
      
  , testCase "removeComments with unterminated block comment" $ do
      removeComments "start /* unterminated" @?= "start "
      
  , testCase "removeComments handles multiple consecutive blocks" $ do
      removeComments "a/*b*/c/*d*/e" @?= "a c e"
  ]

indentationBoundaryTests :: TestTree
indentationBoundaryTests = testGroup "Indentation Boundary Tests"
  [ testCase "normalizeIndentation on empty string" $ do
      normalizeIndentation "" @?= ""
      
  , testCase "normalizeIndentation on only whitespace" $ do
      normalizeIndentation "   \n\t\n  " @?= "\n\n"
      
  , testCase "normalizeIndentation preserves blank lines" $ do
      normalizeIndentation "\n\nline\n\n" @?= "\n\nline\n\n"
      
  , testCase "normalizeIndentation with mixed tabs L.and spaces" $ do
      normalizeIndentation "\t  line\n    \t  next" @?= "line\n  next"
      
  , testCase "forceSingleTabIndentation on empty string" $ do
      forceSingleTabIndentation "" @?= ""
      
  , testCase "forceSingleTabIndentation on only whitespace" $ do
      forceSingleTabIndentation "   \n\t\n  " @?= "\n\n"
      
  , testCase "fixIndentation is alias for normalizeIndentation" $ do
      let input = "  line\n    nested"
      fixIndentation input @?= normalizeIndentation input
  ]

searchBoundaryTests :: TestTree
searchBoundaryTests = testGroup "Search Boundary Tests"
  [ testCase "breakOn with empty pattern" $ do
      breakOn "" "hello" @?= ("", "hello")
      
  , testCase "breakOn with empty string" $ do
      breakOn "pattern" "" @?= ("", "")
      
  , testCase "breakOn with both empty" $ do
      breakOn "" "" @?= ("", "")
      
  , testCase "breakOn with pattern not found" $ do
      breakOn "xyz" "hello" @?= ("hello", "")
      
  , testCase "breakOn with pattern at start" $ do
      breakOn "hello" "hello world" @?= ("", " world")
      
  , testCase "breakOn with pattern at end" $ do
      breakOn "world" "hello world" @?= ("hello ", "")
      
  , testCase "breakOn with multiple occurrences" $ do
      breakOn "b" "abcde" @?= ("a", "cde")
  ]

quickCheckProperties :: TestTree
quickCheckProperties = testGroup "QuickCheck Boundary Properties"
  [ fastProperty "trim removes L.all leading/trailing whitespace" prop_trim_boundary
  , fastProperty "splitBy L.length relationship" prop_splitBy_length
  , fastProperty "splitByCollapsed never returns empty strings" prop_splitByCollapsed_no_empty
  , fastProperty "removeLineComments preserves non-comment lines" prop_removeLineComments_preserve
  , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_line_count
  , fastProperty "breakOn concatenation property" prop_breakOn_concat
  ]

-- QuickCheck property implementations
prop_trim_boundary :: String -> Property
prop_trim_boundary s =
  let trimmed = trim s
  in conjoin
    [ not (null trimmed) ==> property (not (isSpace (L.head trimmed)))
    , not (null trimmed) ==> property (not (isSpace (last trimmed)))
    , null trimmed ==> property True
    ]

prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length delim s =
  let parts = splitBy delim s
      delimiterCount = L.length (L.filter (== delim) s)
  in L.length parts === delimiterCount + 1

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ L.all (not . null) parts

prop_removeLineComments_preserve :: String -> Property
prop_removeLineComments_preserve s =
  let hasNoComment = not ("//" `L.isPrefixOf` s)
      result = removeLineComments s
  in hasNoComment ==> property (s == result)

prop_normalizeIndentation_line_count :: String -> Property
prop_normalizeIndentation_line_count s =
  let originalLines = lines s
      normalizedLines = lines (normalizeIndentation s)
  in L.length originalLines === L.length normalizedLines

prop_breakOn_concat :: String -> String -> Property
prop_breakOn_concat pat s =
  not (null pat) ==>
  let (before, after) = breakOn pat s
  in if pat `L.isInfixOf` s
     then before ++ pat ++ after === s
     else before === s && after === ""
  where
    isInfixOf needle haystack = needle `L.isPrefixOf` dropWhile (not . (L.head needle ==)) haystack