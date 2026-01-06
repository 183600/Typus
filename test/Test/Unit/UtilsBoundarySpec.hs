{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.UtilsBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn)

import Data.Char (isSpace, toLower, toUpper, isLetter, isDigit)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, nub, group)

-- | Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements " \t\n\r"

-- | Generate strings with punctuation
genPunctuationString :: Gen String  
genPunctuationString = listOf $ elements "!@#$%^&*()_+-=[]{}|;':\",./<>?"

-- | Generate strings with alphanumeric characters
genAlphaNumString :: Gen String
genAlphaNumString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- | Generate strings with mixed content
genMixedString :: Gen String
genMixedString = do
  whitespace <- genWhitespaceString
  alphanum <- genAlphaNumString
  punctuation <- genPunctuationString
  oneof [return $ whitespace ++ alphanum ++ punctuation,
         return $ alphanum ++ whitespace ++ punctuation,
         return $ punctuation ++ whitespace ++ alphanum]

-- | Test trim with various edge cases
test_trim_edge_cases :: TestTree
test_trim_edge_cases = testCase "trim handles edge cases" $ do
  let edgeCases = 
        [ ("", "")  -- empty string
        , (" ", "")  -- single space
        , ("\t", "")  -- single tab
        , ("\n", "")  -- single newline
        , ("\r", "")  -- single carriage return
        , ("  \t\n\r  ", "")  -- mixed whitespace only
        , ("a", "a")  -- single character
        , (" a", "a")  -- leading space
        , ("a ", "a")  -- trailing space
        , (" a ", "a")  -- both leading L.and trailing
        , ("  a  b  ", "a  b")  -- preserve internal whitespace
        , ("\t\n\r a b \t\n\r", "a b")  -- mixed whitespace around
        ]
  mapM_ (\(input, expected) -> do
    let result = trim input
    assertEqual ("trim on '" ++ input ++ "'") expected result
  ) edgeCases

-- | Test splitBy with edge cases
test_splitby_edge_cases :: TestTree
test_splitby_edge_cases = testCase "splitBy handles edge cases" $ do
  let edgeCases = 
        [ (':', "", [""])  -- empty string
        , (':', ":", ["", ""])  -- single delimiter
        , (':', "::", ["", "", ""])  -- double delimiter
        , (':', "a:b", ["a", "b"])  -- normal case
        , (':', "a::b", ["a", "", "b"])  -- empty segment
        , (':', ":a:", ["", "a", ""])  -- leading L.and trailing
        , (':', "::a::", ["", "", "a", "", ""])  -- multiple delimiters
        ]
  mapM_ (\(delim, input, expected) -> do
    let result = splitBy delim input
    assertEqual ("splitBy '" ++ [delim] ++ "' on '" ++ input ++ "'") expected result
  ) edgeCases

-- | Test splitByCollapsed with edge cases  
test_splitbycollapsed_edge_cases :: TestTree
test_splitbycollapsed_edge_cases = testCase "splitByCollapsed handles edge cases" $ do
  let edgeCases = 
        [ (':', "", [])  -- empty string
        , (':', ":", [])  -- single delimiter
        , (':', "::", [])  -- double delimiter
        , (':', "a:b", ["a", "b"])  -- normal case
        , (':', "a::b", ["a", "b"])  -- collapsed empty segment
        , (':', ":a:", ["a"])  -- leading L.and trailing collapsed
        , (':', "::a::", ["a"])  -- multiple delimiters collapsed
        ]
  mapM_ (\(delim, input, expected) -> do
    let result = splitByCollapsed delim input
    assertEqual ("splitByCollapsed '" ++ [delim] ++ "' on '" ++ input ++ "'") expected result
  ) edgeCases

-- | Test comment removal with edge cases
test_comment_removal_edge_cases :: TestTree
test_comment_removal_edge_cases = testCase "comment removal handles edge cases" $ do
  let edgeCases = 
        [ ("", "", "")  -- empty string
        , ("// comment", "", "")  -- only line comment
        , ("/* comment */", "", "")  -- only block comment
        , ("code // comment\nmore", "code \nmore", "code \nmore")  -- line comment in code
        , ("code /* comment */ more", "code  more", "code  more")  -- block comment in code
        , ("\"string // not comment\"", "\"string // not comment\"", "\"string // not comment\"")  -- comment in string
        , ("'char // not comment'", "'char // not comment'", "'char // not comment'")  -- comment in char
        , ("\"/* not comment */\"", "\"/* not comment */\"", "\"/* not comment */\"")  -- block comment in string
        ]
  mapM_ (\(input, expectedLine, expectedBoth) -> do
    let resultLine = removeLineComments input
        resultBoth = removeComments input
    assertEqual ("removeLineComments on '" ++ input ++ "'") expectedLine resultLine
    assertEqual ("removeComments on '" ++ input ++ "'") expectedBoth resultBoth
  ) edgeCases

-- | Test indentation normalization with edge cases
test_indentation_edge_cases :: TestTree
test_indentation_edge_cases = testCase "indentation normalization handles edge cases" $ do
  let edgeCases = 
        [ ("", "")  -- empty string
        , ("  ", "  ")  -- only whitespace
        , ("a", "a")  -- single line
        , ("  a", "a")  -- single indented line
        , ("  a\n  b", "a\nb")  -- two indented lines
        , ("  a\n    b\n  c", "a\n  b\nc")  -- mixed indentation
        , ("a\n  b\n    c", "a\n  b\n    c")  -- preserve relative indentation
        ]
  mapM_ (\(input, expected) -> do
    let result = normalizeIndentation input
    assertEqual ("normalizeIndentation on '" ++ input ++ "'") expected result
  ) edgeCases

-- | Property: trim removes L.all leading L.and trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let content = "content"
      input = prefix ++ content ++ suffix
      trimmed = trim input
      hasLeading = not (null prefix) && L.all isSpace prefix
      hasTrailing = not (null suffix) && L.all isSpace suffix
      noLeadingSpace = null trimmed || not (isSpace (L.head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

-- | Property: trim preserves internal whitespace exactly
prop_trim_preserves_internal :: String -> String -> String -> Property
prop_trim_preserves_internal prefix internal suffix =
  let content = prefix ++ internal ++ suffix
      trimmed = trim content
      -- Remove leading/trailing whitespace manually to compare
      manualTrim = dropWhile isSpace $ L.reverse $ dropWhile isSpace $ L.reverse content
  in property $ trimmed == manualTrim

-- | Property: splitBy preserves empty segments
prop_splitby_preserves_empty :: Char -> String -> Property
prop_splitby_preserves_empty delim input =
  let result = splitBy delim input
      expected = L.map (T.unpack . T.dropWhile (== delim)) $ 
                 T.splitOn (T.pack [delim]) (T.pack input)
  in property $ L.length result == L.length (L.filter (not . null) expected) + 
                   L.length (L.filter (== "") expected)

-- | Property: splitByCollapsed removes empty segments
prop_splitbycollapsed_removes_empty :: Char -> String -> Property
prop_splitbycollapsed_removes_empty delim input =
  let result = splitByCollapsed delim input
  in property $ L.all (not . null) result

-- | Property: splitBy L.and splitByCollapsed are related
prop_splitby_relationship :: Char -> String -> Property
prop_splitby_relationship delim input =
  let normal = splitBy delim input
      collapsed = splitByCollapsed delim input
      filtered = L.filter (not . null) normal
  in property $ collapsed == filtered

-- | Property: removeLineComments doesn't affect strings without comments
prop_remove_line_comments_no_effect :: String -> Property
prop_remove_line_comments_no_effect input =
  let hasNoLineComment = not (isInfixOf "//" input)
      result = removeLineComments input
  in hasNoLineComment ==> property $ result == input

-- | Property: removeComments doesn't affect strings without comments
prop_remove_comments_no_effect :: String -> Property
prop_remove_comments_no_effect input =
  let hasNoComments = not (isInfixOf "//" input) && not (isInfixOf "/*" input)
      result = removeComments input
  in hasNoComments ==> property $ result == input

-- | Property: removeLineComments is idempotent
prop_remove_line_comments_idempotent :: String -> Property
prop_remove_line_comments_idempotent input =
  let once = removeLineComments input
      twice = removeLineComments once
  in property $ once == twice

-- | Property: removeComments is idempotent
prop_remove_comments_idempotent :: String -> Property
prop_remove_comments_idempotent input =
  let once = removeComments input
      twice = removeComments once
  in property $ once == twice

-- | Property: normalizeIndentation preserves relative indentation
prop_normalize_preserves_relative :: Property
prop_normalize_preserves_relative = 
  forAll (choose (1, 10)) $ \numLines ->
  forAll (choose (0, 5)) $ \baseIndent ->
    let lines = L.map (\i -> replicate (baseIndent + i) ' ' ++ "line " ++ show i) [0..numLines-1]
        input = unlines lines
        result = normalizeIndentation input
        resultLines = lines result
        -- Check that relative indentation is preserved
        checkRelative [] = True
        checkRelative [_] = True
        checkRelative (x:y:xs) = 
          let indentX = L.length $ takeWhile isSpace x
              indentY = L.length $ takeWhile isSpace y
          in indentY >= indentX && checkRelative (y:xs)
    in property $ checkRelative resultLines

-- | Property: breakOn is equivalent to Data.List.break for simple cases
prop_break_on_equivalence :: Char -> String -> Property
prop_break_on_equivalence delim input =
  let myBreak = breakOn delim input
      listBreak = Data.List.break (== delim) input
  in property $ myBreak == listBreak

-- | Property: splitByComma is equivalent to splitBy ','
prop_split_by_comma_equivalence :: String -> Property
prop_split_by_comma_equivalence input =
  let commaSplit = splitByComma input
      generalSplit = splitBy ',' input
  in property $ commaSplit == generalSplit

-- | Property: splitByCommaCollapsed is equivalent to splitByCollapsed ','
prop_split_by_comma_collapsed_equivalence :: String -> Property
prop_split_by_comma_collapsed_equivalence input =
  let commaSplit = splitByCommaCollapsed input
      generalSplit = splitByCollapsed ',' input
  in property $ commaSplit == generalSplit

tests :: TestTree
tests = testGroup "Utils Boundary Tests"
  [ test_trim_edge_cases
  , test_splitby_edge_cases
  , test_splitbycollapsed_edge_cases
  , test_comment_removal_edge_cases
  , test_indentation_edge_cases
  , fastProperty "trim removes whitespace" prop_trim_removes_whitespace
  , fastProperty "trim preserves internal" prop_trim_preserves_internal
  , fastProperty "splitBy preserves empty" prop_splitby_preserves_empty
  , fastProperty "splitByCollapsed removes empty" prop_splitbycollapsed_removes_empty
  , fastProperty "splitBy relationship" prop_splitby_relationship
  , fastProperty "removeLineComments no effect" prop_remove_line_comments_no_effect
  , fastProperty "removeComments no effect" prop_remove_comments_no_effect
  , fastProperty "removeLineComments idempotent" prop_remove_line_comments_idempotent
  , fastProperty "removeComments idempotent" prop_remove_comments_idempotent
  , fastProperty "normalize preserves relative" prop_normalize_preserves_relative
  , fastProperty "breakOn equivalence" prop_break_on_equivalence
  , fastProperty "splitByComma equivalence" prop_split_by_comma_equivalence
  , fastProperty "splitByCommaCollapsed equivalence" prop_split_by_comma_collapsed_equivalence
  ]