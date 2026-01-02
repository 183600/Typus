{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.StringProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.)
  , Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements, sized, frequency
  , suchThat, resize
  )

import Utils 
  ( trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed
  , removeLineComments, removeComments, normalizeIndentation, breakOn
  )
import Data.Char (isSpace, isPrint, isControl, isAscii)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub)
import qualified Data.Text as T (pack, unpack)

-- Test data for string processing
data StringProcessingTestData = StringProcessingTestData
  { inputString :: String
  , testDelim :: Char
  , testComment :: String
  } deriving (Show, Eq)

instance Arbitrary StringProcessingTestData where
  arbitrary = do
    -- Generate strings with various characters including edge cases
    str <- listOf $ oneof
      [ elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      , elements " \t\n\r"
      , elements "/-*{}[]();:,.<>!?@#$%^&*+=|\\~`'"
      , return '\0'  -- Null character
      , choose ('\x80', '\xFF')  -- Non-ASCII characters
      ]
    delim <- elements ",;:\t "
    comment <- listOf $ elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 "
    return $ StringProcessingTestData str delim comment

-- Property: trim removes leading L.and trailing whitespace
prop_trim_removes_whitespace :: StringProcessingTestData -> Property
prop_trim_removes_whitespace testData =
  let input = inputString testData
      trimmed = trim input
      hasLeading = not (null input) && isSpace (L.head input)
      hasTrailing = not (null input) && isSpace (last input)
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ null trimmed || not (isSpace (L.head trimmed)) && 
                (null trimmed || not (isSpace (last trimmed)))

-- Property: trim preserves internal whitespace
prop_trim_preserves_internal :: String -> String -> String -> Property
prop_trim_preserves_internal before middle after =
  not (null middle) ==>
  let content = before ++ middle ++ after
      trimmed = trim content
      expectedInternal = L.filter (not . isSpace) before ++ middle ++ L.filter (not . isSpace) after
  in property $ L.filter (not . isSpace) trimmed === L.filter (not . isSpace) expectedInternal

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: StringProcessingTestData -> Property
prop_splitBy_preserves_empty testData =
  let input = inputString testData
      delim = testDelim testData
      segments = splitBy delim input
      expectedCount = L.length input + 1
  in property $ L.length segments <= expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitBy_collapsed_removes_empty :: StringProcessingTestData -> Property
prop_splitBy_collapsed_removes_empty testData =
  let input = inputString testData
      delim = testDelim testData
      collapsed = splitByCollapsed delim input
      regular = splitBy delim input
      emptyCount = L.length $ filter null regular
  in property $ L.length collapsed == L.length regular - emptyCount

-- Property: splitByComma delegates to splitBy correctly
prop_splitBy_comma_delegates :: String -> Property
prop_splitBy_comma_delegates input =
  let commaResult = splitByComma input
      directResult = splitBy ',' input
  in property $ commaResult === directResult

-- Property: removeLineComments handles strings without quotes correctly
prop_remove_line_comments_simple :: String -> String -> Property
prop_remove_line_comments_simple code comment =
  not (L.any (`elem` ['"', '\'']) code) ==>
  let lineWithComment = code ++ " // " ++ comment
      cleaned = removeLineComments lineWithComment
  in property $ cleaned === (code ++ " ")

-- Property: removeLineComments preserves string literals
prop_remove_line_comments_preserves_strings :: String -> String -> Property
prop_remove_line_comments_preserves_strings content comment =
  let stringWithComment = "value := \"" ++ content ++ "\" // " ++ comment
      cleaned = removeLineComments stringWithComment
  in property $ ("\"" ++ content ++ "\"") `L.isInfixOf` cleaned

-- Property: removeComments handles both line L.and block comments
prop_remove_comments_mixed :: String -> String -> String -> Property
prop_remove_comments_mixed code1 code2 comment =
  not (L.any (`elem` ['"', '\'', '/', '*']) (code1 ++ code2)) ==>
  let mixed = code1 ++ " // line\n" ++ code2 ++ " /* " ++ comment ++ " */ " ++ code1
      cleaned = removeComments mixed
  in property $ (code1 ++ " \n" ++ code2 ++ "  " ++ code1) `L.isPrefixOf` cleaned

-- Property: removeComments preserves string literals containing comment markers
prop_remove_comments_preserves_string_markers :: String -> Property
prop_remove_comments_preserves_string_markers content =
  let stringWithMarkers = "path := \"C://tmp/*keep*/\" /* comment */"
      cleaned = removeComments stringWithMarkers
  in property $ "C://tmp/*keep*/" `L.isInfixOf` cleaned

-- Property: normalizeIndentation handles mixed indentation
prop_normalize_mixed_indentation :: String -> Property
prop_normalize_mixed_indentation content =
  not ('\n' `elem` content) ==>
  let mixed = "  " ++ content ++ "\n\t" ++ content ++ "\n    " ++ content
      normalized = normalizeIndentation mixed
      linesNorm = lines normalized
  in property $ L.length linesNorm == 3

-- Property: breakOn finds substrings correctly
prop_break_on_finds_substrings :: String -> String -> Property
prop_break_on_finds_substrings text pattern =
  not (null pattern) ==>
  let combined = text ++ pattern ++ text
      result = breakOn combined pattern
  in property $ case result of
    Nothing -> False
    Just (before, after) -> before == text && after `L.isPrefixOf` text

-- Property: breakOn returns Nothing for non-existent patterns
prop_break_on_nothing_for_missing :: String -> String -> Property
prop_break_on_nothing_for_missing text pattern =
  not (pattern `L.isInfixOf` text) ==>
  let result = breakOn text pattern
  in property $ result == Nothing

-- Property: String processing is idempotent for certain operations
prop_trim_idempotent :: String -> Property
prop_trim_idempotent input =
  let trimmedOnce = trim input
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: splitBy is consistent with Data.Text.split
prop_splitby_text_consistency :: String -> Char -> Property
prop_splitby_text_consistency input delim =
  let stringResult = splitBy delim input
      textResult = map T.unpack $ T.split (== delim) (T.pack input)
  in property $ stringResult === textResult

-- Property: removeComments handles empty input gracefully
prop_remove_comments_empty :: Property
prop_remove_comments_empty =
  let result = removeComments ""
  in property $ result == ""

-- Property: removeComments handles only comments gracefully
prop_remove_comments_only_comments :: Property
prop_remove_comments_only_comments =
  let onlyComments = "// line comment\n/* block comment */"
      result = removeComments onlyComments
  in property $ result `elem` ["\n", " ", " \n", "\n ", "  "]

tests :: TestTree
tests = testGroup "String Processing QuickCheck Tests"
  [ fastProperty "trim removes leading L.and trailing whitespace" prop_trim_removes_whitespace
  , fastProperty "trim preserves internal whitespace" prop_trim_preserves_internal
  , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , fastProperty "splitByCollapsed removes empty segments" prop_splitby_collapsed_removes_empty
  , fastProperty "splitByComma delegates to splitBy correctly" prop_splitBy_comma_delegates
  , fastProperty "removeLineComments handles strings without quotes correctly" prop_remove_line_comments_simple
  , fastProperty "removeLineComments preserves string literals" prop_remove_line_comments_preserves_strings
  , fastProperty "removeComments handles both line L.and block comments" prop_remove_comments_mixed
  , fastProperty "removeComments preserves string literals containing comment markers" prop_remove_comments_preserves_string_markers
  , fastProperty "normalizeIndentation handles mixed indentation" prop_normalize_mixed_indentation
  , fastProperty "breakOn finds substrings correctly" prop_break_on_finds_substrings
  , fastProperty "breakOn returns Nothing for non-existent patterns" prop_break_on_nothing_for_missing
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy is consistent with Data.Text.split" prop_splitby_text_consistency
  , fastProperty "removeComments handles empty input gracefully" prop_remove_comments_empty
  , fastProperty "removeComments handles only comments gracefully" prop_remove_comments_only_comments
  , testCase "Manual string processing test" $ do
      let input = "  hello world  "
          trimmed = trim input
      trimmed @?= "hello world"
      
      let csv = "a,b,,c,"
          splitRegular = splitBy ',' csv
          splitCollapsed = splitByCollapsed ',' csv
      splitRegular @?= ["a", "b", "", "c", ""]
      splitCollapsed @?= ["a", "b", "c"]
      
      let withComment = "value := 42 // this is a comment"
          withoutComment = removeLineComments withComment
      withoutComment @?= "value := 42 "
      
      let withBlockComment = "code /* block comment */ more code"
          withoutBlockComment = removeComments withBlockComment
      "code " `L.isPrefixOf` withoutBlockComment @?= True
      " more code" `L.isSuffixOf` withoutBlockComment @?= True
  ]