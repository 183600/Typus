{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CabalUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum, isLetter, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)

import Utils
  ( trim
  , splitBy
  , splitByComma
  , removeLineComments
  , normalizeIndentation
  , escapeString
  , unescapeString
  )

-- Simple arbitrary instances for utils testing
newtype NonEmptyString = NonEmptyString String deriving (Show, Eq)

instance Arbitrary NonEmptyString where
  arbitrary = do
    chars <- listOf1 $ elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']
    return $ NonEmptyString chars

newtype StringWithSpaces = StringWithSpaces String deriving (Show, Eq)

instance Arbitrary StringWithSpaces where
  arbitrary = do
    NonEmptyString core <- arbitrary
    leading <- listOf $ return ' '
    trailing <- listOf $ return ' '
    return $ StringWithSpaces (leading ++ core ++ trailing)

newtype CommaSeparatedString = CommaSeparatedString String deriving (Show, Eq)

instance Arbitrary CommaSeparatedString where
  arbitrary = do
    parts <- listOf1 $ listOf1 $ elements ['a'..'z']
    return $ CommaSeparatedString $ List.intercalate "," parts

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: StringWithSpaces -> Property
prop_trim_removes_whitespace (StringWithSpaces str) =
  let trimmed = trim str
      hasLeading = not (null str) && isSpace (head str)
      hasTrailing = not (null str) && isSpace (last str)
      noLeading = null trimmed || not (isSpace (head trimmed))
      noTrailing = null trimmed || not (isSpace (last trimmed))
  in classify hasLeading "has leading spaces" $
     classify hasTrailing "has trailing spaces" $
     property $ noLeading .&&. noTrailing

-- Property: trim preserves internal content
prop_trim_preserves_content :: NonEmptyString -> String -> String -> Property
prop_trim_preserves_content (NonEmptyString core) prefix suffix =
  let full = prefix ++ core ++ suffix
      trimmed = trim full
  in property $ core `List.isInfixOf` trimmed

-- Property: splitBy correctly splits on delimiter
prop_split_by_correct :: String -> Char -> Property
prop_split_by_correct str delim =
  let parts = splitBy delim str
      rejoined = List.intercalate [delim] parts
  in property $ (filter (/= delim) str) == (filter (/= delim) rejoined)

-- Property: splitByComma handles empty strings
prop_split_by_comma_empty :: Property
prop_split_by_comma_empty =
  let parts = splitByComma ""
  in property $ parts == [""]

-- Property: removeLineComments removes comment lines
prop_remove_line_comments :: [String] -> Property
prop_remove_line_comments lines =
  let withComments = map (\l -> if even (length l) then "// " ++ l else l) lines
      withoutComments = removeLineComments $ unlines withComments
      commentLines = filter (isPrefixOf "//") withComments
  in property $ not $ any (`List.isInfixOf` withoutComments) commentLines
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- Property: normalizeIndentation makes indentation consistent
prop_normalize_indentation :: [String] -> Property
prop_normalize_indentation lines =
  let indented = map (\l -> "  " ++ l) lines
      normalized = normalizeIndentation $ unlines indented
      lines' = lines normalized
  in property $ all (\l -> not ("  " `List.isPrefixOf` l) || l == "  " ++ dropWhile isSpace l) lines'

-- Property: escapeString and unescapeString are inverses
prop_escape_unescape_inverse :: String -> Property
prop_escape_unescape_inverse str =
  let escaped = escapeString str
      unescaped = unescapeString escaped
  in property $ str == unescaped

-- Property: escapeString handles special characters
prop_escape_special_chars :: String -> Property
prop_escape_special_chars str =
  let escaped = escapeString str
      hasNewlines = '\n' `elem` str
      hasTabs = '\t' `elem` str
      hasQuotes = '"' `elem` str
  in classify hasNewlines "has newlines" $
     classify hasTabs "has tabs" $
     classify hasQuotes "has quotes" $
     property $ not (any (`elem` escaped) ['\n', '\t', '"'])

tests :: TestTree
tests = testGroup "Cabal Utils QuickCheck Tests"
  [ fastProperty "Trim removes whitespace" prop_trim_removes_whitespace
  , fastProperty "Trim preserves content" prop_trim_preserves_content
  , fastProperty "Split by delimiter correct" prop_split_by_correct
  , fastProperty "Split by comma handles empty" prop_split_by_comma_empty
  , fastProperty "Remove line comments" prop_remove_line_comments
  , fastProperty "Normalize indentation" prop_normalize_indentation
  , fastProperty "Escape/unescape inverse" prop_escape_unescape_inverse
  , fastProperty "Escape special characters" prop_escape_special_chars
  , testCase "Utils handle complex string operations" $ do
      let complex = "  hello,  world  \n  // comment\n  \"quoted string\"\n  "
          trimmed = trim complex
          parts = splitBy ',' trimmed
          normalized = normalizeIndentation complex
      assertFailure $ "Original: " ++ show complex ++ "\n" ++
                     "Trimmed: " ++ show trimmed ++ "\n" ++
                     "Parts: " ++ show parts ++ "\n" ++
                     "Normalized: " ++ show normalized
  ]