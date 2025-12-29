{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewCabalUtilsStringProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf1, vectorOf, elements, suchThat)

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

import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, nub)
import Data.Char (isSpace, isAlphaNum, isLetter)

-- Generate a string with potential whitespace
genStringWithWhitespace :: Gen String
genStringWithWhitespace = do
  parts <- listOf1 $ do
    whitespace <- listOf $ elements " \t\n"
    content <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-.,;:"
    return $ whitespace ++ content
  finalWhitespace <- listOf $ elements " \t\n"
  return $ concat parts ++ finalWhitespace

-- Generate a string with comments
genStringWithComments :: Gen String
genStringWithComments = do
  linesCount <- choose (1, 5)
  lines' <- vectorOf linesCount $ do
    hasComment <- elements [True, False]
    content <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
    if hasComment
      then return $ unwords content ++ " // comment"
      else return $ unwords content
  return $ unlines lines'

-- Generate a string with block comments
genStringWithBlockComments :: Gen String
genStringWithBlockComments = do
  before <- listOf1 $ elements $ ['a'..'z'] ++ " "
  comment <- listOf1 $ elements $ ['a'..'z'] ++ " "
  after <- listOf1 $ elements $ ['a'..'z'] ++ " "
  return $ before ++ "/* " ++ comment ++ " */" ++ after

-- Generate a string with indentation
genStringWithIndentation :: Gen String
genStringWithIndentation = do
  linesCount <- choose (1, 5)
  lines' <- vectorOf linesCount $ do
    indentLevel <- choose (0, 4)
    indent <- return $ replicate indentLevel ' '
    content <- listOf1 $ elements $ ['a'..'z'] ++ " "
    return $ indent ++ unwords content
  return $ unlines lines'



-- Property: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  let trimmed = trim s
      hasLeadingSpace = not (null s) && isSpace (head s)
      hasTrailingSpace = not (null s) && isSpace (last s)
  in classify hasLeadingSpace "has leading space" $
     classify hasTrailingSpace "has trailing space" $
     counterexample ("Original: " ++ show s ++ ", Trimmed: " ++ show trimmed) $
     (null trimmed || not (isSpace (head trimmed))) .&&.
     (null trimmed || not (isSpace (last trimmed)))

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let once = trim s
      twice = trim once
  in once === twice

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim s =
  let parts = splitBy delim s
      rejoined = concat $ map (\p -> p ++ [delim]) (init parts) ++ [last parts]
  in length parts > 0 ==> rejoined === s

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim s =
  let parts = splitByCollapsed delim
  in all (not . null) parts === True

-- Property: splitByComma is splitBy with comma delimiter
prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy s =
  splitByComma s === splitBy ',' s

-- Property: splitByCommaCollapsed is splitByCollapsed with comma delimiter
prop_splitByCommaCollapsed_equals_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed s =
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- Property: removeLineComments removes lines starting with //
prop_removeLineComments_removes_comments :: String -> Property
prop_removeLineComments_removes_comments s =
  let cleaned = removeLineComments s
      hasComment = "//" `isInfixOf` s
  in classify hasComment "has comment" $
     not ("//" `isInfixOf` cleaned)

-- Property: removeComments removes content between /* and */
prop_removeComments_removes_block_comments :: String -> Property
prop_removeComments_removes_block_comments s =
  let withComment = "before" ++ "/* comment */" ++ "after"
      cleaned = removeComments withComment
  in "before" `isInfixOf` cleaned .&&. 
     "after" `isInfixOf` cleaned .&&.
     not (" comment " `isInfixOf` cleaned)

-- Property: normalizeIndentation preserves line count
prop_normalizeIndentation_preserves_lines :: String -> Property
prop_normalizeIndentation_preserves_lines s =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in length originalLines === length normalizedLines

-- Property: breakOn finds first occurrence or returns original
prop_breakOn_behavior :: String -> String -> Property
prop_breakOn_behavior needle haystack =
  let result = breakOn needle haystack
  in case result of
    (before, after) -> 
      if needle `isInfixOf` haystack
      then before ++ needle ++ after === haystack
      else before === haystack .&&. after === ""

-- Property: isIdentifier correctly identifies valid identifiers
prop_isIdentifier_correct :: String -> Property
prop_isIdentifier_correct s =
  let actualResult = isIdentifier s
      expectedResult = not (null s) && isLetter (head s) && all (\c -> isAlphaNum c || c == '_') (tail s)
  in actualResult === expectedResult

-- Property: isValidTypusIdentifier correctly identifies valid Typus identifiers
prop_isValidTypusIdentifier_correct :: String -> Property
prop_isValidTypusIdentifier_correct s =
  let actualResult = isValidTypusIdentifier s
      expectedResult = not (null s) && isLetter (head s) && all (\c -> isAlphaNum c || c == '_') s
  in actualResult === expectedResult

-- Property: escapeString and unescapeString are inverses
prop_escape_unescape_inverse :: String -> Property
prop_escape_unescape_inverse s =
  let escaped = escapeString s
      unescaped = unescapeString escaped
  in unescaped === s

-- Property: escapeString makes string safe
prop_escapeString_safe :: String -> Property
prop_escapeString_safe s =
  let escaped = escapeString s
  in not ('\n' `elem` escaped) .&&.
     not ('\t' `elem` escaped) .&&.
     not ('\r' `elem` escaped)

tests :: TestTree
tests =
  testGroup "Utils String Processing QuickCheck Tests"
    [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
    , fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
    , fastProperty "splitByComma equals splitBy with comma" prop_splitByComma_equals_splitBy
    , fastProperty "splitByCommaCollapsed equals splitByCollapsed with comma" prop_splitByCommaCollapsed_equals_splitByCollapsed
    , fastProperty "removeLineComments removes comments" prop_removeLineComments_removes_comments
    , fastProperty "removeComments removes block comments" prop_removeComments_removes_block_comments
    , fastProperty "normalizeIndentation preserves lines" prop_normalizeIndentation_preserves_lines
    , fastProperty "breakOn behavior" prop_breakOn_behavior
    , fastProperty "isIdentifier correct" prop_isIdentifier_correct
    , fastProperty "isValidTypusIdentifier correct" prop_isValidTypusIdentifier_correct
    , fastProperty "escape and unescape are inverse" prop_escape_unescape_inverse
    , fastProperty "escapeString makes string safe" prop_escapeString_safe
    ]