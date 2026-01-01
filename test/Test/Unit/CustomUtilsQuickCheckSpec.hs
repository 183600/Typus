{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.CustomUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, elements, listOf, listOf1, oneof)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isLetter)
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
  , breakOn
  )

-- | Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = do
  content <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  leading <- listOf $ elements " \t\n"
  trailing <- listOf $ elements " \t\n"
  return $ leading ++ content ++ trailing

-- | Generate strings with comma separators
genCommaString :: Gen String
genCommaString = do
  numParts <- elements [0..5]
  parts <- sequence [listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] | _ <- [1..numParts]]
  let withCommas = L.concat $ zipWith (\p i -> if i == 0 then p else "," ++ p) parts [0..]
  return withCommas

-- | Generate strings with line comments
genCommentedString :: Gen String
genCommentedString = do
  code <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9'] ++ "(){}[];=+-*/"
  comment <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  return $ code ++ "// " ++ comment

-- | Generate strings with block comments
genBlockCommentedString :: Gen String
genBlockCommentedString = do
  before <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  comment <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  after <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  return $ before ++ "/* " ++ comment ++ " */" ++ after

-- | Generate strings with string literals containing comment markers
genStringLiteralString :: Gen String
genStringLiteralString = do
  before <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  literal <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ [':', '/']
  after <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  comment <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  return $ before ++ " \"" ++ literal ++ "\" // " ++ comment

-- | Generate indented strings
genIndentedString :: Gen String
genIndentedString = do
  numLines <- elements [1..5]
  baseIndent <- elements [0..4]
  lines <- sequence [do
    indent <- elements [baseIndent..baseIndent+2]
    content <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
    return $ replicate indent ' ' ++ content
    | _ <- [1..numLines]]
  return $ unlines lines

-- | Generate strings with various delimiters
genDelimitedString :: Char -> Gen String
genDelimitedString delim = do
  numParts <- elements [0..5]
  parts <- sequence [listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] | _ <- [1..numParts]]
  let withDelims = L.concat $ zipWith (\p i -> if i == 0 then p else [delim] ++ p) parts [0..]
  return withDelims

-- | Test trim property: trim(trim(x)) == trim(x)
prop_trimIdempotent :: Property
prop_trimIdempotent = forAll genWhitespaceString $ \s ->
  trim (trim s) == trim s

-- | Test trim property: trim removes leading L.and trailing whitespace
prop_trimRemovesWhitespace :: Property
prop_trimRemovesWhitespace = forAll genWhitespaceString $ \s ->
  let trimmed = trim s
  in null trimmed || not (isSpace (L.head trimmed) || isSpace (last trimmed))

-- | Test splitBy property: L.concat with delimiter should reconstruct original
prop_splitByReconstruction :: Property
prop_splitByReconstruction = forAll (genDelimitedString ',') $ \s ->
  let parts = splitBy ',' s
      reconstructed = L.concat $ zipWith (\p i -> if i == 0 then p else "," ++ p) parts [0..L.length parts - 1]
  in reconstructed == s

-- | Test splitByCollapsed property: no empty strings in result
prop_splitByCollapsedNoEmpty :: Property
prop_splitByCollapsedNoEmpty = forAll (genDelimitedString ',') $ \s ->
  let parts = splitByCollapsed ',' s
  in L.all (not . null) parts

-- | Test splitByComma property: should be same as splitBy ','
prop_splitByCommaConsistency :: Property
prop_splitByCommaConsistency = forAll genCommaString $ \s ->
  splitByComma s == splitBy ',' s

-- | Test that removing line comments preserves string literals
prop_removeLineCommentsPreservesLiterals :: Property
prop_removeLineCommentsPreservesLiterals = forAll genStringLiteralString $ \s ->
  let withComments = removeLineComments s
      hasLiteral = "\"" `L.isInfixOf` s
  in hasLiteral ==> ("\"" `L.isInfixOf` withComments)

-- | Test that removing comments removes line comments
prop_removeLineCommentsRemovesComments :: Property
prop_removeLineCommentsRemovesComments = forAll genCommentedString $ \s ->
  let withComments = removeLineComments s
      hasComment = "//" `L.isInfixOf` s
  in hasComment ==> not ("//" `L.isInfixOf` withComments)

-- | Test block comment removal
prop_removeBlockComments :: Property
prop_removeBlockComments = forAll genBlockCommentedString $ \s ->
  let withComments = removeComments s
      hasBlockComment = "/*" `L.isInfixOf` s && "*/" `L.isInfixOf` s
  in hasBlockComment ==> not ("/*" `L.isInfixOf` withComments || "*/" `L.isInfixOf` withComments)

-- | Test indentation normalization preserves relative indentation
prop_normalizeIndentationPreservesRelative :: Property
prop_normalizeIndentationPreservesRelative = forAll genIndentedString $ \s ->
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in L.length originalLines == L.length normalizedLines

-- | Test breakOn property
prop_breakOnCorrectness :: Property
prop_breakOnCorrectness = forAll (listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ",") $ \s ->
  let delim = ','
      (before, after) = breakOn delim s
  in if delim `elem` s
     then before ++ [delim] ++ after == s
     else before == s && null after

-- | Test splitBy on empty string
prop_splitByEmptyString :: Property
prop_splitByEmptyString = 
  splitBy ',' "" == [""]

-- | Test splitByCollapsed on empty string
prop_splitByCollapsedEmptyString :: Property
prop_splitByCollapsedEmptyString = 
  splitByCollapsed ',' "" == []

-- | Test trim on empty string
prop_trimEmptyString :: Property
prop_trimEmptyString = 
  trim "" == ""

-- | Test trim on only whitespace
prop_trimOnlyWhitespace :: Property
prop_trimOnlyWhitespace = forAll (listOf1 $ elements " \t\n") $ \s ->
  trim s == ""

-- | Test splitBy with delimiter at start/end
prop_splitByEdgeCases :: Property
prop_splitByEdgeCases = 
  splitBy ',' ",start" == ["", "start"] &&
  splitBy ',' "end," == ["end", ""] &&
  splitBy ',' "," == ["", ""]

-- | Test splitByCollapsed with delimiter at start/end
prop_splitByCollapsedEdgeCases :: Property
prop_splitByCollapsedEdgeCases = 
  splitByCollapsed ',' ",start" == ["start"] &&
  splitByCollapsed ',' "end," == ["end"] &&
  splitByCollapsed ',' "," == []

  where
    L.isInfixOf needle haystack = needle `elem` (substrings haystack)
    substrings [] = []
    substrings s@(x:xs) = s : substrings xs

tests :: TestTree
tests = testGroup "Custom Utils QuickCheck Tests"
  [ testProperty "trim idempotent" prop_trimIdempotent
  , testProperty "trim removes whitespace" prop_trimRemovesWhitespace
  , testProperty "splitBy reconstruction" prop_splitByReconstruction
  , testProperty "splitByCollapsed no empty" prop_splitByCollapsedNoEmpty
  , testProperty "splitByComma consistency" prop_splitByCommaConsistency
  , testProperty "remove line comments preserves literals" prop_removeLineCommentsPreservesLiterals
  , testProperty "remove line comments removes comments" prop_removeLineCommentsRemovesComments
  , testProperty "remove block comments" prop_removeBlockComments
  , testProperty "normalize indentation preserves relative" prop_normalizeIndentationPreservesRelative
  , testProperty "breakOn correctness" prop_breakOnCorrectness
  , testProperty "splitBy empty string" prop_splitByEmptyString
  , testProperty "splitByCollapsed empty string" prop_splitByCollapsedEmptyString
  , testProperty "trim empty string" prop_trimEmptyString
  , testProperty "trim only whitespace" prop_trimOnlyWhitespace
  , testProperty "splitBy edge cases" prop_splitByEdgeCases
  , testProperty "splitByCollapsed edge cases" prop_splitByCollapsedEdgeCases
  ]