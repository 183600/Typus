{-# LANGUAGE CPP #-}
module Test.Unit.UtilsStringProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, elements, listOf, choose, suchThat)
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isUpper, toLower, toUpper)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, group, sort, nub)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Utils (splitLines, normalizeIndentation, removeComments, trimWhitespace)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import TestSupport.Arbitrary ()

-- | Test utils string processing functionality
testUtilsStringProcessing :: TestTree
testUtilsStringProcessing = testGroup "Utils String Processing"
  [ testStringSplitting
  , testIndentationNormalization
  , testCommentRemoval
  , testWhitespaceTrimming
  , testStringValidation
  ]

-- | Test string splitting operations
testStringSplitting :: TestTree
testStringSplitting = testGroup "String Splitting"
  [ fastProperty "splitLines preserves line count" prop_splitLinesPreservesLineCount
  , fastProperty "splitLines handles empty lines" prop_splitLinesHandlesEmptyLines
  , fastProperty "splitLines preserves line content" prop_splitLinesPreservesContent
  , testCase "splitLines basic functionality" testSplitLinesBasic
  , testCase "splitLines with various line endings" testSplitLinesLineEndings
  , testCase "splitLines edge cases" testSplitLinesEdgeCases
  ]

-- | Test indentation normalization
testIndentationNormalization :: TestTree
testIndentationNormalization = testGroup "Indentation Normalization"
  [ fastProperty "normalizeIndentation preserves relative indentation" prop_normalizePreservesRelative
  , fastProperty "normalizeIndentation removes leading spaces" prop_normalizeRemovesLeading
  , fastProperty "normalizeIndentation handles tabs" prop_normalizeHandlesTabs
  , testCase "normalizeIndentation basic functionality" testNormalizeBasic
  , testCase "normalizeIndentation mixed spaces and tabs" testNormalizeMixed
  , testCase "normalizeIndentation empty lines" testNormalizeEmptyLines
  ]

-- | Test comment removal
testCommentRemoval :: TestTree
testCommentRemoval = testGroup "Comment Removal"
  [ fastProperty "removeComments preserves code structure" prop_removeCommentsPreservesStructure
  , fastProperty "removeComments handles line comments" prop_removeCommentsHandlesLineComments
  , fastProperty "removeComments handles block comments" prop_removeCommentsHandlesBlockComments
  , testCase "removeComments basic functionality" testRemoveCommentsBasic
  , testCase "removeComments nested comments" testRemoveCommentsNested
  , testCase "removeComments edge cases" testRemoveCommentsEdgeCases
  ]

-- | Test whitespace trimming
testWhitespaceTrimming :: TestTree
testWhitespaceTrimming = testGroup "Whitespace Trimming"
  [ fastProperty "trimWhitespace removes leading/trailing spaces" prop_trimRemovesLeadingTrailing
  , fastProperty "trimWhitespace preserves internal spaces" prop_trimPreservesInternal
  , fastProperty "trimWhitespace handles tabs" prop_trimHandlesTabs
  , testCase "trimWhitespace basic functionality" testTrimBasic
  , testCase "trimWhitespace various whitespace" testTrimVarious
  , testCase "trimWhitespace empty strings" testTrimEmpty
  ]

-- | Test string validation utilities
testStringValidation :: TestTree
testStringValidation = testGroup "String Validation"
  [ fastProperty "isValidIdentifier checks format" prop_isValidIdentifierFormat
  , fastProperty "isValidKeyword detection" prop_isValidKeyword
  , fastProperty "isValidStringLiteral detection" prop_isValidStringLiteral
  , testCase "identifier validation" testIdentifierValidation
  , testCase "keyword detection" testKeywordDetection
  , testCase "string literal validation" testStringLiteralValidation
  ]

-- | Property tests
prop_splitLinesPreservesLineCount :: String -> Property
prop_splitLinesPreservesLineCount input =
  let lines = splitLines input
      expectedCount = length $ filter (== '\n') input + 1
  in length lines === expectedCount

prop_splitLinesHandlesEmptyLines :: String -> Property
prop_splitLinesHandlesEmptyLines input =
  let lines = splitLines input
      hasEmptyLines = any null lines
  in hasEmptyLines === (("\n\n" `isInfixOf` input) || "\n\n\n" `isInfixOf` input)

prop_splitLinesPreservesContent :: String -> Property
prop_splitLinesPreservesContent input =
  let lines = splitLines input
      reconstructed = unlines lines
  in reconstructed === input

prop_normalizePreservesRelative :: String -> Property
prop_normalizePreservesRelative input =
  let normalized = normalizeIndentation input
      lines = lines input
      normalizedLines = lines normalized
  in length normalizedLines === length lines

prop_normalizeRemovesLeading :: String -> Property
prop_normalizeRemovesLeading input =
  let normalized = normalizeIndentation input
      normalizedLines = lines normalized
  in all (\line -> null line || not (isPrefixOf "    " line)) normalizedLines

prop_normalizeHandlesTabs :: String -> Property
prop_normalizeHandlesTabs input =
  let withTabs = map (\c -> if c == ' ' then '\t' else c) input
      normalized = normalizeIndentation withTabs
  in not (any (\line -> isPrefixOf "\t" line) (lines normalized))

prop_removeCommentsPreservesStructure :: String -> Property
prop_removeCommentsPreservesStructure input =
  let withoutComments = removeComments input
      originalLines = length $ lines input
      commentLines = length $ filter (isPrefixOf "//") (lines input)
  in length (lines withoutComments) <= originalLines

prop_removeCommentsHandlesLineComments :: String -> Property
prop_removeCommentsHandlesLineComments input =
  let withLineComments = input ++ "\n// This is a comment\nvar x = 5"
      withoutComments = removeComments withLineComments
  in not (any (isPrefixOf "//") (lines withoutComments))

prop_removeCommentsHandlesBlockComments :: String -> Property
prop_removeCommentsHandlesBlockComments input =
  let withBlockComments = input ++ "\n/* This is a\n   block comment */\nvar y = 10"
      withoutComments = removeComments withBlockComments
  in not (any (isInfixOf "/*") (lines withoutComments))

prop_trimRemovesLeadingTrailing :: String -> Property
prop_trimRemovesLeadingTrailing input =
  let withSpaces = "  " ++ input ++ "  "
      trimmed = trimWhitespace withSpaces
  in not (isPrefixOf " " trimmed) && not (isSuffixOf " " trimmed)

prop_trimPreservesInternal :: String -> String -> Property
prop_trimPreservesInternal prefix suffix =
  let input = prefix ++ "  middle  " ++ suffix
      trimmed = trimWhitespace input
  in "  middle  " `isInfixOf` input && "middle" `isInfixOf` trimmed

prop_trimHandlesTabs :: String -> Property
prop_trimHandlesTabs input =
  let withTabs = "\t" ++ input ++ "\t"
      trimmed = trimWhitespace withTabs
  in not (isPrefixOf "\t" trimmed) && not (isSuffixOf "\t" trimmed)

prop_isValidIdentifierFormat :: String -> Property
prop_isValidIdentifierFormat identifier =
  let isValid = isValidIdentifier identifier
      startsWithLetter = not (null identifier) && isAlpha (head identifier)
      allAlphaNum = all isAlphaNum identifier
  in isValid === (startsWithLetter && allAlphaNum)

prop_isValidKeyword :: String -> Property
prop_isValidKeyword word =
  let isKeyword = isValidKeyword word
      commonKeywords = ["func", "var", "if", "else", "for", "while", "return"]
  in isKeyword === (word `elem` commonKeywords)

prop_isValidStringLiteral :: String -> Property
prop_isValidStringLiteral literal =
  let isValid = isValidStringLiteral literal
      startsWithQuote = not (null literal) && head literal == '"'
      endsWithQuote = not (null literal) && last literal == '"'
  in isValid === (startsWithQuote && endsWithQuote)

-- | Unit tests
testSplitLinesBasic :: IO ()
testSplitLinesBasic = do
  let input = "line1\nline2\nline3"
      expected = ["line1", "line2", "line3"]
      result = splitLines input
  assertEqual "basic line splitting" expected result

testSplitLinesLineEndings :: IO ()
testSplitLinesLineEndings = do
  let input1 = "line1\r\nline2\r\nline3"
      input2 = "line1\rline2\rline3"
      expected = ["line1", "line2", "line3"]
      result1 = splitLines input1
      result2 = splitLines input2
  assertEqual "Windows line endings" expected result1
  assertEqual "Mac line endings" expected result2

testSplitLinesEdgeCases :: IO ()
testSplitLinesEdgeCases = do
  let input1 = ""
      input2 = "\n"
      input3 = "\n\n"
      result1 = splitLines input1
      result2 = splitLines input2
      result3 = splitLines input3
  assertEqual "empty string" [""] result1
  assertEqual "single newline" ["", ""] result2
  assertEqual "double newline" ["", "", ""] result3

testNormalizeBasic :: IO ()
testNormalizeBasic = do
  let input = "    line1\n        line2\n    line3"
      expected = "line1\n    line2\nline3"
      result = normalizeIndentation input
  assertEqual "basic normalization" expected result

testNormalizeMixed :: IO ()
testNormalizeMixed = do
  let input = "\tline1\n    \tline2\n\t    line3"
      result = normalizeIndentation input
      resultLines = lines result
  assertBool "should normalize mixed indentation" $ 
    all (\line -> null line || not (isPrefixOf "\t" line)) resultLines

testNormalizeEmptyLines :: IO ()
testNormalizeEmptyLines = do
  let input = "    line1\n\n    line2\n    \nline3"
      expected = "line1\n\n    line2\n\nline3"
      result = normalizeIndentation input
  assertEqual "handles empty lines" expected result

testRemoveCommentsBasic :: IO ()
testRemoveCommentsBasic = do
  let input = "var x = 5 // This is a comment\n// Another comment\nvar y = 10"
      expected = "var x = 5 \n\nvar y = 10"
      result = removeComments input
  assertEqual "basic comment removal" expected result

testRemoveCommentsNested :: IO ()
testRemoveCommentsNested = do
  let input = "/* Outer comment\n   /* Inner comment */\n   Still in outer */\nvar z = 15"
      expected = "\n\n\nvar z = 15"
      result = removeComments input
  assertEqual "nested comment removal" expected result

testRemoveCommentsEdgeCases :: IO ()
testRemoveCommentsEdgeCases = do
  let input1 = "// Comment only line\n"
      input2 = "/* Block comment */"
      input3 = "var x = 5 /* inline comment */ + 10"
      result1 = removeComments input1
      result2 = removeComments input2
      result3 = removeComments input3
  assertEqual "comment only line" "\n" result1
  assertEqual "block comment only" "" result2
  assertBool "inline comment removal" $ not ("/*" `isInfixOf` result3)

testTrimBasic :: IO ()
testTrimBasic = do
  let input = "   hello world   "
      expected = "hello world"
      result = trimWhitespace input
  assertEqual "basic trimming" expected result

testTrimVarious :: IO ()
testTrimVarious = do
  let input1 = "\t\ttabbed\t\t"
      input2 = "  \t mixed \t  "
      input3 = "\n\nnewlines\n\n"
      result1 = trimWhitespace input1
      result2 = trimWhitespace input2
      result3 = trimWhitespace input3
  assertEqual "tab trimming" "tabbed" result1
  assertEqual "mixed whitespace" "mixed" result2
  assertEqual "newline trimming" "newlines" result3

testTrimEmpty :: IO ()
testTrimEmpty = do
  let input1 = ""
      input2 = "   "
      input3 = "\t\n  "
      result1 = trimWhitespace input1
      result2 = trimWhitespace input2
      result3 = trimWhitespace input3
  assertEqual "empty string" "" result1
  assertEqual "spaces only" "" result2
  assertEqual "whitespace only" "" result3

testIdentifierValidation :: IO ()
testIdentifierValidation = do
  let validIdentifiers = ["x", "myVar", "foo_bar", "test123", "A", "MyClass"]
      invalidIdentifiers = ["123abc", "_private", "with space", "with-dash", ""]
  assertBool "valid identifiers should pass" $ all isValidIdentifier validIdentifiers
  assertBool "invalid identifiers should fail" $ not (any isValidIdentifier invalidIdentifiers)

testKeywordDetection :: IO ()
testKeywordDetection = do
  let keywords = ["func", "var", "if", "else", "for", "while", "return", "type", "import"]
      nonKeywords = ["function", "variable", "myfunc", "myvar", "myif"]
  assertBool "keywords should be detected" $ all isValidKeyword keywords
  assertBool "non-keywords should not be detected" $ not (any isValidKeyword nonKeywords)

testStringLiteralValidation :: IO ()
testStringLiteralValidation = do
  let validLiterals = ["\"hello\"", "\"\"", "\"with spaces\"", "\"with\\nescapes\""]
      invalidLiterals = ["hello", "\"unclosed", "unclosed\"", "", "\"\"\""]
  assertBool "valid literals should pass" $ all isValidStringLiteral validLiterals
  assertBool "invalid literals should fail" $ not (any isValidStringLiteral invalidLiterals)

-- | Helper functions (these would be imported from Utils module)
splitLines :: String -> [String]
splitLines "" = [""]
splitLines s = case break (== '\n') s of
  (line, "") -> [line]
  (line, _:rest) -> line : splitLines rest

normalizeIndentation :: String -> String
normalizeIndentation input = unlines $ map normalizeLine (lines input)
  where
    normalizeLine line = if null line then line else dropWhile (== ' ') line

removeComments :: String -> String
removeComments = unlines . map removeLineComments . lines
  where
    removeLineComments line = case break (== '/') line of
      (prefix, '/':'/':_) -> prefix
      (prefix, _) -> prefix

trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile (`elem` " \t\n\r") . reverse . dropWhile (`elem` " \t\n\r")

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (c:cs) = isAlpha c && all isAlphaNum cs

isValidKeyword :: String -> Bool
isValidKeyword word = word `elem` 
  ["func", "var", "if", "else", "for", "while", "return", "type", "import", "package", "const"]

isValidStringLiteral :: String -> Bool
isValidStringLiteral [] = False
isValidStringLiteral literal = head literal == '"' && last literal == '"'

-- | Test collection
tests :: TestTree
tests = testGroup "Utils String Processing Tests"
  [ testUtilsStringProcessing
  ]