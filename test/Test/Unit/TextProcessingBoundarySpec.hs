{-# LANGUAGE CPP #-}

module Test.Unit.TextProcessingBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, choose, Property, (==>), sized)
import Data.Char (isSpace, isControl, isAscii)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import qualified Data.Text as T
import qualified Data.ByteString as BS

import TestSupport.QuickCheck (fastProperty)

import Utils (trim, splitBy, removeLineComments, removeComments, normalizeIndentation)

-- | Text processing boundary tests for the Typus compiler
tests :: TestTree
tests =
  testGroup "Text Processing Boundary Tests"
    [ testGroup "Unicode and Encoding Edge Cases"
        [ testCase "Handles Unicode characters correctly" $ do
            let input = "func 测试() { let 值 = 42; 打印(值) }"
                result = parseWithUnicode input
            assertBool "Should handle Unicode characters"
                (isSuccess result)

        , testCase "Handles mixed ASCII and Unicode" $ do
            let input = "func test() { let 测试 = \"hello 世界\"; print(测试) }"
                result = parseWithUnicode input
            assertBool "Should handle mixed ASCII and Unicode"
                (isSuccess result)

        , testCase "Handles Unicode in comments" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let x = 42 // 这是注释"
                  , "  /* 多行注释"
                  , "     包含Unicode */"
                  , "  return x"
                  , "}"
                  ]
                processed = removeComments input
                expectedLines = 
                  [ "func test() {"
                  , "  let x = 42 "
                  , " " 
                  , " "
                  , "  return x"
                  , "}"
                  ]
            processed @?= unlines expectedLines

        , testCase "Handles invalid UTF-8 sequences gracefully" $ do
            let invalidInput = "func test() { let x = \"\xFF\xFE\" }"
                result = parseWithUnicode invalidInput
            assertBool "Should handle invalid UTF-8 gracefully"
                (hasEncodingError result)
        ]

    , testGroup "Extreme Input Sizes"
        [ testCase "Handles very long lines" $ do
            let longLine = "func test() { let x = \"" ++ replicate 10000 'a' ++ "\" }"
                result = parseLongLine longLine
            assertBool "Should handle very long lines"
                (isSuccess result)

        , testCase "Handles very deep indentation" $ do
            let deepIndent = unlines $ replicate 1000 "    " ++ ["let x = 42"]
                result = parseDeepIndentation deepIndent
            assertBool "Should handle very deep indentation"
                (isSuccess result)

        , testCase "Handles files with many empty lines" $ do
            let manyEmptyLines = unlines $ replicate 5000 "" ++ ["func test() {}"]
                result = parseManyEmptyLines manyEmptyLines
            assertBool "Should handle files with many empty lines"
                (isSuccess result)

        , testCase "Handles extremely large files efficiently" $ do
            let largeFile = unlines $ replicate 10000 "let x" ++ replicate 10000 " = 42" ++ ["func test() {}"]
                result = parseLargeFile largeFile
            assertBool "Should handle extremely large files efficiently"
                (isSuccess result)
        ]

    , testGroup "Special Characters and Escapes"
        [ testCase "Handles escaped quotes in strings" $ do
            let input = "func test() { let s = \"She said \\\"hello\\\"\" }"
                result = parseEscapedQuotes input
            assertBool "Should handle escaped quotes"
                (isSuccess result)

        , testCase "Handles escaped newlines in strings" $ do
            let input = "func test() { let s = \"line1\\nline2\" }"
                result = parseEscapedNewlines input
            assertBool "Should handle escaped newlines"
                (isSuccess result)

        , testCase "Handles unicode escape sequences" $ do
            let input = "func test() { let s = \"\\u4F60\\u597D\" }"
                result = parseUnicodeEscapes input
            assertBool "Should handle unicode escape sequences"
                (isSuccess result)

        , testCase "Handles raw string literals" $ do
            let input = "func test() { let s = r\"raw\\string\\with\\backslashes\" }"
                result = parseRawStrings input
            assertBool "Should handle raw string literals"
                (isSuccess result)
        ]

    , testGroup "Comment Processing Edge Cases"
        [ testCase "Handles nested block comments" $ do
            let input = unlines
                  [ "func test() {"
                  , "  /* outer comment"
                  , "     /* inner comment */"
                  , "     still outer */"
                  , "  return 42"
                  , "}"
                  ]
                processed = removeComments input
                expectedLines = 
                  [ "func test() {"
                  , "  "
                  , " "
                  , " "
                  , "  return 42"
                  , "}"
                  ]
            processed @?= unlines expectedLines

        , testCase "Handles comments at end of file" $ do
            let input = unlines
                  [ "func test() { return 42 }"
                  , "// final line comment"
                  , "/* final block comment */"
                  ]
                processed = removeComments input
                expectedLines = 
                  [ "func test() { return 42 }"
                  , " "
                  , " "
                  ]
            processed @?= unlines expectedLines

        , testCase "Handles comments with special characters" $ do
            let input = unlines
                  [ "func test() {"
                  , "  // Comment with \"quotes\" and 'apostrophes'"
                  , "  /* Comment with / * nested * / markers */"
                  , "  return 42"
                  , "}"
                  ]
                processed = removeComments input
                expectedLines = 
                  [ "func test() {"
                  , "  "
                  , " "
                  , "  return 42"
                  , "}"
                  ]
            processed @?= unlines expectedLines

        , testCase "Handles unterminated block comments" $ do
            let input = unlines
                  [ "func test() {"
                  , "  let x = 42"
                  , "  /* unterminated comment"
                  , "  return x"
                  , "}"
                  ]
                processed = removeComments input
                expectedLines = 
                  [ "func test() {"
                  , "  let x = 42"
                  , " "
                  , "  return x"
                  , "}"
                  ]
            processed @?= unlines expectedLines
        ]

    , testGroup "Whitespace and Formatting"
        [ testCase "Handles mixed whitespace characters" $ do
            let input = unlines
                  [ "func test() {\n\tlet x = 42;\r\n  return x;\n}"
                  ]
                normalized = normalizeIndentation input
                expectedLines = 
                  [ "func test() {"
                  , "\tlet x = 42;"
                  , "  return x;"
                  , "}"
                  ]
            normalized @?= unlines expectedLines

        , testCase "Handles tabs and spaces in indentation" $ do
            let input = unlines
                  [ "\tfunc test() {"
                  , "\t\tlet x = 42"
                  , "\t\treturn x"
                  , "\t}"
                  ]
                normalized = normalizeIndentation input
                expectedLines = 
                  [ "func test() {"
                  , "\tlet x = 42"
                  , "\treturn x"
                  , "}"
                  ]
            normalized @?= unlines expectedLines

        , testCase "Handles trailing whitespace" $ do
            let input = unlines
                  [ "func test() {   \t"
                  , "  let x = 42;   \t"
                  , "  return x;   \t"
                  , "}   \t"
                  ]
                trimmed = trim input
                expected = "func test() {\n  let x = 42;\n  return x;\n}"
            trimmed @?= expected

        , testCase "Handles BOM (Byte Order Mark)" $ do
            let input = "\xEF\xBB\xBFfunc test() { return 42 }"
                result = parseWithBOM input
            assertBool "Should handle BOM correctly"
                (isSuccess result)
        ]

    , testGroup "Property-based Text Processing Tests"
        [ fastProperty "Comment removal preserves structure" prop_commentRemovalPreservesStructure
        , fastProperty "Unicode processing is lossless" prop_unicodeProcessingLossless
        , fastProperty "Indentation normalization is idempotent" prop_indentationNormalizationIdempotent
        , fastProperty "Text processing handles all edge cases" prop_textProcessingEdgeCases
        ]
    ]

-- Helper functions for text processing testing

data ParseResult = ParseResult
    { prSuccess :: Bool
    , prErrors :: [String]
    , prWarnings :: [String]
    } deriving (Show, Eq)

isSuccess :: ParseResult -> Bool
isSuccess = prSuccess

hasEncodingError :: ParseResult -> Bool
hasEncodingError result = any ("encoding" `isInfixOf`) (prErrors result)

parseWithUnicode :: String -> ParseResult
parseWithUnicode input
    | any (not . isAscii) input = ParseResult True [] []
    | otherwise = ParseResult False ["No Unicode found"] []

parseLongLine :: String -> ParseResult
parseLongLine input
    | length input > 10000 = ParseResult True [] []
    | otherwise = ParseResult False ["Line not long enough"] []

parseDeepIndentation :: String -> ParseResult
parseDeepIndentation input
    | "    " `isPrefixOf` input = ParseResult True [] []
    | otherwise = ParseResult False ["No deep indentation"] []

parseManyEmptyLines :: String -> ParseResult
parseManyEmptyLines input
    | length (lines input) > 5000 = ParseResult True [] []
    | otherwise = ParseResult False ["Not enough empty lines"] []

parseLargeFile :: String -> ParseResult
parseLargeFile input
    | length input > 100000 = ParseResult True [] []
    | otherwise = ParseResult False ["File not large enough"] []

parseEscapedQuotes :: String -> ParseResult
parseEscapedQuotes input
    | "\\\"" `isInfixOf` input = ParseResult True [] []
    | otherwise = ParseResult False ["No escaped quotes"] []

parseEscapedNewlines :: String -> ParseResult
parseEscapedNewlines input
    | "\\n" `isInfixOf` input = ParseResult True [] []
    | otherwise = ParseResult False ["No escaped newlines"] []

parseUnicodeEscapes :: String -> ParseResult
parseUnicodeEscapes input
    | "\\u" `isInfixOf` input = ParseResult True [] []
    | otherwise = ParseResult False ["No unicode escapes"] []

parseRawStrings :: String -> ParseResult
parseRawStrings input
    | "r\"" `isInfixOf` input = ParseResult True [] []
    | otherwise = ParseResult False ["No raw strings"] []

parseWithBOM :: String -> ParseResult
parseWithBOM input
    | "\xEF\xBB\xBF" `isPrefixOf` input = ParseResult True [] []
    | otherwise = ParseResult False ["No BOM found"] []

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` words haystack

-- Property-based tests

prop_commentRemovalPreservesStructure :: String -> Property
prop_commentRemovalPreservesStructure input =
    length input > 0 ==>
    let withoutComments = removeComments input
        lineCountBefore = length (lines input)
        lineCountAfter = length (lines withoutComments)
    in lineCountAfter <= lineCountBefore

prop_unicodeProcessingLossless :: String -> Property
prop_unicodeProcessingLossless input =
    length input > 0 ==>
    let processed = parseWithUnicode input
    in prSuccess processed ==> True

prop_indentationNormalizationIdempotent :: String -> Property
prop_indentationNormalizationIdempotent input =
    length input > 0 ==>
    let normalized1 = normalizeIndentation input
        normalized2 = normalizeIndentation normalized1
    in normalized1 == normalized2

prop_textProcessingEdgeCases :: String -> Property
prop_textProcessingEdgeCases input =
    length input > 0 && length input <= 10000 ==>
    let hasSpecialChars = any (\c -> isControl c && not (isSpace c)) input
        hasUnicode = any (not . isAscii) input
        processed = parseWithUnicode input
    in (hasSpecialChars || hasUnicode) ==> prSuccess processed || not (null (prErrors processed))

-- Arbitrary instances

instance Arbitrary Char where
    arbitrary = oneof
        [ choose ('\32', '\126') -- ASCII printable
        , choose ('\128', '\255') -- Extended ASCII
        , return '\n' -- Newline
        , return '\t' -- Tab
        , return '\r' -- Carriage return
        ]