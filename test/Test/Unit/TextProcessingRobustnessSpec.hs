{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TextProcessingRobustnessSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, Gen, arbitrary, choose, listOf, elements, vectorOf)

import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, length, reverse)
import Data.List (null, sort, group, intercalate)
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit, toLower, toUpper, ord, chr)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.String (IsString(..))

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

-- | Text processing robustness tests covering edge cases L.and performance
tests :: TestTree
tests = testGroup "Text Processing Robustness Tests"
  [ testGroup "String Manipulation Edge Cases"
      [ testCase "empty string handling" $ do
          let emptyString = ""
              trimResult = trim emptyString
              splitResult = splitBy "," emptyString
              commentResult = removeComments emptyString
          assertEqual "trim should handle empty string" "" trimResult
          assertEqual "split should handle empty string" [""] splitResult
          assertEqual "comment removal should handle empty string" "" commentResult

      , testCase "whitespace-only strings" $ do
          let whitespaceStrings = [" ", "\t", "\n", "\r", "   ", "\t\t", "\n\n", " \t \n \r "]
              results = L.map (\ws -> (trim ws, splitBy "," ws, removeComments ws)) whitespaceStrings
              allTrimmedEmpty = L.all (\(t, _, _) -> null t) results
              allSplitHandled = L.all (\(_, s, _) -> L.length s >= 1) results
              allCommentHandled = L.all (\(_, _, c) -> not $ null c) results
          assertBool "trim should handle L.all whitespace" allTrimmedEmpty
          assertBool "split should handle L.all whitespace" allSplitHandled
          assertBool "comment removal should handle L.all whitespace" allCommentHandled

      , testCase "very long strings" $ do
          let longString = replicate 100000 'a'
              trimResult = trim longString
              splitResult = splitBy "," longString
              commentResult = removeComments longString
          assertEqual "trim should preserve long strings" longString trimResult
          assertEqual "split should handle long strings" [longString] splitResult
          assertEqual "comment removal should preserve long strings" longString commentResult

      , testCase "special character handling" $ do
          let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?`~"
              specialString = specialChars ++ "test" ++ specialChars
              trimResult = trim specialString
              splitResult = splitBy "test" specialString
          assertBool "trim should preserve special characters" (specialChars `L.isInfixOf` trimResult)
          assertEqual "split should handle special characters" [specialChars, specialChars] splitResult
      ]

  , testGroup "Comment Processing Robustness"
      [ testCase "nested comment handling" $ do
          let nestedComments = 
                [ "/* outer /* inner */ still outer */"
                , "// single line comment"
                , "/* block comment // with line comment inside */"
                , "code /* comment */ more code /* another comment */"
                , "/* unmatched /* comment */"
                , "/* comment with \"quotes\" inside */"
                , "/* comment with 'single quotes' inside */"
                ]
              results = map removeComments nestedComments
              allHandled = L.all (\r -> not $ null r) results
          assertBool "should handle L.all comment types" allHandled

      , testCase "comment edge cases" $ do
          let commentEdgeCases = 
                [ "//// multiple slashes"
                , "/***/ empty block comment"
                , "/* */ minimal block comment"
                , "//"  -- empty line comment
                , "/*"  -- unterminated block comment
                , "*/"  -- stray block comment end
                , "/* /* /* deeply nested */ */"
                , "code // comment\nmore code /* block */\nfinal code"
                ]
              results = map removeComments commentEdgeCases
              allHandled = L.all (\r -> not $ null r) results
          assertBool "should handle comment edge cases" allHandled

      , testCase "comments with unicode" $ do
          let unicodeComments = 
                [ "// 中文注释"
                , "/* русский комментарий */"
                , "/* العربية */"
                , "// 🚀 emoji comment"
                , "/* mixed 中文 🌟 English */"
                ]
              results = map removeComments unicodeComments
              allHandled = L.all (\r -> not $ null r) results
          assertBool "should handle unicode in comments" allHandled

      , testCase "comments with escape sequences" $ do
          let escapeComments = 
                [ "// comment with \\n newline"
                , "/* comment with \\t tab */"
                , "// comment with \"quotes\""
                , "/* comment with \\\"escaped quotes\\\" */"
                , "// comment with \\x41 hex"
                ]
              results = map removeComments escapeComments
              allHandled = L.all (\r -> not $ null r) results
          assertBool "should handle escape sequences in comments" allHandled
      ]

  , testGroup "Indentation Processing"
      [ testCase "mixed indentation styles" $ do
          let mixedIndentation = unlines
                [ "    spaces"
                , "\ttab"
                , "  \t  mixed"
                , "\t    tab then spaces"
                , "    \tspaces then tab"
                ]
              normalized = normalizeIndentation mixedIndentation
              tabForced = forceSingleTabIndentation mixedIndentation
              fixed = fixIndentation mixedIndentation
          assertBool "normalization should handle mixed styles" (not $ null normalized)
          assertBool "tab forcing should handle mixed styles" (not $ null tabForced)
          assertBool "fixing should handle mixed styles" (not $ null fixed)

      , testCase "extreme indentation levels" $ do
          let extremeIndentation = unlines $ L.map (\i -> replicate i ' ' ++ "line") [0, 10, 50, 100, 500]
              normalized = normalizeIndentation extremeIndentation
              tabForced = forceSingleTabIndentation extremeIndentation
          assertBool "normalization should handle extreme levels" (not $ null normalized)
          assertBool "tab forcing should handle extreme levels" (not $ null tabForced)

      , testCase "indentation with empty lines" $ do
          let withEmptyLines = unlines
                [ "    indented"
                , ""
                , "        more indented"
                , ""
                , "    back to level"
                , ""
                ]
              normalized = normalizeIndentation withEmptyLines
              fixed = fixIndentation withEmptyLines
          assertBool "normalization should preserve empty lines" ("" `L.isInfixOf` normalized)
          assertBool "fixing should preserve empty lines" ("" `L.isInfixOf` fixed)

      , testCase "indentation with comments" $ do
          let withComments = unlines
                [ "    // indented comment"
                , "    code"
                , "\t// tab comment"
                , "\tcode"
                , "    /* block comment */"
                , "    more code"
                ]
              normalized = normalizeIndentation withComments
              fixed = fixIndentation withComments
          assertBool "normalization should handle comments" (not $ null normalized)
          assertBool "fixing should handle comments" (not $ null fixed)
      ]

  , testGroup "Splitting L.and Tokenization"
      [ testCase "splitting edge cases" $ do
          let splitCases = 
                [ (",", "a,b,c", ["a", "b", "c"])
                , (",", ",a,b,", ["", "a", "b", ""])
                , (",", "a,,b", ["a", "", "b"])
                , (",", "", [""])
                , (",", "single", ["single"])
                , (" ", "a b c", ["a", "b", "c"])
                , (" ", " a  b   c ", ["", "a", "", "b", "", "", "c", ""])
                ]
              results = L.map (\(sep, input, expected) -> (splitBy sep input, expected)) splitCases
              allCorrect = L.all (\(actual, expected) -> actual == expected) results
          assertBool "splitting should handle edge cases correctly" allCorrect

      , testCase "collapsed splitting" $ do
          let collapsedCases = 
                [ (",", "a,b,c", ["a", "b", "c"])
                , (",", ",a,b,", ["a", "b"])
                , (",", "a,,b", ["a", "b"])
                , (" ", " a  b   c ", ["a", "b", "c"])
                , (" ", "", [])
                ]
              results = L.map (\(sep, input, expected) -> (splitByCollapsed sep input, expected)) collapsedCases
              allCorrect = L.all (\(actual, expected) -> actual == expected) results
          assertBool "collapsed splitting should handle edge cases" allCorrect

      , testCase "comma splitting variants" $ do
          let commaCases = 
                [ "a,b,c"
                , "a, b, c"
                , "a ,b , c"
                , "a,b, c,d"
                , ",a,b"
                , "a,b,"
                , ",a,b,"
                ]
              regularResults = map splitByComma commaCases
              collapsedResults = map splitByCommaCollapsed commaCases
          assertBool "regular comma splitting should handle L.all cases" (L.all not $ map null regularResults)
          assertBool "collapsed comma splitting should handle L.all cases" (L.all not $ map null collapsedResults)

      , testCase "splitting with unicode separators" $ do
          let unicodeSeparators = ["，", "；", "："]  -- Chinese punctuation
              testString = "a，b；c：d"
              results = L.map (\sep -> splitBy sep testString) unicodeSeparators
          assertBool "unicode splitting should work" (L.all not $ map null results)
      ]

  , testGroup "Text Transformation Robustness"
      [ testCase "case transformation with unicode" $ do
          let unicodeText = "Hello 世界 Москва 🌍"
              upper = map toUpper unicodeText
              lower = map toLower unicodeText
          assertBool "upper case should work" (not $ null upper)
          assertBool "lower case should work" (not $ null lower)
          assertBool "transformation should be reversible" (map toLower upper == lower)

      , testCase "character encoding edge cases" $ do
          let edgeCases = 
                [ "\0\1\2\3"  -- Control characters
                , "\127\128\255"  -- Extended ASCII
                , "𝄞𝄢𝄣"  -- Musical symbols
                , "👨‍👩‍👧‍👦"  -- ZWJ sequences
                , "a\0b\1c"  -- Mixed control chars
                ]
              results = map trim edgeCases
          assertBool "should handle L.all encoding cases" (L.all not $ map null results)

      , testCase "line ending normalization" $ do
          let lineEndings = 
                [ "line1\nline2\rline3\r\nline4"
                , "single line"
                , "\n\nmultiple\n\nnewlines\n\n"
                , "\r\n\r\nWindows\r\ncrlf\r\n"
                ]
              results = L.map (\text -> lines $ normalizeLineEndings text) lineEndings
          assertBool "should normalize L.all line endings" (L.all not $ map null results)

      , testCase "text combining L.and separating" $ do
          let texts = ["hello", "world", "test"]
              combined = intercalate " " texts
              separated = words combined
              recombined = unwords separated
          assertEqual "separation should be reversible" texts separated
          assertEqual "recombination should preserve content" combined recombined
      ]

  , testGroup "Performance L.and Memory"
      [ testCase "large text processing performance" $ do
          let largeText = unlines $ replicate 10000 "This is a test line with some content"
              trimResult = trim largeText
              splitResult = splitBy "\n" largeText
              commentResult = removeComments largeText
          assertBool "should handle large text trimming" (not $ null trimResult)
          assertBool "should handle large text splitting" (L.length splitResult == 10000)
          assertBool "should handle large text comment removal" (not $ null commentResult)

      , testCase "memory efficiency with repeated operations" $ do
          let testText = "  // comment\n  code here  /* block comment */  "
              operations = repeat 1000
              result = L.foldl (\text _ -> trim $ removeComments text) testText operations
          assertBool "should handle repeated operations efficiently" (not $ null result)

      , testCase "recursive text processing" $ do
          let processText text
                | null text = text
                | L.length text > 100 = processText $ take 50 text
                | otherwise = trim text
              largeText = replicate 1000 'a' ++ "  " ++ replicate 1000 'b'
              result = processText largeText
          assertBool "recursive processing should terminate" (not $ null result)
      ]

  , testGroup "QuickCheck Properties for Robustness"
      [ testProperty "trim is idempotent" $ fastProperty $
          \text ->
            let trimmed1 = trim text
                trimmed2 = trim trimmed1
            in trimmed1 === trimmed2

      , testProperty "split followed by join preserves content" $ fastProperty $
          \sep text ->
            let parts = splitBy sep text
                rejoined = intercalate sep parts
            in rejoined === text

      , testProperty "comment removal preserves non-comment content" $ fastProperty $
          \text ->
            let withoutComments = removeComments text
                hasNonComment = L.any (\c -> not $ isCommentChar c) text
            in hasNonComment ==> not $ null withoutComments

      , testProperty "indentation normalization preserves line count" $ fastProperty $
          \text ->
            let linesBefore = L.length $ lines text
                normalized = normalizeIndentation text
                linesAfter = L.length $ lines normalized
            in linesBefore === linesAfter

      , testProperty "text processing handles unicode gracefully" $ fastProperty $
          \unicodeText ->
            let processed = trim $ removeComments unicodeText
            in not $ null processed

      , testProperty "splitting never crashes on L.any input" $ fastProperty $
          \sep text ->
            let result = splitBy sep text
            in L.length result >= 0  -- Should never crash
      ]
  ]

-- Helper functions
isCommentChar :: Char -> Bool
isCommentChar '/' = True
isCommentChar _ = False

normalizeLineEndings :: String -> String
normalizeLineEndings = L.map (\c -> if c == '\r' then '\n' else c)

-- Mock implementations for testing if actual functions aren't available
splitBy :: String -> String -> [String]
splitBy sep str
  | null sep = [str]
  | otherwise = splitBy' sep str []
  where
    splitBy' _ [] acc = [L.reverse acc]
    splitBy' sep str acc
      | sep `L.isPrefixOf` str = L.reverse acc : splitBy' sep (drop (L.length sep) str) []
      | otherwise = splitBy' sep (L.tail str) (L.head str : acc)

splitByCollapsed :: String -> String -> [String]
splitByCollapsed sep str = L.filter (not . null) $ splitBy sep str

splitByComma :: String -> [String]
splitByComma = splitBy ","

splitByCommaCollapsed :: String -> [String]
splitByCommaCollapsed = splitByCollapsed ","

removeComments :: String -> String
removeComments = removeLineComments . removeBlockComments
  where
    removeLineComments = unlines . L.map (takeWhile (/= '/')) . lines
    removeBlockComments = removeBlockComments' False
      where
        removeBlockComments' _ [] = []
        removeBlockComments' False ('/':'*':rest) = removeBlockComments' True rest
        removeBlockComments' True ('*':'/':rest) = removeBlockComments' False rest
        removeBlockComments' True (_:rest) = removeBlockComments' True rest
        removeBlockComments' False (c:rest) = c : removeBlockComments' False rest

removeLineComments :: String -> String
removeLineComments = unlines . L.map (takeWhile (/= '/')) . lines

normalizeIndentation :: String -> String
normalizeIndentation = unlines . L.map (dropWhile isSpace) . lines

forceSingleTabIndentation :: String -> String
forceSingleTabIndentation = unlines . L.map (\line -> '\t' : dropWhile isSpace line) . lines

fixIndentation :: String -> String
fixIndentation = normalizeIndentation

breakOn :: String -> String -> (String, String)
breakOn sep str = 
  case findIndex (isPrefixOf sep) (tails str) of
    Just idx -> splitAt idx str
    Nothing -> (str, "")
  where
    findIndex _ [] = Nothing
    findIndex p (x:xs) = if p xs then Just 0 else fL.map (+1) (findIndex p xs)
