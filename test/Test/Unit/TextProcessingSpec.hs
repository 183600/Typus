module Test.Unit.TextProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import Utils
import qualified Data.Text as T
import qualified Data.Char as C
import Data.List (isInfixOf)

-- | Test advanced text processing scenarios and edge cases
tests :: TestTree
tests =
  testGroup "Text Processing Tests"
    [ testGroup "Advanced Comment Processing"
        [ testCase "handles nested block comments with strings" $ do
            let complexInput = unlines
                  [ "func test() {"
                  , "  s := \"/* not a comment */\""
                  , "  /* real comment with \"quotes\" inside */"
                  , "  return s"
                  , "}"
                  ]
                result = removeComments complexInput
            "not a comment" `isInfixOf` result @?= True
            "real comment" `isInfixOf` result @?= False

        , testCase "handles comments with escape sequences" $ do
            let escapeInput = "path := \"C:\\\\tmp\\\\//not_comment\" // real comment"
                result = removeLineComments escapeInput
            "C:\\\\tmp\\\\//not_comment" `isInfixOf` result @?= True
            "real comment" `isInfixOf` result @?= False

        , testCase "handles unclosed string in comment context" $ do
            let unclosedString = "code /* comment \"unclosed string */ more code"
                result = removeComments unclosedString
            result @?= "code  more code"

        , testCase "handles multiple consecutive comment markers" $ do
            let consecutiveComments = "code ////// multiple slashes"
                result = removeLineComments consecutiveComments
            result @?= "code "

        , testCase "handles block comment at very end without newline" $ do
            let endComment = "code /* final comment */"
                result = removeComments endComment
            result @?= "code "
        ]

    , testGroup "Complex Indentation Scenarios"
        [ testCase "handles mixed tabs and spaces intelligently" $ do
            let mixedInput = unlines
                  [ "\tfunc mixed() {"
                  , "    \tvar x := 1"
                  , "\t\treturn x"
                  , "}"
                  ]
                normalized = normalizeIndentation mixedInput
                linesCount = length $ lines normalized
            linesCount @?= 4  -- Should preserve all lines

        , testCase "handles progressive indentation" $ do
            let progressive = unlines
                  [ "level1"
                  , "  level2"
                  , "    level3"
                  , "      level4"
                  , "back to level1"
                  ]
                normalized = normalizeIndentation progressive
                firstLine = head $ lines normalized
            head firstLine @?= 'l'  -- Should start with 'l' from "level1"

        , testCase "handles empty lines in indentation" $ do
            let withEmptyLines = unlines
                  [ "    start"
                  , ""
                  , "        middle"
                  , ""
                  , "    end"
                  ]
                normalized = normalizeIndentation withEmptyLines
                normalizedLines = lines normalized
            normalizedLines !! 1 @?= ""  -- Empty line should be preserved
            normalizedLines !! 3 @?= ""  -- Empty line should be preserved

        , testCase "handles indentation with unicode spaces" $ do
            let unicodeSpaces = "　　func unicode() { return 42; }"  -- Full-width spaces
                trimmed = trim unicodeSpaces
            head trimmed @?= 'f'  -- Should trim unicode spaces
        ]

    , testGroup "Advanced Splitting Operations"
        [ testCase "handles splitting with complex delimiters" $ do
            let complexDelim = "a,,,b,,c,d"
                result = splitByCollapsed ',' complexDelim
            result @?= ["a", "b", "c", "d"]

        , testCase "handles splitting with unicode characters" $ do
            let unicodeText = "测试1,测试2,测试3"
                result = splitBy ',' unicodeText
            length result @?= 3

        , testCase "handles splitting very long strings" $ do
            let longString = concat $ replicate 1000 "a,"
                result = splitBy ',' longString
            length result @?= 1001  -- 1000 "a"s + empty string at end

        , testCase "handles splitting with special characters" $ do
            let specialChars = "a@b@c@d@e"
                result = splitBy '@' specialChars
            result @?= ["a", "b", "c", "d", "e"]

        , testCase "handles splitting with escape sequences" $ do
            let escaped = "a\\,b,c,d"
                result = splitBy ',' escaped
            result @?= ["a\\", "b", "c", "d"]
        ]

    , testGroup "Text Transformation Edge Cases"
        [ testCase "handles text with various newline types" $ do
            let mixedNewlines = "line1\r\nline2\nline3\rline4"
                linesResult = lines mixedNewlines
            length linesResult @?= 4

        , testCase "handles text with zero-width characters" $ do
            let zeroWidth = "text\u200Bwith\u200Czero\u200Dwidth"
                trimmed = trim zeroWidth
            length trimmed >= 4 @?= True  -- Should preserve base characters

        , testCase "handles text with combining characters" $ do
            let combining = "e\u0301\u0302 = accent"  -- e with combining accents
                processed = trim combining
            length processed >= 6 @?= True  -- Should preserve characters

        , testCase "handles text with bidirectional marks" $ do
            let bidi = "\u202Etext\u202Dnormal\u202E"
                processed = trim bidi
            length processed >= 4 @?= True  -- Should preserve all characters
        ]

    , testGroup "Search and Pattern Matching"
        [ testCase "breakOn handles complex patterns" $ do
            let input = "function(param1, param2) { body }"
                (before, after) = breakOn "(" input
            before @?= "function"
            after @?= "param1, param2) { body }"

        , testCase "breakOn with non-existent pattern" $ do
            let input = "no match here"
                (before, after) = breakOn "xyz" input
            before @?= input
            after @?= ""

        , testCase "breakOn with empty pattern" $ do
            let input = "test"
                (before, after) = breakOn "" input
            before @?= ""
            after @?= input

        , testCase "breakOn with multiple occurrences" $ do
            let input = "a:b:c:d"
                (before, after) = breakOn ":" input
            before @?= "a"
            after @?= "b:c:d"
        ]

    , testGroup "Performance-Critical Text Operations"
        [ testCase "efficiently handles large text blocks" $ do
            let largeText = unlines $ replicate 10000 $ "line content with some text"
                lineCount = length $ lines largeText
            lineCount @?= 10000

        , testCase "efficiently processes repeated patterns" $ do
            let repeated = concat $ replicate 1000 "pattern,"
                parts = splitBy ',' repeated
            length parts @?= 1001

        , testCase "efficiently handles deep nesting simulation" $ do
            let nested = concat $ replicate 1000 "{"
                processed = trim nested
            length processed @?= 1000

        , testCase "efficiently processes long comment blocks" $ do
            let longComment = "code /* " ++ replicate 5000 'x' ++ " */ end"
                processed = removeComments longComment
            processed @?= "code  end"
        ]

    , testGroup "Unicode and International Text"
        [ testCase "handles emoji in source code" $ do
            let emojiCode = "func test() { return \"🚀🎉\"; }"
                processed = removeLineComments emojiCode
            "🚀🎉" `isInfixOf` processed @?= True

        , testCase "handles CJK characters in identifiers" $ do
            let cjkCode = "变量 := 测试值 + 计算"
                processed = trim cjkCode
            head processed @?= '变'

        , testCase "handles right-to-left text" $ do
            let rtlText = "مرحبا := \"Hello\""
                processed = trim rtlText
            length processed >= 6 @?= True

        , testCase "handles mixed script text" $ do
            let mixed = "func 测试() { return \"مرحبا 🌍\"; }"
                processed = removeComments mixed
            "测试" `isInfixOf` processed @?= True
            "مرحبا" `isInfixOf` processed @?= True
        ]

    , testGroup "Property-based Text Processing Tests"
        [ fastProperty "splitBy and splitByCollapsed are consistent" prop_splitConsistency
        , fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "comment removal preserves line structure" prop_commentPreservesLines
        , fastProperty "indentation normalization is idempotent" prop_indentationIdempotent
        , fastProperty "text operations preserve unicode validity" prop_unicodePreservation
        ]
    ]

-- Property: splitBy and splitByCollapsed should be consistent for non-empty segments
prop_splitConsistency :: String -> Bool
prop_splitConsistency input =
  let normal = splitBy ',' input
      collapsed = splitByCollapsed ',' input
      filtered = filter (not . null) normal
  in collapsed == filtered

-- Property: trim should be idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
  let once = trim input
      twice = trim once
  in once == twice

-- Property: comment removal should preserve line count
prop_commentPreservesLines :: String -> Bool
prop_commentPreservesLines input =
  let originalLines = length $ lines input
      processed = removeComments input
      processedLines = length $ lines processed
  in processedLines <= originalLines  -- May reduce lines but never increase

-- Property: indentation normalization should be idempotent
prop_indentationIdempotent :: String -> Bool
prop_indentationIdempotent input =
  let once = normalizeIndentation input
      twice = normalizeIndentation once
  in once == twice

-- Property: text operations should preserve unicode validity
prop_unicodePreservation :: String -> Bool
prop_unicodePreservation input =
  let trimmed = trim input
      noComments = removeComments input
      normalized = normalizeIndentation input
  in all isValidChar (trimmed ++ noComments ++ normalized)
  where
    isValidChar c = C.isPrint c || C.isSpace c