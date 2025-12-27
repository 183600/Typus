module Test.Unit.CabalUnicodeHandlingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import qualified Utils (trim, splitBy, removeComments, normalizeIndentation)
import qualified SourceLocation (SourcePos(..), SourceSpan(..), advancePos)
import qualified Parser

-- | Unicode and internationalization tests
tests :: TestTree
tests =
  testGroup "Cabal Unicode Handling Tests"
    [ testGroup "Basic Unicode Support"
        [ testCase "Chinese characters in identifiers" $ do
            let chineseCode = "func 测试函数() { return 结果; }"
                result = Parser.parseTypus "chinese" chineseCode
            case result of
              Left err -> @?= "Should handle Chinese" (show err)
              Right _ -> @?= "Success" "Chinese success"

        , testCase "Emoji in comments" $ do
            let emojiComment = "// This is a test 🚀\nfunc test() { return 1; } // Done! ✅"
                result = Parser.parseTypus "emoji" emojiComment
            case result of
              Left err -> @?= "Should handle emoji" (show err)
              Right _ -> @?= "Success" "Emoji success"

        , testCase "Unicode in string literals" $ do
            let unicodeString = "func test() { s := \"Hello 世界 🌍\"; return s; }"
                result = Parser.parseTypus "unicode" unicodeString
            case result of
              Left err -> @?= "Should handle unicode strings" (show err)
              Right _ -> @?= "Success" "Unicode string success"

        , testCase "Mixed language identifiers" $ do
            let mixedCode = "func 测试_test() { let 变量 = hello; return 变量; }"
                result = Parser.parseTypus "mixed" mixedCode
            case result of
              Left err -> @?= "Should handle mixed languages" (show err)
              Right _ -> @?= "Success" "Mixed language success"
        ]

    , testGroup "Utils Unicode Processing"
        [ testCase "trim handles unicode whitespace" $ do
            let unicodeSpace = "\u3000测试\u3000"  -- Chinese full-width space
                trimmed = Utils.trim unicodeSpace
            trimmed @?= "测试"

        , testCase "splitBy handles unicode delimiters" $ do
            let unicodeDelim = "测试，代码，示例"
                parts = Utils.splitBy '，' unicodeDelim
            parts @?= ["测试", "代码", "示例"]

        , testCase "removeComments preserves unicode content" $ do
            let unicodeWithComments = "// 测试注释\nfunc 测试() { return \"世界\"; }"
                result = Utils.removeComments unicodeWithComments
            "func 测试() { return \"世界\"; }" `isInfixOf` result @?= True

        , testCase "normalizeIndentation with unicode content" $ do
            let unicodeIndented = "    \n    func 测试() {\n        return 结果;\n    }\n"
                normalized = Utils.normalizeIndentation unicodeIndented
            "func 测试() {" `isInfixOf` normalized @?= True
        ]

    , testGroup "Source Location with Unicode"
        [ testCase "Unicode character counting in positions" $ do
            let pos = SourceLocation.SourcePos 1 1
                advanced = SourceLocation.advancePos '测' pos
            SourceLocation.sourceLine advanced @?= 1
            SourceLocation.sourceColumn advanced @?= 2

        , testCase "Multi-byte unicode advancement" $ do
            let pos = SourceLocation.SourcePos 1 1
                emoji = '🚀'
                advanced = SourceLocation.advancePos emoji pos
            -- Should count as one character position regardless of byte length
            SourceLocation.sourceColumn advanced @?= 2

        , testCase "Unicode in error messages" $ do
            let unicodeError = "func 测试() { return }"
                result = Parser.parseTypus "unicode-error" unicodeError
            case result of
              Left err -> do
                let errStr = show err
                length errStr > 0 @?= True
              Right _ -> @?= "Should fail appropriately" "Unicode error handling"
        ]

    , testGroup "Parser Unicode Edge Cases"
        [ testCase "Right-to-left scripts" $ do
            let rtlCode = "func دالة() { return نتيجة; }"  -- Arabic
                result = Parser.parseTypus "rtl" rtlCode
            case result of
              Left err -> @?= "Should handle RTL" (show err)
              Right _ -> @?= "Success" "RTL success"

        , testCase "Combining characters" $ do
            let combining = "func tést() { return résultat; }"  -- With combining accents
                result = Parser.parseTypus "combining" combining
            case result of
              Left err -> @?= "Should handle combining" (show err)
              Right _ -> @?= "Success" "Combining success"

        , testCase "Zero-width characters" $ do
            let zeroWidth = "func\u200Btest() { return\u200C1; }"  -- Zero-width space and non-joiner
                result = Parser.parseTypus "zerowidth" zeroWidth
            case result of
              Left err -> @?= "Should handle zero-width" (show err)
              Right _ -> @?= "Success" "Zero-width success"

        , testCase "Mixed unicode and ASCII" $ do
            let mixed = "func test函数() { let 变量 = 42; return 变量; }"
                result = Parser.parseTypus "mixedunicode" mixed
            case result of
              Left err -> @?= "Should handle mixed unicode" (show err)
              Right _ -> @?= "Success" "Mixed unicode success"
        ]

    , testGroup "Internationalization Features"
        [ testCase "Unicode in directives" $ do
            let unicodeDirectives = "// @所有权: true\n// @依赖类型: false\nfunc 测试() {}"
                result = Parser.parseTypus "unicode-directives" unicodeDirectives
            case result of
              Left err -> @?= "Should handle unicode directives" (show err)
              Right _ -> @?= "Success" "Unicode directives success"

        , testCase "Unicode identifiers with numbers" $ do
            let unicodeWithNumbers = "func 变量1() { let 测试2 = 42; return 测试2; }"
                result = Parser.parseTypus "unicode-numbers" unicodeWithNumbers
            case result of
              Left err -> @?= "Should handle unicode with numbers" (show err)
              Right _ -> @?= "Success" "Unicode numbers success"

        , testCase "Unicode in block comments" $ do
            let unicodeBlockComment = "/* 这是多行\n   注释测试 */\nfunc test() { return 1; }"
                result = Utils.removeComments unicodeBlockComment
            "func test() { return 1; }" `isInfixOf` result @?= True

        , testCase "Unicode string escaping" $ do
            let unicodeEscape = "func test() { s := \"Hello \\u4e16\\u754c\"; return s; }"
                result = Parser.parseTypus "unicode-escape" unicodeEscape
            case result of
              Left err -> @?= "Should handle unicode escapes" (show err)
              Right _ -> @?= "Success" "Unicode escape success"
        ]

    , testGroup "Property-based Unicode Tests"
        [ testProperty "Unicode strings round-trip through trim" $ do
            \unicodeStr -> Utils.trim (Utils.trim unicodeStr) == Utils.trim unicodeStr

        , testProperty "Unicode splitBy preserves content" $ do
            \unicodeStr delim -> 
                let parts = Utils.splitBy delim unicodeStr
                    rejoined = concat (intersperse [delim] parts)
                in length rejoined >= length unicodeStr - length (filter (== delim) unicodeStr)

        , testProperty "Unicode comment removal preserves strings" $ do
            \unicodeContent -> 
                let withComments = "func test() { s := \"" ++ unicodeContent ++ "\"; // 注释 }\n"
                    withoutComments = Utils.removeComments withComments
                in "\"" ++ unicodeContent ++ "\"" `isInfixOf` withoutComments

        , testProperty "Unicode normalization preserves line count" $ do
            \unicodeInput -> 
                let normalized = Utils.normalizeIndentation unicodeInput
                    inputLines = length (lines unicodeInput)
                    normLines = length (lines normalized)
                in inputLines == normLines
        ]

    , testGroup "Unicode Performance"
        [ testCase "Large unicode text processing" $ do
            let largeUnicode = unlines $ replicate 100 "测试函数" ++ show [1..100] ++ "{ 返回 " ++ show [1..100] ++ "; }"
                result = Parser.parseTypus "large-unicode" largeUnicode
            case result of
              Left _ -> @?= "Handle large unicode" "Large unicode handling"
              Right _ -> @?= "Success" "Large unicode success"

        , testCase "Complex unicode processing" $ do
            let complexUnicode = unlines 
                  [ "// 这是一行包含各种unicode的注释：🚀🌍✅"
                  , "func 复杂测试() {"
                  , "  let 变量1 = \"Hello 世界 🌍\";"
                  , "  let 变量2 = \"测试 العربية\";"
                  , "  return 变量1 + 变量2;"
                  , "}"
                  ]
                result = Parser.parseTypus "complex-unicode" complexUnicode
            case result of
              Left err -> @?= "Should handle complex unicode" (show err)
              Right _ -> @?= "Success" "Complex unicode success"
        ]
    ]
  where
    isInfixOf needle haystack = needle `elem` (substrings haystack)
    substrings [] = []
    substrings s@(x:xs) = takeWhile (const True) s : substrings xs
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x : sep : intersperse sep (y:xs)