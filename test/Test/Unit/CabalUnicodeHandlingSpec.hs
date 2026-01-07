module Test.Unit.CabalUnicodeHandlingSpec where
import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck 
import qualified SourceLocation (SourcePos(..), SourceSpan(..), advancePos)
import qualified Parser
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | Unicode L.and internationalization tests
tests :: TestTree
tests =
    testGroup "Cabal Unicode Handling Tests"
    [ testGroup "Basic Unicode Support"
        [             testCase "Chinese characters in identifiers" $ do
                        let chineseCode = "func () { return ; }"
                                              result = Parser.parseTypus chineseCode
            case result of
              Left err -> @?= "Should handle Chinese" (show err)
              Right _ -> @?= "Success" "Chinese success"

          ,             testCase "Emoji in comments" $ do
                        let emojiComment = "// This is a test \nfunc test() { return 1; } // Done! "
                                              result = Parser.parseTypus emojiComment
            case result of
              Left err -> @?= "Should handle emoji" (show err)
              Right _ -> @?= "Success" "Emoji success"

          ,             testCase "Unicode in string literals" $ do
                        let unicodeString = "func test() { s := \"Hello  \"; return s; }"
                                              result = Parser.parseTypus unicodeString
            case result of
              Left err -> @?= "Should handle unicode strings" (show err)
              Right _ -> @?= "Success" "Unicode string success"

          ,             testCase "Mixed language identifiers" $ do
                        let mixedCode = "func _test() { let  = hello; return ; }"
                                              result = Parser.parseTypus mixedCode
            case result of
              Left err -> @?= "Should handle mixed languages" (show err)
              Right _ -> @?= "Success" "Mixed language success"
        ]

    , testGroup "Utils Unicode Processing"
        [             testCase "trim handles unicode whitespace" $ do
                        let unicodeSpace = "\u3000\u3000"  -- Chinese full-width space
                                              trimmed = Utils.trim unicodeSpace
            trimmed @?= ""

          ,             testCase "splitBy handles unicode delimiters" $ do
                        let unicodeDelim = ""
                                              parts = Utils.splitBy '' unicodeDelim
            parts @?= ["", "", ""]

          ,             testCase "removeComments preserves unicode content" $ do
                        let unicodeWithComments = "// \nfunc () { return \"\"; }"
                                              result = Utils.removeComments unicodeWithComments
            "func () { return \"\"; }" `L.isInfixOf` result @?= True

          ,             testCase "normalizeIndentation with unicode content" $ do
                        let unicodeIndented = "    \n    func () {\n        return ;\n    }\n"
                                              normalized = Utils.normalizeIndentation unicodeIndented
            "func () {" `L.isInfixOf` normalized @?= True
        ]

    , testGroup "Source Location with Unicode"
        [             testCase "Unicode character counting in positions" $ do
                        let pos = SourceLocation.SourcePos 1 1
                                              advanced = SourceLocation.advancePos '' pos
            SourceLocation.sourceLine advanced @?= 1
            SourceLocation.sourceColumn advanced @?= 2

          ,             testCase "Multi-byte unicode advancement" $ do
                        let pos = SourceLocation.SourcePos 1 1
                                              emoji = ''
                                              advanced = SourceLocation.advancePos emoji pos
            -- Should count as one character position regardless of byte L.length
            SourceLocation.sourceColumn advanced @?= 2

          ,             testCase "Unicode in error messages" $ do
                        let unicodeError = "func () { return }"
                                              result = Parser.parseTypus unicodeError
            case result of
              Left err -> do
                            let errStr = show err
                L.length errStr > 0 @?= True
              Right _ -> @?= "Should fail appropriately" "Unicode error handling"
        ]

    , testGroup "Parser Unicode Edge Cases"
        [             testCase "Right-to-left scripts" $ do
                        let rtlCode = "func () { return ; }"  -- Arabic
                                              result = Parser.parseTypus rtlCode
            case result of
              Left err -> @?= "Should handle RTL" (show err)
              Right _ -> @?= "Success" "RTL success"

          ,             testCase "Combining characters" $ do
                        let combining = "func test() { return resultat; }"  -- With combining accents
                                              result = Parser.parseTypus combining
            case result of
              Left err -> @?= "Should handle combining" (show err)
              Right _ -> @?= "Success" "Combining success"

          ,             testCase "Zero-width characters" $ do
                        let zeroWidth = "func\u200Btest() { return\u200C1; }"  -- Zero-width space L.and non-joiner
                                              result = Parser.parseTypus zeroWidth
            case result of
              Left err -> @?= "Should handle zero-width" (show err)
              Right _ -> @?= "Success" "Zero-width success"

          ,             testCase "Mixed unicode L.and ASCII" $ do
                        let mixed = "func test() { let  = 42; return ; }"
                                              result = Parser.parseTypus mixed
            case result of
              Left err -> @?= "Should handle mixed unicode" (show err)
              Right _ -> @?= "Success" "Mixed unicode success"
        ]

    , testGroup "Internationalization Features"
        [             testCase "Unicode in directives" $ do
                        let unicodeDirectives = "// @: true\n// @: false\nfunc () {}"
                                              result = Parser.parseTypus unicodeDirectives
            case result of
              Left err -> @?= "Should handle unicode directives" (show err)
              Right _ -> @?= "Success" "Unicode directives success"

          ,             testCase "Unicode identifiers with numbers" $ do
                        let unicodeWithNumbers = "func 1() { let 2 = 42; return 2; }"
                                              result = Parser.parseTypus unicodeWithNumbers
            case result of
              Left err -> @?= "Should handle unicode with numbers" (show err)
              Right _ -> @?= "Success" "Unicode numbers success"

          ,             testCase "Unicode in block comments" $ do
                        let unicodeBlockComment = "/* \n    */\nfunc test() { return 1; }"
                                              result = Utils.removeComments unicodeBlockComment
            "func test() { return 1; }" `L.isInfixOf` result @?= True

          ,             testCase "Unicode string escaping" $ do
                        let unicodeEscape = "func test() { s := \"Hello \\u4e16\\u754c\"; return s; }"
                                              result = Parser.parseTypus unicodeEscape
            case result of
              Left err -> @?= "Should handle unicode escapes" (show err)
              Right _ -> @?= "Success" "Unicode escape success"
        ]

    , testGroup "Property-based Unicode Tests"
        [             testProperty "Unicode strings round-trip through trim" $ do
            \unicodeStr -> Utils.trim (Utils.trim unicodeStr) == Utils.trim unicodeStr

        ,             testProperty "Unicode splitBy preserves content" $ do
            \unicodeStr delim -> 
                let parts = Utils.splitBy delim unicodeStr
                                                  rejoined = L.concat (intersperse [delim] parts)
                in L.length rejoined >= L.length unicodeStr - L.length (L.filter (== delim) unicodeStr)

        ,             testProperty "Unicode comment removal preserves strings" $ do
            \unicodeContent -> 
                let withComments = "func test() { s := \"" ++ unicodeContent ++ "\"; //  }\n"
                                                  withoutComments = Utils.removeComments withComments
                in "\"" ++ unicodeContent ++ "\"" `L.isInfixOf` withoutComments

        ,             testProperty "Unicode normalization preserves line count" $ do
            \unicodeInput -> 
                let normalized = Utils.normalizeIndentation unicodeInput
                                                  inputLines = L.length (lines unicodeInput)
                                                  normLines = L.length (lines normalized)
                in                               inputLines == normLines
        ]

    , testGroup "Unicode Performance"
        [             testCase "Large unicode text processing" $ do
                        let largeUnicode = unlines $ replicate 100 "" ++ show [1..100] ++ "{  " ++ show [1..100] ++ "; }"
                                              result = Parser.parseTypus largeUnicode
            case result of
              Left _ -> @?= "Handle large unicode" "Large unicode handling"
              Right _ -> @?= "Success" "Large unicode success"

          ,             testCase "Complex unicode processing" $ do
                        let complexUnicode = unlines 
                  [ "// unicode"
                  , "func () {"
                  , "  let 1 = \"Hello  \";"
                  , "  let 2 = \" \";"
                  , "  return 1 + 2;"
                  , "}"
                  ]
                                              result = Parser.parseTypus complexUnicode
            case result of
              Left err -> @?= "Should handle complex unicode" (show err)
              Right _ -> @?= "Success" "Complex unicode success"
        ]
    ]
  where
      isInfixOf needle                               haystack = needle `elem` (substrings haystack)
    substrings [] = []
    substrings s@(x:xs) = takeWhile (const True) s : substrings xs
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x : sep : intersperse sep (y:xs)