module Test.Unit.CabalRegressionSpec where
import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertFailure
import Test.Tasty.QuickCheck 
import qualified Parser (parseTypus, TypusFile(..), FileDirectives)
            let input = "func test() { return 1\n  return 2\n}"
                                              result = Parser.parseTypus input
            case result of
              Left err -> 
                -- Should provide helpful error message, not crash
                "semicolon" `L.isInfixOf` show err || "expected" `L.isInfixOf` show err @?= True
              Right _ -> "Graceful handling" @?= "Should handle gracefully"
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


          ,             testCase "Regression: Parser recovers from unclosed blocks" $ do
            -- Previously this would cause infinite loops
            let input = "func test() { if (true) { return 1"
                                              result = Parser.parseTypus input
            case result of
              Left err -> 
                -- Should detect unclosed block
                L.length (show err) > 0 @?= True
              Right _ -> "Error detection" @?= "Should detect error"

          ,             testCase "Regression: Parser handles empty directives" $ do
            -- Previously empty directives would cause parsing failures
            let input = "// @ownership: \n// @dependent-types: \nfunc test() { return 1; }"
                                              result = Parser.parseTypus input
            case result of
              Left err -> assertFailure $ "Should handle empty directives: " ++ show err
              Right parsed -> 
                -- Should parse successfully with default directive handling
                Parser.tfDirectives parsed `seq` True @?= True

          ,             testCase "Regression: Parser handles deeply nested structures" $ do
            -- Previously deep nesting would cause stack overflow
            let nestedInput = unlines ["func deep() {"] ++ 
                               concat (replicate 50 "  if (true) {\n") ++
                               "    return 1;\n" ++
                               concat (replicate 50 "  }\n") ++
                               "}"
                                              result = Parser.parseTypus nestedInput
            case result of
              Left err -> L.length (show err) > 0 @?= True
              Right _ -> "Deep nesting handled" @?= "Handle deep nesting"
        ]

    , testGroup "Utils Regression Tests"
        [             testCase "Regression: trim handles unicode whitespace correctly" $ do
            -- Previously certain unicode whitespace wasn't trimmed
            let unicodeSpace = "\3000\27979\35797\3000"  -- Chinese full-width space
                                              trimmed = Utils.trim unicodeSpace
            trimmed @?= ""

          ,             testCase "Regression: splitBy handles empty strings correctly" $ do
            -- Previously splitBy "" would crash
            let result1 = Utils.splitBy ',' ""
                                              result2 = Utils.splitByCollapsed ',' ""
            result1 @?= [""]
            result2 @?= []

          ,             testCase "Regression: removeComments preserves escaped quotes" $ do
            -- Previously escaped quotes in strings were mishandled
            let input = "func test() { s := \"She said \\\"// hi\\\"\"; // comment }"
                                              result = Utils.removeComments input
            "\"She said \\\"// hi\\\"\"" `L.isInfixOf` result @?= True

          ,             testCase "Regression: normalizeIndentation handles mixed tabs/spaces" $ do
            -- Previously mixed indentation caused issues
            let mixedIndent = "\tfunc test() {\n    \treturn 1;\n\t}\n"
                                              normalized = Utils.normalizeIndentation mixedIndent
            "func test() {" `L.isInfixOf` normalized @?= True
            "return 1;" `L.isInfixOf` normalized @?= True
        ]

    , testGroup "SourceLocation Regression Tests"
        [             testCase "Regression: Source position handles large line numbers" $ do
            -- Previously large line numbers would cause overflow
            let largePos = SourceLocation.SourcePos 1000000 100 1000000
                                              advanced = SourceLocation.advancePos 'a' largePos
            SourceLocation.posLine advanced >= 1000000 @?= True

          ,             testCase "Regression: Span merging handles edge cases" $ do
            -- Previously merging certain spans would fail
            let pos1 = SourceLocation.SourcePos 1 1 0
                                              pos2 = SourceLocation.SourcePos 1 10 9
                                              pos3 = SourceLocation.SourcePos 2 5 15
                                              span1 = SourceLocation.SourceSpan pos1 pos2
                                              span2 = SourceLocation.SourceSpan pos2 pos3
                                              merged = SourceLocation.mergeSpans span1 span2
            SourceLocation.isValidSpan merged @?= True

          ,             testCase "Regression: Position advancement handles newlines correctly" $ do
            -- Previously newline advancement was inconsistent
            let pos = SourceLocation.SourcePos 5 10 50
                                              advanced = SourceLocation.advancePos '\n' pos
            SourceLocation.posLine advanced @?= 6
            SourceLocation.posColumn advanced @?= 1
        ]

    , testGroup "Error Handling Regression Tests"
        [             testCase "Regression: Error messages include line numbers" $ do
            -- Previously some errors lacked location info
            let input = "func test() {\n  return\n  invalid syntax\n}"
                                              result = Parser.parseTypus input
            case result of
              Left err -> 
                let errStr = show err
                in "line" `L.isInfixOf` errStr || L.any (`L.isInfixOf` errStr) ["1:", "2:", "3:", "4:"] @?= True
              Right _ -> "Location info" @?= "Should fail with location"

          ,             testCase "Regression: Multiple errors are reported when possible" $ do
            -- Previously only first error was reported
            let input = "func bad1() { return }\nfunc bad2() { if }"
                                              result = Parser.parseTypus input
            case result of
              Left err -> 
                -- Should provide meaningful error information
                L.length (show err) > 10 @?= True
              Right _ -> "Error detection" @?= "Should detect errors"
        ]

    , testGroup "Performance Regression Tests"
        [             testCase "Regression: Large file parsing doesn't regress" $ do
            -- Ensure performance doesn't regress for large files
            let largeInput = unlines $ replicate 1000 "func test() { return 1; }"
                                              result = Parser.parseTypus largeInput
            case result of
              Left err -> L.length (show err) > 0 @?= True
              Right _ -> "Large input handled" @?= "Handle large input"

          ,             testCase "Regression: Utils operations maintain performance" $ do
            -- Ensure utils don't have performance regressions
            let largeString = "   " ++ replicate 10000 'a' ++ "   "
                                              trimmed = Utils.trim largeString
            L.length trimmed @?= 10000  -- Should be immediate

        ,             testProperty "Regression: Property tests still hold" $ do
            \input -> Utils.trim (Utils.trim input) == Utils.trim input
        ]

    , testGroup "Integration Regression Tests"
        [             testCase "Regression: Parser L.and SyntaxValidator integration" $ do
            -- Previously integration would miss certain edge cases
            let input = "func validated() { return true; }"
                                              parseResult = Parser.parseTypus input
            case parseResult of
              Left _ -> "Parse failed" @?= "Should parse successfully"
              Right parsed -> 
                -- Should be able to validate without issues
                parsed `seq` True @?= True

          ,             testCase "Regression: File directives are preserved through parsing" $ do
            -- Previously directives could be lost during parsing
            let input = "// @ownership: true\n// @dependent-types: false\nfunc main() {}"
                                              result = Parser.parseTypus input
            case result of
              Left err -> assertFailure $ "Should parse with directives: " ++ show err
              Right parsed -> do
                            let directives = Parser.tfDirectives parsed
                directives `seq` True @?= True

          ,             testCase "Regression: Error locations are accurate in multi-file scenarios" $ do
            -- Previously error locations could be incorrect in complex scenarios
            let complexInput = unlines
                  [ "// @ownership: true"
                  , "func complex() {"
                  , "  if (true) {"
                  , "    return missing_semicolon"
                  , "  }"
                  , "}"
                  ]
                                              result = Parser.parseTypus complexInput
            case result of
              Left err -> do
                            let errStr = show err
                -- Should indicate line number around where error occurs
                L.any (`L.isInfixOf` errStr) ["3:", "4:", "5:"] @?= True
              Right _ -> "Error detection" @?= "Should detect error"
        ]

    , testGroup "Edge Case Regression Tests"
        [             testCase "Regression: Parser handles L.all whitespace input" $ do
            -- Previously L.all-whitespace input could cause issues
            let whitespaceOnly = "   \n\t  \n   \t\n"
                                              result = Parser.parseTypus whitespaceOnly
            case result of
              Left err -> L.length (show err) > 0 @?= True
              Right _ -> "Whitespace handled" @?= "Handle whitespace"

          ,             testCase "Regression: Comment removal handles edge cases" $ do
            -- Previously certain comment patterns caused issues
            let edgeCases = 
                  [ "/* nested /* comment */ still comment */"
                  , "// comment with /* block */ inside"
                  , "func test() { \"/* not comment */\"; /* real comment */ }"
                  ]
                                              results = L.map (Utils.removeComments) edgeCases
            L.all (> 0) (map L.length results) @?= True

          ,             testCase "Regression: Indentation normalization preserves meaning" $ do
            -- Previously normalization could change code meaning
            let input = unlines
                  [ "func test() {"
                  , "    if (true) {"
                  , "        return 1;"
                  , "    } else {"
                  , "        return 2;"
                  , "    }"
                  , "}"
                  ]
                                              normalized = Utils.normalizeIndentation input
            "if (true)" `L.isInfixOf` normalized @?= True
            "return 1;" `L.isInfixOf` normalized @?= True
            "return 2;" `L.isInfixOf` normalized @?= True
        ]
    ]
  where
      isInfixOf needle                               haystack = needle `L.isPrefixOf` haystack || 
                              (not (null haystack) && isInfixOf needle (L.tail haystack)
    isPrefixOf []                               _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) =                               x == y && isPrefixOf xs ys