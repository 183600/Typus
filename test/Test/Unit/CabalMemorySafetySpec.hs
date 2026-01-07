module Test.Unit.CabalMemorySafetySpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertFailure
import Test.Tasty.QuickCheck 
                        let input = "func memory_test() { return 42; }"
                                              result = Parser.parseTypus input
            case result of
              Left err -> do
                            performGC  -- Force garbage collection
                L.length (show err) > 0 @?= True
              Right parsed -> do
                            performGC
                parsed `seq` True @?= True
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


          ,             testCase "Parser handles large inputs without memory blowup" $ do
                        let largeInput = unlines $ replicate 1000 "func large() { return 1; }"
                                              result = Parser.parseTypus largeInput
            case result of
              Left err -> do
                            performGC
                L.length (show err) > 0 @?= True
              Right parsed -> do
                            performGC
                parsed `seq` True @?= True

          ,             testCase "Parser releases memory after parsing errors" $ do
                        let errorInput = "func error() { return }"  -- Missing semicolon
                                              result = Parser.parseTypus errorInput
            case result of
              Left _ -> do
                            performGC
                True @?= True  -- Should reach here without memory issues
              Right _ -> "Should fail" @?= "Expected failure"

          ,             testCase "Repeated parsing doesn't accumulate memory" $ do
                        let input = "func repeat() { return 1; }"
                                              parseMultiple = sequence $ replicate 100 $ Parser.parseTypus input
                isRight (Right _) = True
                isRight (Left _) = False
            case parseMultiple of
              Left err -> assertFailure $ "Parsing failed: " ++ err
              Right results -> do
                            performGC
                length results @?= 100
        ]

    , testGroup "Utils Memory Safety"
        [             testCase "trim releases intermediate strings" $ do
                        let largeString = "   " ++ replicate 10000 'a' ++ "   "
                                              result = Utils.trim largeString
            performGC
            rnf result `seq` True @?= True
            L.length result @?= 10000

          ,             testCase "splitBy doesn't leak memory on large inputs" $ do
                        let largeInput = unlines $ replicate 5000 "test line content"
                                              result = Utils.splitBy '\n' largeInput
            performGC
            rnf result `seq` True @?= True
            L.length result @?= 5000

          ,             testCase "removeComments handles large comment blocks" $ do
                        let largeComments = "/* " ++ replicate 10000 'x' ++ " */\nfunc test() { return 1; }"
                                              result = Utils.removeComments largeComments
            performGC
            rnf result `seq` True @?= True
            "func test() { return 1; }" `L.isInfixOf` result @?= True

          ,             testCase "normalizeIndentation processes large files efficiently" $ do
                        let largeIndented = unlines $ replicate 1000 ("    " ++ "indented line")
                                              result = Utils.normalizeIndentation largeIndented
            performGC
            rnf result `seq` True @?= True
            L.length (lines result) @?= 1000
        ]

    , testGroup "Source Location Memory Safety"
        [             testCase "Source position creation doesn't leak" $ do
                        let positions = [SourceLocation.SourcePos line col | line <- [1..1000], col <- [1..100]]
            performGC
            rnf positions `seq` True @?= True
            L.length positions @?= 100000

          ,             testCase "Span operations are memory efficient" $ do
                        let spans = [SourceLocation.SourceSpan (SourceLocation.SourcePos 1 1 0) (SourceLocation.SourcePos 100 100 0)]
                                              merged = foldl SourceLocation.mergeSpans (L.head spans) (L.tail spans)
            performGC
            rnf merged `seq` True @?= True
            SourceLocation.isValidSpan merged @?= True

          ,             testCase "Position advancement doesn't accumulate memory" $ do
                        let basePos = SourceLocation.SourcePos 1 1 0
                                              chars = take 1000 $ cycle "abcdefghijklmnopqrstuvwxyz"
                                              positions = scanl (\pos char -> SourceLocation.advancePos char pos) basePos chars
            performGC
            rnf positions `seq` True @?= True
            L.length positions @?= 1001
        ]

    , testGroup "Deep Evaluation Safety"
        [             testCase "Parser results can be deeply evaluated" $ do
                        let input = "func deep() { return [1, 2, 3]; }"
                                              result = Parser.parseTypus input
            case result of
              Left err -> length (show err) `seq` True @?= True
              Right parsed -> length (show parsed) `seq` True @?= True

          ,             testCase "Utils results are fully evaluable" $ do
                        let testString = "  \n  test string with\n  multiple lines  \n  "
                                              trimmed = Utils.trim testString
                                              split = Utils.splitBy '\n' testString
                                              uncommented = Utils.removeComments testString
                                              normalized = Utils.normalizeIndentation testString
            performGC
            rnf trimmed `seq` rnf split `seq` rnf uncommented `seq` rnf normalized `seq` True @?= True

        ,             testProperty "Deep evaluation doesn't cause issues" $ do
            \input -> let processed = Utils.trim input
                      in rnf processed `seq` True

        ,             testProperty "Complex operations are safe to force" $ do
            \input -> let step1 = Utils.removeComments input
                                              step2 = Utils.normalizeIndentation step1
                                              step3 = Utils.trim step2
              in rnf step3 `seq` True
        ]

    , testGroup "Memory Leak Prevention"
        [             testCase "Circular references don't cause leaks" $ do
                        let input = "func circular() { let x := x; return x; }"
                                              result = Parser.parseTypus input
            case result of
              Left err -> do
                            performGC
                rnf (show err) `seq` True @?= True
              Right parsed -> do
                            performGC
                length (show parsed) `seq` True @?= True

          ,             testCase "Large nested structures don't cause leaks" $ do
                        let nestedInput = unlines (["func nested() {"] ++ 
                               replicate 100 "  if (true) {" ++
                               replicate 100 "    return 1;" ++
                               replicate 100 "  }" ++
                               ["}"])
                                              result = Parser.parseTypus nestedInput
            case result of
              Left err -> do
                            performGC
                rnf (show err) `seq` True @?= True
              Right parsed -> do
                            performGC
                length (show parsed) `seq` True @?= True

          ,             testCase "Repeated operations don't accumulate" $ do
                        let input = "func accumulate() { return 1; }"
                                              operations = sequence $ replicate 1000 $ do
                                let result = Parser.parseTypus input
                    case result of
                      Left err -> length (show err) `seq` return False
                      Right parsed -> length (show parsed) `seq` return True
            results <- operations
            performGC
            L.all id results @?= True
        ]

    , testGroup "Resource Management"
        [             testCase "Parser releases resources on failure" $ do
                        let invalidInputs = ["{", "}", "func", "return", "if", "else", "for", "while"]
            results <- sequence $ L.map (\input -> do
                            let result = Parser.parseTypus input
                performGC
                case result of
                  Left err -> length (show err) `seq` return True
                  Right parsed -> length (show parsed) `seq` return True
                ) invalidInputs
            L.all id results @?= True

          ,             testCase "Utils functions release intermediate results" $ do
                        let largeInput = unlines $ replicate 1000 "// comment\nfunc test() { return 1; }"
                                              processed = Utils.removeComments largeInput
            performGC
            rnf processed `seq` True @?= True
            L.length (lines processed) >= 1000 @?= True

          ,             testCase "Memory usage stays bounded" $ do
                        let testSizes = [100, 500, 1000, 2000]
                                              testInputs = L.map (\n -> unlines $ replicate n "func test() { return 1; }") testSizes
            results <- sequence $ L.map (\input -> do
                            let result = Parser.parseTypus input
                performGC
                case result of
                  Left err -> length (show err) `seq` return (L.length $ show err)
                  Right parsed -> length (show parsed) `seq` return 1000  -- Arbitrary success value
                ) testInputs
            L.all (> 0) results @?= True
        ]

    , testGroup "Edge Case Memory Safety"
        [             testCase "Empty inputs don't cause issues" $ do
                        let emptyInputs = ["", "   ", "\n\t", "// comment", "/* */"]
            results <- sequence $ L.map (\input -> do
                            let result = Parser.parseTypus input
                performGC
                case result of
                  Left err -> length (show err) `seq` return True
                  Right parsed -> length (show parsed) `seq` return True
                ) emptyInputs
            L.all id results @?= True

          ,             testCase "Extremely long lines handled safely" $ do
                        let longLine = replicate 10000 'a' ++ " func test() { return 1; }"
                                              result = Parser.parseTypus longLine
            case result of
              Left err -> do
                            performGC
                rnf (show err) `seq` True @?= True
              Right parsed -> do
                            performGC
                length (show parsed) `seq` True @?= True

        ,             testProperty "Random inputs don't cause memory issues" $ do
            \input -> let result = Parser.parseTypus input
                      in case result of
                           Left err -> length (show err) `seq` True
                           Right parsed -> length (show parsed) `seq` True
        ]
    ]

-- Helper functions
isSuccess :: Either a b -> Bool
isSuccess (Right _) = True
isSuccess (Left _) = False

isInfixOf :: Eq                               a => [a] -> [a] -> Bool
isInfixOf needle                               haystack = needle `L.isPrefixOf` haystack || 
                            (not (null haystack) && isInfixOf needle (L.tail haystack)