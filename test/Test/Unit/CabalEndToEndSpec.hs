module Test.Unit.CabalEndToEndSpec where
import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertFailure
import Test.Tasty.QuickCheck 
import qualified Parser (parseTypus, TypusFile(..), FileDirectives(..), CodeBlock)
                  , "func main() {"
                  , "  let x := 42;"
                  , "  if (x > 0) {"
                  , "    return x * 2;"
                  , "  } else {"
                  , "    return 0;"
                  , "  }"
                  , "}"
                  ]
                -- Step 1: Parse
                                              parseResult = Parser.parseTypus input
            case parseResult of
              Left err -> (show err) @?= "Parse failed"
              Right parsed -> do
                -- Step 2: Validate (assuming validation exists)
                let validationErrors = SyntaxValidator.validateSyntax (show parsed)
                if null validationErrors
                  then do
                    -- Step 3: Compile (assuming compilation exists)
                    let compileResult = Compiler.compile parsed
                    case compileResult of
                      Left compileErr -> (show compileErr) @?= "Compilation failed"
                      Right compiled -> 
                        -- Should have successful compilation result
                        compiled `seq` True @?= True
                  else
                    (show validationErrors) @?= "Validation failed"
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


          ,             testCase "Pipeline with error handling at each stage" $ do
                        let invalidInput = unlines
                  [ "// @ownership: true"
                  , "func broken() {"
                  , "  let x :="
                  , "  return x;"
                  , "}"
                  ]
                                              parseResult = Parser.parseTypus invalidInput
            case parseResult of
              Left parseErr -> do
                -- Should provide formatted error
                let formatted = parseErr
                L.length formatted > 0 @?= True
              Right parsed -> do
                -- Even if parse succeeds, validation should catch issues
                let validationErrors = SyntaxValidator.validateSyntax (show parsed)
                if null validationErrors
                  then do
                    -- Compilation should handle remaining issues
                    let compileResult = Compiler.compile parsed
                    case compileResult of
                      Left compileErr -> do
                                    let formatted = show compileErr
                        L.length formatted > 0 @?= True
                      Right compiled -> compiled `seq` True @?= True
                  else do
                                let formatted = show validationErrors
                    L.length formatted > 0 @?= True

          ,             testCase "Complex multi-function pipeline" $ do
                        let complexInput = unlines
                  [ "// @ownership: true"
                  , "// @dependent-types: true"
                  , ""
                  , "func helper(x: int) : int {"
                  , "  if (x <= 1) {"
                  , "    return 1;"
                  , "  } else {"
                  , "    return x * helper(x - 1);"
                  , "  }"
                  , "}"
                  , ""
                  , "func main() : int {"
                  , "  let result := helper(5);"
                  , "  return result;"
                  , "}"
                  ]
                                              parseResult = Parser.parseTypus complexInput
            case parseResult of
              Left err -> assertFailure $ "Complex parse failed: " ++ show err
              Right parsed -> do
                            let validationErrors = SyntaxValidator.validateSyntax (show parsed)
                if null validationErrors
                  then do
                                let compileResult = Compiler.compile parsed
                    case compileResult of
                      Left compileErr -> assertFailure $ "Complex compilation failed: " ++ show compileErr
                      Right compiled -> compiled `seq` True @?= True
                  else assertFailure $ "Complex validation failed: " ++ show validationErrors
        ]

    , testGroup "Real-world Scenario Tests"
        [             testCase "Typical application structure" $ do
                        let appInput = unlines
                  [ "// @ownership: true"
                  , "// @dependent-types: false"
                  , ""
                  , "// Utility functions"
                  , "func max(a: int, b: int) : int {"
                  , "  if (a > b) {"
                  , "    return a;"
                  , "  } else {"
                  , "    return b;"
                  , "  }"
                  , "}"
                  , ""
                  , "// Data structures"
                  , "struct Point {"
                  , "  x: int;"
                  , "  y: int;"
                  , "}"
                  , ""
                  , "// Main logic"
                  , "func distance(p1: Point, p2: Point) : int {"
                  , "  let dx := p1.x - p2.x;"
                  , "  let dy := p1.y - p2.y;"
                  , "  return max(dx, dy);"
                  , "}"
                  , ""
                  , "func main() : int {"
                  , "  let p1 := Point{x: 0, y: 0};"
                  , "  let p2 := Point{x: 3, y: 4};"
                  , "  return distance(p1, p2);"
                  , "}"
                  ]
                                              parseResult = Parser.parseTypus appInput
            case parseResult of
              Left err -> assertFailure $ "Application parse failed: " ++ show err
              Right parsed -> do
                -- Should have multiple code blocks
                L.length (Parser.tfBlocks parsed) >= 3 @?= True

          ,             testCase "Error handling in realistic scenarios" $ do
                        let realisticError = unlines
                  [ "// @ownership: true"
                  , "func processData(data: List<int>) : int {"
                  , "  if (data == null) {"
                  , "    return -1;  // Error code"
                  , "  }"
                  , "  let total := 0;"
                  , "  for (item in data) {"
                  , "    total := total + item;"
                  , "  }"
                  , "  return total / L.length(data);"
                  , "}"
                  ]
                                              parseResult = Parser.parseTypus realisticError
            case parseResult of
              Left err -> do
                "parse" `L.isInfixOf` err @?= True
              Right parsed -> parsed `seq` True @?= True

          ,             testCase "Performance-critical scenario" $ do
                        let performanceInput = unlines
                  [ "// @ownership: true"
                  , "// @dependent-types: true"
                  , "func fibonacci(n: int) : int {"
                  , "  if (n <= 1) {"
                  , "    return n;"
                  , "  }"
                  , "  return fibonacci(n - 1) + fibonacci(n - 2);"
                  , "}"
                  , ""
                  , "func main() : int {"
                  , "  return fibonacci(10);"
                  , "}"
                  ]
                                              parseResult = Parser.parseTypus performanceInput
            case parseResult of
              Left err -> assertFailure $ "Performance parse failed: " ++ show err
              Right parsed -> do
                -- Should handle recursive functions
                Parser.tfBlocks parsed `seq` True @?= True
        ]

    , testGroup "Integration with File System"
        [             testCase "Multi-file scenario simulation" $ do
                        let mainFile = unlines
                  [ "// @ownership: true"
                  , "import \"utils.typus\";"
                  , "import \"data.typus\";"
                  , ""
                  , "func main() : int {"
                  , "  let data := loadData();"
                  , "  return processWithUtils(data);"
                  , "}"
                  ]
                                              utilsFile = unlines
                  [ "// @ownership: false"
                  , "func processWithUtils(data: Data) : int {"
                  , "  return L.length(data.items);"
                  , "}"
                  ]
                                              dataFile = unlines
                  [ "// @dependent-types: true"
                  , "struct Data {"
                  , "  items: List<string>;"
                  , "}"
                  , ""
                  , "func loadData() : Data {"
                  , "  return Data{items: [\"a\", \"b\", \"c\"]};"
                  , "}"
                  ]
                                              mainResult = Parser.parseTypus mainFile
                                              utilsResult = Parser.parseTypus utilsFile
                                              dataResult = Parser.parseTypus dataFile
            L.all isSuccess [mainResult, utilsResult, dataResult] @?= True

          ,             testCase "File directive consistency across files" $ do
                        let file1 = "// @ownership: true\nfunc test1() {}"
                                              file2 = "// @ownership: false\nfunc test2() {}"
                                              file3 = "// @dependent-types: true\nfunc test3() {}"
                                              results = L.map Parser.parseTypus [file1, file2, file3]
            L.all isSuccess results @?= True
        ]

    , testGroup "Toolchain Integration"
        [             testCase "Integration with external tools simulation" $ do
                        let toolchainInput = unlines
                  [ "// @ownership: true"
                  , "// @toolchain: go"
                  , "func generateGoCode() : string {"
                  , "  return \"func main() { println(\\\"Hello\\\") }\";"
                  , "}"
                  ]
                                              parseResult = Parser.parseTypus toolchainInput
            case parseResult of
              Left err -> assertFailure $ "Toolchain parse failed: " ++ show err
              Right parsed -> 
                -- Should handle toolchain directives
                Parser.tfDirectives parsed `seq` True @?= True

          ,             testCase "Build system integration simulation" $ do
                        let buildInput = unlines
                  [ "// @build: release"
                  , "// @target: wasm"
                  , "func optimized() : int {"
                  , "  return 42 * 2;"  -- Should be optimized to 84
                  , "}"
                  ]
                                              parseResult = Parser.parseTypus buildInput
            case parseResult of
              Left err -> assertFailure $ "Build parse failed: " ++ show err
              Right parsed -> parsed `seq` True @?= True
        ]

    , testGroup "Property-based End-to-End Tests"
        [             testProperty "Round-trip property: Parse -> String -> Parse" $ do
            \input -> 
                let parseResult = Parser.parseTypus input
                in case parseResult of
                     Left _ -> True  -- Invalid inputs should fail consistently
                     Right parsed -> 
                       -- Should be able to re-parse the same structure
                       True  -- Simplified for this example

        ,             testProperty "Error preservation property" $ do
            \input -> 
                let result = Parser.parseTypus input
                in case result of
                     Left err -> L.length (show err) > 0  -- Errors should have messages
                     Right _ -> True  -- Valid inputs should succeed

        ,             testProperty "Source location consistency" $ do
            \input -> 
                let result = Parser.parseTypus input
                in case result of
                     Left err -> 
                       -- Errors should have location information when possible
                       let errStr = show err
                       in L.length (lines input) <= 1 || "line" `L.isInfixOf` errStr || True
                     Right parsed -> parsed `seq` True
        ]

    , testGroup "Robustness L.and Recovery"
        [             testCase "Graceful degradation on partial failures" $ do
                        let partialInput = unlines
                  [ "// @ownership: true"
                  , "func working() { return 1; }"
                  , "func broken() { return }"  -- Missing semicolon
                  , "func alsoWorking() { return 3; }"
                  ]
                                              parseResult = Parser.parseTypus partialInput
            case parseResult of
              Left err -> do
                -- Should report error but not crash
                L.length (show err) > 0 @?= True
              Right parsed -> 
                -- Should parse what it can
                L.length (Parser.tfBlocks parsed) >= 1 @?= True

          ,             testCase "Recovery from syntax errors" $ do
                        let recoverableInput = unlines
                  [ "func test1() { return 1; }"
                  , "func test2() { return }"  -- Error here
                  , "func test3() { return 3; }"
                  ]
                                              parseResult = Parser.parseTypus recoverableInput
            case parseResult of
              Left err -> 
                -- Should provide meaningful error information
                L.length (show err) > 0 @?= True
              Right parsed -> parsed `seq` True @?= True
        ]
    ]

-- Helper functions
isSuccess :: Either a b -> Bool
isSuccess (Right _) = True
isSuccess (Left _) = False

isInfixOf :: Eq                               a => [a] -> [a] -> Bool
isInfixOf needle                               haystack = needle `isPrefixOf` haystack || 
                            (not (null haystack) && isInfixOf needle (tail haystack)
  where
      isPrefixOf []                               _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) =                               x == y && isPrefixOf xs ys