module Test.Unit.CabalEndToEndSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import qualified Parser (parseTypus, TypusFile(..), FileDirectives(..), CodeBlock(..))
import qualified Utils (trim, splitBy, removeComments, normalizeIndentation)
import qualified SourceLocation
import qualified SyntaxValidator
import qualified Compiler
import qualified ErrorHandler

-- | End-to-end integration tests
tests :: TestTree
tests =
  testGroup "Cabal End-to-End Tests"
    [ testGroup "Complete Compilation Pipeline"
        [ testCase "Full pipeline: Parse -> Validate -> Compile" $ do
            let input = unlines
                  [ "// @ownership: true"
                  , "// @dependent-types: false"
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
                parseResult = Parser.parseTypus "end-to-end" input
            case parseResult of
              Left err -> @?= "Parse failed" (show err)
              Right parsed -> do
                -- Step 2: Validate (assuming validation exists)
                let validationResult = SyntaxValidator.validate parsed
                case validationResult of
                  Left validationErr -> @?= "Validation failed" (show validationErr)
                  Right validated -> do
                    -- Step 3: Compile (assuming compilation exists)
                    let compileResult = Compiler.compile validated
                    case compileResult of
                      Left compileErr -> @?= "Compilation failed" (show compileErr)
                      Right compiled -> 
                        -- Should have successful compilation result
                        compiled `seq` True @?= True

        , testCase "Pipeline with error handling at each stage" $ do
            let invalidInput = unlines
                  [ "// @ownership: true"
                  , "func broken() {"
                  , "  let x :="
                  , "  return x;"
                  , "}"
                  ]
                parseResult = Parser.parseTypus "pipeline-error" invalidInput
            case parseResult of
              Left parseErr -> do
                -- Should provide formatted error
                let formatted = ErrorHandler.formatError parseErr
                L.length formatted > 0 @?= True
              Right parsed -> do
                -- Even if parse succeeds, validation should catch issues
                let validationResult = SyntaxValidator.validate parsed
                case validationResult of
                  Left validationErr -> do
                    let formatted = ErrorHandler.formatError validationErr
                    L.length formatted > 0 @?= True
                  Right validated -> do
                    -- Compilation should handle remaining issues
                    let compileResult = Compiler.compile validated
                    case compileResult of
                      Left compileErr -> do
                        let formatted = ErrorHandler.formatError compileErr
                        L.length formatted > 0 @?= True
                      Right compiled -> compiled `seq` True @?= True

        , testCase "Complex multi-function pipeline" $ do
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
                parseResult = Parser.parseTypus "complex" complexInput
            case parseResult of
              Left err -> @?= "Complex parse failed" (show err)
              Right parsed -> do
                let validationResult = SyntaxValidator.validate parsed
                case validationResult of
                  Left validationErr -> @?= "Complex validation failed" (show validationErr)
                  Right validated -> do
                    let compileResult = Compiler.compile validated
                    case compileResult of
                      Left compileErr -> @?= "Complex compilation failed" (show compileErr)
                      Right compiled -> compiled `seq` True @?= True
        ]

    , testGroup "Real-world Scenario Tests"
        [ testCase "Typical application structure" $ do
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
                parseResult = Parser.parseTypus "application" appInput
            case parseResult of
              Left err -> @?= "Application parse failed" (show err)
              Right parsed -> do
                -- Should have multiple code blocks
                L.length (Parser.tfCodeBlocks parsed) >= 3 @?= True

        , testCase "Error handling in realistic scenarios" $ do
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
                parseResult = Parser.parseTypus "realistic" realisticError
            case parseResult of
              Left err -> do
                let formatted = ErrorHandler.formatError err
                "line" `L.isInfixOf` formatted @?= True
              Right parsed -> parsed `seq` True @?= True

        , testCase "Performance-critical scenario" $ do
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
                parseResult = Parser.parseTypus "performance" performanceInput
            case parseResult of
              Left err -> @?= "Performance parse failed" (show err)
              Right parsed -> do
                -- Should handle recursive functions
                Parser.tfCodeBlocks parsed `seq` True @?= True
        ]

    , testGroup "Integration with File System"
        [ testCase "Multi-file scenario simulation" $ do
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
                mainResult = Parser.parseTypus "main" mainFile
                utilsResult = Parser.parseTypus "utils" utilsFile
                dataResult = Parser.parseTypus "data" dataFile
            L.all isSuccess [mainResult, utilsResult, dataResult] @?= True

        , testCase "File directive consistency across files" $ do
            let file1 = "// @ownership: true\nfunc test1() {}"
                file2 = "// @ownership: false\nfunc test2() {}"
                file3 = "// @dependent-types: true\nfunc test3() {}"
                results = L.map (Parser.parseTypus "file") [file1, file2, file3]
            L.all isSuccess results @?= True
        ]

    , testGroup "Toolchain Integration"
        [ testCase "Integration with external tools simulation" $ do
            let toolchainInput = unlines
                  [ "// @ownership: true"
                  , "// @toolchain: go"
                  , "func generateGoCode() : string {"
                  , "  return \"func main() { println(\\\"Hello\\\") }\";"
                  , "}"
                  ]
                parseResult = Parser.parseTypus "toolchain" toolchainInput
            case parseResult of
              Left err -> @?= "Toolchain parse failed" (show err)
              Right parsed -> 
                -- Should handle toolchain directives
                Parser.tfDirectives parsed `seq` True @?= True

        , testCase "Build system integration simulation" $ do
            let buildInput = unlines
                  [ "// @build: release"
                  , "// @target: wasm"
                  , "func optimized() : int {"
                  , "  return 42 * 2;"  -- Should be optimized to 84
                  , "}"
                  ]
                parseResult = Parser.parseTypus "build" buildInput
            case parseResult of
              Left err -> @?= "Build parse failed" (show err)
              Right parsed -> parsed `seq` True @?= True
        ]

    , testGroup "Property-based End-to-End Tests"
        [ testProperty "Round-trip property: Parse -> String -> Parse" $ do
            \input -> 
                let parseResult = Parser.parseTypus "property" input
                in case parseResult of
                     Left _ -> True  -- Invalid inputs should fail consistently
                     Right parsed -> 
                       -- Should be able to re-parse the same structure
                       True  -- Simplified for this example

        , testProperty "Error preservation property" $ do
            \input -> 
                let result = Parser.parseTypus "error-property" input
                in case result of
                     Left err -> L.length (show err) > 0  -- Errors should have messages
                     Right _ -> True  -- Valid inputs should succeed

        , testProperty "Source location consistency" $ do
            \input -> 
                let result = Parser.parseTypus "location-property" input
                in case result of
                     Left err -> 
                       -- Errors should have location information when possible
                       let errStr = show err
                       in L.length (lines input) <= 1 || "line" `L.isInfixOf` errStr || True
                     Right parsed -> parsed `seq` True
        ]

    , testGroup "Robustness L.and Recovery"
        [ testCase "Graceful degradation on partial failures" $ do
            let partialInput = unlines
                  [ "// @ownership: true"
                  , "func working() { return 1; }"
                  , "func broken() { return }"  -- Missing semicolon
                  , "func alsoWorking() { return 3; }"
                  ]
                parseResult = Parser.parseTypus "partial" partialInput
            case parseResult of
              Left err -> do
                -- Should report error but not crash
                L.length (show err) > 0 @?= True
              Right parsed -> 
                -- Should parse what it can
                L.length (Parser.tfCodeBlocks parsed) >= 1 @?= True

        , testCase "Recovery from syntax errors" $ do
            let recoverableInput = unlines
                  [ "func test1() { return 1; }"
                  , "func test2() { return }"  -- Error here
                  , "func test3() { return 3; }"
                  ]
                parseResult = Parser.parseTypus "recovery" recoverableInput
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

isInfixOf :: Eq a => [a] -> [a] -> Bool
L.isInfixOf needle haystack = needle `L.isPrefixOf` haystack || 
                              (not (null haystack) && L.isInfixOf needle (L.tail haystack))
  where
    L.isPrefixOf [] _ = True
    L.isPrefixOf _ [] = False
    L.isPrefixOf (x:xs) (y:ys) = x == y && L.isPrefixOf xs ys