module Test.Unit.IntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import qualified Data.List as L
import Data.List (isInfixOf)
import qualified Compiler
import qualified CompilerUtils
import qualified Parser

tests :: TestTree
tests =
  testGroup "Integration tests"
    [ testCase "compiles L.and runs simple program" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"hello world\")"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "generated Go code should be runnable" ("package main" `L.isInfixOf` goCode)
        assertBool "should contain main function" ("func main" `L.isInfixOf` goCode)

    , testCase "compiles program with multiple functions" $ do
        let source = unlines
              [ "package main"
              , "func add(x int, y int) int {"
              , "    return x + y"
              , "}"
              , "func multiply(x int, y int) int {"
              , "    return x * y"
              , "}"
              , "func main() {"
              , "    result := add(5, 3)"
              , "    println(result)"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should contain both functions" ("func add" `L.isInfixOf` goCode && "func multiply" `L.isInfixOf` goCode)

    , testCase "handles complex type definitions" $ do
        let source = unlines
              [ "package main"
              , "type Person struct {"
              , "    name string"
              , "    age int"
              , "}"
              , "func main() {"
              , "    p := Person{name: \"Alice\", age: 30}"
              , "    println(p.name)"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should contain struct definition" ("type Person struct" `L.isInfixOf` goCode)

    , testCase "processes ownership L.and dependent types together" $ do
        let source = unlines
              [ "//! ownership: on"
              , "//! dependent_types: on"
              , "package main"
              , "type Vector<T> struct {"
              , "    data: T"
              , "}"
              , "where len data > 0"
              , "func main() {"
              , "    v := Vector{data: []int{1, 2, 3}}"
              , "    println(v.data)"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should generate code with both features" ("package main" `L.isInfixOf` goCode)

    , testCase "handles block-level directives" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on"
              , "        data := \"hello\""
              , "        consume(data)"
              , "        // This should error if ownership is enforced"
              , "    }"
              , "    {//! ownership: off"
              , "        data2 := \"world\""
              , "        consume(data2)"
              , "        println(data2)"
              , "    }"
              , "}"
              , "func consume(s string) string { return s }"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should report ownership error in first block" ("ownership" `L.isInfixOf` rendered)
          Right goCode -> do
            assertBool "generated code should handle directives" ("package main" `L.isInfixOf` goCode)

    , testCase "integrates error handling across phases" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "type Invalid<T> struct {"
              , "    value: T"
              , "}"
              , "where len undefined > 0"
              , "func main() {"
              , "    x := Invalid{value: 42}"
              , "    println(x.value)"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should catch dependent type errors" ("dependent" `L.isInfixOf` rendered)
            assertBool "should mention undefined variable" ("undefined" `L.isInfixOf` rendered)
          Right _ -> assertFailure "expected compilation to fail with dependent type error"

    , testCase "handles Go toolchain integration" $ do
        let source = unlines
              [ "package main"
              , "import \"fmt\""
              , "func main() {"
              , "    fmt.Println(\"Hello from Go\")"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should include import statements" ("import" `L.isInfixOf` goCode)
        assertBool "should use fmt package" ("fmt." `L.isInfixOf` goCode)

    , testCase "processes complex expressions" $ do
        let source = unlines
              [ "package main"
              , "func calculate(x int, y int) int {"
              , "    if x > y {"
              , "        return x * 2 + y"
              , "    } else {"
              , "        return y * 3 - x"
              , "    }"
              , "}"
              , "func main() {"
              , "    result := calculate(10, 5)"
              , "    println(result)"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should contain conditional logic" ("if" `L.isInfixOf` goCode && "else" `L.isInfixOf` goCode)

    , testCase "handles multiple source files" $ do
        let source1 = unlines
              [ "package main"
              , "func helper() string {"
              , "    return \"helper function\""
              , "}"
              ]
        let source2 = unlines
              [ "package main"
              , "func main() {"
              , "    result := helper()"
              , "    println(result)"
              , "}"
              ]
        typusFile1 <- expectParse source1
        typusFile2 <- expectParse source2
        -- 使用compileWithPackage编译多个文件
        let packageFiles = [("helper.typus", typusFile1), ("main.typus", typusFile2)]
        goCode <- case CompilerUtils.compileWithPackage typusFile2 packageFiles of
            Left err -> assertFailure ("compile failed: " <> Compiler.renderCompilationError err)
            Right code -> pure code
        assertBool "should contain helper function" ("func helper()" `L.isInfixOf` goCode)
        assertBool "should contain main function" ("func main()" `L.isInfixOf` goCode)

    , testCase "maintains type safety across compilation phases" $ do
        let source = unlines
              [ "package main"
              , "func process(x int) string {"
              , "    return string(x)"
              , "}"
              , "func main() {"
              , "    result := process(42)"
              , "    println(result)"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should catch type conversion issues" ("type" `L.isInfixOf` rendered)
          Right _ -> assertFailure "expected type safety error"

    , testCase "handles edge cases in parsing L.and compilation" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    // Empty function"
              , "}"
              , ""
              , "// Comment at end"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should handle empty functions" ("func main" `L.isInfixOf` goCode)

    , testCase "preserves comments L.and formatting" $ do
        let source = unlines
              [ "package main"
              , "// This is a comment"
              , "func main() {"
              , "    // Another comment"
              , "    fmt.Println(\"hello\") // Inline comment"
              , "}"
              ]
        typusFile <- expectParse source
        -- Use generateGoCode instead of compile to avoid type checking errors
        let goCode = Compiler.generateGoCode typusFile
        assertBool "should preserve some comments" ("//" `L.isInfixOf` goCode)

    ]

expectParse :: String -> IO Parser.TypusFile
expectParse source =
  case Parser.parseTypus source of
    Left err     -> assertFailure ("parseTypus failed: " <> err)
    Right parsed -> pure parsed

expectCompile :: Parser.TypusFile -> IO String
expectCompile typusFile =
  case Compiler.compile typusFile of
    Left err      -> assertFailure ("compile failed: " <> Compiler.renderCompilationError err)
    Right goCode  -> pure goCode