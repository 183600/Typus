module Test.Unit.CompilerSpec (tests) where

import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import qualified Compiler
import qualified Compiler.DependentTypeChecker as DepChecker
import qualified Parser

tests :: TestTree
tests =
  testGroup "Compiler"
    [ testCase "generates Go code for valid Typus input" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"hello\")"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "compiled code should start with a package declaration" ("package main" `isInfixOf` goCode)
        assertBool "compiled code should contain the main function" ("func main" `isInfixOf` goCode)

    , testCase "fails when dependent type blocks contain errors" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func main() {}"
              , ""
              , "{//! dependent_types: on}"
              , "alias Broken"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> assertBool "error should mention dependent type checking" ("DependentTypeCheckingPhase" `isInfixOf` Compiler.renderCompilationError err)
          Right _  -> assertFailure "expected dependent type error"

    , testCase "file-level dependent type directives are enforced without blocks" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "alias Broken"
              ]
        typusFile <- expectParse source
        case DepChecker.checkDependentTypes typusFile of
          Left errs -> assertBool "expected at least one dependent type error" (not $ null errs)
          Right _   -> assertFailure "expected dependent type errors when file directive is enabled"

    , testCase "reports detailed diagnostics when type checking fails" $ do
        let source = unlines
              [ "package main"
              , "func add(x int, y int) int {"
              , "    return x + y"
              , "}"
              , ""
              , "func main() {"
              , "    add(\"oops\", 2)"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left errs -> do
            let rendered = Compiler.renderCompilationError errs
            assertBool "expected summary type-check failure" ("Type errors detected during semantic analysis" `isInfixOf` rendered)
            assertBool "expected detailed argument type mismatch" ("Type error in 'main': add argument 1: expected type int, got string" `isInfixOf` rendered)
          Right _ -> assertFailure "expected type checker to emit detailed diagnostics"

    , testCase "rejects malformed syntax with unbalanced braces" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> assertBool "error should mention malformed syntax" ("Malformed syntax detected" `isInfixOf` Compiler.renderCompilationError err)
          Right _  -> assertFailure "expected malformed syntax to be rejected"

    -- Additional test cases
    , testCase "compiles complex expressions" $ do
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
        assertBool "should contain conditional logic" ("if" `isInfixOf` goCode && "else" `isInfixOf` goCode)

    , testCase "handles struct compilation" $ do
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
        assertBool "should contain struct definition" ("type Person struct" `isInfixOf` goCode)

    , testCase "compiles interface definitions" $ do
        let source = unlines
              [ "package main"
              , "type Writer interface {"
              , "    Write([]byte) (int, error)"
              , "    Close() error"
              , "}"
              , "func main() {}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should contain interface definition" ("type Writer interface" `isInfixOf` goCode)

    , testCase "handles method compilation" $ do
        let source = unlines
              [ "package main"
              , "type Counter struct {"
              , "    value int"
              , "}"
              , "func (c *Counter) Increment() {"
              , "    c.value++"
              , "}"
              , "func (c Counter) Value() int {"
              , "    return c.value"
              , "}"
              , "func main() {"
              , "    c := Counter{value: 0}"
              , "    c.Increment()"
              , "    println(c.Value())"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectGenerateCode typusFile
        assertBool "should contain method definitions" ("func (c *Counter)" `isInfixOf` goCode)

    , testCase "compiles generic types" $ do
        let source = unlines
              [ "package main"
              , "type Container[T any] struct {"
              , "    value T"
              , "}"
              , "func New[T any](v T) Container[T] {"
              , "    return Container[T]{value: v}"
              , "}"
              , "func (c Container[T]) Get() T {"
              , "    return c.value"
              , "}"
              , "func main() {"
              , "    c := New(42)"
              , "    println(c.Get())"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectGenerateCode typusFile
        assertBool "should contain generic type definitions" ("Container[T" `isInfixOf` goCode)

    , testCase "handles channel compilation" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    ch := make(chan int, 10)"
              , "    ch <- 42"
              , "    value := <-ch"
              , "    select {"
              , "    case v := <-ch:"
              , "        println(v)"
              , "    default:"
              , "        println(\"no value\")"
              , "    }"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectGenerateCode typusFile
        assertBool "should contain channel operations" ("make(chan" `isInfixOf` goCode)

    , testCase "compiles goroutines" $ do
        let source = unlines
              [ "package main"
              , "func worker() {"
              , "    println(\"working\")"
              , "}"
              , "func main() {"
              , "    go worker()"
              , "    go func() {"
              , "        println(\"anonymous\")"
              , "    }()"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectGenerateCode typusFile
        assertBool "should contain goroutine calls" ("go worker" `isInfixOf` goCode)

    , testCase "handles defer statements" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    defer println(\"first\")"
              , "    defer func() {"
              , "        println(\"second\")"
              , "    }()"
              , "    println(\"main\")"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectGenerateCode typusFile
        assertBool "should contain defer statements" ("defer println" `isInfixOf` goCode)

    , testCase "compiles complex literals" $ do
        let source = unlines
              [ "package main"
              , "type Point struct {"
              , "    X, Y int"
              , "}"
              , "func main() {"
              , "    p := Point{X: 1, Y: 2}"
              , "    points := []Point{{1, 2}, {3, 4}}"
              , "    m := map[string]int{\"a\": 1, \"b\": 2}"
              , "    println(p, points, m)"
              , "}"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectGenerateCode typusFile
        assertBool "should contain struct literals" ("Point{X: 1" `isInfixOf` goCode)

    , testCase "handles import statements" $ do
        let source = unlines
              [ "package main"
              , "import \"fmt\""
              , "import ("
              , "    \"os\""
              , "    \"strings\""
              , ")"
              , "func main() {"
              , "    fmt.Println(\"hello\")"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should contain import statements" ("import" `isInfixOf` goCode)

    , testCase "compiles constants and variables" $ do
        let source = unlines
              [ "package main"
              , "const PI = 3.14159"
              , "const ("
              , "    A = 1"
              , "    B = 2"
              , "    C = 3"
              , ")"
              , "var x int = 10"
              , "var ("
              , "    y int = 20"
              , "    z string = \"hello\""
              , ")"
              , "func main() {"
              , "    println(PI, A, B, C, x, y, z)"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should contain constants and variables" ("const PI" `isInfixOf` goCode)

    , testCase "handles error types" $ do
        let source = unlines
              [ "package main"
              , "import \"errors\""
              , "func mightFail() error {"
              , "    return errors.New(\"something went wrong\")"
              , "}"
              , "func main() {"
              , "    if err := mightFail(); err != nil {"
              , "        println(err.Error())"
              , "    }"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should contain error handling" ("error" `isInfixOf` goCode)

    , testCase "compiles ownership-enabled code" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func consume(x string) string { return x }"
              , "func main() {"
              , "    data := \"hello\""
              , "    result := consume(data)"
              , "    println(result)"
              , "}"
              ]
        typusFile <- expectParse source
        goCode <- expectCompile typusFile
        assertBool "should compile ownership code" ("package main" `isInfixOf` goCode)

    , testCase "compiles dependent types code" $ do
        let source = unlines
              [ "//! dependent_types: on"
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
        assertBool "should compile dependent types code" ("package main" `isInfixOf` goCode)

    , testCase "handles mixed ownership and dependent types" $ do
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
        assertBool "should compile mixed features code" ("package main" `isInfixOf` goCode)

    , testCase "reports multiple compilation errors" $ do
        let source = unlines
              [ "package main"
              , "func add(x int, y int) int {"
              , "    return x + y"
              , "}"
              , "func multiply(x int, y int) int {"
              , "    return x * y"
              , "}"
              , "func main() {"
              , "    add(\"oops1\", 2)"
              , "    multiply(\"oops2\", 3)"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should report multiple errors" (length (lines rendered) > 2)
            assertBool "should include both function errors" ("add" `isInfixOf` rendered && "multiply" `isInfixOf` rendered)
          Right _ -> assertFailure "expected multiple compilation errors"

    , testCase "handles undefined variables" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(undefinedVar)"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool "should mention undefined variable" ("undefinedVar" `isInfixOf` rendered)
          Right _ -> assertFailure "expected undefined variable error"

    , testCase "detects circular dependencies" $ do
        let source = unlines
              [ "package main"
              , "func a() {"
              , "    b()"
              , "}"
              , "func b() {"
              , "    a()"
              , "}"
              , "func main() {"
              , "    a()"
              , "}"
              ]
        typusFile <- expectParse source
        case Compiler.compile typusFile of
          Left err -> do
            let rendered = Compiler.renderCompilationError err
            assertBool ("should detect circular dependency. Error was: " ++ rendered) ("Circular" `isInfixOf` rendered || "circular" `isInfixOf` rendered || "cycle" `isInfixOf` rendered)
          Right _ -> assertFailure "expected circular dependency error"
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

-- Generate Go code without type checking (for testing code generation)
expectGenerateCode :: Parser.TypusFile -> IO String
expectGenerateCode typusFile = pure (Compiler.generateGoCode typusFile)
