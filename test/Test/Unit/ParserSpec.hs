module Test.Unit.ParserSpec (tests) where

import Data.List (find, isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import Parser
  ( BlockDirectives(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedValue
  , spanEnd
  , spanStart
  )

tests :: TestTree
tests =
  testGroup "Parser"
    [ testCase "parses file-level directives" $ do
        let source = unlines
              [ "//! ownership: on"
              , "//! dependent_types: off"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes } = tfDirectives typusFile
            case ownership of
              Nothing -> assertFailure "expected ownership directive"
              Just loc -> do
                locatedValue loc @?= True
                posLine (spanStart (locSpan loc)) @?= 1
                posLine (spanEnd (locSpan loc)) @?= 2
            case dependentTypes of
              Nothing -> assertFailure "expected dependent types directive"
              Just loc -> do
                locatedValue loc @?= False
                posLine (spanStart (locSpan loc)) @?= 2
                posLine (spanEnd (locSpan loc)) @?= 3

    , testCase "treats file-level constraints directive as dependent type alias" $ do
        let source = unlines
              [ "//! constraints: on"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let FileDirectives { fdConstraints = constraints, fdDependentTypes = dependentTypes } = tfDirectives typusFile
            case constraints of
              Nothing -> assertFailure "expected constraints directive"
              Just loc -> locatedValue loc @?= True
            dependentTypes @?= constraints

    , testCase "captures block directives with associated code" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on, dependent_types: on}"
              , "        println(\"inside\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let ownershipBlock = find (maybe False locatedValue . bdOwnership . cbDirectives) (tfBlocks typusFile)
            case ownershipBlock of
              Nothing -> assertFailure "expected to find a block with ownership enabled"
              Just CodeBlock { cbDirectives = directives, cbContent = content, cbSpan = blkSpan } -> do
                case bdOwnership directives of
                  Nothing -> assertFailure "expected ownership flag"
                  Just loc -> do
                    locatedValue loc @?= True
                    posLine (spanStart (locSpan loc)) @?= 3
                case bdDependentTypes directives of
                  Nothing -> assertFailure "expected dependent types flag"
                  Just loc -> locatedValue loc @?= True
                bdConstraints directives @?= Nothing
                assertBool "block content should include println call" ("println(\"inside\")" `isInfixOf` content)
                posLine (spanStart blkSpan) @?= 4
                posLine (spanEnd blkSpan) @?= 5

    , testCase "parses README-style block directives without inline closing brace" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on"
              , "        println(\"ownership block\")"
              , "    }"
              , ""
              , "    {//! constraints: on"
              , "        println(\"dependent block\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let blocks = tfBlocks typusFile
                hasDirective selector CodeBlock { cbDirectives = directives } =
                  maybe False locatedValue (selector directives)
                ownershipBlock = find (hasDirective bdOwnership) blocks
                constraintsBlock = find (hasDirective bdConstraints) blocks
            case ownershipBlock of
              Nothing -> assertFailure "expected README ownership block"
              Just CodeBlock { cbDirectives = directives, cbContent = content } -> do
                case bdOwnership directives of
                  Nothing -> assertFailure "expected ownership directive"
                  Just loc -> locatedValue loc @?= True
                assertBool "ownership block should include println call" ("println(\"ownership block\")" `isInfixOf` content)
            case constraintsBlock of
              Nothing -> assertFailure "expected README constraints block"
              Just CodeBlock { cbDirectives = directives, cbContent = content } -> do
                case bdConstraints directives of
                  Nothing -> assertFailure "expected constraints directive"
                  Just loc -> locatedValue loc @?= True
                case bdDependentTypes directives of
                  Nothing -> assertFailure "expected dependent types alias"
                  Just loc -> locatedValue loc @?= True
                bdConstraints directives @?= bdDependentTypes directives
                assertBool "constraints block should include println call" ("println(\"dependent block\")" `isInfixOf` content)

    , testCase "ignores trailing whitespace-only files" $ do
        let source :: String; source = "\n   \n\n"
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> tfBlocks typusFile @?= []

    , testCase "collects build tags before the first code block" $ do
        let source = unlines
              [ "//go:build ignore"
              , "// +build ignore"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right TypusFile { tfBuildTags = buildTags } -> do
            map locatedValue buildTags @?= ["//go:build ignore", "// +build ignore"]
            case buildTags of
              (firstTag:secondTag:_) -> do
                posLine (spanStart (locSpan firstTag)) @?= 1
                posLine (spanStart (locSpan secondTag)) @?= 2
              _ -> assertFailure "expected two build tags"

    , testCase "rejects unknown file directives" $ do
        let source = unlines
              [ "//! unsupported: on"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertBool ("error should mention unknown directive: " <> err) ("Unknown file directive" `isInfixOf` err)
          Right _ -> assertFailure "expected parse failure for unknown directive"

    , testCase "requires directive blocks to close" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on"
              , "        println(\"inside\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertBool ("error should mention missing closing brace: " <> err) ("Unclosed directive block" `isInfixOf` err)
          Right _ -> assertFailure "expected parse failure for unterminated directive block"

    -- Additional test cases
    , testCase "parses complex function signatures" $ do
        let source = unlines
              [ "package main"
              , "func complex<T, R>(x T, y func(T) R, z ...R) (R, error) {"
              , "    return y(x), nil"
              , "}"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse complex function signatures" (not $ null $ tfBlocks typusFile)

    , testCase "handles interface definitions" $ do
        let source = unlines
              [ "package main"
              , "type Writer interface {"
              , "    Write([]byte) (int, error)"
              , "    Close() error"
              , "}"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse interface definitions" (not $ null $ tfBlocks typusFile)

    , testCase "parses struct definitions with multiple fields" $ do
        let source = unlines
              [ "package main"
              , "type Person struct {"
              , "    name string"
              , "    age int"
              , "    address string"
              , "}"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse struct definitions" (not $ null $ tfBlocks typusFile)

    , testCase "handles method definitions" $ do
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
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse method definitions" (not $ null $ tfBlocks typusFile)

    , testCase "parses channel operations" $ do
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
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse channel operations" (not $ null $ tfBlocks typusFile)

    , testCase "handles go statements and goroutines" $ do
        let source = unlines
              [ "package main"
              , "func worker() {"
              , "    println(\"working\")"
              , "}"
              , "func main() {"
              , "    go worker()"
              , "    go func() {"
              , "        println(\"anonymous goroutine\")"
              , "    }()"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse goroutines" (not $ null $ tfBlocks typusFile)

    , testCase "parses defer statements" $ do
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
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse defer statements" (not $ null $ tfBlocks typusFile)

    , testCase "handles complex literals" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    numbers := []int{1, 2, 3, 4, 5}"
              , "    mapping := map[string]int{\"a\": 1, \"b\": 2}"
              , "    point := struct {"
              , "        x, y int"
              , "    }{x: 10, y: 20}"
              , "    println(numbers, mapping, point)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse complex literals" (not $ null $ tfBlocks typusFile)

    , testCase "parses generic types and functions" $ do
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
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse generic types" (not $ null $ tfBlocks typusFile)

    , testCase "handles embedded structs" $ do
        let source = unlines
              [ "package main"
              , "type Base struct {"
              , "    id int"
              , "}"
              , "type Derived struct {"
              , "    Base"
              , "    name string"
              , "}"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse embedded structs" (not $ null $ tfBlocks typusFile)

    , testCase "parses import statements" $ do
        let source = unlines
              [ "package main"
              , "import \"fmt\""
              , "import ("
              , "    \"os\""
              , "    \"strings\""
              , "    mypkg \"github.com/example/mypkg\""
              , ")"
              , "func main() {"
              , "    fmt.Println(\"hello\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse import statements" (not $ null $ tfBlocks typusFile)

    , testCase "handles multiple package declarations" $ do
        let source = unlines
              [ "package main"
              , "package secondary"  -- This should cause an error
              , "func main() {}"
              ]
        case parseTypus source of
          Left _ -> return ()  -- Expected to fail
          Right _ -> assertFailure "expected parsing to fail with multiple package declarations"

    , testCase "parses constants and variables" $ do
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
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse constants and variables" (not $ null $ tfBlocks typusFile)
    ]
