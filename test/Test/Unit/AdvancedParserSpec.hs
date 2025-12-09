module Test.Unit.AdvancedParserSpec (tests) where

import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import Parser
  ( CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
  )
import SourceLocation
  ( locatedValue
  )

tests :: TestTree
tests =
  testGroup "Advanced Parser"
    [ testCase "parses complex nested directives" $ do
        let source = unlines
              [ "//! ownership: on, dependent_types: on"
              , "package main"
              , "func main() {"
              , "    {//! ownership: off"
              , "        {//! dependent_types: off"
              , "            println(\"nested directives\")"
              , "        }"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes } = tfDirectives typusFile
            case ownership of
              Nothing -> assertFailure "expected ownership directive"
              Just loc -> locatedValue loc @?= True
            case dependentTypes of
              Nothing -> assertFailure "expected dependent types directive"
              Just loc -> locatedValue loc @?= True

    , testCase "handles malformed directive syntax gracefully" $ do
        let source = unlines
              [ "//! ownership: maybe"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertBool "should report directive parsing error" ("directive" `isInfixOf` err)
          Right _ -> assertFailure "expected parsing to fail"

    , testCase "parses complex type definitions" $ do
        let source = unlines
              [ "package main"
              , "type Complex<T, R> struct {"
              , "    field1: T"
              , "    field2: map[string]R"
              , "    field3: []func(T) R"
              , "}"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse complex types" (not $ null $ tfBlocks typusFile)

    , testCase "handles multiple build tags" $ do
        let source = unlines
              [ "//go:build linux && amd64"
              , "// +build linux,amd64"
              , "//go:build !windows"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right TypusFile { tfBuildTags = buildTags } -> do
            length buildTags @?= 3
            all (isInfixOf "go:build" . locatedValue) buildTags @?= True

    , testCase "parses deeply nested code blocks" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    if true {"
              , "        for {"
              , "            switch {"
              , "            case true:"
              , "                select {"
              , "                case <-chan:"
              , "                    println(\"deeply nested\")"
              , "                }"
              , "            }"
              , "        }"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should handle deeply nested blocks" (not $ null $ tfBlocks typusFile)

    , testCase "handles unicode and special characters" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    message := \"你好世界\""
              , "    println(message)"
              , "    emoji := \"🚀\""
              , "    println(emoji)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should handle unicode" (hasContent "你好世界" typusFile)
            assertBool "should handle emojis" (hasContent "🚀" typusFile)

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
            assertBool "should parse complex signatures" (hasContent "complex<T, R>" typusFile)

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
            assertBool "should parse interfaces" (hasContent "type Writer interface" typusFile)

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
            assertBool "should parse channels" (hasContent "make(chan" typusFile)

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
            assertBool "should parse goroutines" (hasContent "go worker" typusFile)

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
            assertBool "should parse defer" (hasContent "defer println" typusFile)

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
            assertBool "should parse complex literals" (hasContent "[]int{" typusFile)

    , testCase "parses method definitions" $ do
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
            assertBool "should parse methods" (hasContent "func (c *Counter)" typusFile)

    , testCase "handles error types and error handling" $ do
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
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            assertBool "should parse error handling" (hasContent "mightFail() error" typusFile)

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
            assertBool "should parse generics" (hasContent "Container[T any]" typusFile)

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
            assertBool "should parse embedded structs" (hasContent "Base" typusFile)
    ]

hasContent :: String -> TypusFile -> Bool
hasContent target typusFile = any (isInfixOf target . cbContent) (tfBlocks typusFile)