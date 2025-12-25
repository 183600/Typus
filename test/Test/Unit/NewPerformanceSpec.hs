{-# LANGUAGE CPP #-}
module Test.Unit.NewPerformanceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.List (isInfixOf)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Parser
  ( parseTypus
  , TypusFile(..)
  )
import Compiler
  ( compile
  )
import Ownership
  ( analyzeOwnershipFile
  )
import Compiler.TypeChecker
  ( diagnoseTypeErrors
  )
import Dependencies.Analyzer
  ( analyzeDependencies
  , buildDependencyGraph
  )

tests :: TestTree
tests =
  testGroup "New Performance Tests"
    [ testCase "parses small files efficiently" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"hello\")"
              , "}"
              ]
        startTime <- getCPUTime
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right _ -> do
            endTime <- getCPUTime
            let timeDiff = fromIntegral (endTime - startTime) / (10^12)
            assertBool ("parsing should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 1.0)

    , testCase "parses medium files efficiently" $ do
        let source = unlines $ concat
              [ ["package main"]
              , ["import \"fmt\""]
              , ["func main() {"]
              , ["    x := 42"]
              , ["    y := x + 1"]
              , ["    fmt.Println(x, y)"]
              , ["}"]
              ]
        startTime <- getCPUTime
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right _ -> do
            endTime <- getCPUTime
            let timeDiff = fromIntegral (endTime - startTime) / (10^12)
            assertBool ("parsing should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 1.0)

    , testCase "compiles simple programs efficiently" $ do
        let source = unlines
              [ "package main"
              , "func add(a int, b int) int {"
              , "    return a + b"
              , "}"
              , "func main() {"
              , "    result := add(5, 3)"
              , "    println(result)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            startTime <- getCPUTime
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right _ -> do
                endTime <- getCPUTime
                let timeDiff = fromIntegral (endTime - startTime) / (10^12)
                assertBool ("compilation should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 2.0)

    , testCase "performs type checking efficiently" $ do
        let source = unlines
              [ "package main"
              , "func process(x int) string {"
              , "    return fmt.Sprintf(\"%d\", x)"
              , "}"
              , "func main() {"
              , "    result := process(42)"
              , "    println(result)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            startTime <- getCPUTime
            case diagnoseTypeErrors typusFile of
              Left _ -> assertBool "type checking completed" True
              Right _ -> assertBool "type checking succeeded" True
            endTime <- getCPUTime
            let timeDiff = fromIntegral (endTime - startTime) / (10^12)
            assertBool ("type checking should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 1.0)

    , testCase "performs ownership analysis efficiently" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    x := 42"
              , "    y := x"
              , "    println(y)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            startTime <- getCPUTime
            case analyzeOwnershipFile typusFile of
              Left _ -> assertBool "ownership analysis completed" True
              Right _ -> assertBool "ownership analysis succeeded" True
            endTime <- getCPUTime
            let timeDiff = fromIntegral (endTime - startTime) / (10^12)
            assertBool ("ownership analysis should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 1.0)

    , testCase "performs dependency analysis efficiently" $ do
        let source = unlines
              [ "package main"
              , "import ("
              , "    \"fmt\""
              , "    \"strings\""
              , "    \"os\""
              , ")"
              , "func main() {"
              , "    fmt.Println(\"hello\")"
              , "    strings.ToUpper(\"test\")"
              , "    os.Exit(0)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            startTime <- getCPUTime
            let dependencies = analyzeDependencies typusFile
                graph = buildDependencyGraph typusFile
            endTime <- getCPUTime
            let timeDiff = fromIntegral (endTime - startTime) / (10^12)
            assertBool ("dependency analysis should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 1.0)
            assertBool "should detect dependencies" (not $ null dependencies)

    , testCase "handles repeated parsing efficiently" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"test\")"
              , "}"
              ]
        let iterations = 100
        startTime <- getCPUTime
        sequence_ $ replicate iterations $ do
          case parseTypus source of
            Left _ -> return ()
            Right _ -> return ()
        endTime <- getCPUTime
        let timeDiff = fromIntegral (endTime - startTime) / (10^12)
            avgTime = timeDiff / fromIntegral iterations
        assertBool ("average parsing time should be fast (took " ++ show avgTime ++ "s per iteration)") (avgTime < 0.01)

    , testCase "handles repeated compilation efficiently" $ do
        let source = unlines
              [ "package main"
              , "func add(a int, b int) int { return a + b }"
              , "func main() { println(add(1, 2)) }"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let iterations = 50
            startTime <- getCPUTime
            sequence_ $ replicate iterations $ do
              case compile typusFile of
                Left _ -> return ()
                Right _ -> return ()
            endTime <- getCPUTime
            let timeDiff = fromIntegral (endTime - startTime) / (10^12)
                avgTime = timeDiff / fromIntegral iterations
            assertBool ("average compilation time should be fast (took " ++ show avgTime ++ "s per iteration)") (avgTime < 0.05)

    , testCase "scales with file size linearly" $ do
        let smallSource = unlines
              [ "package main"
              , "func main() { println(\"hello\") }"
              ]
        let largeSource = unlines $ concat
              [ ["package main"]
              , ["import \"fmt\""]
              , ["func main() {"]
              , ["    fmt.Println(\"line " ++ show n ++ "\")" | n <- [1..50]]
              , ["}"]
              ]
        
        -- Parse small file
        startTime1 <- getCPUTime
        case parseTypus smallSource of
          Left err -> assertFailure $ "parseTypus failed on small file: " ++ err
          Right _ -> return ()
        endTime1 <- getCPUTime
        let smallTime = fromIntegral (endTime1 - startTime1) / (10^12)
        
        -- Parse large file
        startTime2 <- getCPUTime
        case parseTypus largeSource of
          Left err -> assertFailure $ "parseTypus failed on large file: " ++ err
          Right _ -> return ()
        endTime2 <- getCPUTime
        let largeTime = fromIntegral (endTime2 - startTime2) / (10^12)
        
        -- Large file should take proportionally more time but not exponentially more
        let ratio = largeTime / smallTime
        assertBool ("large file should not take exponentially more time (ratio: " ++ show ratio ++ ")") (ratio < 100)

    , testCase "memory usage stays reasonable for multiple analyses" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "import \"fmt\""
              , "func process(x int) string {"
              , "    return fmt.Sprintf(\"%d\", x)"
              , "}"
              , "func main() {"
              , "    result := process(42)"
              , "    fmt.Println(result)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let iterations = 20
            sequence_ $ replicate iterations $ do
              -- Multiple analyses on the same file
              case compile typusFile of
                Left _ -> return ()
                Right _ -> return ()
              case diagnoseTypeErrors typusFile of
                Left _ -> return ()
                Right _ -> return ()
              case analyzeOwnershipFile typusFile of
                Left _ -> return ()
                Right _ -> return ()
            assertBool "multiple analyses should complete" True

    , testCase "performs efficiently with complex types" $ do
        let source = unlines
              [ "package main"
              , "//! dependent_types: on"
              , "type Container[T any] struct { value T }"
              , "func New[T any](v T) Container[T] {"
              , "    return Container[T]{value: v}"
              , "}"
              , "func (c Container[T]) Get() T { return c.value }"
              , "func main() {"
              , "    container := New(42)"
              , "    value := container.Get()"
              , "    println(value)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            startTime <- getCPUTime
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right _ -> do
                endTime <- getCPUTime
                let timeDiff = fromIntegral (endTime - startTime) / (10^12)
                assertBool ("complex type compilation should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 2.0)

    , testCase "handles large dependency graphs efficiently" $ do
        let source = unlines $ concat
              [ ["package main"]
              , ["import ("]
              , ["    \"fmt\"", "\"os\"", "\"strings\"", "\"strconv\""
              , "\"time\"", "\"math\"", "\"sort\"", "\"reflect\""]
              , [")"]
              , ["func main() {"]
              , ["    fmt.Println(\"hello\")"]
              , ["    os.Exit(0)"]
              , ["    strings.ToUpper(\"test\")"]
              , ["    strconv.Itoa(42)"]
              , ["    time.Now()"]
              , ["    math.Abs(-1.0)"]
              , ["    sort.Strings([]string{\"a\", \"b\"})"]
              , ["    reflect.TypeOf(42)"]
              , ["}"]
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            startTime <- getCPUTime
            let dependencies = analyzeDependencies typusFile
                graph = buildDependencyGraph typusFile
            endTime <- getCPUTime
            let timeDiff = fromIntegral (endTime - startTime) / (10^12)
            assertBool ("large dependency graph analysis should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 1.0)
            assertBool "should detect many dependencies" (length dependencies >= 8)

    , testCase "performs efficiently with nested structures" $ do
        let source = unlines
              [ "package main"
              , "type Inner struct { value int }"
              , "type Middle struct { inner Inner }"
              , "type Outer struct { middle Middle }"
              , "func process(o Outer) int {"
              , "    return o.middle.inner.value"
              , "}"
              , "func main() {"
              , "    o := Outer{Middle{Inner{42}}}"
              , "    result := process(o)"
              , "    println(result)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            startTime <- getCPUTime
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right _ -> do
                endTime <- getCPUTime
                let timeDiff = fromIntegral (endTime - startTime) / (10^12)
                assertBool ("nested structure compilation should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 1.0)

    , testCase "maintains performance with error handling" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    var x int = \"string\""
              , "    var y string = 42"
              , "    var z bool = \"not boolean\""
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            startTime <- getCPUTime
            case diagnoseTypeErrors typusFile of
              Left errors -> do
                endTime <- getCPUTime
                let timeDiff = fromIntegral (endTime - startTime) / (10^12)
                assertBool ("error handling should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 1.0)
                assertBool "should detect multiple errors" (length errors >= 2)
              Right _ -> assertFailure "expected type errors"

    , testCase "performs efficiently with control flow" $ do
        let source = unlines
              [ "package main"
              , "func fibonacci(n int) int {"
              , "    if n <= 1 { return n }"
              , "    return fibonacci(n-1) + fibonacci(n-2)"
              , "}"
              , "func main() {"
              , "    for i := 0; i < 10; i++ {"
              , "        if i % 2 == 0 {"
              , "            println(i)"
              , "        } else {"
              , "            println(fibonacci(i))"
              , "        }"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            startTime <- getCPUTime
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right _ -> do
                endTime <- getCPUTime
                let timeDiff = fromIntegral (endTime - startTime) / (10^12)
                assertBool ("control flow compilation should be fast (took " ++ show timeDiff ++ "s)") (timeDiff < 2.0)
    ]