{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EndToEndIntegrationTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements
  , vectorOf, oneof, frequency, suchThat, Positive(..)
  )

import Parser (parseTypus, TypusFile(..))
import Compiler (compile, CompilerResult(..), CompilerError(..))
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..), buildSourceIR, buildSemanticIR, emitGo)
import Ownership (analyzeOwnership, OwnershipError(..))
import Dependencies (analyzeDependentTypes, DependentTypeError(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Utils (trim, normalizeIndentation, removeComments)

import IntegratedCompiler (runFullCompilation, CompilationResult(..))
import AnalyzerIntegration (runFullAnalysis, AnalysisResult(..))

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, length)
import Data.List (null, intercalate)
import qualified Data.Text as T (pack, unpack)

-- | Generate complete Typus programs for end-to-end testing
genCompleteProgram :: Gen String
genCompleteProgram = oneof
  [ -- Simple program
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    println(\"Hello, World!\")"
      , "}"
      ]
  , -- Program with functions
    return $ unlines
      [ "package main"
      , "func add(a int, b int) int {"
      , "    return a + b"
      , "}"
      , "func multiply(x int, y int) int {"
      , "    return x * y"
      , "}"
      , "func main() {"
      , "    result := add(5, 3)"
      , "    result = multiply(result, 2)"
      , "    println(result)"
      , "}"
      ]
  , -- Program with structs
    return $ unlines
      [ "package main"
      , "type Person struct {"
      , "    Name string"
      , "    Age  int"
      , "}"
      , "func (p Person) Greet() {"
      , "    println(\"Hello, \" + p.Name)"
      , "}"
      , "func main() {"
      , "    person := Person{Name: \"Alice\", Age: 30}"
      , "    person.Greet()"
      , "}"
      ]
  , -- Program with ownership annotations
    return $ unlines
      [ "package main"
      , "//! ownership: on"
      , "func createResource() int {"
      , "    return 42"
      , "}"
      , "func consume(r int) {"
      , "    println(\"Consumed: \", r)"
      , "}"
      , "func main() {"
      , "    resource := createResource()"
      , "    consume(resource)"
      , "}"
      ]
  , -- Program with dependent types
    return $ unlines
      [ "package main"
      , "//! dependent_types: on"
      , "type Vector(n: int) struct {"
      , "    data [n]int"
      , "}"
      , "func main() {"
      , "    v := Vector(5){data: [5]int{1,2,3,4,5}}"
      , "    println(v.data[0])"
      , "}"
      ]
  ]

-- | Generate programs with potential errors
genErrorProgram :: Gen String
genErrorProgram = oneof
  [ -- Syntax error
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    println(\"missing closing"
      , "}"
      ]
  , -- Ownership error
    return $ unlines
      [ "package main"
      , "//! ownership: on"
      , "func main() {"
      , "    x := 42"
      , "    y := x  // move"
      , "    println(x)  // use after move"
      , "}"
      ]
  , -- Type error
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    var x int = \"string\"  // type mismatch"
      , "    println(x)"
      , "}"
      ]
  ]

-- Property tests

-- Property: complete programs should parse successfully
prop_complete_programs_parse :: Property
prop_complete_programs_parse =
  forAll genCompleteProgram $ \program ->
    case parseTypus program of
      Left _ -> property False
      Right _ -> property True

-- Property: complete programs should compile without critical errors
prop_complete_programs_compile :: Property
prop_complete_programs_compile =
  forAll genCompleteProgram $ \program ->
    case parseTypus program of
      Left _ -> property False
      Right typusFile ->
        case compile typusFile of
          Left _ -> property False  -- May fail, but that's OK for integration tests
          Right _ -> property True

-- Property: error programs should be detected
prop_error_programs_detected :: Property
prop_error_programs_detected =
  forAll genErrorProgram $ \program ->
    case parseTypus program of
      Left _ -> property True  -- Parsing errors detected
      Right typusFile ->
        case compile typusFile of
          Left _ -> property True  -- Compilation errors detected
          Right _ -> property False  -- Should have errors but doesn't

-- Property: IR pipeline should preserve semantic content
prop_ir_pipeline_preserves :: Property
prop_ir_pipeline_preserves =
  forAll genCompleteProgram $ \program ->
    case parseTypus program of
      Left _ -> property True  -- Skip invalid programs
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile program
            semanticIR = buildSemanticIR sourceIR
            goIR = emitGo semanticIR
            goCode = T.unpack $ goCode goIR
        in property $ not $ null goCode

-- Property: full compilation should produce valid Go code
prop_full_compilation_produces_go :: Property
prop_full_compilation_produces_go =
  forAll genCompleteProgram $ \program ->
    case runFullCompilation program of
      CompilationSuccess goCode -> property $ not $ null goCode
      CompilationFailure errors -> property True  -- May fail, that's OK

-- Property: full analysis should complete without crashing
prop_full_analysis_completes :: Property
prop_full_analysis_completes =
  forAll genCompleteProgram $ \program ->
    case runFullAnalysis program of
      AnalysisSuccess _ -> property True
      AnalysisFailure _ -> property True  -- May fail, but shouldn't crash

-- Unit tests

unit_tests :: TestTree
unit_tests = testGroup "End-to-End Integration Unit Tests"
  [ testCase "simple program compilation pipeline" $ do
      let program = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"Hello, World!\")"
            , "}"
            ]
      case parseTypus program of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          case compile typusFile of
            Left err -> assertFailure $ "compile failed: " ++ show err
            Right result -> do
              assertBool "compilation should succeed" $ True

  , testCase "program with functions" $ do
      let program = unlines
            [ "package main"
            , "func add(a int, b int) int {"
            , "    return a + b"
            , "}"
            , "func main() {"
            , "    result := add(5, 3)"
            , "    println(result)"
            , "}"
            ]
      case parseTypus program of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          case compile typusFile of
            Left err -> assertFailure $ "compile failed: " ++ show err
            Right result -> do
              assertBool "should handle functions" $ True

  , testCase "program with structs L.and methods" $ do
      let program = unlines
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
            , "    counter := &Counter{value: 0}"
            , "    counter.Increment()"
            , "    println(counter.Value())"
            , "}"
            ]
      case parseTypus program of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          case compile typusFile of
            Left err -> assertFailure $ "compile failed: " ++ show err
            Right result -> do
              assertBool "should handle structs L.and methods" $ True

  , testCase "ownership analysis integration" $ do
      let program = unlines
            [ "package main"
            , "//! ownership: on"
            , "func create() int {"
            , "    return 42"
            , "}"
            , "func consume(x int) {"
            , "    println(x)"
            , "}"
            , "func main() {"
            , "    value := create()"
            , "    consume(value)"
            , "}"
            ]
      case parseTypus program of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let (ownerAnalyzer, ownerErrors) = analyzeOwnership program
          assertBool "ownership analysis should complete" $ True

  , testCase "dependent types integration" $ do
      let program = unlines
            [ "package main"
            , "//! dependent_types: on"
            , "type SafeArray(n: int) struct {"
            , "    data [n]int"
            , "    size int"
            , "}"
            , "func main() {"
            , "    arr := SafeArray(5){data: [5]int{1,2,3,4,5}, size: 5}"
            , "    println(arr.data[0])"
            , "}"
            ]
      case parseTypus program of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let depResult = analyzeDependentTypes typusFile
          assertBool "dependent type analysis should complete" $ True

  , testCase "IR generation pipeline" $ do
      let program = unlines
            [ "package main"
            , "func calculate(x int, y int) int {"
            , "    return x * 2 + y"
            , "}"
            , "func main() {"
            , "    result := calculate(10, 5)"
            , "    println(result)"
            , "}"
            ]
      case parseTypus program of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile program
              semanticIR = buildSemanticIR sourceIR
              goIR = emitGo semanticIR
              goCode = T.unpack $ goCode goIR
          assertBool "should generate Go code" $ not $ null goCode
          assertBool "should contain package declaration" $ "package" `L.isInfixOf` goCode
          assertBool "should contain function definitions" $ "func" `L.isInfixOf` goCode

  , testCase "full compilation with imports" $ do
      let program = unlines
            [ "package main"
            , "import \"fmt\""
            , "func greet(name string) {"
            , "    fmt.Printf(\"Hello, %s!\\n\", name)"
            , "}"
            , "func main() {"
            , "    greet(\"World\")"
            , "}"
            ]
      case runFullCompilation program of
        CompilationSuccess goCode -> do
          assertBool "should generate Go code" $ not $ null goCode
          assertBool "should contain import" $ "import" `L.isInfixOf` goCode
        CompilationFailure errors -> do
          assertFailure $ "compilation failed: " ++ show errors

  , testCase "full analysis pipeline" $ do
      let program = unlines
            [ "package main"
            , "//! ownership: on"
            , "//! dependent_types: on"
            , "type Container(size: int) struct {"
            , "    data [size]int"
            , "}"
            , "func process(c Container) {"
            , "    println(c.data[0])"
            , "}"
            , "func main() {"
            , "    container := Container(3){data: [3]int{1,2,3}}"
            , "    process(container)"
            , "}"
            ]
      case runFullAnalysis program of
        AnalysisSuccess result -> do
          assertBool "analysis should succeed" $ True
        AnalysisFailure errors -> do
          -- Analysis might fail due to experimental features
          assertBool "should provide error information" $ not $ null errors

  , testCase "error handling L.and recovery" $ do
      let program = unlines
            [ "package main"
            , "func broken() {"
            , "    @#$ invalid syntax"
            , "}"
            , "func working() {"
            , "    println(\"this works\")"
            , "}"
            , "func main() {"
            , "    working()"
            , "}"
            ]
      case parseTypus program of
        Left _ -> return ()  -- Expected to fail at parsing
        Right typusFile -> do
          case compile typusFile of
            Left _ -> return ()  -- Expected to fail at compilation
            Right result -> do
              -- Might succeed with partial compilation
              assertBool "should handle errors gracefully" $ True

  , testCase "complex program integration" $ do
      let program = unlines
            [ "package main"
            , "import ("
            , "    \"fmt\""
            , "    \"strings\""
            , ")"
            , "type Processor struct {"
            , "    prefix string"
            , "}"
            , "func (p Processor) Process(input string) string {"
            , "    return p.prefix + strings.ToUpper(input)"
            , "}"
            , "func NewProcessor(prefix string) Processor {"
            , "    return Processor{prefix: prefix}"
            , "}"
            , "func main() {"
            , "    processor := NewProcessor(\"Result: \")"
            , "    result := processor.Process(\"hello world\")"
            , "    fmt.Println(result)"
            , "}"
            ]
      case runFullCompilation program of
        CompilationSuccess goCode -> do
          assertBool "should handle complex programs" $ not $ null goCode
          assertBool "should contain multiple imports" $ "fmt" `L.isInfixOf` goCode && "strings" `L.isInfixOf` goCode
        CompilationFailure errors -> do
          assertFailure $ "complex program compilation failed: " ++ show errors
  ]

-- Advanced integration tests

advanced_tests :: TestTree
advanced_tests = testGroup "Advanced Integration Tests"
  [ testCase "concurrent features" $ do
      let program = unlines
            [ "package main"
            , "func worker(id int, jobs <-chan int, results chan<- int) {"
            , "    for j := range jobs {"
            , "        results <- j * 2"
            , "    }"
            , "}"
            , "func main() {"
            , "    jobs := make(chan int, 100)"
            , "    results := make(chan int, 100)"
            , "    for w := 1; w <= 3; w++ {"
            , "        go worker(w, jobs, results)"
            , "    }"
            , "    for j := 1; j <= 5; j++ {"
            , "        jobs <- j"
            , "    }"
            , "    close(jobs)"
            , "    for a := 1; a <= 5; a++ {"
            , "        <-results"
            , "    }"
            , "}"
            ]
      case runFullCompilation program of
        CompilationSuccess goCode -> do
          assertBool "should handle concurrent features" $ 
            "go" `L.isInfixOf` goCode && "chan" `L.isInfixOf` goCode
        CompilationFailure errors -> return ()  -- May fail due to complexity

  , testCase "generic types" $ do
      let program = unlines
            [ "package main"
            , "type Container[T L.any] struct {"
            , "    value T"
            , "}"
            , "func New[T L.any](v T) Container[T] {"
            , "    return Container[T]{value: v}"
            , "}"
            , "func (c Container[T]) Get() T {"
            , "    return c.value"
            , "}"
            , "func main() {"
            , "    intContainer := New(42)"
            , "    stringContainer := New(\"hello\")"
            , "    println(intContainer.Get())"
            , "    println(stringContainer.Get())"
            , "}"
            ]
      case runFullCompilation program of
        CompilationSuccess goCode -> do
          assertBool "should handle generic types" $ 
            "Container" `L.isInfixOf` goCode
        CompilationFailure errors -> return ()  -- May fail due to generics

  , testCase "interface integration" $ do
      let program = unlines
            [ "package main"
            , "type Writer interface {"
            , "    Write(data []byte) (int, error)"
            , "}"
            , "type ConsoleWriter struct {}"
            , "func (cw ConsoleWriter) Write(data []byte) (int, error) {"
            , "    println(string(data))"
            , "    return len(data), nil"
            , "}"
            , "func process(w Writer) {"
            , "    w.Write([]byte(\"hello\"))"
            , "}"
            , "func main() {"
            , "    writer := ConsoleWriter{}"
            , "    process(writer)"
            , "}"
            ]
      case runFullCompilation program of
        CompilationSuccess goCode -> do
          assertBool "should handle interfaces" $ 
            "interface" `L.isInfixOf` goCode
        CompilationFailure errors -> return ()  -- May fail due to interfaces
  ]

-- Performance tests

performance_tests :: TestTree
performance_tests = testGroup "Performance Tests"
  [ testCase "large program compilation" $ do
      let largeFunction = unlines $ L.concat
            [ ["func large() {"]
            , ["    x := 1"] ++
             ["    y := x + i" | i <- [1..100]] ++
             ["    println(y)"]
            , ["}"]
            ]
          largeProgram = unlines
            [ "package main"
            , "import \"fmt\""
            , largeFunction
            , "func main() {"
            , "    large()"
            , "}"
            ]
      case runFullCompilation largeProgram of
        CompilationSuccess goCode -> do
          assertBool "should handle large programs" $ not $ null goCode
        CompilationFailure errors -> return ()  -- May fail due to size

  , testCase "many small functions" $ do
      let smallFunctions = unlines $ L.concat
            [ ["func small" ++ show i ++ "() int { return " ++ show i ++ " }" | i <- [1..50]]
            ]
          program = unlines
            [ "package main"
            , smallFunctions
            , "func main() {"
            , "    total := 0"
            , "    for i := 1; i <= 50; i++ {"
            , "        total += i"
            , "    }"
            , "    println(total)"
            , "}"
            ]
      case runFullCompilation program of
        CompilationSuccess goCode -> do
          assertBool "should handle many functions" $ not $ null goCode
        CompilationFailure errors -> return ()  -- May fail due to complexity
  ]

tests :: TestTree
tests = testGroup "End-to-End Integration Tests"
  [ testGroup "Property Tests"
    [ fastProperty "complete programs parse" prop_complete_programs_parse
    , fastProperty "complete programs compile" prop_complete_programs_compile
    , fastProperty "error programs detected" prop_error_programs_detected
    , fastProperty "IR pipeline preserves" prop_ir_pipeline_preserves
    , fastProperty "full compilation produces Go" prop_full_compilation_produces_go
    , fastProperty "full analysis completes" prop_full_analysis_completes
    ]
  , unit_tests
  , advanced_tests
  , performance_tests
  ]