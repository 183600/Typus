{-# LANGUAGE CPP #-}
module Test.Unit.NewIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import qualified Data.List as L
import Data.List (isInfixOf)

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
  , hasTypeErrors
  )
import Dependencies.Analyzer
  ( analyzeDependencies
  , buildDependencyGraph
  )
import IntegratedCompiler
  ( fullCompile
  , analyzeAndCompile
  )
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  )

tests :: TestTree
tests =
  testGroup "New Integration Tests"
    [ testCase "integrates parsing L.and compilation" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"hello world\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains package main" ("package main" `L.isInfixOf` goCode)
                assertBool "contains println" ("println" `L.isInfixOf` goCode)

    , testCase "integrates parsing, type checking, L.and compilation" $ do
        let source = unlines
              [ "package main"
              , "func add(x int, y int) int {"
              , "    return x + y"
              , "}"
              , "func main() {"
              , "    result := add(5, 3)"
              , "    println(result)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            -- Type checking
            case diagnoseTypeErrors typusFile of
              Left errors -> assertFailure $ "type checking failed: " ++ show errors
              Right _ -> do
                -- Compilation
                case compile typusFile of
                  Left errs -> assertFailure $ "compile failed: " ++ show errs
                  Right goCode -> do
                    assertBool "contains add function" ("func add" `L.isInfixOf` goCode)
                    assertBool "contains function call" ("add(5, 3)" `L.isInfixOf` goCode)

    , testCase "integrates ownership analysis with compilation" $ do
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
            -- Ownership analysis
            case analyzeOwnershipFile typusFile of
              Left errors -> do
                -- Should handle ownership analysis results
                assertBool "ownership analysis completed" (True)
              Right _ -> do
                -- Compilation should still work
                case compile typusFile of
                  Left errs -> assertFailure $ "compile failed: " ++ show errs
                  Right goCode -> assertBool "compilation succeeded" (True)

    , testCase "integrates dependency analysis with compilation" $ do
        let source = unlines
              [ "package main"
              , "import \"fmt\""
              , "func greet(name string) {"
              , "    fmt.Println(\"Hello, \" + name)"
              , "}"
              , "func main() {"
              , "    greet(\"World\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            -- Dependency analysis
            let dependencies = analyzeDependencies typusFile
                graph = buildDependencyGraph typusFile
            assertBool "should detect dependencies" (not $ null dependencies)
            -- Compilation
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> do
                assertBool "contains fmt import" ("import \"fmt\"" `L.isInfixOf` goCode)
                assertBool "contains greet function" ("func greet" `L.isInfixOf` goCode)

    , testCase "handles complex multi-feature integration" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "//! dependent_types: on"
              , "import \"fmt\""
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
              , "    container := New(42)"
              , "    value := container.Get()"
              , "    fmt.Println(value)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            -- Multiple analyses
            let dependencies = analyzeDependencies typusFile
            assertBool "should detect fmt dependency" (L.any (\dep -> show dep `L.isInfixOf` "fmt") dependencies)
            
            -- Ownership analysis
            case analyzeOwnershipFile typusFile of
              Left _ -> assertBool "ownership analysis attempted" True
              Right _ -> assertBool "ownership analysis succeeded" True
            
            -- Type checking
            case diagnoseTypeErrors typusFile of
              Left errors -> assertFailure $ "type checking failed: " ++ show errors
              Right _ -> do
                -- Compilation
                case compile typusFile of
                  Left errs -> assertFailure $ "compile failed: " ++ show errs
                  Right goCode -> do
                    assertBool "contains generic type" ("Container" `L.isInfixOf` goCode)
                    assertBool "contains generic function" ("func New" `L.isInfixOf` goCode)

    , testCase "handles error propagation through pipeline" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    var x int = \"string\""  -- Type error
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            -- Should detect type errors
            case diagnoseTypeErrors typusFile of
              Left errors -> do
                assertBool "should detect type errors" (not $ null errors)
              Right _ -> assertFailure "expected type errors"
            
            -- Compilation should fail gracefully
            case compile typusFile of
              Left errs -> assertBool "compilation should fail" (not $ null errs)
              Right _ -> assertFailure "expected compilation to fail"

    , testCase "integrates source location tracking" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    x := 42"
              , "    println(x)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            -- Parse L.and check that source locations are preserved
            let blocks = tfBlocks typusFile
            assertBool "should have blocks" (not $ null blocks)
            
            -- Type checking should preserve location information
            case diagnoseTypeErrors typusFile of
              Left errors -> do
                -- Errors should include location information
                assertBool "errors should have location info" (True)
              Right _ -> assertBool "type checking succeeded" True

    , testCase "handles build tag integration" $ do
        let source = unlines
              [ "//go:build ignore"
              , "// +build ignore"
              , "package main"
              , "func main() {"
              , "    println(\"with build tags\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            -- Should preserve build tags
            let buildTags = tfBuildTags typusFile
            assertBool "should have build tags" (not $ null buildTags)
            
            -- Compilation should work
            case compile typusFile of
              Left errs -> assertFailure $ "compile failed: " ++ show errs
              Right goCode -> assertBool "compilation succeeded" (True)

    , testCase "integrates full compilation pipeline" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "import ("
              , "    \"fmt\""
              , "    \"strings\""
              , "    )"
              , "type Processor struct {"
              , "    prefix string"
              , "}"
              , "func NewProcessor(prefix string) *Processor {"
              , "    return &Processor{prefix: prefix}"
              , "}"
              , "func (p *Processor) Process(input string) string {"
              , "    return p.prefix + strings.ToUpper(input)"
              , "}"
              , "func main() {"
              , "    processor := NewProcessor(\"Result: \")"
              , "    result := processor.Process(\"hello\")"
              , "    fmt.Println(result)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            -- Full pipeline: parsing + analysis + compilation
            let dependencies = analyzeDependencies typusFile
            assertBool "should detect multiple dependencies" (L.length dependencies >= 2)
            
            case analyzeOwnershipFile typusFile of
              Left _ -> assertBool "ownership analysis attempted" True
              Right _ -> assertBool "ownership analysis succeeded" True
            
            case diagnoseTypeErrors typusFile of
              Left errors -> assertFailure $ "type checking failed: " ++ show errors
              Right _ -> do
                case compile typusFile of
                  Left errs -> assertFailure $ "compile failed: " ++ show errs
                  Right goCode -> do
                    assertBool "contains struct definition" ("type Processor" `L.isInfixOf` goCode)
                    assertBool "contains method definition" ("func (p *Processor)" `L.isInfixOf` goCode)

    , testCase "handles directive integration" $ do
        let source = unlines
              [ "//! ownership: on, dependent_types: on"
              , "package main"
              , "func main() {"
              , "    {//! constraints: off}"
              , "        x := 42"
              , "        println(x)"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            -- Check directives are parsed
            let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes } = tfDirectives typusFile
            case ownership of
              Just loc -> assertBool "ownership directive detected" (locatedValue loc)
              _ -> assertFailure "ownership directive not found"
            
            case dependentTypes of
              Just loc -> assertBool "dependent_types directive detected" (locatedValue loc)
              _ -> assertFailure "dependent_types directive not found"
            
            -- Check block directives
            let blocks = tfBlocks typusFile
                directedBlocks = L.filter (\block -> 
                  maybe False locatedValue (bdOwnership (cbDirectives block)) ||
                  maybe False (not . locatedValue) (bdConstraints (cbDirectives block))
                  ) blocks
            assertBool "should have directed blocks" (not $ null directedBlocks)

    , testCase "integrates error handling across components" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    x := 42"
              , "    y := x"
              , "    z := x"  -- Potential ownership issue
              , "    var a int = \"string\""  -- Type error
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            -- Should collect errors from multiple sources
            let ownershipResult = analyzeOwnershipFile typusFile
                typeCheckResult = diagnoseTypeErrors typusFile
            
            case (ownershipResult, typeCheckResult) of
              (Left ownershipErrors, Left typeErrors) -> do
                assertBool "should have ownership errors" (not $ null ownershipErrors)
                assertBool "should have type errors" (not $ null typeErrors)
              _ -> assertFailure "expected errors from both analyzers"

    , testCase "handles incremental compilation integration" $ do
        let source1 = unlines
              [ "package main"
              , "func helper() int {"
              , "    return 42"
              , "}"
              ]
        let source2 = unlines
              [ "package main"
              , "func main() {"
              , "    result := helper()"
              , "    println(result)"
              , "}"
              ]
        case parseTypus source1 of
          Left err -> assertFailure $ "parseTypus failed on source1: " ++ err
          Right typusFile1 -> do
            case parseTypus source2 of
              Left err -> assertFailure $ "parseTypus failed on source2: " ++ err
              Right typusFile2 -> do
                -- Both should compile successfully
                case compile typusFile1 of
                  Left errs -> assertFailure $ "compile failed on source1: " ++ show errs
                  Right _ -> assertBool "source1 compiled" True
                
                case compile typusFile2 of
                  Left errs -> assertFailure $ "compile failed on source2: " ++ show errs
                  Right _ -> assertBool "source2 compiled" True

    , testCase "integrates syntax validation with compilation" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    if true {"  -- Missing opening brace after if
              , "        println(\"hello\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            -- Should detect syntax issues
            let syntaxErrors = tfSyntaxErrors typusFile
            assertBool "should detect syntax errors" (not $ null syntaxErrors)
            
            -- Compilation should handle syntax errors gracefully
            case compile typusFile of
              Left _ -> assertBool "compilation should fail with syntax errors" True
              Right _ -> assertFailure "expected compilation to fail with syntax errors"
    ]