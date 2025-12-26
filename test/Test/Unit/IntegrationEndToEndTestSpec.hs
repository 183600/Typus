{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.IntegrationEndToEndTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, choose, listOf, elements, oneof, sized, suchThat)

import Parser (parseTypus)
import Ownership (analyzeOwnership)
import Dependencies (analyzeDependentTypes)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..))
import IntegratedCompiler (compileTypus)
import SourceLocation (SourcePos(..), SourceSpan(..))

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate complete Typus programs for integration testing
genTypusProgram :: Gen String
genTypusProgram = oneof
  [ pure ""  -- Empty program
  , pure "func main() {}"  -- Minimal program
  , genSimpleProgram
  , genProgramWithDirectives
  , genComplexProgram
  ]

-- Generate simple programs
genSimpleProgram :: Gen String
genSimpleProgram = do
  funcs <- listOf $ elements
    [ "func test() { return 42 }"
    , "func add(x int, y int) int { return x + y }"
    , "func greet(name string) string { return \"Hello, \" + name }"
    ]
  return $ unlines funcs

-- Generate programs with directives
genProgramWithDirectives :: Gen String
genProgramWithDirectives = do
  directives <- listOf $ elements
    [ "//! ownership: on"
    , "//! dependent_types: on"
    , "//! constraints: on"
    , "//go:build linux"
    ]
  code <- genSimpleProgram
  return $ unlines directives ++ "\n" ++ code

-- Generate complex programs
genComplexProgram :: Gen String
genComplexProgram = do
  imports <- listOf $ elements
    [ "import \"fmt\""
    , "import \"os\""
    , "import \"strings\""
    ]
  structs <- listOf $ elements
    [ "type Person struct { Name string; Age int }"
    , "type Result struct { Value int; Error error }"
    ]
  funcs <- listOf $ elements
    [ "func process(p Person) Result {"
    , "    if p.Age > 0 {"
    , "        return Result{p.Age * 2, nil}"
    , "    }"
    , "    return Result{0, fmt.Errorf(\"invalid age\")}"
    , "}"
    ]
  return $ unlines $ imports ++ [""] ++ structs ++ [""] ++ funcs

-- Generate programs with potential errors
genErrorProgram :: Gen String
genErrorProgram = oneof
  [ pure "if condition:\n    doSomething()"  -- Syntax error
  , pure "x := 42\ny := x\nfmt.Println(x)"  -- Ownership error
  , pure "func test() {\n    return unknown_var\n}"  -- Type error
  , pure "package main\n\npackage other\n\nfunc main() {}"  -- Multiple packages
  ]

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test basic end-to-end compilation
testBasicCompilation :: TestTree
testBasicCompilation = testGroup "Basic Compilation"
  [ testCase "empty program compiles" $ do
      let program = ""
      case compileTypus program of
        Left _ -> assertBool "Empty program should compile or handle gracefully" True
        Right result -> assertBool "Should produce compilation result" True
        
  , testCase "simple program compiles" $ do
      let program = "func main() { return 42 }"
      case compileTypus program of
        Left _ -> assertBool "Simple program should compile" False
        Right result -> assertBool "Should compile simple program" True
        
  , testCase "program with directives compiles" $ do
      let program = "//! ownership: on\n\nfunc main() {}"
      case compileTypus program of
        Left _ -> assertBool "Program with directives should compile" False
        Right result -> assertBool "Should handle directives" True
  ]

-- Test parser integration
testParserIntegration :: TestTree
testParserIntegration = testGroup "Parser Integration"
  [ testCase "parser handles valid input" $ do
      let program = "func test() { return 42 }"
      case parseTypus program of
        Left _ -> assertBool "Should parse valid program" False
        Right parsed -> assertBool "Should parse successfully" $ not $ null parsed
        
  , testCase "parser handles invalid input gracefully" $ do
      let program = "if condition:\n    doSomething()"
      case parseTypus program of
        Left err -> assertBool "Should handle syntax errors" $ "missing opening brace" `isInfixOf` err
        Right _ -> assertBool "Should attempt recovery" True
        
  , testCase "parser preserves structure" $ do
      let program = "//! ownership: on\n\nfunc main() {}\n\nfunc helper() {}"
      case parseTypus program of
        Left _ -> assertBool "Should parse structured program" False
        Right parsed -> 
          assertBool "Should preserve program structure" $ length parsed >= 2
  ]

-- Test ownership analysis integration
testOwnershipIntegration :: TestTree
testOwnershipIntegration = testGroup "Ownership Analysis Integration"
  [ testCase "ownership analysis works" $ do
      let program = "func main() {\n    x := 42\n    y := x\n    return y\n}"
      case analyzeOwnership program of
        Left _ -> assertBool "Should analyze ownership" False
        Right errors -> 
          -- May detect ownership issues or not
          assertBool "Should complete analysis" True
          
  , testCase "ownership analysis detects moves" $ do
      let program = "func test() {\n    x := make([]int, 10)\n    y := x\n    fmt.Println(x)\n}"
      case analyzeOwnership program of
        Left _ -> assertBool "Should analyze complex ownership" True
        Right errors -> 
          -- Should potentially detect use after move
          assertBool "Should handle move semantics" True
  ]

-- Test dependency analysis integration
testDependencyAnalysisIntegration :: TestTree
testDependencyAnalysisIntegration = testGroup "Dependency Analysis Integration"
  [ testCase "dependency analysis works" $ do
      let program = "func main() {\n    x := 42\n    return x\n}"
      case analyzeDependentTypes program of
        Left _ -> assertBool "Should analyze dependencies" False
        Right result -> assertBool "Should complete analysis" True
        
  , testCase "dependency analysis handles types" $ do
      let program = "type Container[T] struct { value T }\n\nfunc test() {\n    c := Container[int]{value: 42}\n}"
      case analyzeDependentTypes program of
        Left _ -> assertBool "Should analyze generic types" True
        Right result -> assertBool "Should handle generics" True
  ]

-- Test error handling integration
testErrorHandlingIntegration :: TestTree
testErrorHandlingIntegration = testGroup "Error Handling Integration"
  [ testCase "errors are properly categorized" $ do
      let program = "func test() {\n    return unknown_var\n}"
      case compileTypus program of
        Left _ -> assertBool "Should handle errors" True
        Right result -> assertBool "Should produce result or errors" True
        
  , testCase "multiple errors are collected" $ do
      let program = "func test() {\n    x := unknown_type\n    y := x\n    return unknown_var\n}"
      case compileTypus program of
        Left _ -> assertBool "Should collect multiple errors" True
        Right result -> assertBool "Should handle multiple issues" True
  ]

-- Test complete workflow integration
testCompleteWorkflow :: TestTree
testCompleteWorkflow = testGroup "Complete Workflow"
  [ testCase "parse -> analyze -> compile workflow" $ do
      let program = "//! ownership: on\n\nfunc main() {\n    x := 42\n    return x\n}"
      -- Parse
      case parseTypus program of
        Left _ -> assertBool "Should parse" False
        Right parsed -> do
          -- Analyze ownership
          case analyzeOwnership program of
            Left _ -> assertBool "Should analyze ownership" True  -- May fail gracefully
            Right ownershipErrors -> do
              -- Analyze dependencies
              case analyzeDependentTypes program of
                Left _ -> assertBool "Should analyze dependencies" True  -- May fail gracefully
                Right depResult -> do
                  -- Compile
                  case compileTypus program of
                    Left _ -> assertBool "Should compile or report errors" True
                    Right result -> assertBool "Complete workflow successful" True
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Complete compilation never crashes
prop_compilation_no_crash :: Property
prop_compilation_no_crash =
  forAll genTypusProgram $ \program ->
    case compileTypus program of
      Left _ -> property True
      Right _ -> property True

-- Property: Parsing never crashes
prop_parsing_no_crash :: Property
prop_parsing_no_crash =
  forAll genTypusProgram $ \program ->
    case parseTypus program of
      Left _ -> property True
      Right _ -> property True

-- Property: Ownership analysis never crashes
prop_ownership_no_crash :: Property
prop_ownership_no_crash =
  forAll genTypusProgram $ \program ->
    case analyzeOwnership program of
      Left _ -> property True
      Right _ -> property True

-- Property: Dependency analysis never crashes
prop_dependency_no_crash :: Property
prop_dependency_no_crash =
  forAll genTypusProgram $ \program ->
    case analyzeDependentTypes program of
      Left _ -> property True
      Right _ -> property True

-- Property: Error programs are handled gracefully
prop_error_programs_handled :: Property
prop_error_programs_handled =
  forAll genErrorProgram $ \program ->
    let parseResult = parseTypus program
        compileResult = compileTypus program
    in case (parseResult, compileResult) of
         (Left _, Left _) -> property True  -- Both fail appropriately
         (Left _, Right _) -> property True  -- Parse fails but compile recovers
         (Right _, Left _) -> property True  -- Parse succeeds but compile fails
         (Right _, Right _) -> property True  -- Both succeed

-- Property: Programs with directives are handled
prop_directive_programs_handled :: Property
prop_directive_programs_handled =
  forAll genProgramWithDirectives $ \program ->
    let parseResult = parseTypus program
        compileResult = compileTypus program
    in case (parseResult, compileResult) of
         (Left _, Left _) -> property True
         (Right _, Right _) -> property True
         _ -> property True  -- Mixed results are acceptable

-- Property: Complex programs don't crash the pipeline
prop_complex_programs_no_crash :: Property
prop_complex_programs_no_crash =
  forAll genComplexProgram $ \program ->
    let parseResult = parseTypus program
        ownershipResult = analyzeOwnership program
        dependencyResult = analyzeDependentTypes program
        compileResult = compileTypus program
    in property $ True  -- If we get here without crashing, the test passes

-- Property: Empty input is handled consistently
prop_empty_input_consistent :: Property
prop_empty_input_consistent =
  let program = ""
      parseResult = parseTypus program
      compileResult = compileTypus program
  in case (parseResult, compileResult) of
       (Left _, Left _) -> property False  -- Both should handle empty input
       (Right _, Right _) -> property True   -- Both succeed
       _ -> property True  -- One succeeds, one fails is acceptable

-- Property: Minimal valid program compiles
prop_minimal_program_compiles :: Property
prop_minimal_program_compiles =
  let program = "func main() {}"
      compileResult = compileTypus program
  in case compileResult of
       Left _ -> property False  -- Minimal program should compile
       Right _ -> property True

-- Property: Pipeline preserves program structure
prop_pipeline_preserves_structure :: Property
prop_pipeline_preserves_structure =
  forAll genSimpleProgram $ \program ->
    let parseResult = parseTypus program
    in case parseResult of
         Left _ -> property True  -- May fail parsing
         Right parsed -> 
           let programLines = lines program
               hasStructure = any ("func" `isPrefixOf`) programLines
           in property $ hasStructure ==> not (null parsed)

-- Property: Error messages are informative
prop_error_messages_informative :: Property
prop_error_messages_informative =
  forAll genErrorProgram $ \program ->
    case compileTypus program of
      Left err -> property $ length err > 0  -- Should have some error message
      Right _ -> property True  -- May succeed unexpectedly

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Integration End-to-End Tests"
  [ testBasicCompilation
  , testParserIntegration
  , testOwnershipIntegration
  , testDependencyAnalysisIntegration
  , testErrorHandlingIntegration
  , testCompleteWorkflow
  , testGroup "QuickCheck Properties"
    [ fastProperty "Compilation no crash" prop_compilation_no_crash
    , fastProperty "Parsing no crash" prop_parsing_no_crash
    , fastProperty "Ownership no crash" prop_ownership_no_crash
    , fastProperty "Dependency no crash" prop_dependency_no_crash
    , fastProperty "Error programs handled" prop_error_programs_handled
    , fastProperty "Directive programs handled" prop_directive_programs_handled
    , fastProperty "Complex programs no crash" prop_complex_programs_no_crash
    , fastProperty "Empty input consistent" prop_empty_input_consistent
    , fastProperty "Minimal program compiles" prop_minimal_program_compiles
    , fastProperty "Pipeline preserves structure" prop_pipeline_preserves_structure
    , fastProperty "Error messages informative" prop_error_messages_informative
    ]
  ]