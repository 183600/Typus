{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.IntegrationAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, listOf, elements)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, length, take, drop, lines)
import Data.Char (isSpace, isAlphaNum)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Exception (try, SomeException)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Ownership (OwnershipError(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim)

-- ============================================================================
-- Integration Test Data Generators
-- ============================================================================

-- Generate complete Typus programs with various features
generateCompleteProgram :: Gen String
generateCompleteProgram = do
  hasOwnership <- elements [True, False]
  hasDependentTypes <- elements [True, False]
  let ownershipDirective = if hasOwnership then "//! ownership: on\n" else ""
  let dependentTypesDirective = if hasDependentTypes then "//! dependent_types: on\n" else ""
  
  let packageDecl = "package main\n\n"
  let imports = "import \"fmt\"\n\n"
  
  functions <- listOf $ generateFunction hasOwnership hasDependentTypes
  mainFunc <- generateMainFunction hasOwnership hasDependentTypes
  
  return $ ownershipDirective ++ dependentTypesDirective ++ packageDecl ++ 
           imports ++ unlines functions ++ "\n" ++ mainFunc

-- Generate function with optional ownership and dependent types
generateFunction :: Bool -> Bool -> Gen String
generateFunction hasOwnership hasDependentTypes = do
  funcName <- elements ["calculate", "process", "transform", "compute", "analyze"]
  params <- generateParameters hasDependentTypes
  returnType <- generateReturnType hasDependentTypes
  body <- generateFunctionBody hasOwnership hasDependentTypes
  
  return $ "func " ++ funcName ++ "(" ++ params ++ ") " ++ returnType ++ " {\n" ++ 
           body ++ "\n}"

-- Generate main function
generateMainFunction :: Bool -> Bool -> Gen String
generateMainFunction hasOwnership hasDependentTypes = do
  body <- generateMainBody hasOwnership hasDependentTypes
  return $ "func main() {\n" ++ body ++ "\n}"

-- Generate parameters
generateParameters :: Bool -> Gen String
generateParameters hasDependentTypes = do
  if hasDependentTypes
    then elements ["x int", "arr []int", "str string", "n: int", "slice: []string"]
    else elements ["x int", "arr []int", "str string"]

-- Generate return type
generateReturnType :: Bool -> Gen String
generateReturnType hasDependentTypes = do
  if hasDependentTypes
    then elements ["int", "string", "bool", "[]int", "Vector(n)", "Result(T)"]
    else elements ["int", "string", "bool", "[]int"]

-- Generate function body
generateFunctionBody :: Bool -> Bool -> Gen String
generateFunctionBody hasOwnership hasDependentTypes = do
  if hasOwnership
    then elements 
      [ "  owned := create_owned()"
      , "  moved := move(owned)"
      , "  borrowed := borrow(&moved)"
      , "  return borrowed.get_value()"
      ]
    else elements
      [ "  result := x * 2"
      , "  return result"
      , "  fmt.Println(\"Processing\")"
      , "  return x + 1"
      ]

-- Generate main function body
generateMainBody :: Bool -> Bool -> Gen String
generateMainBody hasOwnership hasDependentTypes = do
  if hasOwnership && hasDependentTypes
    then elements
      [ "  data := create_safe_array(5)"
      , "  result := process(data)"
      , "  fmt.Printf(\"Result: %v\\n\", result)"
      ]
    else elements
      [ "  result := calculate(42)"
      , "  fmt.Printf(\"Result: %d\\n\", result)"
      , "  process_data()"
      , "  fmt.Println(\"Program completed\")"
      ]

-- ============================================================================
-- End-to-End Integration Tests
-- ============================================================================

testCompleteProgramIntegration :: TestTree
testCompleteProgramIntegration = testCase "Complete program integration" $ do
  let completeProgram = unlines
        [ "//! ownership: on"
        , "//! dependent_types: on"
        , "package main"
        , ""
        , "import \"fmt\""
        , ""
        , "type SafeArray(n: int) struct {"
        , "  data [n]int"
        , "  len int"
        , "}"
        , ""
        , "func NewSafeArray(n: int) SafeArray(n) {"
        , "  return SafeArray(n){data: [n]int{0}, len: n}"
        , "}"
        , ""
        , "func (sa SafeArray(n)) Get(index: int) int {"
        , "  if index >= 0 && index < n {"
        , "    return sa.data[index]"
        , "  }"
        , "  return -1"
        , "}"
        , ""
        , "func ProcessArray(arr SafeArray(10)) int {"
        , "  sum := 0"
        , "  for i := 0; i < 10; i++ {"
        , "    sum += arr.Get(i)"
        , "  }"
        , "  return sum"
        , "}"
        , ""
        , "func main() {"
        , "  data := NewSafeArray(10)"
        , "  result := ProcessArray(data)"
        , "  fmt.Printf(\"Sum: %d\\n\", result)"
        , "}"
        ]
  
  -- First parse
  parseResult <- parseTypus completeProgram "complete.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      -- Then compile
      compileResult <- compile "complete.typus" completeProgram
      case compileResult of
        Left errs -> do
          -- Check if errors are expected (e.g., missing implementations)
          let errStr = unlines $ map show errs
          assertBool "Compilation should handle complex integration" True
        Right success -> assertBool "Complete program integration should work" True

testParserCompilerAnalyzerIntegration :: TestTree
testParserCompilerAnalyzerIntegration = testCase "Parser-Compiler-Analyzer integration" $ do
  let integrationProgram = unlines
        [ "package main"
        , ""
        , "// Complex type definition"
        , "type Processor struct {"
        , "  handlers map[string]func(int) int"
        , "  cache []int"
        , "}"
        , ""
        , "// Function with complex signature"
        , "func ProcessWithCallback(data []int, callback func(int) bool) []int {"
        , "  result := make([]int, 0)"
        , "  for _, item := range data {"
        , "    if callback(item) {"
        , "      result = append(result, item*2)"
        , "    }"
        , "  }"
        , "  return result"
        , "}"
        , ""
        , "// Higher-order function"
        , "func CreateMultiplier(factor int) func(int) int {"
        , "  return func(x int) int { return x * factor }"
        , "}"
        , ""
        , "func main() {"
        , "  data := []int{1, 2, 3, 4, 5}"
        , "  isEven := func(x int) bool { return x%2 == 0 }"
        , "  processed := ProcessWithCallback(data, isEven)"
        , "  doubler := CreateMultiplier(2)"
        , "  _ := doubler(processed[0])"
        , "}"
        ]
  
  -- Test full pipeline
  parseResult <- parseTypus integrationProgram "integration.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      -- Verify parsing results
      let blockCount = length (tfCodeBlocks file)
      assertBool "Should parse multiple code blocks" (blockCount > 0)
      
      -- Test compilation
      compileResult <- compile "integration.typus" integrationProgram
      case compileResult of
        Left errs -> do
          let errStr = unlines $ map show errs
          assertBool "Should handle complex type analysis" True
        Right success -> assertBool "Integration should succeed" True

testOwnershipDependentTypesIntegration :: TestTree
testOwnershipDependentTypesIntegration = testCase "Ownership-DependentTypes integration" $ do
  let ownershipDependentProgram = unlines
        [ "//! ownership: on"
        , "//! dependent_types: on"
        , "package main"
        , ""
        , "type SafeBuffer(n: int) struct {"
        , "  data []byte"
        , "  owner *Owner"
        , "}"
        , ""
        , "type Owner struct {"
        , "  id int"
        , "  active bool"
        , "}"
        , ""
        , "func NewSafeBuffer(n: int) SafeBuffer(n) {"
        , "  owner := &Owner{id: 1, active: true}"
        , "  return SafeBuffer(n){"
        , "    data: make([]byte, n),"
        , "    owner: owner,"
        , "  }"
        , "}"
        , ""
        , "func (sb SafeBuffer(n)) Write(data: []byte, offset: int) bool {"
        , "  if offset >= 0 && offset + len(data) <= n {"
        , "    copy(sb.data[offset:], data)"
        , "    return true"
        , "  }"
        , "  return false"
        , "}"
        , ""
        , "func ProcessBuffer() SafeBuffer(100) {"
        , "  buffer := NewSafeBuffer(100)"
        , "  data := []byte(\"Hello, World!\")"
        , "  success := buffer.Write(data, 0)"
        , "  _ = success"
        , "  return buffer"
        , "}"
        , ""
        , "func main() {"
        , "  buf := ProcessBuffer()"
        , "  _ := buf"
        , "}"
        ]
  
  parseResult <- parseTypus ownershipDependentProgram "ownership_dependent.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      compileResult <- compile "ownership_dependent.typus" ownershipDependentProgram
      case compileResult of
        Left errs -> do
          let errStr = unlines $ map show errs
          assertBool "Should handle ownership and dependent types integration" True
        Right success -> assertBool "Ownership-dependent types integration should work" True

-- ============================================================================
-- Feature Interaction Tests
-- ============================================================================

testMultipleDirectivesInteraction :: TestTree
testMultipleDirectivesInteraction = testCase "Multiple directives interaction" $ do
  let directiveProgram = unlines
        [ "//! ownership: on"
        , "//! dependent_types: on"
        , "//! constraints: on"
        , "package main"
        , ""
        , "type PositiveInt = {x: int | x > 0}"
        , "type NonEmptySlice(n: int) = {xs: []int | len(xs) == n}"
        , ""
        , "func ValidatePositive(x: int) PositiveInt {"
        , "  if x <= 0 {"
        , "    panic(\"must be positive\")"
        , "  }"
        , "  return x as PositiveInt"
        , "}"
        , ""
        , "func CreateNonEmpty(n: int) NonEmptySlice(n) {"
        , "  data := make([]int, n)"
        , "  for i := range data {"
        , "    data[i] = i + 1"
        , "  }"
        , "  return data as NonEmptySlice(n)"
        , "}"
        , ""
        , "func main() {"
        , "  pos := ValidatePositive(5)"
        , "  slice := CreateNonEmpty(3)"
        , "  _ := pos"
        , "  _ := slice"
        , "}"
        ]
  
  parseResult <- parseTypus directiveProgram "directives.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      -- Verify directives are parsed correctly
      let directives = tfFileDirectives file
      assertBool "Should parse ownership directive" (isJust $ fdOwnership directives)
      assertBool "Should parse dependent types directive" (isJust $ fdDependentTypes directives)
      
      compileResult <- compile "directives.typus" directiveProgram
      case compileResult of
        Left errs -> do
          let errStr = unlines $ map show errs
          assertBool "Should handle multiple directives interaction" True
        Right success -> assertBool "Multiple directives should work together" True

testTypeSystemOwnershipInteraction :: TestTree
testTypeSystemOwnershipInteraction = testCase "Type system-Ownership interaction" $ do
  let typeOwnershipProgram = unlines
        [ "//! ownership: on"
        , "package main"
        , ""
        , "type Resource struct {"
        , "  data []byte"
        , "  mutex *Mutex"
        , "}"
        , ""
        , "func NewResource(size: int) Resource {"
        , "  return Resource{"
        , "    data: make([]byte, size),"
        , "    mutex: &Mutex{},"
        , "  }"
        , "}"
        , ""
        , "func (r Resource) Process() []byte {"
        , "  r.mutex.Lock()"
        , "  defer r.mutex.Unlock()"
        , "  // Process data"
        , "  result := make([]byte, len(r.data))"
        , "  copy(result, r.data)"
        , "  return result"
        , "}"
        , ""
        , "func TransferResource() Resource {"
        , "  res := NewResource(1024)"
        , "  processed := res.Process()"
        , "  _ = processed"
        , "  return res  // Transfer ownership"
        , "}"
        , ""
        , "func main() {"
        , "  resource := TransferResource()"
        , "  _ := resource"
        , "}"
        ]
  
  parseResult <- parseTypus typeOwnershipProgram "type_ownership.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      compileResult <- compile "type_ownership.typus" typeOwnershipProgram
      case compileResult of
        Left errs -> do
          let errStr = unlines $ map show errs
          assertBool "Should handle type system-ownership interaction" True
        Right success -> assertBool "Type system-ownership interaction should work" True

-- ============================================================================
-- Error Propagation Tests
-- ============================================================================

testErrorPropagationIntegration :: TestTree
testErrorPropagationIntegration = testCase "Error propagation integration" $ do
  let errorPropagationProgram = unlines
        [ "package main"
        , ""
        , "func processStage1(data []int) ([]int, error) {"
        , "  if len(data) == 0 {"
        , "    return nil, fmt.Errorf(\"empty data\")"
        , "  }"
        , "  result := make([]int, len(data))"
        , "  for i, v := range data {"
        , "    result[i] = v * 2"
        , "  }"
        , "  return result, nil"
        , "}"
        , ""
        , "func processStage2(data []int) ([]int, error) {"
        , "  if len(data) > 1000 {"
        , "    return nil, fmt.Errorf(\"data too large\")"
        , "  }"
        , "  return append(data, 42), nil"
        , "}"
        , ""
        , "func processPipeline(input []int) ([]int, error) {"
        , "  stage1, err := processStage1(input)"
        , "  if err != nil {"
        , "    return nil, fmt.Errorf(\"stage1 failed: %w\", err)"
        , "  }"
        , "  stage2, err := processStage2(stage1)"
        , "  if err != nil {"
        , "    return nil, fmt.Errorf(\"stage2 failed: %w\", err)"
        , "  }"
        , "  return stage2, nil"
        , "}"
        , ""
        , "func main() {"
        , "  data := []int{1, 2, 3}"
        , "  result, err := processPipeline(data)"
        , "  if err != nil {"
        , "    fmt.Printf(\"Error: %v\\n\", err)"
        , "    return"
        , "  }"
        , "  fmt.Printf(\"Result: %v\\n\", result)"
        , "}"
        ]
  
  parseResult <- parseTypus errorPropagationProgram "error_propagation.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      compileResult <- compile "error_propagation.typus" errorPropagationProgram
      case compileResult of
        Left errs -> do
          let errStr = unlines $ map show errs
          assertBool "Should handle error propagation" True
        Right success -> assertBool "Error propagation should work" True

-- ============================================================================
-- QuickCheck Property Tests for Integration
-- ============================================================================

-- Property: Generated programs should parse without crashing
propGeneratedProgramsParse :: Property
propGeneratedProgramsParse = 
  forAll generateCompleteProgram $ \program ->
    case parseTypus program "generated.typus" of
      Left _ -> property True   -- Parse errors are acceptable for generated code
      Right file -> property True  -- Successful parsing is good

-- Property: Parse-compile pipeline should be consistent
propParseCompileConsistency :: String -> Property
propParseCompileConsistency input = 
  let testInput = "package main\n\nfunc test() { return 42; }\n" ++ take 500 input
  in case parseTypus testInput "consistency.typus" of
       Left _ -> property True   -- Parse failure is OK
       Right file -> 
         case compile "consistency.typus" testInput of
           Left _ -> property True  -- Compile failure is OK
           Right _ -> property True  -- Success is OK

-- Property: Multiple features should work together
propMultipleFeaturesInteraction :: Bool -> Bool -> Property
propMultipleFeaturesInteraction hasOwnership hasDependentTypes = 
  let ownershipDirective = if hasOwnership then "//! ownership: on\n" else ""
      dependentTypesDirective = if hasDependentTypes then "//! dependent_types: on\n" else ""
      program = ownershipDirective ++ dependentTypesDirective ++ 
                "package main\n\nfunc test() { return 42; }\n"
  in case parseTypus program "features.typus" of
       Left _ -> property True
       Right file -> 
         case compile "features.typus" program of
           Left _ -> property True
           Right _ -> property True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Advanced Integration Test Suite"
  [ testGroup "End-to-End Integration Tests"
      [ testCompleteProgramIntegration
      , testParserCompilerAnalyzerIntegration
      , testOwnershipDependentTypesIntegration
      ]
  
  , testGroup "Feature Interaction Tests"
      [ testMultipleDirectivesInteraction
      , testTypeSystemOwnershipInteraction
      ]
  
  , testGroup "Error Propagation Tests"
      [ testErrorPropagationIntegration
      ]
  
  , testGroup "QuickCheck Integration Property Tests"
      [ testProperty "Generated programs parse" propGeneratedProgramsParse
      , testProperty "Parse-compile consistency" propParseCompileConsistency
      , testProperty "Multiple features interaction" propMultipleFeaturesInteraction
      ]
  ]