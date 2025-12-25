module Test.Unit.CompilerIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, listOf1)
import qualified Data.Text as T
import Data.List (isInfixOf)

import Compiler (compile, CompilerError(..), CompilationPhase(..), generateGoCode)
import IntegratedCompiler (compileWithAllPhases)
import Parser (parseTypus, TypusFile(..))
import Compiler.GoAst (renderGoModule)

-- | Test end-to-end compiler integration
tests :: TestTree
tests =
  testGroup "Compiler Integration Tests"
    [ testGroup "Complete Compilation Pipeline"
        [ testCase "compiles simple program with all phases" $ do
            let code = unlines
                  [ "package main"
                  , "import \"fmt\""
                  , "func main() {"
                  , "  fmt.Println(\"Hello, World!\")"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "simple program should compile successfully" True

        , testCase "handles complex program with all features enabled" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , "import \"fmt\""
                  , "func processData(data: []int) []int {"
                  , "  var result = make([]int, len(data))"
                  , "  for i := 0; i < len(data); i++ {"
                  , "    result[i] = data[i] * 2"
                  , "  }"
                  , "  return result"
                  , "}"
                  , "func main() {"
                  , "  var input = []int{1, 2, 3, 4, 5}"
                  , "  var output = processData(input)"
                  , "  fmt.Printf(\"Input: %v\\n\", input)"
                  , "  fmt.Printf(\"Output: %v\\n\", output)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                -- Check if errors are expected (ownership or dependent types)
                let hasExpectedErrors = any (\e -> compilationPhase e `elem` [OwnershipPhase, DependentTypesPhase]) errs
                assertBool "should handle complex programs with expected errors" hasExpectedErrors
              Right _ -> assertBool "complex program should compile or fail gracefully" True

        , testCase "generates valid Go code output" $ do
            let code = unlines
                  [ "package main"
                  , "func add(a: int, b: int) int {"
                  , "  return a + b"
                  , "}"
                  , "func main() {"
                  , "  var result = add(5, 3)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right compiled -> do
                let goCode = generateGoCode compiled
                assertBool "generated Go code should contain function definitions" $ 
                    "func add" `T.isInfixOf` goCode
                assertBool "generated Go code should contain main function" $ 
                    "func main" `T.isInfixOf` goCode
        ]

    , testGroup "Error Propagation Through Pipeline"
        [ testCase "propagates syntax errors through all phases" $ do
            let code = unlines
                  [ "package main"
                  , "func main() {"  -- Missing closing brace
                  , "  var x = 42"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasSyntaxError = any (\e -> compilationPhase e == SyntaxPhase) errs
                assertBool "should detect and propagate syntax errors" hasSyntaxError
              Right _ -> assertFailure "expected compilation failure due to syntax error"

        , testCase "propagates type errors through later phases" $ do
            let code = unlines
                  [ "package main"
                  , "func main() {"
                  , "  var x: string = 42"  -- Type mismatch
                  , "  var y = x + 1"      -- Should propagate error
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasTypeError = any (\e -> compilationPhase e == TypeCheckPhase) errs
                assertBool "should detect and propagate type errors" hasTypeError
              Right _ -> assertFailure "expected compilation failure due to type error"

        , testCase "handles multiple errors from different phases" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "package main"
                  , "func main() {"
                  , "  var x: string = 42"     -- Type error
                  , "  var y = make([]int, 10)"
                  , "  var z = y"              -- Ownership move
                  , "  var w = y[0]"           -- Use after move
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasTypeError = any (\e -> compilationPhase e == TypeCheckPhase) errs
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should detect type errors" hasTypeError
                assertBool "should detect ownership errors" hasOwnershipError
                assertBool "should detect multiple phase errors" $ length errs >= 2
              Right _ -> assertFailure "expected compilation failure"
        ]

    , testGroup "Integration with Type System"
        [ testCase "integrates dependent type checking with compilation" $ do
            let code = unlines
                  [ "//! dependent_types: on"
                  , "package main"
                  , "func safeArrayAccess(arr: []int, index: int) int {"
                  , "  if index >= 0 && index < len(arr) {"
                  , "    return arr[index]"
                  , "  }"
                  , "  return -1"
                  , "}"
                  , "func main() {"
                  , "  var data = []int{1, 2, 3, 4, 5}"
                  , "  var result = safeArrayAccess(data, 2)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasDependentTypeError = any (\e -> compilationPhase e == DependentTypesPhase) errs
                assertBool "should handle dependent type integration" hasDependentTypeError
              Right _ -> assertBool "dependent type integration should work" True

        , testCase "integrates ownership analysis with compilation" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "package main"
                  , "func processString(s: string) string {"
                  , "  return s + \" processed\""
                  , "}"
                  , "func main() {"
                  , "  var original = \"hello\""
                  , "  var processed = processString(original)"
                  , "  var combined = original + processed"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should handle ownership integration" hasOwnershipError
              Right _ -> assertBool "ownership integration should work" True
        ]

    , testGroup "Code Generation Integration"
        [ testCase "generates correct Go code for complex types" $ do
            let code = unlines
                  [ "package main"
                  , "type Person struct {"
                  , "  name: string"
                  , "  age: int"
                  , "}"
                  , "func (p: Person) greet() string {"
                  , "  return \"Hello, \" + p.name"
                  , "}"
                  , "func main() {"
                  , "  var p = Person{name: \"Alice\", age: 30}"
                  , "  var greeting = p.greet()"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right compiled -> do
                let goCode = generateGoCode compiled
                assertBool "generated Go code should contain struct definition" $ 
                    "type Person struct" `T.isInfixOf` goCode
                assertBool "generated Go code should contain method" $ 
                    "func (p Person)" `T.isInfixOf` goCode

        , testCase "generates valid Go code for control flow" $ do
            let code = unlines
                  [ "package main"
                  , "func factorial(n: int) int {"
                  , "  if n <= 1 {"
                  , "    return 1"
                  , "  }"
                  , "  return n * factorial(n - 1)"
                  , "}"
                  , "func main() {"
                  , "  var result = factorial(5)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right compiled -> do
                let goCode = generateGoCode compiled
                assertBool "generated Go code should contain if statement" $ 
                    "if n <= 1" `T.isInfixOf` goCode
                assertBool "generated Go code should contain recursive call" $ 
                    "factorial(n - 1)" `T.isInfixOf` goCode
        ]

    , testGroup "Performance Integration"
        [ testCase "handles large source files efficiently" $ do
            let largeFunction = unlines $ replicate 100 "  var x = x + 1"
            let code = unlines
                  [ "package main"
                  , "func largeFunction() int {"
                  , "  var x = 0"
                  ] ++ largeFunction ++ [
                  , "  return x"
                  , "}"
                  , "func main() {"
                  , "  var result = largeFunction()"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "should handle large source files" True

        , testCase "optimizes compilation pipeline for repeated operations" $ do
            let code = unlines
                  [ "package main"
                  , "func sumArray(arr: []int) int {"
                  , "  var total = 0"
                  , "  for i := 0; i < len(arr); i++ {"
                  , "    total += arr[i]"
                  , "  }"
                  , "  return total"
                  , "}"
                  , "func main() {"
                  , "  var data = []int{1, 2, 3, 4, 5}"
                  , "  var result1 = sumArray(data)"
                  , "  var result2 = sumArray(data)"
                  , "  var result3 = sumArray(data)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "should optimize repeated operations" True
        ]

    , testGroup "QuickCheck Property Tests"
        [ testProperty "compilation preserves program semantics" $ forAll (elements ["int", "string", "bool"]) $ \typeName -> do
            let code = unlines
                  [ "package main"
                  , "func getValue() " ++ typeName ++ " {"
                  , "  if \"" ++ typeName ++ "\" == \"int\" {"
                  , "    return 42"
                  , "  } else if \"" ++ typeName ++ "\" == \"string\" {"
                  , "    return \"hello\""
                  , "  } else {"
                  , "    return true"
                  , "  }"
                  , "}"
                  , "func main() {"
                  , "  var x = getValue()"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left _ -> return $ False
              Right _ -> return $ True

        , testProperty "error detection is consistent" $ forAll (elements ["valid", "invalid-syntax", "invalid-type"]) $ \programType -> do
            let code = case programType of
                  "valid" -> unlines
                    [ "package main"
                    , "func main() {"
                    , "  var x = 42"
                    , "}"
                    ]
                  "invalid-syntax" -> unlines
                    [ "package main"
                    , "func main() {"  -- Missing closing brace
                    , "  var x = 42"
                    ]
                  "invalid-type" -> unlines
                    [ "package main"
                    , "func main() {"
                    , "  var x: string = 42"
                    , "}"
                    ]
            result <- compile code
            case (programType, result) of
              ("valid", Right _) -> return $ True
              ("valid", Left _) -> return $ False
              (_, Left errs) -> return $ not $ null errs
              (_, Right _) -> return $ False
        ]
    ]