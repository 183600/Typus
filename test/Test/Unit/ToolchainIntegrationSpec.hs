module Test.Unit.ToolchainIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler (compile, generateGoCode)
import GoToolchain (runGoCommand, withTemporaryGoProject)
import Parser (parseTypus)
import qualified Data.Text as T
import Data.List (isInfixOf, lines)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

-- Test Go code generation
test_go_code_generation :: TestTree
test_go_code_generation = testCase "Go code generation produces valid output" $ do
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
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- generateGoCode compiled
        assertBool "Generated Go code should contain package declaration" $ 
          "package main" `isInfixOf` goCode
        assertBool "Generated Go code should contain function definitions" $ 
          "func add" `isInfixOf` goCode
        assertBool "Generated Go code should contain main function" $ 
          "func main" `isInfixOf` goCode
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test Go build integration
test_go_build_integration :: TestTree
test_go_build_integration = testCase "Generated Go code can be built" $ do
    let source = unlines
          [ "package main"
          , "import \"fmt\""
          , "func greet(name string) string {"
          , "    return \"Hello, \" + name"
          , "}"
          , "func main() {"
          , "    message := greet(\"World\")"
          , "    fmt.Println(message)"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- generateGoCode compiled
        withSystemTempDirectory "typus-test" $ \tempDir -> do
          let goFile = tempDir </> "main.go"
          writeFile goFile goCode
          buildResult <- runGoCommand ["build", goFile]
                  case buildResult of
                    Right _ -> return ()  -- Build succeeded
                    Left err -> assertFailure $ "Go build failed: " ++ show err      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test Go run integration
test_go_run_integration :: TestTree
test_go_run_integration = testCase "Generated Go code can be executed" $ do
    let source = unlines
          [ "package main"
          , "import \"fmt\""
          , "func calculate(x int, y int) int {"
          , "    return x * y + 10"
          , "}"
          , "func main() {"
          , "    result := calculate(5, 3)"
          , "    fmt.Printf(\"%d\", result)"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- generateGoCode compiled
        withSystemTempDirectory "typus-test" $ \tempDir -> do
          let goFile = tempDir </> "main.go"
          writeFile goFile goCode
          runResult <- runGoCommand ["run", goFile]
                  case runResult of
                    Right output -> do
                      let expectedOutput = "25"  -- 5 * 3 + 10 = 25
                      assertBool "Output should match expected result" $ 
                        expectedOutput `isInfixOf` output
                    Left err -> assertFailure $ "Go run failed: " ++ show err      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test Go test generation
test_go_test_generation :: TestTree
test_go_test_generation = testCase "Go tests are generated correctly" $ do
    let source = unlines
          [ "package main"
          , "func add(a int, b int) int {"
          , "    return a + b"
          , "}"
          , "func multiply(a int, b int) int {"
          , "    return a * b"
          , "}"
          , "func main() {"
          , "    _ := add(5, 3)"
          , "    _ := multiply(4, 6)"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- generateGoCode compiled
        withSystemTempDirectory "typus-test" $ \tempDir -> do
          let goFile = tempDir </> "main.go"
              testFile = tempDir </> "main_test.go"
          writeFile goFile goCode
          -- Generate test file
          let testCode = unlines
                [ "package main"
                , "import \"testing\""
                , "func TestAdd(t *testing.T) {"
                , "    result := add(5, 3)"
                , "    expected := 8"
                , "    if result != expected {"
                , "        t.Errorf(\"add(5, 3) = %d; want %d\", result, expected)"
                , "    }"
                , "}"
                , "func TestMultiply(t *testing.T) {"
                , "    result := multiply(4, 6)"
                , "    expected := 24"
                , "    if result != expected {"
                , "        t.Errorf(\"multiply(4, 6) = %d; want %d\", result, expected)"
                , "    }"
                , "}"
                ]
          writeFile testFile testCode
          testResult <- runGoCommand ["test", tempDir]
          case testResult of
            Right _ -> return ()  -- Tests passed
            Left err -> assertFailure $ "Go test failed: " ++ show err
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test Go formatting integration
test_go_formatting_integration :: TestTree
test_go_formatting_integration = testCase "Generated Go code is properly formatted" $ do
    let source = unlines
          [ "package main"
          , "func messy(   x int,y int)int{return x+y}"
          , "func main(){println(messy(5,3))}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- generateGoCode compiled
        withSystemTempDirectory "typus-test" $ \tempDir -> do
          let goFile = tempDir </> "main.go"
          writeFile goFile goCode
          formatResult <- runGoCommand ["fmt", goFile]
          case formatResult of
            Right _ -> do
              -- Read formatted code back
              formatted <- readFile goFile
              assertBool "Code should be properly formatted" $ 
                "func messy(x int, y int) int" `isInfixOf` formatted
            Left err -> assertFailure $ "Go fmt failed: " ++ show err
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test Go vet integration
test_go_vet_integration :: TestTree
test_go_vet_integration = testCase "Generated Go code passes go vet" $ do
    let source = unlines
          [ "package main"
          , "import \"fmt\""
          , "func greet(name string) {"
          , "    fmt.Printf(\"Hello, %s\\n\", name)"
          , "}"
          , "func main() {"
          , "    greet(\"World\")"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- generateGoCode compiled
        withSystemTempDirectory "typus-test" $ \tempDir -> do
          let goFile = tempDir </> "main.go"
          writeFile goFile goCode
          vetResult <- runGoCommand ["vet", tempDir]
          case vetResult of
            Right _ -> return ()  -- Vet passed
            Left err -> assertFailure $ "Go vet failed: " ++ show err
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test toolchain error handling
test_toolchain_error_handling :: TestTree
test_toolchain_error_handling = testCase "Toolchain errors are properly handled" $ do
    let source = unlines
          [ "package main"
          , "func undefined() undefined_type {"  -- invalid syntax
          , "    return nil"
          , "}"
          , "func main() {}"
          ]
    result <- compile source
    case result of
      Left _ -> return ()  -- Expected compilation error
      Right compiled -> do
        -- If compilation somehow succeeds, go build should fail
        goCode <- generateGoCode compiled
        withSystemTempDirectory "typus-test" $ \tempDir -> do
          let goFile = tempDir </> "main.go"
          writeFile goFile goCode
          buildResult <- goBuild goFile
          case buildResult of
            Left _ -> return ()  -- Expected build failure
            Right _ -> assertFailure "Expected build to fail with invalid code"

-- Test multiple file compilation
test_multiple_file_compilation :: TestTree
test_multiple_file_compilation = testCase "Multiple file compilation works" $ do
    let source1 = unlines
          [ "package main"
          , "func add(a int, b int) int {"
          , "    return a + b"
          , "}"
          ]
        source2 = unlines
          [ "package main"
          , "func multiply(a int, b int) int {"
          , "    return a * b"
          , "}"
          , "func main() {"
          , "    sum := add(5, 3)"
          , "    product := multiply(4, 6)"
          , "    println(sum, product)"
          , "}"
          ]
    result1 <- compile source1
    result2 <- compile source2
    case (result1, result2) of
      (Right compiled1, Right compiled2) -> do
        goCode1 <- generateGoCode compiled1
        goCode2 <- generateGoCode compiled2
        withSystemTempDirectory "typus-test" $ \tempDir -> do
          let goFile1 = tempDir </> "add.go"
              goFile2 = tempDir </> "main.go"
          writeFile goFile1 goCode1
          writeFile goFile2 goCode2
          buildResult <- runGoCommand ["build", goFile2]
          case buildResult of
            Right _ -> return ()  -- Build succeeded
            Left err -> assertFailure $ "Multi-file build failed: " ++ show err
      _ -> assertFailure "Compilation failed"

-- QuickCheck property: Generated Go code is syntactically valid
prop_generated_go_syntax_valid :: String -> Property
prop_generated_go_syntax_valid source =
  property $
    case compile source of
      Right compiled -> 
        case generateGoCode compiled of
          Right goCode -> "package main" `isInfixOf` goCode
          Left _ -> property False
      Left _ -> property True  -- Invalid source is allowed to fail

-- QuickCheck property: Toolchain operations are deterministic
prop_toolchain_deterministic :: String -> Property
prop_toolchain_deterministic source =
  property $
    case compile source of
      Right compiled -> 
        case generateGoCode compiled of
          Right goCode1 -> do
            case generateGoCode compiled of
              Right goCode2 -> goCode1 === goCode2
              Left _ -> property False
          Left _ -> property False
      Left _ -> property True

tests :: TestTree
tests = testGroup "Toolchain Integration"
  [ test_go_code_generation
  , test_go_build_integration
  , test_go_run_integration
  , test_go_test_generation
  , test_go_formatting_integration
  , test_go_vet_integration
  , test_toolchain_error_handling
  , test_multiple_file_compilation
  , testCase "QuickCheck: Generated Go syntax valid" $
      fastProperty prop_generated_go_syntax_valid
  , testCase "QuickCheck: Toolchain deterministic" $
      fastProperty prop_toolchain_deterministic
  ]