{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CompilerCoreFunctionalitySpec where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), oneof, (===), property)

import Compiler (compile, CompilerError(..), CompilationPhase(..), generateGoCode)
import Compiler.Errors.Core (errorWithCategory, ErrorCategory(..), ErrorLocation(..), message)
import Parser (parseTypus)
import qualified Data.Text as T
import qualified Data.List as L

-- Test data generators
instance Arbitrary CompilationPhase where
  arbitrary = oneof 
    [ return ParsingPhase
    , return TypeCheckingPhase
    , return CodeGenerationPhase
    ]

instance Arbitrary CompilerError where
  arbitrary = do
    phase <- arbitrary
    -- Generate a minimal CompilerError for testing
    let defaultLoc = ErrorLocation Nothing 0 0 Nothing Nothing
    let typeError = errorWithCategory "TEST001" Parsing (T.pack "Test error") defaultLoc
    return $ CompilerError typeError Nothing [] phase

-- Test cases
compilerCoreFunctionalityTests :: TestTree
compilerCoreFunctionalityTests = testGroup "Compiler Core Functionality Tests"
  [ -- Basic compilation tests
    testCase "Compile simple valid code" $ do
      let content = "let x = 42"
      case parseTypus content of
        Left err -> assertBool ("Should parse simple code: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> assertBool ("Should compile simple code: " ++ show errs) False
            Right goCode -> assertBool "Should generate Go code" (not $ null goCode)

  , testCase "Compile code with type annotations" $ do
      let content = "let x: Int = 42"
      case parseTypus content of
        Left err -> assertBool ("Should parse code with type annotations: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> assertBool ("Should compile code with type annotations: " ++ show errs) False
            Right goCode -> assertBool "Should generate Go code" (not $ null goCode)

  , testCase "Compile code with function definition" $ do
      let content = "package main\n\nfunc add(a: Int, b: Int): Int {\n  return a + b\n}"
      case parseTypus content of
        Left err -> assertBool ("Should parse function definition: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> assertBool ("Should compile function definition: " ++ show errs) False
            Right goCode -> assertBool "Should generate Go code" (not $ null goCode)

  , -- Error handling tests
    testCase "Detect syntax error in incomplete expression" $ do
      let content = "let x = +"
      case parseTypus content of
        Left _ -> return ()  -- Expected to fail at parsing stage
        Right typusFile -> do
          case compile typusFile of
            Left errs -> 
              let hasSyntaxError = any (\e -> T.pack "syntax error: incomplete expression" `T.isInfixOf` message (ceError e)) errs
              in assertBool "Should detect syntax error in incomplete expression" hasSyntaxError
            Right _ -> assertBool "Should fail to compile incomplete expression" False

  , testCase "Detect type error in variable declaration" $ do
      let content = "var x int = \"string\""
      case parseTypus content of
        Left err -> assertBool ("Should parse code with type error: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> 
              let hasTypeError = any (\e -> T.pack "type error: cannot use string as int value" `T.isInfixOf` message (ceError e)) errs
              in assertBool "Should detect type error in variable declaration" hasTypeError
            Right _ -> assertBool "Should fail to compile type error" False

  , testCase "Detect type error in type annotation" $ do
      let content = "let x: Int = \"hello\""
      case parseTypus content of
        Left err -> assertBool ("Should parse code with type annotation error: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> 
              let hasTypeError = any (\e -> T.pack "type error: cannot use string as Int value" `T.isInfixOf` message (ceError e)) errs
              in assertBool "Should detect type error in type annotation" hasTypeError
            Right _ -> assertBool "Should fail to compile type annotation error" False

  , testCase "Detect missing return statement" $ do
      let content = "package main\n\nfunc missingReturn() int {\n  let x = 42\n}"
      case parseTypus content of
        Left err -> assertBool ("Should parse function with missing return: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> 
              let hasMissingReturnError = any (\e -> T.pack "syntax error: missing return statement" `T.isInfixOf` message (ceError e)) errs
              in assertBool "Should detect missing return statement" hasMissingReturnError
            Right _ -> assertBool "Should fail to compile function with missing return" False

  , -- Go code generation tests
    testCase "Generate Go code from simple Typus code" $ do
      let content = "let x = 42"
      case parseTypus content of
        Left err -> assertBool ("Should parse simple code: " ++ err) False
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          assertBool "Should generate Go code" (not $ null goCode)

  , testCase "Generate Go code from complex Typus code" $ do
      let content = "package main\n\n//! ownership: on\n\nfunc add(a: Int, b: Int): Int {\n  return a + b\n}\n\nlet result = add(1, 2)"
      case parseTypus content of
        Left err -> assertBool ("Should parse complex code: " ++ err) False
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          assertBool "Should generate Go code from complex code" (not $ null goCode)

  , -- Directive handling tests
    testCase "Compile code with ownership directive" $ do
      let content = "//! ownership: on\nlet x = 42"
      case parseTypus content of
        Left err -> assertBool ("Should parse code with ownership directive: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> assertBool ("Should compile code with ownership directive: " ++ show errs) False
            Right goCode -> assertBool "Should generate Go code with ownership directive" (not $ null goCode)

  , testCase "Compile code with dependent types directive" $ do
      let content = "//! dependent_types: on\nlet x: Int = 42"
      case parseTypus content of
        Left err -> assertBool ("Should parse code with dependent types directive: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> assertBool ("Should compile code with dependent types directive: " ++ show errs) False
            Right goCode -> assertBool "Should generate Go code with dependent types directive" (not $ null goCode)

  , testCase "Compile code with constraints directive" $ do
      let content = "//! constraints: on\nlet x: Int = 42"
      case parseTypus content of
        Left err -> assertBool ("Should parse code with constraints directive: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> assertBool ("Should compile code with constraints directive: " ++ show errs) False
            Right goCode -> assertBool "Should generate Go code with constraints directive" (not $ null goCode)

  , -- QuickCheck property tests
    testProperty "Compilation preserves variable names" $ property $ \varName value -> do
      let validVarName = take 10 $ filter (\c -> c /= '\0' && c /= '\n' && c /= '\r') varName
          validValue = take 10 $ filter (\c -> c /= '\0' && c /= '\n' && c /= '\r') value
          content = "let " ++ validVarName ++ " = " ++ validValue
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          property $ validVarName `L.isInfixOf` goCode

  , testProperty "Compilation preserves function names" $ property $ \funcName -> do
      let validFuncName = take 10 $ filter (\c -> c /= '\0' && c /= '\n' && c /= '\r' && c /= ' ') funcName
          content = "func " ++ validFuncName ++ "() {\n  return 42\n}"
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          property $ validFuncName `L.isInfixOf` goCode

  , testProperty "Compilation handles multiple statements" $ property $ \(stmtCount :: Int) -> do
      let validCount = max 1 (min 10 (abs stmtCount))
          statements = map (\i -> "let x" ++ show i ++ " = " ++ show i) [1..validCount]
          content = unlines statements
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          property $ not $ null goCode

  , testProperty "Generated Go code is non-empty for valid Typus code" $ property $ \content -> do
      let validContent = filter (\c -> c /= '\0') content
      case parseTypus validContent of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          property $ not $ null goCode

  , testProperty "Error messages contain expected phases" $ property $ \phase -> do
      let defaultLoc = ErrorLocation Nothing 0 0 Nothing Nothing
      let typeError = errorWithCategory "TEST001" Parsing (T.pack "Test error") defaultLoc
      let compError = CompilerError typeError Nothing [] phase
      property $ cePhase compError === phase
  ]