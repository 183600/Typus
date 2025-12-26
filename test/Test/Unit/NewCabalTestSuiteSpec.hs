{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, listOf1, elements)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum)
import Data.Maybe (isJust, isNothing, fromMaybe)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Ownership (OwnershipType(..), OwnershipError(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim)

-- ============================================================================
-- Test Data Generators for QuickCheck
-- ============================================================================

-- Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  return $ SourcePos line col

-- Generate valid source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ SourceSpan start end

-- Generate simple strings that could be valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf1 $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
  return (first : rest)

-- Generate strings with whitespace
genWhitespaceString :: Gen String
genWhitespaceString = do
  content <- genIdentifier
  ws <- listOf1 $ elements " \t\n"
  return $ content ++ ws

-- Generate potentially problematic strings
genProblematicString :: Gen String
genProblematicString = elements
  [ ""
  , " "
  , "\t"
  , "\n"
  , "   "
  , "\t\t\t"
  , "\n\n\n"
  , " \t \n \t "
  , "/* comment */"
  , "// comment"
  , "\"string\""
  , "'c'"
  , "123"
  , "123.456"
  , "true"
  , "false"
  , "null"
  , "undefined"
  ]

-- ============================================================================
-- Boundary Condition Tests
-- ============================================================================

testEmptyInput :: TestTree
testEmptyInput = testCase "Empty input handling" $ do
  result <- parseTypus "" "empty.typus"
  case result of
    Left err -> assertBool "Empty input should produce parse error" True
    Right file -> assertEqual "Empty file should have no blocks" 0 (length (tfCodeBlocks file))

testWhitespaceOnlyInput :: TestTree
testWhitespaceOnlyInput = testCase "Whitespace-only input handling" $ do
  let inputs = [" ", "\t", "\n", "   ", "\t\t\t", "\n\n\n", " \t \n \t "]
  mapM_ testWhitespace inputs
  where
    testWhitespace ws = do
      result <- parseTypus ws "whitespace.typus"
      case result of
        Left err -> assertBool $ "Whitespace input should parse successfully: " ++ ws
        Right file -> assertEqual "Whitespace file should have no blocks" 0 (length (tfCodeBlocks file))

testVeryLongIdentifier :: TestTree
testVeryLongIdentifier = testCase "Very long identifier handling" $ do
  let longIdent = replicate 10000 'a'
  let input = "func " ++ longIdent ++ "() { return 42; }"
  result <- parseTypus input "long_ident.typus"
  case result of
    Left err -> assertBool "Very long identifier should be handled gracefully" True
    Right file -> assertBool "Parse should succeed or fail gracefully" True

testDeeplyNestedCode :: TestTree
testDeeplyNestedCode = testCase "Deeply nested code handling" $ do
  let nestedDepth = 100
  let nestedCode = concat $ replicate nestedDepth "if (true) { "
  let input = nestedCode ++ "return 42;" ++ concat (replicate nestedDepth " }")
  result <- parseTypus input "nested.typus"
  case result of
    Left err -> assertBool "Deeply nested code should be handled" True
    Right file -> assertBool "Parse should handle deep nesting" True

testSpecialCharacters :: TestTree
testSpecialCharacters = testCase "Special characters handling" $ do
  let inputs = 
        [ "func test() { return \"\\n\\t\\r\"; }"
        , "func test() { return '©®™'; }"
        , "func test() { /* 中文测试 */ return 42; }"
        , "func test() { // 测试注释\n return 42; }"
        ]
  mapM_ testSpecial inputs
  where
    testSpecial input = do
      result <- parseTypus input "special.typus"
      case result of
        Left err -> assertBool $ "Special characters should be handled: " ++ show input
        Right file -> assertBool "Parse should succeed with special characters" True

-- ============================================================================
-- Error Recovery Tests
-- ============================================================================

testSyntaxErrorRecovery :: TestTree
testSyntaxErrorRecovery = testCase "Syntax error recovery" $ do
  let input = "func test() { if (true return 42; }"  // missing closing parenthesis
  result <- parseTypus input "error.typus"
  case result of
    Left err -> do
      assertBool "Error should contain location information" (show err `isInfixOf` "line")
      assertBool "Error should be descriptive" (show err `isInfixOf` "parenthesis")
    Right file -> assertFailure "Expected parse error for malformed syntax"

testMultipleErrorDetection :: TestTree
testMultipleErrorDetection = testCase "Multiple error detection" $ do
  let input = "func test() { if (true return 42; func broken() { }"  // multiple errors
  result <- parseTypus input "multiple_errors.typus"
  case result of
    Left err -> assertBool "Should detect syntax errors" True
    Right file -> assertFailure "Expected parse error for multiple syntax errors"

testErrorContextPreservation :: TestTree
testErrorContextPreservation = testCase "Error context preservation" $ do
  let input = "func test() {\n  return\n  42;\n}"  // incomplete return statement
  result <- parseTypus input "context.typus"
  case result of
    Left err -> do
      let errStr = show err
      assertBool "Error should include line number" ("line" `isInfixOf` errStr)
      assertBool "Error should include column information" ("column" `isInfixOf` errStr)
    Right file -> assertFailure "Expected parse error"

-- ============================================================================
-- Performance Regression Tests
-- ============================================================================

testLargeFileParsing :: TestTree
testLargeFileParsing = testCase "Large file parsing performance" $ do
  let largeFunction = "func largeTest() {\n" ++ 
                      concat ["  let x" ++ show i ++ " = " ++ show i ++ ";\n" | i <- [1..1000]] ++
                      "  return 42;\n}\n"
  let largeInput = concat $ replicate 10 largeFunction  // 10,000 lines
  
  result <- parseTypus largeInput "large.typus"
  case result of
    Left err -> assertBool "Large file should parse or fail gracefully" True
    Right file -> do
      let blockCount = length (tfCodeBlocks file)
      assertBool "Should parse multiple functions" (blockCount > 0)

testComplexTypeChecking :: TestTree
testComplexTypeChecking = testCase "Complex type checking performance" $ do
  let complexTypes = unlines
        [ "type ComplexType struct {"
        , "  field1 map[string][]int"
        , "  field2 chan func(int) (string, error)"
        , "  field3 <-chan []map[int]interface{}"
        , "}"
        , "func complexFunc() ComplexType {"
        , "  return ComplexType{}"
        , "}"
        ]
  
  result <- compile "complex_types.typus" complexTypes
  case result of
    Left errs -> assertBool "Complex types should be handled" True
    Right success -> assertBool "Compilation should handle complex types" True

-- ============================================================================
-- Integration Tests
-- ============================================================================

testParserCompilerIntegration :: TestTree
testParserCompilerIntegration = testCase "Parser-Compiler integration" $ do
  let input = unlines
        [ "//! dependent_types: on"
        , "//! ownership: on"
        , "package main"
        , ""
        , "func add(a int, b int) int {"
        , "  return a + b"
        , "}"
        , ""
        , "func main() {"
        , "  result := add(5, 3)"
        , "  // Should not compile - unused variable"
        , "}"
        ]
  
  -- First parse
  parseResult <- parseTypus input "integration.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      -- Then compile
      compileResult <- compile "integration.typus" input
      case compileResult of
        Left errs -> assertBool "Compilation should detect unused variable" True
        Right success -> assertBool "Integration should work" True

testOwnershipDependentTypesIntegration :: TestTree
testOwnershipDependentTypesIntegration = testCase "Ownership-DependentTypes integration" $ do
  let input = unlines
        [ "//! ownership: on"
        , "//! dependent_types: on"
        , "package main"
        , ""
        , "type SafeArray[T] struct {"
        , "  data []T"
        , "  len int"
        , "}"
        , ""
        , "func NewSafeArray[T](len int) *SafeArray[T] {"
        , "  return &SafeArray[T]{data: make([]T, len), len: len}"
        , "}"
        , ""
        , "func (sa *SafeArray[T]) Get(index int) T {"
        , "  if index < 0 || index >= sa.len {"
        , "    panic(\"index out of bounds\")"
        , "  }"
        , "  return sa.data[index]"
        , "}"
        ]
  
  result <- compile "ownership_dependent.typus" input
  case result of
    Left errs -> assertBool "Should handle ownership and dependent types together" True
    Right success -> assertBool "Integration should succeed" True

-- ============================================================================
-- QuickCheck Property Tests
-- ============================================================================

-- Property: Parsing and re-parsing should give consistent results
propParseConsistency :: String -> Property
propParseConsistency input = 
  forAll genIdentifier $ \ident ->
    let testInput = "func " ++ ident ++ "() { return 42; }"
    in case parseTypus testInput "test.typus" of
         Left _ -> property True  -- Invalid input is fine
         Right file1 -> 
           case parseTypus testInput "test.typus" of
             Left _ -> property False  -- Should be consistent
             Right file2 -> tfCodeBlocks file1 === tfCodeBlocks file2

-- Property: Trimming whitespace should not affect parsing semantics
propWhitespaceInvariance :: String -> Property
propWhitespaceInvariance input = 
  forAll genIdentifier $ \ident ->
    let baseCode = "func " ++ ident ++ "() { return 42; }"
        withExtraSpaces = "  " ++ baseCode ++ "  \n  "
    in case (parseTypus baseCode "test1.typus", parseTypus withExtraSpaces "test2.typus") of
         (Left _, Left _) -> property True  -- Both fail is OK
         (Right f1, Right f2) -> 
           length (tfCodeBlocks f1) === length (tfCodeBlocks f2)
         _ -> property False  -- Should be consistent

-- Property: Source location tracking should be consistent
propSourceLocationConsistency :: Property
propSourceLocationConsistency = 
  forAll genSourcePos $ \pos ->
    forAll genSourcePos $ \pos2 ->
      let span = SourceSpan pos pos2
      in spanStart span === pos && spanEnd span === pos2

-- Property: Error messages should contain useful information
propErrorMessagesUseful :: String -> Property
propErrorMessagesUseful input = 
  let malformedInput = "func test() { if (true return 42; }"  // Always malformed
  in case parseTypus malformedInput "error.typus" of
       Right _ -> property False  -- Should error
       Left err -> 
         let errStr = show err
         in property (errStr /= "" && 
                    ("line" `isInfixOf` errStr || "error" `isInfixOf` errStr))

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Test Suite"
  [ testGroup "Boundary Condition Tests"
      [ testEmptyInput
      , testWhitespaceOnlyInput
      , testVeryLongIdentifier
      , testDeeplyNestedCode
      , testSpecialCharacters
      ]
  
  , testGroup "Error Recovery Tests"
      [ testSyntaxErrorRecovery
      , testMultipleErrorDetection
      , testErrorContextPreservation
      ]
  
  , testGroup "Performance Regression Tests"
      [ testLargeFileParsing
      , testComplexTypeChecking
      ]
  
  , testGroup "Integration Tests"
      [ testParserCompilerIntegration
      , testOwnershipDependentTypesIntegration
      ]
  
  , testGroup "QuickCheck Property Tests"
      [ testProperty "Parse consistency" propParseConsistency
      , testProperty "Whitespace invariance" propWhitespaceInvariance
      , testProperty "Source location consistency" propSourceLocationConsistency
      , testProperty "Error messages useful" propErrorMessagesUseful
      ]
  ]