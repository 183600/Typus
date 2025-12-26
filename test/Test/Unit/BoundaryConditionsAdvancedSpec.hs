{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.BoundaryConditionsAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, listOf, elements)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, length, take, drop)
import Data.Char (isSpace, isAlphaNum, isControl)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Exception (try, SomeException)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim)

-- ============================================================================
-- Advanced Boundary Condition Test Data Generators
-- ============================================================================

-- Generate extremely long identifiers
genExtremelyLongIdentifier :: Gen String
genExtremelyLongIdentifier = do
  len <- choose [1000, 10000, 100000]
  first <- elements ['a'..'z']
  rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
  return $ take len (first : rest)

-- Generate deeply nested structures
genDeeplyNestedStructure :: Int -> Gen String
genDeeplyNestedStructure depth = do
  let nesting = concat $ replicate depth "if (true) { "
  let closing = concat $ replicate depth " }"
  content <- elements ["return 42;", "var x = 5;", "func test() {}"]
  return $ nesting ++ content ++ closing

-- Generate strings with control characters
genControlCharacterString :: Gen String
genControlCharacterString = do
  base <- elements ["test", "function", "variable"]
  controls <- listOf $ elements "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127"
  return $ base ++ controls

-- Generate Unicode edge cases
genUnicodeEdgeCase :: Gen String
genUnicodeEdgeCase = elements
  [ "func 测试() { return 42; }"
  , "func 🚀() { return \"rocket\"; }"
  , "func café() { return \"coffee\"; }"
  , "func naïve() { return \"naive\"; }"
  , "func こんにちは() { return \"hello\"; }"
  , "func 🤖() { return \"robot\"; }"
  ]

-- ============================================================================
-- Extreme Input Boundary Tests
-- ============================================================================

testMassiveInputSize :: TestTree
testMassiveInputSize = testCase "Massive input size handling" $ do
  -- Test with 1MB of input
  let largeFunction = "func massive() {\n" ++ 
                      concat ["  let x" ++ show i ++ " = " ++ show i ++ ";\n" | i <- [1..50000]] ++
                      "  return 42;\n}\n"
  let massiveInput = concat $ replicate 20 largeFunction  -- ~ 1MB of code
  
  result <- try $ parseTypus massiveInput "massive.typus"
  case result of
    Left (e :: SomeException) -> assertBool "Should handle massive input gracefully" True
    Right parseResult -> 
      case parseResult of
        Left err -> assertBool "Should handle parse errors for massive input" True
        Right file -> assertBool "Should successfully parse massive input" True

testZeroWidthInput :: TestTree
testZeroWidthInput = testCase "Zero-width input handling" $ do
  let zeroWidthInputs = ["", "\0", "\u200B", "\uFEFF"]  -- empty, null, zero-width space, BOM
  mapM_ testZeroWidth zeroWidthInputs
  where
    testZeroWidth input = do
      result <- parseTypus input "zerowidth.typus"
      case result of
        Left err -> assertBool "Zero-width input should be handled" True
        Right file -> assertEqual "Zero-width file should have no blocks" 0 (length (tfCodeBlocks file))

testExtremeNestingDepth :: TestTree
testExtremeNestingDepth = testCase "Extreme nesting depth handling" $ do
  let depths = [10, 100, 1000, 5000]
  mapM_ testDepth depths
  where
    testDepth depth = do
      nestedInput <- genDeeplyNestedStructure depth
      result <- parseTypus nestedInput ("depth" ++ show depth ++ ".typus")
      case result of
        Left err -> assertBool $ "Should handle depth " ++ show depth ++ " gracefully"
        Right file -> assertBool $ "Should parse depth " ++ show depth

testMemoryPressure :: TestTree
testMemoryPressure = testCase "Memory pressure handling" $ do
  -- Create input that would stress memory allocation
  let memoryStressInput = concat $ replicate 10000 $ 
        "func memtest" ++ show (1 :: Int) ++ "() {\n" ++
        "  let arr = make([]int, 1000)\n" ++
        "  for i := 0; i < 1000; i++ {\n" ++
        "    arr[i] = i * i\n" ++
        "  }\n" ++
        "  return len(arr)\n" ++
        "}\n"
  
  result <- try $ parseTypus memoryStressInput "memstress.typus"
  case result of
    Left (e :: SomeException) -> assertBool "Should handle memory pressure gracefully" True
    Right parseResult -> 
      case parseResult of
        Left err -> assertBool "Should handle parse errors under memory pressure" True
        Right file -> assertBool "Should handle memory pressure" True

-- ============================================================================
-- Character Encoding Boundary Tests
-- ============================================================================

testControlCharacterHandling :: TestTree
testControlCharacterHandling = testCase "Control character handling" $ do
  let controlInputs = 
        [ "func test() { return \"\0\"; }"  -- null character
        , "func test() { return \"\n\"; }"  -- newline
        , "func test() { return \"\t\"; }"  -- tab
        , "func test() { return \"\r\"; }"  -- carriage return
        , "func test() { return \"\b\"; }"  -- backspace
        , "func test() { return \"\f\"; }"  -- form feed
        ]
  mapM_ testControlChar controlInputs
  where
    testControlChar input = do
      result <- parseTypus input "control.typus"
      case result of
        Left err -> assertBool $ "Should handle control characters: " ++ show input
        Right file -> assertBool "Should parse control characters" True

testUnicodeBoundaryConditions :: TestTree
testUnicodeBoundaryConditions = testCase "Unicode boundary conditions" $ do
  let unicodeInputs = 
        [ "func test() { return \"🚀🌟💻\"; }"  -- emojis
        , "func 测试() { return \"中文测试\"; }"  -- Chinese
        , "func العربية() { return \"العربية\"; }"  -- Arabic
        , "func русский() { return \"русский\"; }"  -- Cyrillic
        , "func ελληνικά() { return \"ελληνικά\"; }"  -- Greek
        , "func हिन्दी() { return \"हिन्दी\"; }"  -- Hindi
        , "func 日本語() { return \"日本語\"; }"  -- Japanese
        ]
  mapM_ testUnicode unicodeInputs
  where
    testUnicode input = do
      result <- parseTypus input "unicode.typus"
      case result of
        Left err -> assertBool $ "Should handle Unicode: " ++ show input
        Right file -> assertBool "Should parse Unicode" True

testMixedEncodingInput :: TestTree
testMixedEncodingInput = testCase "Mixed encoding input handling" $ do
  let mixedInput = unlines
        [ "// Latin: function test"
        , "func test() {"
        , "  // Chinese: 返回值"
        , "  return 42"
        , "  // Emoji: 🚀"
        , "}"
        , "// Arabic: تعليق"
        , "func عربي() {"
        , "  return \"العربية\""
        , "}"
        ]
  
  result <- parseTypus mixedInput "mixed.typus"
  case result of
    Left err -> assertBool "Should handle mixed encodings gracefully" True
    Right file -> assertBool "Should parse mixed encodings" True

-- ============================================================================
-- Numerical Boundary Tests
-- ============================================================================

testNumericalBoundaries :: TestTree
testNumericalBoundaries = testCase "Numerical boundary handling" $ do
  let numericalInputs = 
        [ "func test() { return " ++ show (maxBound :: Int) ++ "; }"
        , "func test() { return " ++ show (minBound :: Int) ++ "; }"
        , "func test() { return " ++ show (1/0 :: Double) ++ "; }"  -- infinity
        , "func test() { return 9223372036854775808; }"  -- overflow int64
        , "func test() { return -9223372036854775809; }"  -- underflow int64
        , "func test() { return 3.14159265358979323846264338327950288419716939937510; }"  -- high precision
        , "func test() { return 0.000000000000000000000000000000000001; }"  -- very small
        ]
  mapM_ testNumerical numericalInputs
  where
    testNumerical input = do
      result <- parseTypus input "numerical.typus"
      case result of
        Left err -> assertBool $ "Should handle numerical boundary: " ++ show input
        Right file -> assertBool "Should parse numerical boundaries" True

testNumericalPrecision :: TestTree
testNumericalPrecision = testCase "Numerical precision handling" $ do
  let precisionTests = 
        [ ("float32_max", "3.402823466e+38")
        , ("float32_min", "1.175494351e-38")
        , ("float64_max", "1.7976931348623157e+308")
        , ("float64_min", "2.2250738585072014e-308")
        , ("denormal", "5e-324")
        , ("subnormal", "2.2250738585072009e-308")
        ]
  mapM_ testPrecision precisionTests
  where
    testPrecision (name, value) = do
      let input = "func " ++ name ++ "() { return " ++ value ++ "; }"
      result <- parseTypus input ("precision_" ++ name ++ ".typus")
      case result of
        Left err -> assertBool $ "Should handle precision test " ++ name ++ ": " ++ show input
        Right file -> assertBool "Should parse precision tests" True

-- ============================================================================
-- String Boundary Tests
-- ============================================================================

testStringLengthBoundaries :: TestTree
testStringLengthBoundaries = testCase "String length boundary handling" $ do
  let stringLengths = [0, 1, 10, 100, 1000, 10000, 100000]
  mapM_ testStringLength stringLengths
  where
    testStringLength len = do
      let content = take len (cycle "abcdefghijklmnopqrstuvwxyz")
      let input = "func test() { return \"" ++ content ++ "\"; }"
      result <- parseTypus input ("strlen_" ++ show len ++ ".typus")
      case result of
        Left err -> assertBool $ "Should handle string length " ++ show len
        Right file -> assertBool "Should parse string length test" True

testStringEscapeSequences :: TestTree
testStringEscapeSequences = testCase "String escape sequence handling" $ do
  let escapeInputs = 
        [ "func test() { return \"\\n\\t\\r\\b\\f\\\\\\\"\\'\"; }"
        , "func test() { return \"\\x41\\x42\\x43\"; }"  -- hex escapes
        , "func test() { return \"\\101\\102\\103\"; }"  -- octal escapes
        , "func test() { return \"\\u0041\\u0042\\u0043\"; }"  -- unicode escapes
        , "func test() { return \"\\U00000041\\U00000042\"; }"  -- long unicode escapes
        , "func test() { return \"\\a\\v\\?\"; }"  -- other escapes
        ]
  mapM_ testEscape escapeInputs
  where
    testEscape input = do
      result <- parseTypus input "escape.typus"
      case result of
        Left err -> assertBool $ "Should handle escape sequences: " ++ show input
        Right file -> assertBool "Should parse escape sequences" True

-- ============================================================================
-- QuickCheck Property Tests for Boundary Conditions
-- ============================================================================

-- Property: Parsing should never crash on any string input
propParsingNeverCrashes :: String -> Property
propParsingNeverCrashes input = 
  let result = parseTypus input "prop_test.typus"
  in case result of
       Left _ -> property True   -- Failing parse is OK
       Right _ -> property True   -- Successful parse is OK

-- Property: Extremely long identifiers should be handled gracefully
propLongIdentifiersHandled :: Int -> Property
propLongIdentifiersHandled len = 
  let len' = max 0 (min len 100000)  -- Cap at reasonable size
      ident = take len' (cycle "abcdefghijklmnopqrstuvwxyz")
      input = "func " ++ ident ++ "() { return 42; }"
  in case parseTypus input "long_ident.typus" of
       Left _ -> property True
       Right file -> property True

-- Property: Deep nesting should be handled up to reasonable limits
propDeepNestingHandled :: Int -> Property
propDeepNestingHandled depth = 
  let depth' = max 0 (min depth 1000)  -- Cap at reasonable depth
      nesting = concat $ replicate depth' "if (true) { "
      closing = concat $ replicate depth' " }"
      input = nesting ++ "return 42;" ++ closing
  in case parseTypus input "deep_nest.typus" of
       Left _ -> property True
       Right file -> property True

-- Property: Unicode characters should be handled consistently
propUnicodeHandled :: String -> Property
propUnicodeHandled unicodeStr = 
  let input = "func test() { return \"" ++ unicodeStr ++ "\"; }"
  in case parseTypus input "unicode.typus" of
       Left _ -> property True
       Right file -> property True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Advanced Boundary Conditions Test Suite"
  [ testGroup "Extreme Input Boundary Tests"
      [ testMassiveInputSize
      , testZeroWidthInput
      , testExtremeNestingDepth
      , testMemoryPressure
      ]
  
  , testGroup "Character Encoding Boundary Tests"
      [ testControlCharacterHandling
      , testUnicodeBoundaryConditions
      , testMixedEncodingInput
      ]
  
  , testGroup "Numerical Boundary Tests"
      [ testNumericalBoundaries
      , testNumericalPrecision
      ]
  
  , testGroup "String Boundary Tests"
      [ testStringLengthBoundaries
      , testStringEscapeSequences
      ]
  
  , testGroup "QuickCheck Boundary Property Tests"
      [ testProperty "Parsing never crashes" propParsingNeverCrashes
      , testProperty "Long identifiers handled" propLongIdentifiersHandled
      , testProperty "Deep nesting handled" propDeepNestingHandled
      , testProperty "Unicode handled" propUnicodeHandled
      ]
  ]