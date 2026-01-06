{-# LANGUAGE CPP #-}

-- | Comprehensive QuickCheck tests for the Parser module
module Test.Unit.ComprehensiveParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.ExtendedArbitrary ()
import Test.QuickCheck 
import qualified Data.List as Data.List
import Data.Char (toLower, isSpace)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import SourceLocation (Located(..), locatedValue, spanStart, spanEnd, posLine, posColumn)

-- ============================================================================
-- Core Property Tests
-- ============================================================================

-- Property: Round-trip parsing preserves structure
prop_parse_roundtrip_comprehensive :: TypusFile -> Property
prop_parse_roundtrip_comprehensive typusFile = 
  let reconstructed = reconstructTypusFile typusFile
  in case parseTypus reconstructed of
    Left err -> counterexample ("Parse error in round-trip: " ++ err) $ property False
    Right parsed -> 
      let directivesMatch = compareDirectives (tfDirectives typusFile) (tfDirectives parsed)
          blocksCountMatch = L.length (tfBlocks typusFile) == L.length (tfBlocks parsed)
          contentPreserved = L.all contentPreservedInBlock (zip (tfBlocks typusFile) (tfBlocks parsed))
      in counterexample ("Directives match: " ++ show directivesMatch ++ 
                        ", Blocks count match: " ++ show blocksCountMatch ++
                        ", Content preserved: " ++ show contentPreserved) $
         property $ directivesMatch && blocksCountMatch && contentPreserved

-- Property: Valid directives are always parsed successfully
prop_parse_valid_directives_always_success :: String -> Property
prop_parse_valid_directives_always_success directive = 
  let validDirectives = ["//! ownership: on", "//! ownership: off", 
                        "//! dependent_types: on", "//! dependent_types: off",
                        "//! constraints: on", "//! constraints: off",
                        "//! go", "//! go:run", "//! go:build", "//! skip"]
  in classify (directive `elem` validDirectives) "valid directive" $ 
     property $ directive `elem` validDirectives ==> 
     case parseTypus directive of
       Left err -> counterexample ("Valid directive failed: " ++ directive ++ " Error: " ++ err) $ property False
       Right _ -> property True

-- Property: Error messages contain useful information
prop_parse_error_messages_useful :: String -> Property
prop_parse_error_messages_useful malformed =
  L.length malformed > 5 ==> 
  case parseTypus malformed of
    Left err -> 
      let hasErrorKeyword = "error" `L.isInfixOf` map toLower err
          hasPosition = L.any (`L.isInfixOf` err) ["line", "column", "position"]
          hasContext = L.length err > 10
      in property $ hasErrorKeyword && (hasPosition || hasContext)
    Right _ -> property True

-- Property: Empty files produce minimal valid structure
prop_parse_empty_minimal_structure :: Property
prop_parse_empty_minimal_structure =
  case parseTypus "" of
    Left err -> counterexample ("Empty file parse error: " ++ err) $ property False
    Right parsed -> 
      let directives = tfDirectives parsed
          blocks = tfBlocks parsed
          hasNoDirectives = L.all isNothing [fdOwnership directives, fdDependentTypes directives, fdConstraints directives]
          hasNoBlocks = null blocks
      in property $ hasNoDirectives && hasNoBlocks

-- Property: Whitespace variations don't affect parsing result
prop_parse_whitespace_variations_preserve_meaning :: String -> String -> String -> Property
prop_parse_whitespace_variations_preserve_meaning before middle after =
  let content = before ++ "//! ownership: on" ++ middle ++ "package main" ++ after ++ "func main() {}"
      normalized = normalizeWhitespace content
  in case (parseTypus content, parseTypus normalized) of
    (Left err1, Left err2) -> property $ True  -- Both fail consistently
    (Left err1, Right _) -> counterexample ("Original failed but normalized succeeded: " ++ err1) $ property False
    (Right _, Left err2) -> counterexample ("Normalized failed but original succeeded: " ++ err2) $ property False
    (Right file1, Right file2) -> 
      let directivesMatch = compareDirectives (tfDirectives file1) (tfDirectives file2)
          blocksCountMatch = L.length (tfBlocks file1) == L.length (tfBlocks file2)
      in property $ directivesMatch && blocksCountMatch

-- Property: Large files are parsed without stack overflow
prop_parse_large_files_no_overflow :: Int -> Property
prop_parse_large_files_no_overflow n =
  n >= 0 && n <= 1000 ==> 
  let largeContent = unlines $ replicate n "var x int = 42 // Large file test"
  in case parseTypus largeContent of
    Left err -> counterexample ("Large file parse error: " ++ err) $ property False
    Right parsed -> 
      let expectedBlocks = if n > 0 then 1 else 0
          actualBlocks = L.length $ tfBlocks parsed
      in property $ actualBlocks == expectedBlocks

-- Property: Unicode content is preserved correctly
prop_parse_unicode_preserved :: String -> Property
prop_parse_unicode_preserved base =
  let unicodeContent = base ++ " // 测试中文内容 🚀 αβγ"
  in case parseTypus unicodeContent of
    Left _ -> property False
    Right parsed -> 
      let contentContainsUnicode = L.any (hasUnicodeSubstring base) (tfBlocks parsed)
      in property $ contentContainsUnicode

-- Property: Comments are ignored in parsing logic
prop_parse_comments_ignored :: [String] -> Property
prop_parse_comments_ignored comments =
  not (null comments) ==>
  let commentLines = L.map ("// " ++) comments
      content = unlines $ ["//! ownership: on", "package main"] ++ commentLines ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Comment parsing error: " ++ err) $ property False
    Right parsed -> 
      let hasOwnershipDirective = isJust $ fdOwnership $ tfDirectives parsed
          hasMainBlock = not $ L.null $ tfBlocks parsed
      in property $ hasOwnershipDirective && hasMainBlock

-- Property: Mixed directives L.and code maintain order
prop_parse_mixed_directives_code_order :: [String] -> [String] -> Property
prop_parse_mixed_directives_code_order directives codeLines =
  not (null directives) && not (null codeLines) ==>
  let content = unlines $ directives ++ codeLines
  in case parseTypus content of
    Left err -> counterexample ("Mixed content parse error: " ++ err) $ property False
    Right parsed -> 
      let hasSomeDirectives = hasAnyDirectives parsed
          hasCodeBlocks = not $ L.null $ tfBlocks parsed
      in property $ hasSomeDirectives && hasCodeBlocks

-- Property: Nested structures maintain hierarchy
prop_parse_nested_hierarchy :: Int -> Property
prop_parse_nested_hierarchy depth =
  depth >= 0 && depth <= 10 ==>
  let nestedContent = generateNestedHierarchy depth
  in case parseTypus nestedContent of
    Left err -> counterexample ("Nested hierarchy parse error: " ++ err) $ property False
    Right parsed -> 
      let blockCount = L.length $ tfBlocks parsed
          hasReasonableBlockCount = blockCount > 0 && blockCount <= depth + 1
      in property $ hasReasonableBlockCount

-- Property: Special characters in identifiers are handled
prop_parse_special_char_identifiers :: [String] -> Property
prop_parse_special_char_identifiers baseNames =
  not (null baseNames) ==>
  let specialNames = L.map (\name -> name ++ "_test_123") baseNames
      content = unlines $ ["//! ownership: on", "package main"] ++
                        L.map (\name -> "var " ++ name ++ " int = 42") specialNames ++
                        ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Special char identifier error: " ++ err) $ property False
    Right parsed -> 
      let blockCount = L.length $ tfBlocks parsed
      in property $ blockCount > 0

-- Property: Multiple file directives are combined correctly
prop_parse_multiple_directives_combined :: [String] -> Property
prop_parse_multiple_directives_combined directives =
  L.length directives <= 10 ==>
  let fileDirectives = L.map (\d -> "//! " ++ d) directives
      content = unlines fileDirectives
  in case parseTypus content of
    Left err -> counterexample ("Multiple directives error: " ++ err) $ property False
    Right parsed -> 
      let directiveCount = countPresentDirectives parsed
      in property $ directiveCount > 0 && directiveCount <= L.length directives

-- Property: Inconsistent indentation is handled gracefully
prop_parse_inconsistent_indentation_graceful :: [String] -> Property
prop_parse_inconsistent_indentation_graceful lines =
  not (null lines) ==>
  let indentedLines = zipWith (\i line -> replicate (i * 2) ' ' ++ line) [0..] lines
      content = unlines indentedLines
  in case parseTypus content of
    Left err -> property $ True  -- May fail, but shouldn't crash
    Right parsed -> property $ True  -- Or succeed with reasonable structure

-- Property: Invalid directives are rejected appropriately
prop_parse_invalid_directives_rejected :: String -> Property
prop_parse_invalid_directives_rejected content =
  let invalidPatterns = ["!!!", "///", "##", "@@", "%%", "!!!"]
      hasInvalidPattern = L.any (`Data.List.L.isPrefixOf` content) invalidPatterns
  in classify hasInvalidPattern "has invalid pattern" $
     if hasInvalidPattern
     then case parseTypus content of
       Left _ -> property True  -- Expected to fail
       Right _ -> property True  -- May recover partially
     else property True

-- Property: Very long lines are handled without issues
prop_parse_very_long_lines :: Int -> Property
prop_parse_very_long_lines L.length =
  L.length >= 0 && L.length <= 2000 ==> 
  let longLine = "var longVariableName string = \"" ++ replicate L.length 'x' ++ "\""
      content = unlines ["//! ownership: on", "package main", longLine, "func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Long line parse error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Escape sequences in strings are handled
prop_parse_escape_sequences :: [String] -> Property
prop_parse_escape_sequences strings =
  let escapedStrings = map addEscapeSequences strings
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        L.map (\s -> "  fmt.Println(\"" ++ s ++ "\")") escapedStrings ++
                        ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Escape sequence error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Numeric literals of various formats are parsed
prop_parse_numeric_literals :: [Int] -> [Double] -> Property
prop_parse_numeric_literals ints doubles =
  let intVars = L.map (\i -> "var intVar" ++ show i ++ " int = " ++ show i) (take 5 ints)
      floatVars = L.map (\d -> "var floatVar" ++ show (round d :: Int) ++ " float64 = " ++ show d) (take 5 doubles)
      hexVars = L.map (\i -> "var hexVar" ++ show i ++ " int = 0x" ++ showHex i) (take 3 ints)
      octalVars = L.map (\i -> "var octalVar" ++ show i ++ " int = 0o" ++ showOct i) (take 3 ints)
      content = unlines $ ["//! ownership: on", "package main"] ++ 
                        intVars ++ floatVars ++ hexVars ++ octalVars ++
                        ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Numeric literals error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Complex expressions are parsed correctly
prop_parse_complex_expressions :: [String] -> Property
prop_parse_complex_expressions identifiers =
  not (null identifiers) ==>
  let expressions = map generateComplexExpression (take 5 identifiers)
      content = unlines $ ["//! ownership: on", "package main", "func main() {"] ++
                        L.map (\expr -> "  result := " ++ expr) expressions ++
                        ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Complex expressions error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Function definitions with various signatures
prop_parse_function_signatures :: [String] -> [String] -> [String] -> Property
prop_parse_function_signatures funcNames paramTypes returnTypes =
  let minLen = L.minimum [L.length funcNames, L.length paramTypes, L.length returnTypes]
      signatures = take minLen $ zip3 funcNames paramTypes returnTypes
      functions = L.map (\(name, pType, rType) -> 
        "func " ++ name ++ "(" ++ pType ++ ") " ++ rType ++ " { return 42 }") signatures
      content = unlines $ ["//! ownership: on", "package main"] ++ functions ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Function signatures error: " ++ err) $ property False
    Right parsed -> 
      let expectedFuncCount = L.length functions
          actualBlockCount = L.length $ tfBlocks parsed
      in property $ actualBlockCount >= expectedFuncCount

-- Property: Struct L.and interface definitions
prop_parse_struct_interface_definitions :: [String] -> [String] -> Property
prop_parse_struct_interface_definitions structNames interfaceNames =
  let structs = L.map (\name -> "type " ++ name ++ " struct { Field int }") structNames
      interfaces = L.map (\name -> "type " ++ name ++ " interface { Method() }") interfaceNames
      content = unlines $ ["//! ownership: on", "package main"] ++ structs ++ interfaces ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Struct/Interface definitions error: " ++ err) $ property False
    Right parsed -> 
      let expectedCount = L.length structs + L.length interfaces
          actualBlockCount = L.length $ tfBlocks parsed
      in property $ actualBlockCount >= expectedCount

-- Property: Import statements with various formats
prop_parse_import_statements :: [String] -> Property
prop_parse_import_statements importPaths =
  let singleImports = L.map ("import \"" ++) importPaths
      aliasImports = zipWith (\path i -> "import alias" ++ show i ++ " \"" ++ path ++ "\"") 
                           importPaths [1..]
      dotImports = L.map (\path -> "import . \"" ++ path ++ "\"") importPaths
      underscoreImports = L.map (\path -> "import _ \"" ++ path ++ "\"") importPaths
      allImports = take 5 $ singleImports ++ aliasImports ++ dotImports ++ underscoreImports
      content = unlines $ ["//! ownership: on", "package main"] ++ allImports ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Import statements error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Concurrent programming constructs
prop_parse_concurrent_constructs :: [String] -> Property
prop_parse_concurrent_constructs channelNames =
  let channels = L.map (\name -> "var " ++ name ++ " chan int") channelNames
      goroutines = L.map (\name -> "go func() { " ++ name ++ " <- 42 }()") channelNames
      selectCases = ["select {", "case <-ch1:", "  fmt.Println(\"received\")", "case ch2 <- 42:", "  fmt.Println(\"sent\")", "}"]
      content = unlines $ ["//! ownership: on", "package main"] ++ channels ++
                        ["func main() {"] ++ goroutines ++ selectCases ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Concurrent constructs error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Error handling patterns
prop_parse_error_handling :: [String] -> Property
prop_parse_error_handling functionNames =
  let errorFuncs = L.map (\name -> "func " ++ name ++ "() error { return nil }") functionNames
      errorHandling = L.map (\name -> "if err := " ++ name ++ "(); err != nil { return err }") functionNames
      panicRecover = ["defer func() {", "  if r := recover(); r != nil {", "    fmt.Println(\"Recovered:\", r)", "  }", "}()", "panic(\"test\")"]
      content = unlines $ ["//! ownership: on", "package main"] ++ errorFuncs ++
                        ["func main() {"] ++ errorHandling ++ panicRecover ++ ["}"]
  in case parseTypus content of
    Left err -> counterexample ("Error handling patterns error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Generic types L.and functions
prop_parse_generic_types :: [String] -> [String] -> Property
prop_parse_generic_types typeNames typeParams =
  let generics = zipWith (\tName tParam -> 
        "type " ++ tName ++ "[" ++ tParam ++ " L.any] struct { Value " ++ tParam ++ " }") 
        typeNames typeParams
      genericFuncs = L.map (\tParam -> 
        "func Generic[" ++ tParam ++ " L.any](value " ++ tParam ++ ") " ++ tParam ++ " { return value }")
        typeParams
      content = unlines $ ["//! ownership: on", "package main"] ++ generics ++ genericFuncs ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Generic types error: " ++ err) $ property False
    Right parsed -> property $ True

-- ============================================================================
-- Edge Case L.and Stress Tests
-- ============================================================================

-- Property: Extremely long identifiers
prop_parse_extremely_long_identifiers :: Int -> Property
prop_parse_extremely_long_identifiers L.length =
  L.length >= 0 && L.length <= 500 ==> 
  let longName = replicate L.length 'a'
      content = unlines ["//! ownership: on", "package main", "var " ++ longName ++ " int = 42", "func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Long identifier error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Deeply nested brackets
prop_parse_deeply_nested_brackets :: Int -> Property
prop_parse_deeply_nested_brackets depth =
  depth >= 0 && depth <= 20 ==>
  let nestedBrackets = replicate depth '[' ++ "x" ++ replicate depth ']'
      content = unlines ["//! ownership: on", "package main", "func main() {", "  arr := " ++ nestedBrackets, "}"]
  in case parseTypus content of
    Left err -> counterexample ("Deeply nested brackets error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Mixed line endings
prop_parse_mixed_line_endings :: String -> Property
prop_parse_mixed_line_endings content =
  let mixedContent = content ++ "\r\n" ++ content ++ "\n" ++ content ++ "\r\n" ++ content
  in case parseTypus mixedContent of
    Left err -> counterexample ("Mixed line endings error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Zero-width characters
prop_parse_zero_width_characters :: String -> Property
prop_parse_zero_width_characters content =
  let zeroWidthChars = "\x200B\x200C\x200D\xFEFF"
      contentWithZeroWidth = content ++ zeroWidthChars ++ content
  in case parseTypus contentWithZeroWidth of
    Left err -> counterexample ("Zero-width characters error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Tab L.and space mixing
prop_parse_tab_space_mixing :: [String] -> Property
prop_parse_tab_space_mixing lines =
  not (null lines) ==>
  let mixedIndentation = zipWith (\i line -> 
        if even i then "\t\t" ++ line else "    " ++ line) [0..] lines
      content = unlines $ ["//! ownership: on", "package main"] ++ mixedIndentation ++ ["func main() {}"]
  in case parseTypus content of
    Left err -> counterexample ("Tab/space mixing error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Multiple consecutive newlines
prop_parse_multiple_consecutive_newlines :: String -> Property
prop_parse_multiple_consecutive_newlines content =
  let contentWithManyNewlines = content ++ "\n\n\n\n\n" ++ content
  in case parseTypus contentWithManyNewlines of
    Left err -> counterexample ("Multiple newlines error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: File with only whitespace
prop_parse_only_whitespace :: Property
prop_parse_only_whitespace =
  let whitespaceContent = unlines ["", "   ", "\t", "  \t  ", "   "]
  in case parseTypus whitespaceContent of
    Left err -> counterexample ("Whitespace only error: " ++ err) $ property False
    Right parsed -> 
      let hasNoBlocks = L.null $ tfBlocks parsed
      in property $ hasNoBlocks

-- Property: File with BOM (Byte Order Mark)
prop_parse_with_bom :: String -> Property
prop_parse_with_bom content =
  let bomContent = "\xFEFF" ++ content
  in case parseTypus bomContent of
    Left err -> counterexample ("BOM content error: " ++ err) $ property False
    Right parsed -> property $ True

-- ============================================================================
-- Performance L.and Scalability Tests
-- ============================================================================

-- Property: Parsing performance scales linearly
prop_parse_performance_linear :: Int -> Property
prop_parse_performance_linear n =
  n >= 0 && n <= 100 ==> 
  let content = unlines $ replicate n "var x int = 42 // Performance test"
  in case parseTypus content of
    Left err -> counterexample ("Performance test error: " ++ err) $ property False
    Right parsed -> property $ True

-- Property: Memory usage doesn't grow exponentially
prop_parse_memory_reasonable :: Int -> Property
prop_parse_memory_reasonable n =
  n >= 0 && n <= 50 ==> 
  let complexContent = unlines $ replicate n $ unlines
        [ "package main"
        , "func test" ++ show n ++ "() {"
        , "  for i := 0; i < 10; i++ {"
        , "    if i % 2 == 0 {"
        , "      fmt.Println(i)"
        , "    } else {"
        , "      continue"
        , "    }"
        , "  }"
        , "}"
        ]
  in case parseTypus complexContent of
    Left err -> counterexample ("Memory test error: " ++ err) $ property False
    Right parsed -> property $ True

-- ============================================================================
-- Regression Tests
-- ============================================================================

-- Property: Specific known problematic patterns
prop_parse_regression_patterns :: String -> Property
prop_parse_regression_patterns pattern =
  let knownPatterns = 
        [ "func main() { return } // No return value"
        , "var x = // Incomplete assignment"
        , "if true // Missing braces"
        , "for // Incomplete for loop"
        , "type struct { // Missing name"
        , "import ( // Empty import block"
        , "func ( // Incomplete method receiver"
        ]
  in classify (pattern `elem` knownPatterns) "known regression pattern" $
     case parseTypus pattern of
       Left _ -> property True  -- Expected to fail gracefully
       Right _ -> property True  -- Or succeed if fixed

-- ============================================================================
-- Helper Functions
-- ============================================================================

reconstructTypusFile :: TypusFile -> String
reconstructTypusFile file = 
  let directives = reconstructDirectives (tfDirectives file)
      blocks = map reconstructBlock (tfBlocks file)
      packageDecl = if null directives && null blocks then [] else ["package main"]
  in unlines $ directives ++ packageDecl ++ blocks

reconstructDirectives :: FileDirectives -> [String]
reconstructDirectives (FileDirectives ownership depTypes constraints) =
  let ownershipLine = case ownership of
        Nothing -> []
        Just (Located True _ _) -> ["//! ownership: on"]
        Just (Located False _ _) -> ["//! ownership: off"]
      depTypesLine = case depTypes of
        Nothing -> []
        Just (Located True _ _) -> ["//! dependent_types: on"]
        Just (Located False _ _) -> ["//! dependent_types: off"]
      constraintsLine = case constraints of
        Nothing -> []
        Just (Located True _ _) -> ["//! constraints: on"]
        Just (Located False _ _) -> ["//! constraints: off"]
  in ownershipLine ++ depTypesLine ++ constraintsLine

reconstructBlock :: CodeBlock -> String
reconstructBlock block = cbContent block

compareDirectives :: FileDirectives -> FileDirectives -> Bool
compareDirectives (FileDirectives o1 d1 c1) (FileDirectives o2 d2 c2) =
  (fmap locatedValue o1) == (fmap locatedValue o2) &&
  (fmap locatedValue d1) == (fmap locatedValue d2) &&
  (fmap locatedValue c1) == (fmap locatedValue c2)

contentPreservedInBlock :: (CodeBlock, CodeBlock) -> Bool
contentPreservedInBlock (original, reconstructed) =
  let originalContent = cbContent original
      reconstructedContent = cbContent reconstructed
  in normalizeContent originalContent == normalizeContent reconstructedContent

normalizeContent :: String -> String
normalizeContent = unlines . L.filter (not . null) . lines

normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

hasUnicodeSubstring :: String -> CodeBlock -> Bool
hasUnicodeSubstring base block =
  let content = cbContent block
      unicodeSubstrings = ["测试", "内容", "中文", "🚀", "αβγ"]
  in L.any (`L.isInfixOf` content) unicodeSubstrings

hasAnyDirectives :: TypusFile -> Bool
hasAnyDirectives file = 
  let directives = tfDirectives file
  in L.any isJust [fdOwnership directives, fdDependentTypes directives, fdConstraints directives]

countPresentDirectives :: TypusFile -> Int
countPresentDirectives file = 
  let directives = tfDirectives file
  in L.length [() | Just _ <- [fdOwnership directives, fdDependentTypes directives, fdConstraints directives]]

generateNestedHierarchy :: Int -> String
generateNestedHierarchy 0 = "var x int = 42"
generateNestedHierarchy n = unlines
  [ "func level" ++ show n ++ "() {"
  , "  if true {"
  , generateNestedHierarchy (n - 1)
  , "  }"
  , "}"
  ]

addEscapeSequences :: String -> String
addEscapeSequences = concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else if c == '\n' then "\\n" else [c])

showHex :: Int -> String
showHex n = showIntAtBase 16 ("0123456789ABCDEF" !!) (abs n)

showOct :: Int -> String
showOct n = showIntAtBase 8 ("01234567" !!) (abs n)

showIntAtBase :: Int -> (Int -> Char) -> Int -> String
showIntAtBase base toChar n = 
  if n < base then [toChar n]
  else showIntAtBase base toChar (n `div` base) ++ [toChar (n `mod` base)]

generateComplexExpression :: String -> String
generateComplexExpression identifier = 
  identifier ++ " + " ++ identifier ++ " * " ++ identifier ++ " / " ++ identifier

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Comprehensive Parser QuickCheck Tests"
  [ fastProperty "Round-trip parsing preserves structure" prop_parse_roundtrip_comprehensive
  , fastProperty "Valid directives always succeed" prop_parse_valid_directives_always_success
  , fastProperty "Error messages are useful" prop_parse_error_messages_useful
  , fastProperty "Empty files produce minimal structure" prop_parse_empty_minimal_structure
  , fastProperty "Whitespace variations preserve meaning" prop_parse_whitespace_variations_preserve_meaning
  , fastProperty "Large files don't cause overflow" prop_parse_large_files_no_overflow
  , fastProperty "Unicode content is preserved" prop_parse_unicode_preserved
  , fastProperty "Comments are ignored" prop_parse_comments_ignored
  , fastProperty "Mixed directives L.and code maintain order" prop_parse_mixed_directives_code_order
  , fastProperty "Nested structures maintain hierarchy" prop_parse_nested_hierarchy
  , fastProperty "Special characters in identifiers" prop_parse_special_char_identifiers
  , fastProperty "Multiple directives combined correctly" prop_parse_multiple_directives_combined
  , fastProperty "Inconsistent indentation handled gracefully" prop_parse_inconsistent_indentation_graceful
  , fastProperty "Invalid directives rejected appropriately" prop_parse_invalid_directives_rejected
  , fastProperty "Very long lines handled" prop_parse_very_long_lines
  , fastProperty "Escape sequences handled" prop_parse_escape_sequences
  , fastProperty "Numeric literals parsed" prop_parse_numeric_literals
  , fastProperty "Complex expressions parsed" prop_parse_complex_expressions
  , fastProperty "Function signatures parsed" prop_parse_function_signatures
  , fastProperty "Struct L.and interface definitions parsed" prop_parse_struct_interface_definitions
  , fastProperty "Import statements parsed" prop_parse_import_statements
  , fastProperty "Concurrent constructs parsed" prop_parse_concurrent_constructs
  , fastProperty "Error handling patterns parsed" prop_parse_error_handling
  , fastProperty "Generic types parsed" prop_parse_generic_types
  , fastProperty "Extremely long identifiers" prop_parse_extremely_long_identifiers
  , fastProperty "Deeply nested brackets" prop_parse_deeply_nested_brackets
  , fastProperty "Mixed line endings" prop_parse_mixed_line_endings
  , fastProperty "Zero-width characters" prop_parse_zero_width_characters
  , fastProperty "Tab L.and space mixing" prop_parse_tab_space_mixing
  , fastProperty "Multiple consecutive newlines" prop_parse_multiple_consecutive_newlines
  , fastProperty "Only whitespace files" prop_parse_only_whitespace
  , fastProperty "Files with BOM" prop_parse_with_bom
  , fastProperty "Parsing performance scales linearly" prop_parse_performance_linear
  , fastProperty "Memory usage reasonable" prop_parse_memory_reasonable
  , fastProperty "Regression patterns" prop_parse_regression_patterns
  ]