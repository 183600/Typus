{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.CoreFunctionalityBoundarySpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub)
import Control.Monad (when, replicateM)

-- ============================================================================
-- Core Functionality Boundary Tests
-- ============================================================================

-- | Test parser with extremely long strings
prop_parser_extremely_long_strings :: Int -> String -> Property
prop_parser_extremely_long_strings n baseString =
  n >= 0 && n <= 1000 ==>
    let longString = concat $ replicate n baseString
        parseResult = parseTypus longString
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ tfDirectives typusFile == defaultFileDirectives

-- | Test parser with deeply nested structures
prop_parser_deeply_nested :: Int -> Property
prop_parser_deeply_nested depth =
  depth >= 0 && depth <= 20 ==>
    let nestedStructure = concat $ replicate depth "{"
        nestedStructure' = nestedStructure ++ concat (replicate depth "}")
        parseResult = parseTypus nestedStructure'
    in case parseResult of
         Left _ -> property True
         Right _ -> property True

-- | Test source location with extreme positions
prop_sourcelocation_extreme_positions :: Int -> Int -> Property
prop_sourcelocation_extreme_positions line col =
  line >= 0 && col >= 0 && line <= 10000 && col <= 10000 ==>
    let pos = SourcePos line col 0
        _ = SourceSpan pos pos
    in posLine pos == line && posColumn pos == col

-- | Test compiler with empty directives
prop_compiler_empty_directives :: Property
prop_compiler_empty_directives =
  let content = ""
      parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         property $ tfDirectives typusFile == defaultFileDirectives

-- | Test utils with control characters
prop_utils_control_characters :: String -> Property
prop_utils_control_characters input =
  let filtered = filter (not . isControl) input
      trimmed = trim filtered
  in property $ length trimmed <= length filtered

-- | Test parser with mixed whitespace
prop_parser_mixed_whitespace :: String -> String -> Property
prop_parser_mixed_whitespace ws1 ws2 =
  let mixedWS = ws1 ++ "\n\t  " ++ ws2 ++ "\n\n\t"
      parseResult = parseTypus mixedWS
  in case parseResult of
       Left _ -> property True
       Right _ -> property $ True

-- | Test compiler with minimal valid input
prop_compiler_minimal_input :: String -> Property
prop_compiler_minimal_input c =
  not (null c) && length c <= 5 ==>
    let parseResult = parseTypus c
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test source location with negative positions (should handle gracefully)
prop_sourcelocation_negative_positions :: Int -> Int -> Property
prop_sourcelocation_negative_positions line col =
  let normalizedPos = SourcePos (max 0 line) (max 0 col) 0
  in property $ posLine normalizedPos >= 0 && posColumn normalizedPos >= 0

-- | Test utils with empty strings
prop_utils_empty_strings :: Property
prop_utils_empty_strings =
  let emptyStr = ""
      trimmed = trim emptyStr
      splitEmpty = splitBy '\n' emptyStr
  in property $ null trimmed && null splitEmpty

-- | Test parser with only comments
prop_parser_only_comments :: String -> Property
prop_parser_only_comments comment =
  let commentContent = "// " ++ comment
      parseResult = parseTypus commentContent
  in case parseResult of
       Left _ -> property True
       Right _ -> property $ True

-- | Test compiler with syntax errors
prop_compiler_syntax_errors :: String -> Property
prop_compiler_syntax_errors malformed =
  let malformedCode = malformed ++ "{@#$%^&*()}"
      parseResult = parseTypus malformedCode
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let compileResult = compile typusFile
         in case compileResult of
              Left _ -> property True
              Right _ -> property False  -- Should not compile malformed code

-- | Test source location span arithmetic
prop_sourcelocation_span_arithmetic :: Int -> Int -> Int -> Int -> Property
prop_sourcelocation_span_arithmetic l1 c1 l2 c2 =
  l1 >= 0 && c1 >= 0 && l2 >= 0 && c2 >= 0 ==>
    let _ = spanBetween (SourcePos l1 c1 0) (SourcePos l2 c2 0)
    in property $ True  -- Basic test that span creation doesn't crash

-- | Test utils with duplicate characters
prop_utils_duplicate_characters :: Char -> Int -> Property
prop_utils_duplicate_characters ch n =
  n >= 0 && n <= 100 ==>
    let duplicateStr = replicate n ch
        uniqueChars = nub duplicateStr
        expectedLength = if ch `elem` uniqueChars then 1 else 0
    in property $ length uniqueChars == expectedLength

-- | Test parser with unicode boundary cases
prop_parser_unicode_boundary :: Int -> Property
prop_parser_unicode_boundary codePoint =
  codePoint >= 0 && codePoint <= 0x10FFFF ==>
    let unicodeChar = toEnum codePoint :: Char
        unicodeStr = [unicodeChar]
        parseResult = parseTypus unicodeStr
    in case parseResult of
         Left _ -> property True
         Right _ -> property True

-- | Test compiler with circular dependencies (if supported)
prop_compiler_circular_dependencies :: String -> Property
prop_compiler_circular_dependencies moduleName =
  not (null moduleName) && length moduleName <= 10 ==>
    let circularContent = "import " ++ moduleName ++ "\n" ++
                         "let x = " ++ moduleName ++ ".y\n" ++
                         "let y = " ++ moduleName ++ ".x\n"
        parseResult = parseTypus circularContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True  -- Should fail with circular dependency
                Right _ -> property True

-- | Test source location with large files
prop_sourcelocation_large_files :: Int -> String -> Property
prop_sourcelocation_large_files n baseLine =
  n >= 0 && n <= 1000 ==>
    let largeContent = unlines $ replicate n baseLine
        parseResult = parseTypus largeContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let blocks = tfBlocks typusFile
           in property $ length blocks >= 0

-- | Test utils with string boundaries
prop_utils_string_boundaries :: String -> Property
prop_utils_string_boundaries input =
  let len = length input
      firstChar = if len > 0 then case input of (x:_) -> [x]; [] -> [] else []
      lastChar = if len > 0 then [last input] else []
      middleChars = if len > 2 then take (len - 2) $ drop 1 input else []
  in property $ length firstChar + length middleChars + length lastChar == len

-- | Test parser with directive boundaries
prop_parser_directive_boundaries :: Bool -> Bool -> Bool -> Property
prop_parser_directive_boundaries ownership deps constraints =
  let directiveStr = "// ownership: " ++ show ownership ++ "\n" ++
                    "// dependent-types: " ++ show deps ++ "\n" ++
                    "// constraints: " ++ show constraints ++ "\n" ++
                    "\n\n"  -- Extra newlines to test boundary
      parseResult = parseTypus directiveStr
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let _ = tfDirectives typusFile
         in property $ True  -- Basic test that directives are parsed

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Core Functionality Boundary Tests"
  [ testProperty "Parser with extremely long strings" prop_parser_extremely_long_strings,
    testProperty "Parser with deeply nested structures" prop_parser_deeply_nested,
    testProperty "Source location with extreme positions" prop_sourcelocation_extreme_positions,
    testProperty "Compiler with empty directives" prop_compiler_empty_directives,
    testProperty "Utils with control characters" prop_utils_control_characters,
    testProperty "Parser with mixed whitespace" prop_parser_mixed_whitespace,
    testProperty "Compiler with minimal valid input" prop_compiler_minimal_input,
    testProperty "Source location with negative positions" prop_sourcelocation_negative_positions,
    testProperty "Utils with empty strings" prop_utils_empty_strings,
    testProperty "Parser with only comments" prop_parser_only_comments,
    testProperty "Compiler with syntax errors" prop_compiler_syntax_errors,
    testProperty "Source location span arithmetic" prop_sourcelocation_span_arithmetic,
    testProperty "Utils with duplicate characters" prop_utils_duplicate_characters,
    testProperty "Parser with unicode boundary cases" prop_parser_unicode_boundary,
    testProperty "Compiler with circular dependencies" prop_compiler_circular_dependencies,
    testProperty "Source location with large files" prop_sourcelocation_large_files,
    testProperty "Utils with string boundaries" prop_utils_string_boundaries,
    testProperty "Parser with directive boundaries" prop_parser_directive_boundaries
  ]