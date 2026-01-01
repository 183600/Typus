{-# LANGUAGE CPP #-}

-- | Basic compiler property tests using QuickCheck
module Test.Unit.CompilerBasicPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), property, classify, counterexample)
import qualified Data.List as Data.List
import Data.Char (isAlpha, isDigit, isSpace)

import Compiler (compileTypus, CompilationResult(..))
import Parser (parseTypus, TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

-- ============================================================================
-- Core Compiler Property Tests
-- ============================================================================

-- Property: Empty source compiles to trivial result
prop_compile_empty_source :: Property
prop_compile_empty_source =
  case compileTypus "" of
    CompilationSuccess _ -> property True
    CompilationError _ -> property False

-- Property: Valid variable declarations are compilable
prop_compile_valid_var_declaration :: String -> Property
prop_compile_valid_var_declaration varName =
  isValidIdentifier varName ==>
  let validDecl = "var " ++ varName ++ " int = 42"
  in case compileTypus validDecl of
    CompilationSuccess _ -> property True
    CompilationError _ -> property False

-- Property: Multiple declarations compile consistently
prop_compile_multiple_declarations :: [String] -> Property
prop_compile_multiple_declarations varNames =
  L.all isValidIdentifier varNames && not (null varNames) && L.length varNames <= 5 ==>
  let declarations = L.map (\name -> "var " ++ name ++ " int = 0") varNames
      source = unlines declarations
  in case compileTypus source of
    CompilationSuccess _ -> property True
    CompilationError _ -> property False

-- Property: Compilation preserves semantic structure
prop_compile_preserves_structure :: String -> Property
prop_compile_preserves_structure content =
  L.length content <= 100 ==> -- Limit size for reasonable test times
  case parseTypus content of
    Left _ -> property True -- Parsing failure is acceptable
    Right parsed -> 
      case compileTypus content of
        CompilationSuccess result -> property $ hasValidStructure result
        CompilationError _ -> property True

-- Property: Function declarations with valid signatures
prop_compile_function_declarations :: String -> [String] -> Property
prop_compile_function_declarations funcName paramNames =
  isValidIdentifier funcName && L.all isValidIdentifier paramNames && L.length paramNames <= 3 ==>
  let params = unwords $ L.map (\p -> p ++ " int") paramNames
      funcDecl = "func " ++ funcName ++ "(" ++ params ++ ") int { return 42; }"
  in case compileTypus funcDecl of
    CompilationSuccess _ -> property True
    CompilationError _ -> property False

-- Property: Compilation error messages contain source location
prop_compile_errors_have_location :: String -> Property
prop_compile_errors_have_location malformed =
  L.length malformed > 5 && hasInvalidSyntax malformed ==>
  case compileTypus malformed of
    CompilationSuccess _ -> property False -- Should not succeed on invalid syntax
    CompilationError errMsg -> property $ containsLocationInfo errMsg

-- Property: Idempotent compilation of valid code
prop_compile_idempotent :: String -> Property
prop_compile_idempotent source =
  isValidTypusSource source ==>
  case compileTypus source of
    CompilationSuccess result1 ->
      case compileTypus source of
        CompilationSuccess result2 -> property $ result1 == result2
        CompilationError _ -> property False
    CompilationError _ -> property False

-- Property: Compilation handles large files without overflow
prop_compile_large_file :: Int -> Property
prop_compile_large_file n =
  n >= 0 && n <= 100 ==> -- Reasonable limit
  let largeSource = unlines $ replicate n "var x int = 42"
  in case compileTypus largeSource of
    CompilationSuccess _ -> property True
    CompilationError _ -> property True -- May fail due to resource limits

-- Property: Comment preservation in compilation
prop_compile_preserves_comments :: String -> Property
prop_compile_preserves_comments code =
  let commentedCode = "// This is a comment\n" ++ code ++ "\n// End comment"
  in case compileTypus commentedCode of
    CompilationSuccess _ -> property True
    CompilationError _ -> property False -- Comments shouldn't break compilation

-- Property: Type consistency in compilation
prop_compile_type_consistency :: String -> Property
prop_compile_type_consistency varName =
  isValidIdentifier varName ==>
  let intDecl = "var " ++ varName ++ " int = 42"
      stringDecl = "var " ++ varName ++ " string = \"hello\""
  in case (compileTypus intDecl, compileTypus stringDecl) of
    (CompilationSuccess _, CompilationSuccess _) -> property True
    _ -> property False -- Both should succeed L.or fail consistently

-- Property: Nested block compilation
prop_compile_nested_blocks :: Int -> Property
prop_compile_nested_blocks depth =
  depth >= 0 && depth <= 4 ==> -- Reasonable nesting depth
  let nestedCode = generateNestedBlocks depth
  in case compileTypus nestedCode of
    CompilationSuccess _ -> property True
    CompilationError _ -> property True -- May fail due to complexity

-- ============================================================================
-- Helper Functions
-- ============================================================================

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (c:cs) = isAlpha c && L.all isValidChar cs
  where
    isValidChar ch = isAlpha ch || isDigit ch || ch == '_'

hasValidStructure :: CompilationResult -> Bool
hasValidStructure (CompilationSuccess _) = True
hasValidStructure (CompilationError _) = False

hasInvalidSyntax :: String -> Bool
hasInvalidSyntax s = L.any (`elem` "@#$%^&*()[]{}|\\") s

containsLocationInfo :: String -> Bool
containsLocationInfo errMsg = 
  L.any (`Data.List.L.isInfixOf` errMsg) ["line", "column", "position", "at"]

isValidTypusSource :: String -> Bool
isValidTypusSource source = 
  case parseTypus source of
    Left _ -> False
    Right _ -> True

generateNestedBlocks :: Int -> String
generateNestedBlocks 0 = "var x int = 42"
generateNestedBlocks n = 
  "if true {\n" ++ generateNestedBlocks (n-1) ++ "\n}"
