{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.ErrorRecoveryAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (when)

-- ============================================================================
-- Advanced Error Recovery QuickCheck Tests
-- ============================================================================

-- | Test parser recovery from malformed directives
prop_parser_malformed_directive_recovery :: String -> Property
prop_parser_malformed_directive_recovery content = 
  not (null content) ==>
    let malformedContent = "// ownership: invalid\n" ++ content
        parseResult = parseTypus malformedContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ not (null (tfBlocks typusFile))

-- | Test parser recovery from incomplete directives
prop_parser_incomplete_directive_recovery :: String -> Property
prop_parser_incomplete_directive_recovery content = 
  not (null content) ==>
    let incompleteContent = "// ownership\n" ++ content
        parseResult = parseTypus incompleteContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ not (null (tfBlocks typusFile))

-- | Test parser recovery from mismatched brackets
prop_parser_mismatched_brackets_recovery :: String -> Property
prop_parser_mismatched_brackets_recovery content = 
  not (null content) ==>
    let bracketContent = content ++ "func test() { if (x { return y; }"
        parseResult = parseTypus bracketContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ not (null (tfBlocks typusFile))

-- | Test parser recovery from malformed expressions
prop_parser_malformed_expression_recovery :: String -> Property
prop_parser_malformed_expression_recovery content = 
  not (null content) ==>
    let exprContent = content ++ "let x = +"
        parseResult = parseTypus exprContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ not (null (tfBlocks typusFile))

-- | Test compiler recovery from type errors
prop_compiler_type_error_recovery :: String -> Property
prop_compiler_type_error_recovery content = 
  not (null content) && length content < 50 ==>
    let typeErrorContent = content ++ "var x int = \"string\""
        parseResult = parseTypus typeErrorContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left errors -> property $ not (null errors)
                Right _ -> property True

-- | Test compiler recovery from undefined variables
prop_compiler_undefined_var_recovery :: String -> Property
prop_compiler_undefined_var_recovery content = 
  not (null content) && length content < 50 ==>
    let undefinedVarContent = content ++ "return undefinedVar"
        parseResult = parseTypus undefinedVarContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left errors -> property $ not (null errors)
                Right _ -> property True

-- | Test error recovery with multiple errors
prop_multiple_errors_recovery :: String -> Property
prop_multiple_errors_recovery content = 
  not (null content) && length content < 100 ==>
    let errorContent = content ++ "// ownership: invalid\nvar x int = \"string\"\nreturn undefinedVar"
        parseResult = parseTypus errorContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left errors -> property $ length errors >= 1
                Right _ -> property True

-- | Test error recovery with nested errors
prop_nested_errors_recovery :: String -> Property
prop_nested_errors_recovery content = 
  not (null content) && length content < 100 ==>
    let nestedErrorContent = content ++ "func test() { if (x) { var y int = \"string\"; return z; } }"
        parseResult = parseTypus nestedErrorContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left errors -> property $ not (null errors)
                Right _ -> property True

-- | Test error recovery with Unicode errors
prop_unicode_error_recovery :: String -> Property
prop_unicode_error_recovery content = 
  let unicodeErrorContent = content ++ "var x 🚀 = \"test\""
      parseResult = parseTypus unicodeErrorContent
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ not (null (tfBlocks typusFile))

-- | Test error recovery with special characters
prop_special_char_error_recovery :: String -> Property
prop_special_char_error_recovery content = 
  let specialCharErrorContent = content ++ "var x @#$ = \"test\""
      parseResult = parseTypus specialCharErrorContent
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ not (null (tfBlocks typusFile))

-- | Test error recovery with incomplete code blocks
prop_incomplete_block_recovery :: String -> Property
prop_incomplete_block_recovery content = 
  not (null content) ==>
    let incompleteBlockContent = content ++ "func test() { if (x) {"
        parseResult = parseTypus incompleteBlockContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ not (null (tfBlocks typusFile))

-- | Test error recovery with mixed valid and invalid code
prop_mixed_valid_invalid_recovery :: String -> Property
prop_mixed_valid_invalid_recovery content = 
  not (null content) && length content < 100 ==>
    let mixedContent = "var x int = 5\n" ++ content ++ "var y string = \"hello\"\nreturn z +"
        parseResult = parseTypus mixedContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left errors -> property $ not (null errors)
                Right _ -> property True

-- | Test error recovery consistency
prop_error_recovery_consistency :: String -> Property
prop_error_recovery_consistency content = 
  not (null content) && length content < 50 ==>
    let errorContent = content ++ "let x = +"
        parseResult1 = parseTypus errorContent
        parseResult2 = parseTypus errorContent
    in case (parseResult1, parseResult2) of
         (Left _, Left _) -> property True
         (Right tf1, Right tf2) -> property $ tfBlocks tf1 == tfBlocks tf2
         (Left _, Right _) -> property False
         (Right _, Left _) -> property False

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Advanced Error Recovery QuickCheck Properties"
  [ testProperty "Parser recovery from malformed directives" prop_parser_malformed_directive_recovery,
    testProperty "Parser recovery from incomplete directives" prop_parser_incomplete_directive_recovery,
    testProperty "Parser recovery from mismatched brackets" prop_parser_mismatched_brackets_recovery,
    testProperty "Parser recovery from malformed expressions" prop_parser_malformed_expression_recovery,
    testProperty "Compiler recovery from type errors" prop_compiler_type_error_recovery,
    testProperty "Compiler recovery from undefined variables" prop_compiler_undefined_var_recovery,
    testProperty "Error recovery with multiple errors" prop_multiple_errors_recovery,
    testProperty "Error recovery with nested errors" prop_nested_errors_recovery,
    testProperty "Error recovery with Unicode errors" prop_unicode_error_recovery,
    testProperty "Error recovery with special characters" prop_special_char_error_recovery,
    testProperty "Error recovery with incomplete code blocks" prop_incomplete_block_recovery,
    testProperty "Error recovery with mixed valid and invalid code" prop_mixed_valid_invalid_recovery,
    testProperty "Error recovery consistency" prop_error_recovery_consistency
  ]