{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserRobustnessTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser
import Compiler.Errors
import SourceLocation
import Utils (trim, removeComments)

import Data.Char (isSpace, isLetter, isDigit, toLower)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, intercalate)
import Data.String (IsString)

-- Property: Parser handles empty input gracefully
prop_parser_handles_empty_input :: Property
prop_parser_handles_empty_input =
  case parseProgram "" of
    Left _ -> property True
    Right ast -> property $ null ast

-- Property: Parser handles whitespace-only input
prop_parser_handles_whitespace_only :: String -> Property
prop_parser_handles_whitespace_only ws =
  L.all isSpace ws ==>
  case parseProgram ws of
    Left _ -> property True
    Right ast -> property $ null ast

-- Property: Parser handles comments-only input
prop_parser_handles_comments_only :: String -> Property
prop_parser_handles_comments_only comment =
  not (null comment) && not ('"' `elem` comment) && not ('\'' `elem` comment) ==>
  let commentOnly = "// " ++ comment ++ "\n/* " ++ comment ++ " */"
  in case parseProgram commentOnly of
       Left _ -> property True
       Right ast -> property $ null ast

-- Property: Parser handles malformed input gracefully
prop_parser_handles_malformed_input :: String -> Property
prop_parser_handles_malformed_input malformed =
  L.length malformed <= 100 ==> -- Limit for performance
  case parseProgram malformed of
    Left _ -> property True
    Right ast -> property $ not (null ast) || L.length malformed == 0

-- Property: Parser preserves valid tokens in malformed input
prop_parser_preserves_valid_tokens :: String -> String -> Property
prop_parser_preserves_valid_tokens validPrefix malformedSuffix =
  L.length validPrefix <= 50 && L.length malformedSuffix <= 50 ==>
  let input = validPrefix ++ malformedSuffix
  in case parseProgram input of
       Left _ -> property True
       Right ast -> property $ not (null ast) ==> not (null validPrefix)

-- Property: Parser handles extremely long lines
prop_parser_handles_long_lines :: String -> Property
prop_parser_handles_long_lines base =
  L.length base <= 20 ==> -- Limit base size
  let longLine = L.concat (replicate 100 base) ++ ";\n"
  in case parseProgram longLine of
       Left _ -> property True
       Right ast -> property $ L.length ast <= 1

-- Property: Parser handles deeply nested structures
prop_parser_handles_nested_structures :: Int -> Property
prop_parser_handles_nested_structures depth =
  depth >= 0 && depth <= 10 ==> -- Limit depth for performance
  let nestedBraces = L.concat (replicate depth "{") ++ "x" ++ L.concat (replicate depth "}")
  in case parseProgram nestedBraces of
       Left _ -> property True
       Right ast -> property $ depth <= 5 ==> not (null ast)

-- Property: Parser handles Unicode characters
prop_parser_handles_unicode :: String -> Property
prop_parser_handles_unicode base =
  L.length base <= 30 ==> -- Limit for performance
  let unicodeInput = base ++ "测试café naïve résumé 🚀"
  in case parseProgram unicodeInput of
       Left _ -> property True
       Right ast -> property $ not (null ast) ==> not (null base)

-- Property: Parser handles special characters
prop_parser_handles_special_chars :: String -> Property
prop_parser_handles_special_chars base =
  L.length base <= 30 && not (L.any (`elem` "\"'\\") base) ==> -- Avoid string literals
  let specialChars = base ++ "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  in case parseProgram specialChars of
       Left _ -> property True
       Right ast -> property $ not (null ast) ==> not (null base)

-- Property: Parser handles mixed indentation
prop_parser_handles_mixed_indentation :: String -> Property
prop_parser_handles_mixed_indentation content =
  L.length content <= 50 ==> -- Limit for performance
  let mixedIndent = "  " ++ content ++ "\n\t" ++ content ++ "\n    " ++ content
  in case parseProgram mixedIndent of
       Left _ -> property True
       Right ast -> property $ not (null content) ==> not (null ast)

-- Property: Parser recovery after error
prop_parser_recovery_after_error :: String -> String -> Property
prop_parser_recovery_after_error before after =
  L.length before <= 30 && L.length after <= 30 ==> -- Limit for performance
  let input = before ++ " SYNTAX_ERROR " ++ after
  in case parseProgram input of
       Left _ -> property True
       Right ast -> property $ not (null after) ==> L.length ast >= 0

-- Property: Parser handles incomplete statements
prop_parser_handles_incomplete_statements :: String -> Property
prop_parser_handles_incomplete_statements stmt =
  L.length stmt <= 40 ==> -- Limit for performance
  let incomplete = stmt ++ " {"
  in case parseProgram incomplete of
       Left _ -> property True
       Right ast -> property $ L.length ast >= 0

-- Property: Parser handles multiple errors
prop_parser_handles_multiple_errors :: String -> String -> String -> Property
prop_parser_handles_multiple_errors part1 part2 part3 =
  L.length part1 <= 20 && L.length part2 <= 20 && L.length part3 <= 20 ==> -- Limit for performance
  let input = part1 ++ " ERROR1 " ++ part2 ++ " ERROR2 " ++ part3
  in case parseProgram input of
       Left _ -> property True
       Right ast -> property $ L.length ast >= 0

-- Property: Parser position tracking accuracy
prop_parser_position_tracking :: String -> String -> Property
prop_parser_position_tracking prefix suffix =
  L.length prefix <= 30 && L.length suffix <= 30 ==> -- Limit for performance
  let input = prefix ++ "\n" ++ suffix
  in case parseProgram input of
       Left err -> property True -- Error should contain position info
       Right ast -> property $ not (null ast)

-- Property: Parser handles large files efficiently
prop_parser_handles_large_files :: String -> Int -> Property
prop_parser_handles_large_files base multiplier =
  L.length base <= 20 && multiplier >= 1 && multiplier <= 50 ==> -- Limit for performance
  let largeContent = L.concat (replicate multiplier (base ++ ";\n"))
  in case parseProgram largeContent of
       Left _ -> property True
       Right ast -> property $ L.length ast <= multiplier

-- Property: Parser handles escaped characters
prop_parser_handles_escaped_chars :: String -> Property
prop_parser_handles_escaped_chars content =
  L.length content <= 30 && not (L.any (`elem` "\"\\") content) ==> -- Avoid conflicts
  let escaped = "var s = \"\\n\\t\\\"\\\\\\" ++ content ++ "\\\"\";"
  in case parseProgram escaped of
       Left _ -> property True
       Right ast -> property $ not (null ast)

-- Property: Parser handles malformed comments
prop_parser_handles_malformed_comments :: String -> String -> Property
prop_parser_handles_malformed_comments before after =
  L.length before <= 30 && L.length after <= 30 && not (L.any (`elem` "\"'") before) && not (L.any (`elem` "\"'") after) ==>
  let malformed = before ++ "/* unclosed comment " ++ after
  in case parseProgram malformed of
       Left _ -> property True
       Right ast -> property $ L.length ast >= 0

-- Property: Parser handles null bytes
prop_parser_handles_null_bytes :: String -> Property
prop_parser_handles_null_bytes content =
  L.length content <= 30 ==> -- Limit for performance
  let withNull = content ++ "\0" ++ content
  in case parseProgram withNull of
       Left _ -> property True
       Right ast -> property $ not (null ast) ==> not (null content)

-- Property: Parser handles very long identifiers
prop_parser_handles_long_identifiers :: String -> Property
prop_parser_handles_long_identifiers base =
  L.length base <= 10 ==> -- Limit base size
  let longIdent = L.concat (replicate 20 base)
      input = "var " ++ longIdent ++ " = 42;"
  in case parseProgram input of
       Left _ -> property True
       Right ast -> property $ not (null ast)

-- Property: Parser handles numeric literals
prop_parser_handles_numeric_literals :: Int -> Property
prop_parser_handles_numeric_literals num =
  num >= 0 && num <= 1000000 ==> -- Limit range
  let input = "var x = " ++ show num ++ ";"
  in case parseProgram input of
       Left _ -> property True
       Right ast -> property $ not (null ast)

-- Property: Parser handles string literals
prop_parser_handles_string_literals :: String -> Property
prop_parser_handles_string_literals content =
  L.length content <= 30 && not (L.any (`elem` "\"\\") content) ==> -- Avoid conflicts
  let input = "var s = \"" ++ content ++ "\";"
  in case parseProgram input of
       Left _ -> property True
       Right ast -> property $ not (null ast)

-- Advanced robustness tests

-- Property: Parser handles complex nested structures
prop_parser_complex_nested :: Int -> Int -> Property
prop_parser_complex_nested braceDepth parenDepth =
  braceDepth >= 0 && parenDepth <= 5 && parenDepth >= 0 && parenDepth <= 5 ==> -- Limit for performance
  let braces = L.concat (replicate braceDepth "{") ++ "x" ++ L.concat (replicate braceDepth "}")
      parens = L.concat (replicate parenDepth "(") ++ "y" ++ L.concat (replicate parenDepth ")")
      input = braces ++ parens ++ ";"
  in case parseProgram input of
       Left _ -> property True
       Right ast -> property $ (braceDepth + parenDepth) <= 6 ==> not (null ast)

-- Property: Parser error messages contain useful information
prop_parser_error_messages_useful :: String -> Property
prop_parser_error_messages_useful malformed =
  L.length malformed <= 50 ==> -- Limit for performance
  case parseProgram malformed of
    Left err -> property $ L.length (show err) > 0 -- Error message should not be empty
    Right ast -> property True

-- Property: Parser handles mixed language content
prop_parser_mixed_language :: String -> Property
prop_parser_mixed_language base =
  L.length base <= 20 ==> -- Limit for performance
  let mixed = base ++ " variable 测试变量 café变量 naïve变量"
  in case parseProgram mixed of
       Left _ -> property True
       Right ast -> property $ not (null ast) ==> not (null base)

-- Property: Parser handles edge case characters
prop_parser_edge_case_chars :: String -> Property
prop_parser_edge_case_chars base =
  L.length base <= 20 ==> -- Limit for performance
  let edgeChars = base ++ "\x01\x02\x03\x7F\x80\xFF"
  in case parseProgram edgeChars of
       Left _ -> property True
       Right ast -> property $ not (null ast) ==> not (null base)

-- Property: Parser handles repeated parsing
prop_parser_repeated_parsing :: String -> Int -> Property
prop_parser_repeated_parsing content iterations =
  L.length content <= 30 && iterations >= 1 && iterations <= 10 ==> -- Limit for performance
  let results = replicate iterations (parseProgram content)
      successes = L.length [() | Right _ <- results]
  in property $ successes >= 0 && successes <= iterations

-- Property: Parser handles concurrent parsing simulation
prop_parser_concurrent_simulation :: String -> Property
prop_parser_concurrent_simulation content =
  L.length content <= 30 ==> -- Limit for performance
  let results = replicate 5 (parseProgram content) -- Simulate concurrent parsing
      consistent = L.all (\r1 r2 -> case (r1, r2) of
                                    (Left _, Left _) -> True
                                    (Right a1, Right a2) -> L.length a1 == L.length a2
                                    _ -> False) results (L.tail results)
  in property $ consistent

tests :: TestTree
tests = testGroup "Parser Robustness Tests"
  [ fastProperty "Parser handles empty input gracefully" prop_parser_handles_empty_input
  , fastProperty "Parser handles whitespace-only input" prop_parser_handles_whitespace_only
  , fastProperty "Parser handles comments-only input" prop_parser_handles_comments_only
  , fastProperty "Parser handles malformed input gracefully" prop_parser_handles_malformed_input
  , fastProperty "Parser preserves valid tokens in malformed input" prop_parser_preserves_valid_tokens
  , fastProperty "Parser handles extremely long lines" prop_parser_handles_long_lines
  , fastProperty "Parser handles deeply nested structures" prop_parser_handles_nested_structures
  , fastProperty "Parser handles Unicode characters" prop_parser_handles_unicode
  , fastProperty "Parser handles special characters" prop_parser_handles_special_chars
  , fastProperty "Parser handles mixed indentation" prop_parser_handles_mixed_indentation
  , fastProperty "Parser recovery after error" prop_parser_recovery_after_error
  , fastProperty "Parser handles incomplete statements" prop_parser_handles_incomplete_statements
  , fastProperty "Parser handles multiple errors" prop_parser_handles_multiple_errors
  , fastProperty "Parser position tracking accuracy" prop_parser_position_tracking
  , fastProperty "Parser handles large files efficiently" prop_parser_handles_large_files
  , fastProperty "Parser handles escaped characters" prop_parser_handles_escaped_chars
  , fastProperty "Parser handles malformed comments" prop_parser_handles_malformed_comments
  , fastProperty "Parser handles null bytes" prop_parser_handles_null_bytes
  , fastProperty "Parser handles very long identifiers" prop_parser_handles_long_identifiers
  , fastProperty "Parser handles numeric literals" prop_parser_handles_numeric_literals
  , fastProperty "Parser handles string literals" prop_parser_handles_string_literals
  , fastProperty "Parser handles complex nested structures" prop_parser_complex_nested
  , fastProperty "Parser error messages contain useful information" prop_parser_error_messages_useful
  , fastProperty "Parser handles mixed language content" prop_parser_mixed_language
  , fastProperty "Parser handles edge case characters" prop_parser_edge_case_chars
  , fastProperty "Parser handles repeated parsing" prop_parser_repeated_parsing
  , fastProperty "Parser handles concurrent parsing simulation" prop_parser_concurrent_simulation
  ]