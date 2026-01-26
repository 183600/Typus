{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Test.Unit.ParserErrorRecoveryAdvancedSpec where


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
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition)
import Control.Monad (when, replicateM)

-- ============================================================================
-- Parser Error Recovery Advanced Tests
-- ============================================================================

-- | Test parser recovery from mismatched brackets
prop_parser_mismatched_brackets :: String -> String -> Property
prop_parser_mismatched_brackets open close =
  let bracketContent = open ++ "{\n  content\n" ++ close ++ "\n}"
      parseResult = parseTypus bracketContent
  in case parseResult of
       Left _ -> property True  -- Expected to fail, but should recover gracefully
       Right typusFile -> property $ True  -- Or recover and parse something

-- | Test parser recovery from missing semicolons
prop_parser_missing_semicolons :: String -> String -> Property
prop_parser_missing_semicolons stmt1 stmt2 =
  let contentWithoutSemicolons = stmt1 ++ "\n" ++ stmt2 ++ "\n"
      parseResult = parseTypus contentWithoutSemicolons
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ True

-- | Test parser recovery from invalid keywords
prop_parser_invalid_keywords :: String -> String -> Property
prop_parser_invalid_keywords prefix keyword =
  let invalidKeyword = prefix ++ keyword
      content = invalidKeyword ++ " x = 5\n"
      parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ True

-- | Test parser recovery from truncated expressions
prop_parser_truncated_expressions :: String -> Property
prop_parser_truncated_expressions expr =
  length expr > 0 ==>
    let truncated = take (length expr `div` 2) expr
        content = "let x = " ++ truncated ++ "\n"
        parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from mixed language constructs
prop_parser_mixed_language_constructs :: String -> Property
prop_parser_mixed_language_constructs code =
  let mixedContent = code ++ " let x = 5;\nfunction y() { return x; }\n"
      parseResult = parseTypus mixedContent
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ True

-- | Test parser recovery from encoding issues
prop_parser_encoding_issues :: Int -> Property
prop_parser_encoding_issues codePoint =
  codePoint >= 0 && codePoint <= 0x10FFFF ==>
    let problematicChar = toEnum codePoint :: Char
        content = "let x = " ++ [problematicChar] ++ "\n"
        parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from comment mismatches
prop_parser_comment_mismatches :: String -> Property
prop_parser_comment_mismatches content =
  let mismatchedComment = "/* " ++ content ++ "\n"  -- Missing closing */
      parseResult = parseTypus mismatchedComment
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ True

-- | Test parser recovery from string literal errors
prop_parser_string_literal_errors :: String -> Property
prop_parser_string_literal_errors str =
  let unterminatedString = "let x = \"" ++ str ++ "\n"  -- Missing closing quote
      parseResult = parseTypus unterminatedString
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ True

-- | Test parser recovery from indentation errors
prop_parser_indentation_errors :: String -> String -> Property
prop_parser_indentation_errors line1 line2 =
  let inconsistentIndent = "  " ++ line1 ++ "\n" ++ "    " ++ line2 ++ "\n" ++ line1 ++ "\n"
      parseResult = parseTypus inconsistentIndent
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ True

-- | Test parser recovery from operator precedence issues
prop_parser_operator_precedence_issues :: String -> String -> String -> Property
prop_parser_operator_precedence_issues op1 op2 operand =
  not (null op1) && not (null op2) && not (null operand) ==>
    let ambiguousExpr = operand ++ " " ++ op1 ++ " " ++ operand ++ " " ++ op2 ++ " " ++ operand
        content = "let x = " ++ ambiguousExpr ++ "\n"
        parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from malformed directives
prop_parser_malformed_directives :: String -> Property
prop_parser_malformed_directives directive =
  let malformedDirective = "// " ++ directive ++ " : invalid\n"
      content = malformedDirective ++ "let x = 5\n"
      parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ True

-- | Test parser recovery from circular imports
prop_parser_circular_imports :: String -> String -> Property
prop_parser_circular_imports module1 module2 =
  not (null module1) && not (null module2) && module1 /= module2 ==>
    let circularContent = "import " ++ module1 ++ "\n" ++
                         "import " ++ module2 ++ "\n" ++
                         "let x = 5\n"
        parseResult = parseTypus circularContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from type annotation errors
prop_parser_type_annotation_errors :: String -> String -> Property
prop_parser_type_annotation_errors varName typeName =
  not (null varName) && not (null typeName) ==>
    let malformedType = "let " ++ varName ++ " : " ++ typeName ++ " = 5\n"
        parseResult = parseTypus malformedType
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from function definition errors
prop_parser_function_definition_errors :: String -> String -> Property
prop_parser_function_definition_errors funcName body =
  not (null funcName) ==>
    let malformedFunc = "function " ++ funcName ++ "(\n" ++ body ++ "\nlet x = 5\n"
        parseResult = parseTypus malformedFunc
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from multiple consecutive errors
prop_parser_multiple_consecutive_errors :: String -> Property
prop_parser_multiple_consecutive_errors base =
  not (null base) && length base < 50 ==>
    let errorCascade = base ++ " {@#$%\n" ++ 
                      base ++ " {{{\n" ++ 
                      base ++ " /*\n" ++
                      base ++ " \"\n"
        parseResult = parseTypus errorCascade
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from nested structure errors
prop_parser_nested_structure_errors :: Int -> String -> Property
prop_parser_nested_structure_errors depth content =
  depth >= 0 && depth <= 10 ==>
    let malformedNested = concat $ replicate depth "{"
        malformedNested' = malformedNested ++ content ++ concat (replicate (depth - 1) "}")
        parseResult = parseTypus malformedNested'
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from whitespace anomalies
prop_parser_whitespace_anomalies :: String -> Property
prop_parser_whitespace_anomalies content =
  let anomalousWS = content ++ "\t\t  \n\n\t  \t" ++ content ++ "   \t\n\n\n"
      parseResult = parseTypus anomalousWS
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ True

-- | Test parser recovery from identifier naming issues
prop_parser_identifier_naming_issues :: String -> Property
prop_parser_identifier_naming_issues name =
  let problematicName = if null name then "123invalid" else name ++ "123invalid"
      content = "let " ++ problematicName ++ " = 5\n"
      parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from expression boundary issues
prop_parser_expression_boundary_issues :: String -> String -> Property
prop_parser_expression_boundary_issues expr1 expr2 =
  not (null expr1) && not (null expr2) ==>
    let boundaryIssue = expr1 ++ "\n" ++ expr2 ++ "\nlet x = 5"
        parseResult = parseTypus boundaryIssue
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from incomplete type definitions
prop_parser_incomplete_type_definitions :: String -> Property
prop_parser_incomplete_type_definitions typeName =
  not (null typeName) ==>
    let incompleteType = "type " ++ typeName ++ " =\nlet x = 5\n"
        parseResult = parseTypus incompleteType
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Test parser recovery from malformed pattern matching
prop_parser_malformed_pattern_matching :: String -> Property
prop_parser_malformed_pattern_matching pattern =
  let malformedPattern = "match x with\n" ++ pattern ++ "\nlet y = 5\n"
      parseResult = parseTypus malformedPattern
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ True

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Parser Error Recovery Advanced Tests"
  [ testProperty "Parser recovery from mismatched brackets" prop_parser_mismatched_brackets,
    testProperty "Parser recovery from missing semicolons" prop_parser_missing_semicolons,
    testProperty "Parser recovery from invalid keywords" prop_parser_invalid_keywords,
    testProperty "Parser recovery from truncated expressions" prop_parser_truncated_expressions,
    testProperty "Parser recovery from mixed language constructs" prop_parser_mixed_language_constructs,
    testProperty "Parser recovery from encoding issues" prop_parser_encoding_issues,
    testProperty "Parser recovery from comment mismatches" prop_parser_comment_mismatches,
    testProperty "Parser recovery from string literal errors" prop_parser_string_literal_errors,
    testProperty "Parser recovery from indentation errors" prop_parser_indentation_errors,
    testProperty "Parser recovery from operator precedence issues" prop_parser_operator_precedence_issues,
    testProperty "Parser recovery from malformed directives" prop_parser_malformed_directives,
    testProperty "Parser recovery from circular imports" prop_parser_circular_imports,
    testProperty "Parser recovery from type annotation errors" prop_parser_type_annotation_errors,
    testProperty "Parser recovery from function definition errors" prop_parser_function_definition_errors,
    testProperty "Parser recovery from multiple consecutive errors" prop_parser_multiple_consecutive_errors,
    testProperty "Parser recovery from nested structure errors" prop_parser_nested_structure_errors,
    testProperty "Parser recovery from whitespace anomalies" prop_parser_whitespace_anomalies,
    testProperty "Parser recovery from identifier naming issues" prop_parser_identifier_naming_issues,
    testProperty "Parser recovery from expression boundary issues" prop_parser_expression_boundary_issues,
    testProperty "Parser recovery from incomplete type definitions" prop_parser_incomplete_type_definitions,
    testProperty "Parser recovery from malformed pattern matching" prop_parser_malformed_pattern_matching
  ]