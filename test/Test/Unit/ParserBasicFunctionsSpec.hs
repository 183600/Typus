module Test.Unit.ParserBasicFunctionsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Parser
import qualified SourceLocation
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing)

-- 测试基本解析函数的属性
prop_identifier_valid :: String -> Property
prop_identifier_valid input = 
  let isValid = all (\c -> isAlpha c || isDigit c || c == '_') input
      isNonEmpty = not (null input)
      startsWithAlpha = not (null input) && isAlpha (head input)
  in property $ (isValid && isNonEmpty && startsWithAlpha) ==> 
    case Parser.parseIdentifier input of
      Right result -> property $ result === input
      Left _ -> property False

prop_identifier_invalid :: String -> Property
prop_identifier_invalid input = 
  let hasInvalidChars = any (\c -> not (isAlpha c) && not (isDigit c) && c /= '_') input
      isEmpty = null input
      startsWithDigit = not (null input) && isDigit (head input)
  in property $ (hasInvalidChars || isEmpty || startsWithDigit) ==> 
    case Parser.parseIdentifier input of
      Right _ -> property False
      Left _ -> property True

prop_number_parsing :: Int -> Property
prop_number_parsing n = 
  let numStr = show n
  in case Parser.parseNumber numStr of
    Right result -> property $ result === n
    Left _ -> property False

prop_string_parsing :: String -> Property
prop_string_parsing s = 
  let quotedStr = "\"" ++ s ++ "\""
  in case Parser.parseString quotedStr of
    Right result -> property $ result === s
    Left _ -> property False

prop_whitespace_handling :: String -> String -> Property
prop_whitespace_handling input1 input2 = 
  let combined = input1 ++ "   \t\n  " ++ input2
  in case (Parser.parseToken input1, Parser.parseToken input2, Parser.parseToken combined) of
    (Right t1, Right t2, Right tCombined) -> property $ tCombined === t1
    _ -> property True

prop_comment_stripping :: String -> String -> Property
prop_comment_stripping code comment = 
  let withComment = code ++ " -- " ++ comment
  in case (Parser.parseCode code, Parser.parseCode withComment) of
    (Right ast1, Right ast2) -> property $ ast1 === ast2
    _ -> property True

prop_multiline_comment_stripping :: String -> String -> String -> Property
prop_multiline_comment_stripping before comment after = 
  let withComment = before ++ " {- " ++ comment ++ " -} " ++ after
  in case (Parser.parseCode (before ++ " " ++ after), Parser.parseCode withComment) of
    (Right ast1, Right ast2) -> property $ ast1 === ast2
    _ -> property True

prop_operator_parsing :: String -> Property
prop_operator_parsing op = 
  let validOperators = ["+", "-", "*", "/", "==", "/=", "<", "<=", ">", ">=", "&&", "||", "++"]
      isValidOp = op `elem` validOperators
  in property $ isValidOp ==> 
    case Parser.parseOperator op of
      Right result -> property $ result === op
      Left _ -> property False

prop_keyword_parsing :: String -> Property
prop_keyword_parsing kw = 
  let keywords = ["let", "in", "if", "then", "else", "case", "of", "data", "type", "newtype", "class", "instance"]
      isKeyword = kw `elem` keywords
  in property $ isKeyword ==> 
    case Parser.parseKeyword kw of
      Right result -> property $ result === kw
      Left _ -> property False

prop_parentheses_balancing :: String -> Property
prop_parentheses_balancing input = 
  let balanced = count '(' input == count ')' input
  in case Parser.parseExpression input of
    Right _ -> property $ balanced === True
    Left _ -> property $ balanced === False
  where
    count c = length . filter (== c)

prop_bracket_balancing :: String -> Property
prop_bracket_balancing input = 
  let balanced = count '[' input == count ']' input
  in case Parser.parseList input of
    Right _ -> property $ balanced === True
    Left _ -> property $ balanced === False
  where
    count c = length . filter (== c)

prop_brace_balancing :: String -> Property
prop_brace_balancing input = 
  let balanced = count '{' input == count '}' input
  in case Parser.parseRecord input of
    Right _ -> property $ balanced === True
    Left _ -> property $ balanced === False
  where
    count c = length . filter (== c)

prop_indentation_parsing :: Int -> String -> Property
prop_indentation_parsing indent content = 
  let indented = replicate indent ' ' ++ content
  in case (Parser.parseStatement content, Parser.parseStatement indented) of
    (Right stmt1, Right stmt2) -> property $ stmt1 === stmt2
    _ -> property True

prop_empty_input_handling :: Property
prop_empty_input_handling = 
  case Parser.parseModule "" of
    Right result -> property $ null result === True
    Left _ -> property True

prop_unicode_handling :: String -> Property
prop_unicode_handling input = 
  case Parser.parseIdentifier input of
    Right result -> property $ all isValidUnicodeChar result
    Left _ -> property True
  where
    isValidUnicodeChar c = c > '\127' || isAlpha c || isDigit c || c == '_'

prop_escape_sequence_parsing :: String -> Property
prop_escape_sequence_parsing input = 
  let withEscapes = concatMap (\c -> if c == '\n' then "\\n" else if c == '\t' then "\\t" else [c]) input
  in case Parser.parseString ("\"" ++ withEscapes ++ "\"") of
    Right result -> property $ result === input
    Left _ -> property True

prop_nested_parsing :: String -> String -> Property
prop_nested_parsing outer inner = 
  let nested = outer ++ " (" ++ inner ++ ")"
  in case Parser.parseExpression nested of
    Right _ -> property True
    Left _ -> property False

prop_error_recovery :: String -> String -> Property
prop_error_recovery validPart invalidPart = 
  let mixed = validPart ++ " " ++ invalidPart ++ " " ++ validPart
  in case Parser.parseWithRecovery mixed of
    Right result -> property $ not (null result)
    Left _ -> property True

prop_line_continuation :: String -> String -> Property
prop_line_continuation line1 line2 = 
  let withContinuation = line1 ++ " \\\n" ++ line2
  in case (Parser.parseStatement line1, Parser.parseStatement withContinuation) of
    (Right stmt1, Right stmt2) -> property $ stmt1 === stmt2
    _ -> property True

tests :: TestTree
tests = testGroup "Parser Basic Functions Tests"
  [ testProperty "Identifier valid parsing" prop_identifier_valid
  , testProperty "Identifier invalid parsing" prop_identifier_invalid
  , testProperty "Number parsing" prop_number_parsing
  , testProperty "String parsing" prop_string_parsing
  , testProperty "Whitespace handling" prop_whitespace_handling
  , testProperty "Comment stripping" prop_comment_stripping
  , testProperty "Multiline comment stripping" prop_multiline_comment_stripping
  , testProperty "Operator parsing" prop_operator_parsing
  , testProperty "Keyword parsing" prop_keyword_parsing
  , testProperty "Parentheses balancing" prop_parentheses_balancing
  , testProperty "Bracket balancing" prop_bracket_balancing
  , testProperty "Brace balancing" prop_brace_balancing
  , testProperty "Indentation parsing" prop_indentation_parsing
  , testProperty "Empty input handling" prop_empty_input_handling
  , testProperty "Unicode handling" prop_unicode_handling
  , testProperty "Escape sequence parsing" prop_escape_sequence_parsing
  , testProperty "Nested parsing" prop_nested_parsing
  , testProperty "Error recovery" prop_error_recovery
  , testProperty "Line continuation" prop_line_continuation
  ]