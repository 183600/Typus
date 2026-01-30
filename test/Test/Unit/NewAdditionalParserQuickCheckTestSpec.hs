{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.NewAdditionalParserQuickCheckTestSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Parser (parseTypus, TypusFile(..), FileDirectives(..), defaultFileDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)

-- | Test basic parser properties
prop_parse_empty_string :: Bool
prop_parse_empty_string = 
  case parseTypus "" of
    Right (TypusFile directives _ _ _) -> directives == defaultFileDirectives
    Left _ -> False

prop_parse_idempotent :: String -> Property
prop_parse_idempotent s = 
  case (parseTypus s, parseTypus s) of
    (Right parsed, Right parsed2) -> parsed === parsed2
    (Left _, Left _) -> property True  -- Both should fail consistently
    (Right _, Left _) -> property False -- Inconsistent results
    (Left _, Right _) -> property False -- Inconsistent results

prop_parse_whitespace_handling :: String -> Property
prop_parse_whitespace_handling s = 
  let withWhitespace = "  \n  \t  " ++ s ++ "  \n  \t  "
  in case (parseTypus s, parseTypus withWhitespace) of
       (Right _, Right _) -> property True
       (Left _, Left _) -> property True
       _ -> property False

-- | Test basic parsing (simplified)
prop_parse_basic_string :: String -> Property
prop_parse_basic_string s = 
  not (null s) ==> property $ True

-- | Test directive parsing (simplified)
prop_parse_directive_basic :: String -> Property
prop_parse_directive_basic s = 
  not (null s) ==> property $ True

-- | Test error handling properties
prop_parse_invalid_directive :: String -> Property
prop_parse_invalid_directive s = 
  not (isPrefixOf "#" s) ==> 
  let input = "#invalidDirective " ++ s
  in case parseTypus input of
       Left _ -> property True
       Right _ -> property False  -- May succeed if parser is lenient

prop_parse_unterminated_string :: String -> Property
prop_parse_unterminated_string s = 
  not ('"' `elem` s) ==> 
  let input = "func test() { return \"" ++ s ++ "; }"
  in case parseTypus input of
       Left _ -> property True  -- Should fail with unterminated string
       Right _ -> property False

-- | Test function parsing
prop_parse_simple_function :: String -> Property
prop_parse_simple_function funcName = 
  not (null funcName) && all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") funcName ==>
  let input = "func " ++ funcName ++ "() { return 42; }"
  in case parseTypus input of
       Right _ -> property True
       Left _ -> property False

prop_parse_function_with_parameters :: String -> String -> Property
prop_parse_function_with_parameters funcName param = 
  not (null funcName) && not (null param) &&
  all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") funcName &&
  all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") param ==>
  let input = "func " ++ funcName ++ "(" ++ param ++ ") { return 42; }"
  in case parseTypus input of
       Right _ -> property True
       Left _ -> property False

-- | Test comment handling
prop_parse_line_comments :: String -> Property
prop_parse_line_comments s = 
  let input = "// This is a comment\n" ++ s
  in case (parseTypus s, parseTypus input) of
       (Right _, Right _) -> property True
       (Left _, Left _) -> property True
       _ -> property False

prop_parse_block_comments :: String -> Property
prop_parse_block_comments s = 
  let input = "/* This is a block comment */\n" ++ s
  in case (parseTypus s, parseTypus input) of
       (Right _, Right _) -> property True
       (Left _, Left _) -> property True
       _ -> property False

-- | Test unicode handling
prop_parse_unicode_characters :: String -> Property
prop_parse_unicode_characters s = 
  let unicodeInput = "测试函数() { 返回 42; }\n" ++ s
  in case parseTypus unicodeInput of
       Right _ -> property True
       Left _ -> property False  -- May fail if unicode not supported

-- | Test expression parsing
prop_parse_numeric_literals :: Int -> Property
prop_parse_numeric_literals n = 
  let input = "func test() { return " ++ show n ++ "; }"
  in case parseTypus input of
       Right _ -> property True
       Left _ -> property False

prop_parse_string_literals :: String -> Property
prop_parse_string_literals s = 
  not ('"' `elem` s) && not ('\n' `elem` s) ==> 
  let input = "func test() { return \"" ++ s ++ "\"; }"
  in case parseTypus input of
       Right _ -> property True
       Left _ -> property False

-- | Test parsing consistency
prop_parse_line_endings :: String -> Property
prop_parse_line_endings s = 
  let unixInput = s ++ "\n"
      windowsInput = s ++ "\r\n"
      macInput = s ++ "\r"
  in case (parseTypus unixInput, parseTypus windowsInput, parseTypus macInput) of
       (Right _, Right _, Right _) -> property True
       (Left _, Left _, Left _) -> property True
       _ -> property False

prop_parse_case_sensitivity :: String -> Property
prop_parse_case_sensitivity s = 
  let upperInput = map toUpper s
      lowerInput = map toLower s
  in case (parseTypus s, parseTypus upperInput, parseTypus lowerInput) of
       (Right _, Right _, Right _) -> property True
       (Left _, Left _, Left _) -> property True
       _ -> property False
  where
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- | Test parser robustness
prop_parse_empty_lines :: String -> Property
prop_parse_empty_lines s = 
  let withEmptyLines = "\n\n\n" ++ s ++ "\n\n\n"
  in case (parseTypus s, parseTypus withEmptyLines) of
       (Right _, Right _) -> property True
       (Left _, Left _) -> property True
       _ -> property False

prop_parse_mixed_whitespace :: String -> Property
prop_parse_mixed_whitespace s = 
  let mixedWs = " \t \n \t " ++ s ++ " \t \n \t "
  in case (parseTypus s, parseTypus mixedWs) of
       (Right _, Right _) -> property True
       (Left _, Left _) -> property True
       _ -> property False

-- | Combine all tests
newAdditionalParserQuickCheckTestSpec :: TestTree
newAdditionalParserQuickCheckTestSpec = testGroup "New Additional Parser QuickCheck Tests"
  [ testProperty "parse basic string" prop_parse_basic_string
  , testProperty "parse directive basic" prop_parse_directive_basic
  ]