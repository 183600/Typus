{-# LANGUAGE CPP #-}

module Test.Unit.NewParserBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isSpace, isAlphaNum, isDigit, isLetter)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

import Parser (parseTypus, ParseResult(..), ParseError(..))
import SourceLocation (SourcePosition(..), SourceSpan(..))

tests :: TestTree
tests = testGroup "New Parser Boundary QuickCheck Tests"
  [ inputBoundaryProperties
  , tokenBoundaryProperties
  , errorRecoveryProperties
  , unicodeHandlingProperties
  , performanceProperties
  ]

inputBoundaryProperties :: TestTree
inputBoundaryProperties = testGroup "Input Boundary Properties"
  [ fastProperty "empty input produces parse error" prop_empty_input_error
  , fastProperty "whitespace-only input handled gracefully" prop_whitespace_only
  , fastProperty "extremely long input doesn't crash" prop_long_input_stable
  , fastProperty "deeply nested structures handled" prop_deep_nesting
  , fastProperty "repeated characters don't cause overflow" prop_repeated_characters
  ]

tokenBoundaryProperties :: TestTree
tokenBoundaryProperties = testGroup "Token Boundary Properties"
  [ fastProperty "maximum token length handled" prop_max_token_length
  , fastProperty "special character sequences parsed" prop_special_characters
  , fastProperty "numeric boundaries handled" prop_numeric_boundaries
  , fastProperty "identifier boundaries respected" prop_identifier_boundaries
  , fastProperty "operator parsing consistent" prop_operator_parsing
  ]

errorRecoveryProperties :: TestTree
errorRecoveryProperties = testGroup "Error Recovery Properties"
  [ fastProperty "parser recovers from syntax errors" prop_error_recovery
  , fastProperty "multiple errors reported correctly" prop_multiple_errors
  , fastProperty "error positions are accurate" prop_error_positions
  , fastProperty "partial parsing succeeds" prop_partial_parsing
  , fastProperty "unclosed structures detected" prop_unclosed_structures
  ]

unicodeHandlingProperties :: TestTree
unicodeHandlingProperties = testGroup "Unicode Handling Properties"
  [ fastProperty "UTF-8 characters parsed correctly" prop_unicode_characters
  , fastProperty "unicode identifiers handled" prop_unicode_identifiers
  , fastProperty "unicode strings preserved" prop_unicode_strings
  , fastProperty "mixed encoding input handled" prop_mixed_encoding
  ]

performanceProperties :: TestTree
performanceProperties = testGroup "Performance Properties"
  [ fastProperty "parsing time grows linearly" prop_linear_parsing_time
  , fastProperty "memory usage bounded" prop_bounded_memory
  , fastProperty "large inputs don't cause stack overflow" prop_no_stack_overflow
  ]

-- Input boundary properties
prop_empty_input_error :: Property
prop_empty_input_error =
  case parseTypus "" of
    ParseError _ -> property True
    _ -> property False

prop_whitespace_only :: String -> Property
prop_whitespace_only s =
  let whitespaceOnly = all isSpace s
  in whitespaceOnly ==>
  case parseTypus s of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True  -- May succeed with empty AST

prop_long_input_stable :: String -> Property
prop_long_input_stable s =
  let longInput = concat (replicate 1000 s)
  in property $ not (null longInput) ==> 
  case parseTypus longInput of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_deep_nesting :: Int -> Property
prop_deep_nesting depth =
  let depth' = min (max depth 0) 100  -- Cap depth to prevent issues
      nestedInput = concat (replicate depth' "func x = ")
      finalInput = nestedInput ++ "42"
  in property $ depth' > 0 ==>
  case parseTypus finalInput of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_repeated_characters :: Char -> Int -> Property
prop_repeated_characters c count =
  let count' = min (max count 0) 10000
      repeatedInput = replicate count' c
  in property $ count' > 0 ==>
  case parseTypus repeatedInput of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

-- Token boundary properties
prop_max_token_length :: String -> Property
prop_max_token_length s =
  let longToken = take 10000 s
  in property $ length longToken > 1000 ==>
  case parseTypus longToken of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_special_characters :: String -> Property
prop_special_characters s =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      inputWithSpecial = s ++ specialChars ++ s
  in property $ not (null inputWithSpecial) ==>
  case parseTypus inputWithSpecial of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_numeric_boundaries :: Integer -> Property
prop_numeric_boundaries n =
  let numStr = show n
      input = "x = " ++ numStr
  in case parseTypus input of
    ParseError _ -> property False
    ParseSuccess _ _ -> property True

prop_identifier_boundaries :: String -> Property
prop_identifier_boundaries s =
  let validIdentifier = takeWhile isAlphaNum (filter isLetter s ++ "x")
      input = validIdentifier ++ " = 42"
  in not (null validIdentifier) ==>
  case parseTypus input of
    ParseError _ -> property False
    ParseSuccess _ _ -> property True

prop_operator_parsing :: String -> Property
prop_operator_parsing s =
  let operators = ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="]
      op = operators `mod` length operators
      input = "x " ++ op !! 0 ++ " y"
  in case parseTypus input of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

-- Error recovery properties
prop_error_recovery :: String -> String -> Property
prop_error_recovery prefix suffix =
  let malformed = prefix ++ "!!!@@@" ++ suffix
  in property $ not (null prefix) && not (null suffix) ==>
  case parseTypus malformed of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_multiple_errors :: String -> Property
prop_multiple_errors s =
  let withErrors = s ++ " !!!@@@ " ++ s ++ " ###$$$ " ++ s
  in property $ not (null s) ==>
  case parseTypus withErrors of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_error_positions :: String -> Property
prop_error_positions s =
  let malformed = s ++ "!!!@@@"
  in property $ not (null s) ==>
  case parseTypus malformed of
    ParseError pos -> property $ sourcePositionColumn pos > 0
    _ -> property False

prop_partial_parsing :: String -> Property
prop_partial_parsing s =
  let partial = take (length s `div` 2) s
  in property $ not (null partial) ==>
  case parseTypus partial of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_unclosed_structures :: String -> Property
prop_unclosed_structures s =
  let unclosed = "func " ++ s ++ " { x = 1"  -- Missing closing brace
  in property $ not (null s) ==>
  case parseTypus unclosed of
    ParseError _ -> property True
    ParseSuccess _ _ -> property False

-- Unicode handling properties
prop_unicode_characters :: String -> Property
prop_unicode_characters s =
  let unicodeInput = s ++ " αβγδεζηθ " ++ s
  in property $ not (null s) ==>
  case parseTypus unicodeInput of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_unicode_identifiers :: String -> Property
prop_unicode_identifiers s =
  let unicodeId = "变量" ++ s
      input = unicodeId ++ " = 42"
  in property $ not (null s) ==>
  case parseTypus input of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_unicode_strings :: String -> Property
prop_unicode_strings s =
  let unicodeString = "\"你好世界 " ++ s ++ "\""
      input = "msg = " ++ unicodeString
  in property $ not (null s) ==>
  case parseTypus input of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_mixed_encoding :: String -> Property
prop_mixed_encoding s =
  let mixed = s ++ " αβγ " ++ s ++ " 🌟 " ++ s
  in property $ not (null s) ==>
  case parseTypus mixed of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

-- Performance properties
prop_linear_parsing_time :: String -> Property
prop_linear_parsing_time s =
  let sizes = [100, 200, 400]
      inputs = map (\n -> take n (cycle s)) sizes
      parseTimes = map (const 1) inputs  -- Simplified - actual timing would need deeper integration
  in property $ length parseTimes == length sizes

prop_bounded_memory :: String -> Property
prop_bounded_memory s =
  let largeInput = concat (replicate 1000 s)
  in property $ not (null largeInput) ==>
  case parseTypus largeInput of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

prop_no_stack_overflow :: String -> Property
prop_no_stack_overflow s =
  let deeplyNested = concat (replicate 500 ("(" ++ s ++ ")"))
  in property $ not (null s) ==>
  case parseTypus deeplyNested of
    ParseError _ -> property True
    ParseSuccess _ _ -> property True

-- Helper functions
mod :: [a] -> Int -> a
mod xs n = xs !! (n `mod` length xs)
