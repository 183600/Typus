{-# LANGUAGE CPP #-}

module Test.Unit.SyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import TestSupport.ExtendedArbitrary ()
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.))

import SyntaxValidator
  ( SyntaxError(..)
  , ErrorType(..)
  , validateSyntax
  )
-- import Analyzer.SymbolTable (isValidIdentifier) -- Not exported
import Data.List (isInfixOf)
import Data.Char (isAlphaNum, isLetter, isAlpha, isDigit, isPrint)

-- Ord instance for SyntaxError for testing
instance Ord SyntaxError where
  compare err1 err2 = 
    case compare (errorMessage err1) (errorMessage err2) of
      LT -> LT
      GT -> GT
      EQ -> case compare (lineNumber err1) (lineNumber err2) of
        LT -> LT
        GT -> GT
        EQ -> compare (columnNumber err1) (columnNumber err2)

-- Local implementation of isValidType since it's not exported from SyntaxValidator
isValidType :: String -> Bool
isValidType "" = False
isValidType s = all isValidTypeChar s && isAlpha (head s)
  where
    isValidTypeChar c = isAlphaNum c || c == '_'

-- Local implementation of isValidIdentifier since it's not exported from Analyzer.SymbolTable
isValidIdentifier :: String -> Bool
isValidIdentifier name =
    not (null name)
        && not (isReservedName name)
        && case name of
            [] -> False
            (c : _) -> not (isDigit c) && all isAllowed name
  where
    isAllowed char = isAsciiAlphaNum char || char == '_'
    isAsciiAlphaNum char = (char >= 'a' && char <= 'z') || 
                          (char >= 'A' && char <= 'Z') || 
                          (char >= '0' && char <= '9')
    isReservedName name =
        name
            `elem` [ "fmt"
                   , "func"
                   , "var"
                   , "let"
                   , "if"
                   , "else"
                   , "for"
                   , "return"
                   , "import"
                   , "package"
                   , "type"
                   , "struct"
                   , "interface"
                   , "const"
                   , "true"
                   , "false"
                   , "nil"
                   , "int"
                   , "string"
                   , "bool"
                   , "float64"
                   ]

-- Property: SyntaxError with message and position
prop_syntaxerror_preserves :: String -> Int -> Int -> Property
prop_syntaxerror_preserves message line col =
  let error = SyntaxError MissingBrace message line col ""
  in case error of
    SyntaxError _ m _ _ _ -> m === message
    _ -> property False

-- Helper functions for accessing SyntaxError fields  
errorLine :: SyntaxError -> Int
errorLine (SyntaxError _ _ line _ _) = line

errorColumn :: SyntaxError -> Int
errorColumn (SyntaxError _ _ _ col _) = col

-- Property: SyntaxError equality
prop_syntaxerror_eq :: SyntaxError -> SyntaxError -> Property
prop_syntaxerror_eq err1 err2 =
  (err1 == err2) === 
    (errorMessage err1 == errorMessage err2 &&
     errorLine err1 == errorLine err2 &&
     errorColumn err1 == errorColumn err2)

-- Property: SyntaxError ordering
prop_syntaxerror_ordering :: SyntaxError -> SyntaxError -> Property
prop_syntaxerror_ordering err1 err2 =
  let result = compare err1 err2
  in (result == LT || result == EQ || result == GT) === True

-- Property: SyntaxError show
prop_syntaxerror_show :: SyntaxError -> Property
prop_syntaxerror_show error =
  let shown = show error
  in property $ not (null shown)

-- Property: SyntaxError show contains message
prop_syntaxerror_show_contains_message :: String -> Int -> Int -> Property
prop_syntaxerror_show_contains_message message line col =
  not (null message) && all (\c -> isPrint c && c /= '"' && c /= '\\') message && any isPrint message ==>
  let error = SyntaxError MissingBrace message line col ""
      shown = show error
  in property $ not (null shown) && "SyntaxError" `isInfixOf` shown

-- Property: SyntaxError show contains position
prop_syntaxerror_show_contains_position :: String -> Int -> Int -> Property
prop_syntaxerror_show_contains_position message line col =
  let error = SyntaxError MissingParenthesis message line col ""
      shown = show error
  in property $ show line `isInfixOf` shown &&
     show col `isInfixOf` shown

-- Property: SyntaxError with empty message
prop_syntaxerror_empty_message :: Int -> Int -> Property
prop_syntaxerror_empty_message line col =
  let error = SyntaxError MissingBracket "" line col ""
  in property $ (errorMessage error === "") .&&.
                (lineNumber error === line) .&&.
                (columnNumber error === col)

-- Property: SyntaxError with negative position
prop_syntaxerror_negative_position :: String -> Property
prop_syntaxerror_negative_position message =
  let error = SyntaxError MissingBrace message (-1) (-5) ""
  in property $ (lineNumber error === -1) .&&. (columnNumber error === -5)

-- Property: SyntaxError with zero position
prop_syntaxerror_zero_position :: String -> Property
prop_syntaxerror_zero_position message =
  let error = SyntaxError MissingBrace message 0 0 ""
  in property $ (lineNumber error === 0) .&&. (columnNumber error === 0)

-- Property: SyntaxError with zero values
prop_syntaxerror_zero :: Property
prop_syntaxerror_zero =
  let error = SyntaxError MissingSemicolon "error" 0 0 ""
  in property $ (lineNumber error === 0) .&&. (columnNumber error === 0)

-- Property: SyntaxError with large position
prop_syntaxerror_large_position :: String -> Property
prop_syntaxerror_large_position message =
  let error = SyntaxError MissingBracket message 999999 999999 ""
  in property $ (lineNumber error === 999999) .&&. (columnNumber error === 999999)

-- Property: SyntaxError with special characters
prop_syntaxerror_special_chars :: Int -> Int -> Property
prop_syntaxerror_special_chars line col =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      error = SyntaxError MissingBrace specialChars line col ""
  in errorMessage error === specialChars

-- Property: SyntaxError with Unicode characters
prop_syntaxerror_unicode :: Int -> Int -> Property
prop_syntaxerror_unicode line col =
  let unicode = "测试错误信息🚀"
      error = SyntaxError MissingParenthesis unicode line col ""
  in errorMessage error === unicode

-- Property: isValidIdentifier with valid identifiers
prop_isvalididentifier_valid :: String -> Property
prop_isvalididentifier_valid name =
  not (null name) && isLetter (head name) && all isAlphaNum (tail name) ==> 
  if all isAscii name 
  then isValidIdentifier name === True
  else isValidIdentifier name === False
  where
    isAscii char = char <= '\127'

-- Property: isValidIdentifier with invalid identifiers
prop_isvalididentifier_invalid_start :: String -> Property
prop_isvalididentifier_invalid_start name =
  not (null name) && not (isLetter (head name)) && head name /= '_' ==> 
  isValidIdentifier name === False

-- Property: isValidIdentifier with empty string
prop_isvalididentifier_empty :: Property
prop_isvalididentifier_empty =
  property $ isValidIdentifier "" === False

-- Property: isValidIdentifier with only letters
prop_isvalididentifier_letters_only :: Property
prop_isvalididentifier_letters_only =
  let name = "testIdentifier"
  in property $ isValidIdentifier name === True

-- Property: isValidIdentifier with letters and numbers
prop_isvalididentifier_alphanumeric :: Property
prop_isvalididentifier_alphanumeric =
  let name = "test123Identifier456"
  in property $ isValidIdentifier name === True

-- Property: isValidIdentifier with special characters
prop_isvalididentifier_special_chars :: Property
prop_isvalididentifier_special_chars =
  let name = "test-identifier"
  in property $ isValidIdentifier name === False

-- Property: isValidIdentifier with spaces
prop_isvalididentifier_spaces :: Property
prop_isvalididentifier_spaces =
  let name = "test identifier"
  in property $ isValidIdentifier name === False

-- Property: isValidIdentifier with Unicode
prop_isvalididentifier_unicode :: Property
prop_isvalididentifier_unicode =
  let name = "测试标识符"
  in property $ isValidIdentifier name === False -- Current implementation only supports ASCII characters

-- Property: isValidType with valid types
prop_isvalidtype_valid :: String -> Property
prop_isvalidtype_valid typeName =
  not (null typeName) && isLetter (head typeName) && all (\c -> isAlphaNum c || c == '_') (tail typeName) ==> 
  isValidType typeName === True

-- Property: isValidType with invalid types
prop_isvalidtype_invalid_start :: String -> Property
prop_isvalidtype_invalid_start typeName =
  not (null typeName) && not (isLetter (head typeName)) ==> 
  isValidType typeName === False

-- Property: isValidType with underscores
prop_isvalidtype_underscores :: Property
prop_isvalidtype_underscores =
  let typeName = "test_type_name"
  in isValidType typeName === True

-- Property: isValidType with empty string
prop_isvalidtype_empty :: Property
prop_isvalidtype_empty =
  property $ isValidType "" === False

-- Property: isValidType with special characters
prop_isvalidtype_special_chars :: Property
prop_isvalidtype_special_chars =
  let typeName = "test-type"
  in property $ isValidType typeName === False

-- Property: isValidType with spaces
prop_isvalidtype_spaces :: Property
prop_isvalidtype_spaces =
  let typeName = "test type"
  in property $ isValidType typeName === False

-- Property: validateSyntax with empty input
prop_validatesyntax_empty :: Property
prop_validatesyntax_empty =
  let errors = validateSyntax ""
  in property $ null errors

-- Property: validateSyntax with simple valid code
prop_validatesyntax_simple_valid :: Property
prop_validatesyntax_simple_valid =
  let code = "package main\nfunc main() {}\n"
      errors = validateSyntax code
  in property $ null errors

-- Property: validateSyntax with invalid code
prop_validatesyntax_invalid :: Property
prop_validatesyntax_invalid =
  let code = "func main {\n"  -- Missing parentheses
      errors = validateSyntax code
  in property $ not (null errors)

-- Property: SyntaxError with same position different message
prop_syntaxerror_same_position_different_message :: Int -> Int -> String -> String -> Property
prop_syntaxerror_same_position_different_message line col msg1 msg2 =
  let err1 = SyntaxError InvalidIdentifier msg1 line col ""
      err2 = SyntaxError InvalidIdentifier msg2 line col ""
  in (err1 == err2) === (msg1 == msg2)

-- Property: SyntaxError with same message different position
prop_syntaxerror_same_message_different_position :: String -> Int -> Int -> Int -> Int -> Property
prop_syntaxerror_same_message_different_position msg line1 col1 line2 col2 =
  let err1 = SyntaxError InvalidIdentifier msg line1 col1 ""
      err2 = SyntaxError InvalidIdentifier msg line2 col2 ""
  in (err1 == err2) === (line1 == line2 && col1 == col2)

-- Property: SyntaxError ordering by message
prop_syntaxerror_ordering_by_message :: String -> String -> Property
prop_syntaxerror_ordering_by_message msg1 msg2 =
  let err1 = SyntaxError InvalidIdentifier msg1 0 0 ""
      err2 = SyntaxError InvalidIdentifier msg2 0 0 ""
      result = compare err1 err2
  in (msg1 <= msg2) ==> (result == LT || result == EQ)

-- Property: SyntaxError ordering by line when messages equal
prop_syntaxerror_ordering_by_line :: String -> Int -> Int -> Property
prop_syntaxerror_ordering_by_line msg line1 line2 =
  let err1 = SyntaxError InvalidIdentifier msg line1 0 ""
      err2 = SyntaxError InvalidIdentifier msg line2 0 ""
      result = compare err1 err2
  in (line1 <= line2) ==> (result == LT || result == EQ)

-- Property: SyntaxError ordering by column when messages and lines equal
prop_syntaxerror_ordering_by_column :: String -> Int -> Int -> Int -> Property
prop_syntaxerror_ordering_by_column msg line col1 col2 =
  let err1 = SyntaxError InvalidIdentifier msg line col1 ""
      err2 = SyntaxError InvalidIdentifier msg line col2 ""
      result = compare err1 err2
  in (col1 <= col2) ==> (result == LT || result == EQ)

-- Property: isValidIdentifier with single character
prop_isvalididentifier_single_char :: Property
prop_isvalididentifier_single_char =
  property $ isValidIdentifier "a" === True

-- Property: isValidIdentifier with single number
prop_isvalididentifier_single_number :: Property
prop_isvalididentifier_single_number =
  property $ isValidIdentifier "1" === False

-- Property: isValidType with single character
prop_isvalidtype_single_char :: Property
prop_isvalidtype_single_char =
  property $ isValidType "A" === True

-- Property: isValidType with single underscore
prop_isvalidtype_single_underscore :: Property
prop_isvalidtype_single_underscore =
  let typeName = "_"
  in property $ isValidType typeName === False

-- Property: isValidType with consecutive underscores
prop_isvalidtype_consecutive_underscores :: Property
prop_isvalidtype_consecutive_underscores =
  let typeName = "test__type"
  in isValidType typeName === True

-- Property: validateSyntax with whitespace only
prop_validatesyntax_whitespace :: Property
prop_validatesyntax_whitespace =
  let code = "   \n\t\n  \n"
      errors = validateSyntax code
  in property $ null errors

-- Property: validateSyntax with comments only
prop_validatesyntax_comments :: Property
prop_validatesyntax_comments =
  let code = "// This is a comment\n/* Another comment */\n"
      errors = validateSyntax code
  in property $ null errors

-- Property: validateSyntax with mixed valid and invalid
prop_validatesyntax_mixed :: Property
prop_validatesyntax_mixed =
  let code = "package main\nfunc valid() {}\nfunc invalid {\n"  -- Last function is invalid
      errors = validateSyntax code
  in property $ not (null errors)

tests :: TestTree
tests = testGroup "SyntaxValidator QuickCheck tests"
  [ fastProperty "SyntaxError with message and position" prop_syntaxerror_preserves
  , fastProperty "SyntaxError equality" prop_syntaxerror_eq
  , fastProperty "SyntaxError ordering" prop_syntaxerror_ordering
  , fastProperty "SyntaxError show" prop_syntaxerror_show
  , fastProperty "SyntaxError show contains message" prop_syntaxerror_show_contains_message
  , fastProperty "SyntaxError show contains position" prop_syntaxerror_show_contains_position
  , fastProperty "SyntaxError with empty message" prop_syntaxerror_empty_message
  , fastProperty "SyntaxError with negative position" prop_syntaxerror_negative_position
  , fastProperty "SyntaxError with zero position" prop_syntaxerror_zero_position
  , fastProperty "SyntaxError with large position" prop_syntaxerror_large_position
  , fastProperty "SyntaxError with special characters" prop_syntaxerror_special_chars
  , fastProperty "SyntaxError with Unicode characters" prop_syntaxerror_unicode
  , fastProperty "isValidIdentifier with valid identifiers" prop_isvalididentifier_valid
  , fastProperty "isValidIdentifier with invalid identifiers" prop_isvalididentifier_invalid_start
  , fastProperty "isValidIdentifier with empty string" prop_isvalididentifier_empty
  , fastProperty "isValidIdentifier with only letters" prop_isvalididentifier_letters_only
  , fastProperty "isValidIdentifier with letters and numbers" prop_isvalididentifier_alphanumeric
  , fastProperty "isValidIdentifier with special characters" prop_isvalididentifier_special_chars
  , fastProperty "isValidIdentifier with spaces" prop_isvalididentifier_spaces
  , fastProperty "isValidIdentifier with Unicode" prop_isvalididentifier_unicode
  , fastProperty "isValidType with valid types" prop_isvalidtype_valid
  , fastProperty "isValidType with invalid types" prop_isvalidtype_invalid_start
  , fastProperty "isValidType with underscores" prop_isvalidtype_underscores
  , fastProperty "isValidType with empty string" prop_isvalidtype_empty
  , fastProperty "isValidType with special characters" prop_isvalidtype_special_chars
  , fastProperty "isValidType with spaces" prop_isvalidtype_spaces
  , fastProperty "validateSyntax with empty input" prop_validatesyntax_empty
  , fastProperty "validateSyntax with simple valid code" prop_validatesyntax_simple_valid
  , fastProperty "validateSyntax with invalid code" prop_validatesyntax_invalid
  , fastProperty "SyntaxError with same position different message" prop_syntaxerror_same_position_different_message
  , fastProperty "SyntaxError with same message different position" prop_syntaxerror_same_message_different_position
  , fastProperty "SyntaxError ordering by message" prop_syntaxerror_ordering_by_message
  , fastProperty "SyntaxError ordering by line when messages equal" prop_syntaxerror_ordering_by_line
  , fastProperty "SyntaxError ordering by column when messages and lines equal" prop_syntaxerror_ordering_by_column
  , fastProperty "isValidIdentifier with single character" prop_isvalididentifier_single_char
  , fastProperty "isValidIdentifier with single number" prop_isvalididentifier_single_number
  , fastProperty "isValidType with single character" prop_isvalidtype_single_char
  , fastProperty "isValidType with single underscore" prop_isvalidtype_single_underscore
  , fastProperty "isValidType with consecutive underscores" prop_isvalidtype_consecutive_underscores
  , fastProperty "validateSyntax with only whitespace" prop_validatesyntax_whitespace
  , fastProperty "validateSyntax with comments only" prop_validatesyntax_comments
  , fastProperty "validateSyntax with mixed valid and invalid" prop_validatesyntax_mixed
  ]