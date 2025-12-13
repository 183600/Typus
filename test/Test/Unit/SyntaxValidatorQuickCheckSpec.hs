{-# LANGUAGE CPP #-}

module Test.Unit.SyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import SyntaxValidator
  ( SyntaxError(..)
  , validateSyntax
  , isValidIdentifier
  , isValidType
  )
import Data.List (isInfixOf)
import Data.Char (isAlphaNum, isLetter)

-- Property: SyntaxError with message and position
prop_syntaxerror_preserves :: String -> Int -> Int -> Property
prop_syntaxerror_preserves message line col =
  let error = SyntaxError message line col
  in errorMessage error === message &&
     errorLine error === line &&
     errorColumn error === col

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
  in not (null shown)

-- Property: SyntaxError show contains message
prop_syntaxerror_show_contains_message :: String -> Int -> Int -> Property
prop_syntaxerror_show_contains_message message line col =
  let error = SyntaxError message line col
      shown = show error
  in message `isInfixOf` shown

-- Property: SyntaxError show contains position
prop_syntaxerror_show_contains_position :: String -> Int -> Int -> Property
prop_syntaxerror_show_contains_position message line col =
  let error = SyntaxError message line col
      shown = show error
  in show line `isInfixOf` shown &&
     show col `isInfixOf` shown

-- Property: SyntaxError with empty message
prop_syntaxerror_empty_message :: Int -> Int -> Property
prop_syntaxerror_empty_message line col =
  let error = SyntaxError "" line col
  in errorMessage error === "" &&
     errorLine error === line &&
     errorColumn error === col

-- Property: SyntaxError with negative position
prop_syntaxerror_negative_position :: String -> Property
prop_syntaxerror_negative_position message =
  let error = SyntaxError message (-1) (-5)
  in errorMessage error === message &&
     errorLine error === -1 &&
     errorColumn error === -5

-- Property: SyntaxError with zero position
prop_syntaxerror_zero_position :: String -> Property
prop_syntaxerror_zero_position message =
  let error = SyntaxError message 0 0
  in errorMessage error === message &&
     errorLine error === 0 &&
     errorColumn error === 0

-- Property: SyntaxError with large position
prop_syntaxerror_large_position :: String -> Property
prop_syntaxerror_large_position message =
  let error = SyntaxError message 999999 999999
  in errorMessage error === message &&
     errorLine error === 999999 &&
     errorColumn error === 999999

-- Property: SyntaxError with special characters
prop_syntaxerror_special_chars :: Int -> Int -> Property
prop_syntaxerror_special_chars line col =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      error = SyntaxError specialChars line col
  in errorMessage error === specialChars

-- Property: SyntaxError with Unicode characters
prop_syntaxerror_unicode :: Int -> Int -> Property
prop_syntaxerror_unicode line col =
  let unicode = "测试错误信息🚀"
      error = SyntaxError unicode line col
  in errorMessage error === unicode

-- Property: isValidIdentifier with valid identifiers
prop_isvalididentifier_valid :: String -> Property
prop_isvalididentifier_valid name =
  not (null name) && isLetter (head name) && all isAlphaNum (tail name) ==> 
  isValidIdentifier name === True

-- Property: isValidIdentifier with invalid identifiers
prop_isvalididentifier_invalid_start :: String -> Property
prop_isvalididentifier_invalid_start name =
  not (null name) && not (isLetter (head name)) ==> 
  isValidIdentifier name === False

-- Property: isValidIdentifier with empty string
prop_isvalididentifier_empty :: Property
prop_isvalididentifier_empty =
  isValidIdentifier "" === False

-- Property: isValidIdentifier with only letters
prop_isvalididentifier_letters_only :: Property
prop_isvalididentifier_letters_only =
  let name = "testIdentifier"
  in isValidIdentifier name === True

-- Property: isValidIdentifier with letters and numbers
prop_isvalididentifier_alphanumeric :: Property
prop_isvalididentifier_alphanumeric =
  let name = "test123Identifier456"
  in isValidIdentifier name === True

-- Property: isValidIdentifier with special characters
prop_isvalididentifier_special_chars :: Property
prop_isvalididentifier_special_chars =
  let name = "test-identifier"
  in isValidIdentifier name === False

-- Property: isValidIdentifier with spaces
prop_isvalididentifier_spaces :: Property
prop_isvalididentifier_spaces =
  let name = "test identifier"
  in isValidIdentifier name === False

-- Property: isValidIdentifier with Unicode
prop_isvalididentifier_unicode :: Property
prop_isvalididentifier_unicode =
  let name = "测试标识符"
  in isValidIdentifier name === False -- Assuming only ASCII is valid

-- Property: isValidType with valid types
prop_isvalidtype_valid :: String -> Property
prop_isvalidtype_valid typeName =
  not (null typeName) && isLetter (head typeName) && all (\c -> isAlphaNum c || c == '_') (tail name) ==> 
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
  isValidType "" === False

-- Property: isValidType with special characters
prop_isvalidtype_special_chars :: Property
prop_isvalidtype_special_chars =
  let typeName = "test-type"
  in isValidType typeName === False

-- Property: isValidType with spaces
prop_isvalidtype_spaces :: Property
prop_isvalidtype_spaces =
  let typeName = "test type"
  in isValidType typeName === False

-- Property: validateSyntax with empty input
prop_validatesyntax_empty :: Property
prop_validatesyntax_empty =
  let errors = validateSyntax ""
  in null errors

-- Property: validateSyntax with simple valid code
prop_validatesyntax_simple_valid :: Property
prop_validatesyntax_simple_valid =
  let code = "package main\nfunc main() {}\n"
      errors = validateSyntax code
  in null errors

-- Property: validateSyntax with invalid code
prop_validatesyntax_invalid :: Property
prop_validatesyntax_invalid =
  let code = "func main {\n"  // Missing parentheses
      errors = validateSyntax code
  in not (null errors)

-- Property: SyntaxError with same position different message
prop_syntaxerror_same_position_different_message :: Int -> Int -> String -> String -> Property
prop_syntaxerror_same_position_different_message line col msg1 msg2 =
  let err1 = SyntaxError msg1 line col
      err2 = SyntaxError msg2 line col
  in (err1 == err2) === (msg1 == msg2)

-- Property: SyntaxError with same message different position
prop_syntaxerror_same_message_different_position :: String -> Int -> Int -> Int -> Int -> Property
prop_syntaxerror_same_message_different_position msg line1 col1 line2 col2 =
  let err1 = SyntaxError msg line1 col1
      err2 = SyntaxError msg line2 col2
  in (err1 == err2) === (line1 == line2 && col1 == col2)

-- Property: SyntaxError ordering by message
prop_syntaxerror_ordering_by_message :: String -> String -> Property
prop_syntaxerror_ordering_by_message msg1 msg2 =
  let err1 = SyntaxError msg1 0 0
      err2 = SyntaxError msg2 0 0
      result = compare err1 err2
  in (msg1 <= msg2) ==> (result == LT || result == EQ)

-- Property: SyntaxError ordering by line when messages equal
prop_syntaxerror_ordering_by_line :: String -> Int -> Int -> Property
prop_syntaxerror_ordering_by_line msg line1 line2 =
  let err1 = SyntaxError msg line1 0
      err2 = SyntaxError msg line2 0
      result = compare err1 err2
  in (line1 <= line2) ==> (result == LT || result == EQ)

-- Property: SyntaxError ordering by column when messages and lines equal
prop_syntaxerror_ordering_by_column :: String -> Int -> Int -> Int -> Property
prop_syntaxerror_ordering_by_column msg line col1 col2 =
  let err1 = SyntaxError msg line col1
      err2 = SyntaxError msg line col2
      result = compare err1 err2
  in (col1 <= col2) ==> (result == LT || result == EQ)

-- Property: isValidIdentifier with single character
prop_isvalididentifier_single_char :: Property
prop_isvalididentifier_single_char =
  let name = "a"
  in isValidIdentifier name === True

-- Property: isValidIdentifier with single number
prop_isvalididentifier_single_number :: Property
prop_isvalididentifier_single_number =
  let name = "1"
  in isValidIdentifier name === False

-- Property: isValidType with single character
prop_isvalidtype_single_char :: Property
prop_isvalidtype_single_char =
  let typeName = "T"
  in isValidType typeName === True

-- Property: isValidType with single underscore
prop_isvalidtype_single_underscore :: Property
prop_isvalidtype_single_underscore =
  let typeName = "_"
  in isValidType typeName === False

-- Property: isValidType with consecutive underscores
prop_isvalidtype_consecutive_underscores :: Property
prop_isvalidtype_consecutive_underscores =
  let typeName = "test__type"
  in isValidType typeName === True

-- Property: validateSyntax with only whitespace
prop_validatesyntax_whitespace :: Property
prop_validatesyntax_whitespace =
  let code = "   \n\t\n  \n"
      errors = validateSyntax code
  in null errors

-- Property: validateSyntax with comments only
prop_validatesyntax_comments :: Property
prop_validatesyntax_comments =
  let code = "// This is a comment\n/* Another comment */\n"
      errors = validateSyntax code
  in null errors

-- Property: validateSyntax with mixed valid and invalid
prop_validatesyntax_mixed :: Property
prop_validatesyntax_mixed =
  let code = "package main\nfunc valid() {}\nfunc invalid {\n"  // Last function is invalid
      errors = validateSyntax code
  in not (null errors)

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