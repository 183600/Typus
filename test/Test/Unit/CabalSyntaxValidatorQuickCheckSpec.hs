{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CabalSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum, isLetter, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)

import SyntaxValidator (validateSyntax, SyntaxError(..), ValidationResult(..))
import Parser (parseTypus)
import SourceLocation (SourceSpan(..), mkSourceSpan, mkSourcePos)

-- Simple arbitrary instances for syntax validation testing
newtype ValidIdentifier = ValidIdentifier String deriving (Show, Eq)

instance Arbitrary ValidIdentifier where
  arbitrary = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
    return $ ValidIdentifier (first : rest)

newtype ValidFunctionName = ValidFunctionName String deriving (Show, Eq)

instance Arbitrary ValidFunctionName where
  arbitrary = do
    ValidIdentifier ident <- arbitrary
    return $ ValidFunctionName ident

newtype ValidTypeName = ValidTypeName String deriving (Show, Eq)

instance Arbitrary ValidTypeName where
  arbitrary = do
    first <- elements ['A'..'Z']
    rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    return $ ValidTypeName (first : rest)

-- Property: Valid function names pass validation
prop_valid_function_name_passes :: ValidFunctionName -> Property
prop_valid_function_name_passes (ValidFunctionName name) =
  let code = "func " ++ name ++ "() { return 42 }"
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case validateSyntax parsed of
           ValidationResult [] -> property True
           ValidationResult errors -> counterexample ("Validation failed: " ++ show errors) $ property False

-- Property: Valid type names pass validation
prop_valid_type_name_passes :: ValidTypeName -> Property
prop_valid_type_name_passes (ValidTypeName name) =
  let code = "func test() " ++ name ++ " { return " ++ name ++ "{} }"
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case validateSyntax parsed of
           ValidationResult [] -> property True
           ValidationResult errors -> counterexample ("Validation failed: " ++ show errors) $ property False

-- Property: Invalid identifiers fail validation
prop_invalid_identifier_fails :: String -> Property
prop_invalid_identifier_fails ident =
  let isValid = not (null ident) && isLetter (head ident) && all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])) ident
      code = "func " ++ ident ++ "() { return 42 }"
  in not isValid ==> 
    case parseTypus code of
      Left _ -> property True  -- Parse failure is acceptable
      Right parsed -> 
        case validateSyntax parsed of
          ValidationResult [] -> property False  -- Should not validate
          ValidationResult _ -> property True   -- Should fail validation

-- Property: Valid function parameters pass validation
prop_valid_parameters_pass :: [ValidIdentifier] -> Property
prop_valid_parameters_pass idents =
  let paramNames = map (\(ValidIdentifier name) -> name ++ ": int") idents
      paramStr = if null paramNames then "" else List.intercalate ", " paramNames
      code = "func test(" ++ paramStr ++ ") int { return 42 }"
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case validateSyntax parsed of
           ValidationResult [] -> property True
           ValidationResult errors -> counterexample ("Validation failed: " ++ show errors) $ property False

-- Property: Valid return types pass validation
prop_valid_return_type_passes :: ValidTypeName -> Property
prop_valid_return_type_passes (ValidTypeName typeName) =
  let code = "func test() " ++ typeName ++ " { return " ++ typeName ++ "{} }"
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case validateSyntax parsed of
           ValidationResult [] -> property True
           ValidationResult errors -> counterexample ("Validation failed: " ++ show errors) $ property False

-- Property: Syntax validation preserves error locations
prop_validation_preserves_error_locations :: String -> Property
prop_validation_preserves_error_locations code =
  case parseTypus code of
    Left _ -> property True  -- Parse failure is acceptable
    Right parsed -> 
      case validateSyntax parsed of
        ValidationResult [] -> property True
        ValidationResult errors -> 
          property $ all (\err -> seLocation err /= mkSourceSpan (mkSourcePos 0 0) (mkSourcePos 0 0)) errors

tests :: TestTree
tests = testGroup "Cabal Syntax Validator QuickCheck Tests"
  [ fastProperty "Valid function names pass validation" prop_valid_function_name_passes
  , fastProperty "Valid type names pass validation" prop_valid_type_name_passes
  , fastProperty "Invalid identifiers fail validation" prop_invalid_identifier_fails
  , fastProperty "Valid parameters pass validation" prop_valid_parameters_pass
  , fastProperty "Valid return types pass validation" prop_valid_return_type_passes
  , fastProperty "Validation preserves error locations" prop_validation_preserves_error_locations
  , testCase "Syntax validator handles complex constructs" $ do
      let source = unlines
            [ "//! ownership: on"
            , "//! dependent_types: on"
            , "package main"
            , ""
            , "func processData<T: Sized>(data: Vector<T>) Result<T, Error> {"
            , "    let size = data.size()"
            , "    if size == 0 {"
            , "        return Error{message: \"Empty data\"}"
            , "    }"
            , "    return Ok{value: data[0]}"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed: " ++ err
        Right parsed -> 
          case validateSyntax parsed of
            ValidationResult [] -> return ()
            ValidationResult errors -> assertFailure $ "Validation failed: " ++ show errors
  ]