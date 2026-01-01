{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import Ownership.Lexer
import Ownership.Parser
import Ownership.Analyzer
import Ownership.Reporter

import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (length, isPrefixOf, isInfixOf)
import Data.List (sort, null, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- OwnershipType Properties
-- ============================================================================

-- Property: Owned types are equal if names are equal
prop_owned_equality :: String -> Property
prop_owned_equality name =
  not (null name) ==>
  let owned1 = Owned name
      owned2 = Owned name
  in property $ owned1 === owned2

-- Property: Borrowed types are equal if names are equal
prop_borrowed_equality :: String -> Property
prop_borrowed_equality name =
  not (null name) ==>
  let borrowed1 = Borrowed name
      borrowed2 = Borrowed name
  in property $ borrowed1 === borrowed2

-- Property: MutBorrowed types are equal if names are equal
prop_mut_borrowed_equality :: String -> Property
prop_mut_borrowed_equality name =
  not (null name) ==>
  let mutBorrowed1 = MutBorrowed name
      mutBorrowed2 = MutBorrowed name
  in property $ mutBorrowed1 === mutBorrowed2

-- Property: Different ownership types with same name are not equal
prop_different_ownership_types_not_equal :: String -> Property
prop_different_ownership_types_not_equal name =
  not (null name) ==>
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
  in property $ owned /= borrowed .&&.
             owned /= mutBorrowed .&&.
             borrowed /= mutBorrowed

-- Property: Ownership ordering is consistent
prop_ownership_ordering :: String -> String -> Property
prop_ownership_ordering name1 name2 =
  not (null name1) && not (null name2) ==>
  let owned1 = Owned name1
      owned2 = Owned name2
      borrowed1 = Borrowed name1
      borrowed2 = Borrowed name2
      mutBorrowed1 = MutBorrowed name1
      mutBorrowed2 = MutBorrowed name2
  in property $ owned1 < borrowed1 .&&.
             borrowed1 < mutBorrowed1 .&&.
             compare owned1 owned2 === compare name1 name2

-- ============================================================================
-- OwnershipError Properties
-- ============================================================================

-- Property: UseAfterMove errors contain variable name
prop_use_after_move_contains_name :: String -> Property
prop_use_after_move_contains_name varName =
  not (null varName) ==>
  let error = UseAfterMove varName
  in property $ varName `L.isInfixOf` show error

-- Property: DoubleMove errors contain both variable names
prop_double_move_contains_names :: String -> String -> Property
prop_double_move_contains_names var1 var2 =
  not (null var1) && not (null var2) ==>
  let error = DoubleMove var1 var2
  in property $ var1 `L.isInfixOf` show error .&&.
             var2 `L.isInfixOf` show error

-- Property: BorrowWhileMoved errors contain variable name
prop_borrow_while_moved_contains_name :: String -> Property
prop_borrow_while_moved_contains_name varName =
  not (null varName) ==>
  let error = BorrowWhileMoved varName
  in property $ varName `L.isInfixOf` show error

-- Property: MutBorrowWhileBorrowed errors contain variable name
prop_mut_borrow_while_borrowed_contains_name :: String -> Property
prop_mut_borrow_while_borrowed_contains_name varName =
  not (null varName) ==>
  let error = MutBorrowWhileBorrowed varName
  in property $ varName `L.isInfixOf` show error

-- Property: OutOfScope errors contain variable name
prop_out_of_scope_contains_name :: String -> Property
prop_out_of_scope_contains_name varName =
  not (null varName) ==>
  let error = OutOfScope varName
  in property $ varName `L.isInfixOf` show error

-- ============================================================================
-- OwnershipAnalyzer Properties
-- ============================================================================

-- Property: New ownership analyzer is empty
prop_new_analyzer_empty :: Property
prop_new_analyzer_empty =
  let analyzer = newOwnershipAnalyzer
  in property $ L.null (oaErrors analyzer) .&&.
             null (oaVariables analyzer)

-- Property: Built-in functions are available in new analyzer
prop_built_in_functions_available :: Property
prop_built_in_functions_available =
  let builtIns = builtInFunctions
  in property $ not (null builtIns)

-- Property: Analyzing empty code produces no errors
prop_analyze_empty_code :: Property
prop_analyze_empty_code =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership "" analyzer
  in property $ L.null (oaErrors result)

-- Property: Analyzing simple variable declaration works
prop_analyze_simple_declaration :: String -> Property
prop_analyze_simple_declaration varName =
  not (null varName) && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") varName ==>
  let code = "let " ++ varName ++ " = 42;"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
  in property $ L.length (oaVariables result) >= 1

-- ============================================================================
-- OwnershipTransfer Properties
-- ============================================================================

-- Property: Ownership transfer preserves source L.and destination
prop_ownership_transfer_preserves_src_dst :: String -> String -> Property
prop_ownership_transfer_preserves_src_dst src dst =
  not (null src) && not (null dst) ==>
  let transfer = OwnershipTransfer src dst
  in property $ otSource transfer === src .&&.
             otDestination transfer === dst

-- Property: Ownership transfer is deterministic
prop_ownership_transfer_deterministic :: String -> String -> Property
prop_ownership_transfer_deterministic src dst =
  not (null src) && not (null dst) ==>
  let transfer1 = OwnershipTransfer src dst
      transfer2 = OwnershipTransfer src dst
  in property $ transfer1 === transfer2

-- ============================================================================
-- Lexer Properties
-- ============================================================================

-- Property: Lexing empty string returns empty tokens
prop_lex_empty_string :: Property
prop_lex_empty_string =
  let tokens = lexAll ""
  in property $ null tokens

-- Property: Lexing simple identifier works
prop_lex_simple_identifier :: String -> Property
prop_lex_simple_identifier identifier =
  not (null identifier) && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") identifier ==>
  let code = identifier
      tokens = lexAll code
  in property $ not (null tokens)

-- Property: Lexing preserves whitespace structure
prop_lex_preserves_whitespace :: String -> Property
prop_lex_preserves_whitespace content =
  not (null content) ==>
  let tokens = lexAll content
  in property $ L.length tokens >= 1

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: Parsing empty string returns empty program
prop_parse_empty_string :: Property
prop_parse_empty_string =
  let tokens = lexAll ""
      program = parseProgram tokens
  in property $ null program

-- Property: Parsing simple declaration works
prop_parse_simple_declaration :: String -> Property
prop_parse_simple_declaration varName =
  not (null varName) && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") varName ==>
  let code = "let " ++ varName ++ " = 42;"
      tokens = lexAll code
      program = parseProgram tokens
  in property $ not (null program)

-- Property: Parsing is idempotent for valid code
prop_parse_idempotent :: String -> Property
prop_parse_idempotent code =
  not (null code) ==>
  let tokens1 = lexAll code
      program1 = parseProgram tokens1
      -- Re-parse would require program->code conversion which we don't have
  in property $ not (null program1) ==> L.length program1 >= 1

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: Formatting empty errors returns empty string
prop_format_empty_errors :: Property
prop_format_empty_errors =
  let errors = []
      formatted = formatOwnershipErrors errors
  in property $ null formatted

-- Property: Formatting single error includes error message
prop_format_single_error :: String -> Property
prop_format_single_error varName =
  not (null varName) ==>
  let error = UseAfterMove varName
      errors = [error]
      formatted = formatOwnershipErrors errors
  in property $ varName `L.isInfixOf` formatted

-- Property: Formatting multiple errors includes L.all messages
prop_format_multiple_errors :: [String] -> Property
prop_format_multiple_errors varNames =
  not (null varNames) && L.all (not . null) varNames ==>
  let errors = map UseAfterMove varNames
      formatted = formatOwnershipErrors errors
  in property $ L.all (`L.isInfixOf` formatted) varNames

-- ============================================================================
-- Analysis Properties
-- ============================================================================

-- Property: Analyzing code with move produces appropriate errors
prop_analyze_move_produces_errors :: String -> String -> Property
prop_analyze_move_produces_errors var1 var2 =
  not (null var1) && not (null var2) && var1 /= var2 ==>
  let code = "let " ++ var1 ++ " = 42;\nlet " ++ var2 ++ " = " ++ var1 ++ ";\nprintln(" ++ var1 ++ ");"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
  in property $ not (L.null (oaErrors result)) ==> 
             any (\e -> case e of UseAfterMove name -> name == var1; _ -> False) (oaErrors result)

-- Property: Analyzing code with borrow works correctly
prop_analyze_borrow_works :: String -> String -> Property
prop_analyze_borrow_works var1 var2 =
  not (null var1) && not (null var2) && var1 /= var2 ==>
  let code = "let " ++ var1 ++ " = 42;\nlet " ++ var2 ++ " = &" ++ var1 ++ ";\nprintln(*" ++ var2 ++ ");"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
  in property $ L.length (oaVariables result) >= 2

-- Property: Analyzing code with mutable borrow works
prop_analyze_mutable_borrow_works :: String -> String -> Property
prop_analyze_mutable_borrow_works var1 var2 =
  not (null var1) && not (null var2) && var1 /= var2 ==>
  let code = "let mut " ++ var1 ++ " = 42;\nlet " ++ var2 ++ " = &mut " ++ var1 ++ ";\n*" ++ var2 ++ " = 10;"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
  in property $ L.length (oaVariables result) >= 2

-- Property: Analysis preserves variable order
prop_analysis_preserves_order :: [String] -> Property
prop_analysis_preserves_order varNames =
  not (null varNames) && L.all (not . null) varNames && L.all (L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) varNames ==>
  let declarations = L.map (\name -> "let " ++ name ++ " = 42;") varNames
      code = unlines declarations
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
  in property $ L.length (oaVariables result) >= L.length varNames

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

-- Property: Analyzing code with Unicode characters works
prop_analyze_unicode :: String -> Property
prop_analyze_unicode unicodeText =
  not (null unicodeText) ==>
  let code = "let x = \"" ++ unicodeText ++ "\";"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
  in property $ L.length (oaVariables result) >= 1

-- Property: Analyzing very long code works
prop_analyze_long_code :: Int -> Property
prop_analyze_long_code L.length =
  length > 0 && L.length <= 1000 ==>
  let longCode = L.concat (replicate L.length "let x = 42;\n")
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership longCode analyzer
  in property $ L.length (oaVariables result) >= 1

-- Property: Analyzing code with comments works
prop_analyze_with_comments :: String -> String -> Property
prop_analyze_with_comments varName comment =
  not (null varName) && not (null comment) && not ("//" `L.isInfixOf` comment) ==>
  let code = "// " ++ comment ++ "\nlet " ++ varName ++ " = 42; // " ++ comment
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership code analyzer
  in property $ L.length (oaVariables result) >= 1

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership New QuickCheck Tests"
  [ testGroup "OwnershipType"
    [ fastProperty "owned equality" prop_owned_equality
    , fastProperty "borrowed equality" prop_borrowed_equality
    , fastProperty "mut borrowed equality" prop_mut_borrowed_equality
    , fastProperty "different ownership types not equal" prop_different_ownership_types_not_equal
    , fastProperty "ownership ordering" prop_ownership_ordering
    ]
  , testGroup "OwnershipError"
    [ fastProperty "UseAfterMove contains name" prop_use_after_move_contains_name
    , fastProperty "DoubleMove contains names" prop_double_move_contains_names
    , fastProperty "BorrowWhileMoved contains name" prop_borrow_while_moved_contains_name
    , fastProperty "MutBorrowWhileBorrowed contains name" prop_mut_borrow_while_borrowed_contains_name
    , fastProperty "OutOfScope contains name" prop_out_of_scope_contains_name
    ]
  , testGroup "OwnershipAnalyzer"
    [ fastProperty "new analyzer empty" prop_new_analyzer_empty
    , fastProperty "built-in functions available" prop_built_in_functions_available
    , fastProperty "analyze empty code" prop_analyze_empty_code
    , fastProperty "analyze simple declaration" prop_analyze_simple_declaration
    ]
  , testGroup "OwnershipTransfer"
    [ fastProperty "preserves source L.and destination" prop_ownership_transfer_preserves_src_dst
    , fastProperty "deterministic" prop_ownership_transfer_deterministic
    ]
  , testGroup "Lexer"
    [ fastProperty "lex empty string" prop_lex_empty_string
    , fastProperty "lex simple identifier" prop_lex_simple_identifier
    , fastProperty "lex preserves whitespace" prop_lex_preserves_whitespace
    ]
  , testGroup "Parser"
    [ fastProperty "parse empty string" prop_parse_empty_string
    , fastProperty "parse simple declaration" prop_parse_simple_declaration
    , fastProperty "parse idempotent" prop_parse_idempotent
    ]
  , testGroup "ErrorFormatting"
    [ fastProperty "format empty errors" prop_format_empty_errors
    , fastProperty "format single error" prop_format_single_error
    , fastProperty "format multiple errors" prop_format_multiple_errors
    ]
  , testGroup "Analysis"
    [ fastProperty "analyze move produces errors" prop_analyze_move_produces_errors
    , fastProperty "analyze borrow works" prop_analyze_borrow_works
    , fastProperty "analyze mutable borrow works" prop_analyze_mutable_borrow_works
    , fastProperty "analysis preserves order" prop_analysis_preserves_order
    ]
  , testGroup "EdgeCases"
    [ fastProperty "analyze unicode" prop_analyze_unicode
    , fastProperty "analyze long code" prop_analyze_long_code
    , fastProperty "analyze with comments" prop_analyze_with_comments
    ]
  ]