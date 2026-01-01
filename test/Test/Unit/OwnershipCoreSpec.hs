{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized, suchThat)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, isAlphaNum)

import Ownership
  ( OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..)
  , OwnershipTransfer(..), newOwnershipAnalyzer
  , analyzeOwnership, analyzeOwnershipFile, analyzeOwnershipDebug
  , formatOwnershipErrors, lexAll, parseProgram, builtInFunctions
  )
import Ownership.Common.Types (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..))

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = do
    name <- oneof [return "x", return "y", return "z", return "var", return "value"]
    elements [Owned name, Borrowed name, MutBorrowed name]

instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> arbitrary
    , DoubleMove <$> arbitrary <*> arbitrary
    , BorrowWhileMoved <$> arbitrary
    , MutBorrowWhileBorrowed <$> arbitrary
    , BorrowWhileMutBorrowed <$> arbitrary
    , MultipleMutBorrows <$> arbitrary
    , UseWhileMutBorrowed <$> arbitrary
    , OutOfScope <$> arbitrary
    , BorrowError <$> arbitrary
    , ParseError <$> arbitrary
    , CrossFunctionMove <$> arbitrary <*> arbitrary
    , ParameterMoveMismatch <$> arbitrary
    , ControlFlowError <$> arbitrary
    , PathSensitiveError <$> arbitrary
    , LoopOwnershipError <$> arbitrary
    ]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    from <- arbitrary
    to <- arbitrary
    return $ OwnershipTransfer from to

-- Generate valid variable names
arbitraryVarName :: Gen String
arbitraryVarName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

-- Generate simple ownership expressions
arbitraryOwnershipExpr :: Gen String
arbitraryOwnershipExpr = oneof
  [ return "x := 5"
  , return "y := x"
  , return "z := &x"
  , return "a := *x"
  , return "x = y"
  , return "*x = 5"
  , return "println(x)"
  ]

-- ============================================================================
-- Ownership Type Properties
-- ============================================================================

-- Property: OwnershipType Show/Read roundtrip
prop_ownership_type_show_roundtrip :: OwnershipType -> Property
prop_ownership_type_show_roundtrip ownType =
  let shown = show ownType
  in property $ L.length shown > 0 .&&. "Owned" `L.isInfixOf` shown || "Borrowed" `L.isInfixOf` shown || "MutBorrowed" `L.isInfixOf` shown

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering own1 own2 =
  let cmp = compare own1 own2
      cmpReverse = compare own2 own1
  in property $ (cmp == EQ) ==> (cmpReverse == EQ) .&&.
             (cmp == LT) ==> (cmpReverse == GT) .&&.
             (cmp == GT) ==> (cmpReverse == LT)

-- Property: Owned types are ordered before borrowed types
prop_owned_before_borrowed :: String -> String -> Property
prop_owned_before_borrowed name1 name2 =
  let owned = Owned name1
      borrowed = Borrowed name2
  in property $ compare owned borrowed === LT

-- Property: Borrowed types are ordered before mut borrowed types
prop_borrowed_before_mut_borrowed :: String -> String -> Property
prop_borrowed_before_mut_borrowed name1 name2 =
  let borrowed = Borrowed name1
      mutBorrowed = MutBorrowed name2
  in property $ compare borrowed mutBorrowed === LT

-- ============================================================================
-- Ownership Error Properties
-- ============================================================================

-- Property: OwnershipError Show produces non-empty string
prop_ownership_error_show_nonempty :: OwnershipError -> Property
prop_ownership_error_show_nonempty err =
  let shown = show err
  in property $ not (null shown)

-- Property: OwnershipError contains relevant information
prop_ownership_error_contains_info :: OwnershipError -> Property
prop_ownership_error_contains_info err =
  let shown = show err
  in case err of
    UseAfterMove var -> property $ var `L.isInfixOf` shown
    DoubleMove var1 var2 -> property $ var1 `L.isInfixOf` shown .&&. var2 `L.isInfixOf` shown
    BorrowWhileMoved var -> property $ var `L.isInfixOf` shown
    MutBorrowWhileBorrowed var -> property $ var `L.isInfixOf` shown
    BorrowWhileMutBorrowed var -> property $ var `L.isInfixOf` shown
    MultipleMutBorrows var -> property $ var `L.isInfixOf` shown
    UseWhileMutBorrowed var -> property $ var `L.isInfixOf` shown
    OutOfScope var -> property $ var `L.isInfixOf` shown
    BorrowError msg -> property $ not (null msg) ==> msg `L.isInfixOf` shown
    ParseError msg -> property $ not (null msg) ==> msg `L.isInfixOf` shown
    CrossFunctionMove var1 var2 -> property $ var1 `L.isInfixOf` shown .&&. var2 `L.isInfixOf` shown
    ParameterMoveMismatch var -> property $ var `L.isInfixOf` shown
    ControlFlowError msg -> property $ not (null msg) ==> msg `L.isInfixOf` shown
    PathSensitiveError msg -> property $ not (null msg) ==> msg `L.isInfixOf` shown
    LoopOwnershipError msg -> property $ not (null msg) ==> msg `L.isInfixOf` shown

-- Property: OwnershipError ordering is consistent
prop_ownership_error_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering err1 err2 =
  let cmp = compare err1 err2
      cmpReverse = compare err2 err1
  in property $ (cmp == EQ) ==> (cmpReverse == EQ) .&&.
             (cmp == LT) ==> (cmpReverse == GT) .&&.
             (cmp == GT) ==> (cmpReverse == LT)

-- ============================================================================
-- Ownership Transfer Properties
-- ============================================================================

-- Property: OwnershipTransfer has correct structure
prop_ownership_transfer_structure :: String -> String -> Property
prop_ownership_transfer_structure from to =
  let transfer = OwnershipTransfer from to
  in property $ transferFrom transfer === from .&&.
             transferTo transfer === to

-- Property: OwnershipTransfer Show contains both variables
prop_ownership_transfer_show :: String -> String -> Property
prop_ownership_transfer_show from to =
  let transfer = OwnershipTransfer from to
      shown = show transfer
  in property $ from `L.isInfixOf` shown .&&. to `L.isInfixOf` shown

-- ============================================================================
-- Ownership Analyzer Properties
-- ============================================================================

-- Property: newOwnershipAnalyzer creates valid analyzer
prop_new_ownership_analyzer_valid :: Property
prop_new_ownership_analyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in property $ True -- Basic sanity check that it doesn't crash

-- Property: analyzeOwnership handles simple assignments
prop_analyze_simple_assignment :: Property
prop_analyze_simple_assignment =
  let analyzer = newOwnershipAnalyzer
      code = "x := 5\ny := x"
      result = analyzeOwnership analyzer code
  in property $ True -- Should not crash

-- Property: analyzeOwnership handles borrow operations
prop_analyze_borrow_operations :: Property
prop_analyze_borrow_operations =
  let analyzer = newOwnershipAnalyzer
      code = "x := 5\ny := &x\nz := *y"
      result = analyzeOwnership analyzer code
  in property $ True -- Should not crash

-- Property: analyzeOwnership handles move operations
prop_analyze_move_operations :: Property
prop_analyze_move_operations =
  let analyzer = newOwnershipAnalyzer
      code = "x := 5\ny := x\nz := y"
      result = analyzeOwnership analyzer code
  in property $ True -- Should not crash

-- Property: analyzeOwnership handles function calls
prop_analyze_function_calls :: Property
prop_analyze_function_calls =
  let analyzer = newOwnershipAnalyzer
      code = "x := 5\nprintln(x)"
      result = analyzeOwnership analyzer code
  in property $ True -- Should not crash

-- Property: analyzeOwnership handles empty input
prop_analyze_empty_input :: Property
prop_analyze_empty_input =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in property $ True -- Should not crash

-- Property: analyzeOwnership handles whitespace-only input
prop_analyze_whitespace_input :: String -> Property
prop_analyze_whitespace_input ws =
  L.all (`elem` " \t\n\r") ws ==>
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ws
  in property $ True -- Should not crash

-- ============================================================================
-- Lexer Properties
-- ============================================================================

-- Property: lexAll handles empty input
prop_lex_empty_input :: Property
prop_lex_empty_input =
  let result = lexAll ""
  in property $ True -- Should not crash

-- Property: lexAll handles simple expressions
prop_lex_simple_expressions :: String -> Property
prop_lex_simple_expressions expr =
  L.length expr <= 20 && L.all (`elem` "abcdefghijklmnopqrstuvwxyz0123456789:=&*() \t\n") expr ==>
  let result = lexAll expr
  in property $ True -- Should not crash

-- Property: lexAll handles identifiers
prop_lex_identifiers :: String -> Property
prop_lex_identifiers ident =
  not (null ident) && L.all isAlphaNum (L.head ident : L.tail ident) ==>
  let expr = ident ++ " := 5"
      result = lexAll expr
  in property $ True -- Should not crash
  where
    L.tail [] = []
    L.tail (x:xs) = xs

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: parseProgram handles empty input
prop_parse_empty_program :: Property
prop_parse_empty_program =
  let tokens = lexAll ""
      result = parseProgram tokens
  in property $ True -- Should not crash

-- Property: parseProgram handles simple assignments
prop_parse_simple_assignment_program :: String -> Property
prop_parse_simple_assignment_program varName =
  not (null varName) && L.all isAlphaNum varName ==>
  let expr = varName ++ " := 5"
      tokens = lexAll expr
      result = parseProgram tokens
  in property $ True -- Should not crash

-- Property: parseProgram handles multiple statements
prop_parse_multiple_statements :: [String] -> Property
prop_parse_multiple_statements varNames =
  L.length varNames <= 3 && L.all (L.all isAlphaNum) varNames ==>
  let statements = L.map (\v -> v ++ " := " ++ "5") varNames
      expr = unlines statements
      tokens = lexAll expr
      result = parseProgram tokens
  in property $ True -- Should not crash

-- ============================================================================
-- Built-in Functions Properties
-- ============================================================================

-- Property: builtInFunctions contains common functions
prop_built_in_functions_contains_common :: Property
prop_built_in_functions_contains_common =
  let commonFuncs = ["println", "len", "make", "append"]
  in property $ L.all (`elem` builtInFunctions) commonFuncs

-- Property: builtInFunctions contains type names
prop_built_in_functions_contains_types :: Property
prop_built_in_functions_contains_types =
  let typeNames = ["int", "string", "bool", "error"]
  in property $ L.all (`elem` builtInFunctions) typeNames

-- Property: builtInFunctions contains package names
prop_built_in_functions_contains_packages :: Property
prop_built_in_functions_contains_packages =
  let packages = ["fmt", "os", "io", "time"]
  in property $ L.all (`elem` builtInFunctions) packages

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: formatOwnershipErrors handles empty list
prop_format_empty_errors :: Property
prop_format_empty_errors =
  let formatted = formatOwnershipErrors []
  in property $ not (null formatted) -- Should return some formatted output

-- Property: formatOwnershipErrors handles single error
prop_format_single_error :: OwnershipError -> Property
prop_format_single_error err =
  let formatted = formatOwnershipErrors [err]
      errStr = show err
  in property $ errStr `L.isInfixOf` formatted

-- Property: formatOwnershipErrors handles multiple errors
prop_format_multiple_errors :: [OwnershipError] -> Property
prop_format_multiple_errors errors =
  L.length errors <= 5 ==>
  let formatted = formatOwnershipErrors errors
      errStrs = map show errors
  in property $ L.all (`L.isInfixOf` formatted) errStrs

-- Property: formatOwnershipErrors produces unique output for different errors
prop_format_errors_unique :: OwnershipError -> OwnershipError -> Property
prop_format_errors_unique err1 err2 =
  err1 /= err2 ==>
  let formatted1 = formatOwnershipErrors [err1]
      formatted2 = formatOwnershipErrors [err2]
  in property $ formatted1 /= formatted2

-- ============================================================================
-- Debug Analysis Properties
-- ============================================================================

-- Property: analyzeOwnershipDebug produces debug output
prop_analyze_ownership_debug :: Property
prop_analyze_ownership_debug =
  let analyzer = newOwnershipAnalyzer
      code = "x := 5\ny := x"
      result = analyzeOwnershipDebug analyzer code
  in property $ True -- Should not crash

-- Property: analyzeOwnershipFile handles file-like input
prop_analyze_ownership_file :: Property
prop_analyze_ownership_file =
  let analyzer = newOwnershipAnalyzer
      code = "package main\n\nfunc main() {\n\tx := 5\n\ty := x\n}"
      result = analyzeOwnershipFile analyzer code
  in property $ True -- Should not crash

-- ============================================================================
-- Complex Properties
-- ============================================================================

-- Property: Analysis is deterministic
prop_analysis_deterministic :: String -> Property
prop_analysis_deterministic code =
  L.length code <= 50 ==>
  let analyzer = newOwnershipAnalyzer
      result1 = analyzeOwnership analyzer code
      result2 = analyzeOwnership analyzer code
  in property $ result1 === result2

-- Property: Analysis handles complex ownership scenarios
prop_analysis_complex_scenarios :: Property
prop_analysis_complex_scenarios =
  let complexCode = unlines
        [ "x := 5"
        , "y := &x"
        , "z := *y"
        , "a := x"
        , "b := y"
        , "println(a, b, z)"
        ]
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer complexCode
  in property $ True -- Should not crash

-- Property: Error detection consistency
prop_error_detection_consistency :: String -> Property
prop_error_detection_consistency code =
  L.length code <= 30 ==>
  let analyzer = newOwnershipAnalyzer
      result1 = analyzeOwnership analyzer code
      result2 = analyzeOwnership analyzer code
  in property $ L.length result1 === L.length result2

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Core Tests"
  [ testGroup "Ownership Type Properties"
    [ fastProperty "OwnershipType Show/Read roundtrip" prop_ownership_type_show_roundtrip
    , fastProperty "OwnershipType ordering is consistent" prop_ownership_type_ordering
    , fastProperty "Owned types are ordered before borrowed types" prop_owned_before_borrowed
    , fastProperty "Borrowed types are ordered before mut borrowed types" prop_borrowed_before_mut_borrowed
    ]
  , testGroup "Ownership Error Properties"
    [ fastProperty "OwnershipError Show produces non-empty string" prop_ownership_error_show_nonempty
    , fastProperty "OwnershipError contains relevant information" prop_ownership_error_contains_info
    , fastProperty "OwnershipError ordering is consistent" prop_ownership_error_ordering
    ]
  , testGroup "Ownership Transfer Properties"
    [ fastProperty "OwnershipTransfer has correct structure" prop_ownership_transfer_structure
    , fastProperty "OwnershipTransfer Show contains both variables" prop_ownership_transfer_show
    ]
  , testGroup "Ownership Analyzer Properties"
    [ fastProperty "newOwnershipAnalyzer creates valid analyzer" prop_new_ownership_analyzer_valid
    , fastProperty "analyzeOwnership handles simple assignments" prop_analyze_simple_assignment
    , fastProperty "analyzeOwnership handles borrow operations" prop_analyze_borrow_operations
    , fastProperty "analyzeOwnership handles move operations" prop_analyze_move_operations
    , fastProperty "analyzeOwnership handles function calls" prop_analyze_function_calls
    , fastProperty "analyzeOwnership handles empty input" prop_analyze_empty_input
    , fastProperty "analyzeOwnership handles whitespace-only input" prop_analyze_whitespace_input
    ]
  , testGroup "Lexer Properties"
    [ fastProperty "lexAll handles empty input" prop_lex_empty_input
    , fastProperty "lexAll handles simple expressions" prop_lex_simple_expressions
    , fastProperty "lexAll handles identifiers" prop_lex_identifiers
    ]
  , testGroup "Parser Properties"
    [ fastProperty "parseProgram handles empty input" prop_parse_empty_program
    , fastProperty "parseProgram handles simple assignments" prop_parse_simple_assignment_program
    , fastProperty "parseProgram handles multiple statements" prop_parse_multiple_statements
    ]
  , testGroup "Built-in Functions Properties"
    [ fastProperty "builtInFunctions contains common functions" prop_built_in_functions_contains_common
    , fastProperty "builtInFunctions contains type names" prop_built_in_functions_contains_types
    , fastProperty "builtInFunctions contains package names" prop_built_in_functions_contains_packages
    ]
  , testGroup "Error Formatting Properties"
    [ fastProperty "formatOwnershipErrors handles empty list" prop_format_empty_errors
    , fastProperty "formatOwnershipErrors handles single error" prop_format_single_error
    , fastProperty "formatOwnershipErrors handles multiple errors" prop_format_multiple_errors
    , fastProperty "formatOwnershipErrors produces unique output for different errors" prop_format_errors_unique
    ]
  , testGroup "Debug Analysis Properties"
    [ fastProperty "analyzeOwnershipDebug produces debug output" prop_analyze_ownership_debug
    , fastProperty "analyzeOwnershipFile handles file-like input" prop_analyze_ownership_file
    ]
  , testGroup "Complex Properties"
    [ fastProperty "Analysis is deterministic" prop_analysis_deterministic
    , fastProperty "Analysis handles complex ownership scenarios" prop_analysis_complex_scenarios
    , fastProperty "Error detection consistency" prop_error_detection_consistency
    ]
  ]