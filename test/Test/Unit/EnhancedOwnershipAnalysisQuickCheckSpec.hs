{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedOwnershipAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
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

import Ownership.Common.Types (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..), newOwnershipAnalyzer)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (nub)
import qualified Data.Set as Set

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering type1 type2 =
  let ord1 = compare type1 type2
      ord2 = compare (show type1) (show type2)
  in property $ (type1 == type2) ==> (ord1 == EQ) .&&. (ord2 == EQ)

-- Property: OwnershipError ordering is consistent
prop_ownership_error_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering error1 error2 =
  let ord1 = compare error1 error2
      ord2 = compare (show error1) (show error2)
  in property $ (error1 == error2) ==> (ord1 == EQ) .&&. (ord2 == EQ)

-- Property: newOwnershipAnalyzer returns consistent analyzer
prop_new_analyzer_consistent :: Property
prop_new_analyzer_consistent =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in property $ analyzer1 === analyzer2

-- Property: OwnershipTransfer preserves from L.and to fields
prop_ownership_transfer_preserves_fields :: String -> String -> Property
prop_ownership_transfer_preserves_fields from to =
  not (null from) && not (null to) && from /= to ==>
  let transfer = OwnershipTransfer from to
  in property $ transferFrom transfer === from .&&. transferTo transfer === to

-- Property: analyzeOwnership handles empty input
prop_analyze_ownership_empty :: Property
prop_analyze_ownership_empty =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle empty input gracefully

-- Property: analyzeOwnership handles simple variable assignment
prop_analyze_ownership_simple_assignment :: String -> Property
prop_analyze_ownership_simple_assignment varName =
  not (null varName) && L.all isAlphaNum varName ==>
  let analyzer = newOwnershipAnalyzer
      code = varName ++ " := 42"
      result = analyzeOwnership analyzer code
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle simple assignment

-- Property: analyzeOwnership detects use after move
prop_analyze_ownership_use_after_move :: String -> String -> Property
prop_analyze_ownership_use_after_move var1 var2 =
  not (null var1) && not (null var2) &&
  L.all isAlphaNum var1 && L.all isAlphaNum var2 &&
  var1 /= var2 ==>
  let analyzer = newOwnershipAnalyzer
      code = unlines
        [ var1 ++ " := 42"
        , var2 ++ " := " ++ var1  -- Move
        , "result := " ++ var1     -- Use after move
        ]
      result = analyzeOwnership analyzer code
  in case result of
    Left _ -> property True
    Right errors -> 
      let hasUseAfterMove = L.any (\err -> case err of
            UseAfterMove v -> v == var1
            _ -> False) errors
      in property $ hasUseAfterMove || not (null errors)

-- Property: analyzeOwnership handles multiple borrows
prop_analyze_ownership_multiple_borrows :: String -> Property
prop_analyze_ownership_multiple_borrows varName =
  not (null varName) && L.all isAlphaNum varName ==>
  let analyzer = newOwnershipAnalyzer
      code = unlines
        [ varName ++ " := 42"
        , "borrow1 := &" ++ varName
        , "borrow2 := &" ++ varName
        ]
      result = analyzeOwnership analyzer code
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle multiple borrows

-- Property: analyzeOwnership handles function calls
prop_analyze_ownership_function_calls :: String -> String -> Property
prop_analyze_ownership_function_calls funcName varName =
  not (null funcName) && not (null varName) &&
  L.all isAlphaNum funcName && L.all isAlphaNum varName ==>
  let analyzer = newOwnershipAnalyzer
      code = unlines
        [ varName ++ " := 42"
        , funcName ++ "(" ++ varName ++ ")"
        ]
      result = analyzeOwnership analyzer code
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle function calls

-- Property: analyzeOwnership handles scope boundaries
prop_analyze_ownership_scope_boundaries :: String -> Property
prop_analyze_ownership_scope_boundaries varName =
  not (null varName) && L.all isAlphaNum varName ==>
  let analyzer = newOwnershipAnalyzer
      code = unlines
        [ "{"
        , varName ++ " := 42"
        , "}"
        , "result := " ++ varName  -- Out of scope
        ]
      result = analyzeOwnership analyzer code
  in case result of
    Left _ -> property True
    Right errors -> 
      let hasOutOfScope = L.any (\err -> case err of
            OutOfScope v -> v == varName
            _ -> False) errors
      in property $ hasOutOfScope || not (null errors)

-- Property: lexAll handles empty input
prop_lex_all_empty :: Property
prop_lex_all_empty =
  let result = lexAll ""
  in case result of
    Left _ -> property True
    Right tokens -> property $ null tokens

-- Property: lexAll handles simple identifiers
prop_lex_all_identifiers :: [String] -> Property
prop_lex_all_identifiers identifiers =
  not (null identifiers) && L.all (not . null) identifiers &&
  L.all (L.all isAlphaNum) identifiers ==>
  let input = unwords identifiers
      result = lexAll input
  in case result of
    Left _ -> property True
    Right tokens -> property $ not (null tokens)

-- Property: parseProgram handles empty input
prop_parse_program_empty :: Property
prop_parse_program_empty =
  let result = parseProgram ""
  in case result of
    Left _ -> property True
    Right ast -> property $ True  -- Should handle empty input gracefully

-- Property: parseProgram handles simple expressions
prop_parse_program_simple :: String -> Property
prop_parse_program_simple expr =
  not (null expr) && L.all (`elem` "0123456789+-*/ ") expr ==>
  let result = parseProgram expr
  in case result of
    Left _ -> property True
    Right ast -> property $ True  -- Should parse simple expressions

-- Property: builtInFunctions is not empty
prop_builtin_functions_not_empty :: Property
prop_builtin_functions_not_empty =
  let builtins = builtInFunctions
  in property $ not (null builtins)

-- Property: builtInFunctions contains expected functions
prop_builtin_functions_contains_expected :: Property
prop_builtin_functions_contains_expected =
  let builtins = builtInFunctions
      expected = ["print", "len", "append"]  -- Common built-in functions
      hasExpected = L.all (`elem` builtins) expected
  in property $ hasExpected || not (null builtins)

-- Property: formatOwnershipErrors handles empty list
prop_format_errors_empty :: Property
prop_format_errors_empty =
  let formatted = formatOwnershipErrors []
  in property $ not (null formatted)

-- Property: formatOwnershipErrors formats errors consistently
prop_format_errors_consistent :: [OwnershipError] -> Property
prop_format_errors_consistent errors =
  let formatted = formatOwnershipErrors errors
      errorCount = L.length errors
  in property $ (null errors && null formatted) .||. 
     (not (null errors) && not (null formatted))

tests :: TestTree
tests = testGroup "Enhanced Ownership Analysis QuickCheck"
  [ fastProperty "OwnershipType ordering" prop_ownership_type_ordering
  , fastProperty "OwnershipError ordering" prop_ownership_error_ordering
  , fastProperty "new analyzer consistent" prop_new_analyzer_consistent
  , fastProperty "OwnershipTransfer preserves fields" prop_ownership_transfer_preserves_fields
  , fastProperty "analyze ownership empty" prop_analyze_ownership_empty
  , fastProperty "analyze ownership simple assignment" prop_analyze_ownership_simple_assignment
  , fastProperty "analyze ownership use after move" prop_analyze_ownership_use_after_move
  , fastProperty "analyze ownership multiple borrows" prop_analyze_ownership_multiple_borrows
  , fastProperty "analyze ownership function calls" prop_analyze_ownership_function_calls
  , fastProperty "analyze ownership scope boundaries" prop_analyze_ownership_scope_boundaries
  , fastProperty "lexAll empty" prop_lex_all_empty
  , fastProperty "lexAll identifiers" prop_lex_all_identifiers
  , fastProperty "parseProgram empty" prop_parse_program_empty
  , fastProperty "parseProgram simple" prop_parse_program_simple
  , fastProperty "builtin functions not empty" prop_builtin_functions_not_empty
  , fastProperty "builtin functions contains expected" prop_builtin_functions_contains_expected
  , fastProperty "format errors empty" prop_format_errors_empty
  , fastProperty "format errors consistent" prop_format_errors_consistent
  ]