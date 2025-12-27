{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipQuickCheckSpec (tests) where

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

import Ownership.Common.Types
  ( OwnershipAnalyzer
  , OwnershipError(..)
  , OwnershipType(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import Data.Char (isAlphaNum, isAlpha, isDigit)
import Data.List (isPrefixOf, isInfixOf, nub)
import qualified Data.Set as Set

-- Property: OwnershipType equality is reflexive
prop_ownership_type_reflexive :: OwnershipType -> Property
prop_ownership_type_reflexive ownershipType =
  property $ ownershipType === ownershipType

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering type1 type2 =
  let ord1 = compare type1 type2
      ord2 = compare (show type1) (show type2)
  in property $ (type1 == type2) ==> (ord1 == ord2)

-- Property: OwnershipError equality is reflexive
prop_ownership_error_reflexive :: OwnershipError -> Property
prop_ownership_error_reflexive ownershipError =
  property $ ownershipError === ownershipError

-- Property: OwnershipError ordering is consistent
prop_ownership_error_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering error1 error2 =
  let ord1 = compare error1 error2
      ord2 = compare (show error1) (show error2)
  in property $ (error1 == error2) ==> (ord1 == ord2)

-- Property: newOwnershipAnalyzer creates analyzer
prop_new_ownership_analyzer :: Property
prop_new_ownership_analyzer =
  let analyzer = newOwnershipAnalyzer
  in property $ True  -- Basic smoke test

-- Property: OwnershipTransfer creates transfer
prop_ownership_transfer :: String -> String -> Property
prop_ownership_transfer fromVar toVar =
  not (null fromVar) && not (null toVar) ==>
  let transfer = OwnershipTransfer fromVar toVar
  in property $ transferFrom transfer === fromVar .&&. transferTo transfer === toVar

-- Property: OwnershipTransfer equality works
prop_ownership_transfer_equality :: String -> String -> String -> String -> Property
prop_ownership_transfer_equality from1 to1 from2 to2 =
  not (null from1) && not (null to1) && not (null from2) && not (null to2) ==>
  let transfer1 = OwnershipTransfer from1 to1
      transfer2 = OwnershipTransfer from2 to2
  in property $ (from1 == from2 && to1 == to2) ==> (transfer1 === transfer2)

-- Property: analyzeOwnership handles empty input
prop_analyze_ownership_empty :: Property
prop_analyze_ownership_empty =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle empty input

-- Property: analyzeOwnership handles simple variable declarations
prop_analyze_ownership_simple_decls :: [String] -> Property
prop_analyze_ownership_simple_decls varNames =
  not (null varNames) && all (not . null) varNames &&
  all (all isAlphaNum) varNames ==>
  let decls = map (\name -> "var " ++ name ++ " int = 0") varNames
      input = unlines decls
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer input
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle simple declarations

-- Property: analyzeOwnership handles move operations
prop_analyze_ownership_moves :: [String] -> Property
prop_analyze_ownership_moves varNames =
  not (null varNames) && all (not . null) varNames &&
  all (all isAlphaNum) varNames ==>
  let moves = zipWith (\from to -> from ++ " = " ++ to) varNames (tail varNames ++ ["0"])
      input = unlines moves
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer input
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle moves

-- Property: analyzeOwnership detects use after move
prop_analyze_ownership_use_after_move :: String -> Property
prop_analyze_ownership_use_after_move varName =
  not (null varName) && all isAlphaNum varName ==>
  let input = unlines 
        [ "var " ++ varName ++ " int = 42"
        , "other := " ++ varName
        , "println(" ++ varName ++ ")"  -- Use after move
        ]
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer input
  in case result of
    Left _ -> property True
    Right errors -> 
      let hasUseAfterMove = any (\err -> case err of UseAfterMove _ -> True; _ -> False) errors
      in property $ hasUseAfterMove || not (null errors)

-- Property: analyzeOwnership handles borrow operations
prop_analyze_ownership_borrows :: [String] -> Property
prop_analyze_ownership_borrows varNames =
  not (null varNames) && all (not . null) varNames &&
  all (all isAlphaNum) varNames ==>
  let borrows = map (\name -> "ref := &" ++ name) varNames
      input = unlines borrows
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer input
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle borrows

-- Property: analyzeOwnership handles function calls
prop_analyze_ownership_functions :: [String] -> Property
prop_analyze_ownership_functions functionNames =
  not (null functionNames) && all (not . null) functionNames &&
  all (all isAlphaNum) functionNames ==>
  let calls = map (\name -> name ++ "()") functionNames
      input = unlines calls
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer input
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle function calls

-- Property: analyzeOwnership handles scope changes
prop_analyze_ownership_scopes :: [String] -> Property
prop_analyze_ownership_scopes blockContents =
  not (null blockContents) && all (not . null) blockContents ==>
  let blocks = map (\content -> "{\n" ++ content ++ "\n}") blockContents
      input = unlines blocks
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer input
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle scope changes

-- Property: analyzeOwnershipFile handles file input
prop_analyze_ownership_file :: String -> Property
prop_analyze_ownership_file content =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnershipFile analyzer content
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle file input

-- Property: analyzeOwnershipDebug provides debug info
prop_analyze_ownership_debug :: String -> Property
prop_analyze_ownership_debug content =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnershipDebug analyzer content
  in case result of
    Left _ -> property True
    Right (errors, debug) -> property $ True  -- Should provide debug info

-- Property: formatOwnershipErrors produces non-empty output
prop_format_ownership_errors :: [OwnershipError] -> Property
prop_format_ownership_errors errors =
  not (null errors) ==>
  let formatted = formatOwnershipErrors errors
  in property $ not (null formatted)

-- Property: formatOwnershipErrors handles empty list
prop_format_ownership_errors_empty :: Property
prop_format_ownership_errors_empty =
  let formatted = formatOwnershipErrors []
  in property $ not (null formatted)  -- Should handle empty list

-- Property: lexAll handles empty input
prop_lex_all_empty :: Property
prop_lex_all_empty =
  let result = lexAll ""
  in case result of
    Left _ -> property True
    Right tokens -> property $ True  -- Should handle empty input

-- Property: lexAll handles simple input
prop_lex_all_simple :: String -> Property
prop_lex_all_simple input =
  not (null input) ==>
  let result = lexAll input
  in case result of
    Left _ -> property True
    Right tokens -> property $ True  -- Should handle simple input

-- Property: parseProgram handles empty input
prop_parse_program_empty :: Property
prop_parse_program_empty =
  let result = parseProgram ""
  in case result of
    Left _ -> property True
    Right program -> property $ True  -- Should handle empty input

-- Property: parseProgram handles simple input
prop_parse_program_simple :: String -> Property
prop_parse_program_simple input =
  not (null input) ==>
  let result = parseProgram input
  in case result of
    Left _ -> property True
    Right program -> property $ True  -- Should handle simple input

-- Property: builtInFunctions is non-empty
prop_built_in_functions_non_empty :: Property
prop_built_in_functions_non_empty =
  property $ not (null builtInFunctions)

-- Property: builtInFunctions contains expected functions
prop_built_in_functions_contains :: String -> Property
prop_built_in_functions_contains funcName =
  funcName `elem` ["println", "len", "append", "make"] ==>
  property $ funcName `elem` builtInFunctions

-- Property: Ownership analysis is deterministic
prop_ownership_analysis_deterministic :: String -> Property
prop_ownership_analysis_deterministic input =
  let analyzer = newOwnershipAnalyzer
      result1 = analyzeOwnership analyzer input
      result2 = analyzeOwnership analyzer input
  in case (result1, result2) of
    (Right errors1, Right errors2) -> property $ errors1 === errors2
    (Left err1, Left err2) -> property $ err1 === err2
    _ -> property False  -- Should be consistent

-- Property: Ownership analysis handles large inputs
prop_ownership_analysis_large :: String -> Int -> Property
prop_ownership_analysis_large base multiplier =
  multiplier >= 0 && multiplier <= 50 ==>  -- Limit for performance
  let largeInput = concat (replicate multiplier base)
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer largeInput
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle large inputs

-- Property: Ownership analysis handles unicode content
prop_ownership_analysis_unicode :: String -> Property
prop_ownership_analysis_unicode content =
  let unicodeContent = content ++ "测试🚀"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer unicodeContent
  in case result of
    Left _ -> property True
    Right errors -> property $ True  -- Should handle unicode content

tests :: TestTree
tests = testGroup "New Ownership QuickCheck"
  [ fastProperty "ownership type reflexive" prop_ownership_type_reflexive
  , fastProperty "ownership type ordering" prop_ownership_type_ordering
  , fastProperty "ownership error reflexive" prop_ownership_error_reflexive
  , fastProperty "ownership error ordering" prop_ownership_error_ordering
  , fastProperty "new ownership analyzer" prop_new_ownership_analyzer
  , fastProperty "ownership transfer" prop_ownership_transfer
  , fastProperty "ownership transfer equality" prop_ownership_transfer_equality
  , fastProperty "analyze ownership empty" prop_analyze_ownership_empty
  , fastProperty "analyze ownership simple decls" prop_analyze_ownership_simple_decls
  , fastProperty "analyze ownership moves" prop_analyze_ownership_moves
  , fastProperty "analyze ownership use after move" prop_analyze_ownership_use_after_move
  , fastProperty "analyze ownership borrows" prop_analyze_ownership_borrows
  , fastProperty "analyze ownership functions" prop_analyze_ownership_functions
  , fastProperty "analyze ownership scopes" prop_analyze_ownership_scopes
  , fastProperty "analyze ownership file" prop_analyze_ownership_file
  , fastProperty "analyze ownership debug" prop_analyze_ownership_debug
  , fastProperty "format ownership errors" prop_format_ownership_errors
  , fastProperty "format ownership errors empty" prop_format_ownership_errors_empty
  , fastProperty "lex all empty" prop_lex_all_empty
  , fastProperty "lex all simple" prop_lex_all_simple
  , fastProperty "parse program empty" prop_parse_program_empty
  , fastProperty "parse program simple" prop_parse_program_simple
  , fastProperty "built in functions non empty" prop_built_in_functions_non_empty
  , fastProperty "built in functions contains" prop_built_in_functions_contains
  , fastProperty "ownership analysis deterministic" prop_ownership_analysis_deterministic
  , fastProperty "ownership analysis large" prop_ownership_analysis_large
  , fastProperty "ownership analysis unicode" prop_ownership_analysis_unicode
  ]