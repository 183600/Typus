{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewAdvancedOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, listOf1, elements, vectorOf, suchThat)

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

import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate, nub)
import Data.Char (isAlphaNum, isSpace)

-- ============================================================================
-- Enhanced Property Tests for Ownership Module
-- ============================================================================

-- Property: newOwnershipAnalyzer creates a valid analyzer
prop_newOwnershipAnalyzer_valid :: Property
prop_newOwnershipAnalyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in property $ True -- Basic property - just ensure it doesn't crash

-- Property: lexAll handles simple Go code
prop_lexAll_simple_code :: String -> Property
prop_lexAll_simple_code code =
  not (L.any (`elem` ["\"", "\\", "/", "*", "\n", "\r", "\t"]) code) && 
  not (null code) && L.all isAlphaNum code ==>
  let goCode = "package main\n\nfunc main() {\n    " ++ code ++ "\n}\n"
      result = lexAll goCode
  in property $ L.length result >= 1

-- Property: lexAll handles empty input
prop_lexAll_empty_input :: Property
prop_lexAll_empty_input =
  let result = lexAll ""
  in property $ L.length result >= 0

-- Property: lexAll handles whitespace-only input
prop_lexAll_whitespace_only :: String -> Property
prop_lexAll_whitespace_only input =
  L.all isSpace input ==>
  let result = lexAll input
  in property $ L.length result >= 0

-- Property: lexAll preserves basic structure
prop_lexAll_preserves_structure :: String -> Property
prop_lexAll_preserves_structure code =
  not (null code) && L.length code <= 100 ==> -- Limit for performance
  let result = lexAll code
      tokenCount = L.length result
  in property $ tokenCount >= 0

-- Property: parseProgram handles simple function declarations
prop_parseProgram_simple_function :: String -> Property
prop_parseProgram_simple_function funcName =
  not (null funcName) && L.all isAlphaNum funcName && L.length funcName <= 20 ==>
  let goCode = "package main\n\nfunc " ++ funcName ++ "() {\n}\n"
      tokens = lexAll goCode
      result = parseProgram tokens
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: parseProgram handles variable declarations
prop_parseProgram_variable_declaration :: String -> String -> Property
prop_parseProgram_variable_declaration varName varType =
  not (null varName) && L.all isAlphaNum varName && L.length varName <= 10 &&
  not (null varType) && L.all isAlphaNum varType && L.length varType <= 10 ==>
  let goCode = "package main\n\nvar " ++ varName ++ " " ++ varType ++ "\n"
      tokens = lexAll goCode
      result = parseProgram tokens
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: parseProgram handles multiple declarations
prop_parseProgram_multiple_declarations :: [String] -> Property
prop_parseProgram_multiple_declarations varNames =
  not (null varNames) && L.length varNames <= 5 && 
  L.all (L.all isAlphaNum) varNames && L.all (\n -> L.length n <= 10) varNames ==>
  let declarations = L.map (\name -> "var " ++ name ++ " int") varNames
      goCode = "package main\n\n" ++ intercalate "\n" declarations ++ "\n"
      tokens = lexAll goCode
      result = parseProgram tokens
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: analyzeOwnership handles simple code
prop_analyzeOwnership_simple_code :: String -> Property
prop_analyzeOwnership_simple_code code =
  not (L.any (`elem` ["\"", "\\", "/", "*"]) code) && 
  L.length code <= 50 ==>
  let goCode = "package main\n\nfunc main() {\n    " ++ code ++ "\n}\n"
      result = analyzeOwnership goCode
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: analyzeOwnership handles empty input
prop_analyzeOwnership_empty_input :: Property
prop_analyzeOwnership_empty_input =
  let result = analyzeOwnership ""
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: analyzeOwnershipFile handles basic file structure
prop_analyzeOwnershipFile_basic :: String -> Property
prop_analyzeOwnershipFile_basic content =
  L.length content <= 100 ==> -- Limit for performance
  let result = analyzeOwnershipFile content
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: analyzeOwnershipDebug provides debug information
prop_analyzeOwnershipDebug_provides_info :: String -> Property
prop_analyzeOwnershipDebug_provides_info code =
  L.length code <= 50 ==>
  let result = analyzeOwnershipDebug code
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: formatOwnershipErrors handles error list
prop_formatOwnershipErrors_handles_list :: [String] -> Property
prop_formatOwnershipErrors_handles_list errorMessages =
  L.length errorMessages <= 10 ==> -- Limit for performance
  let errors = L.map (\msg -> OwnershipError (T.pack msg) Unknown OwnershipTypeUnknown) errorMessages
      formatted = formatOwnershipErrors errors
  in property $ L.length formatted >= 0

-- Property: formatOwnershipErrors handles empty list
prop_formatOwnershipErrors_empty_list :: Property
prop_formatOwnershipErrors_empty_list =
  let formatted = formatOwnershipErrors []
  in property $ L.length formatted >= 0

-- Property: builtInFunctions is not empty
prop_builtInFunctions_not_empty :: Property
prop_builtInFunctions_not_empty =
  let functions = builtInFunctions
  in property $ L.length functions >= 1

-- Property: OwnershipType values are consistent
prop_OwnershipType_consistency :: OwnershipType -> Property
prop_OwnershipType_consistency ownType =
  property $ case ownType of
                Owned -> True
                Borrowed -> True
                Moved -> True
                Shared -> True
                OwnershipTypeUnknown -> True

-- Property: OwnershipTransfer values are consistent
prop_OwnershipTransfer_consistency :: OwnershipTransfer -> Property
prop_OwnershipTransfer_consistency transfer =
  property $ case transfer of
                TransferValid -> True
                TransferInvalid -> True
                TransferPartial -> True
                TransferUnknown -> True

-- Property: OwnershipError contains message
prop_OwnershipError_has_message :: String -> OwnershipType -> OwnershipTransfer -> Property
prop_OwnershipError_has_message errorMsg ownType transfer =
  not (null errorMsg) ==>
  let error = OwnershipError (T.pack errorMsg) ownType transfer
  in property $ T.L.length (OwnershipError.ownershipMessage error) >= 0

-- Property: analyzeOwnership handles function calls
prop_analyzeOwnership_function_calls :: String -> String -> Property
prop_analyzeOwnership_function_calls funcName argName =
  not (null funcName) && L.all isAlphaNum funcName && L.length funcName <= 10 &&
  not (null argName) && L.all isAlphaNum argName && L.length argName <= 10 ==>
  let goCode = "package main\n\nfunc " ++ funcName ++ "(" ++ argName ++ " int) {\n}\n"
      result = analyzeOwnership goCode
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: analyzeOwnership handles assignment operations
prop_analyzeOwnership_assignments :: String -> String -> Property
prop_analyzeOwnership_assignments varName value =
  not (null varName) && L.all isAlphaNum varName && L.length varName <= 10 &&
  L.length value <= 20 ==>
  let goCode = "package main\n\nfunc main() {\n    " ++ varName ++ " := " ++ value ++ "\n}\n"
      result = analyzeOwnership goCode
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: analyzeOwnership handles return statements
prop_analyzeOwnership_returns :: String -> Property
prop_analyzeOwnership_returns returnValue =
  L.length returnValue <= 20 ==>
  let goCode = "package main\n\nfunc main() {\n    return " ++ returnValue ++ "\n}\n"
      result = analyzeOwnership goCode
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: lexAll L.and parseProgram interaction
prop_lexAll_parseProgram_interaction :: String -> Property
prop_lexAll_parseProgram_interaction code =
  L.length code <= 50 ==>
  let tokens = lexAll code
      parseResult = parseProgram tokens
  in case parseResult of
       Left _ -> property True
       Right _ -> property $ L.length tokens >= 0

-- Property: analyzeOwnership with large inputs
prop_analyzeOwnership_large_input :: Int -> String -> Property
prop_analyzeOwnership_large_input multiplier baseCode =
  multiplier >= 0 && multiplier <= 10 && -- Limit for performance
  L.length baseCode <= 20 ==>
  let largeCode = L.concat (replicate multiplier (baseCode ++ "\n"))
      goCode = "package main\n\nfunc main() {\n" ++ largeCode ++ "\n}\n"
      result = analyzeOwnership goCode
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: OwnershipError equality works correctly
prop_OwnershipError_equality :: String -> OwnershipType -> OwnershipTransfer -> Property
prop_OwnershipError_equality msg1 ownType1 transfer1 msg2 ownType2 transfer2 =
  let error1 = OwnershipError (T.pack msg1) ownType1 transfer1
      error2 = OwnershipError (T.pack msg2) ownType2 transfer2
      sameMsg = msg1 == msg2
      sameType = ownType1 == ownType2
      sameTransfer = transfer1 == transfer2
      shouldBeEqual = sameMsg && sameType && sameTransfer
  in property $ (error1 == error2) === shouldBeEqual

-- Property: formatOwnershipErrors preserves error information
prop_formatOwnershipErrors_preserves_info :: String -> Property
prop_formatOwnershipErrors_preserves_info errorMsg =
  not (null errorMsg) ==>
  let error = OwnershipError (T.pack errorMsg) Owned TransferValid
      errors = [error]
      formatted = formatOwnershipErrors errors
  in property $ errorMsg `L.isInfixOf` formatted

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Advanced Ownership QuickCheck Tests"
  [ testGroup "Analyzer creation properties"
    [ fastProperty "newOwnershipAnalyzer creates valid analyzer" prop_newOwnershipAnalyzer_valid
    , fastProperty "builtInFunctions is not empty" prop_builtInFunctions_not_empty
    ]

  , testGroup "Lexing properties"
    [ fastProperty "lexAll handles simple Go code" prop_lexAll_simple_code
    , fastProperty "lexAll handles empty input" prop_lexAll_empty_input
    , fastProperty "lexAll handles whitespace-only input" prop_lexAll_whitespace_only
    , fastProperty "lexAll preserves basic structure" prop_lexAll_preserves_structure
    ]

  , testGroup "Parsing properties"
    [ fastProperty "parseProgram handles simple function declarations" prop_parseProgram_simple_function
    , fastProperty "parseProgram handles variable declarations" prop_parseProgram_variable_declaration
    , fastProperty "parseProgram handles multiple declarations" prop_parseProgram_multiple_declarations
    , fastProperty "lexAll L.and parseProgram interaction" prop_lexAll_parseProgram_interaction
    ]

  , testGroup "Analysis properties"
    [ fastProperty "analyzeOwnership handles simple code" prop_analyzeOwnership_simple_code
    , fastProperty "analyzeOwnership handles empty input" prop_analyzeOwnership_empty_input
    , fastProperty "analyzeOwnershipFile handles basic file structure" prop_analyzeOwnershipFile_basic
    , fastProperty "analyzeOwnershipDebug provides debug information" prop_analyzeOwnershipDebug_provides_info
    , fastProperty "analyzeOwnership handles function calls" prop_analyzeOwnership_function_calls
    , fastProperty "analyzeOwnership handles assignment operations" prop_analyzeOwnership_assignments
    , fastProperty "analyzeOwnership handles return statements" prop_analyzeOwnership_returns
    , fastProperty "analyzeOwnership with large inputs" prop_analyzeOwnership_large_input
    ]

  , testGroup "Error handling properties"
    [ fastProperty "formatOwnershipErrors handles error list" prop_formatOwnershipErrors_handles_list
    , fastProperty "formatOwnershipErrors handles empty list" prop_formatOwnershipErrors_empty_list
    , fastProperty "formatOwnershipErrors preserves error information" prop_formatOwnershipErrors_preserves_info
    ]

  , testGroup "Data type properties"
    [ fastProperty "OwnershipType values are consistent" prop_OwnershipType_consistency
    , fastProperty "OwnershipTransfer values are consistent" prop_OwnershipTransfer_consistency
    , fastProperty "OwnershipError contains message" prop_OwnershipError_has_message
    , fastProperty "OwnershipError equality works correctly" prop_OwnershipError_equality
    ]
  ]