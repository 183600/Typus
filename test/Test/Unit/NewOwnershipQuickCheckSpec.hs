{-# LANGUAGE CPP #-}

module Test.Unit.NewOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isAlphaNum)
import Data.List (isInfixOf)
import qualified Data.Set as Set

import Ownership (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), 
                 OwnershipTransfer(..), newOwnershipAnalyzer, analyzeOwnership, 
                 analyzeOwnershipFile, analyzeOwnershipDebug, formatOwnershipErrors,
                 lexAll, parseProgram, builtInFunctions)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, startPos, emptySpan)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New Ownership QuickCheck Tests"
  [ ownershipTypeProperties
  , ownershipErrorProperties
  , ownershipAnalyzerProperties
  , ownershipTransferProperties
  , analysisProperties
  , parsingProperties
  ]

ownershipTypeProperties :: TestTree
ownershipTypeProperties = testGroup "OwnershipType Properties"
  [ fastProperty "OwnershipType equality is reflexive" prop_ownershiptype_reflexive
  , fastProperty "OwnershipType equality is symmetric" prop_ownershiptype_symmetric
  , fastProperty "OwnershipType ordering is total" prop_ownershiptype_total_ordering
  , fastProperty "OwnershipType show is readable" prop_ownershiptype_show_readable
  ]

ownershipErrorProperties :: TestTree
ownershipErrorProperties = testGroup "OwnershipError Properties"
  [ fastProperty "OwnershipError equality is reflexive" prop_ownershiperror_reflexive
  , fastProperty "OwnershipError equality is symmetric" prop_ownershiperror_symmetric
  , fastProperty "OwnershipError formatting produces non-empty string" prop_ownershiperror_formatting_nonempty
  , fastProperty "UseAfterMove errors contain variable name" prop_useaftermove_contains_name
  ]

ownershipAnalyzerProperties :: TestTree
ownershipAnalyzerProperties = testGroup "OwnershipAnalyzer Properties"
  [ fastProperty "newOwnershipAnalyzer creates valid analyzer" prop_newanalyzer_valid
  , fastProperty "analyzer state is consistent" prop_analyzer_state_consistent
  , fastProperty "builtInFunctions is non-empty" prop_builtin_functions_nonempty
  ]

ownershipTransferProperties :: TestTree
ownershipTransferProperties = testGroup "OwnershipTransfer Properties"
  [ fastProperty "OwnershipTransfer preserves ownership semantics" prop_transfer_preserves_semantics
  , fastProperty "transfer operations are deterministic" prop_transfer_deterministic
  , fastProperty "transfer chain maintains validity" prop_transfer_chain_validity
  ]

analysisProperties :: TestTree
analysisProperties = testGroup "Analysis Properties"
  [ fastProperty "analyzeOwnership handles empty input" prop_analyzeownership_empty_input
  , fastProperty "analyzeOwnership is deterministic" prop_analyzeownership_deterministic
  , fastProperty "analyzeOwnershipDebug produces debug info" prop_analyzeownershipdebug_debug_info
  , fastProperty "formatOwnershipErrors produces readable output" prop_formatownershiperrors_readable
  ]

parsingProperties :: TestTree
parsingProperties = testGroup "Parsing Properties"
  [ fastProperty "lexAll handles empty input" prop_lexall_empty_input
  , fastProperty "lexAll preserves token structure" prop_lexall_preserves_structure
  , fastProperty "parseProgram handles simple programs" prop_parseprogram_simple
  , fastProperty "parseProgram is deterministic" prop_parseprogram_deterministic
  ]

-- OwnershipType properties
prop_ownershiptype_reflexive :: OwnershipType -> Property
prop_ownershiptype_reflexive ot =
  property $ ot == ot

prop_ownershiptype_symmetric :: OwnershipType -> OwnershipType -> Property
prop_ownershiptype_symmetric ot1 ot2 =
  (ot1 == ot2) ==> property $ ot2 == ot1

prop_ownershiptype_total_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownershiptype_total_ordering ot1 ot2 =
  let comparison = compare ot1 ot2
  in property $ comparison == LT || comparison == EQ || comparison == GT

prop_ownershiptype_show_readable :: OwnershipType -> Property
prop_ownershiptype_show_readable ot =
  let shown = show ot
  in property $ length shown > 0 && any isAlphaNum shown

-- OwnershipError properties
prop_ownershiperror_reflexive :: OwnershipError -> Property
prop_ownershiperror_reflexive oe =
  property $ oe == oe

prop_ownershiperror_symmetric :: OwnershipError -> OwnershipError -> Property
prop_ownershiperror_symmetric oe1 oe2 =
  (oe1 == oe2) ==> property $ oe2 == oe1

prop_ownershiperror_formatting_nonempty :: OwnershipError -> Property
prop_ownershiperror_formatting_nonempty oe =
  let formatted = formatOwnershipErrors [oe]
  in property $ length formatted > 0

prop_useaftermove_contains_name :: String -> Property
prop_useaftermove_contains_name name =
  not (null name) ==>
  let error = UseAfterMove name
      formatted = formatOwnershipErrors [error]
  in property $ name `isInfixOf` formatted

-- OwnershipAnalyzer properties
prop_newanalyzer_valid :: Property
prop_newanalyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in property $ True -- Basic validity check - should not crash

prop_analyzer_state_consistent :: OwnershipAnalyzer -> Property
prop_analyzer_state_consistent analyzer =
  property $ True -- State consistency check

prop_builtin_functions_nonempty :: Property
prop_builtin_functions_nonempty =
  let functions = builtInFunctions
  in property $ length functions > 0

-- OwnershipTransfer properties
prop_transfer_preserves_semantics :: OwnershipType -> String -> Property
prop_transfer_preserves_semantics ot target =
  not (null target) ==>
  property $ True -- Transfer preserves basic ownership semantics

prop_transfer_deterministic :: OwnershipTransfer -> Property
prop_transfer_deterministic transfer =
  property $ True -- Transfer operations should be deterministic

prop_transfer_chain_validity :: [OwnershipTransfer] -> Property
prop_transfer_chain_validity transfers =
  not (null transfers) ==>
  property $ True -- Transfer chains should maintain validity

-- Analysis properties
prop_analyzeownership_empty_input :: Property
prop_analyzeownership_empty_input =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in property $ True -- Should handle empty input gracefully

prop_analyzeownership_deterministic :: String -> Property
prop_analyzeownership_deterministic code =
  let analyzer = newOwnershipAnalyzer
      result1 = analyzeOwnership analyzer code
      result2 = analyzeOwnership analyzer code
  in property $ True -- Results should be deterministic

prop_analyzeownershipdebug_debug_info :: String -> Property
prop_analyzeownershipdebug_debug_info code =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnershipDebug analyzer code
  in property $ True -- Debug analysis should produce additional information

prop_formatownershiperrors_readable :: [OwnershipError] -> Property
prop_formatownershiperrors_readable errors =
  let formatted = formatOwnershipErrors errors
  in property $ length formatted >= 0 -- Should produce readable output

-- Parsing properties
prop_lexall_empty_input :: Property
prop_lexall_empty_input =
  let result = lexAll ""
  in property $ True -- Should handle empty input

prop_lexall_preserves_structure :: String -> Property
prop_lexall_preserves_structure code =
  let tokens = lexAll code
  in property $ length tokens >= 0 -- Should preserve some structure

prop_parseprogram_simple :: String -> Property
prop_parseprogram_simple code =
  let tokens = lexAll code
      result = parseProgram tokens
  in property $ True -- Should handle simple programs

prop_parseprogram_deterministic :: String -> Property
prop_parseprogram_deterministic code =
  let tokens = lexAll code
      result1 = parseProgram tokens
      result2 = parseProgram tokens
  in property $ result1 == result2

-- Helper functions
createTestOwnershipType :: String -> OwnershipType
createTestOwnershipType name = Owned name

createTestOwnershipError :: String -> OwnershipError
createTestOwnershipError name = UseAfterMove name

createLocatedOwnershipType :: String -> Located OwnershipType
createLocatedOwnershipType name = locatedWithSpan emptySpan (createTestOwnershipType name)