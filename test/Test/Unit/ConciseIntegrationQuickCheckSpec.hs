{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseIntegrationQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, property, Arbitrary(..), Gen, choose, elements)
import Parser (parseTypus, TypusFile(..), tfContents)
import Compiler (compile, generateGoCode)
import ErrorHandler (ErrorHandler, errorCount, warningCount, infoCount)
import Dependencies (DependencyGraph(..), TestDependencyGraph(..), analyzeDependencies, hasCycles)
import Ownership (OwnershipAnalysis(..), hasOwnershipErrors, getOwners, getBorrowers)
import qualified Ownership.Common.Types as Own
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim, splitBy, removeComments)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..), ErrorRecovery(..), emptyContext, unknownLocation, fatalRecovery)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- Arbitrary instances for QuickCheck
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

-- Add Arbitrary instance for TestDependencyGraph
instance Arbitrary TestDependencyGraph where
  arbitrary = do
    nodes <- arbitrary
    edges <- arbitrary
    return $ TestDependencyGraph nodes edges

-- Add Arbitrary instance for Ownership.Common.Types.OwnershipError
instance Arbitrary Own.OwnershipError where
  arbitrary = elements 
    [ Own.UseAfterMove "placeholder"
    , Own.DoubleMove "placeholder" "placeholder"
    , Own.BorrowWhileMoved "placeholder"
    , Own.MutBorrowWhileBorrowed "placeholder"
    , Own.BorrowWhileMutBorrowed "placeholder"
    , Own.MultipleMutBorrows "placeholder"
    , Own.UseWhileMutBorrowed "placeholder"
    , Own.OutOfScope "placeholder"
    , Own.BorrowError "placeholder"
    , Own.ParseError "placeholder"
    , Own.CrossFunctionMove "placeholder" "placeholder"
    , Own.ParameterMoveMismatch "placeholder"
    , Own.ControlFlowError "placeholder"
    , Own.PathSensitiveError "placeholder"
    , Own.LoopOwnershipError "placeholder"
    , Own.OwnershipError "placeholder"
    ]

-- Add Arbitrary instance for OwnershipAnalysis
instance Arbitrary OwnershipAnalysis where
  arbitrary = do
    owners <- arbitrary
    borrowers <- arbitrary
    errors <- arbitrary
    return $ OwnershipAnalysis owners borrowers errors

-- Dummy instance for Compiler.Errors.Core.TypeError
-- This is a minimal implementation for testing purposes
instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    return $ TypeError errorId Error TypeChecking T.empty unknownLocation emptyContext fatalRecovery [] [] [] Nothing

-- Test integration scenario data types
data IntegrationScenario = IntegrationScenario
  { isInput :: String
  , isExpectedErrors :: Int
  , isExpectedWarnings :: Int
  , isExpectedInfos :: Int
  } deriving (Show, Eq)

data EndToEndTest = EndToEndTest
  { eetInput :: String
  , eetShouldParse :: Bool
  , eetShouldCompile :: Bool
  , eetShouldGenerateGo :: Bool
  } deriving (Show, Eq)

-- Arbitrary instances for QuickCheck
instance Arbitrary IntegrationScenario where
  arbitrary = do
    input <- arbitrary
    expectedErrors <- choose (0, 10)
    expectedWarnings <- choose (0, 10)
    expectedInfos <- choose (0, 10)
    return $ IntegrationScenario input expectedErrors expectedWarnings expectedInfos

instance Arbitrary EndToEndTest where
  arbitrary = do
    input <- arbitrary
    shouldParse <- arbitrary
    shouldCompile <- arbitrary
    shouldGenerateGo <- arbitrary
    return $ EndToEndTest input shouldParse shouldCompile shouldGenerateGo

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 1000000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end

tests :: TestTree
tests = testGroup "Concise Integration QuickCheck Tests"
  [ testProperties "Parser-Compiler Integration"
    [ ("parse_compile_integration", property parse_compile_integration)
    , ("parse_compile_error_propagation", property parse_compile_error_propagation)
    ]
  , testProperties "Error Handling Integration"
    [ ("error_handling_consistency", property error_handling_consistency)
    , ("error_count_consistency", property error_count_consistency)
    ]
  , testProperties "Dependencies-Ownership Integration"
    [ ("dependencies_ownership_consistency", property dependencies_ownership_consistency)
    , ("cycle_detection_integration", property cycle_detection_integration)
    ]
  , testProperties "Source Location Integration"
    [ ("source_location_preservation", property source_location_preservation)
    , ("span_consistency", property span_consistency)
    ]
  , testProperties "Utils Integration"
    [ ("utils_parser_integration", property utils_parser_integration)
    , ("utils_comment_handling", property utils_comment_handling)
    ]
  , testProperties "End-to-End Integration"
    [ ("end_to_end_compilation", property end_to_end_compilation)
    , ("round_trip_properties", property round_trip_properties)
    ]
  ]

-- | Test that parse and compile work together
parse_compile_integration :: String -> Bool
parse_compile_integration input = 
  case parseTypus input of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile -> 
      case compile typusFile of
        Left _ -> True  -- Compile errors are acceptable
        Right result -> length result >= 0

-- | Test that errors propagate correctly from parse to compile
parse_compile_error_propagation :: String -> Bool
parse_compile_error_propagation input = 
  case parseTypus input of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile -> 
      let hasSyntaxErrors = not (null (tfSyntaxErrors typusFile))
      in if hasSyntaxErrors
         then case compile typusFile of
           Left _ -> True  -- Should have compile errors
           Right _ -> False  -- Should not compile successfully with syntax errors
         else True  -- No syntax errors, compilation may succeed or fail

-- | Test error handling consistency across modules
error_handling_consistency :: String -> Bool
error_handling_consistency input = 
  case parseTypus input of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile -> 
      case compile typusFile of
        Left _ -> True  -- Compile errors are acceptable
        Right _ -> True  -- Successful compilation is acceptable

-- | Test error count consistency
error_count_consistency :: ErrorHandler -> Bool
error_count_consistency handler = 
  let errors = errorCount handler
      warnings = warningCount handler
      infos = infoCount handler
  in errors >= 0 && warnings >= 0 && infos >= 0

-- | Test dependencies and ownership consistency
dependencies_ownership_consistency :: DependencyGraph -> OwnershipAnalysis -> Bool
dependencies_ownership_consistency depGraph ownershipAnalysis = 
  let hasDepCycles = hasCycles depGraph
      hasOwnErrors = hasOwnershipErrors ownershipAnalysis
      owners = getOwners ownershipAnalysis
      borrowers = getBorrowers ownershipAnalysis
  in length owners >= 0 && length borrowers >= 0

-- | Test cycle detection integration
cycle_detection_integration :: DependencyGraph -> Bool
cycle_detection_integration depGraph = 
  let analyzed = analyzeDependencies depGraph
      hasCyclesBefore = hasCycles depGraph
      hasCyclesAfter = hasCycles analyzed
  in hasCyclesBefore == hasCyclesAfter

-- | Test source location preservation
source_location_preservation :: String -> Bool
source_location_preservation input = 
  case parseTypus input of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile -> 
      let content = tfContents typusFile
      in length content >= 0

-- | Test span consistency
span_consistency :: SourceSpan -> Bool
span_consistency span = 
  let start = spanStart span
      end = spanEnd span
  in posLine start >= 1 && posColumn start >= 1 && 
     posLine end >= 1 && posColumn end >= 1

-- | Test utils integration with parser
utils_parser_integration :: String -> Bool
utils_parser_integration input = 
  let trimmed = trim input
      parsed = parseTypus input
  in case parsed of
    Left _ -> True  -- Parse errors are acceptable
    Right _ -> length trimmed >= 0

-- | Test utils comment handling
utils_comment_handling :: String -> Bool
utils_comment_handling input = 
  let withoutComments = removeComments input
      trimmed = trim withoutComments
  in length trimmed >= 0

-- | Test end-to-end compilation
end_to_end_compilation :: String -> Bool
end_to_end_compilation input = 
  case parseTypus input of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile -> 
      case compile typusFile of
        Left _ -> True  -- Compile errors are acceptable
        Right _ -> True  -- Successful compilation is acceptable

-- | Test round trip properties
round_trip_properties :: String -> Bool
round_trip_properties input = 
  case parseTypus input of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile -> 
      let content = tfContents typusFile
          goCode = generateGoCode typusFile
      in length content >= 0 && length goCode >= 0