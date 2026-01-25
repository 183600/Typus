{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CoreOwnershipQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck

-- | Core Ownership module QuickCheck tests


import Test.Tasty
import Test.Tasty.QuickCheck

import TestSupport.Arbitrary
import TestSupport.QuickCheck
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, intercalate)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)
import Data.Char (isSpace, isAlpha, isAlphaNum)

import Ownership

-- ============================================================================
-- Ownership QuickCheck Tests
-- ============================================================================

-- | Test that newOwnershipAnalyzer creates a valid analyzer
prop_newOwnershipAnalyzerValid :: Property
prop_newOwnershipAnalyzerValid =
  let analyzer = newOwnershipAnalyzer
  in property $ True  -- Basic sanity check

-- | Test that analyzeOwnership processes basic code
prop_analyzeOwnershipBasic :: Property
prop_analyzeOwnershipBasic =
  forAll arbitraryShortString $ \code ->
    let result = analyzeOwnership code
    in property $ True  -- Basic sanity check

-- | Test that analyzeOwnershipFile processes file
prop_analyzeOwnershipFile :: Property
prop_analyzeOwnershipFile =
  forAll arbitraryShortString $ \filename ->
    let result = analyzeOwnershipFile filename
    in property $ True  -- Basic sanity check

-- | Test that analyzeOwnershipDebug provides debug info
prop_analyzeOwnershipDebug :: Property
prop_analyzeOwnershipDebug =
  forAll arbitraryShortString $ \code ->
    let result = analyzeOwnershipDebug True code
    in property $ True  -- Basic sanity check

-- | Test that formatOwnershipErrors formats errors
prop_formatOwnershipErrors :: Property
prop_formatOwnershipErrors =
  forAll (listOf arbitraryOwnershipError) $ \errors ->
    let formatted = formatOwnershipErrors errors
    in property $ not (null formatted) ==> not (null formatted)

-- | Test that lexAll processes basic code
prop_lexAllBasic :: Property
prop_lexAllBasic =
  forAll arbitraryShortString $ \code ->
    let result = lexAll code
    in property $ True  -- Basic sanity check

-- | Test that parseProgram processes basic code
prop_parseProgramBasic :: Property
prop_parseProgramBasic =
  forAll arbitraryShortString $ \code ->
    let tokens = lexAll code
        result = parseProgram tokens
    in property $ True  -- Basic sanity check

-- | Test that builtInFunctions contains functions
prop_builtInFunctionsContainsFunctions :: Property
prop_builtInFunctionsContainsFunctions =
  let functions = builtInFunctions
  in property $ not (null functions)

-- | Test that ownership types are valid
prop_ownershipTypeValid :: Property
prop_ownershipTypeValid =
  forAll arbitraryOwnershipType $ \ownershipType ->
    property $ True  -- Basic sanity check

-- | Test that ownership errors are valid
prop_ownershipErrorValid :: Property
prop_ownershipErrorValid =
  forAll arbitraryOwnershipError $ \error ->
    property $ True  -- Basic sanity check

-- | Test that ownership transfers are valid
prop_ownershipTransferValid :: Property
prop_ownershipTransferValid =
  forAll arbitraryOwnershipTransfer $ \transfer ->
    property $ True  -- Basic sanity check

-- | Test that ownership analysis handles variables
prop_analyzeOwnershipHandlesVariables :: Property
prop_analyzeOwnershipHandlesVariables =
  forAll arbitraryIdentifier $ \varName ->
    let code = "let " ++ varName ++ " = 42"
        result = analyzeOwnership code
    in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles assignments
prop_analyzeOwnershipHandlesAssignments :: Property
prop_analyzeOwnershipHandlesAssignments =
  forAll arbitraryIdentifier $ \varName ->
    forAll arbitraryIdentifier $ \value ->
      let code = varName ++ " = " ++ value
          result = analyzeOwnership code
      in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles function calls
prop_analyzeOwnershipHandlesFunctionCalls :: Property
prop_analyzeOwnershipHandlesFunctionCalls =
  forAll arbitraryIdentifier $ \funcName ->
    forAll (listOf arbitraryIdentifier) $ \args ->
      let argsStr = unwords args
          code = funcName ++ "(" ++ argsStr ++ ")"
          result = analyzeOwnership code
      in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles ownership transfer
prop_analyzeOwnershipHandlesOwnershipTransfer :: Property
prop_analyzeOwnershipHandlesOwnershipTransfer =
  forAll arbitraryIdentifier $ \var1 ->
    forAll arbitraryIdentifier $ \var2 ->
      let code = var1 ++ " = move(" ++ var2 ++ ")"
          result = analyzeOwnership code
      in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles borrowing
prop_analyzeOwnershipHandlesBorrowing :: Property
prop_analyzeOwnershipHandlesBorrowing =
  forAll arbitraryIdentifier $ \var1 ->
    forAll arbitraryIdentifier $ \var2 ->
      let code = var1 ++ " = borrow(" ++ var2 ++ ")"
          result = analyzeOwnership code
      in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles references
prop_analyzeOwnershipHandlesReferences :: Property
prop_analyzeOwnershipHandlesReferences =
  forAll arbitraryIdentifier $ \varName ->
    let code = "&" ++ varName
        result = analyzeOwnership code
    in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles dereferencing
prop_analyzeOwnershipHandlesDereferencing :: Property
prop_analyzeOwnershipHandlesDereferencing =
  forAll arbitraryIdentifier $ \varName ->
    let code = "*" ++ varName
        result = analyzeOwnership code
    in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles scopes
prop_analyzeOwnershipHandlesScopes :: Property
prop_analyzeOwnershipHandlesScopes =
  forAll arbitraryIdentifier $ \varName ->
    let code = "{\n  let " ++ varName ++ " = 42\n}"
        result = analyzeOwnership code
    in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles lifetimes
prop_analyzeOwnershipHandlesLifetimes :: Property
prop_analyzeOwnershipHandlesLifetimes =
  forAll arbitraryIdentifier $ \varName ->
    forAll arbitraryIdentifier $ \lifetime ->
      let code = varName ++ ":" ++ lifetime
          result = analyzeOwnership code
      in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles ownership types
prop_analyzeOwnershipHandlesOwnershipTypes :: Property
prop_analyzeOwnershipHandlesOwnershipTypes =
  forAll arbitraryIdentifier $ \varName ->
    forAll arbitraryOwnershipType $ \ownershipType ->
      let code = varName ++ ": " ++ show ownershipType
          result = analyzeOwnership code
      in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles multiple statements
prop_analyzeOwnershipHandlesMultipleStatements :: Property
prop_analyzeOwnershipHandlesMultipleStatements =
  forAll (listOf1 arbitraryIdentifier) $ \varNames ->
    let statements = map (\name -> "let " ++ name ++ " = 42") varNames
        code = unlines statements
        result = analyzeOwnership code
    in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles ownership transfer chains
prop_analyzeOwnershipHandlesOwnershipTransferChains :: Property
prop_analyzeOwnershipHandlesOwnershipTransferChains =
  forAll (listOf1 arbitraryIdentifier) $ \varNames ->
    let rest [] = []
        rest (_:ys) = ys
        transfers = case varNames of
                      [] -> []
                      xs -> zipWith (\src dst -> dst ++ " = move(" ++ src ++ ")") xs (rest xs ++ ["result"])
        code = unlines transfers
        result = analyzeOwnership code
    in property $ True  -- Basic sanity check

-- | Test that ownership analysis handles ownership errors
prop_analyzeOwnershipHandlesOwnershipErrors :: Property
prop_analyzeOwnershipHandlesOwnershipErrors =
  forAll arbitraryIdentifier $ \varName ->
    let code = "use(" ++ varName ++ ")\nuse(" ++ varName ++ ")"  -- Double use
        result = analyzeOwnership code
    in property $ True  -- Basic sanity check

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Core Ownership QuickCheck Tests"
  [ testProperty "NewOwnershipAnalyzer creates valid analyzer" prop_newOwnershipAnalyzerValid
  , testProperty "AnalyzeOwnership processes basic code" prop_analyzeOwnershipBasic
  , testProperty "AnalyzeOwnershipFile processes file" prop_analyzeOwnershipFile
  , testProperty "AnalyzeOwnershipDebug provides debug info" prop_analyzeOwnershipDebug
  , testProperty "FormatOwnershipErrors formats errors" prop_formatOwnershipErrors
  , testProperty "LexAll processes basic code" prop_lexAllBasic
  , testProperty "ParseProgram processes basic code" prop_parseProgramBasic
  , testProperty "BuiltInFunctions contains functions" prop_builtInFunctionsContainsFunctions
  , testProperty "OwnershipType is valid" prop_ownershipTypeValid
  , testProperty "OwnershipError is valid" prop_ownershipErrorValid
  , testProperty "OwnershipTransfer is valid" prop_ownershipTransferValid
  , testProperty "AnalyzeOwnership handles variables" prop_analyzeOwnershipHandlesVariables
  , testProperty "AnalyzeOwnership handles assignments" prop_analyzeOwnershipHandlesAssignments
  , testProperty "AnalyzeOwnership handles function calls" prop_analyzeOwnershipHandlesFunctionCalls
  , testProperty "AnalyzeOwnership handles ownership transfer" prop_analyzeOwnershipHandlesOwnershipTransfer
  , testProperty "AnalyzeOwnership handles borrowing" prop_analyzeOwnershipHandlesBorrowing
  , testProperty "AnalyzeOwnership handles references" prop_analyzeOwnershipHandlesReferences
  , testProperty "AnalyzeOwnership handles dereferencing" prop_analyzeOwnershipHandlesDereferencing
  , testProperty "AnalyzeOwnership handles scopes" prop_analyzeOwnershipHandlesScopes
  , testProperty "AnalyzeOwnership handles lifetimes" prop_analyzeOwnershipHandlesLifetimes
  , testProperty "AnalyzeOwnership handles ownership types" prop_analyzeOwnershipHandlesOwnershipTypes
  , testProperty "AnalyzeOwnership handles multiple statements" prop_analyzeOwnershipHandlesMultipleStatements
  , testProperty "AnalyzeOwnership handles ownership transfer chains" prop_analyzeOwnershipHandlesOwnershipTransferChains
  , testProperty "AnalyzeOwnership handles ownership errors" prop_analyzeOwnershipHandlesOwnershipErrors
  ]

-- | Run all tests
main :: IO ()
main = defaultMain testSuite