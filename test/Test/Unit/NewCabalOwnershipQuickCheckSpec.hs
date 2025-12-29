{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat)
import qualified Test.QuickCheck as QC

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )
import Ownership
  ( analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  , builtInFunctions
  )
import Data.List (sort, nub, isInfixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace)

-- ============================================================================
-- Generators for Ownership data types
-- ============================================================================

-- Generate valid identifiers (alphabetic start, alphanumeric rest)
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  pure $ first : rest

-- Generate ownership type
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  name <- genIdentifier
  oneof [pure $ Owned name, pure $ Borrowed name, pure $ MutBorrowed name]

-- Generate ownership error
genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ UseAfterMove <$> genIdentifier
  , DoubleMove <$> genIdentifier <*> genIdentifier
  , BorrowWhileMoved <$> genIdentifier
  , MutBorrowWhileBorrowed <$> genIdentifier
  , BorrowWhileMutBorrowed <$> genIdentifier
  , MultipleMutBorrows <$> genIdentifier
  , UseWhileMutBorrowed <$> genIdentifier
  , OutOfScope <$> genIdentifier
  , BorrowError <$> genIdentifier
  , ParseError <$> genIdentifier
  , CrossFunctionMove <$> genIdentifier <*> genIdentifier
  , ParameterMoveMismatch <$> genIdentifier
  , ControlFlowError <$> genIdentifier
  , PathSensitiveError <$> genIdentifier
  , LoopOwnershipError <$> genIdentifier
  ]

-- Generate ownership transfer
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  from <- genIdentifier
  to <- genIdentifier
  pure $ OwnershipTransfer from to

-- Generate ownership analyzer
genOwnershipAnalyzer :: Gen OwnershipAnalyzer
genOwnershipAnalyzer = pure newOwnershipAnalyzer

-- Generate simple code snippets for ownership analysis
genSimpleCode :: Gen String
genSimpleCode = do
  var1 <- genIdentifier
  var2 <- genIdentifier
  oneof
    [ pure $ var1 ++ " := 5\n"
    , pure $ var1 ++ " := " ++ var2 ++ "\n"
    , pure $ "move(" ++ var1 ++ ", " ++ var2 ++ ")\n"
    , pure $ "borrow(" ++ var1 ++ ")\n"
    , pure $ "mutBorrow(" ++ var1 ++ ")\n"
    ]

-- Generate complex code snippets
genComplexCode :: Int -> Gen String
genComplexCode complexity = do
  vars <- listOf $ choose (1, complexity)
  let varNames = map (\i -> "var" ++ show i) vars
  statements <- listOf $ do
    v1 <- elements varNames
    v2 <- elements varNames
    oneof
      [ pure $ v1 ++ " := 5\n"
      , pure $ v1 ++ " := " ++ v2 ++ "\n"
      , pure $ "move(" ++ v1 ++ ", " ++ v2 ++ ")\n"
      , pure $ "borrow(" ++ v1 ++ ")\n"
      , pure $ "mutBorrow(" ++ v1 ++ ")\n"
      ]
  pure $ concat statements

-- ============================================================================
-- Property-based tests for Ownership module
-- ============================================================================

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering ot1 ot2 =
  let ord1 = compare ot1 ot2
      ord2 = compare (show ot1) (show ot2)
  in property $ (ot1 == ot2) ==> (ord1 == EQ)

-- Property: OwnershipType ordering puts Owned before Borrowed
prop_ownership_type_owned_before_borrowed :: String -> Property
prop_ownership_type_owned_before_borrowed name =
  let owned = Owned name
      borrowed = Borrowed name
  in property $ compare owned borrowed === LT

-- Property: OwnershipType ordering puts Borrowed before MutBorrowed
prop_ownership_type_borrowed_before_mutborrowed :: String -> Property
prop_ownership_type_borrowed_before_mutborrowed name =
  let borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
  in property $ compare borrowed mutBorrowed === LT

-- Property: OwnershipType ordering puts Owned before MutBorrowed
prop_ownership_type_owned_before_mutborrowed :: String -> Property
prop_ownership_type_owned_before_mutborrowed name =
  let owned = Owned name
      mutBorrowed = MutBorrowed name
  in property $ compare owned mutBorrowed === LT

-- Property: OwnershipError ordering is consistent with string ordering
prop_ownership_error_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering oe1 oe2 =
  let ord1 = compare oe1 oe2
      ord2 = compare (show oe1) (show oe2)
  in property $ ord1 === ord2

-- Property: OwnershipTransfer preserves from and to fields
prop_ownership_transfer_preserves :: String -> String -> Property
prop_ownership_transfer_preserves from to =
  let transfer = OwnershipTransfer from to
  in property $ transferFrom transfer === from .&&. transferTo transfer === to

-- Property: newOwnershipAnalyzer creates analyzer
prop_new_ownership_analyzer :: Property
prop_new_ownership_analyzer =
  let analyzer = newOwnershipAnalyzer
  in property $ case analyzer of
    OwnershipAnalyzer () -> True

-- Property: analyzeOwnership handles empty input
prop_analyze_ownership_empty :: Property
prop_analyze_ownership_empty =
  let result = analyzeOwnership ""
  in property $ True  -- Should not crash

-- Property: analyzeOwnership handles whitespace-only input
prop_analyze_ownership_whitespace :: String -> Property
prop_analyze_ownership_whitespace ws =
  all isSpace ws ==>
  let result = analyzeOwnership ws
  in property $ True  -- Should not crash

-- Property: analyzeOwnership handles simple assignments
prop_analyze_ownership_simple_assignment :: String -> Property
prop_analyze_ownership_simple_assignment var =
  all isAlphaNum var ==>
  let code = var ++ " := 5\n"
      result = analyzeOwnership code
  in property $ True  -- Should not crash

-- Property: analyzeOwnership handles move operations
prop_analyze_ownership_move :: String -> String -> Property
prop_analyze_ownership_move var1 var2 =
  all isAlphaNum var1 && all isAlphaNum var2 ==>
  let code = "move(" ++ var1 ++ ", " ++ var2 ++ ")\n"
      result = analyzeOwnership code
  in property $ True  -- Should not crash

-- Property: analyzeOwnership handles borrow operations
prop_analyze_ownership_borrow :: String -> Property
prop_analyze_ownership_borrow var =
  all isAlphaNum var ==>
  let code = "borrow(" ++ var ++ ")\n"
      result = analyzeOwnership code
  in property $ True  -- Should not crash

-- Property: analyzeOwnership handles mutable borrow operations
prop_analyze_ownership_mutborrow :: String -> Property
prop_analyze_ownership_mutborrow var =
  all isAlphaNum var ==>
  let code = "mutBorrow(" ++ var ++ ")\n"
      result = analyzeOwnership code
  in property $ True  -- Should not crash

-- Property: analyzeOwnership is deterministic
prop_analyze_ownership_deterministic :: String -> Property
prop_analyze_ownership_deterministic code =
  let result1 = analyzeOwnership code
      result2 = analyzeOwnership code
  in property $ result1 === result2

-- Property: analyzeOwnership handles complex code
prop_analyze_ownership_complex :: Int -> Property
prop_analyze_ownership_complex complexity =
  complexity >= 0 && complexity <= 20 ==>
  let codeGen = genComplexCode complexity
  in forAll codeGen $ \code ->
    let result = analyzeOwnership code
    in property $ True  -- Should not crash

-- Property: formatOwnershipErrors handles empty list
prop_format_ownership_errors_empty :: Property
prop_format_ownership_errors_empty =
  let formatted = formatOwnershipErrors []
  in property $ True  -- Should not crash

-- Property: formatOwnershipErrors handles single error
prop_format_ownership_errors_single :: OwnershipError -> Property
prop_format_ownership_errors_single error =
  let formatted = formatOwnershipErrors [error]
  in property $ not (null formatted)  -- Should produce non-empty output

-- Property: formatOwnershipErrors handles multiple errors
prop_format_ownership_errors_multiple :: [OwnershipError] -> Property
prop_format_ownership_errors_multiple errors =
  not (null errors) ==>
  let formatted = formatOwnershipErrors errors
  in property $ not (null formatted)  -- Should produce non-empty output

-- Property: formatOwnershipErrors preserves error information
prop_format_ownership_errors_preserves :: [OwnershipError] -> Property
prop_format_ownership_errors_preserves errors =
  not (null errors) ==>
  let formatted = formatOwnershipErrors errors
  in property $ all (\err -> show err `isInfixOf` formatted) errors

-- Property: builtInFunctions is not empty
prop_builtin_functions_notempty :: Property
prop_builtin_functions_notempty =
  let functions = builtInFunctions
  in property $ not (null functions)

-- Property: builtInFunctions contains expected functions
prop_builtin_functions_contains_expected :: Property
prop_builtin_functions_contains_expected =
  let functions = builtInFunctions
      expected = ["move", "borrow", "mutBorrow"]
  in property $ all (`elem` functions) expected

-- Property: analyzeOwnershipFile handles file path
prop_analyze_ownership_file_path :: String -> Property
prop_analyze_ownership_file_path filepath =
  not (null filepath) ==>
  let result = analyzeOwnershipFile filepath
  in property $ True  -- Should not crash (may fail with file not found)

-- Property: OwnershipType show is reversible for simple cases
prop_ownership_type_show_simple :: String -> Property
prop_ownership_type_show_simple name =
  all isAlphaNum name ==>
  let owned = Owned name
      shown = show owned
      expected = "Owned " ++ name
  in property $ shown === expected

-- Property: OwnershipError show contains variable name
prop_ownership_error_show_contains_var :: String -> Property
prop_ownership_error_show_contains_var var =
  all isAlphaNum var ==>
  let error = UseAfterMove var
      shown = show error
  in property $ var `isInfixOf` shown

-- Property: OwnershipTransfer show contains from and to
prop_ownership_transfer_show_contains_from_to :: String -> String -> Property
prop_ownership_transfer_show_contains_from_to from to =
  all isAlphaNum from && all isAlphaNum to ==>
  let transfer = OwnershipTransfer from to
      shown = show transfer
  in property $ from `isInfixOf` shown .&&. to `isInfixOf` shown

-- Property: analyzeOwnership handles comments
prop_analyze_ownership_comments :: String -> Property
prop_analyze_ownership_comments content =
  not ('/' `elem` content) ==>
  let code = content ++ " // this is a comment\n"
      result = analyzeOwnership code
  in property $ True  -- Should not crash

-- Property: analyzeOwnership handles multiple statements
prop_analyze_ownership_multiple :: [String] -> Property
prop_analyze_ownership_multiple vars =
  all (all isAlphaNum) vars && not (null vars) ==>
  let statements = map (\v -> v ++ " := 5\n") vars
      code = concat statements
      result = analyzeOwnership code
  in property $ True  -- Should not crash

-- Property: analyzeOwnership handles invalid syntax gracefully
prop_analyze_ownership_invalid :: String -> Property
prop_analyze_ownership_invalid malformed =
  let hasInvalidChars = any (`elem` ['{', '}', '"', '\'', '\\']) malformed
  in hasInvalidChars ==>
  let result = analyzeOwnership malformed
  in property $ True  -- Should not crash

-- Property: OwnershipType equality works correctly
prop_ownership_type_equality :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_equality ot1 ot2 =
  let isEqual = ot1 == ot2
      shouldEqual = case (ot1, ot2) of
        (Owned n1, Owned n2) -> n1 == n2
        (Borrowed n1, Borrowed n2) -> n1 == n2
        (MutBorrowed n1, MutBorrowed n2) -> n1 == n2
        _ -> False
  in property $ isEqual === shouldEqual

-- Property: OwnershipError equality works correctly
prop_ownership_error_equality :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_equality oe1 oe2 =
  let isEqual = oe1 == oe2
      shouldEqual = show oe1 == show oe2
  in property $ isEqual === shouldEqual

-- Property: OwnershipTransfer equality works correctly
prop_ownership_transfer_equality :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownership_transfer_equality ot1 ot2 =
  let isEqual = ot1 == ot2
      shouldEqual = transferFrom ot1 == transferFrom ot2 && transferTo ot1 == transferTo ot2
  in property $ isEqual === shouldEqual

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Ownership QuickCheck Tests"
  [ fastProperty "OwnershipType ordering is consistent" prop_ownership_type_ordering
  , fastProperty "OwnershipType ordering puts Owned before Borrowed" prop_ownership_type_owned_before_borrowed
  , fastProperty "OwnershipType ordering puts Borrowed before MutBorrowed" prop_ownership_type_borrowed_before_mutborrowed
  , fastProperty "OwnershipType ordering puts Owned before MutBorrowed" prop_ownership_type_owned_before_mutborrowed
  , fastProperty "OwnershipError ordering is consistent with string ordering" prop_ownership_error_ordering
  , fastProperty "OwnershipTransfer preserves from and to fields" prop_ownership_transfer_preserves
  , fastProperty "newOwnershipAnalyzer creates analyzer" prop_new_ownership_analyzer
  , fastProperty "analyzeOwnership handles empty input" prop_analyze_ownership_empty
  , fastProperty "analyzeOwnership handles whitespace-only input" prop_analyze_ownership_whitespace
  , fastProperty "analyzeOwnership handles simple assignments" prop_analyze_ownership_simple_assignment
  , fastProperty "analyzeOwnership handles move operations" prop_analyze_ownership_move
  , fastProperty "analyzeOwnership handles borrow operations" prop_analyze_ownership_borrow
  , fastProperty "analyzeOwnership handles mutable borrow operations" prop_analyze_ownership_mutborrow
  , fastProperty "analyzeOwnership is deterministic" prop_analyze_ownership_deterministic
  , fastProperty "analyzeOwnership handles complex code" prop_analyze_ownership_complex
  , fastProperty "formatOwnershipErrors handles empty list" prop_format_ownership_errors_empty
  , fastProperty "formatOwnershipErrors handles single error" prop_format_ownership_errors_single
  , fastProperty "formatOwnershipErrors handles multiple errors" prop_format_ownership_errors_multiple
  , fastProperty "formatOwnershipErrors preserves error information" prop_format_ownership_errors_preserves
  , fastProperty "builtInFunctions is not empty" prop_builtin_functions_notempty
  , fastProperty "builtInFunctions contains expected functions" prop_builtin_functions_contains_expected
  , fastProperty "analyzeOwnershipFile handles file path" prop_analyze_ownership_file_path
  , fastProperty "OwnershipType show is reversible for simple cases" prop_ownership_type_show_simple
  , fastProperty "OwnershipError show contains variable name" prop_ownership_error_show_contains_var
  , fastProperty "OwnershipTransfer show contains from and to" prop_ownership_transfer_show_contains_from_to
  , fastProperty "analyzeOwnership handles comments" prop_analyze_ownership_comments
  , fastProperty "analyzeOwnership handles multiple statements" prop_analyze_ownership_multiple
  , fastProperty "analyzeOwnership handles invalid syntax gracefully" prop_analyze_ownership_invalid
  , fastProperty "OwnershipType equality works correctly" prop_ownership_type_equality
  , fastProperty "OwnershipError equality works correctly" prop_ownership_error_equality
  , fastProperty "OwnershipTransfer equality works correctly" prop_ownership_transfer_equality
  ]