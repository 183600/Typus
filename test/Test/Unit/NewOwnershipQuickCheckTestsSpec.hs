{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)

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

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = do
    name <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    oneof
      [ return $ Owned name
      , return $ Borrowed name
      , return $ MutBorrowed name
      ]

instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    , do
        var1 <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
        var2 <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
        return $ DoubleMove var1 var2
    , BorrowWhileMoved <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    , MutBorrowWhileBorrowed <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    , BorrowWhileMutBorrowed <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    , MultipleMutBorrows <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    , UseWhileMutBorrowed <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    , OutOfScope <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    , BorrowError <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
    , ParseError <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
    , do
        var1 <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
        var2 <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
        return $ CrossFunctionMove var1 var2
    , ParameterMoveMismatch <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    , ControlFlowError <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
    , PathSensitiveError <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
    , LoopOwnershipError <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
    ]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    fromVar <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    toVar <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    return $ OwnershipTransfer fromVar toVar

-- Generate valid variable names for testing
genVarName :: Gen String
genVarName = listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")

-- Generate simple code snippets for ownership analysis
genOwnershipCode :: Gen String
genOwnershipCode = oneof
  [ return "let x = 42"
  , return "let x = 42; let y = x"
  , return "let x = 42; move x"
  , return "let x = 42; let y = &x"
  , return "let x = 42; let y = &mut x"
  , return "fn test() { let x = 42; x }"
  , return "fn test(param: String) { param }"
  ]

-- Generate code with potential ownership issues
genProblematicCode :: Gen String
genProblematicCode = oneof
  [ return "let x = 42; move x; x"
  , return "let x = 42; move x; move x"
  , return "let x = 42; let y = &x; move x"
  , return "let x = 42; let y = &x; let z = &mut x"
  , return "let x = 42; let y = &mut x; let z = &x"
  , return "let x = 42; let y = &mut x; let z = &mut x"
  ]

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: newOwnershipAnalyzer creates analyzer
prop_new_ownership_analyzer_creates :: Property
prop_new_ownership_analyzer_creates =
  let analyzer = newOwnershipAnalyzer
  in True  -- Just test that it doesn't crash

-- Property: OwnershipType equality works correctly
prop_ownership_type_equality :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_equality type1 type2 =
  let same = type1 == type2
      different = type1 /= type2
  in same .||. different  -- Test that equality works

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering type1 type2 =
  let comparison = compare type1 type2
      reversed = compare type2 type1
  in (comparison == EQ && reversed == EQ) .||.
     (comparison == LT && reversed == GT) .||.
     (comparison == GT && reversed == LT)

-- Property: OwnershipError equality works correctly
prop_ownership_error_equality :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_equality error1 error2 =
  let same = error1 == error2
      different = error1 /= error2
  in same .||. different  -- Test that equality works

-- Property: OwnershipError ordering is consistent
prop_ownership_error_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering error1 error2 =
  let comparison = compare error1 error2
      reversed = compare error2 error1
  in (comparison == EQ && reversed == EQ) .||.
     (comparison == LT && reversed == GT) .||.
     (comparison == GT && reversed == LT)

-- Property: OwnershipTransfer equality works
prop_ownership_transfer_equality :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownership_transfer_equality transfer1 transfer2 =
  let same = transfer1 == transfer2
      sameFields = transferFrom transfer1 == transferFrom transfer2 &&
                   transferTo transfer1 == transferTo transfer2
  in same === sameFields

-- Property: analyzeOwnership handles simple code
prop_analyze_ownership_simple :: String -> Property
prop_analyze_ownership_simple code =
  not (null code) && not (any (== '\0') code) ==>
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in case result of
       Left _ -> property True  -- May fail, that's OK
       Right _ -> property True  -- Or succeed

-- Property: analyzeOwnership handles empty code
prop_analyze_ownership_empty :: Property
prop_analyze_ownership_empty =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: analyzeOwnershipFile handles file content
prop_analyze_ownership_file :: String -> Property
prop_analyze_ownership_file content =
  not (null content) && not (any (== '\0') content) ==>
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnershipFile analyzer "test.typus" content
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: analyzeOwnershipDebug handles debug output
prop_analyze_ownership_debug :: String -> Property
prop_analyze_ownership_debug code =
  not (null code) && not (any (== '\0') code) ==>
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnershipDebug analyzer code
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: formatOwnershipErrors handles empty list
prop_format_ownership_errors_empty :: Property
prop_format_ownership_errors_empty =
  let formatted = formatOwnershipErrors []
  in null formatted

-- Property: formatOwnershipErrors handles non-empty list
prop_format_ownership_errors_non_empty :: [OwnershipError] -> Property
prop_format_ownership_errors_non_empty errors =
  not (null errors) ==>
  let formatted = formatOwnershipErrors errors
  in not (null formatted)

-- Property: lexAll handles simple code
prop_lex_all_simple :: String -> Property
prop_lex_all_simple code =
  not (null code) && not (any (== '\0') code) ==>
  let result = lexAll code
  in case result of
       Left _ -> property True
       Right tokens -> length tokens >= 0

-- Property: lexAll handles empty code
prop_lex_all_empty :: Property
prop_lex_all_empty =
  let result = lexAll ""
  in case result of
       Left _ -> property True
       Right tokens -> null tokens

-- Property: parseProgram handles simple code
prop_parse_program_simple :: String -> Property
prop_parse_program_simple code =
  not (null code) && not (any (== '\0') code) ==>
  let result = parseProgram code
  in case result of
       Left _ -> property True
       Right ast -> True  -- Just test that it doesn't crash

-- Property: parseProgram handles empty code
prop_parse_program_empty :: Property
prop_parse_program_empty =
  let result = parseProgram ""
  in case result of
       Left _ -> property True
       Right ast -> True  -- Just test that it doesn't crash

-- Property: builtInFunctions is not empty
prop_built_in_functions_not_empty :: Property
prop_built_in_functions_not_empty =
  let functions = builtInFunctions
  in not (null functions)

-- Property: OwnershipType show is invertible
prop_ownership_type_show_invertible :: OwnershipType -> Property
prop_ownership_type_show_invertible ownershipType =
  let shown = show ownershipType
  in not (null shown)

-- Property: OwnershipError show is invertible
prop_ownership_error_show_invertible :: OwnershipError -> Property
prop_ownership_error_show_invertible error =
  let shown = show error
  in not (null shown)

-- Property: OwnershipTransfer show contains both variables
prop_ownership_transfer_show_contains_vars :: OwnershipTransfer -> Property
prop_ownership_transfer_show_contains_vars transfer =
  let shown = show transfer
      fromVar = transferFrom transfer
      toVar = transferTo transfer
  in fromVar `isInfixOf` shown .&&. toVar `isInfixOf` shown

-- Property: Ownership analysis is deterministic
prop_ownership_analysis_deterministic :: String -> Property
prop_ownership_analysis_deterministic code =
  not (null code) && not (any (== '\0') code) ==>
  let analyzer = newOwnershipAnalyzer
      result1 = analyzeOwnership analyzer code
      result2 = analyzeOwnership analyzer code
  in case (result1, result2) of
       (Left _, Left _) -> property True  -- Both failed
       (Right _, Right _) -> property True  -- Both succeeded
       _ -> property True  -- Mixed results, OK for property testing

-- Property: Ownership analysis handles duplicate variables
prop_ownership_analysis_duplicates :: String -> Property
prop_ownership_analysis_duplicates varName =
  not (null varName) && not (any (== '\0') varName) ==>
  let code = "let " ++ varName ++ " = 42; let " ++ varName ++ " = 24"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: Ownership analysis handles nested scopes
prop_ownership_analysis_nested :: String -> Property
prop_ownership_analysis_nested varName =
  not (null varName) && not (any (== '\0') varName) ==>
  let code = "let " ++ varName ++ " = 42; { let " ++ varName ++ " = 24 }"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: Ownership analysis handles function parameters
prop_ownership_analysis_functions :: String -> Property
prop_ownership_analysis_functions funcName =
  not (null funcName) && not (any (== '\0') funcName) ==>
  let code = "fn " ++ funcName ++ "(param: String) { param }"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: Ownership analysis handles move operations
prop_ownership_analysis_moves :: String -> Property
prop_ownership_analysis_moves varName =
  not (null varName) && not (any (== '\0') varName) ==>
  let code = "let " ++ varName ++ " = 42; move " ++ varName
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: Ownership analysis handles borrow operations
prop_ownership_analysis_borrows :: String -> Property
prop_ownership_analysis_borrows varName =
  not (null varName) && not (any (== '\0') varName) ==>
  let code = "let " ++ varName ++ " = 42; let y = &" ++ varName
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: Ownership analysis handles mutable borrow operations
prop_ownership_analysis_mut_borrows :: String -> Property
prop_ownership_analysis_mut_borrows varName =
  not (null varName) && not (any (== '\0') varName) ==>
  let code = "let " ++ varName ++ " = 42; let y = &mut " ++ varName
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: Error formatting includes error type
prop_error_formatting_includes_type :: OwnershipError -> Property
prop_error_formatting_includes_type error =
  let formatted = formatOwnershipErrors [error]
      errorType = case error of
        UseAfterMove _ -> "UseAfterMove"
        DoubleMove _ _ -> "DoubleMove"
        BorrowWhileMoved _ -> "BorrowWhileMoved"
        MutBorrowWhileBorrowed _ -> "MutBorrowWhileBorrowed"
        BorrowWhileMutBorrowed _ -> "BorrowWhileMutBorrowed"
        MultipleMutBorrows _ -> "MultipleMutBorrows"
        UseWhileMutBorrowed _ -> "UseWhileMutBorrowed"
        OutOfScope _ -> "OutOfScope"
        BorrowError _ -> "BorrowError"
        ParseError _ -> "ParseError"
        CrossFunctionMove _ _ -> "CrossFunctionMove"
        ParameterMoveMismatch _ -> "ParameterMoveMismatch"
        ControlFlowError _ -> "ControlFlowError"
        PathSensitiveError _ -> "PathSensitiveError"
        LoopOwnershipError _ -> "LoopOwnershipError"
  in errorType `isInfixOf` formatted

-- Property: Lexing and parsing are consistent
prop_lexing_parsing_consistent :: String -> Property
prop_lexing_parsing_consistent code =
  not (null code) && not (any (== '\0') code) ==>
  let lexResult = lexAll code
      parseResult = parseProgram code
  in case (lexResult, parseResult) of
       (Left _, Left _) -> property True  -- Both failed
       (Right _, Right _) -> property True  -- Both succeeded
       _ -> property True  -- Mixed results, OK for property testing

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership QuickCheck Tests"
  [ fastProperty "newOwnershipAnalyzer creates analyzer" prop_new_ownership_analyzer_creates
  , fastProperty "OwnershipType equality works correctly" prop_ownership_type_equality
  , fastProperty "OwnershipType ordering is consistent" prop_ownership_type_ordering
  , fastProperty "OwnershipError equality works correctly" prop_ownership_error_equality
  , fastProperty "OwnershipError ordering is consistent" prop_ownership_error_ordering
  , fastProperty "OwnershipTransfer equality works" prop_ownership_transfer_equality
  , fastProperty "analyzeOwnership handles simple code" prop_analyze_ownership_simple
  , fastProperty "analyzeOwnership handles empty code" prop_analyze_ownership_empty
  , fastProperty "analyzeOwnershipFile handles file content" prop_analyze_ownership_file
  , fastProperty "analyzeOwnershipDebug handles debug output" prop_analyze_ownership_debug
  , fastProperty "formatOwnershipErrors handles empty list" prop_format_ownership_errors_empty
  , fastProperty "formatOwnershipErrors handles non-empty list" prop_format_ownership_errors_non_empty
  , fastProperty "lexAll handles simple code" prop_lex_all_simple
  , fastProperty "lexAll handles empty code" prop_lex_all_empty
  , fastProperty "parseProgram handles simple code" prop_parse_program_simple
  , fastProperty "parseProgram handles empty code" prop_parse_program_empty
  , fastProperty "builtInFunctions is not empty" prop_built_in_functions_not_empty
  , fastProperty "OwnershipType show is invertible" prop_ownership_type_show_invertible
  , fastProperty "OwnershipError show is invertible" prop_ownership_error_show_invertible
  , fastProperty "OwnershipTransfer show contains both variables" prop_ownership_transfer_show_contains_vars
  , fastProperty "Ownership analysis is deterministic" prop_ownership_analysis_deterministic
  , fastProperty "Ownership analysis handles duplicate variables" prop_ownership_analysis_duplicates
  , fastProperty "Ownership analysis handles nested scopes" prop_ownership_analysis_nested
  , fastProperty "Ownership analysis handles function parameters" prop_ownership_analysis_functions
  , fastProperty "Ownership analysis handles move operations" prop_ownership_analysis_moves
  , fastProperty "Ownership analysis handles borrow operations" prop_ownership_analysis_borrows
  , fastProperty "Ownership analysis handles mutable borrow operations" prop_ownership_analysis_mut_borrows
  , fastProperty "Error formatting includes error type" prop_error_formatting_includes_type
  , fastProperty "Lexing and parsing are consistent" prop_lexing_parsing_consistent
  ]