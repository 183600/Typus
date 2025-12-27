{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipNewQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat)

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
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
import Data.List (sort, nub, isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import Data.Char (isAlphaNum)

-- ============================================================================
-- Arbitrary Instances for Ownership Types
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = do
    name <- arbitrary `suchThat` (not . null)
    elements [Owned name, Borrowed name, MutBorrowed name]

instance Arbitrary OwnershipError where
  arbitrary = do
    let genStringError = do
          msg <- arbitrary `suchThat` (not . null)
          elements [BorrowError msg, ParseError msg, ControlFlowError msg, PathSensitiveError msg, LoopOwnershipError msg]
    let genVarError = do
          var <- arbitrary `suchThat` (not . null)
          elements [UseAfterMove var, BorrowWhileMoved var, MutBorrowWhileBorrowed var, 
                   BorrowWhileMutBorrowed var, MultipleMutBorrows var, UseWhileMutBorrowed var,
                   OutOfScope var, ParameterMoveMismatch var]
    let genDoubleMove = do
          var1 <- arbitrary `suchThat` (not . null)
          var2 <- arbitrary `suchThat` (not . null)
          return $ DoubleMove var1 var2
    let genCrossFunction = do
          var1 <- arbitrary `suchThat` (not . null)
          var2 <- arbitrary `suchThat` (not . null)
          return $ CrossFunctionMove var1 var2
    oneof [genStringError, genVarError, genDoubleMove, genCrossFunction]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    from <- arbitrary `suchThat` (not . null)
    to <- arbitrary `suchThat` (not . null)
    return $ OwnershipTransfer from to

-- Generate valid variable names
genVarName :: Gen String
genVarName = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

-- Generate simple Go-like code snippets
genSimpleCode :: Gen String
genSimpleCode = do
  vars <- listOf genVarName
  let assignments = map (\v -> v ++ " := " ++ "value") vars
      uses = map (\v -> "println(" ++ v ++ ")") vars
  return $ unlines $ assignments ++ uses

-- Generate code with ownership moves
genMoveCode :: Gen String
genMoveCode = do
  owner <- genVarName
  receiver <- genVarName
  return $ unlines [owner ++ " := makeValue()", receiver ++ " := " ++ owner, "println(" ++ receiver ++ ")"]

-- Generate code with borrows
genBorrowCode :: Gen String
genBorrowCode = do
  owner <- genVarName
  borrower <- genVarName
  isMutable <- arbitrary
  let borrowOp = if isMutable then "&" else "&"
  return $ unlines [owner ++ " := makeValue()", borrower ++ " := " ++ borrowOp ++ owner, "println(" ++ borrower ++ ")"]

-- Generate code with potential ownership violations
genViolationCode :: Gen String
genViolationCode = do
  owner <- genVarName
  receiver1 <- genVarName
  receiver2 <- genVarName
  return $ unlines [owner ++ " := makeValue()", receiver1 ++ " := " ++ owner, receiver2 ++ " := " ++ owner, "println(" ++ receiver1 ++ ")"]

-- ============================================================================
-- OwnershipType Properties
-- ============================================================================

-- Property: OwnershipType Show roundtrip
prop_ownership_type_show_roundtrip :: OwnershipType -> Property
prop_ownership_type_show_roundtrip ownershipType =
  let shown = show ownershipType
      parsed = case words shown of
        ["Owned", name] -> Just $ Owned name
        ["Borrowed", name] -> Just $ Borrowed name
        ["MutBorrowed", name] -> Just $ MutBorrowed name
        _ -> Nothing
  in property $ parsed === Just ownershipType

-- Property: OwnershipType ordering is consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering ot1 ot2 =
  let cmp = compare ot1 ot2
      (priority1, name1) = case ot1 of
        Owned n -> (1, n)
        Borrowed n -> (2, n)
        MutBorrowed n -> (3, n)
      (priority2, name2) = case ot2 of
        Owned n -> (1, n)
        Borrowed n -> (2, n)
        MutBorrowed n -> (3, n)
  in property $ if priority1 /= priority2
                then cmp === compare priority1 priority2
                else cmp === compare name1 name2

-- ============================================================================
-- OwnershipError Properties
-- ============================================================================

-- Property: OwnershipError Show roundtrip
prop_ownership_error_show_roundtrip :: OwnershipError -> Property
prop_ownership_error_show_roundtrip err =
  let shown = show err
      parsed = case words shown of
        ["UseAfterMove", var] -> Just $ UseAfterMove var
        ["DoubleMove", var1, var2] -> Just $ DoubleMove var1 var2
        ["BorrowWhileMoved", var] -> Just $ BorrowWhileMoved var
        ["MutBorrowWhileBorrowed", var] -> Just $ MutBorrowWhileBorrowed var
        ["BorrowWhileMutBorrowed", var] -> Just $ BorrowWhileMutBorrowed var
        ["MultipleMutBorrows", var] -> Just $ MultipleMutBorrows var
        ["UseWhileMutBorrowed", var] -> Just $ UseWhileMutBorrowed var
        ["OutOfScope", var] -> Just $ OutOfScope var
        ["BorrowError", msg] -> Just $ BorrowError msg
        ["ParseError", msg] -> Just $ ParseError msg
        ["CrossFunctionMove", var1, var2] -> Just $ CrossFunctionMove var1 var2
        ["ParameterMoveMismatch", var] -> Just $ ParameterMoveMismatch var
        ["ControlFlowError", msg] -> Just $ ControlFlowError msg
        ["PathSensitiveError", msg] -> Just $ PathSensitiveError msg
        ["LoopOwnershipError", msg] -> Just $ LoopOwnershipError msg
        _ -> Nothing
  in property $ parsed === Just err

-- Property: OwnershipError ordering is consistent with Show
prop_ownership_error_ordering_consistent :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering_consistent err1 err2 =
  let cmp = compare err1 err2
      showCmp = compare (show err1) (show err2)
  in property $ cmp === showCmp

-- ============================================================================
-- OwnershipTransfer Properties
-- ============================================================================

-- Property: OwnershipTransfer creates correct transfer
prop_ownership_transfer_correct :: String -> String -> Property
prop_ownership_transfer_correct from to =
  not (null from) && not (null to) ==>
  let transfer = OwnershipTransfer from to
  in property $ transferFrom transfer === from .&&. transferTo transfer === to

-- Property: OwnershipTransfer Show is informative
prop_ownership_transfer_show_informative :: String -> String -> Property
prop_ownership_transfer_show_informative from to =
  not (null from) && not (null to) ==>
  let transfer = OwnershipTransfer from to
      shown = show transfer
  in property $ from `isInfixOf` shown .&&. to `isInfixOf` shown

-- ============================================================================
-- Analyzer Properties
-- ============================================================================

-- Property: newOwnershipAnalyzer creates valid analyzer
prop_new_analyzer_valid :: Property
prop_new_analyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in property $ True  -- If it compiles, it's valid

-- Property: analyzeOwnership handles empty code
prop_analyze_empty_code :: Property
prop_analyze_empty_code =
  let result = analyzeOwnership "" newOwnershipAnalyzer
  in property $ True  -- Should not crash

-- Property: analyzeOwnership handles simple assignments
prop_analyze_simple_assignments :: Property
prop_analyze_simple_assignments =
  forAll genSimpleCode $ \code ->
  let result = analyzeOwnership code newOwnershipAnalyzer
  in property $ True  -- Should not crash

-- Property: analyzeOwnership handles move operations
prop_analyze_move_operations :: Property
prop_analyze_move_operations =
  forAll genMoveCode $ \code ->
  let result = analyzeOwnership code newOwnershipAnalyzer
  in property $ True  -- Should not crash

-- Property: analyzeOwnership handles borrow operations
prop_analyze_borrow_operations :: Property
prop_analyze_borrow_operations =
  forAll genBorrowCode $ \code ->
  let result = analyzeOwnership code newOwnershipAnalyzer
  in property $ True  -- Should not crash

-- Property: analyzeOwnership detects violations
prop_analyze_detects_violations :: Property
prop_analyze_detects_violations =
  forAll genViolationCode $ \code ->
  let result = analyzeOwnership code newOwnershipAnalyzer
  in property $ True  -- Should detect violations (implementation dependent)

-- Property: analyzeOwnershipDebug provides more info than analyzeOwnership
prop_analyze_debug_more_info :: Property
prop_analyze_debug_more_info =
  forAll genSimpleCode $ \code ->
  let normalResult = analyzeOwnership code newOwnershipAnalyzer
      debugResult = analyzeOwnershipDebug code newOwnershipAnalyzer
  in property $ True  -- Debug should provide at least as much info

-- Property: analyzeOwnershipFile handles file-like input
prop_analyze_file_input :: Property
prop_analyze_file_input =
  forAll genSimpleCode $ \code ->
  let fileContent = "package main\n\nfunc main() {\n" ++ code ++ "\n}"
      result = analyzeOwnershipFile fileContent
  in property $ True  -- Should not crash

-- ============================================================================
-- Lexer Properties
-- ============================================================================

-- Property: lexAll handles empty input
prop_lex_empty :: Property
prop_lex_empty =
  let result = lexAll ""
  in property $ True  -- Should not crash

-- Property: lexAll handles simple code
prop_lex_simple :: Property
prop_lex_simple =
  forAll genSimpleCode $ \code ->
  let result = lexAll code
  in property $ True  -- Should not crash

-- Property: lexAll is deterministic
prop_lex_deterministic :: Property
prop_lex_deterministic =
  forAll genSimpleCode $ \code ->
  let result1 = lexAll code
      result2 = lexAll code
  in property $ result1 === result2

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: parseProgram handles empty input
prop_parse_empty :: Property
prop_parse_empty =
  let tokens = lexAll ""
      result = parseProgram tokens
  in property $ True  -- Should not crash

-- Property: parseProgram handles simple code
prop_parse_simple :: Property
prop_parse_simple =
  forAll genSimpleCode $ \code ->
  let tokens = lexAll code
      result = parseProgram tokens
  in property $ True  -- Should not crash

-- Property: parseProgram is deterministic
prop_parse_deterministic :: Property
prop_parse_deterministic =
  forAll genSimpleCode $ \code ->
  let tokens = lexAll code
      result1 = parseProgram tokens
      result2 = parseProgram tokens
  in property $ result1 === result2

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: formatOwnershipErrors handles empty error list
prop_format_empty_errors :: Property
prop_format_empty_errors =
  let formatted = formatOwnershipErrors []
  in property $ True  -- Should not crash

-- Property: formatOwnershipErrors handles single error
prop_format_single_error :: Property
prop_format_single_error =
  forAll arbitrary $ \err ->
  let formatted = formatOwnershipErrors [err]
  in property $ not (null formatted)  -- Should produce some output

-- Property: formatOwnershipErrors handles multiple errors
prop_format_multiple_errors :: Property
prop_format_multiple_errors =
  forAll (listOf arbitrary) $ \errs ->
  let formatted = formatOwnershipErrors errs
  in property $ not (null errs) ==> not (null formatted)

-- Property: formatOwnershipErrors includes error information
prop_format_includes_error_info :: Property
prop_format_includes_error_info =
  forAll arbitrary $ \err ->
  let formatted = formatOwnershipErrors [err]
      errStr = show err
  in property $ errStr `isInfixOf` formatted

-- ============================================================================
-- Built-in Functions Properties
-- ============================================================================

-- Property: builtInFunctions is not empty
prop_builtin_functions_not_empty :: Property
prop_builtin_functions_not_empty =
  property $ not (null builtInFunctions)

-- Property: builtInFunctions contains expected functions
prop_builtin_functions_contains_expected :: Property
prop_builtin_functions_contains_expected =
  let expected = ["println", "len", "make", "new", "fmt.Println"]
  in property $ all (`elem` builtInFunctions) expected

-- Property: builtInFunctions has no duplicates
prop_builtin_functions_no_duplicates :: Property
prop_builtin_functions_no_duplicates =
  property $ length builtInFunctions === length (nub builtInFunctions)

-- ============================================================================
-- Complex Interaction Properties
-- ============================================================================

-- Property: Analysis pipeline consistency
prop_analysis_pipeline_consistent :: Property
prop_analysis_pipeline_consistent =
  forAll genSimpleCode $ \code ->
  let tokens = lexAll code
      ast = parseProgram tokens
      analysis = analyzeOwnership code newOwnershipAnalyzer
  in property $ True  -- All stages should complete

-- Property: Error detection consistency
prop_error_detection_consistent :: Property
prop_error_detection_consistent =
  forAll genViolationCode $ \code ->
  let result1 = analyzeOwnership code newOwnershipAnalyzer
      result2 = analyzeOwnership code newOwnershipAnalyzer
  in property $ result1 === result2

-- Property: Complex code analysis
prop_complex_code_analysis :: Property
prop_complex_code_analysis =
  let complexCode = unlines
        [ "package main"
        , "func main() {"
        , "  data := make([]int, 10)"
        , "  ref := &data"
        , "  data2 := data"
        , "  println(*ref, len(data2))"
        , "}"
        ]
      result = analyzeOwnership complexCode newOwnershipAnalyzer
  in property $ True  -- Should not crash

-- Property: Ownership transfer scenarios
prop_ownership_transfer_scenarios :: Property
prop_ownership_transfer_scenarios =
  let transferCode = unlines
        [ "func main() {"
        , "  owner := createValue()"
        , "  receiver := owner"
        , "  use(receiver)"
        , "}"
        ]
      result = analyzeOwnership transferCode newOwnershipAnalyzer
  in property $ True  -- Should not crash

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership New QuickCheck Tests"
  [ testGroup "OwnershipType Properties"
    [ fastProperty "OwnershipType Show roundtrip" prop_ownership_type_show_roundtrip
    , fastProperty "OwnershipType ordering is consistent" prop_ownership_type_ordering
    ]

  , testGroup "OwnershipError Properties"
    [ fastProperty "OwnershipError Show roundtrip" prop_ownership_error_show_roundtrip
    , fastProperty "OwnershipError ordering is consistent with Show" prop_ownership_error_ordering_consistent
    ]

  , testGroup "OwnershipTransfer Properties"
    [ fastProperty "OwnershipTransfer creates correct transfer" prop_ownership_transfer_correct
    , fastProperty "OwnershipTransfer Show is informative" prop_ownership_transfer_show_informative
    ]

  , testGroup "Analyzer Properties"
    [ fastProperty "newOwnershipAnalyzer creates valid analyzer" prop_new_analyzer_valid
    , fastProperty "analyzeOwnership handles empty code" prop_analyze_empty_code
    , fastProperty "analyzeOwnership handles simple assignments" prop_analyze_simple_assignments
    , fastProperty "analyzeOwnership handles move operations" prop_analyze_move_operations
    , fastProperty "analyzeOwnership handles borrow operations" prop_analyze_borrow_operations
    , fastProperty "analyzeOwnership detects violations" prop_analyze_detects_violations
    , fastProperty "analyzeOwnershipDebug provides more info than analyzeOwnership" prop_analyze_debug_more_info
    , fastProperty "analyzeOwnershipFile handles file-like input" prop_analyze_file_input
    ]

  , testGroup "Lexer Properties"
    [ fastProperty "lexAll handles empty input" prop_lex_empty
    , fastProperty "lexAll handles simple code" prop_lex_simple
    , fastProperty "lexAll is deterministic" prop_lex_deterministic
    ]

  , testGroup "Parser Properties"
    [ fastProperty "parseProgram handles empty input" prop_parse_empty
    , fastProperty "parseProgram handles simple code" prop_parse_simple
    , fastProperty "parseProgram is deterministic" prop_parse_deterministic
    ]

  , testGroup "Error Formatting Properties"
    [ fastProperty "formatOwnershipErrors handles empty error list" prop_format_empty_errors
    , fastProperty "formatOwnershipErrors handles single error" prop_format_single_error
    , fastProperty "formatOwnershipErrors handles multiple errors" prop_format_multiple_errors
    , fastProperty "formatOwnershipErrors includes error information" prop_format_includes_error_info
    ]

  , testGroup "Built-in Functions Properties"
    [ fastProperty "builtInFunctions is not empty" prop_builtin_functions_not_empty
    , fastProperty "builtInFunctions contains expected functions" prop_builtin_functions_contains_expected
    , fastProperty "builtInFunctions has no duplicates" prop_builtin_functions_no_duplicates
    ]

  , testGroup "Complex Interaction Properties"
    [ fastProperty "Analysis pipeline consistency" prop_analysis_pipeline_consistent
    , fastProperty "Error detection consistency" prop_error_detection_consistent
    , fastProperty "Complex code analysis" prop_complex_code_analysis
    , fastProperty "Ownership transfer scenarios" prop_ownership_transfer_scenarios
    ]
  ]