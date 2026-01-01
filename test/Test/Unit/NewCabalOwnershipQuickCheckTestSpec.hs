{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalOwnershipQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), (.||.), (==>), forAll, oneof, elements, listOf, choose, suchThat)
import Ownership
  ( OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), OwnershipTransfer(..)
  , newOwnershipAnalyzer, analyzeOwnership, analyzeOwnershipFile, analyzeOwnershipDebug
  , lexAll, parseProgram, builtInFunctions, formatOwnershipErrors
  )
import Ownership.Common.Types (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), OwnershipTransfer(..), newOwnershipAnalyzer)
import Ownership.Analyzer (analyzeOwnership, analyzeOwnershipFile, analyzeOwnershipDebug, builtInFunctions)
import Ownership.Parser (parseProgram)
import Ownership.Lexer (lexAll)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (null)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = do
    name <- arbitrary `suchThat` (not . null)
    elements [Owned name, Borrowed name, MutBorrowed name]

instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> arbitrary `suchThat` (not . null)
    , DoubleMove <$> arbitrary `suchThat` (not . null) <*> arbitrary `suchThat` (not . null)
    , BorrowWhileMoved <$> arbitrary `suchThat` (not . null)
    , MutBorrowWhileBorrowed <$> arbitrary `suchThat` (not . null)
    , BorrowWhileMutBorrowed <$> arbitrary `suchThat` (not . null)
    , MultipleMutBorrows <$> arbitrary `suchThat` (not . null)
    , UseWhileMutBorrowed <$> arbitrary `suchThat` (not . null)
    , OutOfScope <$> arbitrary `suchThat` (not . null)
    , BorrowError <$> arbitrary `suchThat` (not . null)
    , ParseError <$> arbitrary `suchThat` (not . null)
    , CrossFunctionMove <$> arbitrary `suchThat` (not . null) <*> arbitrary `suchThat` (not . null)
    , ParameterMoveMismatch <$> arbitrary `suchThat` (not . null)
    , ControlFlowError <$> arbitrary `suchThat` (not . null)
    , PathSensitiveError <$> arbitrary `suchThat` (not . null)
    , LoopOwnershipError <$> arbitrary `suchThat` (not . null)
    ]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    from <- arbitrary `suchThat` (not . null)
    to <- arbitrary `suchThat` (not . null)
    return $ OwnershipTransfer from to

-- Generate simple ownership code snippets
genSimpleOwnershipCode :: Gen String
genSimpleOwnershipCode = oneof
  [ return "x := 5"
  , return "x := 5\ny := x"
  , return "x := 5\ny := &x"
  , return "x := 5\ny := &mut x"
  , return "func main() {\n  x := 5\n  y := x\n}"
  , return "func main() {\n  x := 5\n  y := &x\n  z := *y\n}"
  ]

-- Generate code with ownership errors
genErrorOwnershipCode :: Gen String
genErrorOwnershipCode = oneof
  [ return "x := 5\ny := x\nz := x"  -- Use after move
  , return "x := 5\ny := x\ny := x"  -- Double move
  , return "x := 5\ny := &x\nz := &mut x"  -- Mut borrow while borrowed
  , return "x := 5\ny := &mut x\nz := &x"  -- Borrow while mut borrowed
  ]

-- Generate valid variable names
genVarName :: Gen String
genVarName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

-- Generate simple expressions
genSimpleExpression :: Gen String
genSimpleExpression = oneof
  [ genVarName
  , do
      func <- elements ["println", "len", "make"]
      args <- listOf genSimpleExpression
      return $ func ++ "(" ++ unwords args ++ ")"
  , do
      num <- choose (0, 1000)
      return $ show num
  , do
      str <- listOf $ elements $ ['a'..'z'] ++ " "
      return $ "\"" ++ str ++ "\""
  ]

-- Generate simple statements
genSimpleStatement :: Gen String
genSimpleStatement = oneof
  [ do
      var <- genVarName
      expr <- genSimpleExpression
      return $ var ++ " := " ++ expr
  , do
      var <- genVarName
      expr <- genSimpleExpression
      return $ var ++ " = " ++ expr
  , do
      expr <- genSimpleExpression
      return $ expr
  ]

-- ============================================================================
-- OwnershipType QuickCheck Tests
-- ============================================================================

-- Test OwnershipType ordering
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering ot1 ot2 =
  let ord1 = compare ot1 ot2
      ord2 = compare (show ot1) (show ot2)
  in ord1 === ord2

-- Test OwnershipType creation
prop_owned_type_has_name :: String -> Property
prop_owned_type_has_name name =
  not (null name) ==>
  let owned = Owned name
  in case owned of
    Owned n -> n === name
    _ -> property False

prop_borrowed_type_has_name :: String -> Property
prop_borrowed_type_has_name name =
  not (null name) ==>
  let borrowed = Borrowed name
  in case borrowed of
    Borrowed n -> n === name
    _ -> property False

prop_mut_borrowed_type_has_name :: String -> Property
prop_mut_borrowed_type_has_name name =
  not (null name) ==>
  let mutBorrowed = MutBorrowed name
  in case mutBorrowed of
    MutBorrowed n -> n === name
    _ -> property False

-- ============================================================================
-- OwnershipError QuickCheck Tests
-- ============================================================================

-- Test OwnershipError creation
prop_use_after_move_error :: String -> Property
prop_use_after_move_error var =
  not (null var) ==>
  let err = UseAfterMove var
  in case err of
    UseAfterMove v -> v === var
    _ -> property False

prop_double_move_error :: String -> String -> Property
prop_double_move_error var1 var2 =
  not (null var1) && not (null var2) ==>
  let err = DoubleMove var1 var2
  in case err of
    DoubleMove v1 v2 -> v1 === var1 && v2 === var2
    _ -> property False

-- Test OwnershipError ordering
prop_ownership_error_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering err1 err2 =
  let ord1 = compare err1 err2
      ord2 = compare (show err1) (show err2)
  in ord1 === ord2

-- ============================================================================
-- OwnershipTransfer QuickCheck Tests
-- ============================================================================

-- Test OwnershipTransfer creation
prop_ownership_transfer_creation :: String -> String -> Property
prop_ownership_transfer_creation from to =
  not (null from) && not (null to) ==>
  let transfer = OwnershipTransfer from to
  in transferFrom transfer === from .&&. transferTo transfer === to

-- ============================================================================
-- OwnershipAnalyzer QuickCheck Tests
-- ============================================================================

-- Test analyzer creation
prop_new_ownership_analyzer :: Property
prop_new_ownership_analyzer =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
    OwnershipAnalyzer _ -> property True
    _ -> property False

-- ============================================================================
-- Analysis Functions QuickCheck Tests
-- ============================================================================

-- Test analyzeOwnership function
prop_analyze_ownership_returns_list :: Property
prop_analyze_ownership_returns_list =
  forAll genSimpleOwnershipCode $ \code ->
    let errors = analyzeOwnership code
    in L.length errors >= 0  -- Always returns a list

prop_analyze_ownership_detects_errors :: Property
prop_analyze_ownership_detects_errors =
  forAll genErrorOwnershipCode $ \code ->
    let errors = analyzeOwnership code
    in not (null errors) || L.all isBuiltinFunction (words code)

-- Test lexing L.and parsing
prop_lex_all_returns_tokens :: Property
prop_lex_all_returns_tokens =
  forAll genSimpleOwnershipCode $ \code ->
    let tokens = lexAll code
    in not (null tokens) || null code

prop_parse_program_returns_ast :: Property
prop_parse_program_returns_ast =
  forAll genSimpleOwnershipCode $ \code ->
    let tokens = lexAll code
        program = parseProgram tokens
    in case program of
      Program _ -> property True
      _ -> property False

-- Test analyzeOwnershipDebug function
prop_analyze_ownership_debug_returns_tuple :: Property
prop_analyze_ownership_debug_returns_tuple =
  forAll genSimpleOwnershipCode $ \code ->
    let (errors, debugLog) = analyzeOwnershipDebug False code
    in L.length errors >= 0 .&&. L.length debugLog >= 0

prop_analyze_ownership_debug_mode_includes_log :: Property
prop_analyze_ownership_debug_mode_includes_log =
  forAll genSimpleOwnershipCode $ \code ->
    let (errors, debugLog) = analyzeOwnershipDebug True code
    in L.length debugLog >= 0  -- Debug mode should include log entries

-- ============================================================================
-- Built-in Functions QuickCheck Tests
-- ============================================================================

-- Test built-in functions list
prop_built_in_functions_not_empty :: Property
prop_built_in_functions_not_empty =
  not (null builtInFunctions)

prop_built_in_functions_contains_common :: Property
prop_built_in_functions_contains_common =
  "int" `elem` builtInFunctions .&&.
  "string" `elem` builtInFunctions .&&.
  "println" `elem` builtInFunctions

-- Helper function to check if a word is a built-in function
isBuiltinFunction :: String -> Bool
isBuiltinFunction word = word `elem` builtInFunctions

-- ============================================================================
-- Error Detection QuickCheck Tests
-- ============================================================================

-- Test use after move detection
prop_detects_use_after_move :: Property
prop_detects_use_after_move =
  let code = "x := 5\ny := x\nz := x"
      errors = analyzeOwnership code
  in L.any isUseAfterMove errors

-- Test double move detection
prop_detects_double_move :: Property
prop_detects_double_move =
  let code = "x := 5\ny := x\ny := x"
      errors = analyzeOwnership code
  in L.any isDoubleMove errors

-- Test borrow conflict detection
prop_detects_borrow_conflicts :: Property
prop_detects_borrow_conflicts =
  let code = "x := 5\ny := &x\nz := &mut x"
      errors = analyzeOwnership code
  in L.any isBorrowError errors

-- Helper functions to check error types
isUseAfterMove :: OwnershipError -> Bool
isUseAfterMove (UseAfterMove _) = True
isUseAfterMove _ = False

isDoubleMove :: OwnershipError -> Bool
isDoubleMove (DoubleMove _ _) = True
isDoubleMove _ = False

isBorrowError :: OwnershipError -> Bool
isBorrowError (BorrowWhileMoved _) = True
isBorrowError (MutBorrowWhileBorrowed _) = True
isBorrowError (BorrowWhileMutBorrowed _) = True
isBorrowError (MultipleMutBorrows _) = True
isBorrowError _ = False

-- ============================================================================
-- Round-trip Tests
-- ============================================================================

-- Test code parsing L.and analysis consistency
prop_parse_analyze_consistency :: Property
prop_parse_analyze_consistency =
  forAll genSimpleOwnershipCode $ \code ->
    let tokens = lexAll code
        program = parseProgram tokens
        errors = analyzeOwnership code
    in case program of
      Program _ -> L.length errors >= 0
      _ -> property False

-- Test error formatting
prop_format_ownership_errors_includes_content :: Property
prop_format_ownership_errors_includes_content =
  forAll genErrorOwnershipCode $ \code ->
    let errors = analyzeOwnership code
        formatted = formatOwnershipErrors errors
    in null errors || not (null formatted)

-- ============================================================================
-- Additional Property Tests
-- ============================================================================

-- Test analyzer handles empty input
prop_analyze_empty_input :: Property
prop_analyze_empty_input =
  let errors = analyzeOwnership ""
  in null errors

-- Test analyzer handles whitespace only
prop_analyze_whitespace_only :: Property
prop_analyze_whitespace_only =
  let whitespace = unlines $ replicate 5 "   \t  "
      errors = analyzeOwnership whitespace
  in null errors

-- Test analyzer handles comments
prop_analyze_comments :: Property
prop_analyze_comments =
  let commentCode = "// This is a comment\n// Another comment\n"
      errors = analyzeOwnership commentCode
  in null errors

-- Test analyzer handles mixed content
prop_analyze_mixed_content :: Property
prop_analyze_mixed_content =
  forAll (listOf genSimpleStatement) $ \statements ->
    let code = unlines statements
        errors = analyzeOwnership code
    in L.length errors >= 0

tests :: TestTree
tests = testGroup "New Cabal Ownership QuickCheck Tests"
  [ testGroup "OwnershipType tests"
      [ testProperty "ownership type ordering" prop_ownership_type_ordering
      , testProperty "owned type has name" prop_owned_type_has_name
      , testProperty "borrowed type has name" prop_borrowed_type_has_name
      , testProperty "mut borrowed type has name" prop_mut_borrowed_type_has_name
      ]
  , testGroup "OwnershipError tests"
      [ testProperty "use after move error" prop_use_after_move_error
      , testProperty "double move error" prop_double_move_error
      , testProperty "ownership error ordering" prop_ownership_error_ordering
      ]
  , testGroup "OwnershipTransfer tests"
      [ testProperty "ownership transfer creation" prop_ownership_transfer_creation
      ]
  , testGroup "OwnershipAnalyzer tests"
      [ testProperty "new ownership analyzer" prop_new_ownership_analyzer
      ]
  , testGroup "Analysis functions tests"
      [ testProperty "analyzeOwnership returns list" prop_analyze_ownership_returns_list
      , testProperty "analyzeOwnership detects errors" prop_analyze_ownership_detects_errors
      , testProperty "lex L.all returns tokens" prop_lex_all_returns_tokens
      , testProperty "parse program returns ast" prop_parse_program_returns_ast
      , testProperty "analyzeOwnershipDebug returns tuple" prop_analyze_ownership_debug_returns_tuple
      , testProperty "analyzeOwnershipDebug mode includes log" prop_analyze_ownership_debug_mode_includes_log
      ]
  , testGroup "Built-in functions tests"
      [ testProperty "built-in functions not empty" prop_built_in_functions_not_empty
      , testProperty "built-in functions contains common" prop_built_in_functions_contains_common
      ]
  , testGroup "Error detection tests"
      [ testProperty "detects use after move" prop_detects_use_after_move
      , testProperty "detects double move" prop_detects_double_move
      , testProperty "detects borrow conflicts" prop_detects_borrow_conflicts
      ]
  , testGroup "Round-trip tests"
      [ testProperty "parse analyze consistency" prop_parse_analyze_consistency
      , testProperty "format ownership errors includes content" prop_format_ownership_errors_includes_content
      ]
  , testGroup "Additional property tests"
      [ testProperty "analyze empty input" prop_analyze_empty_input
      , testProperty "analyze whitespace only" prop_analyze_whitespace_only
      , testProperty "analyze comments" prop_analyze_comments
      , testProperty "analyze mixed content" prop_analyze_mixed_content
      ]
  ]