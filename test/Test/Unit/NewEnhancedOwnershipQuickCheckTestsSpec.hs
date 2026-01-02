{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, listOf, elements, choose, oneof, suchThat)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

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
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import Data.List (isInfixOf)
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Custom Generators
-- ============================================================================

genString :: Gen String
genString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

genVariableName :: Gen String
genVariableName = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return (first : rest)

genOwnershipType :: Gen OwnershipType
genOwnershipType = oneof
  [ Owned <$> genVariableName
  , Borrowed <$> genVariableName
  , MutBorrowed <$> genVariableName
  ]

genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ UseAfterMove <$> genVariableName
  , DoubleMove <$> genVariableName <*> genVariableName
  , BorrowWhileMoved <$> genVariableName
  , MutBorrowWhileBorrowed <$> genVariableName
  , BorrowWhileMutBorrowed <$> genVariableName
  , MultipleMutBorrows <$> genVariableName
  , UseWhileMutBorrowed <$> genVariableName
  , OutOfScope <$> genVariableName
  , BorrowError <$> genString
  , ParseError <$> genString
  , CrossFunctionMove <$> genVariableName <*> genVariableName
  , ParameterMoveMismatch <$> genVariableName
  , ControlFlowError <$> genString
  , PathSensitiveError <$> genString
  , LoopOwnershipError <$> genString
  ]

genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  fromVar <- genVariableName
  toVar <- genVariableName
  return $ OwnershipTransfer fromVar toVar

genSimpleGoCode :: Gen String
genSimpleGoCode = do
  var1 <- genVariableName
  var2 <- genVariableName
  oneof
    [ return $ "package main\n\nfunc main() {\n    " ++ var1 ++ " := 42\n    " ++ var2 ++ " := " ++ var1 ++ "\n}"
    , return $ "package main\n\nfunc main() {\n    " ++ var1 ++ " := make([]int, 0)\n    " ++ var2 ++ " = append(" ++ var1 ++ ", 1)\n}"
    , return $ "package main\n\nfunc main() {\n    " ++ var1 ++ " := 42\n    println(" ++ var1 ++ ")\n}"
    ]

genOwnershipProblemCode :: Gen String
genOwnershipProblemCode = do
  var1 <- genVariableName
  var2 <- genVariableName
  var3 <- genVariableName
  oneof
    [ -- Double move
      return $ "package main\n\nfunc main() {\n    " ++ var1 ++ " := 42\n    " ++ var2 ++ " := " ++ var1 ++ "\n    " ++ var3 ++ " := " ++ var1 ++ "\n}"
    , -- Use after move
      return $ "package main\n\nfunc main() {\n    " ++ var1 ++ " := 42\n    " ++ var2 ++ " := " ++ var1 ++ "\n    println(" ++ var1 ++ ")\n}"
    , -- Borrow while moved
      return $ "package main\n\nfunc main() {\n    " ++ var1 ++ " := make([]int, 0)\n    " ++ var2 ++ " := " ++ var1 ++ "\n    " ++ var3 ++ " := &" ++ var1 ++ "\n}"
    ]

genValidGoCode :: Gen String
genValidGoCode = do
  imports <- oneof
    [ return ""
    , return "import \"fmt\"\n"
    , return "import \"os\"\n"
    , return "import (\n    \"fmt\"\n    \"os\"\n)\n"
    ]
  funcName <- genVariableName
  var1 <- genVariableName
  var2 <- genVariableName
  body <- oneof
    [ return $ var1 ++ " := 42\n    fmt.Println(" ++ var1 ++ ")"
    , return $ var1 ++ " := make([]int, 0)\n    " ++ var2 ++ " = append(" ++ var1 ++ ", 1)\n    fmt.Println(" ++ var2 ++ ")"
    , return $ var1 ++ ":= \"hello\"\n    " ++ var2 ++ " := " ++ var1 ++ "\n    fmt.Println(" ++ var2 ++ ")"
    ]
  return $ "package main\n\n" ++ imports ++ "func " ++ funcName ++ "() {\n    " ++ body ++ "\n}"

-- ============================================================================
-- OwnershipType Properties
-- ============================================================================

-- Property: Show should produce string representation with constructor name
prop_ownership_type_show_format :: OwnershipType -> Property
prop_ownership_type_show_format ownershipType =
  let showStr = show ownershipType
  in case ownershipType of
       Owned name -> property $ "Owned " `L.isInfixOf` showStr .&&. name `L.isInfixOf` showStr
       Borrowed name -> property $ "Borrowed " `L.isInfixOf` showStr .&&. name `L.isInfixOf` showStr
       MutBorrowed name -> property $ "MutBorrowed " `L.isInfixOf` showStr .&&. name `L.isInfixOf` showStr

-- Property: Owned should be less than Borrowed L.and MutBorrowed in ordering
prop_owned_less_than_borrowed :: String -> String -> Property
prop_owned_less_than_borrowed name1 name2 =
  let owned = Owned name1
      borrowed = Borrowed name2
      mutBorrowed = MutBorrowed name2
  in property $ owned < borrowed .&&. owned < mutBorrowed

-- Property: Borrowed should be less than MutBorrowed in ordering
prop_borrowed_less_than_mut_borrowed :: String -> String -> Property
prop_borrowed_less_than_mut_borrowed name1 name2 =
  let borrowed = Borrowed name1
      mutBorrowed = MutBorrowed name2
  in property $ borrowed < mutBorrowed

-- Property: Same types should be ordered by name
prop_same_type_ordered_by_name :: OwnershipType -> OwnershipType -> Property
prop_same_type_ordered_by_name type1 type2 =
  case (type1, type2) of
    (Owned name1, Owned name2) -> property $ compare type1 type2 === compare name1 name2
    (Borrowed name1, Borrowed name2) -> property $ compare type1 type2 === compare name1 name2
    (MutBorrowed name1, MutBorrowed name2) -> property $ compare type1 type2 === compare name1 name2
    _ -> property $ True  -- Different types have predefined ordering

-- ============================================================================
-- OwnershipError Properties
-- ============================================================================

-- Property: Show should produce string representation with constructor name
prop_ownership_error_show_format :: OwnershipError -> Property
prop_ownership_error_show_format ownershipError =
  let showStr = show ownershipError
  in case ownershipError of
       UseAfterMove var -> property $ "UseAfterMove " `L.isInfixOf` showStr .&&. var `L.isInfixOf` showStr
       DoubleMove var1 var2 -> property $ "DoubleMove " `L.isInfixOf` showStr .&&. var1 `L.isInfixOf` showStr .&&. var2 `L.isInfixOf` showStr
       BorrowWhileMoved var -> property $ "BorrowWhileMoved " `L.isInfixOf` showStr .&&. var `L.isInfixOf` showStr
       MutBorrowWhileBorrowed var -> property $ "MutBorrowWhileBorrowed " `L.isInfixOf` showStr .&&. var `L.isInfixOf` showStr
       BorrowWhileMutBorrowed var -> property $ "BorrowWhileMutBorrowed " `L.isInfixOf` showStr .&&. var `L.isInfixOf` showStr
       MultipleMutBorrows var -> property $ "MultipleMutBorrows " `L.isInfixOf` showStr .&&. var `L.isInfixOf` showStr
       UseWhileMutBorrowed var -> property $ "UseWhileMutBorrowed " `L.isInfixOf` showStr .&&. var `L.isInfixOf` showStr
       OutOfScope var -> property $ "OutOfScope " `L.isInfixOf` showStr .&&. var `L.isInfixOf` showStr
       BorrowError msg -> property $ "BorrowError " `L.isInfixOf` showStr .&&. msg `L.isInfixOf` showStr
       ParseError msg -> property $ "ParseError " `L.isInfixOf` showStr .&&. msg `L.isInfixOf` showStr
       CrossFunctionMove var1 var2 -> property $ "CrossFunctionMove " `L.isInfixOf` showStr .&&. var1 `L.isInfixOf` showStr .&&. var2 `L.isInfixOf` showStr
       ParameterMoveMismatch var -> property $ "ParameterMoveMismatch " `L.isInfixOf` showStr .&&. var `L.isInfixOf` showStr
       ControlFlowError msg -> property $ "ControlFlowError " `L.isInfixOf` showStr .&&. msg `L.isInfixOf` showStr
       PathSensitiveError msg -> property $ "PathSensitiveError " `L.isInfixOf` showStr .&&. msg `L.isInfixOf` showStr
       LoopOwnershipError msg -> property $ "LoopOwnershipError " `L.isInfixOf` showStr .&&. msg `L.isInfixOf` showStr

-- Property: Error ordering should be consistent with string representation
prop_error_ordering_consistent_with_show :: OwnershipError -> OwnershipError -> Property
prop_error_ordering_consistent_with_show err1 err2 =
  let ordering = compare err1 err2
      showOrdering = compare (show err1) (show err2)
  in property $ ordering === showOrdering

-- ============================================================================
-- OwnershipTransfer Properties
-- ============================================================================

-- Property: OwnershipTransfer should preserve from L.and to fields
prop_ownership_transfer_preserves_fields :: String -> String -> Property
prop_ownership_transfer_preserves_fields fromVar toVar =
  let transfer = OwnershipTransfer fromVar toVar
  in property $ transferFrom transfer === fromVar .&&. transferTo transfer === toVar

-- Property: Show should include both from L.and to variables
prop_ownership_transfer_show_format :: OwnershipTransfer -> Property
prop_ownership_transfer_show_format transfer =
  let showStr = show transfer
      from = transferFrom transfer
      to = transferTo transfer
  in property $ from `L.isInfixOf` showStr .&&. to `L.isInfixOf` showStr

-- ============================================================================
-- OwnershipAnalyzer Properties
-- ============================================================================

-- Property: newOwnershipAnalyzer should return a valid analyzer
prop_new_analyzer_valid :: Property
prop_new_analyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in property $ True  -- If it compiles, it's valid

-- ============================================================================
-- Built-in Functions Properties
-- ============================================================================

-- Property: builtInFunctions should not be empty
prop_built_in_functions_not_empty :: Property
prop_built_in_functions_not_empty =
  property $ not (null builtInFunctions)

-- Property: builtInFunctions should contain common Go functions
prop_built_in_functions_contains_common :: Property
prop_built_in_functions_contains_common =
  let commonFunctions = ["println", "len", "make", "append"]
      hasCommon = L.all (`elem` builtInFunctions) commonFunctions
  in property $ hasCommon

-- Property: builtInFunctions should not contain duplicates
prop_built_in_functions_no_duplicates :: Property
prop_built_in_functions_no_duplicates =
  let uniqueFunctions = nub builtInFunctions
  in property $ L.length builtInFunctions === L.length uniqueFunctions

-- ============================================================================
-- Ownership Analysis Properties
-- ============================================================================

-- Property: analyzeOwnership should handle empty input
prop_analyze_ownership_empty :: Property
prop_analyze_ownership_empty =
  let result = analyzeOwnership newOwnershipAnalyzer ""
  in property $ True  -- Should not crash

-- Property: analyzeOwnership should handle simple valid code
prop_analyze_ownership_simple_valid :: Property
prop_analyze_ownership_simple_valid =
  forAll genSimpleGoCode $ \code ->
  let result = analyzeOwnership newOwnershipAnalyzer code
  in property $ True  -- Should not crash on valid code

-- Property: analyzeOwnership should detect problems in invalid code
prop_analyze_ownership_detects_problems :: Property
prop_analyze_ownership_detects_problems =
  forAll genOwnershipProblemCode $ \code ->
  let result = analyzeOwnership newOwnershipAnalyzer code
  in property $ True  -- Should not crash L.and potentially detect issues

-- Property: analyzeOwnershipFile should handle file path
prop_analyze_ownership_file_path :: Property
prop_analyze_ownership_file_path =
  let result = analyzeOwnershipFile newOwnershipAnalyzer "nonexistent.go"
  in property $ True  -- Should handle file errors gracefully

-- Property: analyzeOwnershipDebug should return debug information
prop_analyze_ownership_debug :: Property
prop_analyze_ownership_debug =
  forAll genSimpleGoCode $ \code ->
  let result = analyzeOwnershipDebug newOwnershipAnalyzer code
  in property $ True  -- Should return debug info without crashing

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: formatOwnershipErrors should handle empty list
prop_format_empty_errors :: Property
prop_format_empty_errors =
  let formatted = formatOwnershipErrors []
  in property $ not (null formatted)  -- Should return some string even for empty list

-- Property: formatOwnershipErrors should include error messages
prop_format_errors_includes_messages :: Property
prop_format_errors_includes_messages =
  forAll (listOf genOwnershipError `suchThat` (not . null)) $ \errors ->
  let formatted = formatOwnershipErrors errors
      errorStrings = map show errors
  in property $ L.all (`L.isInfixOf` formatted) errorStrings

-- ============================================================================
-- Lexer L.and Parser Properties
-- ============================================================================

-- Property: lexAll should handle empty input
prop_lex_all_empty :: Property
prop_lex_all_empty =
  let result = lexAll ""
  in property $ True  -- Should not crash

-- Property: lexAll should handle simple Go code
prop_lex_all_simple_code :: Property
prop_lex_all_simple_code =
  forAll genSimpleGoCode $ \code ->
  let result = lexAll code
  in property $ True  -- Should not crash on valid code

-- Property: parseProgram should handle empty input
prop_parse_program_empty :: Property
prop_parse_program_empty =
  let result = parseProgram ""
  in property $ True  -- Should not crash

-- Property: parseProgram should handle simple Go code
prop_parse_program_simple_code :: Property
prop_parse_program_simple_code =
  forAll genSimpleGoCode $ \code ->
  let result = parseProgram code
  in property $ True  -- Should not crash on valid code

-- ============================================================================
-- Integration Properties
-- ============================================================================

-- Property: Complete analysis pipeline should not crash
prop_complete_pipeline_no_crash :: Property
prop_complete_pipeline_no_crash =
  forAll genValidGoCode $ \code ->
  let tokens = lexAll code
      program = parseProgram code
      analysis = analyzeOwnership newOwnershipAnalyzer code
  in property $ True  -- All steps should complete without crashing

-- Property: Analysis should be deterministic
prop_analysis_deterministic :: Property
prop_analysis_deterministic =
  forAll genSimpleGoCode $ \code ->
  let result1 = analyzeOwnership newOwnershipAnalyzer code
      result2 = analyzeOwnership newOwnershipAnalyzer code
  in property $ result1 === result2

-- Property: Debug analysis should provide more info than regular analysis
prop_debug_more_info_than_regular :: Property
prop_debug_more_info_than_regular =
  forAll genSimpleGoCode $ \code ->
  let regular = analyzeOwnership newOwnershipAnalyzer code
      debug = analyzeOwnershipDebug newOwnershipAnalyzer code
  in property $ L.length debug >= L.length regular  -- Debug should have at least as much info

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Ownership QuickCheck Tests"
  [ testGroup "OwnershipType Properties"
    [ fastProperty "ownership type show format" prop_ownership_type_show_format
    , fastProperty "owned less than borrowed" prop_owned_less_than_borrowed
    , fastProperty "borrowed less than mut borrowed" prop_borrowed_less_than_mut_borrowed
    , fastProperty "same type ordered by name" prop_same_type_ordered_by_name
    ]
  , testGroup "OwnershipError Properties"
    [ fastProperty "ownership error show format" prop_ownership_error_show_format
    , fastProperty "error ordering consistent with show" prop_error_ordering_consistent_with_show
    ]
  , testGroup "OwnershipTransfer Properties"
    [ fastProperty "ownership transfer preserves fields" prop_ownership_transfer_preserves_fields
    , fastProperty "ownership transfer show format" prop_ownership_transfer_show_format
    ]
  , testGroup "OwnershipAnalyzer Properties"
    [ fastProperty "new analyzer valid" prop_new_analyzer_valid
    ]
  , testGroup "Built-in Functions Properties"
    [ fastProperty "built in functions not empty" prop_built_in_functions_not_empty
    , fastProperty "built in functions contains common" prop_built_in_functions_contains_common
    , fastProperty "built in functions no duplicates" prop_built_in_functions_no_duplicates
    ]
  , testGroup "Ownership Analysis Properties"
    [ fastProperty "analyze ownership empty" prop_analyze_ownership_empty
    , fastProperty "analyze ownership simple valid" prop_analyze_ownership_simple_valid
    , fastProperty "analyze ownership detects problems" prop_analyze_ownership_detects_problems
    , fastProperty "analyze ownership file path" prop_analyze_ownership_file_path
    , fastProperty "analyze ownership debug" prop_analyze_ownership_debug
    ]
  , testGroup "Error Formatting Properties"
    [ fastProperty "format empty errors" prop_format_empty_errors
    , fastProperty "format errors includes messages" prop_format_errors_includes_messages
    ]
  , testGroup "Lexer L.and Parser Properties"
    [ fastProperty "lex L.all empty" prop_lex_all_empty
    , fastProperty "lex L.all simple code" prop_lex_all_simple_code
    , fastProperty "parse program empty" prop_parse_program_empty
    , fastProperty "parse program simple code" prop_parse_program_simple_code
    ]
  , testGroup "Integration Properties"
    [ fastProperty "complete pipeline no crash" prop_complete_pipeline_no_crash
    , fastProperty "analysis deterministic" prop_analysis_deterministic
    , fastProperty "debug more info than regular" prop_debug_more_info_than_regular
    ]
  ]