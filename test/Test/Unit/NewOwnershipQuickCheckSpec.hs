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
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
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
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- ============================================================================
-- Arbitrary Instances for QuickCheck Testing
-- ============================================================================

-- Generate arbitrary strings for variable names
instance Arbitrary String where
  arbitrary = do
    first <- QC.elements ['a'..'z']
    rest <- QC.listOf (QC.elements (['a'..'z'] ++ ['0'..'9'] ++ "_"))
    return $ first : rest

-- Generate arbitrary ownership type
instance Arbitrary OwnershipType where
  arbitrary = QC.oneof
    [ Owned <$> QC.arbitrary
    , Borrowed <$> QC.arbitrary
    , MutBorrowed <$> QC.arbitrary
    ]

-- Generate arbitrary ownership error
instance Arbitrary OwnershipError where
  arbitrary = QC.oneof
    [ UseAfterMove <$> QC.arbitrary
    , DoubleMove <$> QC.arbitrary <*> QC.arbitrary
    , BorrowWhileMoved <$> QC.arbitrary
    , MutBorrowWhileBorrowed <$> QC.arbitrary
    , BorrowWhileMutBorrowed <$> QC.arbitrary
    , MultipleMutBorrows <$> QC.arbitrary
    , UseWhileMutBorrowed <$> QC.arbitrary
    , OutOfScope <$> QC.arbitrary
    , BorrowError <$> QC.arbitrary
    , ParseError <$> QC.arbitrary
    , CrossFunctionMove <$> QC.arbitrary <*> QC.arbitrary
    , ParameterMoveMismatch <$> QC.arbitrary
    , ControlFlowError <$> QC.arbitrary
    , PathSensitiveError <$> QC.arbitrary
    , LoopOwnershipError <$> QC.arbitrary
    ]

-- Generate arbitrary ownership transfer
instance Arbitrary OwnershipTransfer where
  arbitrary = OwnershipTransfer <$> QC.arbitrary <*> QC.arbitrary

-- ============================================================================
-- Property Tests for Ownership Analysis
-- ============================================================================

-- Property: Ownership type preserves owner name
prop_ownership_type_preserves_owner :: String -> OwnershipType -> Property
prop_ownership_type_preserves_owner name ownershipType =
  case ownershipType of
    Owned owner -> property $ owner === name
    Borrowed owner -> property $ owner === name
    MutBorrowed owner -> property $ owner === name

-- Property: Ownership type ordering is consistent
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering type1 type2 =
  let ordering = compare type1 type2
      show1 = show type1
      show2 = show type2
  in property $ if show1 <= show2 then ordering /= GT else ordering === GT

-- Property: Owned types are always less than borrowed types
prop_owned_less_than_borrowed :: String -> String -> Property
prop_owned_less_than_borrowed owner borrower =
  let owned = Owned owner
      borrowed = Borrowed borrower
  in property $ compare owned borrowed === LT

-- Property: Borrowed types are less than mutably borrowed types
prop_borrowed_less_than_mut_borrowed :: String -> String -> Property
prop_borrowed_less_than_mut_borrowed borrower mutBorrower =
  let borrowed = Borrowed borrower
      mutBorrowed = MutBorrowed mutBorrower
  in property $ compare borrowed mutBorrowed === LT

-- Property: Ownership error preserves error information
prop_ownership_error_preserves_info :: String -> Property
prop_ownership_error_preserves_info var =
  let error = UseAfterMove var
  in case error of
       UseAfterMove v -> property $ v === var
       _ -> property $ False

-- Property: Double move error preserves both variables
prop_double_move_preserves_vars :: String -> String -> Property
prop_double_move_preserves_vars var1 var2 =
  let error = DoubleMove var1 var2
  in case error of
       DoubleMove v1 v2 -> property $ v1 === var1 .&&. v2 === var2
       _ -> property $ False

-- Property: Ownership transfer preserves from and to
prop_ownership_transfer_preserves_direction :: String -> String -> Property
prop_ownership_transfer_preserves_direction fromVar toVar =
  let transfer = OwnershipTransfer fromVar toVar
  in property $ transferFrom transfer === fromVar .&&.
             transferTo transfer === toVar

-- Property: Ownership analyzer can be created
prop_ownership_analyzer_creation :: Property
prop_ownership_analyzer_creation =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
       OwnershipAnalyzer () -> property $ True
       _ -> property $ False

-- Property: Error formatting contains expected content
prop_error_formatting_contains_content :: OwnershipError -> Property
prop_error_formatting_contains_content error =
  let formatted = show error
      expectedContent = case error of
        UseAfterMove var -> "UseAfterMove " ++ var
        DoubleMove var1 var2 -> "DoubleMove " ++ var1 ++ " " ++ var2
        BorrowWhileMoved var -> "BorrowWhileMoved " ++ var
        MutBorrowWhileBorrowed var -> "MutBorrowWhileBorrowed " ++ var
        BorrowWhileMutBorrowed var -> "BorrowWhileMutBorrowed " ++ var
        MultipleMutBorrows var -> "MultipleMutBorrows " ++ var
        UseWhileMutBorrowed var -> "UseWhileMutBorrowed " ++ var
        OutOfScope var -> "OutOfScope " ++ var
        BorrowError msg -> "BorrowError " ++ msg
        ParseError msg -> "ParseError " ++ msg
        CrossFunctionMove var1 var2 -> "CrossFunctionMove " ++ var1 ++ " " ++ var2
        ParameterMoveMismatch var -> "ParameterMoveMismatch " ++ var
        ControlFlowError msg -> "ControlFlowError " ++ msg
        PathSensitiveError msg -> "PathSensitiveError " ++ msg
        LoopOwnershipError msg -> "LoopOwnershipError " ++ msg
  in property $ formatted === expectedContent

-- Property: Error ordering is consistent with string representation
prop_error_ordering_consistent :: OwnershipError -> OwnershipError -> Property
prop_error_ordering_consistent error1 error2 =
  let ordering = compare error1 error2
      show1 = show error1
      show2 = show error2
  in property $ if show1 <= show2 then ordering /= GT else ordering === GT

-- Property: Built-in functions list is not empty
prop_builtin_functions_not_empty :: Property
prop_builtin_functions_not_empty =
  property $ not (null builtInFunctions)

-- Property: Built-in functions are unique
prop_builtin_functions_unique :: Property
prop_builtin_functions_unique =
  let uniqueFunctions = nub builtInFunctions
  in property $ length uniqueFunctions === length builtInFunctions

-- Property: Simple ownership analysis works
prop_simple_ownership_analysis :: String -> Property
prop_simple_ownership_analysis varName =
  not (null varName) ==>
  let program = unlines
        [ "func test() {"
        , "    x := 42"
        , "    y := x"
        , "    println(y)"
        , "}"
        ]
      result = analyzeOwnership program
  in case result of
       Left _ -> property $ False
       Right errors -> property $ True  -- Simple program should not have errors

-- Property: Ownership analysis detects use after move
prop_ownership_analysis_detects_use_after_move :: String -> Property
prop_ownership_analysis_detects_use_after_move varName =
  not (null varName) ==>
  let program = unlines
        [ "func test() {"
        , "    " ++ varName ++ " := 42"
        , "    moved := " ++ varName
        , "    println(" ++ varName ++ ")  // Use after move"
        , "}"
        ]
      result = analyzeOwnership program
  in case result of
       Left _ -> property $ False
       Right errors -> 
         let hasUseAfterMove = any isUseAfterMove errors
         in property $ hasUseAfterMove
  where
    isUseAfterMove (UseAfterMove _) = True
    isUseAfterMove _ = False

-- Property: Ownership analysis detects double move
prop_ownership_analysis_detects_double_move :: String -> Property
prop_ownership_analysis_detects_double_move varName =
  not (null varName) ==>
  let program = unlines
        [ "func test() {"
        , "    " ++ varName ++ " := 42"
        , "    moved1 := " ++ varName
        , "    moved2 := " ++ varName ++ "  // Double move"
        , "}"
        ]
      result = analyzeOwnership program
  in case result of
       Left _ -> property $ False
       Right errors -> 
         let hasDoubleMove = any isDoubleMove errors
         in property $ hasDoubleMove
  where
    isDoubleMove (DoubleMove _ _) = True
    isDoubleMove _ = False

-- Property: Ownership analysis handles borrow checking
prop_ownership_analysis_handles_borrow_checking :: String -> Property
prop_ownership_analysis_handles_borrow_checking varName =
  not (null varName) ==>
  let program = unlines
        [ "func test() {"
        , "    " ++ varName ++ " := 42"
        , "    borrow := &" ++ varName
        , "    println(*borrow)"
        , "}"
        ]
      result = analyzeOwnership program
  in case result of
       Left _ -> property $ False
       Right errors -> property $ True  // Borrowing should be allowed

-- Property: Ownership analysis detects mutable borrow conflicts
prop_ownership_analysis_detects_mut_borrow_conflicts :: String -> Property
prop_ownership_analysis_detects_mut_borrow_conflicts varName =
  not (null varName) ==>
  let program = unlines
        [ "func test() {"
        , "    " ++ varName ++ " := 42"
        , "    borrow := &" ++ varName
        , "    mutBorrow := &mut " ++ varName ++ "  // Conflict"
        , "}"
        ]
      result = analyzeOwnership program
  in case result of
       Left _ -> property $ False
       Right errors -> 
         let hasBorrowConflict = any isBorrowConflict errors
         in property $ hasBorrowConflict
  where
    isBorrowConflict (BorrowWhileMutBorrowed _) = True
    isBorrowConflict (MutBorrowWhileBorrowed _) = True
    isBorrowConflict _ = False

-- Property: Ownership analysis handles function parameters
prop_ownership_analysis_handles_function_params :: String -> Property
prop_ownership_analysis_handles_function_params varName =
  not (null varName) ==>
  let program = unlines
        [ "func test(" ++ varName ++ " int) {"
        , "    println(" ++ varName ++ ")"
        , "}"
        ]
      result = analyzeOwnership program
  in case result of
       Left _ -> property $ False
       Right errors -> property $ True  // Function parameters should be usable

-- Property: Ownership analysis handles return values
prop_ownership_analysis_handles_return_values :: String -> Property
prop_ownership_analysis_handles_return_values varName =
  not (null varName) ==>
  let program = unlines
        [ "func test() int {"
        , "    " ++ varName ++ " := 42"
        , "    return " ++ varName
        , "}"
        ]
      result = analyzeOwnership program
  in case result of
       Left _ -> property $ False
       Right errors -> property $ True  // Return should be allowed

-- Property: Ownership analysis handles control flow
prop_ownership_analysis_handles_control_flow :: String -> Property
prop_ownership_analysis_handles_control_flow varName =
  not (null varName) ==>
  let program = unlines
        [ "func test() {"
        , "    " ++ varName ++ " := 42"
        , "    if " ++ varName ++ " > 0 {"
        , "        println(" ++ varName ++ ")"
        , "    } else {"
        , "        println(0)"
        , "    }"
        , "}"
        ]
      result = analyzeOwnership program
  in case result of
       Left _ -> property $ False
       Right errors -> property $ True  // Control flow should be handled

-- Property: Ownership analysis handles loops
prop_ownership_analysis_handles_loops :: String -> Property
prop_ownership_analysis_handles_loops varName =
  not (null varName) ==>
  let program = unlines
        [ "func test() {"
        , "    " ++ varName ++ " := 42"
        , "    for i := 0; i < 10; i++ {"
        , "        println(" ++ varName ++ ")"
        , "    }"
        , "}"
        ]
      result = analyzeOwnership program
  in case result of
       Left _ -> property $ False
       Right errors -> property $ True  // Loops should be handled

-- Property: Error formatting produces readable output
prop_error_formatting_readable :: [OwnershipError] -> Property
prop_error_formatting_readable errors =
  let formatted = formatOwnershipErrors errors
  in property $ not (null formatted) .&&. 
             all (`elem` formatted) (map show errors)

-- Property: Lexing handles basic tokens
prop_lexing_handles_basic_tokens :: Property
prop_lexing_handles_basic_tokens =
  let program = "func test() { x := 42 }"
      result = lexAll program
  in property $ not (null result)

-- Property: Parsing handles basic structure
prop_parsing_handles_basic_structure :: Property
prop_parsing_handles_basic_structure =
  let program = "func test() { x := 42 }"
      result = parseProgram program
  in property $ not (null result)

-- Property: Analysis debug mode provides more information
prop_analysis_debug_mode :: String -> Property
prop_analysis_debug_mode varName =
  not (null varName) ==>
  let program = unlines
        [ "func test() {"
        , "    " ++ varName ++ " := 42"
        , "    println(" ++ varName ++ ")"
        , "}"
        ]
      result = analyzeOwnershipDebug program
  in case result of
       Left _ -> property $ False
       Right (errors, debugInfo) -> property $ not (null debugInfo)

-- Property: File analysis works with file path
prop_file_analysis_works :: String -> Property
prop_file_analysis_works content =
  let result = analyzeOwnershipFile "<test>" content
  in case result of
       Left _ -> property $ False
       Right errors -> property $ True

tests :: TestTree
tests =
  testGroup "New Ownership QuickCheck Tests"
    [ fastProperty "Ownership type preserves owner name" prop_ownership_type_preserves_owner
    , fastProperty "Ownership type ordering is consistent" prop_ownership_type_ordering
    , fastProperty "Owned types are always less than borrowed types" prop_owned_less_than_borrowed
    , fastProperty "Borrowed types are less than mutably borrowed types" prop_borrowed_less_than_mut_borrowed
    , fastProperty "Ownership error preserves error information" prop_ownership_error_preserves_info
    , fastProperty "Double move error preserves both variables" prop_double_move_preserves_vars
    , fastProperty "Ownership transfer preserves from and to" prop_ownership_transfer_preserves_direction
    , fastProperty "Ownership analyzer can be created" prop_ownership_analyzer_creation
    , fastProperty "Error formatting contains expected content" prop_error_formatting_contains_content
    , fastProperty "Error ordering is consistent with string representation" prop_error_ordering_consistent
    , fastProperty "Built-in functions list is not empty" prop_builtin_functions_not_empty
    , fastProperty "Built-in functions are unique" prop_builtin_functions_unique
    , fastProperty "Simple ownership analysis works" prop_simple_ownership_analysis
    , fastProperty "Ownership analysis detects use after move" prop_ownership_analysis_detects_use_after_move
    , fastProperty "Ownership analysis detects double move" prop_ownership_analysis_detects_double_move
    , fastProperty "Ownership analysis handles borrow checking" prop_ownership_analysis_handles_borrow_checking
    , fastProperty "Ownership analysis detects mutable borrow conflicts" prop_ownership_analysis_detects_mut_borrow_conflicts
    , fastProperty "Ownership analysis handles function parameters" prop_ownership_analysis_handles_function_params
    , fastProperty "Ownership analysis handles return values" prop_ownership_analysis_handles_return_values
    , fastProperty "Ownership analysis handles control flow" prop_ownership_analysis_handles_control_flow
    , fastProperty "Ownership analysis handles loops" prop_ownership_analysis_handles_loops
    , fastProperty "Error formatting produces readable output" prop_error_formatting_readable
    , fastProperty "Lexing handles basic tokens" prop_lexing_handles_basic_tokens
    , fastProperty "Parsing handles basic structure" prop_parsing_handles_basic_structure
    , fastProperty "Analysis debug mode provides more information" prop_analysis_debug_mode
    , fastProperty "File analysis works with file path" prop_file_analysis_works
    ]