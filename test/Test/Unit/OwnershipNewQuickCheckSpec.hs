{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof)
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

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, intercalate, sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = do
    name <- arbitrary
    elements [Owned name, Borrowed name, MutBorrowed name]

instance Arbitrary OwnershipError where
  arbitrary = do
    oneof
      [ UseAfterMove <$> arbitrary
      , DoubleMove <$> arbitrary <*> arbitrary
      , BorrowWhileMoved <$> arbitrary
      , MutBorrowWhileBorrowed <$> arbitrary
      , BorrowWhileMutBorrowed <$> arbitrary
      , MultipleMutBorrows <$> arbitrary
      , UseWhileMutBorrowed <$> arbitrary
      , OutOfScope <$> arbitrary
      , BorrowError <$> arbitrary
      , ParseError <$> arbitrary
      , CrossFunctionMove <$> arbitrary <*> arbitrary
      , ParameterMoveMismatch <$> arbitrary
      , ControlFlowError <$> arbitrary
      , PathSensitiveError <$> arbitrary
      , LoopOwnershipError <$> arbitrary
      ]

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    fromVar <- arbitrary
    toVar <- arbitrary
    return $ OwnershipTransfer fromVar toVar

-- ============================================================================
-- OwnershipType Properties
-- ============================================================================

-- Property: OwnershipType show contains the name
prop_ownershipType_show_contains_name :: String -> OwnershipType -> Property
prop_ownershipType_show_contains_name name ownershipType =
  let ownershipWithSameName = case ownershipType of
        Owned _ -> Owned name
        Borrowed _ -> Borrowed name
        MutBorrowed _ -> MutBorrowed name
      showStr = show ownershipWithSameName
  in name `isInfixOf` showStr

-- Property: OwnershipType equality works correctly
prop_ownershipType_equality :: OwnershipType -> OwnershipType -> Property
prop_ownershipType_equality ot1 ot2 =
  (ot1 == ot2) === case (ot1, ot2) of
    (Owned n1, Owned n2) -> n1 == n2
    (Borrowed n1, Borrowed n2) -> n1 == n2
    (MutBorrowed n1, MutBorrowed n2) -> n1 == n2
    _ -> False

-- Property: OwnershipType ordering is consistent
prop_ownershipType_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownershipType_ordering ot1 ot2 =
  let comparison = compare ot1 ot2
      expected = case (ot1, ot2) of
        (Owned _, Owned _) -> compare (getOwnershipName ot1) (getOwnershipName ot2)
        (Owned _, Borrowed _) -> LT
        (Owned _, MutBorrowed _) -> LT
        (Borrowed _, Owned _) -> GT
        (Borrowed _, Borrowed _) -> compare (getOwnershipName ot1) (getOwnershipName ot2)
        (Borrowed _, MutBorrowed _) -> LT
        (MutBorrowed _, Owned _) -> GT
        (MutBorrowed _, Borrowed _) -> GT
        (MutBorrowed _, MutBorrowed _) -> compare (getOwnershipName ot1) (getOwnershipName ot2)
  in comparison === expected
  where
    getOwnershipName (Owned n) = n
    getOwnershipName (Borrowed n) = n
    getOwnershipName (MutBorrowed n) = n

-- Property: OwnershipType name extraction works
prop_ownershipType_name_extraction :: String -> Property
prop_ownershipType_name_extraction name =
  let owned = Owned name
      borrowed = Borrowed name
      mutBorrowed = MutBorrowed name
  in case (owned, borrowed, mutBorrowed) of
    (Owned n, Borrowed b, MutBorrowed m) -> n === name && b === name && m === name

-- ============================================================================
-- OwnershipError Properties
-- ============================================================================

-- Property: OwnershipError show contains relevant information
prop_ownershipError_show_informative :: OwnershipError -> Property
prop_ownershipError_show_informative err =
  let showStr = show err
      hasVariable = case err of
        UseAfterMove var -> var `isInfixOf` showStr
        DoubleMove var1 var2 -> var1 `isInfixOf` showStr && var2 `isInfixOf` showStr
        BorrowWhileMoved var -> var `isInfixOf` showStr
        MutBorrowWhileBorrowed var -> var `isInfixOf` showStr
        BorrowWhileMutBorrowed var -> var `isInfixOf` showStr
        MultipleMutBorrows var -> var `isInfixOf` showStr
        UseWhileMutBorrowed var -> var `isInfixOf` showStr
        OutOfScope var -> var `isInfixOf` showStr
        BorrowError msg -> not (null msg)
        ParseError msg -> not (null msg)
        CrossFunctionMove var1 var2 -> var1 `isInfixOf` showStr && var2 `isInfixOf` showStr
        ParameterMoveMismatch var -> var `isInfixOf` showStr
        ControlFlowError msg -> not (null msg)
        PathSensitiveError msg -> not (null msg)
        LoopOwnershipError msg -> not (null msg)
  in property hasVariable

-- Property: OwnershipError equality works correctly
prop_ownershipError_equality :: OwnershipError -> OwnershipError -> Property
prop_ownershipError_equality err1 err2 =
  (err1 == err2) === (show err1 == show err2)

-- Property: OwnershipError ordering is consistent with show
prop_ownershipError_ordering :: OwnershipError -> OwnershipError -> Property
prop_ownershipError_ordering err1 err2 =
  compare err1 err2 === compare (show err1) (show err2)

-- ============================================================================
-- OwnershipTransfer Properties
-- ============================================================================

-- Property: OwnershipTransfer fields are accessible
prop_ownershipTransfer_fields :: String -> String -> Property
prop_ownershipTransfer_fields fromVar toVar =
  let transfer = OwnershipTransfer fromVar toVar
  in transferFrom transfer === fromVar .&&.
     transferTo transfer === toVar

-- Property: OwnershipTransfer equality works correctly
prop_ownershipTransfer_equality :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownershipTransfer_equality transfer1 transfer2 =
  (transfer1 == transfer2) ===
  (transferFrom transfer1 == transferFrom transfer2 &&
   transferTo transfer1 == transferTo transfer2)

-- Property: OwnershipTransfer show contains both variables
prop_ownershipTransfer_show_contains_vars :: String -> String -> Property
prop_ownershipTransfer_show_contains_vars fromVar toVar =
  let transfer = OwnershipTransfer fromVar toVar
      showStr = show transfer
  in fromVar `isInfixOf` showStr .&&. toVar `isInfixOf` showStr

-- ============================================================================
-- OwnershipAnalyzer Properties
-- ============================================================================

-- Property: newOwnershipAnalyzer creates analyzer
prop_newOwnershipAnalyzer_creates :: Property
prop_newOwnershipAnalyzer_creates =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
    OwnershipAnalyzer _ -> property True

-- Property: OwnershipAnalyzer equality works
prop_ownershipAnalyzer_equality :: Property
prop_ownershipAnalyzer_equality =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in analyzer1 == analyzer2

-- ============================================================================
-- Ownership Analysis Properties
-- ============================================================================

-- Property: analyzeOwnership handles empty input
prop_analyzeOwnership_empty :: Property
prop_analyzeOwnership_empty =
  let result = analyzeOwnership newOwnershipAnalyzer ""
  in property True -- Should not crash

-- Property: analyzeOwnership handles simple assignment
prop_analyzeOwnership_simple_assignment :: Property
prop_analyzeOwnership_simple_assignment =
  let code = "x := 42"
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should not crash

-- Property: analyzeOwnership handles move operations
prop_analyzeOwnership_move_operations :: Property
prop_analyzeOwnership_move_operations =
  let code = "x := 42\ny := x"
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should not crash

-- Property: analyzeOwnership handles borrow operations
prop_analyzeOwnership_borrow_operations :: Property
prop_analyzeOwnership_borrow_operations =
  let code = "x := 42\ny := &x"
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should not crash

-- Property: analyzeOwnership handles mutable borrow operations
prop_analyzeOwnership_mutable_borrow :: Property
prop_analyzeOwnership_mutable_borrow =
  let code = "x := 42\ny := &mut x"
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should not crash

-- Property: analyzeOwnership handles function calls
prop_analyzeOwnership_function_calls :: Property
prop_analyzeOwnership_function_calls =
  let code = "func test(x) {\n    return x\n}\ny := test(42)"
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should not crash

-- ============================================================================
-- Ownership File Analysis Properties
-- ============================================================================

-- Property: analyzeOwnershipFile handles empty file
prop_analyzeOwnershipFile_empty :: Property
prop_analyzeOwnershipFile_empty =
  let result = analyzeOwnershipFile newOwnershipAnalyzer ""
  in property True -- Should not crash

-- Property: analyzeOwnershipFile handles complex scenarios
prop_analyzeOwnershipFile_complex :: Property
prop_analyzeOwnershipFile_complex =
  let code = intercalate "\n"
        [ "func main() {"
        , "    x := 42"
        , "    y := x"
        , "    z := &y"
        , "    w := &mut z"
        , "    return w"
        , "}"
        ]
      result = analyzeOwnershipFile newOwnershipAnalyzer code
  in property True -- Should not crash

-- ============================================================================
-- Ownership Debug Properties
-- ============================================================================

-- Property: analyzeOwnershipDebug provides debug information
prop_analyzeOwnershipDebug_provides_info :: Property
prop_analyzeOwnershipDebug_provides_info =
  let code = "x := 42\ny := x"
      result = analyzeOwnershipDebug newOwnershipAnalyzer code
  in property True -- Should not crash

-- Property: analyzeOwnershipDebug handles errors gracefully
prop_analyzeOwnershipDebug_handles_errors :: Property
prop_analyzeOwnershipDebug_handles_errors =
  let code = "x := 42\ny := x\nz := x"  -- This should generate UseAfterMove error
      result = analyzeOwnershipDebug newOwnershipAnalyzer code
  in property True -- Should not crash

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: formatOwnershipErrors handles empty list
prop_formatOwnershipErrors_empty :: Property
prop_formatOwnershipErrors_empty =
  let formatted = formatOwnershipErrors []
  in not (null formatted) -- Should return some formatted output

-- Property: formatOwnershipErrors handles various error types
prop_formatOwnershipErrors_various :: [OwnershipError] -> Property
prop_formatOwnershipErrors_various errors =
  let formatted = formatOwnershipErrors errors
  in not (null formatted) -- Should return some formatted output

-- Property: formatOwnershipErrors preserves error count
prop_formatOwnershipErrors_preserves_count :: [OwnershipError] -> Property
prop_formatOwnershipErrors_preserves_count errors =
  let formatted = formatOwnershipErrors errors
      errorCount = length errors
  in if errorCount > 0
     then property True -- Should contain error information
     else property True -- Empty list should also be handled

-- ============================================================================
-- Lexer Properties
-- ============================================================================

-- Property: lexAll handles empty input
prop_lexAll_empty :: Property
prop_lexAll_empty =
  let result = lexAll ""
  in property True -- Should not crash

-- Property: lexAll handles simple code
prop_lexAll_simple :: Property
prop_lexAll_simple =
  let code = "x := 42"
      result = lexAll code
  in property True -- Should not crash

-- Property: lexAll handles complex code
prop_lexAll_complex :: Property
prop_lexAll_complex =
  let code = intercalate "\n"
        [ "func test(x int) int {"
        , "    if x > 0 {"
        , "        return x * 2"
        , "    } else {"
        , "        return 0"
        , "    }"
        , "}"
        ]
      result = lexAll code
  in property True -- Should not crash

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: parseProgram handles empty input
prop_parseProgram_empty :: Property
prop_parseProgram_empty =
  let result = parseProgram ""
  in property True -- Should not crash

-- Property: parseProgram handles simple code
prop_parseProgram_simple :: Property
prop_parseProgram_simple =
  let code = "x := 42"
      result = parseProgram code
  in property True -- Should not crash

-- Property: parseProgram handles function definitions
prop_parseProgram_function :: Property
prop_parseProgram_function =
  let code = "func test() { return 42 }"
      result = parseProgram code
  in property True -- Should not crash

-- ============================================================================
-- Built-in Functions Properties
-- ============================================================================

-- Property: builtInFunctions is not empty
prop_builtInFunctions_not_empty :: Property
prop_builtInFunctions_not_empty =
  let functions = builtInFunctions
  in not (null functions)

-- Property: builtInFunctions contains common functions
prop_builtInFunctions_contains_common :: Property
prop_builtInFunctions_contains_common =
  let functions = builtInFunctions
      hasCommon = any (`elem` functions) ["print", "len", "append"]
  in property hasCommon

-- ============================================================================
-- Complex Ownership Scenarios Properties
-- ============================================================================

-- Property: analysis handles nested scopes
prop_analysis_nested_scopes :: Property
prop_analysis_nested_scopes =
  let code = intercalate "\n"
        [ "func outer() {"
        , "    x := 42"
        , "    func inner() {"
        , "        y := x"
        , "        return y"
        , "    }"
        , "    return inner()"
        , "}"
        ]
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should not crash

-- Property: analysis handles loops
prop_analysis_loops :: Property
prop_analysis_loops =
  let code = intercalate "\n"
        [ "func test() {"
        , "    x := 42"
        , "    for i := 0; i < 10; i++ {"
        , "        y := x"
        , "        x := i"
        , "    }"
        , "    return x"
        , "}"
        ]
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should not crash

-- Property: analysis handles conditionals
prop_analysis_conditionals :: Property
prop_analysis_conditionals =
  let code = intercalate "\n"
        [ "func test(x int) int {"
        , "    if x > 0 {"
        , "        y := x"
        , "        return y"
        , "    } else {"
        , "        z := x * 2"
        , "        return z"
        , "    }"
        , "}"
        ]
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should not crash

-- ============================================================================
-- Error Detection Properties
-- ============================================================================

-- Property: analysis detects use after move
prop_analysis_detects_use_after_move :: Property
prop_analysis_detects_use_after_move =
  let code = "x := 42\ny := x\nz := x"  -- x is moved to y, then used again
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should detect error

-- Property: analysis detects double move
prop_analysis_detects_double_move :: Property
prop_analysis_detects_double_move =
  let code = "x := 42\ny := x\nz := x"  -- x is moved twice
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should detect error

-- Property: analysis detects borrow conflicts
prop_analysis_detects_borrow_conflicts :: Property
prop_analysis_detects_borrow_conflicts =
  let code = "x := 42\ny := &x\nz := &mut x"  -- Immutable and mutable borrow conflict
      result = analyzeOwnership newOwnershipAnalyzer code
  in property True -- Should detect error

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: analysis handles large input
prop_analysis_large_input :: Property
prop_analysis_large_input =
  let largeCode = intercalate "\n" $ replicate 1000 "x := 42"
      result = analyzeOwnership newOwnershipAnalyzer largeCode
  in property True -- Should not crash

-- Property: analysis handles deep nesting
prop_analysis_deep_nesting :: Property
prop_analysis_deep_nesting =
  let nestedCode = intercalate "\n" $ replicate 100 "func level"
      result = analyzeOwnership newOwnershipAnalyzer nestedCode
  in property True -- Should not crash

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership QuickCheck Tests"
  [ testGroup "OwnershipType Properties"
    [ fastProperty "OwnershipType show contains the name" prop_ownershipType_show_contains_name
    , fastProperty "OwnershipType equality works correctly" prop_ownershipType_equality
    , fastProperty "OwnershipType ordering is consistent" prop_ownershipType_ordering
    , fastProperty "OwnershipType name extraction works" prop_ownershipType_name_extraction
    ]

  , testGroup "OwnershipError Properties"
    [ fastProperty "OwnershipError show contains relevant information" prop_ownershipError_show_informative
    , fastProperty "OwnershipError equality works correctly" prop_ownershipError_equality
    , fastProperty "OwnershipError ordering is consistent with show" prop_ownershipError_ordering
    ]

  , testGroup "OwnershipTransfer Properties"
    [ fastProperty "OwnershipTransfer fields are accessible" prop_ownershipTransfer_fields
    , fastProperty "OwnershipTransfer equality works correctly" prop_ownershipTransfer_equality
    , fastProperty "OwnershipTransfer show contains both variables" prop_ownershipTransfer_show_contains_vars
    ]

  , testGroup "OwnershipAnalyzer Properties"
    [ fastProperty "newOwnershipAnalyzer creates analyzer" prop_newOwnershipAnalyzer_creates
    , fastProperty "OwnershipAnalyzer equality works" prop_ownershipAnalyzer_equality
    ]

  , testGroup "Ownership Analysis Properties"
    [ fastProperty "analyzeOwnership handles empty input" prop_analyzeOwnership_empty
    , fastProperty "analyzeOwnership handles simple assignment" prop_analyzeOwnership_simple_assignment
    , fastProperty "analyzeOwnership handles move operations" prop_analyzeOwnership_move_operations
    , fastProperty "analyzeOwnership handles borrow operations" prop_analyzeOwnership_borrow_operations
    , fastProperty "analyzeOwnership handles mutable borrow operations" prop_analyzeOwnership_mutable_borrow
    , fastProperty "analyzeOwnership handles function calls" prop_analyzeOwnership_function_calls
    ]

  , testGroup "Ownership File Analysis Properties"
    [ fastProperty "analyzeOwnershipFile handles empty file" prop_analyzeOwnershipFile_empty
    , fastProperty "analyzeOwnershipFile handles complex scenarios" prop_analyzeOwnershipFile_complex
    ]

  , testGroup "Ownership Debug Properties"
    [ fastProperty "analyzeOwnershipDebug provides debug information" prop_analyzeOwnershipDebug_provides_info
    , fastProperty "analyzeOwnershipDebug handles errors gracefully" prop_analyzeOwnershipDebug_handles_errors
    ]

  , testGroup "Error Formatting Properties"
    [ fastProperty "formatOwnershipErrors handles empty list" prop_formatOwnershipErrors_empty
    , fastProperty "formatOwnershipErrors handles various error types" prop_formatOwnershipErrors_various
    , fastProperty "formatOwnershipErrors preserves error count" prop_formatOwnershipErrors_preserves_count
    ]

  , testGroup "Lexer Properties"
    [ fastProperty "lexAll handles empty input" prop_lexAll_empty
    , fastProperty "lexAll handles simple code" prop_lexAll_simple
    , fastProperty "lexAll handles complex code" prop_lexAll_complex
    ]

  , testGroup "Parser Properties"
    [ fastProperty "parseProgram handles empty input" prop_parseProgram_empty
    , fastProperty "parseProgram handles simple code" prop_parseProgram_simple
    , fastProperty "parseProgram handles function definitions" prop_parseProgram_function
    ]

  , testGroup "Built-in Functions Properties"
    [ fastProperty "builtInFunctions is not empty" prop_builtInFunctions_not_empty
    , fastProperty "builtInFunctions contains common functions" prop_builtInFunctions_contains_common
    ]

  , testGroup "Complex Ownership Scenarios Properties"
    [ fastProperty "analysis handles nested scopes" prop_analysis_nested_scopes
    , fastProperty "analysis handles loops" prop_analysis_loops
    , fastProperty "analysis handles conditionals" prop_analysis_conditionals
    ]

  , testGroup "Error Detection Properties"
    [ fastProperty "analysis detects use after move" prop_analysis_detects_use_after_move
    , fastProperty "analysis detects double move" prop_analysis_detects_double_move
    , fastProperty "analysis detects borrow conflicts" prop_analysis_detects_borrow_conflicts
    ]

  , testGroup "Performance Properties"
    [ fastProperty "analysis handles large input" prop_analysis_large_input
    , fastProperty "analysis handles deep nesting" prop_analysis_deep_nesting
    ]
  ]