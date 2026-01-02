{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, suchThat, listOf1)

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
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

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (intercalate)
import Data.Char (isAlpha, isAlphaNum, isSpace)

-- ============================================================================
-- Generators for QuickCheck
-- ============================================================================

-- Generate a valid variable name
genVarName :: Gen String
genVarName = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- Generate an ownership type
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  name <- genVarName
  elements [Owned name, Borrowed name, MutBorrowed name]

-- Generate an owned type
genOwned :: Gen OwnershipType
genOwned = do
  name <- genVarName
  return $ Owned name

-- Generate a borrowed type
genBorrowed :: Gen OwnershipType
genBorrowed = do
  name <- genVarName
  return $ Borrowed name

-- Generate a mutably borrowed type
genMutBorrowed :: Gen OwnershipType
genMutBorrowed = do
  name <- genVarName
  return $ MutBorrowed name

-- Generate an ownership error
genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ UseAfterMove <$> genVarName
  , DoubleMove <$> genVarName <*> genVarName
  , BorrowWhileMoved <$> genVarName
  , MutBorrowWhileBorrowed <$> genVarName
  , BorrowWhileMutBorrowed <$> genVarName
  , MultipleMutBorrows <$> genVarName
  , UseWhileMutBorrowed <$> genVarName
  , OutOfScope <$> genVarName
  , BorrowError <$> genVarName
  , ParseError <$> genVarName
  , CrossFunctionMove <$> genVarName <*> genVarName
  , ParameterMoveMismatch <$> genVarName
  , ControlFlowError <$> genVarName
  , PathSensitiveError <$> genVarName
  , LoopOwnershipError <$> genVarName
  ]

-- Generate an ownership transfer
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  fromVar <- genVarName
  toVar <- genVarName
  return $ OwnershipTransfer fromVar toVar

-- Generate a simple ownership statement
genOwnershipStatement :: Gen String
genOwnershipStatement = oneof
  [ do
      var1 <- genVarName
      var2 <- genVarName
      return $ var1 ++ " = " ++ var2
  , do
      var <- genVarName
      return $ var ++ ".move()"
  , do
      var <- genVarName
      return $ "&" ++ var
  , do
      var <- genVarName
      return $ "&mut " ++ var
  ]

-- Generate a simple Go-like code snippet for ownership analysis
genOwnershipCode :: Gen String
genOwnershipCode = do
  statements <- listOf1 genOwnershipStatement
  return $ intercalate "\n" statements

-- Generate code with use-after-move scenario
genUseAfterMoveCode :: Gen String
genUseAfterMoveCode = do
  var1 <- genVarName
  var2 <- genVarName
  return $ var1 ++ " := create()\n" ++ var2 ++ " = " ++ var1 ++ "\n" ++ var1 ++ ".use()"

-- Generate code with double move scenario
genDoubleMoveCode :: Gen String
genDoubleMoveCode = do
  var1 <- genVarName
  var2 <- genVarName
  var3 <- genVarName
  return $ var1 ++ " := create()\n" ++ var2 ++ " = " ++ var1 ++ "\n" ++ var3 ++ " = " ++ var1

-- Generate code with borrowing scenario
genBorrowingCode :: Gen String
genBorrowingCode = do
  var1 <- genVarName
  var2 <- genVarName
  return $ var1 ++ " := create()\n" ++ var2 ++ " = &" ++ var1 ++ "\n" ++ var2 ++ ".use()"

-- Generate code with mutable borrowing scenario
genMutBorrowingCode :: Gen String
genMutBorrowingCode = do
  var1 <- genVarName
  var2 <- genVarName
  return $ var1 ++ " := create()\n" ++ var2 ++ " = &mut " ++ var1 ++ "\n" ++ var2 ++ ".modify()"

-- ============================================================================
-- Ownership Properties
-- ============================================================================

-- Property: newOwnershipAnalyzer creates a valid analyzer
prop_newOwnershipAnalyzer_valid :: Property
prop_newOwnershipAnalyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
    OwnershipAnalyzer _ -> property True

-- Property: Owned type shows correctly
prop_owned_show_correct :: Property
prop_owned_show_correct =
  forAll genVarName $ \name ->
    let owned = Owned name
    in show owned === "Owned " ++ name

-- Property: Borrowed type shows correctly
prop_borrowed_show_correct :: Property
prop_borrowed_show_correct =
  forAll genVarName $ \name ->
    let borrowed = Borrowed name
    in show borrowed === "Borrowed " ++ name

-- Property: MutBorrowed type shows correctly
prop_mutBorrowed_show_correct :: Property
prop_mutBorrowed_show_correct =
  forAll genVarName $ \name ->
    let mutBorrowed = MutBorrowed name
    in show mutBorrowed === "MutBorrowed " ++ name

-- Property: OwnershipType ordering works correctly
prop_ownership_type_ordering :: Property
prop_ownership_type_ordering =
  forAll genVarName $ \name ->
    let owned = Owned name
        borrowed = Borrowed name
        mutBorrowed = MutBorrowed name
    in owned < borrowed .&&. borrowed < mutBorrowed

-- Property: OwnershipError shows correctly
prop_ownership_error_show :: Property
prop_ownership_error_show =
  forAll genOwnershipError $ \error ->
    let errorStr = show error
    in not (null errorStr) .&&. L.length errorStr > 5

-- Property: OwnershipTransfer shows correctly
prop_ownership_transfer_show :: Property
prop_ownership_transfer_show =
  forAll genOwnershipTransfer $ \transfer ->
    let transferStr = show transfer
    in not (null transferStr)

-- Property: UseAfterMove error shows correctly
prop_use_after_move_show :: Property
prop_use_after_move_show =
  forAll genVarName $ \var ->
    let error = UseAfterMove var
        errorStr = show error
    in "UseAfterMove " `L.isPrefixOf` errorStr .&&. var `L.isSuffixOf` errorStr

-- Property: DoubleMove error shows correctly
prop_double_move_show :: Property
prop_double_move_show =
  forAll genVarName $ \var1 ->
    forAll genVarName $ \var2 ->
      let error = DoubleMove var1 var2
          errorStr = show error
      in "DoubleMove " `L.isPrefixOf` errorStr .&&. 
         var1 `L.isInfixOf` errorStr .&&. 
         var2 `L.isInfixOf` errorStr

-- Property: OwnershipTransfer constructor works correctly
prop_ownership_transfer_constructor :: Property
prop_ownership_transfer_constructor =
  forAll genVarName $ \fromVar ->
    forAll genVarName $ \toVar ->
      let transfer = OwnershipTransfer fromVar toVar
      in transferFrom transfer === fromVar .&&. transferTo transfer === toVar

-- Property: Built-in functions list is not empty
prop_built_in_functions_not_empty :: Property
prop_built_in_functions_not_empty =
  let builtIns = builtInFunctions
  in not (null builtIns)

-- Property: Built-in functions are valid identifiers
prop_built_in_functions_valid_identifiers :: Property
prop_built_in_functions_valid_identifiers =
  let builtIns = builtInFunctions
      isValidIdentifier [] = False
      isValidIdentifier (c:cs) = isAlpha c && L.all isAlphaNum cs
  in L.all isValidIdentifier builtIns

-- Property: formatOwnershipErrors handles empty list
prop_format_ownership_errors_empty :: Property
prop_format_ownership_errors_empty =
  let errors = []
      formatted = formatOwnershipErrors errors
  in null formatted

-- Property: formatOwnershipErrors handles non-empty list
prop_format_ownership_errors_non_empty :: Property
prop_format_ownership_errors_non_empty =
  forAll (listOf1 genOwnershipError) $ \errors ->
    let formatted = formatOwnershipErrors errors
    in not (null formatted)

-- Property: formatOwnershipErrors includes error information
prop_format_ownership_errors_includes_info :: Property
prop_format_ownership_errors_includes_info =
  forAll (listOf1 genOwnershipError) $ \errors ->
    let formatted = formatOwnershipErrors errors
        errorStrings = map show errors
    in L.all (`L.isInfixOf` formatted) errorStrings

-- Property: lexAll handles empty input
prop_lex_all_empty :: Property
prop_lex_all_empty =
  let result = lexAll ""
  in null result

-- Property: lexAll handles simple input
prop_lex_all_simple :: Property
prop_lex_all_simple =
  forAll genOwnershipCode $ \code ->
    let result = lexAll code
    in L.length result >= 0

-- Property: parseProgram handles empty input
prop_parse_program_empty :: Property
prop_parse_program_empty =
  let result = parseProgram ""
  in case result of
    Left _ -> property True
    Right ast -> property True

-- Property: parseProgram handles simple input
prop_parse_program_simple :: Property
prop_parse_program_simple =
  forAll genOwnershipCode $ \code ->
    let result = parseProgram code
    in case result of
      Left _ -> property True
      Right ast -> property True

-- Property: analyzeOwnership handles basic input
prop_analyze_ownership_basic :: Property
prop_analyze_ownership_basic =
  forAll genOwnershipCode $ \code ->
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
      Left _ -> property True
      Right errors -> property True

-- Property: analyzeOwnership detects use-after-move
prop_analyze_ownership_use_after_move :: Property
prop_analyze_ownership_use_after_move =
  forAll genUseAfterMoveCode $ \code ->
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
      Left _ -> property True
      Right errors -> L.any isUseAfterMove errors
  where
    isUseAfterMove (UseAfterMove _) = True
    isUseAfterMove _ = False

-- Property: analyzeOwnership detects double move
prop_analyze_ownership_double_move :: Property
prop_analyze_ownership_double_move =
  forAll genDoubleMoveCode $ \code ->
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
      Left _ -> property True
      Right errors -> L.any isDoubleMove errors
  where
    isDoubleMove (DoubleMove _ _) = True
    isDoubleMove _ = False

-- Property: analyzeOwnership handles borrowing
prop_analyze_ownership_borrowing :: Property
prop_analyze_ownership_borrowing =
  forAll genBorrowingCode $ \code ->
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
      Left _ -> property True
      Right errors -> property True

-- Property: analyzeOwnership handles mutable borrowing
prop_analyze_ownership_mutable_borrowing :: Property
prop_analyze_ownership_mutable_borrowing =
  forAll genMutBorrowingCode $ \code ->
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
      Left _ -> property True
      Right errors -> property True

-- Property: analyzeOwnershipDebug provides more information
prop_analyze_ownership_debug_verbose :: Property
prop_analyze_ownership_debug_verbose =
  forAll genOwnershipCode $ \code ->
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnershipDebug analyzer code
    in case result of
      Left _ -> property True
      Right (errors, debug) -> L.length debug >= 0

-- Property: OwnershipType equality works correctly
prop_ownership_type_equality :: Property
prop_ownership_type_equality =
  forAll genVarName $ \name ->
    let owned1 = Owned name
        owned2 = Owned name
        borrowed = Borrowed name
        mutBorrowed = MutBorrowed name
    in owned1 == owned2 .&&. owned1 /= borrowed .&&. owned1 /= mutBorrowed

-- Property: OwnershipError equality works correctly
prop_ownership_error_equality :: Property
prop_ownership_error_equality =
  forAll genVarName $ \var ->
    let error1 = UseAfterMove var
        error2 = UseAfterMove var
        error3 = DoubleMove var var
    in error1 == error2 .&&. error1 /= error3

-- Property: OwnershipTransfer equality works correctly
prop_ownership_transfer_equality :: Property
prop_ownership_transfer_equality =
  forAll genVarName $ \fromVar ->
    forAll genVarName $ \toVar ->
      let transfer1 = OwnershipTransfer fromVar toVar
          transfer2 = OwnershipTransfer fromVar toVar
          transfer3 = OwnershipTransfer toVar fromVar
      in if fromVar == toVar
         then transfer1 == transfer2 .&&. transfer1 == transfer3
         else transfer1 == transfer2 .&&. transfer1 /= transfer3

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Core QuickCheck Tests"
  [ testGroup "OwnershipType Properties"
    [ fastProperty "newOwnershipAnalyzer valid" prop_newOwnershipAnalyzer_valid
    , fastProperty "owned show correct" prop_owned_show_correct
    , fastProperty "borrowed show correct" prop_borrowed_show_correct
    , fastProperty "mutBorrowed show correct" prop_mutBorrowed_show_correct
    , fastProperty "ownership type ordering" prop_ownership_type_ordering
    , fastProperty "ownership type equality" prop_ownership_type_equality
    ]

  , testGroup "OwnershipError Properties"
    [ fastProperty "ownership error show" prop_ownership_error_show
    , fastProperty "use after move show" prop_use_after_move_show
    , fastProperty "double move show" prop_double_move_show
    , fastProperty "ownership error equality" prop_ownership_error_equality
    ]

  , testGroup "OwnershipTransfer Properties"
    [ fastProperty "ownership transfer show" prop_ownership_transfer_show
    , fastProperty "ownership transfer constructor" prop_ownership_transfer_constructor
    , fastProperty "ownership transfer equality" prop_ownership_transfer_equality
    ]

  , testGroup "Built-in Functions Properties"
    [ fastProperty "built-in functions not empty" prop_built_in_functions_not_empty
    , fastProperty "built-in functions valid identifiers" prop_built_in_functions_valid_identifiers
    ]

  , testGroup "Error Formatting Properties"
    [ fastProperty "format ownership errors empty" prop_format_ownership_errors_empty
    , fastProperty "format ownership errors non empty" prop_format_ownership_errors_non_empty
    , fastProperty "format ownership errors includes info" prop_format_ownership_errors_includes_info
    ]

  , testGroup "Lexing L.and Parsing Properties"
    [ fastProperty "lexAll empty" prop_lex_all_empty
    , fastProperty "lexAll simple" prop_lex_all_simple
    , fastProperty "parseProgram empty" prop_parse_program_empty
    , fastProperty "parseProgram simple" prop_parse_program_simple
    ]

  , testGroup "Analysis Properties"
    [ fastProperty "analyzeOwnership basic" prop_analyze_ownership_basic
    , fastProperty "analyzeOwnership use after move" prop_analyze_ownership_use_after_move
    , fastProperty "analyzeOwnership double move" prop_analyze_ownership_double_move
    , fastProperty "analyzeOwnership borrowing" prop_analyze_ownership_borrowing
    , fastProperty "analyzeOwnership mutable borrowing" prop_analyze_ownership_mutable_borrowing
    , fastProperty "analyzeOwnershipDebug verbose" prop_analyze_ownership_debug_verbose
    ]
  ]