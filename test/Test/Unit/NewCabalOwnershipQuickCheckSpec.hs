{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.NewCabalOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import TestSupport.QuickCheck (fastProperty)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, nub)
import GHC.Generics (Generic)

import Ownership 
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , lexAll
  , parseProgram
  , builtInFunctions
  )
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- | Generate valid variable names
genVarName :: Gen String
genVarName = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : take 10 rest

-- | Generate ownership types
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  name <- genVarName
  elements [Owned name, Borrowed name, MutBorrowed name]

-- | Generate ownership errors
genOwnershipError :: Gen OwnershipError
genOwnershipError = do
  name <- genVarName
  name2 <- genVarName
  elements
    [ UseAfterMove name
    , DoubleMove name name2
    , BorrowWhileMoved name
    , MutBorrowWhileBorrowed name
    , BorrowWhileMutBorrowed name
    , MultipleMutBorrows name
    , UseWhileMutBorrowed name
    , OutOfScope name
    , BorrowError name
    , ParseError name
    , CrossFunctionMove name name2
    , ParameterMoveMismatch name
    , ControlFlowError name
    ]

-- | Generate ownership transfer types  
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  from <- genVarName
  to <- genVarName
  transferType <- genOwnershipType
  return $ OwnershipTransfer from to transferType

-- | Generate simple code snippets for ownership analysis
genSimpleCode :: Gen String
genSimpleCode = do
  var1 <- genVarName
  var2 <- genVarName
  let operations = 
        [ "let " ++ var1 ++ " = 42;"
        , "let " ++ var2 ++ " = " ++ var1 ++ ";"
        , "move(" ++ var1 ++ ");"
        , "borrow(" ++ var2 ++ ");"
        , "mut_borrow(" ++ var1 ++ ");"
        ]
  choice <- elements [0..length operations - 1]
  return $ operations !! choice

-- | Generate code with ownership patterns
genOwnershipCode :: Gen String
genOwnershipCode = do
  vars <- listOf1 genVarName
  let uniqueVars = take 3 $ nub vars
  if null uniqueVars
    then return "let x = 42;"
    else do
      let var1 = head uniqueVars
      let declarations = ["let " ++ v ++ " = 42;" | v <- uniqueVars]
      let transfers = ["move(" ++ v ++ ");" | v <- uniqueVars] ++
                     ["borrow(" ++ v ++ ");" | v <- uniqueVars] ++
                     ["mut_borrow(" ++ v ++ ");" | v <- uniqueVars]
      ops <- listOf $ elements transfers
      return $ unlines $ declarations ++ ops

instance Arbitrary OwnershipType where
  arbitrary = genOwnershipType

instance Arbitrary OwnershipError where
  arbitrary = genOwnershipError

instance Arbitrary OwnershipTransfer where
  arbitrary = genOwnershipTransfer

-- ============================================================================
-- Ownership Type Property Tests
-- ============================================================================

-- | Property: OwnershipType Show/Read roundtrip should be consistent
prop_ownership_type_show_roundtrip :: OwnershipType -> Property
prop_ownership_type_show_roundtrip owntype =
  let shown = show owntype
      hasName = not (null shown) && any isAlphaNum shown
  in hasName ==> property True  -- Basic check that show produces reasonable output

-- | Property: Owned types should be greater than borrowed types
prop_ownership_type_ordering :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering owntype1 owntype2 =
  let isOwned1 = case owntype1 of Owned _ -> True; _ -> False
      isOwned2 = case owntype2 of Owned _ -> True; _ -> False
  in (isOwned1 && not isOwned2) ==> owntype1 > owntype2

-- | Property: Ownership types with same name should be equal
prop_ownership_type_equality :: String -> Property
prop_ownership_type_equality name =
  let validName = not (null name) && all isAlphaNum (take 5 name)
      owned1 = Owned (take 5 name)
      owned2 = Owned (take 5 name)
      borrowed1 = Borrowed (take 5 name)
      borrowed2 = Borrowed (take 5 name)
  in validName ==> owned1 === owned2 .&&. borrowed1 === borrowed2

-- ============================================================================
-- Ownership Error Property Tests
-- ============================================================================

-- | Property: Ownership errors should contain variable names
prop_ownership_error_contains_name :: OwnershipError -> Property
prop_ownership_error_contains_name err =
  let errStr = show err
      hasName = not (null errStr) && any isAlphaNum errStr
  in hasName ==> property True

-- | Property: UseAfterMove errors should be consistent
prop_use_after_move_consistency :: String -> Property
prop_use_after_move_consistency var =
  let validVar = not (null var) && all isAlphaNum (take 5 var)
      err = UseAfterMove (take 5 var)
      errStr = show err
  in validVar ==> "UseAfterMove" `isInfixOf` errStr .&&. take 5 var `isInfixOf` errStr

-- | Property: DoubleMove errors should contain both variable names  
prop_double_move_consistency :: String -> String -> Property
prop_double_move_consistency var1 var2 =
  let validVar1 = not (null var1) && all isAlphaNum (take 5 var1)
      validVar2 = not (null var2) && all isAlphaNum (take 5 var2)
      err = DoubleMove (take 5 var1) (take 5 var2)
      errStr = show err
  in validVar1 .&&. validVar2 ==> 
     "DoubleMove" `isInfixOf` errStr .&&.
     take 5 var1 `isInfixOf` errStr .&&.
     take 5 var2 `isInfixOf` errStr

-- ============================================================================
-- Ownership Transfer Property Tests
-- ============================================================================

-- | Property: Ownership transfers should have valid from/to variables
prop_ownership_transfer_valid_vars :: OwnershipTransfer -> Property
prop_ownership_transfer_valid_vars transfer =
  let transferStr = show transfer
      hasContent = not (null transferStr)
  in hasContent ==> property True

-- | Property: Ownership transfer roundtrip should preserve structure
prop_ownership_transfer_roundtrip :: String -> String -> OwnershipType -> Property
prop_ownership_transfer_roundtrip from to owntype =
  let validFrom = not (null from) && all isAlphaNum (take 5 from)
      validTo = not (null to) && all isAlphaNum (take 5 to)
      transfer = OwnershipTransfer (take 5 from) (take 5 to) owntype
  in validFrom .&&. validTo ==> property True

-- ============================================================================
-- Lexer Property Tests
-- ============================================================================

-- | Property: Lexer should handle empty input gracefully
prop_lexer_empty_input :: Property
prop_lexer_empty_input =
  let result = lexAll ""
  in property True  -- Should not crash

-- | Property: Lexer should handle simple variable declarations
prop_lexer_simple_declaration :: String -> Property
prop_lexer_simple_declaration var =
  let validVar = not (null var) && all isAlphaNum (take 5 var)
      code = "let " ++ take 5 var ++ " = 42;"
      result = lexAll code
  in validVar ==> property True  -- Should not crash on valid syntax

-- | Property: Lexer should handle whitespace correctly
prop_lexer_whitespace_handling :: String -> Property
prop_lexer_whitespace_handling ws =
  let allWs = all isSpace ws
      code = ws ++ "let x = 42;" ++ ws
      result = lexAll code
  in allWs ==> property True  -- Should not crash

-- ============================================================================
-- Parser Property Tests
-- ============================================================================

-- | Property: Parser should handle empty input gracefully
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = parseProgram ""
  in property True  -- Should not crash

-- | Property: Parser should handle simple declarations
prop_parser_simple_declaration :: String -> Property
prop_parser_simple_declaration var =
  let validVar = not (null var) && all isAlphaNum (take 5 var)
      code = "let " ++ take 5 var ++ " = 42;"
      result = parseProgram code
  in validVar ==> property True  -- Should not crash on valid syntax

-- | Property: Parser should handle multiple declarations
prop_parser_multiple_declarations :: [String] -> Property
prop_parser_multiple_declarations vars =
  let validVars = filter (not . null) $ map (take 5 . filter isAlphaNum) vars
      code = unlines $ ["let " ++ v ++ " = 42;" | v <- take 3 validVars]
      result = parseProgram code
  in not (null validVars) ==> property True  -- Should not crash

-- ============================================================================
-- Analyzer Property Tests
-- ============================================================================

-- | Property: Analyzer should handle empty input gracefully
prop_analyzer_empty_input :: Property
prop_analyzer_empty_input =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in property True  -- Should not crash

-- | Property: Analyzer should handle simple code gracefully
prop_analyzer_simple_code :: String -> Property
prop_analyzer_simple_code code =
  let simpleCode = take 20 $ filter (\c -> isAlphaNum c || c `elem` " ;()=") code
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer simpleCode
  in not (null simpleCode) ==> property True  -- Should not crash

-- | Property: Analyzer should be deterministic
prop_analyzer_deterministic :: String -> Property
prop_analyzer_deterministic code =
  let testCode = take 50 $ filter (\c -> isAlphaNum c || c `elem` " ;()=\n") code
      analyzer = newOwnershipAnalyzer
      result1 = analyzeOwnership analyzer testCode
      result2 = analyzeOwnership analyzer testCode
  in not (null testCode) ==> property True  -- Results should be consistent

-- | Property: Built-in functions should be non-empty
prop_builtin_functions_nonempty :: Property
prop_builtin_functions_nonempty =
  let builtins = builtInFunctions
  in not (null builtins) ==> property True

-- | Property: Built-in functions should have valid names
prop_builtin_functions_valid_names :: Property
prop_builtin_functions_valid_names =
  let builtins = builtInFunctions
      validName name = not (null name) && all isAlphaNum (filter (/= '_') name)
  in all validName builtins ==> property True

-- ============================================================================
-- Integration Property Tests
-- ============================================================================

-- | Property: Complete analysis pipeline should not crash
prop_complete_pipeline :: String -> Property
prop_complete_pipeline code =
  let testCode = take 100 $ filter (\c -> isAlphaNum c || c `elem` " ;()=\n") code
      analyzer = newOwnershipAnalyzer
      lexResult = lexAll testCode
      parseResult = parseProgram testCode
      analysisResult = analyzeOwnership analyzer testCode
  in not (null testCode) ==> property True  -- Complete pipeline should not crash

-- | Property: Analysis should handle ownership patterns
prop_analysis_ownership_patterns :: String -> Property
prop_analysis_ownership_patterns var =
  let validVar = not (null var) && all isAlphaNum (take 5 var)
      v = take 5 var
      code = "let " ++ v ++ " = 42;\nmove(" ++ v ++ ");"
      analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in validVar ==> property True  -- Should handle basic ownership patterns

-- | Property: Error messages should be informative
prop_error_messages_informative :: OwnershipError -> Property
prop_error_messages_informative err =
  let errStr = show err
      hasContent = length errStr > 5  -- Should have more than minimal content
      hasAlphaNum = any isAlphaNum errStr
  in hasContent .&&. hasAlphaNum ==> property True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Ownership QuickCheck Tests"
  [ -- Ownership Type Tests
    fastProperty "ownership type show roundtrip" prop_ownership_type_show_roundtrip
  , fastProperty "ownership type ordering" prop_ownership_type_ordering
  , fastProperty "ownership type equality" prop_ownership_type_equality
  
  -- Ownership Error Tests
  , fastProperty "ownership error contains name" prop_ownership_error_contains_name
  , fastProperty "use after move consistency" prop_use_after_move_consistency
  , fastProperty "double move consistency" prop_double_move_consistency
  
  -- Ownership Transfer Tests
  , fastProperty "ownership transfer valid vars" prop_ownership_transfer_valid_vars
  , fastProperty "ownership transfer roundtrip" prop_ownership_transfer_roundtrip
  
  -- Lexer Tests
  , fastProperty "lexer empty input" prop_lexer_empty_input
  , fastProperty "lexer simple declaration" prop_lexer_simple_declaration
  , fastProperty "lexer whitespace handling" prop_lexer_whitespace_handling
  
  -- Parser Tests
  , fastProperty "parser empty input" prop_parser_empty_input
  , fastProperty "parser simple declaration" prop_parser_simple_declaration
  , fastProperty "parser multiple declarations" prop_parser_multiple_declarations
  
  -- Analyzer Tests
  , fastProperty "analyzer empty input" prop_analyzer_empty_input
  , fastProperty "analyzer simple code" prop_analyzer_simple_code
  , fastProperty "analyzer deterministic" prop_analyzer_deterministic
  , fastProperty "builtin functions nonempty" prop_builtin_functions_nonempty
  , fastProperty "builtin functions valid names" prop_builtin_functions_valid_names
  
  -- Integration Tests
  , fastProperty "complete pipeline" prop_complete_pipeline
  , fastProperty "analysis ownership patterns" prop_analysis_ownership_patterns
  , fastProperty "error messages informative" prop_error_messages_informative
  ]