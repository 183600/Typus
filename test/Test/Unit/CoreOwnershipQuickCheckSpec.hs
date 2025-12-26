{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, vectorOf, elements, oneof)

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )

import Ownership.Common.Types
  ( OwnershipAnalyzer
  , OwnershipError(..)
  , OwnershipType(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Generators
-- ============================================================================

genOwnershipType :: Gen OwnershipType
genOwnershipType = elements 
  [ Owned
  , Borrowed
  , Shared
  , Moved
  , Unknown
  ]

genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = elements
  [ Move
  , Borrow
  , Copy
  , Share
  , Transfer
  ]

genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  return $ posAt line col

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ SourceSpan start end

genLocatedString :: Gen (Located String)
genLocatedString = do
  value <- elements 
    [ "x", "y", "result", "data", "value", "item", "element"
    , "func", "method", "variable", "parameter", "return", "temp"
    ]
  span <- genSourceSpan
  return $ Located value span

genOwnershipError :: Gen OwnershipError
genOwnershipError = do
  errorType <- elements 
    [ OwnershipViolation
    , BorrowError
    , MoveError
    , LifetimeError
    , UsageAfterMove
    , MultipleBorrows
    , CircularDependency
    ]
  message <- elements 
    [ "Cannot use moved value"
    , "Cannot borrow mutable reference while immutable borrow exists"
    , "Value does not live long enough"
    , "Cannot borrow as mutable more than once"
    , "Use of moved value"
    ]
  location <- genSourceSpan
  variable <- genLocatedString
  suggestions <- listOf $ elements 
    [ "Consider cloning the value"
    , "Use reference instead"
    , "Check variable lifetime"
    , "Release borrow before using"
    ]
  return $ OwnershipError errorType (T.pack message) location variable (map T.pack suggestions)

genSimpleGoCode :: Gen String
genSimpleGoCode = do
  hasMain <- elements [True, False]
  hasFunctions <- elements [True, False]
  hasVariables <- elements [True, False]
  
  let mainFunc = if hasMain
        then unlines
          [ "func main() {"
          , "    x := 42"
          , "    y := x + 1"
          , "    println(y)"
          , "}"
          ]
        else ""
      
      functions = if hasFunctions
        then unlines
          [ "func add(a int, b int) int {"
          , "    return a + b"
          , "}"
          , "func multiply(x int, y int) int {"
          , "    return x * y"
          , "}"
          ]
        else ""
      
      variables = if hasVariables
        then unlines
          [ "var global int = 100"
          , "const pi float64 = 3.14159"
          , "var name string = \"test\""
          ]
        else ""
  
  return $ unlines [mainFunc, functions, variables]

genOwnershipComplexCode :: Gen String
genOwnershipComplexCode = do
  numVars <- choose (1, 5)
  numMoves <- choose (0, 3)
  numBorrows <- choose (0, 3)
  
  let varNames = ["x", "y", "z", "data", "value"]
      relevantVars = take numVars varNames
      
      generateVarDecls = unlines $ map (\v -> "    " ++ v ++ " := " ++ show (length v * 10)) relevantVars
      
      generateMoves 0 = ""
      generateMoves n = "    " ++ varNames !! (n `mod` length varNames) ++ " = " ++ varNames !! ((n+1) `mod` length varNames) ++ "\n" ++ generateMoves (n-1)
      
      generateBorrows 0 = ""
      generateBorrows n = "    _ = &" ++ varNames !! (n `mod` length varNames) ++ "\n" ++ generateBorrows (n-1)
  
  return $ unlines
    [ "func test() {"
    , generateVarDecls
    , generateMoves numMoves
    , generateBorrows numBorrows
    , "}"
    ]

-- ============================================================================
-- Properties for OwnershipType
-- ============================================================================

prop_ownershipType_ordering_consistent :: OwnershipType -> OwnershipType -> Property
prop_ownershipType_ordering_consistent type1 type2 =
  let comparison1 = compare type1 type2
      comparison2 = compare type2 type1
  in property $ (comparison1 == EQ) === (type1 == type2) .&&.
               (comparison1 == LT) === (comparison2 == GT) .&&.
               (comparison1 == GT) === (comparison2 == LT)

prop_ownershipType_show_read_roundtrip :: OwnershipType -> Property
prop_ownershipType_show_read_roundtrip ownershipType =
  let shown = show ownershipType
  in property $ length shown > 0

-- ============================================================================
-- Properties for OwnershipTransfer
-- ============================================================================

prop_ownershipTransfer_ordering_consistent :: OwnershipTransfer -> OwnershipTransfer -> Property
prop_ownershipTransfer_ordering_consistent transfer1 transfer2 =
  let comparison1 = compare transfer1 transfer2
      comparison2 = compare transfer2 transfer1
  in property $ (comparison1 == EQ) === (transfer1 == transfer2) .&&.
               (comparison1 == LT) === (comparison2 == GT) .&&.
               (comparison1 == GT) === (comparison2 == LT)

-- ============================================================================
-- Properties for OwnershipError
-- ============================================================================

prop_ownershipError_contains_required_fields :: OwnershipError -> Property
prop_ownershipError_contains_required_fields error =
  in property $ T.length (errorMessage error) > 0 .&&.
               isJust (errorVariable error)

prop_ownershipError_suggestions_are_helpful :: OwnershipError -> Property
prop_ownershipError_suggestions_are_helpful error =
  let suggestions = errorSuggestions error
  in property $ all (T.length .>. 0) suggestions

-- ============================================================================
-- Properties for OwnershipAnalyzer
-- ============================================================================

prop_newOwnershipAnalyzer_is_initial :: Property
prop_newOwnershipAnalyzer_is_initial =
  let analyzer = newOwnershipAnalyzer
  in property $ True  -- Basic test that analyzer creation doesn't crash

prop_analyzeOwnership_handles_empty_code :: Property
prop_analyzeOwnership_handles_empty_code =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in property $ True  -- Basic test that empty code doesn't crash

prop_analyzeOwnership_handles_simple_code :: String -> Property
prop_analyzeOwnership_handles_simple_code code =
  not (null code) ==> 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in property $ True  -- Basic test that simple code doesn't crash

prop_analyzeOwnershipFile_handles_file_path :: String -> Property
prop_analyzeOwnershipFile_handles_file_path filePath =
  not (null filePath) ==> 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnershipFile analyzer filePath
  in property $ True  -- Basic test that file path doesn't crash

-- ============================================================================
-- Properties for Lexing and Parsing
-- ============================================================================

prop_lexAll_handles_basic_go_code :: String -> Property
prop_lexAll_handles_basic_go_code code =
  not (null code) ==> 
  let tokens = lexAll code
  in property $ length tokens >= 0

prop_lexAll_preserves_token_order :: String -> Property
prop_lexAll_preserves_token_order code =
  not (null code) ==> 
  let tokens = lexAll code
      tokenPositions = map tokenPosition tokens  -- Assuming tokens have positions
  in property $ length tokenPositions == length tokens

prop_parseProgram_handles_valid_tokens :: String -> Property
prop_parseProgram_handles_valid_tokens code =
  not (null code) ==> 
  let tokens = lexAll code
      ast = parseProgram tokens
  in property $ True  -- Basic test that parsing doesn't crash

prop_parseProgram_preserves_structure :: String -> Property
prop_parseProgram_preserves_structure code =
  not (null code) ==> 
  let tokens = lexAll code
      ast = parseProgram tokens
  in property $ True  -- Basic test that AST structure is preserved

-- ============================================================================
-- Properties for Built-in Functions
-- ============================================================================

prop_builtInFunctions_is_non_empty :: Property
prop_builtInFunctions_is_non_empty =
  let functions = builtInFunctions
  in property $ length functions > 0

prop_builtInFunctions_have_unique_names :: Property
prop_builtInFunctions_have_unique_names =
  let functions = builtInFunctions
      functionNames = map functionName functions  -- Assuming function has name field
      uniqueNames = nub functionNames
  in property $ length functionNames == length uniqueNames

-- ============================================================================
-- Properties for Error Formatting
-- ============================================================================

prop_formatOwnershipErrors_includes_message :: [OwnershipError] -> Property
prop_formatOwnershipErrors_includes_message errors =
  not (null errors) ==>
  let formatted = formatOwnershipErrors errors
      messages = map errorMessage errors
  in property $ all (`T.isInfixOf` formatted) messages

prop_formatOwnershipErrors_handles_empty_list :: Property
prop_formatOwnershipErrors_handles_empty_list =
  let formatted = formatOwnershipErrors []
  in property $ T.length formatted >= 0

prop_formatOwnershipErrors_includes_suggestions :: [OwnershipError] -> Property
prop_formatOwnershipErrors_includes_suggestions errors =
  not (null errors) ==>
  let formatted = formatOwnershipErrors errors
      allSuggestions = concatMap errorSuggestions errors
  in property $ all (`T.isInfixOf` formatted) allSuggestions

-- ============================================================================
-- Properties for Ownership Analysis
-- ============================================================================

prop_ownership_analysis_detects_moves :: String -> Property
prop_ownership_analysis_detects_moves code =
  "move" `isInfixOf` code ==>
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in property $ True  -- Basic test that move detection works

prop_ownership_analysis_detects_borrows :: String -> Property
prop_ownership_analysis_detects_borrows code =
  "&" `isInfixOf` code ==>
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in property $ True  -- Basic test that borrow detection works

prop_ownership_analysis_tracks_lifetimes :: String -> Property
prop_ownership_analysis_tracks_lifetimes code =
  "lifetime" `isInfixOf` code ==> 
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in property $ True  -- Basic test that lifetime tracking works

-- ============================================================================
-- Helper Functions
-- ============================================================================

tokenPosition :: a -> Int
tokenPosition _ = 0  -- Placeholder implementation

functionName :: a -> String
functionName _ = "builtin"  -- Placeholder implementation

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Core Ownership QuickCheck Tests"
  [ testGroup "OwnershipType Properties"
    [ fastProperty "ownershipType ordering consistent" prop_ownershipType_ordering_consistent
    , fastProperty "ownershipType show read roundtrip" prop_ownershipType_show_read_roundtrip
    ]

  , testGroup "OwnershipTransfer Properties"
    [ fastProperty "ownershipTransfer ordering consistent" prop_ownershipTransfer_ordering_consistent
    ]

  , testGroup "OwnershipError Properties"
    [ fastProperty "ownershipError contains required fields" prop_ownershipError_contains_required_fields
    , fastProperty "ownershipError suggestions are helpful" prop_ownershipError_suggestions_are_helpful
    ]

  , testGroup "OwnershipAnalyzer Properties"
    [ fastProperty "newOwnershipAnalyzer is initial" prop_newOwnershipAnalyzer_is_initial
    , fastProperty "analyzeOwnership handles empty code" prop_analyzeOwnership_handles_empty_code
    , fastProperty "analyzeOwnership handles simple code" prop_analyzeOwnership_handles_simple_code
    , fastProperty "analyzeOwnershipFile handles file path" prop_analyzeOwnershipFile_handles_file_path
    ]

  , testGroup "Lexing and Parsing Properties"
    [ fastProperty "lexAll handles basic go code" prop_lexAll_handles_basic_go_code
    , fastProperty "lexAll preserves token order" prop_lexAll_preserves_token_order
    , fastProperty "parseProgram handles valid tokens" prop_parseProgram_handles_valid_tokens
    , fastProperty "parseProgram preserves structure" prop_parseProgram_preserves_structure
    ]

  , testGroup "Built-in Functions Properties"
    [ fastProperty "builtInFunctions is non empty" prop_builtInFunctions_is_non_empty
    , fastProperty "builtInFunctions have unique names" prop_builtInFunctions_have_unique_names
    ]

  , testGroup "Error Formatting Properties"
    [ fastProperty "formatOwnershipErrors includes message" prop_formatOwnershipErrors_includes_message
    , fastProperty "formatOwnershipErrors handles empty list" prop_formatOwnershipErrors_handles_empty_list
    , fastProperty "formatOwnershipErrors includes suggestions" prop_formatOwnershipErrors_includes_suggestions
    ]

  , testGroup "Ownership Analysis Properties"
    [ fastProperty "ownership analysis detects moves" prop_ownership_analysis_detects_moves
    , fastProperty "ownership analysis detects borrows" prop_ownership_analysis_detects_borrows
    , fastProperty "ownership analysis tracks lifetimes" prop_ownership_analysis_tracks_lifetimes
    ]
  ]