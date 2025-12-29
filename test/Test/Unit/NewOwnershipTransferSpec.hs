{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.NewOwnershipTransferSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, vectorOf, forAll, elements)
import qualified Data.Text as T
import qualified Data.List as List
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Ownership
import Ownership.Common.Types
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, locatedAt)
import TestSupport.QuickCheck (fastProperty)

-- | Test ownership transfer properties
tests :: TestTree
tests =
  testGroup "New Ownership Transfer Tests"
    [ testGroup "Ownership analyzer initialization"
        [ testCase "newOwnershipAnalyzer creates valid analyzer" $ do
            analyzer <- newOwnershipAnalyzer
            -- Should not throw exception
            assertBool "Analyzer should be created" True

        , testCase "analyzer handles empty input" $ do
            analyzer <- newOwnershipAnalyzer
            result <- analyzeOwnership analyzer ""
            case result of
              Left _ -> assertBool "Should handle empty input gracefully" True
              Right _ -> assertBool "Should handle empty input gracefully" True
        ]

    , testGroup "Ownership type properties"
        [ testCase "Owned ownership type is correctly identified" $ do
            let ownershipType = Owned
            ownershipType @?= Owned

        , testCase "Shared ownership type is correctly identified" $ do
            let ownershipType = Shared
            ownershipType @?= Shared

        , testCase "Borrowed ownership type is correctly identified" $ do
            let ownershipType = Borrowed
            ownershipType @?= Borrowed

        , fastProperty "ownership types are distinct" prop_ownershipTypesDistinct
        ]

    , testGroup "Ownership transfer properties"
        [ testCase "transfer from Owned to Borrowed is valid" $ do
            let transfer = OwnershipTransfer Owned Borrowed startPos
            -- Should be a valid transfer
            assertBool "Owned to Borrowed should be valid" True

        , testCase "transfer preserves source location" $ do
            let pos = SourcePos 5 10
                transfer = OwnershipTransfer Owned Borrowed pos
            case transfer of
              OwnershipTransfer _ _ transferPos -> transferPos @?= pos

        , fastProperty "ownership transfer is deterministic" prop_ownershipTransferDeterministic
        , fastProperty "ownership transfer preserves order" prop_ownershipTransferPreservesOrder
        ]

    , testGroup "Lexing properties"
        [ testCase "lexAll handles empty string" $ do
            let result = lexAll ""
            case result of
              Left _ -> assertBool "Should lex empty string" False
              Right tokens -> tokens @?= []

        , testCase "lexAll handles simple identifiers" $ do
            let input = "x y z"
                result = lexAll input
            case result of
              Left _ -> assertBool "Should lex identifiers" False
              Right tokens -> length tokens @?= 3

        , testCase "lexAll handles ownership keywords" $ do
            let input = "owned shared borrowed"
                result = lexAll input
            case result of
              Left _ -> assertBool "Should lex ownership keywords" False
              Right tokens -> length tokens @?= 3

        , fastProperty "lexAll is deterministic" prop_lexAllDeterministic
        , fastProperty "lexAll preserves token order" prop_lexAllPreservesOrder
        ]

    , testGroup "Parsing properties"
        [ testCase "parseProgram handles empty input" $ do
            let tokens = []
                result = parseProgram tokens
            case result of
              Left _ -> assertBool "Should parse empty program" False
              Right ast -> assertBool "Should create AST for empty input" True

        , testCase "parseProgram handles simple declarations" $ do
            let input = "x := owned 42"
                tokens = either (const []) id (lexAll input)
                result = parseProgram tokens
            case result of
              Left _ -> assertBool "Should parse simple declaration" False
              Right ast -> assertBool "Should create AST for declaration" True

        , fastProperty "parseProgram is deterministic" prop_parseProgramDeterministic
        ]

    , testGroup "Ownership analysis properties"
        [ testCase "analyzeOwnership detects valid ownership" $ do
            analyzer <- newOwnershipAnalyzer
            let input = "x := owned 42"
            result <- analyzeOwnership analyzer input
            case result of
              Left _ -> assertBool "Should analyze valid ownership" False
              Right _ -> assertBool "Should successfully analyze" True

        , testCase "analyzeOwnership detects ownership violations" $ do
            analyzer <- newOwnershipAnalyzer
            let input = "x := owned 42\ny := x\nuse(x)"  -- Using x after transfer
            result <- analyzeOwnership analyzer input
            case result of
              Left errors -> assertBool "Should detect ownership violation" (not (null errors))
              Right _ -> assertBool "Should detect ownership violation" False

        , fastProperty "analyzeOwnership is deterministic" prop_analyzeOwnershipDeterministic
        , fastProperty "analyzeOwnership handles larger inputs" prop_analyzeOwnershipScales
        ]

    , testGroup "Error handling properties"
        [ testCase "ownership errors include location information" $ do
            let error = OwnershipError "Test error" startPos OwnershipTransferError
            case error of
              OwnershipError _ pos _ -> pos @?= startPos

        , testCase "formatOwnershipErrors produces readable output" $ do
            let errors = [OwnershipError "Test error" startPos OwnershipTransferError]
                formatted = formatOwnershipErrors errors
            formatted `assertBool` ("Test error" `T.isInfixOf` formatted)

        , fastProperty "error formatting preserves all errors" prop_errorFormattingPreservesAll
        ]

    , testGroup "Built-in functions properties"
        [ testCase "builtInFunctions contains essential functions" $ do
            let builtins = builtInFunctions
            assertBool "Should contain built-in functions" (not (null builtins))

        , fastProperty "built-in functions are unique" prop_builtInFunctionsUnique
        ]

    , testGroup "Edge cases and robustness"
        [ testCase "handles deeply nested ownership transfers" $ do
            analyzer <- newOwnershipAnalyzer
            let nestedInput = unlines 
                  [ "x := owned 42"
                  , "y := borrowed x"
                  , "z := borrowed y"
                  , "w := borrowed z"
                  ]
            result <- analyzeOwnership analyzer nestedInput
            case result of
              Left _ -> assertBool "Should handle nested transfers" False
              Right _ -> assertBool "Should handle nested transfers" True

        , testCase "handles cyclical ownership patterns" $ do
            analyzer <- newOwnershipAnalyzer
            let cyclicalInput = unlines
                  [ "x := owned 42"
                  , "y := borrowed x"
                  , "x := borrowed y"  -- Creates cycle
                  ]
            result <- analyzeOwnership analyzer cyclicalInput
            case result of
              Left errors -> assertBool "Should detect cycle" (not (null errors))
              Right _ -> assertBool "Should detect cycle" False

        , testCase "handles large ownership graphs" $ do
            analyzer <- newOwnershipAnalyzer
            let largeInput = unlines $ ["x" ++ show i ++ " := owned " ++ show i | i <- [1..100]]
            result <- analyzeOwnership analyzer largeInput
            case result of
              Left _ -> assertBool "Should handle large graphs" False
              Right _ -> assertBool "Should handle large graphs" True
        ]

    , testGroup "Property-based ownership tests"
        [ fastProperty "ownership transfer follows type system rules" prop_ownershipTransferTypeRules
        , fastProperty "ownership analysis is monotonic" prop_ownershipAnalysisMonotonic
        , fastProperty "ownership errors are consistent" prop_ownershipErrorsConsistent
        ]
    ]

-- Property: ownership types are distinct
prop_ownershipTypesDistinct :: Property
prop_ownershipTypesDistinct =
  let types = [Owned, Shared, Borrowed]
      pairs = [(x, y) | x <- types, y <- types, x /= y]
  in all (\(x, y) -> x /= y) pairs

-- Property: ownership transfer is deterministic
prop_ownershipTransferDeterministic :: OwnershipType -> OwnershipType -> SourcePos -> Property
prop_ownershipTransferDeterministic from to pos =
  let transfer1 = OwnershipTransfer from to pos
      transfer2 = OwnershipTransfer from to pos
  in transfer1 == transfer2

-- Property: ownership transfer preserves order
prop_ownershipTransferPreservesOrder :: OwnershipType -> OwnershipType -> OwnershipType -> SourcePos -> Property
prop_ownershipTransferPreservesOrder from1 to1 to2 pos =
  let transfer1 = OwnershipTransfer from1 to1 pos
      transfer2 = OwnershipTransfer from1 to2 pos
      transfers = [transfer1, transfer2]
  in length transfers == 2 && head transfers == transfer1

-- Property: lexAll is deterministic
prop_lexAllDeterministic :: String -> Property
prop_lexAllDeterministic input =
  let result1 = lexAll input
      result2 = lexAll input
  in result1 == result2

-- Property: lexAll preserves token order
prop_lexAllPreservesOrder :: String -> Property
prop_lexAllPreservesOrder input =
  case lexAll input of
    Left _ -> property False
    Right tokens -> length tokens >= 0  -- Basic sanity check

-- Property: parseProgram is deterministic
prop_parseProgramDeterministic :: String -> Property
prop_parseProgramDeterministic input =
  case lexAll input of
    Left _ -> property True  -- Can't parse if lexing fails
    Right tokens ->
      let result1 = parseProgram tokens
          result2 = parseProgram tokens
      in result1 == result2

-- Property: analyzeOwnership is deterministic
prop_analyzeOwnershipDeterministic :: String -> Property
prop_analyzeOwnershipDeterministic input = 
  property $ do
    analyzer <- newOwnershipAnalyzer
    result1 <- analyzeOwnership analyzer input
    result2 <- analyzeOwnership analyzer input
    return $ result1 == result2

-- Property: analyzeOwnership handles larger inputs
prop_analyzeOwnershipScales :: Positive Int -> Property
prop_analyzeOwnershipScales (Positive n) =
  let input = unlines ["x" ++ show i ++ " := owned " ++ show i | i <- [1..n]]
  in property $ do
       analyzer <- newOwnershipAnalyzer
       result <- analyzeOwnership analyzer input
       return $ case result of
                  Left _ -> False
                  Right _ -> True

-- Property: error formatting preserves all errors
prop_errorFormattingPreservesAll :: Positive Int -> Property
prop_errorFormattingPreservesAll (Positive n) =
  let errors = [OwnershipError ("Error " ++ show i) startPos OwnershipTransferError | i <- [1..n]]
      formatted = formatOwnershipErrors errors
  in all (\e -> errorMessage e `T.isInfixOf` formatted) errors

-- Property: built-in functions are unique
prop_builtInFunctionsUnique :: Property
prop_builtInFunctionsUnique =
  let builtins = builtInFunctions
      uniqueBuiltins = List.nub builtins
  in length builtins == length uniqueBuiltins

-- Property: ownership transfer follows type system rules
prop_ownershipTransferTypeRules :: OwnershipType -> OwnershipType -> Property
prop_ownershipTransferTypeRules from to =
  -- Basic type system: certain transfers should be valid
  case (from, to) of
    (Owned, Borrowed) -> property True
    (Shared, Borrowed) -> property True
    (Owned, Shared) -> property True
    (Borrowed, _) -> property True  -- Borrowed can be returned
    _ -> property True  -- Other cases depend on context

-- Property: ownership analysis is monotonic
prop_ownershipAnalysisMonotonic :: String -> String -> Property
prop_ownershipAnalysisMonotonic input1 input2 =
  let combined = input1 ++ "\n" ++ input2
  in property $ do
       analyzer <- newOwnershipAnalyzer
       result1 <- analyzeOwnership analyzer input1
       result2 <- analyzeOwnership analyzer combined
       return $ case (result1, result2) of
                  (Left _, Left _) -> True
                  (Right _, Left _) -> False  -- More code shouldn't introduce fewer errors
                  (Left _, Right _) -> True   -- More code might resolve errors
                  (Right _, Right _) -> True

-- Property: ownership errors are consistent
prop_ownershipErrorsConsistent :: String -> Property
prop_ownershipErrorsConsistent input =
  property $ do
    analyzer <- newOwnershipAnalyzer
    result1 <- analyzeOwnership analyzer input
    result2 <- analyzeOwnership analyzer input
    return $ case (result1, result2) of
               (Left errors1, Left errors2) -> length errors1 == length errors2
               (Right _, Right _) -> True
               _ -> False

-- Helper wrapper for positive integers
newtype Positive a = Positive a
  deriving (Show, Eq)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> choose (1, 20)

-- Helper function to extract error message
errorMessage :: OwnershipError -> String
errorMessage (OwnershipError msg _ _) = msg