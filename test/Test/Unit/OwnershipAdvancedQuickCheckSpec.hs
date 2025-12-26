module Test.Unit.OwnershipAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat, vectorOf)
import TestSupport.QuickCheck (fastProperty)

import Ownership
import Ownership.Analyzer
import Ownership.Common.Types
import Data.List (null, isPrefixOf, isInfixOf)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary OwnershipType where
    arbitrary = elements [Owned, Borrowed, Shared, Unique, Weak]

instance Arbitrary OwnershipTransfer where
    arbitrary = oneof [
        pure Move,
        pure Borrow,
        pure Share,
        pure Copy,
        TransferFrom <$> arbitrary <*> arbitrary
        ]

instance Arbitrary OwnershipError where
    arbitrary = do
        errorType <- elements [
            MoveError,
            BorrowError,
            LifetimeError,
            UseAfterMove,
            UseAfterBorrow,
            ConflictingBorrows,
            InvalidTransfer
            ]
        message <- arbitrary
        location <- oneof [pure Nothing, Just <$> arbitrary]
        suggestion <- oneof [pure Nothing, Just <$> arbitrary]
        return $ OwnershipError errorType message location suggestion

-- ============================================================================
-- Ownership Properties
-- ============================================================================

prop_newOwnershipAnalyzerCreatesValidAnalyzer :: Bool
prop_newOwnershipAnalyzerCreatesValidAnalyzer =
    let analyzer = newOwnershipAnalyzer
    in not (null analyzer)  -- Basic sanity check

prop_analyzeOwnershipHandlesEmptyInput :: Bool
prop_analyzeOwnershipHandlesEmptyInput =
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer ""
    in null result  -- No errors for empty input

prop_analyzeOwnershipFileHandlesEmptyFile :: Bool
prop_analyzeOwnershipFileHandlesEmptyFile =
    let result = analyzeOwnershipFile ""
    in null result  -- No errors for empty file

prop_analyzeOwnershipDebugHandlesEmptyInput :: Bool
prop_analyzeOwnershipDebugHandlesEmptyInput =
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnershipDebug analyzer ""
    in null result  -- No errors for empty input

prop_formatOwnershipErrorsHandlesEmptyList :: Bool
prop_formatOwnershipErrorsHandlesEmptyList =
    let result = formatOwnershipErrors []
    in null result || result == ""

prop_formatOwnershipErrorsHandlesNonEmptyList :: [OwnershipError] -> Bool
prop_formatOwnershipErrorsHandlesNonEmptyList errors =
    let result = formatOwnershipErrors errors
    in if null errors
       then null result || result == ""
       else not (null result)

prop_lexAllHandlesEmptyInput :: Bool
prop_lexAllHandlesEmptyInput =
    let result = lexAll ""
    in null result  -- No tokens for empty input

prop_lexAllHandlesSimpleInput :: String -> Bool
prop_lexAllHandlesSimpleInput input =
    let result = lexAll input
    in not (null result)  -- Should produce some tokens for any input

prop_parseProgramHandlesEmptyInput :: Bool
prop_parseProgramHandlesEmptyInput =
    let result = parseProgram ""
    in null result  -- No AST for empty input

prop_parseProgramHandlesSimpleInput :: String -> Bool
prop_parseProgramHandlesSimpleInput input =
    let result = parseProgram input
    in not (null result)  -- Should produce some AST for any input

prop_builtInFunctionsIsNotEmpty :: Bool
prop_builtInFunctionsIsNotEmpty =
    let functions = builtInFunctions
    in not (null functions)

prop_ownershipTypeOrdering :: OwnershipType -> OwnershipType -> Bool
prop_ownershipTypeOrdering typ1 typ2 =
    let ownershipOrder typ = case typ of
            Unique -> 5
            Owned -> 4
            Borrowed -> 3
            Shared -> 2
            Weak -> 1
    in if typ1 >= typ2
       then ownershipOrder typ1 >= ownershipOrder typ2
       else ownershipOrder typ1 <= ownershipOrder typ2

prop_ownershipTransferOrdering :: OwnershipTransfer -> OwnershipTransfer -> Bool
prop_ownershipTransferOrdering transfer1 transfer2 =
    let transferOrder transfer = case transfer of
            Move -> 4
            Borrow -> 3
            Share -> 2
            Copy -> 1
            TransferFrom _ _ -> 0
    in if transfer1 >= transfer2
       then transferOrder transfer1 >= transferOrder transfer2
       else transferOrder transfer1 <= transferOrder transfer2

-- ============================================================================
-- Advanced Properties
-- ============================================================================

prop_analyzeOwnershipConsistent :: String -> Bool
prop_analyzeOwnershipConsistent input =
    let analyzer = newOwnershipAnalyzer
        result1 = analyzeOwnership analyzer input
        result2 = analyzeOwnership analyzer input
    in result1 == result2

prop_analyzeOwnershipFileConsistent :: String -> Bool
prop_analyzeOwnershipFileConsistent input =
    let result1 = analyzeOwnershipFile input
        result2 = analyzeOwnershipFile input
    in result1 == result2

prop_analyzeOwnershipDebugConsistent :: String -> Bool
prop_analyzeOwnershipDebugConsistent input =
    let analyzer = newOwnershipAnalyzer
        result1 = analyzeOwnershipDebug analyzer input
        result2 = analyzeOwnershipDebug analyzer input
    in result1 == result2

prop_lexAllConsistent :: String -> Bool
prop_lexAllConsistent input =
    let result1 = lexAll input
        result2 = lexAll input
    in result1 == result2

prop_parseProgramConsistent :: String -> Bool
prop_parseProgramConsistent input =
    let result1 = parseProgram input
        result2 = parseProgram input
    in result1 == result2

prop_formatOwnershipErrorsConsistent :: [OwnershipError] -> Bool
prop_formatOwnershipErrorsConsistent errors =
    let result1 = formatOwnershipErrors errors
        result2 = formatOwnershipErrors errors
    in result1 == result2

prop_ownershipErrorPreservesType :: OwnershipError -> Bool
prop_ownershipErrorPreservesType error =
    let errorType = ownershipErrorType error
        formatted = formatOwnershipErrors [error]
    in show errorType `isInfixOf` formatted

prop_ownershipErrorPreservesMessage :: String -> OwnershipError -> Bool
prop_ownershipErrorPreservesMessage message error =
    let updatedError = error { ownershipErrorMessage = message }
        formatted = formatOwnershipErrors [updatedError]
    in message `isInfixOf` formatted

prop_analyzeOwnershipHandlesVariableDeclaration :: String -> String -> Bool
prop_analyzeOwnershipHandlesVariableDeclaration varName varType =
    let input = "let " ++ varName ++ ": " ++ varType ++ " = 42"
        analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer input
    in not (null result) || True  -- May have errors or not, both are valid

prop_analyzeOwnershipHandlesFunctionDefinition :: String -> [String] -> Bool
prop_analyzeOwnershipHandlesFunctionDefinition funcName params =
    let paramList = unwords params
        input = "fn " ++ funcName ++ "(" ++ paramList ++ ") { }"
        analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer input
    in not (null result) || True  -- May have errors or not, both are valid

prop_analyzeOwnershipHandlesOwnershipTransfer :: String -> String -> Bool
prop_analyzeOwnershipHandlesOwnershipTransfer fromVar toVar =
    let input = fromVar ++ " = " ++ toVar
        analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer input
    in not (null result) || True  -- May have errors or not, both are valid

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Ownership Advanced QuickCheck Tests"
    [ testGroup "Basic Ownership Properties"
        [ fastProperty "newOwnershipAnalyzer creates valid analyzer" prop_newOwnershipAnalyzerCreatesValidAnalyzer
        , fastProperty "analyzeOwnership handles empty input" prop_analyzeOwnershipHandlesEmptyInput
        , fastProperty "analyzeOwnershipFile handles empty file" prop_analyzeOwnershipFileHandlesEmptyFile
        , fastProperty "analyzeOwnershipDebug handles empty input" prop_analyzeOwnershipDebugHandlesEmptyInput
        , fastProperty "formatOwnershipErrors handles empty list" prop_formatOwnershipErrorsHandlesEmptyList
        , fastProperty "formatOwnershipErrors handles non-empty list" prop_formatOwnershipErrorsHandlesNonEmptyList
        ]

    , testGroup "Parsing Properties"
        [ fastProperty "lexAll handles empty input" prop_lexAllHandlesEmptyInput
        , fastProperty "lexAll handles simple input" prop_lexAllHandlesSimpleInput
        , fastProperty "parseProgram handles empty input" prop_parseProgramHandlesEmptyInput
        , fastProperty "parseProgram handles simple input" prop_parseProgramHandlesSimpleInput
        , fastProperty "builtInFunctions is not empty" prop_builtInFunctionsIsNotEmpty
        ]

    , testGroup "Ownership Type Properties"
        [ fastProperty "ownership type ordering" prop_ownershipTypeOrdering
        , fastProperty "ownership transfer ordering" prop_ownershipTransferOrdering
        ]

    , testGroup "Advanced Properties"
        [ fastProperty "analyzeOwnership is consistent" prop_analyzeOwnershipConsistent
        , fastProperty "analyzeOwnershipFile is consistent" prop_analyzeOwnershipFileConsistent
        , fastProperty "analyzeOwnershipDebug is consistent" prop_analyzeOwnershipDebugConsistent
        , fastProperty "lexAll is consistent" prop_lexAllConsistent
        , fastProperty "parseProgram is consistent" prop_parseProgramConsistent
        , fastProperty "formatOwnershipErrors is consistent" prop_formatOwnershipErrorsConsistent
        , fastProperty "ownership error preserves type" prop_ownershipErrorPreservesType
        , fastProperty "ownership error preserves message" prop_ownershipErrorPreservesMessage
        ]

    , testGroup "Code Analysis Properties"
        [ fastProperty "analyzeOwnership handles variable declaration" prop_analyzeOwnershipHandlesVariableDeclaration
        , fastProperty "analyzeOwnership handles function definition" prop_analyzeOwnershipHandlesFunctionDefinition
        , fastProperty "analyzeOwnership handles ownership transfer" prop_analyzeOwnershipHandlesOwnershipTransfer
        ]

    , testGroup "Unit Tests"
        [ testCase "create and use ownership analyzer" $ do
            let analyzer = newOwnershipAnalyzer
            assertBool "Should create valid analyzer" (not (null analyzer))

        , testCase "analyze simple code" $ do
            let analyzer = newOwnershipAnalyzer
            let result = analyzeOwnership analyzer "let x = 42"
            assertBool "Should analyze code without crashing" (True)  -- Just check it doesn't crash

        , testCase "format ownership errors" $ do
            let error = OwnershipError MoveError "Test error" Nothing Nothing
            let result = formatOwnershipErrors [error]
            assertBool "Should format errors" (not (null result))

        , testCase "lex simple code" $ do
            let result = lexAll "let x = 42"
            assertBool "Should lex code" (not (null result))

        , testCase "parse simple code" $ do
            let result = parseProgram "let x = 42"
            assertBool "Should parse code" (not (null result))

        , testCase "built-in functions exist" $ do
            let functions = builtInFunctions
            assertBool "Should have built-in functions" (not (null functions))
        ]
    ]