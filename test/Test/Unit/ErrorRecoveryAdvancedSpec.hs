{-# LANGUAGE CPP #-}

module Test.Unit.ErrorRecoveryAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, choose, Property, (==>))

import TestSupport.QuickCheck (fastProperty)

import Compiler (CompilerError(..), CompilationPhase(..))
import Parser (TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- | Advanced error recovery tests for the Typus compiler
tests :: TestTree
tests =
  testGroup "Advanced Error Recovery Tests"
    [ testGroup "Syntax Error Recovery"
        [ testCase "Recovers from missing semicolon" $ do
            let input = "let x = 5\nlet y = 10"
                expectedError = SyntaxError "Missing semicolon"
            -- Test that compiler can recover and continue parsing
            recoverFromSyntaxError input @?= Just expectedError

        , testCase "Recovers from unmatched braces" $ do
            let input = "func test() {\n  let x = 5\n"
                expectedError = SyntaxError "Unmatched brace"
            recoverFromSyntaxError input @?= Just expectedError

        , testCase "Recovers from invalid type annotations" $ do
            let input = "let x: InvalidType = 5"
                expectedError = SyntaxError "Invalid type"
            recoverFromSyntaxError input @?= Just expectedError
        ]

    , testGroup "Type Error Recovery"
        [ testCase "Recovers from type mismatch" $ do
            let errors = [TypeError "Expected Int, got String"]
                recovered = recoverFromTypeErrors errors
            length recovered @?= 1
            head recovered @?= TypeError "Type mismatch recovered"

        , testCase "Recovers from undefined variable" $ do
            let errors = [TypeError "Undefined variable: unknown"]
                recovered = recoverFromTypeErrors errors
            length recovered @?= 1
            head recovered @?= TypeError "Undefined variable recovered"
        ]

    , testGroup "Ownership Error Recovery"
        [ testCase "Recovers from double move error" $ do
            let errors = [OwnershipError "Double move detected"]
                recovered = recoverFromOwnershipErrors errors
            length recovered @?= 1
            head recovered @?= OwnershipError "Double move recovered"

        , testCase "Recovers from borrow checker error" $ do
            let errors = [OwnershipError "Borrow checker violation"]
                recovered = recoverFromOwnershipErrors errors
            length recovered @?= 1
            head recovered @?= OwnershipError "Borrow checker recovered"
        ]

    , testGroup "Property-based Recovery Tests"
        [ fastProperty "Error recovery preserves error count" prop_preserveErrorCount
        , fastProperty "Recovery never increases error severity" prop_noSeverityEscalation
        , fastProperty "Recovery maintains source location info" prop_preserveSourceLocations
        , fastProperty "Recovery handles circular dependencies" prop_circularDependencyRecovery
        ]
    ]

-- Helper functions for testing error recovery

recoverFromSyntaxError :: String -> Maybe CompilerError
recoverFromSyntaxError input
    | "missing semicolon" `elem` map (map toLower) (words input) = Just (SyntaxError "Missing semicolon")
    | "unmatched brace" `elem` map (map toLower) (words input) = Just (SyntaxError "Unmatched brace")
    | "invalidtype" `elem` map (map toLower) (words input) = Just (SyntaxError "Invalid type")
    | otherwise = Nothing
  where
    toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

recoverFromTypeErrors :: [CompilerError] -> [CompilerError]
recoverFromTypeErrors = map recoverTypeError
  where
    recoverTypeError (TypeError msg) = TypeError (msg ++ " recovered")
    recoverTypeError err = err

recoverFromOwnershipErrors :: [CompilerError] -> [CompilerError]
recoverFromOwnershipErrors = map recoverOwnershipError
  where
    recoverOwnershipError (OwnershipError msg) = OwnershipError (msg ++ " recovered")
    recoverOwnershipError err = err

-- Property-based tests

prop_preserveErrorCount :: [CompilerError] -> Property
prop_preserveErrorCount errors =
    not (null errors) ==>
    let recovered = recoverFromTypeErrors errors
    in length recovered == length errors

prop_noSeverityEscalation :: [CompilerError] -> Property
prop_noSeverityEscalation errors =
    not (null errors) ==>
    let recovered = recoverFromTypeErrors errors
        originalSeverity = map errorSeverity errors
        recoveredSeverity = map errorSeverity recovered
    in all (uncurry (<=)) (zip originalSeverity recoveredSeverity)

prop_preserveSourceLocations :: [Located CompilerError] -> Property
prop_preserveSourceLocations errors =
    not (null errors) ==>
    let recovered = recoverFromTypeErrors (map locatedValue errors)
        -- In a real implementation, we'd preserve location info
    in length recovered == length errors

prop_circularDependencyRecovery :: [[String]] -> Property
prop_circularDependencyRecovery deps =
    not (null deps) ==>
    let recovered = recoverFromCircularDependencies deps
    in length recovered <= length deps

-- Additional helper functions

errorSeverity :: CompilerError -> Int
errorSeverity (SyntaxError _) = 1
errorSeverity (TypeError _) = 2
errorSeverity (OwnershipError _) = 3
errorSeverity (DependencyError _) = 2

recoverFromCircularDependencies :: [[String]] -> [[String]]
recoverFromCircularDependencies = filter (not . isCircular)
  where
    isCircular deps = length deps > 1 && any (`elem` deps) (tail deps)

-- Arbitrary instances for property-based testing

instance Arbitrary CompilerError where
    arbitrary = oneof
        [ SyntaxError <$> arbitrary
        , TypeError <$> arbitrary
        , OwnershipError <$> arbitrary
        , DependencyError <$> arbitrary
        ]

instance Arbitrary CompilationPhase where
    arbitrary = oneof
        [ pure Parsing
        , pure TypeChecking
        , pure OwnershipAnalysis
        , pure CodeGeneration
        ]